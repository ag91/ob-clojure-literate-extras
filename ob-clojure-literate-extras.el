(require 'dash)
(require 's)

;; support a :deps header in ob-clojure blocks (can't work for bb and nbb, because they manage deps natively)
(defcustom ob-clojure-extras-cider-extra-deps nil "Extra deps to add to cider startup")

(defun ob-clojure-extras-cider-add-extra-deps (orig-fun &rest args)
  (append (apply orig-fun args) ob-clojure-extras-cider-extra-deps))

(advice-add 'cider--jack-in-required-dependencies :around #'ob-clojure-extras-cider-add-extra-deps)

(add-to-list
 'org-babel-header-args:clojure
 '(deps . :any))

(defun ob-clojure-eval-with-cider (expanded params)
  "Evaluate EXPANDED code block with PARAMS using cider."
  (condition-case nil (require 'cider)
    (user-error "cider not available"))
  (let ((connection (cider-current-connection (cdr (assq :target params))))
        (result-params (cdr (assq :result-params params)))
        result0)
    (unless connection (let ((ob-clojure-extras-cider-extra-deps (alist-get :deps params))) (sesman-start-session 'CIDER)))
    (if (not connection)
        ;; Display in the result instead of using `user-error'
        (setq result0 "Please reevaluate when nREPL is connected")
      (ob-clojure-with-temp-expanded expanded params
        (let ((response (nrepl-sync-request:eval exp connection)))
          (push (or (nrepl-dict-get response "root-ex")
                    (nrepl-dict-get response "ex")
                    (nrepl-dict-get
                     response (if (or (member "output" result-params)
                                      (member "pp" result-params))
                                  "out"
                                "value")))
                result0)))
      (ob-clojure-string-or-list
       ;; Filter out s-expressions that return nil (string "nil"
       ;; from nrepl eval) or comment forms (actual nil from nrepl)
       (reverse (delete "" (mapcar (lambda (r)
                                     (replace-regexp-in-string "nil" "" (or r "")))
                                   result0)))))))
  ;;; begin - make ob-clojure work with a no project clojurescript shadow-cljs session with deps
(defun ob-clojure-extras-cider-run-projectless-cljs-repl (&optional repl-type deps extras)
  "Start a projectless cljs REPL running on REPL-TYPE with DEPS.
Any other shadow-cljs options goes verbatim in EXTRAS.
DEPS needs to be something like '((\"foo/bar\" \"0.0.1\") (\"foo/baz\" \"0.0.2\"))"
  (when (and
         ;; I have available shadowcljs
         (executable-find "shadow-cljs")
         ;; check if cider has a shadow session running
         (not (--any (and (s-contains-p "cider-repl" (buffer-name it))
                          (s-contains-p "cljs" (buffer-name it))
                          (s-contains-p (file-name-base (directory-file-name default-directory)) (buffer-name it)))
                     (buffer-list)))
         ;; not in a cljs project
         (not (--any (or (s-ends-with-p ".edn" it)
                         (s-ends-with-p ".clj" it))
                     (directory-files (or (ignore-errors (project-root (project-current)))
                                          default-directory)))))
    (let* ((repl-type (--> (or
                            repl-type
                            (completing-read "Shadow-cljs REPL type:" '("browser-repl" "node-repl") nil t))
                           (if (stringp it) it (symbol-name it))))
           (deps-as-vectors (concat "["
                                    (s-join "\n"
                                            (--map (concat "[" (nth 0 it) " \"" (nth 1 it) "\"]")
                                                   deps))
                                    "]")))
      (with-temp-file "shadow-cljs.edn"
        (insert (concat "{\n:dependencies " deps-as-vectors
                        "\n"
                        extras
                        "}")))
      (async-shell-command (concat "shadow-cljs " repl-type))
      ;; in a bit get rid of the temporary shadow-cljs.edn
      (run-with-idle-timer 10 nil (lambda () (shell-command "rm shadow-cljs.edn")))
      repl-type)))

(defun ob-clojure-extras-setup-shadow-cljs-project-if-possible (orig-fun &rest args)
  (if-let* ((headers (nth 1 args))
            (_ (equal "cljs" (alist-get :target headers)))
            (cider-default-cljs-repl 'shadow)
            (cider-shadow-default-options
             (ob-clojure-extras-cider-run-projectless-cljs-repl (alist-get :shadowcljs-type headers)
                                                                (alist-get :deps headers)
                                                                (alist-get :shadowcljs-extra headers))))
      (progn
        (while (not (car-safe (nrepl-extract-ports (cider--file-path "."))))
          (message "Waiting for shadow-cljs to connect...")
          (sleep-for 1))
        (cider-connect-cljs (list
                             :host "localhost"
                             :port (car (nrepl-extract-ports (cider--file-path ".")))
                             :project-dir (concat "~/" (file-relative-name "." "~"))
                             :cljs-repl-type 'shadow))
        (apply orig-fun args))
    (apply orig-fun args)))

(advice-add 'ob-clojure-eval-with-cider :around #'ob-clojure-extras-setup-shadow-cljs-project-if-possible)

(add-to-list
 'org-babel-header-args:clojurescript
 '(shadowcljs-type (browser-repl node-repl)))

(add-to-list
 'org-babel-header-args:clojurescript
 '(deps . :any))
 ;;; end

;; Also add an utility to insert clj dependencies in the right format digested by this extension

(defun ob-clojure-extras-get-clojars-artifacts! ()
  "Returns a vector of [[some/lib \"0.1\"]...]."
  (let ((filename (concat (temporary-file-directory) "all-jars.clj.gz")))
    (with-demoted-errors
        (url-copy-file "https://clojars.org/repo/all-jars.clj.gz" filename)
      (s-lines (shell-command-to-string (format "zcat %s" filename))))))

(defun ob-clojure-extras-get-mvn-artifacts! ()
  "All the artifacts under org.clojure in mvn central"
  (-distinct
   (-flatten
    (-map
     (lambda (group-id)
       (let* ((search-prefix "https://search.maven.org/solrsearch/select?q=g:%22")
              (search-suffix "%22+AND+p:%22jar%22&rows=2000&wt=json")
              (search-url (concat search-prefix group-id search-suffix))
              (json (with-current-buffer
                        (url-retrieve-synchronously search-url)
                      (goto-char url-http-end-of-headers)
                      (delete-region
                       (point-min)
                       (point))
                      (save-excursion
                        (let
                            ((json-object-type 'plist)
                             (json-array-type 'list))
                          (goto-char
                           (point-min))
                          (json-read))))))
         (--> json

              (plist-get it :response)
              (plist-get it :docs)
              (--keep (concat "[" (plist-get it :g) "/" (plist-get it :a) " \"" (plist-get it :latestVersion) "\"" "]") it))))
     '("org.clojure" "com.cognitect")))))

(defvar clj-deps-cache nil "xx")

(defun ob-clojure-extras-get-available-clj-deps (&optional force)
  (if (and (not force) clj-deps-cache)
      clj-deps-cache
    (setq clj-deps-cache (append (ob-clojure-extras-get-clojars-artifacts!) (ob-clojure-extras-get-mvn-artifacts!)))))

(defun ob-clojure-extras-insert-clj-dep ()
  (interactive)
  (insert
   (format
    "'%S" ;; adding the quote to save user's time
    (--map
     (--map
      (if (symbolp it) (symbol-name it) it)
      (read (s-replace-all '(("[" . "(") ("]" . ")")) it))) ;; converting EDN vectors into strings
     (completing-read-multiple "Add Clj dep:" (ob-clojure-extras-get-available-clj-deps))))))
