;; support a :deps header in ob-clojure blocks (can't work for bb and nbb, because they manage deps natively)
(defcustom my/cider-extra-deps nil "Extra deps to add to cider startup")

(defun my/cider-add-extra-deps (orig-fun &rest args)
  (append (apply orig-fun args) my/cider-extra-deps))


(advice-add 'cider--jack-in-required-dependencies :around #'my/cider-add-extra-deps)
(defun ob-clojure-eval-with-cider (expanded params)
  "Evaluate EXPANDED code block with PARAMS using cider."
  (condition-case nil (require 'cider)
    (user-error "cider not available"))
  (let ((connection (cider-current-connection (cdr (assq :target params))))
        (result-params (cdr (assq :result-params params)))
        result0)
    (unless connection (let ((my/cider-extra-deps (alist-get :deps params))) (sesman-start-session 'CIDER)))
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
(defun my/cider-run-projectless-cljs-repl (&optional repl-type deps)
  "Start a projectless cljs REPL running on REPL-TYPE with DEPS.
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
    (let* ((repl-type (or
                       repl-type
                       (completing-read "Shadow-cljs REPL type:" '("browser-repl" "node-repl") nil t)))
           (deps-as-vectors (concat "["
                                    (s-join "\n"
                                            (--map (concat "[" (nth 0 it) " \"" (nth 1 it) "\"]")
                                                   deps))
                                    "]")))
      (with-temp-file "shadow-cljs.edn"
        (insert (concat "{:dependencies " deps-as-vectors "}")))
      (async-shell-command (concat "shadow-cljs " repl-type))
      ;; in a bit get rid of the temporary shadow-cljs.edn
      (run-with-idle-timer 10 nil (lambda () (shell-command "rm shadow-cljs.edn")))
      repl-type)))

(defun my/setup-shadow-cljs-project-if-possible (orig-fun &rest args)
  (if-let* ((headers (nth 1 args))
            (_ (equal "cljs" (alist-get :target headers)))
            (cider-default-cljs-repl 'shadow)
            (cider-shadow-default-options
             (my/cider-run-projectless-cljs-repl (alist-get :shadowcljs-type headers)
                                                 (alist-get :deps headers))))
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

(advice-add 'ob-clojure-eval-with-cider :around #'my/setup-shadow-cljs-project-if-possible)
  ;;; end
