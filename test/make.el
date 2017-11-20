;;; Emacs-Make -- simple, transparent functionality for automated elisp testing
;;; Based on http://edkolev.github.io/posts/2017-09-10-travis-for-emacs-packages.html

;;; Basic support for Caskfile syntax; override with environment variables:

;; PACKAGE_FILE     := the root file of your package
;; PACKAGE_DEV_DEPS := list of development-environment dependencies separated by spaces
;; PACKAGE_SOURCES  := list of package dependencies separated by spaces

;;; You can debug emacs-make by setting the environment variable DEBUG.

(setq debug-on-error (getenv "DEBUG"))

(require 'package)
(require 'subr-x)

(defun make-get-directive-in-caskfile (directive &optional get-all)
  "Get DIRECTIVE from the caskfile if it exists.
If GET-ALL is non-nil, all instances of DIRECTIVE are collected
into a list."
  (let ((caskfile "Cask") expr found)
    (when-let ((caskdir (locate-dominating-file default-directory caskfile))
               (caskfile (expand-file-name caskfile caskdir)))
      (with-temp-buffer
        (insert-file-contents-literally caskfile)
        (ignore-errors ; may not be able to find DIRECTIVE; don't let `read' error out
          (while (not (or (eobp) found))
            (let ((e (read (current-buffer))))
              (when (eq directive (car-safe e))
                (if get-all
                    (push e expr)
                  (setq expr e
                        found t)))))))
      (cond
       (get-all expr)
       (found (cdr-safe expr))))))

(defconst make-package-file
  (or (getenv "PACKAGE_FILE")
      (car-safe (make-get-directive-in-caskfile 'package-file))
      (error "No PACKAGE_FILE and no Caskfile found."))
  "Main package file.")
(defconst make-project-root
  (locate-dominating-file default-directory make-package-file)
  "Root directory of the project.")
(defconst make-package-archives
  (or (when-let ((deps (getenv "PACKAGE_SOURCES")))
        (thread-last (split-string deps nil t)
          (mapcar #'downcase)
          (mapcar #'intern)))
      (mapcar (lambda (source) (pcase source (`(source ,src) src)))
              (make-get-directive-in-caskfile 'source t))
      '(gnu))
  "List of sources (symbols) to use.")
(defconst make-dev-deps
  (or (when-let ((deps (getenv "PACKAGE_DEV_DEPS")))
        (thread-last (split-string deps nil t)
          (mapcar #'downcase)
          (mapcar #'intern)))
      (mapcar (lambda (dep) (pcase dep (`(depends-on ,package) (intern package))))
              (make-get-directive-in-caskfile 'development)))
  "List of package-symbols that are development dependencies.")

(defconst make-package-archive-master-alist
  '(("melpa" . "http://melpa.org/packages/")
    ("gnu" . "http://elpa.gnu.org/packages/")))

(defvar make-package-desc
  (with-temp-buffer
    (insert-file-contents-literally (expand-file-name make-package-file make-project-root))
    (package-buffer-info))
  "A package-desc object for the current package.")

(defun make (target)
  "Run `make-TARGET' if bound."
  (let ((fun (intern (format "make-%s" target))))
    (unless (fboundp fun)
      (error "%S target not found" target))
    (message "Project file detected as: %S" make-package-file)
    (message "Development dependencies detected as: %S" make-dev-deps)
    (message "Running target %S with %S\n" target fun)
    (funcall fun)))

(defun make-test ()
  "Run all tests in \"PACKAGE-NAME-test.el\"."
  (let* ((project-tests-file (format "%S-test.el" (package-desc-name make-package-desc)))
         (project-tests-path (format "%s/test/" make-project-root)))

    ;; add the package being tested to `load-path' so it can be required
    (add-to-list 'load-path make-project-root)
    (add-to-list 'load-path project-tests-path)

    ;; load the file with tests
    (load (expand-file-name project-tests-file project-tests-path) nil t)

    ;; run the tests
    (ert-run-tests-batch-and-exit)))

(defun make-update ()
  "Update required packages.
Required packages include those that `make-package-file' lists as
dependencies and those in `make-dev-deps'."
  (let ((pkglist (append
                  make-dev-deps
                  (thread-last make-package-desc
                    (package-desc-reqs)
                    (mapcar #'car)
                    (delq 'emacs))))
        (package-user-dir (expand-file-name (format ".elpa/%s/elpa" emacs-version)))
        (package-archives
         (let (l)
           (dolist (pair make-package-archive-master-alist)
             (when (memq (intern (car pair)) make-package-archives)
               (push pair l)))
           l)))

    (message "installing in %s..." package-user-dir)
    (package-initialize)
    (package-refresh-contents)

    ;; install dependencies
    (dolist (package pkglist)
      (unless (package-installed-p package)
        (ignore-errors
          (package-install package))))

    ;; upgrade dependencies
    (save-window-excursion
      (package-list-packages t)
      (condition-case _
          (progn
            (package-menu-mark-upgrades)
            (package-menu-execute t))
        (error
         (message "All packages up to date"))))))
