;;; load-path.el

;;;; Custom Themes
(defconst user-theme-directory
  (expand-file-name "themes/" user-emacs-directory))
(defconst site-theme-directory
  (expand-file-name "themes/" data-directory))

(defun add-to-custom-theme-load-path (path &optional dir)
  (setq custom-theme-load-path
        (cons (expand-file-name path (or dir user-theme-directory))
              custom-theme-load-path)))

(setq custom-theme-load-path nil)
(dolist (dir (list site-theme-directory user-theme-directory))
  (dolist (entry (nreverse (directory-files-and-attributes dir)))
    (if (cadr entry)
        (add-to-custom-theme-load-path (car entry) dir))))


;;;; External Packages
(require 'package)
(setq package-archives
      (append package-archives
              '(("melpa" . "http://melpa.milkbox.net/packages/"))
              '(("org" . "http://orgmode.org/elpa/"))))

;; Load the list of packages but don't initialize them.
;; `use-package' will arrange the necessary autoload entries.
(package-initialize nil)

;; If never connected to repositories before, download package descriptions so
;; `use-package' can trigger installation of missing packages.
(unless package-archive-contents
    (message "Refreshing ELPA package archives...")
    (package-refresh-contents))

;; ...but before everything, make sure `use-package' is installed.
(unless (package-installed-p 'use-package)
  (message "`use-package' not found.  Installing...")
  (package-install 'use-package))

(require 'use-package)
(setq use-package-minimum-reported-time 0)
