;;; load-path.el

(defconst user-lisp-directory
  (expand-file-name "lisp/" user-emacs-directory))
(defconst user-site-lisp-directory
  (expand-file-name "site-lisp/" user-emacs-directory))
;(defconst user-data-directory
;  (expand-file-name "data/" user-emacs-directory))
;(defconst user-lib-directory
;  (expand-file-name "lib/" user-emacs-directory))
;(defconst user-override-directory
;  (expand-file-name "override/" user-emacs-directory))

(defun add-to-load-path (path &optional dir)
  (setq load-path
        (cons (expand-file-name path (or dir user-emacs-directory)) load-path)))

;; add all color themes under "theme/" directory.
(defconst user-theme-directory
  (expand-file-name "themes/" user-emacs-directory))
(defconst site-theme-directory
  (expand-file-name "themes/" data-directory))
(defun add-to-custom-theme-load-path (path &optional dir)
  (setq custom-theme-load-path
    (cons (expand-file-name path (or dir user-theme-directory)) custom-theme-load-path)))

(setq custom-theme-load-path nil)
(dolist (dir (list site-theme-directory user-theme-directory))
  (dolist (entry (nreverse (directory-files-and-attributes dir)))
    (if (cadr entry)
      (add-to-custom-theme-load-path (car entry) dir))))

;; Add top-level lisp directories, in case they were not setup by the
;; environment.
(dolist (dir (nreverse
              (list
                    ;user-override-directory
                    ;user-lib-directory
                    user-lisp-directory
                    user-site-lisp-directory)))
  (dolist (entry (nreverse (directory-files-and-attributes dir)))
    (if (cadr entry)
        (add-to-load-path (car entry) dir))))

(mapc #'add-to-load-path
      (nreverse
       (list
        user-emacs-directory
        )))

(let ((cl-p load-path))
  (while cl-p
    (setcar cl-p (file-name-as-directory
                  (expand-file-name (car cl-p))))
    (setq cl-p (cdr cl-p))))

(setq load-path (delete-dups load-path))

(require 'autoloads nil t)
(require 'cus-load nil t)

(provide 'load-path)

;;; load-path.el ends here
