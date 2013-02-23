;;; load-path.el

(defconst user-lisp-directory
  (expand-file-name "lisp/" user-emacs-directory))
(defconst user-theme-directory
  (expand-file-name "themes/" user-emacs-directory))
(defconst site-theme-directory
  (expand-file-name "themes/" data-directory))
(defconst elpa-package-directory
  (expand-file-name "elpa/" user-emacs-directory))

(defun add-to-load-path (path &optional dir)
  (setq load-path
        (cons (expand-file-name path (or dir user-emacs-directory))
              load-path)))

(defun add-to-custom-theme-load-path (path &optional dir)
  (setq custom-theme-load-path
        (cons (expand-file-name path (or dir user-theme-directory))
              custom-theme-load-path)))

;; load-path
(dolist (dir (nreverse (list user-lisp-directory
                             elpa-package-directory
                             user-theme-directory)))
  (dolist (entry (nreverse (directory-files-and-attributes dir)))
    (if (cadr entry)
        (add-to-load-path (car entry) dir))))

(mapc #'add-to-load-path
      (nreverse (list user-emacs-directory)))

(let ((cl-p load-path))
  (while cl-p
    (setcar cl-p (file-name-as-directory
                  (expand-file-name (car cl-p))))
    (setq cl-p (cdr cl-p))))

(setq load-path (delete-dups load-path))


;; custom-theme-load-path
(setq custom-theme-load-path nil)
(dolist (dir (list site-theme-directory user-theme-directory))
  (dolist (entry (nreverse (directory-files-and-attributes dir)))
    (if (cadr entry)
        (add-to-custom-theme-load-path (car entry) dir))))

(require 'autoloads nil t)
(require 'cus-load nil t)

(provide 'load-path)
