;;; bdd-defuns.el --- personal functions and key bindings

;;; Commentary:
;; Anything that's not third party package specific goes here.
;;
;; - Only use `bind-key' to assign functions to keys unless remapping.
;; - If imported, add the source URL as a comment.

(require 'bind-key)

;;; Code:
(defun kill-region-or-backward-kill-word (arg)
  "If mark is active kill the region else backward kill word.
With argument ARG, do this that many times.

Traditionally Unix uses `C-w' for backward kill word.  Preserve
Emacs default of `kill-region' if the mark is active, otherwise
fallback to `backward-kill-word'.  Also fix `backward-kill-word'
so that it stops at whitespace."
  (interactive "p")


  (defun backward-kill-word-without-spaces (arg)
    "Wrap backward-kill-word to swallow spaces separate from words."

    (if (looking-back "\\s-+") ; whitespace
        (kill-region (point)
                     (progn
                       (re-search-backward "\\S-") ; not whitespace
                       (forward-char 1)
                       (point)))
      (backward-kill-word arg)))

  (if mark-active
      (kill-region (point) (mark))
    (backward-kill-word-without-spaces arg)))

(bind-key "C-w" 'kill-region-or-backward-kill-word)


(global-unset-key (kbd "C-x m")) ; Need this as a prefix. (was `compose-mail')


(defun mark-line (&optional arg)
  "Mark the line where point is.
With argument ARG, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (beginning-of-line)
  (set-mark (point))
  (end-of-line arg))

(bind-key "C-x m l" 'mark-line)


(defun mark-sentence (&optional arg)
  "Mark the sentece around point.
With argument ARG, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (backward-sentence)
  (mark-end-of-sentence arg))

(bind-key "C-x m s" 'mark-sentence)


(defun duplicate-line (&optional arg)
  "Duplicate the where point is.
With argument ARG, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (save-excursion
    (end-of-line)
    (let (line-text)
      (setq line-text (buffer-substring (line-beginning-position) (point)))
      (while (> arg 0)
        (setq arg (- arg 1))
        (newline)
        (delete-horizontal-space) ; delete auto-indendation whitespace
        (insert line-text)))))

(bind-key "C-M-y" 'duplicate-line)


;; originally from http://whattheemacsd.com/key-bindings.el-01.html
;; modified to use `forward-line' because `goto-line' is interactive use only.
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (let ((line (read-number "Go to line: " (line-number-at-pos))))
          (goto-char (point-min))
          (forward-line (1- line))))
    (linum-mode -1)))

(global-set-key [remap goto-line] 'goto-line-with-feedback)


;; called from packages.el
(defun spell-check-and-wrap-at-80 ()
  "Enable three minor modes for neat text."
  (flyspell-mode)
  (auto-fill-mode)
  (set-fill-column 80))

(provide 'bdd-defuns)

;;; bdd-defuns.el ends here
