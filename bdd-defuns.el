(defun insert-utc ()
  "Insert UTC date and time in ISO 8601 format"
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%H:%MZ")))

;; Fix kill-word.
;; Default implementation swallows whitespace after the word.
(defun bdd-kill-word (arg)
  "Special version of kill-word which swallows spaces separate from words"
  (interactive "p")
  (let ((whitespace-regexp "\\s-+"))
    (kill-region (point)
                 (cond
                  ((looking-at whitespace-regexp)
		   (re-search-forward whitespace-regexp) (point))
                  ((looking-at "\n")
		   (kill-line) (bdd-kill-word arg))
                  (t (forward-word arg) (point))))))

(defun bdd-backward-kill-word (arg)
  "Special version of backward-kill-word which swallows spaces separate from words"
  (interactive "p")
  (if (looking-back "\\s-+")
      (kill-region (point) (progn (re-search-backward "\\S-") (forward-char 1) (point)))
    (backward-kill-word arg)))

;; Traditionally Unix uses `C-w' for backward kill word.  Preserve Emacs default
;; of kill-region if the mark is active, otherwise fallback to backward-kill-word.
(defun bdd-kill-region-or-backward-kill-word (arg)
  "If mark is active kill the region else backward kill word."
  (interactive "p")
  (if mark-active
      (kill-region (point) (mark))
    (bdd-backward-kill-word arg)))

; Frequently opened directories.
(setq bdd-code-dir "~/src")
(defun bdd-ido-find-project ()
  (interactive)
  (find-file
   (expand-file-name (ido-completing-read "Project: "
					  (directory-files bdd-code-dir nil "^[^.]")))
   (expand-file-name bdd-code-dir)))
