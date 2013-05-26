;;;; Emacs Configuration
;;;; Berk D. Demir <bdd@mindcast.org>

(defconst emacs-start-time (current-time))
(setq message-log-max 16384)

(defun emacs-d (filename)
  "Expand FILENAME relative to `user-emacs-directory'."
  (expand-file-name filename user-emacs-directory))

(defmacro hook-into-modes (function mode-hooks)
  "Add FUNCTION to hooks in MODE-HOOKS."
  `(dolist (hook ,mode-hooks)
     (add-hook hook ,function)))

;;; External Packages
(load (emacs-d "packages"))
;;; Personal Elisp functions
(load (emacs-d "bdd-defuns"))
;;; Twitter
(load (emacs-d "twitter") 'missing-ok)
;;; Theme
(load-theme 'sanityinc-tomorrow-eighties t)


;;;; Environment
(setq shell-file-name "zsh")
(add-to-list 'exec-path "/usr/local/bin")

;;; Registers
(set-register ?i
              (cons 'file (emacs-d "init.el")))
(set-register ?p
              (cons 'file (emacs-d "packages.el")))

;;; no backup files, no auto-saving
;;;--- TODO: Consolidate save files to a common directory.
(setq make-backup-files nil)
(setq auto-save-default nil
      auto-save-list-file-prefix nil)


;;;; UI
(if window-system
    (progn
      (tool-bar-mode 0)
      (scroll-bar-mode 0)
      ;; 4px left, and no right right fringe
      (set-fringe-style '(4 . 0)))
  ;; No menu bar when running from a terminal.
  (menu-bar-mode 0))


;;;; Mode Line
(setq size-indication-mode t
      line-number-mode t
      column-number-mode t)


;;;; Ido
(ido-mode 1)
(setq ido-enanble-flex-matching t
      ido-everywhere t
      ido-use-virtual-buffers t
      recentf-save-file (emacs-d "var/recentf")
      ido-save-directory-list-file (emacs-d "var/ido-last.el"))

;;;; Keyboard
(when (string= system-type "darwin")
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super
        delete-by-moving-to-trash t
        trash-directory (expand-file-name ".Trash" (getenv "HOME"))))

;;;; Mouse
(when (boundp 'mouse-wheel-scroll-amount)
  (setq mouse-wheel-scroll-amount '(0.01)))


;;;; Global Key Bindings
(require 'bind-key)
(bind-key "C-h" 'delete-backward-char) ; unixism
(bind-key "C-?" 'help-command) ; C-h is gone and <f1> is not really convenient
(bind-key "C-S-k" 'kill-whole-line)
(bind-key "C-j" '(lambda () (interactive) (join-line -1))) ; more useful C-j
(bind-key "C-x C-b" 'ibuffer)
(bind-key "C-x C-d" 'dired)

;;; Window Movement
(bind-key "C-<return>" 'other-window)
(windmove-default-keybindings) ; default modifier key is 'shift.
(setq windmove-wrap-around t)  ;---??? not sure if I really want this.
(winner-mode) ; C-c <left> to restore last window layout

;;; TAB behavior
(setq tab-always-indent 'complete)
(setq-default indent-tabs-mode nil)   ; never use tabs to indent.


;;;; Electric
(electric-pair-mode)   ; paranthesis, braces, quotation marks.
;(electric-indent-mode) ; on-the-fly reindentation.
;; Use auto indentation only in programming modes.
(hook-into-modes '(lambda ()
                    (local-set-key (kbd "RET") 'newline-and-indent))
                 '(prog-mode-hook))

;;;; Whitespace
(setq-default indicate-empty-lines t) ; in the left fringe
(setq require-final-newline 'ask)
(setq whitespace-style '(face trailing tabs tab-mark))
(hook-into-modes 'whitespace-mode '(prog-mode-hook))

;;;; *scratch* buffer
(setq initial-scratch-message nil)
(setq initial-major-mode 'text-mode)
;; Never kill, just bury
(defun dont-kill-but-bury-scratch ()
  (if (equal (buffer-name (current-buffer)) "*scratch*")
      (progn (bury-buffer) nil)
    t))
(add-hook 'kill-buffer-query-functions 'dont-kill-but-bury-scratch)

;;;; Annoyances
(setq inhibit-splash-screen t)
(fset 'yes-or-no-p 'y-or-n-p) ; brevity
(setq ring-bell-function 'ignore) ; hush...
;;; Disable commonly unintended key presses.
(global-unset-key (kbd "C-z")) ; suspend-frame
(global-unset-key (kbd "s-p")) ; ns-print-buffer
(global-unset-key (kbd "s-q")) ; save-buffers-kill-emacs
(global-unset-key (kbd "s-t")) ; ns-popup-font-panel


;;;; Disabled commands
(defun enable-commands (commands)
  (dolist (cmd commands)
    (put cmd 'disabled nil)))

(enable-commands
 '(erase-buffer
   upcase-region
   downcase-region
   dired-find-alternate-file
   narrow-to-region))


;;;; Misc
(show-paren-mode)
(global-auto-revert-mode)
(setq tramp-persistency-file-name (emacs-d "var/tramp-history.el"))


;;;; Internal Packages
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(when window-system
  (add-hook 'after-init-hook 'server-start t))


(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed (float-time (time-subtract (current-time)
                                                       emacs-start-time))))
               (message "Initialization complete. (%.3fs)\n%s"
                        elapsed
                        (make-string 80 ?\-))))
          t)
