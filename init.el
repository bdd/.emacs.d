;;; init.el --- The Emacs Initialization File
;; Berk D. Demir <bdd@mindcast.org>

(defconst emacs-start-time (current-time))
(setq message-log-max 16384)

(defun emacs-d (filename)
  "Expand FILENAME relative to `user-emacs-directory'."
  (expand-file-name filename user-emacs-directory))

(defmacro hook-into-modes (function mode-hooks)
  "Add FUNCTION to hooks in MODE-HOOKS."
  `(dolist (hook ,mode-hooks)
     (add-hook hook ,function)))

;;; Facebook
(load (emacs-d "facebook") 'missing-ok)
;;; External Packages
(load (emacs-d "packages"))
;;; Personal Elisp functions
(load (emacs-d "bdd-defuns"))

;;;; Environment
(setq shell-file-name "zsh")
(add-to-list 'exec-path "/usr/local/bin")

;;; Registers
(set-register ?i
              (cons 'file (emacs-d "init.el")))
(set-register ?p
              (cons 'file (emacs-d "packages.el")))

;;; no backup files, no auto-saving
;; TODO: Consolidate save files to a common directory.
(setq make-backup-files nil)
(setq auto-save-default nil
      auto-save-list-file-prefix nil)


;;; UI
(menu-bar-mode 0)
(when window-system
  (set-fringe-style '(10 . 10)) ; 10px left, 10px right
  (tool-bar-mode 0)
  (scroll-bar-mode 0))

;;; Mouse
(unless window-system
  (require 'mouse)
  (require 'mwheel)
  (xterm-mouse-mode)
  (mouse-wheel-mode t))

;;; Mode Line
(setq size-indication-mode t
      line-number-mode t
      column-number-mode t)


;;; Ido
(ido-mode 1)
(ido-everywhere 1)
(setq ido-use-virtual-buffers t
      recentf-save-file (emacs-d "var/recentf")
      save-place-file (emacs-d "var/saved-places")
      ido-save-directory-list-file (emacs-d "var/ido-last.el"))

;; Display completions vertically
(setq ido-decorations (quote ("\n> " "" "\n  " "\n  ..." "[" "]"
                              " [No Match]" " [Matched]" " [Not Readable]"
                              " [Too Big]" " [Confirm]")))

(defun ido-disable-line-truncation ()
  "Do not truncate lines in ido mode."
  (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)

(defun ido-define-keys ()
  "C-(n|p) is more intuitive in vertical layout."
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
(add-hook 'ido-setup-hook 'ido-define-keys)


;;;; Keyboard
(when (string= system-type "darwin")
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super
        delete-by-moving-to-trash t
        trash-directory (expand-file-name ".Trash" (getenv "HOME"))))

;;; Mouse
(when (boundp 'mouse-wheel-scroll-amount)
  (setq mouse-wheel-scroll-amount '(0.01)))


;;; Global Key Bindings
(require 'bind-key)
(bind-key "C-h" 'delete-backward-char) ; unixism. use <f1> for help
(bind-key "C-c C-k" 'kill-whole-line)
(bind-key "C-j" '(lambda () (interactive) (join-line -1))) ; more useful C-j
(bind-key "C-x C-b" 'ibuffer)
(bind-key "C-x C-d" 'dired)

(bind-key "C-x k" 'kill-this-buffer) ; instead of `kill-buffer' -- or <s-k>
(bind-key "C-x !" 'revert-buffer) ; -- or <s-u>

;;; Window Movement
(windmove-default-keybindings) ; default modifier key is 'shift.
(winner-mode) ; C-c <left> to restore last window layout

;;; TAB behavior
(setq tab-always-indent 'complete)
(setq-default indent-tabs-mode nil)   ; never use tabs to indent.


;; Use auto indentation only in programming modes.
(hook-into-modes '(lambda ()
                    (local-set-key (kbd "RET") 'newline-and-indent))
                 '(prog-mode-hook))

;; Line wrap at 100 char for all programming modes.
;; An indicator line will be drawn by `fci-mode` defined in `packages.el`
(hook-into-modes '(lambda ()
                    (set-fill-column 100))
                 '(prog-mode-hook))

;;; Whitespace
(setq-default indicate-empty-lines t) ; in the left fringe
(setq require-final-newline t)
(setq whitespace-style '(face trailing))
(hook-into-modes 'whitespace-mode '(prog-mode-hook))

;;; *scratch* buffer
(setq initial-scratch-message nil)
(setq initial-major-mode 'text-mode)
(add-hook 'kill-buffer-query-functions 'dont-kill-but-bury-scratch)
(defun dont-kill-but-bury-scratch ()
  "Don't kill but burry *scratch* buffer."
  (if (equal (buffer-name (current-buffer)) "*scratch*")
      (progn (bury-buffer) nil)
    t))

;;; Annoyances
(setq inhibit-splash-screen t
      ring-bell-function 'ignore)
(fset 'yes-or-no-p 'y-or-n-p)

;;; Disable commonly unintended key presses.
(global-unset-key (kbd "C-z")) ; was `suspend-frame'.
(global-unset-key (kbd "s-p")) ; was `ns-print-buffer'
(global-unset-key (kbd "s-q")) ; was `save-buffers-kill-emacs'
(global-unset-key (kbd "s-t")) ; was `ns-popup-font-panel'


;;; Disabled commands
(dolist (cmd
         '(erase-buffer
           upcase-region
           downcase-region
           dired-find-alternate-file
           narrow-to-region))
  (put cmd 'disabled nil))


;;; Misc
(show-paren-mode)
(global-auto-revert-mode)
(setq tramp-persistency-file-name (emacs-d "var/tramp-history.el"))
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
(hook-into-modes 'hl-line-mode '(prog-mode-hook
                                 package-menu-mode-hook))


;;; Internal Packages
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(when window-system
  (add-hook 'after-init-hook 'server-start t))

(defun init-duration-message ()
  "Print time spent in initialization to *Messages*."
  (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
    (message "Initialization complete.  (%.3fs)\n%s" elapsed (make-string 80 ?\-))))

(add-hook 'after-init-hook 'init-duration-message 'append)

;;; This non-sense below is to stop Emacs 25 modifying `init.el'
;;
;; We initialize packages in `packages.el' but Emacs developers decided to impose their opinions.
;; Keeping it in here, _even commented out_, stops automatic prepending nonsense.
;(package-initialize)
;;
;; More packages enforcement.  `package-selected-packages' custom variable is set automatically.
;; To prevent appending to `init.el' we define a `custom-file' and load it to keep Emacs 25 happy.
(setq custom-file (emacs-d "var/custom-file.el"))
(load custom-file 'missing-ok)

;;; init.el ends here
