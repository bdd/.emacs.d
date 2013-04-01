;;;; Emacs Configuration
;;;; Berk D. Demir <bdd@mindcast.org>

(defconst emacs-start-time (current-time))
(setq message-log-max 16384)

(defun emacs-d (filename)
  "Expand FILENAME relative to `user-emacs-directory.'"
  (expand-file-name filename user-emacs-directory))

(defmacro hook-into-modes (function mode-hooks)
  "Add FUNCTION to hooks in MODE-HOOKS."
  `(dolist (hook ,mode-hooks)
     (add-hook hook ,function)))

;;; Load 'load-path.el' so we know where to load from.
(load (emacs-d "load-path"))
;;; Personal elisp functions.
(load (emacs-d "bdd-defuns"))
;;; Theme
(load-theme 'tomorrow-night-eighties t)


;;;; Environment
(setq shell-file-name "zsh")
(add-to-list 'exec-path "/usr/local/bin")

;;; Registers
(set-register ?i
              (cons 'file (emacs-d "init.el")))

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

;;; TAB behavior
(setq tab-always-indent 'complete)
(setq-default indent-tabs-mode nil)   ; never use tabs to indent.


;;;; Electric
(electric-pair-mode)   ; paranthesis, braces, quotation marks.
(electric-indent-mode) ; on-the-fly reindentation.


;;;; Whitespace
(setq-default indicate-empty-lines t) ; in the left fringe
(setq require-final-newline 'ask)
(setq whitespace-style '(face trailing tabs tab-mark))
(hook-into-modes 'whitespace-mode '(prog-mode-hook))


;;;; Annoyances
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'text-mode)
(fset 'yes-or-no-p 'y-or-n-p) ; brevity
(setq ring-bell-function 'ignore) ; hush...
;;; Disable commonly unintended key presses.
(global-unset-key (kbd "C-z")) ; suspend-frame
(global-unset-key (kbd "s-p")) ; ns-print-buffer
(global-unset-key (kbd "s-q")) ; save-buffers-kill-emacs
(global-unset-key (kbd "s-t")) ; ns-popup-font-panel


;;;; Misc
(show-paren-mode)
(global-auto-revert-mode)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)


;;;; Internal Packages
(when window-system
  (add-hook 'after-init-hook 'server-start t))


;;;; External Packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize nil) ; just load the list.  don't initialize.
(unless package-archive-contents ; never connected to an ELPA repository
  (progn
    (message "Refreshing ELPA package archives.")
    (package-refresh-contents)))

(require 'use-package)
(setq use-package-minimum-reported-time 0)


(use-package ag
  :ensure t
  :defer t
  :config
  (progn
    (setq ag-highlight-search t)
    (bind-key "n" 'compilation-next-error ag-mode-map)
    (bind-key "p" 'compilation-previous-error ag-mode-map)
    (bind-key "N" 'compilation-next-file ag-mode-map)
    (bind-key "P" 'compilation-previous-file ag-mode-map)))

(use-package browse-kill-ring
  :ensure t
  :defer t
  :config
  (progn
    (browse-kill-ring-default-keybindings)))

(use-package diminish
  :ensure t)

(use-package edit-server
  :ensure t
  :if window-system
  :init
  (add-hook 'after-init-hook 'edit-server-start)
  :config
  (progn
    (bind-key "C-c C-k" 'edit-server-abort edit-server-edit-mode-map)
    (add-hook 'edit-server-start-hook
              '(lambda ()
                 (auto-fill-mode)
                 (flyspell-mode)
                 (set-fill-column 80)))))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package fill-column-indicator
  :ensure t
  :defer t)

(use-package gist
  :ensure t
  :bind ("C-c g p" . gist-region-or-buffer-private))

(use-package git-commit-mode
  :ensure t
  :defer t)

(use-package gitconfig-mode
  :ensure t
  :defer t)

(use-package gitignore-mode
  :ensure t
  :defer t)

(use-package go-mode
  :ensure t
  :defer t
  :init
  (progn
    (defun go-capitalize-previous-word ()
      (interactive)
      (backward-word)
      (capitalize-word 1)))

  :config
  (progn
    (bind-key "C-c C-c" 'go-capitalize-previous-word go-mode-map)
    (bind-key "C-c f" 'gofmt go-mode-map)
    (bind-key "C-c d" 'godoc go-mode-map)))

(use-package google-c-style
  :ensure t
  :defer t)

(use-package ido-ubiquitous
  :ensure t
  :init
  ;; Fix ido-ubiquitous for newer packages
  (defmacro ido-ubiquitous-use-new-completing-read (cmd package)
    `(eval-after-load ,package
       '(defadvice ,cmd (around ido-ubiquitous-new activate)
          (let ((ido-ubiquitous-enable-compatibility nil))
            ad-do-it))))
  :config
  (progn
    (ido-ubiquitous-use-new-completing-read yas/expand 'yasnippet)
    (ido-ubiquitous-use-new-completing-read yas/visit-snippet-file 'yasnippet)))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (progn
    (setenv "GIT_PAGER" "")
    (add-hook 'magit-log-edit-mode-hook
              '(lambda ()
                 (auto-fill-mode)
                 (flyspell-mode)
                 (set-fill-column 80)))))

(use-package markdown-mode
  :ensure t
  :mode ("\\.\\(m\\(ark\\)?down\\|md\\)$" . markdown-mode)
  :config
  (progn
    (let ((preferred-markdown-impl "peg-markdown"))
      (when (executable-find preferred-markdown-impl)
        (setq markdown-command preferred-markdown-impl)))

    (add-hook 'markdown-mode-hook
              '(lambda ()
                 (auto-fill-mode)
                 (flyspell-mode)))))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-prvious-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  :config
  (progn
    (setq mc/list-file (emacs-d "var/multiple-cursors-all-or-once.el"))))

(use-package mustache-mode
  :ensure t
  :defer t)

(use-package protobuf-mode
  :ensure t
  :defer t)

(use-package puppet-mode
  :ensure t
  :mode ("\\.pp$" . puppet-mode))

(use-package regex-tool
  :ensure t
  :defer t)

(use-package ruby-tools
  :ensure t
  :defer t)

(use-package scala-mode2
  :ensure t
  :defer t)

(use-package yaml-mode
  :ensure t
  :defer t)

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :init
  (progn
    (setq yas-verbosity 3)
    (yas-global-mode 1)))


(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed (float-time (time-subtract (current-time)
                                                       emacs-start-time))))
               (message "Initialization complete. (%.3fs)\n%s"
                        elapsed
                        (make-string 80 ?\-))))
          t)
