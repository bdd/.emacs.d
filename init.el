;;;; Emacs Configuration
;;;; Berk D. Demir <bdd@mindcast.org>

(setq message-log-max 16384)

(defun emacs-d (fn)
  "Expand file name relative to user-emacs-directory."
  (expand-file-name fn user-emacs-directory))

;;; Load 'load-path.el' so we know where to load from.
(load (emacs-d "load-path"))
;;; Personal elisp functions.
(load (emacs-d "bdd-defuns"))
;;; Emacs Customization values.
(let ((custom-file (emacs-d "custom.el")))
  (when (file-exists-p custom-file)
    (load custom-file)))

(require 'use-package)
(eval-when-compile
  (setq use-package-verbose (null byte-compile-current-file)))

;;;; Environment
(setq shell-file-name "zsh")
(add-to-list 'exec-path "/usr/local/bin")

;;; Registers
(set-register ?i
              (cons 'file (emacs-d "init.el")))

;;; no backup files, no auto-saving
;;;--- TODO: Consolidate save files to a common directory.
(setq make-backup-files nil)
(setq auto-save-default nil)


;;;; UI
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode (if window-system 1 0))


;;;; Annoyances
(setq inhibit-splash-screen t)
(fset 'yes-or-no-p 'y-or-n-p) ; brevity
(setq ring-bell-function 'ignore) ; hush...
;;; Disable commonly unintended key presses.
(global-unset-key (kbd "C-z")) ; suspend-frame
(global-unset-key (kbd "s-p")) ; ns-print-buffer
(global-unset-key (kbd "s-q")) ; save-buffers-kill-emacs
(global-unset-key (kbd "s-t")) ; ns-popup-font-panel


;;;; Ido
(ido-mode 1)
(setq ido-enanble-flex-matching t
      ido-everywhere t)

;;;; Keyboard
(when (string= system-type "darwin")
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super
        delete-by-moving-to-trash t))

;;;; Mouse
(when (boundp 'mouse-wheel-scroll-amount)
  (setq mouse-wheel-scroll-amount '(0.01)))


;;;; Global Key Bindings
(define-key global-map (kbd "RET") 'newline-and-indent) ; was C-j

(bind-key "C-h" 'delete-backward-char) ; unixism.  use <f1> for help-command.
(bind-key "C-w" 'bdd-kill-region-or-backward-kill-word) ; more unixism.
(bind-key "C-S-k" 'kill-whole-line)
(bind-key "C-j" 'join-line) ; more useful C-j
(bind-key "C-x C-b" 'ibuffer)

(when (fboundp 'ns-toggle-fullscreen)
  (bind-key "C-M-S-f" 'ns-toggle-fullscreen))

;;; Window Movement
(bind-key "C-<return>" 'other-window)
(windmove-default-keybindings) ; default modifier key is 'shift.
(setq windmove-wrap-around t)  ;---??? not sure if I really want this.

;;;; Misc
;;; Whitespace
(setq-default indicate-empty-lines t)
(setq-default show-trailing-whitespace t)
(setq whitespace-style '(face empty tabs lines-tail tab-mark))
(setq whitespace-line-column nil) ; equals to fill-column
;(global-whitespace-mode t)

(setq-default indent-tabs-mode nil)
(setq tab-always-indent 'complete)
(show-paren-mode t)
(global-auto-revert-mode t)
(setq require-final-newline 'ask)

;;; Electric parans, braces, double quotes, etc.
(electric-pair-mode)


;;;; Mode Line
(setq size-indication-mode t
      line-number-mode t
      column-number-mode t)


;;;; Packages
(require 'cl)
(require 'package)
(setq package-enable-at-startup nil) ; do not initialize ELPA packages at startup.
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(defun my-elpa-pkgs-installed-p ()
  (loop for pkg in my-elpa-pkgs
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(defun install-my-elpa-pkgs ()
  (interactive)
  (package-initialize)
  (unless (my-elpa-pkgs-installed-p)
    (message "%s" "Refreshing ELPA database...")
    (package-refresh-contents)
    (message "%s" " done.")
    (dolist (pkg my-elpa-pkgs)
      (when (not (package-installed-p pkg))
        (package-install pkg)))))

(defvar my-elpa-pkgs
  '(expand-region
    gist
    git-commit-mode
    gitconfig-mode
    gitignore-mode
    go-mode
    google-c-style
    ido-ubiquitous
    magit
    markdown-mode
    multiple-cursors
    protobuf-mode
    regex-tool
    scala-mode
    yasnippet
    zenburn-theme))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package gist
  :bind ("C-c G" . gist-region-or-buffer))

(use-package git-commit-mode)

(use-package gitignore-mode)

(use-package gitconfig-mode)

(use-package go-mode
  :mode ("\\.go$" . go-mode)
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

(use-package google-c-style)

(use-package ido-ubiquitous)

(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (progn
    (setenv "GIT_PAGER" "")

    (add-hook 'magit-log-edit-mode-hook
              #'(lambda ()
                  (set-fill-column 72)
                  (auto-fill-mode)
                  (flyspell-mode)))))

(use-package markdown-mode
  :mode ("\\.\\(markdown\\|mdown\\|md\\)$" . markdown-mode)
  :config
  (progn
    (let ((preferred-markdown-impl "peg-markdown"))
      (when (executable-find preferred-markdown-impl)
        (setq markdown-command preferred-markdown-impl)))

    (add-hook 'markdown-mode-hook
              #'(lambda ()
                  (auto-fill-mode 1)))))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package protobuf-mode
  :mode ("\\.proto$" . protobuf-mode))

(use-package regex-tool)

(use-package scala-mode
  :mode ("\\.scala$" . scala-mode))

(use-package yasnippet)


;;;; End of Initialization
;;; Run emacs server for GUI instance.
(when window-system (server-start))
