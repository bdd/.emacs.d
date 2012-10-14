;;;; Emacs Configuration
;;;; Berk D. Demir <bdd@mindcast.org>

(setq message-log-max 16384)

(defun emacs-d (fn)
  "Expand file name relative to user-emacs-directory."
  (expand-file-name fn user-emacs-directory))

;;; Load 'load-path.el' so we know where to load from.
(load (emacs-d "load-path"))
(load (emacs-d "bdd-defuns"))

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


;;;; Global Key Bindings
(define-key global-map (kbd "RET") 'newline-and-indent) ; was C-j

(bind-key "C-h" 'delete-backward-char) ; unixism.  use <f1> for help-command.
(bind-key "C-w" 'bdd-kill-region-or-backward-kill-word) ; more unixism.
(bind-key "C-S-k" 'kill-whole-line)
(bind-key "C-j" 'join-line) ; more useful C-j
(bind-key "C-x C-b" 'ibuffer)

;;; Window Movement
(bind-key "C-<return>" 'other-window)
;; Switch with arrows.
;; - Graphical: Command-<arrow key>
;; - Terminal : C-c <arrow key>
(let
    ((prefix (if window-system "s-" "C-c ")))
  (defun with-prefix (key)
    (concat prefix "<" key ">"))

  (bind-key (with-prefix "left") 'windmove-left)
  (bind-key (with-prefix "right") 'windmove-right)
  (bind-key (with-prefix "up") 'windmove-up)
  (bind-key (with-prefix "down") 'windmove-down))


;;;; Misc
;;; Whitespace
(set-default 'indicate-empty-lines t)
(set-default 'show-trailing-whitespace t)
(setq whitespace-style '(face empty tabs lines-tail tab-mark))
(setq whitespace-line-column nil) ; equals to fill-column
;(global-whitespace-mode t)

(setq indent-tabs-mode nil)
(setq tab-always-indent 'complete)
(show-paren-mode t)
(global-auto-revert-mode t)
(setq require-final-newline 'ask)

;;; Mouse
(when (boundp 'mouse-wheel-scroll-amount)
  (setq mouse-wheel-scroll-amount '(0.01)))

;;; Electric parans, braces, double quotes, etc.
(electric-pair-mode)


;;;; Mode Line
(setq size-indication-mode t
      line-number-mode t
      column-number-mode t)


;;;; Package Declerations

;;; Magit: A magical emacs mode for git.
;;; (git submodule)
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

;;; scala
;;; https://github.com/scala/scala-dist/tree/master/tool-support/src/emacs
(use-package scala-mode
  :mode ("\\.scala$" . scala-mode))

;;; go
;;; http://go.googlecode.com/hg/misc/emacs/go-mode.el
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

;;; expand-region: increase the selected region by semantic units.
;;; (git submodule)
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;;; protobuf: Google Protocol Buffers.
;;; http://protobuf.googlecode.com/svn/trunk/editors/protobuf-mode.el
(use-package protobuf-mode
  :mode ("\\.proto$" . protobuf-mode))

;;; gist: Create a GitHub Gist from region or buffer.
;;; (git submodule)
(use-package gist
  :bind ("C-c G" . gist-region-or-buffer))

;;; regex-tool: regexp eval tool.
;;; (git submodule)
(use-package regex-tool)

;;; erc: emacs irc client
;;; (git submodule)
(use-package erc)

;;; color-theme-buffer-local & load-theme-buffer-local
;;; load-theme-buffer-local: Per buffer color themes for Emacs 24.
;;; (git submodule)
(use-package load-theme-buffer-local)

;;; markdown-mode
;;; (git submodule)
(use-package markdown-mode
  :mode ("\\.\\(markdown\\|mdown\\|md\\)$" . markdown-mode)
  :init
  (progn
    (defun prefer-markdown-command (command)
      (when (executable-find command)
	(setq markdown-command command))))

  :config
  (progn
    (prefer-markdown-command "peg-markdown")
    (add-hook 'markdown-mode-hook
	      #'(lambda ()
		  (auto-fill-mode 1)))))

;;; multiple-cursors
;;; (git submodule)
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))


;;;; Emacs Managed Customizations
(setq custom-file (emacs-d "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))


;;;; End of Initialization
;;; Run emacs server for GUI instance.
(when window-system (server-start))
