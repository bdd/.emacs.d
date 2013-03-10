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
;;; Theme
(load-theme 'tomorrow-night-eighties t)


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
(setq auto-save-default nil
      auto-save-list-file-prefix nil)


;;;; UI
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode (if window-system 1 0))
(set-fringe-style '(4 . 0)) ; 4px left fringe and no right fringe.


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
      ido-everywhere t
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
(global-set-key [remap goto-line] 'bdd-goto-line-with-feedback)
(define-key global-map (kbd "RET") 'newline-and-indent) ; was C-j

(bind-key "C-h" 'delete-backward-char) ; unixism
(bind-key "C-w" 'bdd-kill-region-or-backward-kill-word) ; more unixism
(bind-key "C-?" 'help-command) ; C-h is gone and <f1> is not really convenient
(bind-key "C-S-k" 'kill-whole-line)
(bind-key "C-j" 'join-line) ; more useful C-j
(bind-key "C-x C-b" 'ibuffer)
(bind-key "C-x C-d" 'dired)

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


;;;; Internal Packages
(setq org-babel-load-languages
      '((awk . t)
        (C . t)
        (emacs-lisp . t)
        (gnuplot . t)
        (java . t)
        (org . t)
        (perl . t)
        (python . t)
        (ruby . t)
        (sh . t)))


;;;; External Packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize nil) ; just load the list.  don't initialize.
(unless package-archive-contents ; never connected to an ELPA repository
  (progn
    (message "Refreshing ELPA package archives.")
    (package-refresh-contents)))

(use-package ag
  :ensure t
  :config
  (progn
    (setq ag-highlight-search t)
    (bind-key "n" 'compilation-next-error ag-mode-map)
    (bind-key "p" 'compilation-previous-error ag-mode-map)
    (bind-key "N" 'compilation-next-file ag-mode-map)
    (bind-key "P" 'compilation-previous-file ag-mode-map)))

(use-package browse-kill-ring
  :ensure t
  :config
  (progn
    (browse-kill-ring-default-keybindings)))

(use-package edit-server
  :ensure t
  :if window-system
  :init
  (progn
    (add-hook 'after-init-hook 'server-start t)
    (add-hook 'after-init-hook 'edit-server-start t))
  :config
  (progn
    (add-hook 'edit-server-start-hook
              #'(lambda ()
                  (set-fill-column 80)
                  (auto-fill-mode)
                  (flyspell-mode)))))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package fill-column-indicator
  :ensure t)

(use-package gist
  :ensure t
  :bind ("C-c G" . gist-region-or-buffer))

(use-package git-commit-mode
  :ensure t)

(use-package gitconfig-mode
  :ensure t)

(use-package gitignore-mode
  :ensure t)

(use-package go-mode
  :ensure t
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

(use-package google-c-style
  :ensure t)

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
              #'(lambda ()
                  (set-fill-column 72)
                  (auto-fill-mode)
                  (flyspell-mode)))))

(use-package markdown-mode
  :ensure t
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
  :mode ("\\.mustache" . mustache-mode))

(use-package protobuf-mode
  :ensure t
  :mode ("\\.proto$" . protobuf-mode))

(use-package puppet-mode
  :ensure t
  :mode ("\\.pp$" . puppet-mode))

(use-package regex-tool
  :ensure t)

(use-package ruby-mode
  :ensure t)

(use-package ruby-tools
  :ensure t)

(use-package scala-mode2
  :ensure t
  :mode ("\\.scala$" . scala-mode))

(use-package yaml-mode
  :ensure t
  :mode ("\\.\\(yml\\|yaml\\)$" . yaml-mode))

(use-package yasnippet
  :ensure t
  :init
  (progn
    (setq yas/prompt-functions '(yas/ido-prompt))))

;;--- Resize the window as a remedy to display bug leaving a char wide space next to right bar.
;;--- TODO: Find out the root cause of this bug.
(when window-system
      (set-frame-size (selected-frame) 100 30))
