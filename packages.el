;;; packages.el --- External packages configured via `use-package'

;;; Commentary:
;; - Sort alphabetically.
;; - Define common functions in `bdd-defuns.el'
;; - Define package specific functions under :init or :config

(require 'package)

;;; Code:
(setq package-archives
      (append package-archives
              '(("melpa" . "http://melpa.milkbox.net/packages/"))
              '(("org" . "http://orgmode.org/elpa/"))))

(package-initialize)
(setq package-enable-at-startup nil)

;; If never connected to repositories before, download package descriptions so
;; `use-package' can trigger installation of missing packages.
(unless package-archive-contents
  (message "Refreshing ELPA package archives...")
  (package-refresh-contents))

;; ...but before everything, make sure `use-package' is installed.
(unless (package-installed-p 'use-package)
  (message "`use-package' not found.  Installing...")
  (package-install 'use-package))

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

(use-package aurora-config-mode
  :ensure t
  :defer t)

(use-package browse-kill-ring
  :ensure t
  :defer t
  :config
  (browse-kill-ring-default-keybindings))

(use-package color-theme-sanityinc-tomorrow
  :ensure t)

(use-package dash-at-point
  :ensure t
  :defer t)

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
    (add-hook 'edit-server-start-hook 'spell-check-and-wrap-at-80)))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package fill-column-indicator
  :ensure t
  :defer t
  :init
  (hook-into-modes 'fci-mode '(prog-mode-hook)))

(use-package flx-ido
  :ensure t
  :defer t
  :init
  (flx-ido-mode 1)
  :config
  (progn
    (setq gc-cons-threshold (* 20 (expt 2 20)) ; megabytes
          ido-use-faces nil)))

(use-package flycheck
  :ensure t
  :defer t
  :init
  (hook-into-modes 'flycheck-mode '(prog-mode-hook)))

(use-package gist
  :ensure t
  :bind ("C-c g p" . gist-region-or-buffer-private)
  :config
  (setq gist-view-gist t))

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
  (defun go-capitalize-previous-word ()
    (interactive)
    (backward-word)
    (capitalize-word 1))

  :config
  (progn
    (bind-key "C-c C-c" 'go-capitalize-previous-word go-mode-map)
    (bind-key "C-c f" 'gofmt go-mode-map)
    (bind-key "C-c d" 'godoc go-mode-map)))

(use-package google-c-style
  :ensure t
  :defer t)

(use-package goto-chg
  :ensure t
  :defer t
  :bind (("C-." . goto-last-change)
         ("C-," . goto-last-change-reverse)))

(use-package guide-key
  :ensure t
  :config
  (progn
    (setq guide-key/popup-window-position 'bottom
          guide-key/guide-key-sequence t  ; enable for all prefixes
          guide-key/recursive-key-sequence-flag t)

    (defun guide-key/org-mode-hook ()
      (guide-key/add-local-highlight-command-regexp "org-"))
    (add-hook 'org-mode-hook 'guide-key/org-mode-hook)

    (guide-key-mode 1)))

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
    (setq magit-completing-read-function 'magit-ido-completing-read)))

(use-package markdown-mode
  :ensure t
  :mode ("\\.\\(m\\(ark\\)?down\\|md\\)$" . markdown-mode)
  :config
  (progn
    (let ((preferred-markdown-impl "peg-markdown"))
      (when (executable-find preferred-markdown-impl)
        (setq markdown-command preferred-markdown-impl)))

    (add-hook 'markdown-mode-hook 'spell-check-and-wrap-at-80)))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  :config
  (setq mc/list-file (emacs-d "var/multiple-cursors-all-or-once.el")))

(use-package mustache-mode
  :ensure t
  :defer t)

(use-package org
  :ensure t
  :defer t
  :init
  (setq org-replace-disputed-keys t
        org-default-notes-file (expand-file-name "notes.org" (getenv "HOME")))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((awk . t)
     (emacs-lisp . t)
     (java . t)
     (python . t)
     (ruby . t)
     (sh . t))))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (progn
    (setq projectile-enable-caching t
          projectile-known-projects-file (emacs-d "var/projectile-bookmarks.eld"))
    (projectile-global-mode)))

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

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)))

(use-package smex
  :ensure t
  :defer t
  :bind ("M-x" . smex))

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

(provide 'packages)

;;; packages.el ends here
