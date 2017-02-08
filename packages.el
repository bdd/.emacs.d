;;; packages.el --- External packages configured via `use-package'

;;; Commentary:
;; - Sort alphabetically.
;; - Define common functions in `bdd-defuns.el'
;; - Define package specific functions under :init or :config

(require 'package)

;;; Code:
(setq package-archives
      (append package-archives
              '(("melpa" . "http://melpa.milkbox.net/packages/"))))

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
(setq use-package-minimum-reported-time 0
      use-package-verbose t)


(use-package ag
  :ensure t
  :defer t
  :bind (:map ag-mode-map
              ("p" . compilation-previous-error)
              ("n" . compilation-next-error)
              ("N" . compilation-next-file)
              ("P" . compilation-previous-file))
  :config
  (setq ag-highlight-search t))

(use-package company
  :ensure t
  :pin melpa
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-c-headers
  :ensure t
  :init
  (add-to-list 'company-backends 'company-c-headers))

(use-package company-flx
  :ensure t
  :init
  (with-eval-after-load 'company (company-flx-mode +1)))

(use-package company-shell
  :ensure t
  :init
  (add-to-list 'company-backends 'company-shell))

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (load-theme 'sanityinc-tomorrow-eighties t))

(use-package diminish
  :ensure t
  :config
  (progn
    (eval-after-load "whitespace" '(diminish 'whitespace-mode))))

(use-package exec-path-from-shell
  :ensure t
  :if window-system
  :config
  (progn
    (exec-path-from-shell-initialize)
    (message "%s: %s" "exec-path-from-shell post config" (getenv "PATH"))))

(use-package expand-region
  :ensure t
  :bind ("C-@" . er/expand-region))

(use-package fill-column-indicator
  :ensure t
  :defer t
  :init
  (hook-into-modes 'fci-mode '(prog-mode-hook)))

(use-package flx-ido
  :ensure t
  :init
  (progn
    (setq gc-cons-threshold (* 20 (expt 2 20)) ; megabytes
          ido-use-faces nil))
  :config
  (flx-ido-mode 1))

(use-package flycheck
  :ensure t
  :defer t
  :init
  (hook-into-modes 'flycheck-mode '(prog-mode-hook)))

(use-package fzf
  :ensure t)

(use-package gist
  :ensure t
  :bind ("C-c G p" . gist-region-or-buffer-private)
  :config
  (setq gist-view-gist t))

(use-package gitconfig-mode
  :ensure t
  :defer t)

(use-package gitignore-mode
  :ensure t
  :defer t)

(use-package google-c-style
  :ensure t
  :defer t)

(use-package goto-chg
  :ensure t
  :bind (("C-c ." . goto-last-change)
         ("C-c ," . goto-last-change-reverse)))

(use-package guide-key
  :ensure t
  :diminish guide-key-mode
  :config
  (progn
    (setq guide-key/popup-window-position 'bottom
          guide-key/guide-key-sequence t  ; enable for all prefixes
          guide-key/recursive-key-sequence-flag t)

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
  :bind ("C-c g g" . magit-status)
  :config
  (progn
    (setenv "GIT_PAGER" "")
    (setq magit-completing-read-function 'magit-ido-completing-read)))

(use-package markdown-mode
  :ensure t
  :mode ("\\.\\(m\\(ark\\)?down\\|md\\)$" . markdown-mode)
  :init
  (add-hook 'markdown-mode-hook 'spell-check-and-wrap-at-80)
  :config
  (progn
    (let ((preferred-markdown-impl "peg-markdown"))
      (when (executable-find preferred-markdown-impl)
        (setq markdown-command preferred-markdown-impl)))))

(use-package multiple-cursors
  :ensure t
  :bind (("C-c C-c >" . mc/edit-lines)
         ("C-c C-c <" . mc/mark-all-like-this)
         ("C-c >" . mc/mark-next-like-this)
         ("C-c <" . mc/mark-previous-like-this))
  :config
  (setq mc/list-file (emacs-d "var/multiple-cursors-all-or-once.el")))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (setq projectile-enable-caching t
        projectile-cache-file (emacs-d "var/projectile.cache")
        projectile-known-projects-file (emacs-d "var/projectile-bookmarks.eld")))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)))

(use-package regex-tool
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
  :bind ("M-x" . smex))
  :init
  (progn
    (setq smex-save-file (emacs-d "var/smex-items")))

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
