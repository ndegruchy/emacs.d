;; required-packages.el
;; These packages are required and will be loaded on launch of Emacs

(require 'package)

;; Package sources
;; List repositories to download files from

(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
;; (add-to-list 'package-archives '("melpa-stable-mirror" . "https://www.mirrorservice.org/sites/stable.melpa.org/packages/"))

;; Fix for 26.2 elpa 'bad request' issue
(if (version<= emacs-version "26.2")
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; Fetch packages

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(eval-when-compile
  (require 'use-package))

;; Package list

(use-package async
  :config
  (dired-async-mode 1))

(use-package bind-key
  :after (use-package))

(use-package crontab-mode
  :load-path "~/.emacs.d/site-lisp.d/")

(use-package dired+
  :after dired
  :load-path "~/.emacs.d/site-lisp.d/"
  :config
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$")))

(use-package editorconfig
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

(use-package embrace
  :ensure t
  :after (expand-region)
  :bind (("C-c E" . embrace-commander)
	 ("C-c e" . embrace-add)))

(use-package emmet-mode
  :after web-mode
  :after sgml-mode
  :after css-mode
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package expand-region
  :bind ("C-c s" . er/expand-region))

(use-package no-littering
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(use-package smex
  :config
  (smex-initialize)
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands)
	 ("C-c ; M-x" . execute-extended-command)))

(use-package systemd)

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (setq web-mode-markup-indent-offset    4
	web-mode-css-indent-offset       4
	web-mode-code-indent-offset      4
	web-mode-enable-auto-pairing     t
	web-mode-enable-css-colorization t))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package windresize
  :ensure t
  :bind ("C-c r" . windresize))
