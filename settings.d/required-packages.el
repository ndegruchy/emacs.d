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

(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-tomorrow-night t))

(use-package avy
  :bind (("M-g M-g" . avy-goto-line)
	 ("M-g c"   . avy-goto-char)
	 ("M-g w"   . avy-goto-word-0)))

(use-package bind-key
  :after (use-package))

(use-package counsel)

(use-package dired+
  :after dired
  :load-path "~/.emacs.d/site-lisp.d/"
  :config
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$")))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package embrace
  :ensure t
  :after (expand-region)
  :bind (("C-c E" . embrace-commander)
	 ("C-c e" . embrace-add)))

(use-package emmet-mode
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package expand-region
  :bind ("C-c s" . er/expand-region))

(use-package ivy
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
	ivy-wrap t
	ivy-extra-directories nil
	enable-recursive-minibuffers t)
  :bind (("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("<f1> f" . counsel-describe-function)
	 ("<f1> v" . counsel-describe-variable)
	 ("C-s" . swiper)
	 ("C-c C-r" . ivy-resume)
	 :map ivy-minibuffer-map
	 ("C-j" . ivy-immediate-done)
	 ("RET" . ivy-alt-done)))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package no-littering
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(use-package nov
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package olivetti)

(use-package systemd)

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.sass?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.scss?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php?\\'" . web-mode))

  (setq web-mode-markup-indent-offset 4
	web-mode-css-indent-offset 4
	web-mode-code-indent-offset 4
	web-mode-enable-auto-pairing t
	web-mode-enable-css-colorization t))

(use-package which-key
  :config
  (which-key-mode))

(use-package windresize
  :ensure t
  :bind ("C-c r" . windresize))

(use-package yaml-mode)
