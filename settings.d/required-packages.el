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

(use-package auctex
  :ensure t
  :defer t
  :config
  (TeX-view-program-selection
   (quote
    (((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Zathura")
     (output-html "xdg-open")))))

(use-package avy
  :ensure t
  :bind (("M-g M-g" . avy-goto-line)
	 ("M-g c"   . avy-goto-char)
	 ("M-g w"   . avy-goto-word-0)))

(use-package bind-key
  :ensure t
  :after (use-package))

(use-package dired+
  :after dired
  :load-path "~/.emacs.d/site-lisp.d/"
  :config
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$")))

(use-package editorconfig
  :ensure t
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
  :ensure t
  :bind ("C-c s" . er/expand-region))

(use-package fish-mode
  :ensure t)

(use-package ido-vertical-mode
  :ensure t)

(use-package muse
  :ensure t)

(use-package no-littering
  :ensure t)

(use-package systemd
  :ensure t)

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.sass?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.scss?\\'" . web-mode))

  (setq web-mode-markup-indent-offset 4
	web-mode-css-indent-offset 4
	web-mode-code-indent-offset 4
	web-mode-enable-auto-pairing t
	web-mode-enable-css-colorization t))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package windresize
  :ensure t
  :bind ("C-c r" . windresize))
