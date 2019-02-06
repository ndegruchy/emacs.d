;; required-packages.el
;; These packages are required and will be loaded on launch of Emacs

(require 'package)

;; Package sources
;; List repositories to download files from

(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
;; (add-to-list 'package-archives '("melpa-stable-mirror" . "https://www.mirrorservice.org/sites/stable.melpa.org/packages/"))

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

(use-package embrace
  :ensure t
  :after (expand-region)
  :bind (("C-c E" . embrace-commander)
	 ("C-c e" . embrace-add)))

(use-package expand-region
  :ensure t
  :bind ("C-c s" . er/expand-region))

(use-package ido-vertical-mode
  :ensure t)

(use-package no-littering
  :ensure t
  :config
  (setq no-littering-etc-directory
	(expand-file-name "etc.d/" user-emacs-directory)))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package windresize
  :ensure t
  :bind ("C-c ;" . windresize))
