;; required-packages.el
;; These packages are required and will be loaded on launch of Emacs

(require 'package)

;; Package sources
;; List repositories to download files from

(add-to-list 'package-archives
			 '("melpa-stable" . "https://stable.melpa.org/packages/")
			 '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
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

(use-package bind-key
  :ensure t
  :after use-package)

(use-package diminish
  :ensure t
  :after use-package)

(use-package embrace
  :ensure t
  :bind (("C-c E" . embrace-commander)
		 ("C-c e" . embrace-add)))

(use-package emmet-mode
  :ensure t
  :hook ((sgml-mode . emmet-mode)
		 (css-mode . emmet-mode)
		 (php-mode . emmet-mode)))

(use-package expand-region
  :ensure t
  :bind ("C-c s" . er/expand-region))

(use-package exec-path-from-shell
  :ensure t)

(use-package helm
  :ensure t
  :demand t
  :bind (("M-x" . helm-M-x)
		 ("C-x C-f" . helm-find-files)
		 ("C-x b" . helm-buffers-list)
		 ("M-y" . helm-show-kill-ring))
  :config
  (helm-mode 1))

(use-package no-littering
  :ensure t
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(use-package php-mode
  :ensure t)

(use-package systemd
  :ensure t)

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package windresize
  :ensure t
  :bind ("C-c r" . windresize))
