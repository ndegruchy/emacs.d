;; required-packages.el
;; These packages are required and will be loaded on launch of Emacs

(require 'package)

;; Package sources
;; List repositories to download files from

(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

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

(use-package ace-window
  :ensure t)

(use-package avy
  :ensure t)

(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-tomorrow-night 1))

(use-package embrace
  :ensure t)

(use-package expand-region
  :ensure t)

(use-package fish-mode
  :ensure t)

(use-package ido-vertical-mode
  :ensure t)

(use-package iedit
  :ensure t)

(use-package magit
  :ensure t
  :config
  (define-key magit-mode-map "e" nil)
  (define-key magit-mode-map "E" nil)
  (when (file-exists-p "~/.gnupg/pubring.kbx")
    (setq magit-commit-arguments (quote ("--gpg-sign=nathan@degruchy.org")))))

(use-package no-littering
  :ensure t
  :config
  (setq no-littering-etc-directory
	(expand-file-name "etc.d/" user-emacs-directory)
	no-littering-etc-directory
	(expand-file-name "var.d/" user-emacs-directory)))

(use-package smex
  :ensure t)

(use-package tex-site
  :ensure auctex
  :config
  (setq TeX-view-program-list
        '(("Zathura" "/usr/bin/zathura %o")))
  (setq TeX-view-program-selection
        (quote
         (((output-dvi style-pstricks)
           "dvips and gv")
          (output-dvi "xdvi")
          (output-pdf "Zathura")
          (output-html "xdg-open")))))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))
