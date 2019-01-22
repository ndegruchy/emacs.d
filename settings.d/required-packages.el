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

(use-package avy
  :ensure t)

(use-package dired-atool
  :ensure t
  :config
  (dired-atool-setup))

(use-package dired-rsync
  :ensure t
  :config
  (bind-key "y" 'dired-rsync dired-mode-map))

(use-package embrace
  :ensure t)

(use-package expand-region
  :ensure t)

(use-package fish-mode
  :ensure t)

(use-package flycheck
  :ensure t
  :config
  (flycheck-define-checker proselint
    "A linter for prose."
    :command ("proselint" source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
              (id (one-or-more (not (any " "))))
              (message) line-end))
    :modes (text-mode markdown-mode gfm-mode message-mode latex-mode))
  
  (add-to-list 'flycheck-checkers 'proselint))

(use-package ido-vertical-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

(use-package no-littering
  :ensure t
  :config
  (setq no-littering-etc-directory
	(expand-file-name "etc.d/" user-emacs-directory)))

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
  :config
  (which-key-mode))

(use-package windresize
  :ensure t
  :bind ("C-c ;" . windresize))

(use-package yaml-mode
  :ensure t)
