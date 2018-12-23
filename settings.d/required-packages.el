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

(use-package ace-jump-mode
  :ensure t)

(use-package embrace
  :ensure t)

(use-package expand-region
  :ensure t)

;; Joke package, especially for the holidays...
(use-package fireplace
  :ensure t)

(use-package fish-mode
  :ensure t)

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
	'(("Okular" "/usr/bin/okular %o")))
  (setq TeX-view-program-selection
        (quote
         (((output-dvi style-pstricks)
           "dvips and gv")
          (output-dvi "xdvi")
          (output-pdf "Okular")
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
