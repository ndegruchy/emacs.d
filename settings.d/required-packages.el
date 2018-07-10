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
  :ensure t
  :bind ("M-o" . ace-window))

(use-package avy
  :ensure t
  :bind (("C-:"		. avy-goto-char)
	 ("M-g g"	. avy-goto-line)
	 ("C-`"		. avy-isearch)
	 ("C-z"		. avy-zap-to-char)))

(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-tomorrow-night 1))

(use-package elfeed
  :ensure t
  ;; Feeds stored in `elfeed-settings.el'
  )

(use-package embrace
  :ensure t
  :bind ("C-," . embrace-commander))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package fish-mode
  :ensure t)

(use-package ido-vertical-mode
  :ensure t)

(use-package iedit
  :ensure t)

(use-package magit
  :ensure t
  :bind ("C-c C-c m s" . magit-status)
  :config
  (define-key magit-mode-map "e" nil)
  (define-key magit-mode-map "E" nil)
  (when (file-exists-p "~/.gnupg/pubring.kbx")
    (setq magit-commit-arguments (quote ("--gpg-sign=nathan@degruchy.org")))))

(use-package smex
  :ensure t
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands)
	 ("C-c C-c M-x" . execute-extended-command)))

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
