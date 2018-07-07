;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;

(require 'package)

;;;;;;;;;;;;;
;; Sources ;;
;;;;;;;;;;;;;

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
			 '("melpa-stable" . "https://stable.melpa.org/packages/"))

(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;;;;;;;;;;;;;;;;;;;;;;
;; Package Fetching ;;
;;;;;;;;;;;;;;;;;;;;;;

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(eval-when-compile
  (require 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;
;; 3rd-party packages ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package async
  :ensure t)

(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-tomorrow-night 1))

(use-package diminish
  :ensure t
  :config
  (diminish 'auto-revert-mode)
  (diminish flyspell-mode ""))

(use-package dired
  :config
  (setq dired-listing-switches "-aBhl  --group-directories-first"))

(use-package dired+)

(use-package dired-open
  :ensure t
  :config
  (setq dired-open-extensions
        '(("gif"	.	"mpv --loop=inf")
          ("webm"	.	"mpv --loop=inf")
          ("mp4"	.	"mpv --loop=inf")
		  ("mkv"    .   "mpv --loop=inf")
		  ("pdf"	.	"zathura")
          ("jpg"	.	"feh --auto-zoom --fullscreen --borderless --geometry 1920x1080 --image-bg black --auto-rotate --draw-filename --no-menus")
          ("jpeg"	.	"feh --auto-zoom --fullscreen --borderless --geometry 1920x1080 --image-bg black --auto-rotate --draw-filename --no-menus")
          ("png"	.	"feh --auto-zoom --fullscreen --borderless --geometry 1920x1080 --image-bg black --auto-rotate --draw-filename --no-menus"))))

(use-package dired-single
  :ensure t)

(use-package dired-atool
  :ensure t
  :config
  (dired-atool-setup)
  (define-key dired-mode-map "z" #'dired-atool-do-unpack)
  (define-key dired-mode-map "Z" #'dired-atool-do-pack))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode)
  (add-hook 'sgml-mode-hook 'toggle-truncate-lines)
  (setq emmet-preview-default t))

(use-package embrace
  :ensure t
  :bind (("C-c ," . embrace-commander)))

(use-package expand-region
  :ensure t
  :bind (("C-c =" . er/expand-region)))

(use-package flycheck
  :ensure t)

(use-package fish-mode
  :ensure t)

(use-package grunt
  :ensure t)

(use-package hungry-delete
  :ensure t
  :diminish hungry-delete-mode
  :config
  (global-hungry-delete-mode))

(use-package ido-completing-read+
  :ensure t)

(use-package ido-vertical-mode
  :ensure t)

(use-package iedit
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package magit
  :ensure t
  :pin melpa-stable
  :bind ("C-c C-c g" . magit-status)
  :config
  (define-key magit-mode-map "e" nil)
  (define-key magit-mode-map "E" nil)
  (when (file-exists-p "~/.gnupg/pubring.kbx")
    (setq magit-commit-arguments (quote ("--gpg-sign=nathan@degruchy.org")))))

(use-package markdown-mode
  :ensure t
  :config
  (setq markdown-command "pandoc"))

(use-package no-littering
  :ensure t)

(use-package olivetti
  :ensure t)

(use-package sass-mode
  :ensure t)

(use-package scss-mode
  :ensure t)

(use-package smartparens
  :ensure t)

(use-package smex
  :ensure t
  :config
  (smex-initialize)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command)))

(use-package systemd
  :ensure t
  :pin melpa-stable)

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
  (which-key-mode)
  :diminish "")

(use-package yaml-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;
;; Built-in packages ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Using Ido-Switch-Buffers for now, seems less complicated.
;; (use-package bs
;;   :bind (("C-c b" . bs-show))
;;   :config
;;   ;; Whoo boy, this one was a hard one to track down. Basically I'm
;;   ;; telling buffer-show to always show a certain set of buffer names
;;   ;; regardless of the configuration (files only). Since I use eshell
;;   ;; and the scratch buffer a lot, this is handy for me to have always
;;   ;; visible
;;   ;;
;;   ;; Found from an ancient (2005) mailing list:
;;   ;; https://lists.gnu.org/archive/html/help-gnu-emacs/2005-10/msg00597.html
;;   (add-to-list 'bs-configurations
;;                '("ndegruchy" "\\*scratch\\*\\|\\*eshell\\*"
;; 				 nil
;;                  nil
;;                  bs-visits-non-file
;;                  bs--sort-by-name))
;;   (add-to-list 'bs-configurations
;; 			   '("dired" nil nil nil
;; 				 (lambda (buf)
;; 				   (with-current-buffer buf
;; 					 (not (eq major-mode 'dired-mode))))
;; 				 bs--sort-by-name))
;;   (setq bs-default-configuration "ndegruchy"))

(use-package ido
  :config
  (ido-mode +1)
  (ido-vertical-mode 1)
  (setq ido-enable-flex-matching +1
        ido-everywhere +1
        ido-vertical-define-keys 'C-n-C-p-up-and-down
        ido-file-extensions-order '(".org" ".html" ".php" ".tex" ".el" ".js" ".coffee")))

(use-package remember
  :config
  (setq remember-data-directory "~/.emacs.d/etc/remember"
        remember-data-file      "~/.emacs.d/etc/remember/notes")
  :bind (("C-c r" . remember)
         ("C-c l" . remember-notes)))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style    'reverse
        uniquify-separator            "/"
        uniquify-after-kill-buffer-p  t
        uniquify-ignore-buffers-re    "^\\*"))
