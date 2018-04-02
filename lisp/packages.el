;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;

(require 'package)

;;;;;;;;;;;;;
;; Sources ;;
;;;;;;;;;;;;;

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

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

(use-package ace-window
  :ensure t
  :bind (("M-p" . ace-window)))

(use-package async
  :ensure t)

(use-package avy
  :ensure t
  :bind (("C-c \"" .     avy-goto-char-2)
         ("C-c C-c \"" . avy-goto-line)))

(use-package base16-theme
  :ensure t
  :demand
  :config
  (load-theme 'base16-tomorrow-night t))

(use-package diminish
  :ensure t
  :config
  (diminish 'auto-revert-mode)
  (diminish flyspell-mode ""))

(use-package dired+)

(use-package dired-open
  :ensure t
  :config
  (setq dired-open-extensions
        '(("gif" . "mpv --loop=inf")
          ("webm" . "mpv --loop=inf")
          ("mp4" . "mpv --loop=inf")
          ("jpg" . "feh --auto-zoom --fullscreen --borderless --geometry 1920x1080 --image-bg black")
          ("jpeg" . "feh --auto-zoom --fullscreen --borderless --geometry 1920x1080 --image-bg black")
          ("png" . "feh --auto-zoom --fullscreen --borderless --geometry 1920x1080 --image-bg black"))))

(use-package elfeed
  :ensure t
  :config
  (setq elfeed-feeds
        '(("http://xkcd.com/rss.xml" comics)
          ("http://www.planet.emacsen.org/atom.xml" emacs)
          ("https://theintercept.com/feed/?lang=en" news)
          ("https://genji-cat.tumblr.com/rss" comics))))

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

;; (use-package erc
;;   :config
;;   (require 'tls)
;;   (erc-tls :server "irc.freenode.net" :port 6697
;;            :nick "ndegruchy" :full-name "Nathan DeGruchy")
;;   (setq erc-auto-join-channels-alist '(("frenode.net" "#emacs" "#archlinux" "#firefox"))
;;         erc-hide-list                '("JOIN" "PART" "QUIT"))
;;   (require 'erc-services)
;;   (erc-services-mode 1)
;;   (erc-spelling-mode 1))

(use-package expand-region
  :ensure t
  :bind (("C-c =" . er/expand-region)))

(use-package flycheck
  :ensure t)

(use-package fish-mode
  :ensure t)

(use-package grunt
  :ensure t
  :bind (("C-x C-c g" . grunt-exec)))

(use-package graphviz-dot-mode
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

(use-package nyan-mode
  :ensure t)

(use-package olivetti
  :ensure t)

(use-package pandoc-mode
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

(use-package tex-site
  :ensure auctex
  :config
  (setq TeX-view-program-list
        '(("MuPDF" "/usr/bin/mupdf %o")))
  (setq TeX-view-program-selection
        (quote
         (((output-dvi style-pstricks)
           "dvips and gv")
          (output-dvi "xdvi")
          (output-pdf "MuPDF")
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

(use-package abbrev
  :diminish abbrev-mode
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

(use-package bs
  :config
  ;; Whoo boy, this one was a hard one to track down. Basically I'm
  ;; telling buffer-show to always show a certain set of buffer names
  ;; regardless of the configuration (files only). Since I use eshell
  ;; and the scratch buffer a lot, this is handy for me to have always
  ;; visible
  ;;
  ;; Found from an ancient (2005) mailing list:
  ;; https://lists.gnu.org/archive/html/help-gnu-emacs/2005-10/msg00597.html
  (add-to-list 'bs-configurations
               '("ndegruchy" "\\*scratch\\*\\|\\*eshell\\*" nil
                 nil
                 bs-visits-non-file
                 bs--sort-by-name))
  (setq bs-default-configuration "ndegruchy"))

(use-package ido
  :config
  (ido-mode +1)
  (ido-vertical-mode 1)
  (setq ido-enable-flex-matching +1
        ido-everywhere +1
        ido-vertical-define-keys 'C-n-C-p-up-and-down
        ido-file-extensions-order '(".org" ".html" ".php" ".tex" ".el" ".js" ".coffee")))

(use-package org-mouse
  :config
  (add-hook 'org-mode-hook 'auto-fill-mode)
  (add-hook 'org-mode-hook 'electric-indent-mode)
  (add-hook 'org-mode-hook 'org-display-inline-images)
  (setq org-src-fontify-natively ()
        org-src-tab-acts-natively t
        org-support-shift-select t))

(use-package remember
  :config
  (setq remember-data-directory "~/.emacs.d/etc/remember"
        remember-data-file      "~/.emacs.d/etc/remember/notes")
  :bind (("C-c r" . remember)
         ("C-c l" . remember-notes)))

(use-package saveplace
  :config
  (setq-default save-place t))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style    'reverse
        uniquify-separator            "/"
        uniquify-after-kill-buffer-p  t
        uniquify-ignore-buffers-re    "^\\*"))
