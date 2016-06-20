;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;

(require 'package)

;;;;;;;;;;;;;
;; Sources ;;
;;;;;;;;;;;;;

;; Make MELPA the default and only
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; If we're running less than emacs 24, load the gnu archives as well
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;;;;;;;;;;;;;;;;;;;;;;
;; Package Fetching ;;
;;;;;;;;;;;;;;;;;;;;;;

(package-initialize)

;; Refresh package contents, so that we don't get the errors of things
;; not being available. This only happens when we haven't loaded the
;; package list before
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

(use-package no-littering
  :ensure t)

(use-package async
  :ensure t)

(use-package coffee-mode
  :ensure t)

(use-package dired+
  :ensure t)

(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook  'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode)
  (add-hook 'sgml-mode-hook 'toggle-truncate-lines)
  (add-hook 'web-mode-hook  'toggle-truncate-lines)
  (add-hook 'php-mode-hook  'toggle-truncate-lines)
  (setq emmet-preview-default t))

(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    "b"   'bs-show
    "g s" 'magit-status
    "d"   'ndegruchy/insert-date))

(use-package evil
  :ensure t
  :after evil-leader
  :diminish undo-tree-mode
  :config
  (evil-mode 1)
  ;; Loop through a list of buffer modes to set their various states
  (loop for (mode . state) in '((shell-mode . insert)
                                (eshell-mode . emacs)
                                (git-commit-mode . insert)
                                (git-rebase-mode . emacs)
                                (help-mode . emacs)
                                (grep-mode . emacs)
                                (bc-menu-mode . emacs)
                                (bs-mode . emacs)
                                (magit-branch-manager-mode . emacs)
                                (dired-mode . emacs)
                                (gomoku-mode . emacs)
                                (pong-mode . emacs)
                                (5x5-mode . emacs)
                                (blackbox-mode . emacs)
                                (hanoi-mode . emacs)
                                (landmark-mode . emacs)
                                (life-mode . emacs)
                                (snake-mode . emacs)
                                (solitaire-mode . emacs)
                                (tetris-mode . emacs)
                                (dunnet-mode . emacs)
                                (bubbles-mode . emacs)
                                (artist-mode . emacs)
                                (makey-key-mode . emacs)
                                (wdired-mode . normal))
        do (evil-set-initial-state mode state)))

(use-package evil-args
  :ensure t)

(use-package evil-numbers
  :ensure t
  :config
  (define-key evil-normal-state-map (kbd "<kp-add>") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "<kp-subtract>") 'evil-numbers/dec-at-pt))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-matchit
  :ensure t
  :config
  (global-evil-matchit-mode 1))

;; Since I use FISH as my preferred shell, I have to
;; have Emacs parse the $PATH in a different way
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package fish-mode
  :ensure t)

(use-package graphviz-dot-mode
  :ensure t)

(use-package ido-vertical-mode
  :ensure t)

(use-package iedit
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package magit
  :ensure t
  :bind ("C-c g s" . magit-status)
  :config
  (define-key magit-mode-map "e" nil)
  (define-key magit-mode-map "E" nil)
  (setq magit-commit-arguments (quote ("--gpg-sign=nathan@degruchy.org"))))

(use-package markdown-mode
  :ensure t
  :config
  (autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(use-package sass-mode
  :ensure t)

(use-package scss-mode
  :ensure t)

(use-package smartparens
  :ensure t)

(use-package smex
  :ensure t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command)))

(use-package systemd
  :ensure t)

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

(use-package web-mode
  :ensure t
  :config
  ;; TODO: add some leader shortcuts for common web functions, like
  ;; rename and wrap with tag
  (evil-leader/set-key
    "w w" 'web-mode-element-wrap
    "w r" 'web-mode-element-rename) 
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  :diminish "")

(use-package yaml-mode
  :ensure t)

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

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

(use-package flyspell
  :diminish flyspell-mode)

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
  ;; Saves your place in a file
  :config
  (setq-default save-place t))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style    'reverse
        uniquify-separator            "/"
        uniquify-after-kill-buffer-p  t
        uniquify-ignore-buffers-re    "^\\*"))

(use-package zone
  :config
  (zone-when-idle 120))
