;; Package management on
(require 'package)

;; Make MELPA the default and only
;; TODO: Add orgmode?
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; If we're running less than emacs 24, load the gnu archives as well
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; Start package system
(package-initialize)

;; Refresh package contents, so that we don't get the errors of things
;; not being available. This only happens when we haven't loaded the
;; package list before
(unless package-archive-contents
  (package-refresh-contents))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Start `use-package' for later
(require 'use-package)

;; ================= Packages

(eval-when-compile
  (require 'use-package))

;; Built In


(use-package diminish)
(use-package bind-key)

(use-package no-littering
  :ensure t)

(use-package saveplace
  ;; Saves your place in a file
  :config
  (setq-default save-place t))

(use-package async)

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

(use-package dired+
  :ensure t)

(use-package ls-lisp)

(use-package browse-kill-ring
  :ensure t)

(use-package coffee-mode
  :ensure t)

(use-package cl
  :ensure t)

(use-package dash
  :ensure t)

(use-package abbrev
  :diminish abbrev-mode
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    "b"   'bs-show
    "g s" 'magit-status
    "d"   'ndegruchy/insert-date
    "w g" 'writegood-mode))

(use-package evil
  :ensure t
  :after evil-leader
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

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package fish-mode
  :ensure t)

(use-package git-commit
  :ensure t)

(use-package graphviz-dot-mode
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
  (define-key magit-mode-map "E" nil))

(use-package magit-find-file
  :ensure t)

(use-package magit-gitflow
  :ensure t)

(use-package magit-popup
  :ensure t)

(use-package makey
  :ensure t)

(use-package sass-mode
  :ensure t)

(use-package scss-mode
  :ensure t)

(use-package smartparens
  :ensure t)

(use-package systemd
  :ensure t)

(use-package sudo-edit
  :ensure t)

(use-package with-editor
  :ensure t)

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

(use-package yaml-mode
  :ensure t)

;; Since I use FISH as my preferred shell, I have to
;; have Emacs parse the $PATH in a different way
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; YaSnippet
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

;; IDO
(use-package ido-vertical-mode
  :ensure t)
(use-package ido
  :ensure t
  :config
  (ido-mode +1)
  (ido-vertical-mode 1)
  (setq ido-enable-flex-matching +1
        ido-everywhere +1
        ido-vertical-define-keys 'C-n-C-p-up-and-down
        ido-file-extensions-order '(".org" ".html" ".php" ".tex" ".el" ".js" ".coffee")))

;; Begrudgingly, because it's easier to edit this document WITH it...
(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           'enable-paredit-mode))

;; Emmet
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

;; Org Mode
(use-package org-mouse
  :config
  (add-hook 'org-mode-hook 'auto-fill-mode)
  (add-hook 'org-mode-hook 'electric-indent-mode)
  (add-hook 'org-mode-hook 'org-display-inline-images)
  (setq org-src-fontify-natively ()
        org-src-tab-acts-natively t
        org-support-shift-select t))

;; Markdown
(use-package markdown-mode
  :ensure t
  :config
  (autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

;; Uniquify
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style    'reverse
        uniquify-separator            "/"
        uniquify-after-kill-buffer-p  t
        uniquify-ignore-buffers-re    "^\\*"))

;; Rainbow Mode
(use-package rainbow-mode
  :ensure t
  :config
  (dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook lisp-mode))
    (add-hook hook 'rainbow-mode)))

;; Smex
(use-package smex
  :ensure t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command)))

(use-package zone
  :config
  (zone-when-idle 120))
