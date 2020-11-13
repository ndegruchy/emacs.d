;; required-packages.el
;; These packages are required and will be loaded on launch of Emacs

(require 'package)

;; Package sources
;; List repositories to download files from

(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
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

(use-package async
  :config
  (dired-async-mode 1))

(use-package bind-key
  :after (use-package))

(use-package dired+
  :load-path "~/.emacs.d/site-lisp.d/"
  :after dired)

(use-package dired-x
  :after dired)

(use-package ebdb
  :ensure t
  :init
  (require 'ebdb-message)
  (require 'ebdb-notmuch)
  :config
  (setq ebdb-notmuch-auto-update-p 'existing
		ebdb-default-country "United States"
		ebdb-use-diary t
		ebdb-completion-display-record nil))

(use-package elfeed
  :bind ("C-c f" . elfeed)
  :config
  ;; Cleanup for current theme
  (set-face-attribute 'elfeed-search-filter-face nil :inherit 'header-line))

(use-package elpher
  :bind ("C-c ," . elpher)
  :load-path "~/.emacs.d/site-lisp.d/elpher/")

(use-package embrace
  :ensure t
  :bind (("C-c E" . embrace-commander)
		 ("C-c e" . embrace-add)))

(use-package emmet-mode
  :hook (web-mode css-mode sgml-mode)
  :ensure t)

(use-package emms
  :bind (("C-c b b" . emms-smart-browse)
		 ("C-c b l" . emms-pause)
		 ("C-c b s" . emms-stop)
		 ("C-c b n" . emms-next)
		 ("C-c b p" . emms-previous)
		 ("C-c b +" . emms-volume-mode-plus)
		 ("C-c b -" . emms-volume-mode-minus))
  :defer t
  :config
  (emms-all)
  (emms-standard)
  (emms-mode-line 1)
  (emms-playing-time-disable-display)
  (require 'emms-info-libtag)
  (setq emms-info-functions '(emms-info-libtag))
  (setq	emms-directory "~/Music"
		emms-source-file-default-directory "~/Music"
		emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find
		emms-cache-file "~/.emacs.d/var/emms/cache"
		emms-player-list '(emms-player-vlc)
		emms-volume-change-function 'emms-volume-pulse-change
		emms-info-auto-update t
		emms-librefm-scrobbler-enable t
		emms-librefm-scrobbler-username "ndegruchy"))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package expand-region
  :bind ("C-c s" . er/expand-region))

(use-package no-littering
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(use-package notmuch
  :bind (("C-c n" . notmuch)
		 :map notmuch-tree-mode-map
		 ("d" . nm/tree-delete)
		 ("t" . nm/tree-trash)
		 ("s" . nm/tree-sent)
		 :map notmuch-search-mode-map
		 ("d" . nm/search-delete)
		 ("t" . nm/search-trash)
		 ("s" . nm/search-sent))
  :hook (message-send-hook . message-sign-encrypt-if-all-keys-available)
  :hook (message-setup-hook . mml-secure-sign-pgpmime)
  :init
  (load-file "~/.emacs.d/site-lisp.d/notmuch/notmuch-calendar-patch.el")
  (load-file "~/.emacs.d/site-lisp.d/notmuch/notmuch-functions.el")
  (load-file "~/.emacs.d/site-lisp.d/notmuch/notmuch-saved.el")
  :config
  (setq mail-specify-envelope-from t
		mail-envelope-from 'header
		message-directory "~/.local/share/mail/"
		message-sendmail-envelope-from 'header
		sendmail-program "~/.local/bin/msmtpq"
		message-send-mail-function 'message-send-mail-with-sendmail
		message-kill-buffer-on-exit t
		notmuch-fcc-dirs "sent +sent -inbox -unread"
		notmuch-mua-compose-in 'new-frame
		notmuch-crypto-process-mime t
		notmuch-show-logo nil))

(use-package systemd)

(use-package undo-tree
  :ensure t
  :diminish
  :config
  (global-undo-tree-mode t))

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (setq web-mode-markup-indent-offset    4
		web-mode-css-indent-offset       4
		web-mode-code-indent-offset      4
		web-mode-enable-auto-pairing     t
		web-mode-enable-css-colorization t
		indent-tabs-mode                 t
		tab-width                        4))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package windresize
  :ensure t
  :bind ("C-c r" . windresize))
