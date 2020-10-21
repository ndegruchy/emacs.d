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

(use-package browse-kill-ring
  :ensure t
  :bind ("C-c k" . browse-kill-ring))

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
  (setq ebdb-notmuch-auto-update-p 'existing))

(use-package embrace
  :ensure t
  :bind (("C-c E" . embrace-commander)
		 ("C-c e" . embrace-add)))

(use-package emmet-mode
  :hook (web-mode css-mode sgml-mode)
  :ensure t)

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
  (load-file "~/.emacs.d/site-lisp.d/notmuch-calendar-patch.el")
  (load-file "~/.emacs.d/site-lisp.d/notmuch-functions.el")
  (load-file "~/.emacs.d/site-lisp.d/notmuch-saved.el")
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

(use-package web-mode
  :config
  (emmet-mode 1)
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
