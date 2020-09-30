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

(use-package bbdb
  :init
  (bbdb-initialize)
  (bbdb-mua-auto-update-init)
  :config
  (setq bbdb-mua-pop-up t))

(use-package bind-key
  :after (use-package))

(use-package crontab-mode
  :load-path "~/.emacs.d/site-lisp.d/")

(use-package dired+
  :after dired
  :load-path "~/.emacs.d/site-lisp.d/"
  :config
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$")))

(use-package elfeed
  :bind ("C-c l" . elfeed))

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

(use-package hide-lines
  :load-path "~/.emacs.d/site-lisp.d/"
  :config
  (defadvice hide-lines-matching (around hide-line-clear activate)
    (if (equal search-text "")
		(hide-lines-show-all)
      ad-do-it)))

(use-package magit
  :ensure t)

(use-package magit-gitflow
  :ensure t
  :after magit)

(use-package magit-lfs
  :ensure t
  :after magit)

(use-package markdown-mode
  :init (setq markdown-command "pandoc")
  :mode (("README\\.md"    . gfm-mode)
		 ("\\.md\\'"       . markdown-mode)
		 ("\\.markdown\\'" . markdown-mode))
  :hook ((markdown-mode . auto-fill-mode)
		 (markdown-mode . flyspell-mode)))

(use-package no-littering
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(use-package notmuch
  :bind ("C-c n" . notmuch)
  :init
  (eval-after-load 'notmuch-show
	'(define-key notmuch-show-mode-map "`" 'notmuch-show-apply-tag-macro))
  (defun notmuch-show-apply-tag-macro (key)
	(interactive "k")
	(let ((macro (assoc key notmuch-show-tag-macro-alist)))
      (apply 'notmuch-show-tag-message (cdr macro))))
  :hook (message-send-hook . message-sign-encrypt-if-all-keys-available)
  :config
  (setq notmuch-show-tag-macro-alist
		(list
		 '("s-m r" "-unread")
		 '("s-m s" "+sent -inbox -unread")
		 '("s-m d" "+delete +deleted -unread")
		 '("s-m i" "+important")))
  (setq mail-specify-envelope-from t
		message-sendmail-envelope-from 'header
		mail-envelope-from 'header
		sendmail-program "/usr/sbin/sendmail"
		message-send-mail-function 'message-send-mail-with-sendmail
		message-directory "~/.local/share/mail/drafts/"
		notmuch-fcc-dirs "~/.local/share/mail/sent/"
		notmuch-crypto-process-mime t
		notmuch-saved-searches '((:name "inbox"
										:key "i"
										:query "tag:inbox"
										:count-query "tag:inbox"
										:sort-order newest-first
										:search-type tree)
								 (:name "unread"
										:key "u"
										:query "tag:unread"
										:count-query "tag:unread"
										:sort-order oldest-first)
								 (:name "sent"
										:key "s"
										:query "tag:sent"
										:count-query "tag:sent"
										:sort-order newest-first
										:search-type tree)
								 (:name "all"
										:key "a"
										:query "*"
										:count-query "*"
										:sort-order newest-first
										:search-type tree)
								 (:name "important"
										:key "p"
										:query "tag:important"
										:count-query "tag:important"
										:sort-order newest-first)))
  (add-hook 'message-setup-hook 'mml-secure-sign-pgpmime))

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
