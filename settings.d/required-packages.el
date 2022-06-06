;;; required-packages.el
;;; These packages are required and will be loaded on launch of Emacs

(require 'package)

;; Package sources
;; List repositories to download files from

(add-to-list 'package-archives
			 '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives
			 '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(add-to-list 'package-archives
			 '("melpa" . "https://melpa.org/packages/") t)

(setq package-archive-priorities
	  '(("melpa-stable" . 20)
		("nongnu" . 15)
		("melpa" . 1))) ;; Sets download priority, higher = more likely

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

;;; (Package list)

(use-package bind-key
  :ensure t
  :after use-package)

(use-package circe
  :ensure t
  :bind ("C-c l" . circe)
  :config
  (setq circe-reduce-lurker-spam t
		circe-network-options
		`(("Libera"
		   :host "irc.libera.chat"
		   :server-buffer-name "Libera.Chat"
		   :port (6667 . 6697)
		   :use-tls t
		   :nick "ndegruchy"
		   :user "ndegruchy"
		   :sasl-username "ndegruchy"
		   :sasl-password ,circe-libera-password
		   :channels (:after-auth
					  "#emacs"
					  "#linux"
					  "#debian"
					  "#firefox"
					  "#kde"))))
  (set-face-attribute 'circe-my-message-face nil :background "transparent")
  (set-face-attribute 'circe-my-message-face nil :foreground "tomato")

  (defun ndegruchy/circe-switch-to-buffer ()
	"Create a buffer list for helm that contain only circe buffers using the function below"
	(interactive)
	(let ((helm-source-buffers-list ndegruchy/circe-buffers-source))
      (helm-buffers-list)))
  
  (setq ndegruchy/circe-buffers-source
		(helm-make-source "Circe Buffers" 'helm-source-buffers
		  :buffer-list
		  (lambda ()
			(mapcar #'buffer-name
					(cl-remove-if-not
					 (lambda (buf)
					   (with-current-buffer buf
						 (derived-mode-p 'lui-mode)))
					 (buffer-list))))))
  
  (bind-keys ("C-c c b" . ndegruchy/circe-switch-to-buffer)))

(use-package diminish
  :ensure t
  :after use-package)

(use-package editorconfig
  :ensure t
  :diminish t
  :config
  (editorconfig-mode 1))

(use-package embrace
  :ensure t
  :bind (("C-c E" . embrace-commander)
		 ("C-c e" . embrace-add)))

(use-package emmet-mode
  :ensure t
  :diminish t
  :hook ((sgml-mode . emmet-mode)
		 (css-mode . emmet-mode)
		 (php-mode . emmet-mode)))

(use-package emms
  :ensure t
  :bind (("C-c x b" . emms-smart-browse)
		 ("C-c x p" . emms-pause)
		 ("C-c x N" . emms-next)
		 ("C-c x P" . emms-previous)
		 ("C-c x s" . emms-stop))
  :init
  (require 'emms-setup)
  (emms-all)
  :config
  (setq emms-source-file-default-directory (concat (getenv "HOME") "/Music")
		emms-info-asynchronosly t
		emms-show-format "%s")
  (if (executable-find "cvlc")
	  (setq emms-player-list '(emms-player-vlc))
	(emms-default-players)))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "SSH_AGENT_PID")
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

(use-package expand-region
  :ensure t
  :bind ("C-c s" . er/expand-region))

(use-package helm
  :ensure t
  :demand t
  :diminish t
  :bind (("M-x" . helm-M-x)
		 ("C-x C-f" . helm-find-files)
		 ("C-x b" . helm-buffers-list)
		 ("M-y" . helm-show-kill-ring))
  :config
  (helm-mode 1)
  (setq helm-move-to-line-cycle-in-source t
		helm-M-x-always-save-history t
		helm-M-x-fuzzy-match t
		helm-buffers-fuzzy-matching t
		helm-recentf-fuzzy-match    t))

(use-package helm-swoop
  :ensure t
  :after helm
  :bind (("C-s" . helm-swoop)))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "pandoc"))

(use-package modus-themes
  :ensure t)

(use-package no-littering
  :ensure t
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(use-package systemd
  :ensure t)

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  (add-to-list 'hippie-expand-try-functions-list 'yas/hippie-try-expand))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package windresize
  :ensure t
  :bind ("C-c r" . windresize))
