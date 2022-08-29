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
;; (add-to-list 'package-archives
;;			 '("melpa-stable-mirror" . "https://www.mirrorservice.org/sites/stable.melpa.org/packages/") t)

(setq package-archive-priorities
	'(("melpa-stable" . 20)
		 ("nongnu" . 15)
		 ("melpa" . 1))) ;; Sets download priority, higher = more likely

;; Fetch packages

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
	:config
	(setq circe-reduce-lurker-spam t
		lui-flyspell-p t
		circe-network-options
		`(("Libera Chat"
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
							"#momw"
							"#openmw"
							"#kde"))))
	(set-face-attribute 'circe-my-message-face nil :foreground "tomato")

	(with-eval-after-load 'circe
		(circe-set-display-handler "001" 'circe-display-ignore)
		(circe-set-display-handler "002" 'circe-display-ignore)
		(circe-set-display-handler "003" 'circe-display-ignore)
		(circe-set-display-handler "004" 'circe-display-ignore)
		(circe-set-display-handler "005" 'circe-display-ignore)
		(circe-set-display-handler "353" 'circe-display-ignore)
		(circe-set-display-handler "366" 'circe-display-ignore)))

(use-package diminish
	:ensure t)

(use-package editorconfig
	:ensure t
	:config
	(editorconfig-mode 1))

(use-package embrace
	:ensure t
	:bind (("C-c E" . embrace-commander)
			  ("C-c e" . embrace-add)))

(use-package emmet-mode
	:ensure t
	:hook ((sgml-mode . emmet-mode)
			  (css-mode	. emmet-mode)))

(use-package emms
	;; Having 'ensure t' here causes it to try and be downloaded from
	;; melpa, instead of using the system-provided package in debian
	;; package 'elpa-emms'
	:ensure t
	:bind (("C-c x b" . emms-smart-browse)
			  ("C-c x p" . emms-pause)
			  ("C-c x N" . emms-next)
			  ("C-c x P" . emms-previous)
			  ("C-c x s" . emms-stop))
	:init
	(require 'emms-setup)
	(require 'emms-mode-line)
	(require 'emms-player-vlc)
	(require 'emms-info-libtag)
	(emms-all)
	(emms-mode-line 1)
	:config
	(setq emms-info-functions '(emms-info-libtag)
		emms-source-file-default-directory (concat (getenv "HOME") "/Media/Music")
		emms-info-asynchronosly t
		emms-show-format "%s")
	(setq emms-browser-covers 'emms-browser-cache-thumbnail-async)
	(add-to-list 'emms-info-functions 'emms-info-libtag)

	(if (executable-find "cvlc")
		(setq emms-player-list '(emms-player-vlc))
		(emms-default-players))
	(if (executable-find "mpv")
		(setq emms-player-list '(emms-player-mpv))
		(emms-default-players)))

(use-package expand-region
	:ensure t
	:bind ("C-c s" . er/expand-region))

(use-package helm
	:ensure t
	:demand t
	;; :diminish t
	:bind (("M-x" . helm-M-x)
			  ("<menu>" . helm-M-x)
			  ("C-x C-f" . helm-find-files)
			  ("C-x b" . helm-buffers-list)
			  ("M-y" . helm-show-kill-ring)
			  ("C-h a" . helm-apropos))
	:config
	(helm-mode 1)
	(setq helm-move-to-line-cycle-in-source t
		helm-M-x-fuzzy-match t
		helm-buffers-fuzzy-matching t
		helm-recentf-fuzzy-match	t)
	(setq history-delete-duplicates t
		history-length 20))

(use-package helm-swoop
	:ensure t
	:after helm
	:bind (("C-s" . helm-swoop))
	:config
	(setq helm-swoop-pre-input-function (lambda () "")))

(use-package modus-themes
	:ensure t
	:init
	(load-theme 'modus-vivendi t))

(use-package no-littering
	:ensure t
	:config
	(require 'recentf)
	(add-to-list 'recentf-exclude no-littering-var-directory)
	(add-to-list 'recentf-exclude no-littering-etc-directory))

(use-package pulsar
	:ensure t
	:init
	(pulsar-global-mode)
	:config
	(setq pulsar-pulse-on-window-change t
		pulsar-pulse t
		pulsar-delay 0.055
		pulsar-iterations 10
		pulsar-face 'pulsar-magenta
		pulsar-highlight-face 'pulsar-yellow
		pulsar-pulse-functions
		'(recenter-top-bottom
			 move-to-window-line-top-bottom
			 scroll-up-command
			 scroll-down-command)))

(use-package titlecase
	:ensure t)

(use-package which-key
	:ensure t
	:config
	(which-key-mode))

(use-package windresize
	:ensure t
	:bind ("C-c r" . windresize))
