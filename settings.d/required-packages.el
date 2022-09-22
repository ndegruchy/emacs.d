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

;;; Still deciding if I want/need this
;; (use-package emmet-mode
;; 	:ensure t
;; 	:hook ((sgml-mode . emmet-mode)
;; 			  (css-mode	. emmet-mode)
;; 			  (web-mode . emmet-mode)))

(use-package emms
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
	(require 'emms-info-native)
	(emms-all)
	(emms-mode-line 1)
	:config
	(setq emms-info-functions '(emms-info-native)
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

(use-package lin
	:ensure t
	:config
	(setq lin-face 'lin-blue
		lin-mode-hooks '(dired-mode-hook
							proced-mode-hook
							vc-dir-mode-hook
							package-menu-mode-hook))
	(lin-global-mode 1))

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
	:ensure t
	:bind ("C-c p" . titlecase-dwim))

(use-package which-key
	:ensure t
	:config
	(which-key-mode))

(use-package windresize
	:ensure t
	:bind ("C-c r" . windresize))
