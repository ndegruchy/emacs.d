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

(use-package consult
	:ensure t
	:bind (("C-x M-:" 	. consult-complex-command)
			  ("C-x b"   	. consult-buffer)
			  ("C-x 4 b" 	. consult-buffer-other-window)
			  ("C-x 5 b" 	. consult-buffer-other-frame)
			  ("C-x r b" 	. consult-bookmark)
			  ("M-y"     	. consult-yank-pop)
			  ("<help> a" 	. consult-apropos)
			  ("M-s l"		. consult-line))
	:hook (completion-list-mode . consult-preview-at-point-mode)
	:init
	(setq register-preview-delay 1
		register-preview-function #'consult-register-format))

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

(use-package embark
	:ensure t
	:bind (:map minibuffer-local-map
			  ("C-," . embark-export)
			  ("C-z" . embark-become))
	:config
	(setq embark-confirm-act-all nil)
	(add-to-list 'display-buffer-alist
		'("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
			 nil
			 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
	:ensure t
	:after (embark consult)
	:demand t
	:hook (embark-collect-mode . consult-preview-at-point-mode))

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
	;; :ensure t
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

(use-package fontaine
	:ensure t
	:bind (("C-c f" . fontaine-set-preset))
	:config
	(setq fontaine-presets
		'((normal
			  :default-family "Ioveska"
			  :default-weight normal
			  :default-height 170)
			 (presentation
				 :default-family "Ioveska"
				 :default-weight normal
				 :default-height 300))))

;; (use-package helm
;; 	:ensure t
;; 	:demand t
;; 	:diminish t
;; 	:bind (("M-x" . helm-M-x)
;; 			  ("<menu>" . helm-M-x)
;; 			  ("C-x C-f" . helm-find-files)
;; 			  ("C-x b" . helm-buffers-list)
;; 			  ("M-y" . helm-show-kill-ring)
;; 			  ("C-h a" . helm-apropos))
;; 	:config
;; 	(helm-mode 1)
;; 	(setq helm-move-to-line-cycle-in-source t
;; 		helm-M-x-fuzzy-match t
;; 		helm-buffers-fuzzy-matching t
;; 		helm-recentf-fuzzy-match	t)
;; 	(setq history-delete-duplicates t
;; 		history-length 20))

;; (use-package helm-swoop
;; 	:ensure t
;; 	:after helm
;; 	:bind (("C-s" . helm-swoop))
;; 	:config
;; 	(setq helm-swoop-pre-input-function (lambda () "")))

(use-package lin
	:ensure t
	:config
	(setq lin-face 'lin-blue
		lin-mode-hooks '(dired-mode-hook
							proced-mode-hook))
	(lin-global-mode 1))

(use-package marginalia
	:ensure t
	:init
	(marginalia-mode))

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

(use-package orderless
	:ensure t
	:custom
	(completion-styles '(orderless basic))
	(completion-category-overrides '((file (styles basic partial-completion))))
	:config
	(setq orderless-matching-styles '(orderless-initialism ;; Use this for cool things like 'plp'
										 orderless-literal    ;; Things like EMMS need to be literal
										 orderless-regexp     ;; Last resort
										 )))

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

(use-package tmr
	:ensure t
	:bind (("C-c t l" . tmr-tabulated-view)
			  ("C-c t t" . tmr)
			  ("C-c t T" . tmr-with-description)))

(use-package vertico
	:ensure t
	:bind (:map vertico-map
			  ("RET" . vertico-directory-enter)
			  ("C-l" . vertico-directory-up))
	:init
	(vertico-mode)
	(setq vertico-cyle t))

(use-package web-mode
	:init
	(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

(use-package which-key
	:ensure t
	:config
	(which-key-mode))

(use-package windresize
	:ensure t
	:bind ("C-c r" . windresize))
