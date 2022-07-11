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
;; (add-to-list 'package-archives '("melpa-stable-mirror" . "https://www.mirrorservice.org/sites/stable.melpa.org/packages/"))

(setq package-archive-priorities
	  '(("melpa-stable" . 20)
		("nongnu" . 15)
		("melpa" . 1))) ;; Sets download priority, higher = more likely


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
  (set-face-attribute 'circe-my-message-face nil :background "transparent")
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
		 (css-mode  . emmet-mode)
		 (web-mode  . emmet-mode)
		 (php-mode  . emmet-mode)))

(use-package expand-region
  :ensure t
  :bind ("C-c s" . er/expand-region))

(use-package helm
  :ensure t
  :demand t
  :diminish t
  :bind (("M-x" . helm-M-x)
		 ("<menu>" . helm-M-x)
		 ("C-x C-f" . helm-find-files)
		 ("C-x b" . helm-buffers-list)
		 ("M-y" . helm-show-kill-ring)
		 ("C-x k" . kill-this-buffer))
  :config
  (helm-mode 1)
  (setq helm-move-to-line-cycle-in-source t
		helm-M-x-fuzzy-match t
		helm-buffers-fuzzy-matching t
		helm-recentf-fuzzy-match    t)
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

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (setq web-mode-markup-indent-offset 4
		web-mode-css-indent-offset 4
		web-mode-code-indent-offset 4))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package windresize
  :ensure t
  :bind ("C-c r" . windresize))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))
