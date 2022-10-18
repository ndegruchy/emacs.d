;;; required-packages.el
;;; These packages are required and will be loaded on launch of Emacs

(require 'package)

;; Package sources
;; List repositories to download files from

(add-to-list 'package-archives
			 '("stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives
			 '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(add-to-list 'package-archives
			 '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;			 '("melpa-stable-mirror" . "https://www.mirrorservice.org/sites/stable.melpa.org/packages/") t)

(setq package-archive-priorities
	  '(("stable" . 20)
		("nongnu" . 15)
		("melpa" . 1))) ;; Sets download priority, higher = more likely

;; Fetch required packages

(unless (package-installed-p 'ef-themes)
  (package-refresh-contents)
  (package-install 'ef-themes))

(require 'rec-mode)

(require 'windmove)
