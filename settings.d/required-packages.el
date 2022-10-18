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

;; Package archive priorities
;; Higher number = picked first
(setq package-archive-priorities
	  '(("stable" . 20) ;; Make stable the highest priority
		("nongnu" . 15) ;; Non-GNU has some good stuff
		("melpa" . 1))) ;; I don't care much for the unstable "latest" stuff

;; Fetch required packages

(unless (package-installed-p 'ef-themes)
  (package-refresh-contents)
  (package-install 'ef-themes))

(unless (package-installed-p 'windresize)
  (package-refresh-contents)
  (package-install 'windresize))

(require 'windresize)
(require 'rec-mode) ;; Loaded from site-lisp.d

