;;; required-packages.el --- External packages that need to be installed  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Nathan DeGruchy

;; Author: Nathan DeGruchy <nathan@degruchy.org>
;; Keywords: local, data, processes, convenience

(require 'package)

;; Package sources
;; List repositories to download files from

(add-to-list 'package-archives
			 '("elpa" . "https://elpa.gnu.org/packages/"))
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
	  '(("elpa"   . 25) ;; Make ELPA the highest priority
		("stable" . 20) 
		("nongnu" . 15) ;; Non-GNU has some good stuff
		("melpa" . 1))) ;; I don't care much for the unstable "latest" stuff

;; Fetch required packages

(unless (package-installed-p 'ef-themes)
  (package-refresh-contents)
  (package-install 'ef-themes))

(require 'ef-themes)
(load-theme 'ef-bio :no-confirm)

(require 'rec-mode) ;; Loaded from site-lisp.d

