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

(unless (package-installed-p 'emms)
  (package-refresh-contents)
  (package-install 'emms))

(require 'ef-themes)
(load-theme 'ef-bio :no-confirm)

(require 'rec-mode) ;; Loaded from site-lisp.d

;; Emms
(require 'emms-setup)
(require 'emms-mode-line)
(require 'emms-player-vlc)
(require 'emms-info-libtag)
(emms-all)
(emms-mode-line 1)
(setq emms-info-functions '(emms-info-libtag))
(setq emms-source-file-default-directory (concat (getenv "HOME") "/Media/Music"))
(setq emms-info-asynchronosly t)
(setq emms-show-format "%s")
(setq emms-browser-covers 'emms-browser-cache-thumbnail-async)
(if (executable-find "cvlc")
	(setq emms-player-list '(emms-player-vlc))
  (emms-default-players))
(global-set-key (kbd "C-c e b") 'emms-smart-browse)
(global-set-key (kbd "C-c e p") 'emms-pause)
(global-set-key (kbd "C-c e N") 'emms-next)
(global-set-key (kbd "C-c e P") 'emms-previous)
(global-set-key (kbd "C-c e s") 'emms-stop)

;; Local Variables:
;; truncate-lines: t
;; End:
