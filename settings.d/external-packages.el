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
;; Load all the bits I want
(require 'emms-setup)
(require 'emms-mode-line)
(require 'emms-mode-line-icon)
(require 'emms-player-vlc)
(require 'emms-info-native)
(require 'emms-librefm-scrobbler)

;; Enable the functions
(emms-all)
(emms-mode-line 1)
(emms-playing-time-disable-display)

;; Settings
(setq emms-mode-line-icon-enabled-p nil)
(setq emms-info-functions '(emms-info-native))
(setq emms-source-file-default-directory (concat (getenv "HOME") "/Media/Music"))
(setq emms-info-asynchronosly t)
(setq emms-show-format "%s")
(setq emms-browser-covers 'emms-browser-cache-thumbnail-async)

;; Use console-vlc, if available
(if (executable-find "cvlc")
	(setq emms-player-list '(emms-player-vlc))
  (emms-default-players))

;; Libre.FM scrobbling
(setq emms-librefm-scrobbler-username "ndegruchy"
	  emms-librefm-scrobbler-password librefm-password)

;; Control surfaces
(global-set-key (kbd "C-c e b") 'emms-smart-browse)
(global-set-key (kbd "C-c e p") 'emms-pause)
(global-set-key (kbd "C-c e N") 'emms-next)
(global-set-key (kbd "C-c e P") 'emms-previous)
(global-set-key (kbd "C-c e s") 'emms-stop)
(global-set-key (kbd "C-c e l") 'ndegruchy/emms-show-playlist) ;; Show current playlist
(global-set-key (kbd "C-c e L") 'emms-metaplaylist-mode-go) ;; Show all playlists

;; Custom functions for emms
(defun ndegruchy/emms-show-playlist ()
  "Shows *just* the playlist buffer without having to invoke some
form of the EMMS browse functions"
  (interactive)
  (switch-to-buffer emms-playlist-buffer))
(defun ndegruchy/emms-mode-hook ()
  "Customizations to various emms buffers and modes"
  (hl-line-mode 1))

;; Emms hooks
(add-hook 'emms-browser-mode-hook #'ndegruchy/emms-mode-hook)
(add-hook 'emms-playlist-mode-hook #'ndegruchy/emms-mode-hook)

;; Local Variables:
;; truncate-lines: t
;; End:
