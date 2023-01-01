;;; required-packages.el --- External packages that need to be installed  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Nathan DeGruchy

;; Author: Nathan DeGruchy <nathan@degruchy.org>
;; Keywords: local, data, processes, convenience

(require 'package)

;; Fetch required packages
(setq package-selected-packages '(emms
								  web-mode))

(package-install-selected-packages)

;; EMMS
;; Load all the bits I want
(require 'emms-setup)
(require 'emms-mode-line)
(require 'emms-mode-line-icon)
(require 'emms-player-vlc)
(require 'emms-info-native)

;; Enable the functions
(emms-all)
(emms-mode-line 1)
;; (emms-librefm-scrobbler-enable)
(emms-playing-time-disable-display)

;; Settings
(setq emms-mode-line-icon-enabled-p nil)
(setq emms-info-functions '(emms-info-native))
(setq emms-source-file-default-directory (concat (getenv "HOME") "/Media/Music"))
(setq emms-info-asynchronosly t)
(setq emms-show-format "%s")

(when (window-system)
  (setq emms-browser-covers 'emms-browser-cache-thumbnail-async))

;; Player (vlc has a wide support for media formats, and has a
;; "headless" console version, perfect.
(setq emms-player-list '(emms-player-vlc))

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

;; Web Mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(defun ndegruchy/web-mode-hook ()
  "My custom web mode hooks"
  (setq web-mode-markup-ident-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4))
(add-hook 'web-mode-hook #'ndegruchy/web-mode-hook)

;; Local Variables:
;; truncate-lines: t
;; End:
