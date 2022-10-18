;;; win32-settings.el --- A place for win32 platform settings  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Nathan DeGruchy

;; Author: Nathan DeGruchy <nathan@degruchy.org>
;; Keywords: local, processes, convenience

;; Cascadia is more likely to be installed on Windows than Ioveska
(add-to-list 'default-frame-alist '(font . "Cascadia Code-14"))

;; Set my default directory to the root of the profile, not %APPDATA%
;; And set some ls-lisp.el arguments
(setq default-directory "C:/Users/NathanRDeGruchy/"
	dired-listing-switches "-a -F -l")
