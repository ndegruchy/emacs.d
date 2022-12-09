;;; gui-settings --- Emacs GUI settings  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Nathan DeGruchy

;; Author: Nathan DeGruchy <nathan@degruchy.org>
;; Keywords: local, convenience

;; Disable some UI chrome that I don't need
(scroll-bar-mode -1)
(tooltip-mode    -1)
(tool-bar-mode   -1)
(menu-bar-mode   -1)

(setq x-gtk-use-system-tooltips nil  ;; I don't use GTK build anyway
	  inhibit-x-resources       t    ;; I also want to customize this myself, thanks
	  use-dialog-box 			nil) ;; No UI dialogs, either

;; Fonts
(add-to-list 'default-frame-alist '(font . "Hack-19"))

;; Frame
(setq frame-resize-pixelwise t)
(toggle-frame-maximized)

;; Image support
(setq image-use-external-converter t)

;; Local Variables:
;; truncate-lines: t
;; End:
