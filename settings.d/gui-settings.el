;; GUI mode settings

(scroll-bar-mode -1)
(tooltip-mode    -1)
(tool-bar-mode   -1)

(setq x-gtk-use-system-tooltips nil
	  inhibit-x-resources       t)

;; Fonts
;; If using Cascadia Code, use the *static* fonts, not the variable
;; ones. Emacs doesn't seem to like them.
;; https://github.com/microsoft/cascadia-code/issues/589
;; (add-to-list 'default-frame-alist '(font . "Cascadia Code-15"))
(add-to-list 'default-frame-alist '(font . "Iosevka-17"))

;; Frame
(setq frame-resize-pixelwise nil)
;; (add-to-list 'default-frame-alist '(height . 24))
;; (add-to-list 'default-frame-alist '(width . 80))