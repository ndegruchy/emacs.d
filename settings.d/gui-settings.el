;; GUI mode settings

;; Disable some UI chrome that I don't need
(scroll-bar-mode -1)
(tooltip-mode    -1)
(tool-bar-mode   -1)
(menu-bar-mode   -1)

(setq x-gtk-use-system-tooltips nil ;; I don't use GTK build anyway
	  inhibit-x-resources       t   ;; I also want to customize this myself, thanks
	  use-dialog-box 			nil ;; No UI dialogs, either
	  )

;; Fonts
(add-to-list 'default-frame-alist '(font . "Iosevka-17"))

;; Frame
(setq frame-resize-pixelwise nil)
(add-to-list 'default-frame-alist '(height . 24))
(add-to-list 'default-frame-alist '(width . 80))
