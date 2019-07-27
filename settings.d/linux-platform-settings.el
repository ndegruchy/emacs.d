;; linux-platform-settings.el
;; Set linux-specific settings here

(setq x-gtk-use-system-tooltips nil)

(use-package fish-mode
  :ensure t)

(when (string= (system-name) "ndegruchy-chbk.degruchy.org"))

(when (string= (system-name) "ndegruchy-dt.degruchy.org"))
