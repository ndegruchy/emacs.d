;; linux-platform-settings.el
;; Set linux-specific settings here

(setq x-gtk-use-system-tooltips nil)



(use-package material-theme
  :ensure t
  :config
  (load-theme 'material t nil))

;; (use-package base16-theme
;;   :ensure t
;;   :config
;;   (load-theme 'base16-tomorrow-night t))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package fish-mode
  :ensure t)

(when (string= (system-name) "ndegruchy-chbk.degruchy.org"))

(when (string= (system-name) "ndegruchy-dt.degruchy.org")
  ;; Only load on my desktop, please.
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/emms")
  (use-package emms
    :config
    (require 'emms-setup)
    (emms-standard)
    (emms-default-players)
    (require 'emms-info-libtag)
    (setq emms-info-functions '(emms-info-libtag)
	  emms-source-file-default-directory "/mnt/ndegruchy/Music/"))
  )
