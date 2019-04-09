;; linux-platform-settings.el
;; Set linux-specific settings here

(setq x-gtk-use-system-tooltips nil)

(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-tomorrow-night t))

(use-package dired-atool
  :ensure t
  :config
  (dired-atool-setup))

(use-package dired-rsync
  :ensure t
  :config
  (bind-key "y" 'dired-rsync dired-mode-map))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package fish-mode
  :ensure t)

(when (string= (system-name) "ndegruchy-chbk.degruchy.org")
  ;; Chromebook
  (add-to-list 'default-frame-alist '(font . "Fira Mono-16"))

(when (string= (system-name) "ndegruchy-dt.degruchy.org")
  ;; Only load on my desktop, please.

  (add-to-list 'default-frame-alist '(font . "Fira Code-14"))
  
  (use-package pdf-tools
    :ensure t
    :config
    (pdf-tools-install))
  
  (use-package tex-site
    :ensure auctex
    :config
    (setq TeX-view-program-list
  	  '(("Zathura" "/usr/bin/zathura %o")))
    (setq TeX-view-program-selection
          (quote
           (((output-dvi style-pstricks)
             "dvips and gv")
            (output-dvi "xdvi")
            (output-pdf "Zathura")
            (output-html "xdg-open"))))))
