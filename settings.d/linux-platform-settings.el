;; linux-platform-settings.el
;; Set linux-specific settings here

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package tex-site
  :ensure auctex
  :if (eq system-name "ndegruchy-dt.degruchy.org")
  :config
  (setq TeX-view-program-list
  	'(("Zathura" "/usr/bin/zathura %o")))
  (setq TeX-view-program-selection
        (quote
         (((output-dvi style-pstricks)
           "dvips and gv")
          (output-dvi "xdvi")
          (output-pdf "Zathura")
          (output-html "xdg-open")))))
