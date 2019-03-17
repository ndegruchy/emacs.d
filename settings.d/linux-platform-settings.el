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

(when (string= (system-name) "ndegruchy-dt.degruchy.org")
  ;; Only load on my desktop, please.

  (use-package pdf-tools
    :ensure t
    :config
    (pdf-tools-install))
  
  (use-package emms
    :config
    (require 'emms-setup)
    (require 'emms-info-libtag)
    (require 'emms-source-file)
    (emms-all)
    (emms-default-players)
    (emms-cache 1)
    (emms-cache-restore)
    (emms-cache-sync)
    (setq emms-source-list                         '((emms-directory-tree "/mnt/ndegruchy/Music"))
          emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find
	  emms-info-functions                      '(emms-info-libtag)
	  emms-info-auto-update                    t
	  emms-info-asynchronously                 t
	  emms-browser-covers                      'emms-browser-cache-thumbnail-async
	  emms-player-list                         '(emms-player-vlc)))
  
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
