;; custom-hooks.el
;; Set custom mode or setting hooks here

;; Timestamps
(add-hook 'before-save-hook 'time-stamp)

;; Protect buffers
(add-hook 'after-init-hook #'protect-buffers)

;; Fix for Material that doesn't load right when run via emacs daemon
;; Taken from https://github.com/cpaulik/emacs-material-theme/issues/45#issuecomment-385247309
(add-hook 'after-make-frame-functions
            (lambda (frame)
              (with-selected-frame frame
                (load-theme 'material t))))
