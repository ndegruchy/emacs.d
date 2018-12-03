;; general-settings.el
;; Basic setup and configuration

;; Me
(setq user-full-name    "Nathan DeGruchy"
      user-mail-address "nathan@degruchy.org")

;; Startup
;; Some default settings
(setq inhibit-startup-message                t
      initial-scratch-message                ";; Scratch buffer\n"
      initial-major-mode                     (quote text-mode))

;; Misc
(setq ring-bell-function                     (quote ignore)
      confirm-kill-emacs                     'y-or-n-p
      load-prefer-newer                      t)

;; Mouse
(setq mouse-wheel-progressive-speed          nil)

;; Case sensitivity
(setq read-file-name-complection-ignore-case t
      completion-ignore-case                 t)

;; UI
(setq inhibit-x-resources                    t
      global-font-lock-mode                  t
      delete-selection-mode                  t
      show-paren-mode                        t)

(tool-bar-mode       -1)
(menu-bar-mode       -1)
(scroll-bar-mode     -1)
(display-battery-mode t)
(global-linum-mode   -1)

(fset 'yes-or-no-p                          'y-or-n-p)

;;(add-to-list 'default-frame-alist '(font . "Source Code Pro-14"))
(add-to-list 'default-frame-alist '(font . "Fira Code-14"))

;; Editing - Pairs
(electric-pair-mode 1)
(show-paren-mode    1)

;; Editing - selection
(delete-selection-mode t)

;; Enable narrowing
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-defun  'disabled nil)
