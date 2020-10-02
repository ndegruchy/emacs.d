;; general-settings.el
;; Basic setup and configuration

;; Me
(setq user-full-name         "Nathan DeGruchy"
      user-mail-address      "nathan@degruchy.org"
	  message-signature      t
	  message-signature-file "~/.config/signature.txt")

;; Startup
;; Some default settings
(setq inhibit-startup-message                t
      initial-scratch-message                ";; Scratch buffer\n"
      initial-major-mode                     (quote text-mode))

;; Misc
(setq ring-bell-function                     (quote ignore)
      confirm-kill-emacs                     'y-or-n-p
      load-prefer-newer                      t
      enable-local-variables                 :safe
      delete-by-moving-to-trash              t
      suggest-key-bindings                   t)

;; Abbreviations
(setq-default abbrev-mode t)
(setq save-abbrevs 'silently)

;; Mouse
(setq mouse-wheel-progressive-speed          nil)

;; Case sensitivity
(setq read-file-name-completion-ignore-case  t
      completion-ignore-case                 t)

;; UI
(setq inhibit-x-resources                    t
      global-font-lock-mode                  t
      delete-selection-mode                  t
      tooltip-mode                           nil
      show-paren-mode                        t
      default-tab-width                      4
      x-gtk-use-system-tooltips              nil)

(tool-bar-mode       -1)
(menu-bar-mode       -1)
(scroll-bar-mode     -1)
(display-battery-mode t)
(tooltip-mode        -1)
(global-linum-mode   -1)

(fset 'yes-or-no-p                          'y-or-n-p)

;; Fonts
(add-to-list 'default-frame-alist
			 '(font . "Fira Code-16"))

;; Needed if using the default theme
(set-face-attribute 'region nil :background "light goldenrod")
(load-theme 'tango)

;; Editing - Pairs
(electric-pair-mode 1)
(show-paren-mode    1)

;; Editing - selection
;; (delete-selection-mode t)
;; Use a more Emacs style mark mode
(transient-mark-mode 1)
(setq shift-select-mode nil)

;; Editing - Indentation
(setq-default tab-always-indent 'complete
			  tab-width         4)

;; Enable narrowing
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-defun  'disabled nil)

;; Upcasing
(put 'upcase-region 'disabled nil)

;; Battery
(setq-default battery-mode-line-format "[%b%p%%]")

;; Email
(setq mail-specify-envelope-from t
	  message-sendmail-envelope-from 'header
	  mail-envelope-from 'header)

;; Birthday

(when (string= "12-21" (format-time-string "%m-%d"))
  (run-with-idle-timer
   1 nil
   (lambda ()
     (let (cursor-type)
       (animate-birthday-present user-full-name)))))

;; Private settings
(load-file "~/.emacs.d/settings.d/private.el")
