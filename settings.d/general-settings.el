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
      initial-major-mode                     (quote fundamental-mode))

;; Misc
(setq ring-bell-function                     (quote ignore)
      confirm-kill-emacs                     'y-or-n-p
      load-prefer-newer                      t
      enable-local-variables                 :safe
      delete-by-moving-to-trash              t
      suggest-key-bindings                   t
	  display-time-24hr-format	             t)

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
      show-paren-mode                        t
      default-tab-width                      4
      x-gtk-use-system-tooltips              nil)

(tool-bar-mode       -1)
(menu-bar-mode        1)
(scroll-bar-mode     -1)
(display-battery-mode t)
(tooltip-mode         t)
(global-linum-mode   -1)

(fset 'yes-or-no-p   'y-or-n-p)

;; Fonts
(add-to-list 'default-frame-alist '(font . "Hack-15"))

;; Frame
(setq frame-resize-pixelwise nil)
(add-to-list 'default-frame-alist '(height . 24))
(add-to-list 'default-frame-alist '(width . 80))
(setq-default frame-title-format '("%b [%m]"))

;; After frame creation
(if (daemonp)
	(add-hook 'after-make-frame-functions #'ndegruchy/setup-gui)
  (load-theme 'zenburn t))

;; Needed if using the default theme
;; (set-face-attribute 'region nil :background "light goldenrod")
;; (load-theme 'tango)
(load-theme 'leuven)

;; Editing - Pairs

(electric-pair-mode 1)
(show-paren-mode    1)

;; Editing - selection
(delete-selection-mode t)
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
(setq-default battery-mode-line-format "%b%p%%")
(display-battery-mode 1)

;; Custom modeline
(setq-default mode-line-format
	  (list
	   ;; Dirty flag
	   "[%+] "
	   ;; Narrowing enabled?
	   "%n "
	   ;; Buffer name
	   mode-line-buffer-identification
	   ;; Sep
	   " | "
	   ;; Battery
	   "B:" 'battery-mode-line-string
	   ;; Sep
	   " | "
	   ;; Position
	   mode-line-position
	   ;; Dashes
	   mode-line-end-spaces))


;; Birthday
(when (string= "12-21" (format-time-string "%m-%d"))
  (run-with-idle-timer
   1 nil
   (lambda ()
     (let (cursor-type)
       (animate-birthday-present user-full-name)))))

;; Timestamps
(add-hook 'before-save-hook 'time-stamp)

;; Protect buffers
(add-hook 'after-init-hook #'protect-buffers)

;; Removes weird characters in the title of a frame
(setq frame-title-format "%b")

;; Save minibuffer history
(savehist-mode 1)
