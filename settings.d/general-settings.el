;; general-settings.el
;; Basic setup and configuration

;; Me
(setq user-full-name         "Nathan DeGruchy"
      user-mail-address      "nathan@degruchy.org"
      message-signature      t)

;; Signature
(if (file-exists-p "~/.config/signature.txt")
	(setq message-signature-file "~/.config/signature.txt"))

;; Startup
;; Some default settings
(setq inhibit-startup-message                t
      initial-scratch-message                ";; Scratch buffer\n"
      initial-major-mode                     (quote fundamental-mode))

;; Misc
(setq ring-bell-function                     (quote ignore)
      load-prefer-newer                      t
      enable-local-variables                 :safe
      delete-by-moving-to-trash              t
      suggest-key-bindings                   t
	  display-time-24hr-format	             t
	  sentence-end-double-space				 nil)

;; Abbreviations
(setq-default abbrev-mode t)
(setq save-abbrevs 'silently)

;; Mouse
(setq mouse-wheel-progressive-speed          nil)

;; Case sensitivity
(setq read-file-name-completion-ignore-case  t
      completion-ignore-case                 t)

;; UI
(setq global-font-lock-mode                  t
      delete-selection-mode                  t
	  show-paren-mode                        t)

(menu-bar-mode		 -1)
(display-battery-mode t)
(global-linum-mode   -1)
(fset 'yes-or-no-p   'y-or-n-p)

(setq-default frame-title-format '("%b [%m]"))

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
			  tab-width         4
			  default-tab-width 4
			  indent-tabs-mode  t
			  backward-delete-char-untabify-method 'hungry)

;; Enable narrowing
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-defun  'disabled nil)

;; Upcasing
(put 'upcase-region 'disabled nil)

;; Battery
(setq-default battery-mode-line-format "B:%b%p%% |")
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
	   ;; Position
	   mode-line-position
   	   ;; Sep
	   " | "
	   ;; Misc
	   mode-line-misc-info
	   ;; Dashes
	   mode-line-end-spaces))

;; Calendaring
(setq calendar-christian-all-holidays-flag 	t
	  calendar-date-style    				'iso
	  calendar-mark-diary-entries-flag 		t)
;; Appointments
(setq appt-audible 				t
	  appt-display-mode-line 	t
	  appt-display-diary		nil)
(appt-activate)

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
(add-hook 'after-init-hook #'ndegruchy/protect-buffers)

;; Removes weird characters in the title of a frame
(setq frame-title-format "%b")

;; Save minibuffer history
(savehist-mode 1)

;; Uniquify
(setq uniquify-buffer-name-style    'reverse
      uniquify-separator            "/"
      uniquify-after-kill-buffer-p  t
      uniquify-ignore-buffers-re    "^\\*")

;; Window moving keybindings
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
