;;; general-settings --- General emacs configurations  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Nathan DeGruchy

;; Author: Nathan DeGruchy <nathan@degruchy.org>
;; Keywords: local, data, processes, convenience

;; Me
(setq user-full-name         "Nathan DeGruchy"
      user-mail-address      "nathan@degruchy.org"
      message-signature      t)

;; GPG Agent Stuff
(setq epg-pinentry-mode 'loopback)

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

;; Disable all other themes to avoid awkward blending:
(mapc #'disable-theme custom-enabled-themes)

(menu-bar-mode		  t)
(global-linum-mode   -1)
(fset 'yes-or-no-p   'y-or-n-p)

;; UI - Minibuffer completion
(setq completion-styles '(initials partial-completion substring)
	  completions-format 'vertical
	  read-buffer-completion-ignore-case t
	  read-file-name-completion-ignore-case t
	  ;; Emacs 29+ features...
	  completions-header-format nil
	  completions-max-height 10
	  completion-auto-select nil)

;; Editing - Pairs
;; This is annoying when you don't want it...
;; (electric-pair-mode 1)

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

;; Custom modeline
(setq-default mode-line-format
			  (list " " mode-line-modified
					;; Buffer name
					" " mode-line-buffer-identification
					;; Position
					" " mode-line-position
					" " mode-line-misc-info))

;; Calendaring
(require 'remember)
(setq calendar-christian-all-holidays-flag 	t
	  calendar-date-style    				'iso
	  calendar-mark-diary-entries-flag 		t
	  calendar-view-diary-initially-flag	t)
(add-to-list 'remember-handler-functions 'remember-diary-extract-entries)
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

;; Backup file settings
(setq make-backup-files nil) ;; don't
 
;; Protect buffers
(add-hook 'after-init-hook #'ndegruchy/protect-buffers)

;; Removes weird characters in the title of a frame
(setq frame-title-format "Emacs - %b")

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

;; Disable customize functions
(dolist (sym '(customize-option customize-browse customize-group customize-face
								customize-rogue customize-saved customize-apropos
								customize-changed customize-unsaved customize-variable
								customize-set-value customize-customized customize-set-variable
								customize-apropos-faces customize-save-variable
								customize-apropos-groups customize-apropos-options
								customize-changed-options customize-save-customized))
  (put sym 'disabled "I've disabled `customize', configure Emacs from $XDG_CONFIG_HOME/emacs/config.el instead"))
(put 'customize-themes 'disabled "I've disabled `customize', configure Emacs from $XDG_CONFIG_HOME/emacs/config.el instead")

;; Local Variables:
;; truncate-lines: t
;; End:
