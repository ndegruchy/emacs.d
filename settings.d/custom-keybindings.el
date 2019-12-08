;; custom-keybindings.el
;; Set custom keybindings here

;; Unbind keys (may be useful for later bindings)
(dolist (key '("\C-z"		; Don't suspend the frame
	       "\C-x\C-z"	; Ditto
	       "\C-x\C-d"	; List directory is dumb, use dired
	       "\M-o"		; ??
	       "\C-x\C-r"))
  (global-unset-key key))

(bind-keys*
 ;; Personal map for packages
 ("C-c h"		. split-window-vertically)
 ("C-c v"		. split-window-horizontally)
 ("C-S-z"		. bury-buffer)
 ;; Custom functions
 ("C-c R"		. ndegruchy/rename-file-and-buffer)
 ("S-<return>"		. ndegruchy/smart-open-line)
 ("C-S-<return>"	. ndegruchy/smart-open-line-above)
 ("C-c m"		. ndegruchy/select-current-line-dwim)
 ("C-c d"		. ndegruchy/insert-date)
 ("C-c a"		. ndegruchy/duplicate-line)
 ("M-<up>"              . ndegruchy/move-line-up)
 ("M-<down>"            . ndegruchy/move-line-down)
 ("M-Q"                 . unfill-paragraph)
 ;; Replacement keys
 ("M-z"                 . zap-up-to-char)
 ("M-Z"                 . zap-to-char)
 ("C-x C-d"             . ido-dired))		; Replacing the directory list with dired
