;; custom-keybindings.el
;; Set custom keybindings here

;; Unbind keys (may be useful for later bindings)
(dolist (key '("\C-z"					; Don't suspend the frame
	       "\C-x\C-z"				; Ditto
	       "\C-x\C-d"				; List directory is dumb, use dired
	       "\M-o"					; ??
	       "\C-x\C-r"				; Ditto
	       "s-x"))                                  ; macOS remove Super-X combo
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
 ;; Replacement keys
 ("C-x C-d"             . ido-dired))		; Replacing the directory list with dired
