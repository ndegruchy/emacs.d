;; custom-keybindings.el
;; Set custom keybindings here

;; Unbind keys (may be useful for later bindings)
(dolist (key '("\C-z"					; Don't suspend the frame
			   "\C-x\C-z"				; Ditto
			   "\C-x\C-d"				; List directory is dumb, use dired
			   "\M-o"					; ??
			   "\C-x\C-r"				; ??
			   "\M-g\g"					; Replacing below
			   "\M-g\c"					; Ditto
			   "\M-g\M-g"))				; Ditto
  (global-unset-key key))

(bind-keys*
 ;; Personal map for packages
 ("C-c h"			. split-window-vertically)
 ("C-c v"			. split-window-horizontally)
 ("C-S-z"			. bury-buffer)
 ;; Custom functions
 ("C-c R"			. ndegruchy/rename-file-and-buffer)
 ("S-<return>"		. ndegruchy/smart-open-line)
 ("C-S-<return>"	. ndegruchy/smart-open-line-above)
 ("C-c m"			. ndegruchy/select-current-line-dwim)
 ("C-c d"			. ndegruchy/insert-date)
 ("C-c a"			. ndegruchy/duplicate-line)
 ;; Package keybinds
 ("C-c E"			. embrace-commander)
 ("C-c e"			. embrace-add)
 ("C-c s"			. er/expand-region)
 ("M-g M-g"         . avy-goto-line)	; Replaces standard goto-line function (and wraps it)
 ("M-g c"           . avy-goto-char)	; Replaces standard goto-char function
 ("M-g w"           . avy-goto-word-0)	; Doesn't replace a function, but here for completion sake
 ("C-c r"		    . windresize)
 ;; Replacement keys
 ("C-x C-d"         . ido-dired))		; Replacing the directory list with dired
