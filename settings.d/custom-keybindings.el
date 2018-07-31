;; custom-keybindings.el
;; Set custom keybindings here

;; Unbind keys (may be useful for later bindings)
(dolist (key '("\C-\\"
	       "\C-z"
	       "\C-x\C-z"
	       "\C-c\C-c m"))
  (global-unset-key key))

(bind-keys*
 ("C-c \\"	. align-regexp)
 ("C-c f"	. ffap)
 ("C-c e"	. eshell)
 ("C-c b"	. ido-switch-buffer)
 ("C-c /"	. repeat)
 ("C-c :"	. avy-goto-word-0)
 ("M-g g"	. avy-goto-line)
 ("M-o"		. ace-window)
 ("C-c ,"	. embrace-commander)
 ("C-c +"	. embrace-add)
 ("C-c ="	. er/expand-region)
 ("C-c M"	. magit-status)
 ("M-x"		. smex)
 ("M-X"		. smex-major-mode-commands)
 ("C-c M-x"	. execute-extended-command))

;; Custom functions
(bind-keys*
 ("C-c o"	. ndegruchy/open-line-below)
 ("C-c O"	. ndegruchy/open-line-above)
 ("C-c k"	. ndegruchy/kill-whole-line)
 ("C-c m"	. ndegruchy/select-line)
 ("C-c w"	. ndegruchy/select-current-word)
 ("C-c d"	. ndegruchy/insert-date))
