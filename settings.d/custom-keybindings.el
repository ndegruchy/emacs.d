;; custom-keybindings.el
;; Set custom keybindings here

;; Unbind keys (may be useful for later bindings)
(dolist (key '("\C-\\"
	       "\C-z"
	       "\C-x\C-z"
	       "\C-c\C-c m"))
  (global-unset-key key))

(bind-keys*
 ;; Personal map for packages
 ("C-c a"   . align-regexp)
 ("C-c G"   . avy-goto-word-0)
 ("C-c g"   . avy-goto-line)
 ("C-c W"   . ace-window)
 ("C-c e"   . embrace-commander)
 ("C-c E"   . embrace-add)
 ("C-c s"   . er/expand-region)
 ("C-c M"   . magit-status)
 ;; Smex
 ("M-x"	    . smex)
 ("M-X"	    . smex-major-mode-commands)
 ("C-c M-x" . execute-extended-command))

;; Custom functions
(bind-keys*
 ("C-c l"   . ndegruchy/open-line-below)
 ("C-c L"   . ndegruchy/open-line-above)
 ("C-c k"   . ndegruchy/kill-whole-line)
 ("C-c m"   . ndegruchy/select-line)
 ("C-c w"   . ndegruchy/select-current-word)
 ("C-c d"   . ndegruchy/insert-date))
