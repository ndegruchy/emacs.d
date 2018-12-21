;; custom-keybindings.el
;; Set custom keybindings here

;; Unbind keys (may be useful for later bindings)
(dolist (key '("\C-z"
	       "\C-x\C-z"))
  (global-unset-key key))

(bind-keys*
 ;; Personal map for packages
 ("C-c a"   . align-regexp)
 ("C-c G"   . avy-goto-word-0)
 ("C-c g"   . avy-goto-line)
 ("C-c c"   . avy-goto-char)
 ("C-c W"   . ace-window)
 ("C-c E"   . embrace-commander)
 ("C-c e"   . embrace-add)
 ("C-c s"   . er/expand-region))

;; Custom functions
(bind-keys*
 ("C-c l"   . ndegruchy/open-line-below)
 ("C-c L"   . ndegruchy/open-line-above)
 ("C-c m"   . ndegruchy/select-current-line-dwim)
 ("C-c d"   . ndegruchy/insert-date))
