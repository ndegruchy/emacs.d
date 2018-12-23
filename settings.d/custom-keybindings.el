;; custom-keybindings.el
;; Set custom keybindings here

;; Unbind keys (may be useful for later bindings)
(dolist (key '("\C-z"
	       "\C-x\C-z"))
  (global-unset-key key))

(bind-keys*
 ;; Personal map for packages
 ("C-c E"  . embrace-commander)
 ("C-c e"  . embrace-add)
 ("C-c s"  . er/expand-region)
 ("C-c j"  . ace-jump-mode))

;; Custom functions
(bind-keys*
 ("S-<return>"	.  ndegruchy/smart-open-line)
 ("C-S-<return>".  ndegruchy/open-line-above)
 ("C-c m"	.  ndegruchy/select-current-line-dwim)
 ("C-c d"	.  ndegruchy/insert-date))
