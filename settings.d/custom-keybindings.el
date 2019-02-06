;; custom-keybindings.el
;; Set custom keybindings here

;; Unbind keys (may be useful for later bindings)
(dolist (key '("\C-z"
	       "\C-x\C-z"))
  (global-unset-key key))

(bind-keys*
 ;; Personal map for packages
 ("C-c -"  . split-window-vertically)
 ("C-c |"  . split-window-horizontally)
 ("C-S-z"  . bury-buffer))

;; Custom functions
(bind-keys*
 ("C-c R"       .  ndegruchy/rename-file-and-buffer)
 ("S-<return>"	.  ndegruchy/smart-open-line)
 ("C-S-<return>".  ndegruchy/smart-open-line-above)
 ("C-c m"	.  ndegruchy/select-current-line-dwim)
 ("C-c d"	.  ndegruchy/insert-date)
 ("C-c a"       .  ndegruchy/duplicate-line))

;; Package keybinds
(bind-keys*
 ("C-c E" . embrace-commander)
 ("C-c e" . embrace-add)
 ("C-c s" . er/expand-region)
 ("C-z"   . unfill-toggle)
 ("C-c ;" . windresize))
