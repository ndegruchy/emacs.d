;; custom-keybindings.el
;; Set custom keybindings here

;; Unbind keys (may be useful for later bindings)
(dolist (key '("\C-\\"
	       "\C-z"
	       "\C-x\C-z"
	       "\C-c\C-c m"))
  (global-unset-key key))

;; Custom function binds

(bind-keys
 :prefix-map ndegruchy-map
 :prefix-docstring "My Keymappings"
 :prefix "C-c C-,"
 ("o" . ndegruchy/open-line-below)
 ("O" . ndegruchy/open-line-above)
 ("k" . ndegruchy/kill-whole-line)
 ("w" . ndegruchy/select-current-word)
 ("d" . ndegruchy/insert-date))
