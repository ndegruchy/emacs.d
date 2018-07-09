;; custom-keybindings.el
;; Set custom keybindings here

;; Unbind keys (may be useful for later bindings)
(dolist (key '("\C-\\"
	       "\C-z"
	       "\C-x\C-z"
	       "\C-c\C-c m"))
  (global-unset-key key))

(global-set-key (kbd "C-c \\")		'align-regexp)
(global-set-key (kbd "C-c C-c f")	'ffap)
(global-set-key (kbd "C-c C-c e")	'eshell)
(global-set-key (kbd "C-c b")		'ibuffer)
(global-set-key (kbd "C-c /")           'repeat)

;; Custom function binds
(global-set-key (kbd "C-c o")          #'ndegruchy/open-line-below)
(global-set-key (kbd "C-c O")          #'ndegruchy/open-line-above)
(global-set-key (kbd "C-c k")          #'ndegruchy/kill-whole-line)
(global-set-key (kbd "C-c m")          #'ndegruchy/select-line)
(global-set-key (kbd "C-c w")          #'ndegruchy/select-current-word)
(global-set-key (kbd "C-c d")          #'ndegruchy/insert-date)
