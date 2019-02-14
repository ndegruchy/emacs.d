;; custom-keybindings.el
;; Set custom keybindings here

;; Unbind keys (may be useful for later bindings)
(dolist (key '("\C-z"
	       "\C-x\C-z"))
  (global-unset-key key))

(bind-keys*
 ;; Personal map for packages
 ("C-c -"		. split-window-vertically)
 ("C-c |"		. split-window-horizontally)
 ("C-S-z"		. bury-buffer)
 ;; Custom functions
 ("C-c R"		. ndegruchy/rename-file-and-buffer)
 ("S-<return>"		. ndegruchy/smart-open-line)
 ("C-S-<return>"	. ndegruchy/smart-open-line-above)
 ("C-c m"		. ndegruchy/select-current-line-dwim)
 ("C-c d"		. ndegruchy/insert-date)
 ("C-c a"		. ndegruchy/duplicate-line)
 ;; Package keybinds
 ("C-c E"		. embrace-commander)
 ("C-c e"		. embrace-add)
 ("C-c s"		. er/expand-region)
 ("M-x"                 . helm-M-x)
 ("C-x C-f"             . helm-find-files)
 ("C-x r b"             . helm-filtered-bookmarks)
 ("C-x b"               . helm-buffers-list)
 ("C-c ;"		. windresize))
