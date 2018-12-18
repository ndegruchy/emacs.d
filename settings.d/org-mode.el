;; org-mode.el
;; Org mode is big and complicated, I keep it's settings seperate so
;; that I'm not trying to cram it all inside a `use-package' directive

(bind-keys*
 ("C-c o a" . org-agenda)
 ("C-c o l" . org-store-link)
 ("C-c o b" . org-iswitchb))

(setq org-agenda-files (quote ("~/Documents/Notes"))
      org-blank-before-new-entry t)
