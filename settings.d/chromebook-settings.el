;; Special settings for my Chromebook which runs Xubuntu/Gallium OS

(use-package git-commit
  ;; Pin git-commit to melpa-stable.
  ;; When you try to install magit (from stable), it tries to pull in
  ;; the 'newest' git-commit, which happens to come from the regular
  ;; melpa packages, rather than stable. This requires Emacs 25.1,
  ;; whereas my GalliumOS Chromebook only has 24.5 available.
  :ensure t
  :pin melpa-stable)
