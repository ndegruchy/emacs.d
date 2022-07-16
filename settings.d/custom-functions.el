;; custom-functions.el
;; Set custom functions here

;; Insert the date (formatted)
(defun ndegruchy/insert-date ()
  "Inserts an ISO date by default, prefixing with C-u asks for a
  format."
  (interactive)
  (if (null current-prefix-arg)
	  (insert (format-time-string "%F")))
  (if (eq (prefix-numeric-value current-prefix-arg) 4)
	  (insert (format-time-string "%FT%T%z")))
  (if (> (prefix-numeric-value current-prefix-arg) 4)
	  (insert (format-time-string (read-string "Format: ")))))

(defun ndegruchy/select-current-line-dwim (arg)
  ;; Retrieved from https://emacs.stackexchange.com/questions/15033/how-to-mark-current-line-and-move-cursor-to-next-line
  ;; on 2018-08-07
  "Select the current line and move the cursor by ARG lines IF
no region is selected.

If a region is already selected when calling this command, only move
the cursor by ARG lines."
  (interactive "p")
  (when (not (use-region-p))
    (forward-line 0)
    (set-mark-command nil))
  (forward-line arg))

(defun ndegruchy/rename-file-and-buffer ()
  "Rename current buffer and if the buffer is visiting a file, rename it too.
  Pulled from the CRUX package of useful stuff:
  https://github.com/bbatsov/crux/blob/master/crux.el"
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
      (let* ((new-name (read-from-minibuffer "New name: " filename))
             (containing-dir (file-name-directory new-name)))
        (make-directory containing-dir t)
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
	  (set-visited-file-name new-name t t)))))))

(defun ndegruchy/smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode.
Retrieved from: https://emacsredux.com/blog/2013/03/26/smarter-open-line/ (2018-12-22)"
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun ndegruchy/smart-open-line-above ()
  "Inserts a line above the current line, moving the cursor to
that line and setting the indent properly"
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(defun ndegruchy/duplicate-line ()
    "Makes a copy of the current line after the current
line. Useful for listing directories, etc."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

(defun ndegruchy/move-line-up()
  "Move the line up by one"
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun ndegruchy/move-line-down()
  "Move the line down by one"
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun protect-buffers ()
  (let ((protected '("*scratch*" "*Messages*")))
    (dolist (buf protected)
      (with-current-buffer buf
        (emacs-lock-mode 'kill)))))

(defun split-name (s)
  (split-string
   (let ((case-fold-search nil))
	 (downcase
	  (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" s)))
   "[^A-Za-z0-9]+"))

(defun dasherize (s)
  (mapconcat 'downcase
			 (split-name s) "-"))

(defun ndegruchy/goto-matching-paren (&optional arg)
  "Go to the matching paren, similar to vi's %."
  (interactive "p")
  (or arg (setq arg 1))
  (cond
   ;; Check for "outside of bracket" positions
   ((looking-at "[\[\(\{]") (forward-sexp arg))
   ((looking-back "[\]\)\}]" 1) (backward-sexp arg))
   ;; Otherwise, move from inside the bracket
   ((looking-at "[\]\)\}]") (forward-char) (backward-sexp arg))
   ((looking-back "[\[\(\{]" 1) (backward-char) (forward-sexp arg))
   (t (up-list arg t t))))

;; Launch IRC
(defun ndegruchy/irc ()
  (interactive)
  (circe "Libera Chat"))

;; Presentation settings
;; Modified from:
;; https://config.daviwil.com/emacs#org-present
(defun ndegruchy/presentation-settings ()
  (setq visual-fill-column-width 110
		visual-fill-column-center-text t)
  (visual-fill-column 1))

(defun ndegruchy/prepare-slide ()
  (org-overview)
  (org-show-children)
  (org-show-entry))

(defun ndegruchy/present-hook ()
  (setq-local face-remapping-alist '((default (:height 1.5))
                                     (header-line (:height 4.5))
                                     (org-code (:height 1.55) org-code)
                                     (org-verbatim (:height 1.55) org-verbatim)
                                     (org-block (:height 1.25) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))
			  (setq header-line-format " ")
			  (org-display-inline-images)
			  (ndegruchy/prepare-slide))

(defun ndegruchy/present-quit-hook ()
  (setq-local face-remapping-alist '((default default)))
  (setq header-line-format nil)
  (org-present-small)
  (org-remove-inline-images))

(defun ndegruchy/present-prev ()
  (interactive)
  (org-present-prev)
  (ndegruchy/prepare-slide))

(defun ndegruchy/present-next ()
  (interactive)
  (org-present-next)
  (ndegruchy/prepare-slide))

(defun dwim-shell-command-unzip ()
  (interactive)
  (dwim-shell-command-on-marked-files
   "Unzip" "atool --extract --explain ' '"
   :utils "atool"))
