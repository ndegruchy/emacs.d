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

(defun ndegruchy/protect-buffers ()
	(let ((protected '("*scratch*" "*Messages*")))
		(dolist (buf protected)
			(with-current-buffer buf
				(emacs-lock-mode 'kill)))))

(defun ndegruchy/split-name (s)
  "Find the word seperators and split them into a list"
  (split-string
   (let ((case-fold-search nil))
	 (downcase
	  (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" s)))
   "[^A-Za-z0-9]+"))

(defun ndegruchy/dasherize (s)
  "Takes a string and splits it across word boundaries, inserting
dashes where those boundaries would be. This is very useful when
dealing with making URL slugs, or other computer tokens."
  (mapconcat 'downcase
			 (ndegruchy/split-name s) "-"))

;; Make
(defun ndegruchy/make ()
  "Runs `make -k`, searching 'up' a directory tree to find a
valid Makefile to run against"
  (interactive)
  (let ((default-directory (locate-dominating-file "." "Makefile")))
	(compile "make -k")))

(defun append-to-list (list-var elements)
	"Append ELEMENTS to the end of LIST-VAR.

The return value is the new value of LIST-VAR."
	(unless (consp elements)
		(error "ELEMENTS must be a list"))
	(let ((list (symbol-value list-var)))
		(if list
			(setcdr (last list) elements)
			(set list-var elements)))
	(symbol-value list-var))

(defun ndegruchy/up-directory (path)
  "Move up a directory in PATH without affecting the kill buffer.

Taken, shamelessly, from: https://www.reddit.com/r/emacs/comments/re31i6/how_to_go_up_one_directory_when_using_findfile_cx/"
  (interactive "p")
  (if (string-match-p "/." (minibuffer-contents))
      (let ((end (point)))
	    (re-search-backward "/.")
	    (forward-char)
	    (delete-region (point) end))))
