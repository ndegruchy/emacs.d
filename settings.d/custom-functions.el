;;; custom-functions --- A collection of site-local functions for my emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Nathan DeGruchy

;; Author: Nathan DeGruchy <nathan@degruchy.org>
;; Keywords: local, data, processes, convenience

;; Insert the date (formatted)
(defun ndegruchy/insert-date ()
  "Inserts an ISO date by default, prefixing with C-u prints the
  full ISO date, double-prefixing asks for a format."
  (interactive)
  (if (null current-prefix-arg)
	  (insert (format-time-string "%F")))
  (if (eq (prefix-numeric-value current-prefix-arg) 4)
	  (insert (format-time-string "%FT%T%z")))
  (if (> (prefix-numeric-value current-prefix-arg) 4)
	  (insert (format-time-string (read-string "Format: ")))))

(defun ndegruchy/select-current-line-dwim (arg)
  "Select the current line and move the cursor by ARG lines IF no
region is selected.

If a region is already selected when calling this command, only
move the cursor by ARG lines.

Retrieved from https://emacs.stackexchange.com/a/15040/11870 on
2018-08-07"
  (interactive "p")
  (when (not (use-region-p))
	(forward-line 0)
	(set-mark-command nil))
  (forward-line arg))

(defun ndegruchy/duplicate-line ()
  "Makes a copy of the current line after the current line. Useful
for listing directories, etc."
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
  "Prevents certain defined buffers from being killed/deleted"
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

(defun ndegruchy/up-directory (path)
  "Move up a directory in PATH without affecting the kill buffer.

Taken, shamelessly, from: https://www.reddit.com/r/emacs/comments/re31i6/how_to_go_up_one_directory_when_using_findfile_cx/"
  (interactive "p")
  (if (string-match-p "/." (minibuffer-contents))
      (let ((end (point)))
	    (re-search-backward "/.")
	    (forward-char)
	    (delete-region (point) end))))

(defun ndegruchy/occur-dwim ()
  "Switch to occur, or the existing buffer"
  (interactive)
  (if (get-buffer "*Occur*")
	  (switch-to-buffer "*Occur*")
	(call-interactively 'occur)))

(defun ndegruchy/revert-buffer-noconfirm (&optional force-reverting)
  "Interactive call to revert-buffer. Ignoring the auto-save file
 and not requesting for confirmation. When the current buffer is
 modified, the command refuses to revert it, unless you specify
 the optional argument: force-reverting to true."
  (interactive "P")
  ;;(message "force-reverting value is %s" force-reverting)
  (if (or force-reverting (not (buffer-modified-p)))
      (revert-buffer :ignore-auto :noconfirm)
    (error "The buffer has been modified")))

(defun ndegruchy/forward-or-backward-sexp (&optional arg)
  "Go to the matching parenthesis character if one is adjacent to point.

Stolen from: https://www.emacswiki.org/emacs/NavigatingParentheses#h5o-3"
  (interactive "^p")
  (cond ((looking-at "\\s(") (forward-sexp arg))
        ((looking-back "\\s)" 1) (backward-sexp arg))
        ;; Now, try to succeed from inside of a bracket
        ((looking-at "\\s)") (forward-char) (backward-sexp arg))
        ((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))

(defun ndegruchy/capitalize-dwim (word)
  "Moves to the beginning of the word before capitalizing"
  (interactive "p")
  (save-excursion
	(backward-word)
	(capitalize-word word)))

;; Local Variables:
;; truncate-lines: t
;; End:
