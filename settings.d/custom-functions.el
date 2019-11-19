;; custom-functions.el
;; Set custom functions here

;; Insert the date (formatted)
(defun ndegruchy/insert-date (format)
  "Wrapper around format-time-string"
  (interactive "MFormat: ")
  (insert (format-time-string format)))

;; Kill all open dired buffers
(defun ndegruchy/kill-all-dired-buffers ()
  "Kill all dired buffers."
  (interactive)
  (save-excursion
    (let((count 0))
      (dolist(buffer (buffer-list))
        (set-buffer buffer)
        (when (equal major-mode 'dired-mode)
          (setq count (1+ count))
          (kill-buffer buffer)))
      (message "Killed %i dired buffer(s)." count ))))

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

(defun ndegruchy/sudo-save ()
  "Save the buffer using sudo.
   Shamelessly stolen from: https://www.reddit.com/r/emacs/comments/aoqcyl/third_trial_for_a_weekly_tipstricksetc_thread/eg9n7wp/"
  (interactive)
  (let* ((split (cl-subseq (split-string (buffer-file-name (current-buffer)) "/") 1))
         (split2 (split-string (first split) ":"))
         (has-colons (= 3 (length split2))))
    (if has-colons
        (write-file (concat "/" (s-join ":" (append (list "sudo") (cl-subseq split2 1))) "/" (s-join "/" (cl-subseq split 1))))
      (if (not buffer-file-name)
          (write-file (concat "/sudo::" (read-file-name "File:")))
        (write-file (concat "/sudo::" buffer-file-name))))))

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

(defun ndegruchy/open-in-external-app (&optional @fname)
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference.

When called in emacs lisp, if @fname is given, open that.

URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2019-01-18"
  (interactive)
  (let* (
         ($file-list
          (if @fname
              (progn (list @fname))
            (if (string-equal major-mode "dired-mode")
                (dired-get-marked-files)
              (list (buffer-file-name)))))
         ($do-it-p (if (<= (length $file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when $do-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda ($fpath)
           (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" $fpath t t)))
	 $file-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda ($fpath)
           (shell-command
            (concat "open " (shell-quote-argument $fpath))))
	 $file-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda ($fpath) (let ((process-connection-type nil))
                            (start-process "" nil "xdg-open" $fpath)))
	 $file-list))))))

(defun protect-buffers ()
  (let ((protected '("*scratch*" "*Messages*")))
    (dolist (buf protected)
      (with-current-buffer buf
        (emacs-lock-mode 'kill)))))

;; Copied from: https://oremacs.com/2015/01/12/dired-file-size/
(defun dired-get-size ()
  "Use the `du' shell command to calculate the size of the marked
files."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message
       "Size of all marked files: %s"
       (progn
         (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*total$")
         (match-string 1))))))
