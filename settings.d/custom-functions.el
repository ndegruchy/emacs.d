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

;; Mirror vim's "open" lines above and below
(defun ndegruchy/open-line-below ()
  "Inserts a line below the current line, moving the cursor to
that line and setting the indent properly"
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun ndegruchy/open-line-above ()
  "Inserts a line above the current line, moving the cursor to
that line and setting the indent properly"
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

;; Create parent folder(s) when visiting a non-existant file
(defun ndegruchy/my-create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))
