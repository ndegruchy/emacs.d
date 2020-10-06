(defun notmuch-show-icalendar-send-buffer-by-mail (calendar-reply status)
  (let ((message-signature nil))
    (notmuch-show-reply-sender)
    (message-goto-body)
    (delete-region (point) (point-max))
    (mml-insert-multipart "alternative")
    (mml-insert-empty-tag 'part 'type "text/plain")
    (mml-insert-part "text/calendar; method=REPLY; charset=UTF-8")
    (insert calendar-reply "\n")

    (let* ((re-subject
	    (save-restriction
	      (message-narrow-to-headers-or-head)
	      (message-fetch-field "subject")))
	   (subject (concat (capitalize (symbol-name status))
			    (substring re-subject 2))))
      (message-goto-subject)
      (delete-region (line-beginning-position) (line-end-position))
      (insert "Subject: " subject)
      (message-goto-body))))

(defun notmuch-show-icalendar-reply (status handle)
  (let* ((reply (gnus-icalendar-with-decoded-handle handle
                  (gnus-icalendar-event-reply-from-buffer
                   (current-buffer) status (notmuch-user-emails)))))
    (when reply
      (with-temp-buffer
	(insert reply)
	(goto-char (point-min))
	(while (re-search-forward "^\\(.\\{72\\}\\)\\(.+\\)$" nil t)
	  (replace-match "\\1\n \\2")
	  (goto-char (line-beginning-position)))
	(buffer-substring-no-properties (point-min) (point-max))))))

(defun notmuch-show-calendar-reply-part ()
  "Reply to calendar invite."
  (interactive)
  (let* ((part (notmuch-show-get-part-properties))
	 (computed-type (plist-get part :computed-type)))
    (unless (notmuch-match-content-type computed-type "text/calendar")
      (error "Not a calendar part!"))
    (require 'gnus-icalendar)
    (let* ((str-status (completing-read "Accept, decline or tentative? "
					(list "accept" "decline" "tentative") nil 't))
	   (status (pcase str-status
		     ("accept" 'accepted)
		     ("decline" 'declined)
		     ("tentative" 'tentative))))
      (when status
	(let ((calendar-part
	       (notmuch-show-apply-to-current-part-handle
		(apply-partially #'notmuch-show-icalendar-reply status))))
	  ;; Back in show buffer
	  (notmuch-show-icalendar-send-buffer-by-mail calendar-part status))))))
