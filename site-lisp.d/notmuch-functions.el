(defun message-recipients ()
  "Return a list of all recipients in the message, looking at TO,
CC and BCC. Each recipient is in the format of
`mail-extract-address-components'."
  (mapcan (lambda (header)
            (let ((header-value (message-fetch-field header)))
              (and
               header-value
               (mail-extract-address-components header-value t))))
          '("To" "Cc" "Bcc")))

(defun message-all-epg-keys-available-p ()
  "Return non-nil if the pgp keyring has a public key for each recipient."
  (require 'epa)
  (let ((context (epg-make-context epa-protocol)))
    (catch 'break
	  (dolist (recipient (message-recipients))
        (let ((recipient-email (cadr recipient)))
		  (when (and recipient-email (not (epg-list-keys context recipient-email)))
            (throw 'break nil))))
	  t)))

(defun message-sign-encrypt-if-all-keys-available ()
  "Add MML tag to encrypt message when there is a key for each
recipient. Consider adding this function to `message-send-hook'
to systematically send encrypted emails when possible."
  (when (message-all-epg-keys-available-p)
    (mml-secure-message-sign-encrypt)))

(defun nm/tree-delete()
  "Deletes a message from tree view"
  (interactive)
  (notmuch-tree-tag (list "+delete" "-unread" "-inbox")))

(defun nm/tree-trash()
  "Move a message into the trash from tree view"
  (interactive)
  (notmuch-tree-tag (list "+trash" "-unread" "-inbox")))
  
(defun nm/tree-sent()
  "Marks a message as sent"
  (interactive)
  (notmuch-tree-tag (list "+sent" "-inbox" "-unread")))

(defun nm/search-delete()
  "Deletes a message from search view"
  (interactive)
  (notmuch-search-tag (list "+delete" "-unread" "-inbox")))

(defun nm/search-trash()
  "Deletes a message from search view"
  (interactive)
  (notmuch-search-tag (list "+trash" "-unread" "-inbox")))

(defun nm/search-sent()
  "Marks a message as sent"
	(interactive)
	(notmuch-search-tag (list "+sent" "-inbox" "-unread")))
