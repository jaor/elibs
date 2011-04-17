(defun jao--gnus-message-id ()
  (require 'org-gnus)
  (let ((header (with-current-buffer gnus-summary-buffer
                  (gnus-summary-article-header))))
    (and header (org-remove-angle-brackets (mail-header-id header)))))

(defun jao-gnus-open-in-mail ()
  (interactive)
  (let ((id (jao--gnus-message-id)))
    (unless id (error "no message selected"))
    (shell-command-to-string (format "open 'message:<%s>'" id))))

(provide 'jao-osx-mail)
