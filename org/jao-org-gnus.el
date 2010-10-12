;; Support for saving Gnus messages by Message-ID
(defun mde-org-gnus-save-by-mid ()
  (when (memq major-mode '(gnus-summary-mode gnus-article-mode))
    (when (eq major-mode 'gnus-article-mode)
      (gnus-article-show-summary))
    (let* ((group gnus-newsgroup-name)
           (method (gnus-find-method-for-group group)))
      (when (eq 'nnml (car method))
        (let* ((article (gnus-summary-article-number))
               (header (gnus-summary-article-header article))
               (from (mail-header-from header))
               (message-id
                (save-match-data
                  (let ((mid (mail-header-id header)))
                    (if (string-match "<\\(.*\\)>" mid)
                        (match-string 1 mid)
                      (error "Malformed message ID header %s" mid)))))
               (date (mail-header-date header))
               (subject (gnus-summary-subject-string)))
          (org-store-link-props :type "mid" :from from :subject subject
                                :message-id message-id :group group
                                :link (org-make-link "mid:" message-id))
          (apply 'org-store-link-props
                 :description (org-email-link-description)
                 org-store-link-plist)
          t)))))

(defvar mde-mid-resolve-methods '()
  "List of methods to try when resolving message ID's.  For Gnus,
it is a cons of 'gnus and the select (type and name).")
(setq mde-mid-resolve-methods
      '((gnus nnml "")))

(defvar mde-org-gnus-open-level 1
  "Level at which Gnus is started when opening a link")
(defun mde-org-gnus-open-message-link (msgid)
  "Open a message link with Gnus"
  (require 'gnus)
  (require 'org-table)
  (catch 'method-found
    (message "[MID linker] Resolving %s" msgid)
    (dolist (method mde-mid-resolve-methods)
      (cond
       ((and (eq (car method) 'gnus)
             (eq (cadr method) 'nnml))
        (funcall (cdr (assq 'gnus org-link-frame-setup))
                 mde-org-gnus-open-level)
        (when gnus-other-frame-object
          (select-frame gnus-other-frame-object))
        (let* ((msg-info (nnml-find-group-number
                          (concat "<" msgid ">")
                          (cdr method)))
               (group (and msg-info (car msg-info)))
               (message (and msg-info (cdr msg-info)))
               (qname (and group
                           (if (gnus-methods-equal-p
                                (cdr method)
                                gnus-select-method)
                               group
                             (gnus-group-full-name group (cdr method))))))
          (when msg-info
            (gnus-summary-read-group qname nil t)
            (gnus-summary-goto-article message nil t))
          (throw 'method-found t)))
       (t (error "Unknown link type"))))))

(eval-after-load 'org-gnus
  '(progn
     (add-to-list 'org-store-link-functions 'mde-org-gnus-save-by-mid)
     (org-add-link-type "mid" 'mde-org-gnus-open-message-link)))

(provide 'jao-org-gnus)
