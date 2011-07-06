(require 'jao-org-utils)
(require 'jao-devon)

(autoload 'jao-as-safari-doc "jao-applescript.el")

;; doc links
(defvar jao-org--sink-dir "./")
(org-add-link-type "doc" 'jao-org-follow-doc 'identity)
(defun jao-org-follow-doc (link)
  (let ((dest-path (concat "./doc/"
                           (and (boundp 'docs-dir)
                                (concat (symbol-name docs-dir) "/"))
                           link)))
    (when (not (file-exists-p dest-path))
      (let* ((sink-file (expand-file-name link jao-org--sink-dir))
             (real-file (if (file-exists-p sink-file) sink-file
                          (read-file-name "Import file: "
                                          jao-org--sink-dir link link))))
        (shell-command (format "mv %s %s" real-file dest-path))))
    (browse-url (format "file://%s" (expand-file-name  dest-path)))))

;; devon links
(org-add-link-type "x-devonthink-item" 'jao-devon-open 'identity)

(defun jao-org-insert-devon-link ()
  (interactive)
  (insert (jao-devon-selection)))

(defsubst jao-org--title->file (title)
  (concat (mapconcat 'downcase (split-string title nil t) "-") ".pdf"))

(defun jao-org-insert-doc (title)
  (interactive "sDocument title: ")
  (insert (format "[[doc:%s][%s]]" (jao-org--title->file title) title)))

(defun jao-org-links-setup (sink-dir)
  (setq jao-org--sink-dir (file-name-as-directory sink-dir)))

(defun jao-org-insert-safari-link ()
  (interactive)
  (let ((ln (jao-as-safari-doc)))
    (when ln (jao-org-insert-link (car ln) (cdr ln)))))


(provide 'jao-org-links)
