(require 'org)

;;; links
(defun jao-org-link-at-point ()
  (when (thing-at-point-looking-at "\\[\\[\\([^]]+\\)\\]\\[[^]]+\\]\\]")
    (match-string-no-properties 1)))

(defun jao-org-copy-link-at-point ()
  (interactive)
  (message "%s" (or (jao-org-link-at-point) "No link at point")))

;;; eldoc
(defun jao-org-eldoc--hook ()
  (set (make-local-variable 'eldoc-documentation-function)
       'jao-org-link-at-point)
  (eldoc-mode))

(defun jao-org-utils-eldoc-setup ()
  (add-hook 'org-mode-hook 'jao-org-eldoc--hook))

;;; play fair with saveplace
(defun jao-org--show-if-hidden ()
  (when (outline-invisible-p)
    (save-excursion
      (outline-previous-visible-heading 1)
      (org-show-subtree))))

;;; verifying org refile targets
(defun jao-org--refile-target-verify ()
  (not (looking-at-p ".*\\[\\[.+$")))

(defun jao-org-utils-setup ()
  (setq org-refile-target-verify-function 'jao-org--refile-target-verify)
  (add-hook 'org-mode-hook 'jao-org--show-if-hidden t))


(provide 'jao-org-utils)
