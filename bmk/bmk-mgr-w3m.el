;;; bmk-mgr-w3m.el --- w3m specific code for bmk-mgr

;; Copyright (C) 2007  Jose Antonio Ortega Ruiz.
;;
;; Author: Robert D. Crawford
;; Author: Jose A Ortega Ruiz <jao@gnu.org>
;; Keywords: hypermedia

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;;  Set up bmk-mgr for w3m.

;;; Code:

;;;; Dependencies:

(require 'bmk-mgr)
(require 'w3m)

(defsubst bmk-mgr-w3m-url-at-point ()
  "Return the url at point in w3m."
  (or (w3m-anchor (point)) (w3m-image (point))))

(defsubst bmk-mgr-w3m-current-url ()
  "Returns the value of w3m-current-url."
  w3m-current-url)

(add-hook 'w3m-fontify-after-hook
          (lambda ()
            (setq bmk-mgr-document-title 'w3m-current-title)
            (setq bmk-mgr-url-at-point 'bmk-mgr-w3m-url-at-point)
            (setq bmk-mgr-current-url 'bmk-mgr-w3m-current-url)))

(bmk-mgr-import-add-formatter "w3m" 'bmk-mgr-w3m-import)

(defun bmk-mgr-w3m-import (file name)
  (if (not (file-readable-p file)) (error "Cannot read file"))
  (with-temp-buffer
    (let ((result (bmk-mgr-node-folder-new (or name "w3m")))
          (coding-system-for-read
           (if (boundp 'w3m-bookmark-file-coding-system)
               w3m-bookmark-file-coding-system
             coding-system-for-read))
          (sec-delim (if (boundp 'w3m-bookmark-section-delimiter)
                         w3m-bookmark-section-delimiter
                       "<!--End of section (do not delete this comment)-->\n")))
      (insert-file-contents file)
      (goto-char 1)
      (while (re-search-forward "<h2>\\([^<]+\\)</h2>\n<ul>\n" nil t)
        (let* ((folder
                (bmk-mgr-node-folder-new (match-string 1) t))
               (limit
                (save-excursion
                  (and (search-forward sec-delim nil t) (point)))))
          (while (search-forward "<li><a href=\"" limit t)
            (if (re-search-forward "\\([^\"]+\\)\">\\([^<]+\\)</a>\n" nil t)
                (bmk-mgr-node-add-child
                 folder
                 (bmk-mgr-node-url-new (match-string 2) (match-string 1)))))
          (bmk-mgr-node-add-child result folder)))
      result)))

(provide 'bmk-mgr-w3m)

;; Local variables **
;; indent-tabs-mode: nil  **
;; end **

;;; bmk-mgr-w3m.el ends here
