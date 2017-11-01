;; jao-recoll.el -- Displaying recoll queries

;; Copyright (c) 2017 Jose Antonio Ortega Ruiz

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Wed Nov 01, 2017 18:14


;;; Comentary:

;; A simple interactive command to perform recoll queries and display
;; its results using org-mode.

;;; Code:


(require 'org)

(define-derived-mode recoll-mode org-mode "Recoll"
  "Simple mode for showing recoll query results"
  (read-only-mode 1))

(defvar jao-recoll--file-regexp
  "\\(\\w+/\\w+\\)\t+\\[\\([^]]+\\)\\]\t+\\[\\([^]]+\\)\\].+")

(defvar jao-recoll-flags "-A")

(defun jao-recoll (keywords)
  "Performs a query using recoll and shows the results in a
buffer using org mode."
  (interactive "sRecoll query string: ")
  (with-current-buffer (get-buffer-create (format "* Recoll: '%s' *" keywords))
    (read-only-mode -1)
    (delete-region (point-min) (point-max))
    (let ((c (format "recoll %s -t %s"
                     jao-recoll-flags (shell-quote-argument keywords))))
      (shell-command c t))
    (goto-char (point-min))
    (forward-line 2)
    (open-line 1)
    (while (search-forward-regexp jao-recoll--file-regexp nil t)
      (replace-match "* [[\\2][\\3]] (\\1)")
      (forward-line)
      (beginning-of-line)
      (let ((kill-whole-line nil)) (kill-line))
      (forward-line)
      (let ((p (point)))
        (re-search-forward "/ABSTRACT")
        (beginning-of-line)
        (fill-region p (point))
        (let ((kill-whole-line nil)) (kill-line))))
    (recoll-mode)
    (pop-to-buffer (current-buffer))))

(define-key recoll-mode-map [?n] 'org-next-link)
(define-key recoll-mode-map [?p] 'org-previous-link)
(define-key recoll-mode-map [?q] 'bury-buffer)
(define-key recoll-mode-map [?r] 'jao-recoll)



(provide 'jao-recoll)
;;; jao-recoll.el ends here
