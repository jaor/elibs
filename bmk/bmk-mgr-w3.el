;;; bmk-mgr-w3.el --- w3 specific code for bmk-mgr

;; Copyright (C) 2007, 2008  Jose Antonio Ortega Ruiz.
;;
;; Author: Robert D. Crawford
;; Author: Jose A Ortega Ruiz <jao@gnu.org>
;; Keywords: hypermedia

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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
;;  Set up bmk-mgr for w3.

;;; Code:

;;;; Dependencies:

(require 'bmk-mgr)
(require 'w3)

(defun bmk-mgr-w3-current-url ()
  "Returns the current document url
without the string properties."
  (interactive)
  (substring-no-properties (url-view-url)))

(defun bmk-mgr-w3-document-title-fixed ()
  "Removes the newline in long titles that
seems to have cropped up in current versions of w3."
  (replace-regexp-in-string "\n" " " (buffer-name)))

(add-hook 'w3-mode-hook
          (lambda ()
            (setq bmk-mgr-document-title
		  'bmk-mgr-w3-document-title-fixed)
            (setq bmk-mgr-url-at-point 'w3-view-this-url)
            (setq bmk-mgr-current-url 'bmk-mgr-w3-current-url)))
;;            (setq bmk-mgr-document-title 'buffer-name)
(provide 'bmk-mgr-w3)

;; Local variables **
;; indent-tabs-mode: nil  **
;; end **
;;; bmk-mgr-w3.el ends here
