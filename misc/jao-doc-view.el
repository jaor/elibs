;; jao-doc-view.el -- Remembering visited documents

;; Copyright (c) 2013 Jose Antonio Ortega Ruiz

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
;; Start date: Fri Feb 15, 2013 01:21

;;; Comentary:

;; Some utilities to keep track of visited documents and the last
;; visited page.

;;; Code:

(defvar jao-doc-view-bmk-file "~/.emacs.d/doc-view-bmk")
(defvar jao-doc-view--current-bmks nil)

(defun jao-doc-view--read-bmks ()
  (let* ((buff (find-file-noselect jao-doc-view-bmk-file))
         (bmks (ignore-errors
                 (with-current-buffer buff
                   (goto-char (point-min)))
                 (read buff))))
    (if (hash-table-p bmks) bmks (make-hash-table :test 'equal))))

(defun jao-doc-view--save-bmks ()
  (with-current-buffer (find-file-noselect jao-doc-view-bmk-file)
    (erase-buffer)
    (insert (format "%S" (jao-doc-view--current-bmks)))
    (save-buffer)))

(defun jao-doc-view--current-bmks ()
  (or jao-doc-view--current-bmks
      (setq jao-doc-view--current-bmks (jao-doc-view--read-bmks))))

(defun jao-doc-view-open (file)
  (let* ((buffs (buffer-list))
         (b (catch 'done
              (while buffs
                (when (string-equal (buffer-file-name (car buffs)) file)
                  (throw 'done (car buffs)))
                (setq buffs (cdr buffs))))))
    (when (not b)
      (find-file file)
      (doc-view-goto-page (gethash (expand-file-name file)
                                   (jao-doc-view--current-bmks) 1)))))

(defun jao-doc-view--save-hook ()
  (when (eq major-mode 'doc-view-mode)
    (ignore-errors (puthash (buffer-file-name)
                            (doc-view-current-page)
                            (jao-doc-view--current-bmks))
                   (jao-doc-view--save-bmks))))

(eval-after-load "doc-view"
  '(progn
     (add-hook 'kill-buffer-hook 'jao-doc-view--save-hook)
     (add-hook 'kill-emacs-hook 'jao-doc-view--save-bmks)))



(provide 'jao-doc-view)
