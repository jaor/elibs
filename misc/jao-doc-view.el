;; jao-doc-view.el -- Remembering visited documents

;; Copyright (c) 2013, 2015 Jose Antonio Ortega Ruiz

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
(defvar jao-doc-view-session-file "~/.emacs.d/doc-view-session")
(defvar jao-doc-view--current-bmks nil)

(defun jao-doc-view--save-bmk ()
  (when (or (eq major-mode 'doc-view-mode)
            (eq major-mode 'pdf-view-mode))
    (ignore-errors
      (puthash (buffer-file-name)
               (or (if (fboundp 'pdf-view-current-page)
                       (pdf-view-current-page)
                     (doc-view-current-page))
                   1)
               (jao-doc-view--current-bmks)))))

(defun jao-doc-view--read-file (file)
  (let ((buff (find-file-noselect file)))
    (ignore-errors
      (with-current-buffer buff
        (goto-char (point-min)))
      (read buff))))

(defun jao-doc-view--save-to-file (file value)
  (with-current-buffer (find-file-noselect file)
    (erase-buffer)
    (insert (format "%S" value))
    (save-buffer)))

(defun jao-doc-view--read-bmks ()
  (let ((bmks (jao-doc-view--read-file jao-doc-view-bmk-file)))
    (if (hash-table-p bmks) bmks (make-hash-table :test 'equal))))

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
      (let ((p (gethash (expand-file-name file)
                        (jao-doc-view--current-bmks)
                        0)))
        (when (and (numberp p) (> p 0))
          (if (fboundp 'pdf-view-goto-page)
              (pdf-view-goto-page p)
            (doc-view-goto-page p))))
      (jao-doc-view--save-bmks))))


(defun jao-doc-view-session (&optional file)
  (let ((file (or file jao-doc-view-session-file)))
    (jao-doc-view--read-file file)))

(defun jao-doc-view-load-session (&optional file)
  (interactive)
  (let ((docs (jao-doc-view-session file)))
    (when (not (listp docs)) (error "Empty session"))
    (dolist (d docs) (jao-doc-view-open d))))

(defun jao-doc-view--save-bmks ()
  (let ((docs '()))
    (dolist (b (buffer-list))
      (with-current-buffer b
        (when (jao-doc-view--save-bmk)
          (add-to-list 'docs (buffer-file-name)))))
    (jao-doc-view--save-to-file jao-doc-view-session-file docs))
  (jao-doc-view--save-to-file jao-doc-view-bmk-file
                              (jao-doc-view--current-bmks)))

(eval-after-load "doc-view"
  '(add-hook 'kill-buffer-hook 'jao-doc-view--save-bmk))



(provide 'jao-doc-view)
