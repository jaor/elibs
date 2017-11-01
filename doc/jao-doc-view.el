;; jao-doc-view.el -- Remembering visited documents

;; Copyright (c) 2013, 2015, 2017 Jose Antonio Ortega Ruiz

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

(defun jao-doc-view-purge-bmks ()
  (interactive)
  (let ((ht jao-doc-view--current-bmks))
    (when ht
      (maphash (lambda (k v)
                 (when (or (= 1 v) (not (file-exists-p k)))
                   (remhash k ht)))
               ht))))

(defun jao-doc-view-goto-bmk ()
  (interactive)
  (when (eq major-mode 'pdf-view-mode)
    (let* ((bmks (jao-doc-view--current-bmks))
           (fname (buffer-file-name))
           (p (when fname (gethash (expand-file-name fname) bmks 1))))
      (when (and (numberp p) (> p 1))
        (message "Found bookmark at page %d" p)
        (ignore-errors (pdf-view-goto-page p))))))

(defun jao-doc-view-open (file)
  (let* ((buffs (buffer-list))
         (b (catch 'done
              (while buffs
                (when (string-equal (buffer-file-name (car buffs)) file)
                  (throw 'done (car buffs)))
                (setq buffs (cdr buffs))))))
    (if b (pop-to-buffer b) (find-file file))))

(defun jao-doc-view-session (&optional file)
  (let ((file (or file jao-doc-view-session-file)))
    (jao-doc-view--read-file file)))

(defun jao-doc-view-load-session (&optional file)
  (interactive)
  (let ((docs (jao-doc-view-session file)))
    (when (not (listp docs)) (error "Empty session"))
    (dolist (d docs) (jao-doc-view-open d))))

(defun jao-doc-view--save-bmks ()
  (jao-doc-view-purge-bmks)
  (jao-doc-view--save-to-file jao-doc-view-bmk-file
                              (jao-doc-view--current-bmks)))

(defun jao-doc-view--save-bmk (&rest ignored)
  (when (eq major-mode 'pdf-view-mode)
    (ignore-errors
      (puthash (buffer-file-name)
               (max (pdf-view-current-page) 1)
               (jao-doc-view--current-bmks)))))

(defun jao-doc-view-save-session (&optional skip-current)
  (interactive)
  (let ((docs '())
        (cb (when skip-current (current-buffer))))
    (dolist (b (buffer-list))
      (with-current-buffer b
        (when (and (equalp major-mode 'pdf-view-mode)
                   (not (equalp cb b)))
          (jao-doc-view--save-bmk)
          (add-to-list 'docs (buffer-file-name)))))
    (jao-doc-view--save-bmks)
    (when (> (length docs) 0)
      (jao-doc-view--save-to-file jao-doc-view-session-file docs))))

(defun jao-doc-view--save-session-1 ()
  (when (equalp major-mode 'pdf-view-mode)
    (jao-doc-view-purge-bmks)
    (jao-doc-view-save-session t)))

(defun jao-doc-view-install ()
  (jao-doc-view--current-bmks)
  (add-hook 'kill-buffer-hook 'jao-doc-view--save-bmk)
  (add-hook 'kill-buffer-hook 'jao-doc-view--save-session-1 t)
  (add-hook 'kill-emacs-hook 'jao-doc-view-save-session))



(provide 'jao-doc-view)
