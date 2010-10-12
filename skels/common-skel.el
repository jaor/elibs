;; common definitions and functions

;; Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009, 2010 Jose Antonio Ortega Ruiz

;; Author: Jose A Ortega Ruiz <jao@gnu.org>
;; Keywords: tools

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

;; Aux functions used in other skeletons

;;; Code:

(require 'skeleton)

(defvar jao-company-name nil
  "Company name used in copyright notice")

(defvar jao-copyright-file ".copyright"
  "Basename of the raw (uncommented) copyright file")

(defvar jao-skels-default-scm nil
  "Default SCM system")

(defun jao-prefix (pref) (or pref (concat comment-start " ")))
(defun jao-suffix (suff) (or suff (concat " " comment-end)))

(defun jao-copyright-line (prefix &optional suffix omit-cpy)
  "Create a brief copyright notice with given PREFIX and SUFFIX"
  (concat (jao-prefix prefix)
          (if omit-cpy "" "Copyright ")
          "(c) " (format-time-string "%Y") " "
          (or jao-company-name (user-full-name))
          (jao-suffix suffix) "\n"))

(defun jao-date-line (prefix &optional suffix)
  "Create a start date line"
  (concat (jao-prefix prefix)
          "Start date: " (format-time-string "%a %b %d, %Y %H:%M")
          (jao-suffix suffix) "\n"))

(defun jao-author-line (prefix &optional suffix)
  "Create an author date line"
  (concat (jao-prefix prefix)
          "Author: " (user-full-name) " <" user-mail-address "> "
          (jao-suffix suffix) "\n"))

(defun jao-cvs-line (prefix &optional suffix)
  "Create a CVS ID line"
  (concat (jao-prefix prefix) "$" "Id$" (jao-suffix suffix) "\n"))

(defun jao-svn-line (prefix &optional suffix)
  "Create a SVN ID line"
  (concat (jao-prefix prefix) "X-SVN: $" "Id$" (jao-suffix suffix) "\n"))

(defun jao-arch-line (&optional prefix suffix)
  "Create an arch-tag line"
  (let ((uuid (shell-command-to-string "uuidgen")))
    (concat (jao-prefix prefix) "arch-tag: " uuid (jao-suffix suffix) "\n")))

(defun jao-insert-arch-line ()
  (interactive)
  (insert (jao-arch-line)))

(defun jao-scm-line (prefix &optional suffix)
  "Create an scm line"
  (let* ((formats '(("arch" . jao-arch-line)
                    ("svn" . jao-svn-line)
                    ("cvs" . jao-cvs-line)
                    ("none" . (lambda (p f) ""))))
         (names (mapcar 'car formats))
         (prompt (concat "SCM (" (mapconcat 'identity names ", ") "): "))
         (sel (or jao-skels-default-scm
                  (completing-read prompt formats nil 1)))
         (fun (cdr (assoc sel formats))))
    (when fun (concat (funcall fun prefix suffix)))))

(defun jao-c&co-line (&optional prefix suffix)
  (concat (jao-scm-line prefix suffix) "\n"
          (jao-co-line prefix suffix)))

(defun jao-co-line (&optional prefix suffix)
  (concat (jao-copyright-line prefix suffix) "\n"
          (jao-author-line prefix suffix)
          (jao-date-line prefix suffix)))

;; aux functions ---------------------------------------------------------
(defun jao-basename ()
  "Get buffer file name without dir nor extension"
  (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))

(defun jao-basedir ()
  "Base directory"
  (file-name-nondirectory
   (substring (file-name-directory (buffer-file-name)) 0 -1)))

(defun jao-dir-level (l)
  (let ((elems (split-string
                (file-name-sans-extension (buffer-file-name)) "/")))
    (mapconcat 'identity (nthcdr (- (length elems) (+ 1 l)) elems) "/")))

(defun jao-extension ()
  "Find the extension of the currently visited file"
  (let ((elems (split-string (file-name-nondirectory (buffer-file-name))
                             "\\.")))
    (nth (- (length elems) 1) elems)))

(defun jao-other-file-name (ext1 ext2)
  "Find the complimentary file name of header/source file"
  (let ((extension (jao-extension))
        (basename (jao-basename)))
    (if (string= extension ext1) (concat basename "." ext2)
      (concat basename "." ext1))))

(defun jao-insert-commented-file (file-name)
  (let* ((start (point))
         (end (+ start (cadr (insert-file-contents file-name)))))
    (goto-char end)
    (comment-region start (point))))

(defun jao-insert-copyright-file ()
  (let ((dir (locate-dominating-file (buffer-file-name) jao-copyright-file)))
    (when dir
      (let ((file (expand-file-name jao-copyright-file dir)))
        (when (file-exists-p file)
          (jao-insert-commented-file file))))))

(defun jao-provide-skel (regexp skel)
  (let ((ex (assoc regexp auto-insert-alist)))
    (if ex (setf (cdr ex) skel)
      (add-to-list 'auto-insert-alist (cons regexp skel)))))

(defsubst jao-skel-provide (lst)
  (mapc (lambda (x) (apply #'jao-provide-skel x))  lst))

(provide 'common-skel)
