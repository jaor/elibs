;;; pika variants of c skeletons

;; Copyright (C) 2004, 2005 Jose Antonio Ortega Ruiz

;; Author: Jose A Ortega Ruiz <jao@gnu.org>
;; Keywords: tools

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

;; Pika variants for c skeletons

;;; Code:

(require 'common-skel)

(defun jao-pika-guard ()
  (upcase (concat "include__" (jao-basedir) "__" (jao-basename) "_h")))

(define-skeleton jao-pika-header
  ""
  "Brief file description: "
  "/* " (file-name-nondirectory (buffer-file-name)) ": " str
  \n "*" > \n
  "****************************************************************"
  > \n (jao-copyright-line "* ")
  > \n "*" > \n
  "* See the file \"COPYING\" for further information about"
  > n
  "* the copyright and warranty status of this work."
  > n
  "*/" \n "" \n _)

(define-skeleton jao-skel-pika-h
  "Standard pika c header"
  nil
  (jao-pika-header)
  '(setq guard (jao-pika-guard))
  "#ifndef " guard \n
  "#define " guard \n
  ""
  \n \n "#include \"" _ "\""\n \n
  ""
  \n \n \n
  ""
  \n
  "#endif /* " guard " */"
  \n \n "" \n
  (jao-arch-line "/* " "*/")
  \n)

(define-skeleton jao-skel-pika-c
  "Standard pika c body"
  nil
  (jao-pika-header)
  \n "#include \"" (jao-dir-level 2) ".h\"" \n
  \n
  ""
  \n \n _ \n \n "" \n
  (jao-arch-line "/* " "*/")
  \n)

(defun jao-skel-pika-activate ()
  (interactive)
  (let ((c (assoc "\\.c$" auto-insert-alist))
        (h (assoc "\\.h$" auto-insert-alist)))
    (if c (setf (cdr c) 'jao-skel-pika-c)
      (add-to-list 'auto-insert-alist '("\\.c$" . jao-skel-pika-c)))
    (if h (setf (cdr h) 'jao-skel-pika-h)
      (add-to-list 'auto-insert-alist '("\\.h$" . jao-skel-pika-h)))))


(provide 'pika-skel)

