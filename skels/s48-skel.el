;;; s48-skel.el --- skeleton for s48

;; Copyright (C) 2003, 2004, 2005, 2006, 2008, 2009  Jose A Ortega Ruiz

;; Author: Jose A Ortega Ruiz <jao@gnu.org>
;; Keywords: lisp

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

;; Skeleton for s48/slime48 like languages

;;; Code:

(require 'common-skel)
(require 'lisp-skel)

(define-skeleton jao-skel-s48-file
  "Slime/Scheme48 file header"
  "Package: "
  ";; -*- mode: scheme48; scheme48-package: " str " -*-"
  ?\n
  (jao-co-line ";; ")
  ?\n
  (jao-insert-copyright-file)
  \n ";;; Comentary: " \n \n ";; " _ \n
  \n ";;; Code: " \n \n \n \n
  ";;; " (file-name-nondirectory (buffer-file-name)) " ends here"
  '(scheme48-mode)
  \n
  \n)

(define-skeleton jao-skel-s48-file-maybe
  "Choose between a s48 file and a plain scheme one"
  nil
  '(if (y-or-n-p "Is this a s48 file? ") (jao-skel-s48-file)
     (jao-skel-lisp-file))
    '(hack-local-variables))


(jao-provide-skel "\\.scm\\'" 'jao-skel-s48-file-maybe)


(provide 's48-skel)


;;; lisp-skel.el ends here
