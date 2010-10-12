;;; lisp-skel.el --- skeleton for lisp-like languages

;; Copyright (C) 2003, 2004, 2005, 2008, 2009  Jose A Ortega Ruiz

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

;; Skeleton for lisp like languages

;;; Code:

(require 'common-skel)

(define-skeleton jao-skel-lisp-file
  "Lisp file header"
  "Brief description: "
  ";; " (file-name-nondirectory (buffer-file-name)) " -- " str ""
  ?\n
  (jao-c&co-line ";; ")
  ?\n
  (jao-insert-copyright-file)
  \n ";;; Comentary: " \n \n ";; " _ \n
  \n ";;; Code: " \n \n \n \n
  '(when (eq major-mode 'emacs-lisp-mode)
     (insert (format "\n(provide '%s)\n" (jao-basename))))
  ";;; " (file-name-nondirectory (buffer-file-name)) " ends here"
  \n
  \n)

(jao-provide-skel "\\.\\(scm\\|ss\\|lisp\\|cl\\|el\\)\\'" 'jao-skel-lisp-file)

(provide 'lisp-skel)
;;; lisp-skel.el ends here
