;;; asdf-skel.el --- Skels for ASDF system definition files

;; Copyright (C) 2007  Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Keywords: lisp

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
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'common-skel)

(define-skeleton jao-skel-asdf-file
  "ASDF file header"
  "Description: "
  '(setq sys (jao-basename))
  '(lisp-mode)
  ";; -*- lisp -*- " sys " definition"
  \n \n
  "(defpackage " sys "-system" \n "  (:use :common-lisp :asdf))"
  \n \n
  "(in-package " sys "-system)"
  \n \n
  "(defsystem " sys
  > \n ":description \"" str "\""
  > \n ":version \"0.1\""
  > \n ":author \"" (user-full-name) " <" user-mail-address ">\""
  > \n ":maintainer \"" (user-full-name) " <" user-mail-address ">\""
  > \n ":licence \"GPL\""
  > \n ":depends-on ()"
  > \n ":components ((:file \"packages\")))"
  \n \n)

(add-to-list 'auto-insert-alist '("\\.asd\\'" . jao-skel-asdf-file))


(provide 'asdf-skel)
;;; asdf-skel.el ends here
