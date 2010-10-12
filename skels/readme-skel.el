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

;; Simple skeleton for README files.

;;; Code:

(require 'common-skel)

(define-skeleton jao-skel-readme-file
  "README file header"
  "Brief description: "
  \n str
  \n "-----------------------------------------------------" \n
  _ \n \n \n
  "-----------------------------------------------------" \n
  (jao-copyright-line "" "")
  \n \n
  "$Id" "$"
  \n)

(add-to-list 'auto-insert-alist '("README" . jao-skel-readme-file))

(provide 'readme-skel)

