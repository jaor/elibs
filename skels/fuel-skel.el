;;; fuel-skel.el --- skeleton for fuel elisp files

;; Copyright (C) 2008  Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'common-skel)

(define-skeleton jao-skel-fuel-file
  "Fuel file header"
  "Brief description: "
  ";;; " (file-name-nondirectory (buffer-file-name)) " -- " str ""
  \n \n
  (jao-copyright-line ";; ")
  ";; See http://factorcode.org/license.txt for BSD license."
  \n \n (jao-author-line ";; ") ";; Keywords: languages, fuel, factor"
  \n (jao-date-line ";; ")
  \n ";;; Comentary: " \n \n ";; " _ \n
  \n ";;; Code: " \n \n \n \n
  "" \n "(provide '" (jao-basename) ")" \n
  ";;; " (file-name-nondirectory (buffer-file-name)) " ends here"
  \n
  \n)

(jao-provide-skel "misc/fuel/.+\\.el\\'" 'jao-skel-fuel-file)


(provide 'fuel-skel)
;;; fuel-skel.el ends here
