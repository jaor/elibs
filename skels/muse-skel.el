;;; muse-skel.el --- muse pages

;; Copyright (C) 2006  Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
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
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'common-skel)

(define-skeleton jao-skel-muse-file
  "Muse file skeleton"
  "Documents (sub)dir: "
  _ \n \n \n \n \n \n \n
  "----" \n
  ";;; Local Variables:" \n
  ";;; wiki-docs: " str \n
  ";;; End:" \n \n
   '(hack-local-variables))

(add-to-list 'auto-insert-alist
             '("\\.muse\\'" . jao-skel-muse-file))

(provide 'muse-skel)
;;; muse-skel.el ends here
