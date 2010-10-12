;;; noweb-skel.el --- skeleton for noweb files

;; Copyright (C) 2003, 2004, 2005  Jose Antonio Ortega Ruiz

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

;; Skeleton for noweb files

;;; Code:

(require 'common-skel)
(require 'latex-skel)

(define-skeleton jao-skel-noweb
  "Noweb standard header"
  "Code mode (without -mode suffix): "
  "% -*- mode: Noweb; noweb-code-mode: " str "-mode -*-"
  '(setq noweb-code-mode (intern (concat str "-mode")))
  \n
  '(jao-skel-latex)
  \n _ \n \n
  "%%% end of file"
  \n)

(add-to-list 'auto-insert-alist '("\\.nw$" . jao-skel-noweb))

(provide 'noweb-skel)


;;; noweb-skel.el ends here
