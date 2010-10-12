;;; python-skel.el

;; Copyright (C) 2004, 2005, 2009 Aleix Conchillo Flaque

;; Author: Aleix Conchillo Flaque <aleix@member.fsf.org>
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

;; Python skeletons

;;; Code:

(require 'common-skel)

(define-skeleton jao-skel-python-module
  "Standard python module header"
  "Brief file description: "
  "#! /usr/bin/env python"
  > \n
  "#"
  > \n
  "# $Id" "$"
  > \n \n
  "# " (file-name-nondirectory (buffer-file-name)) ": " str
  > \n
  "#"
  > ?\n
  (jao-copyright-line "# ")
  > ?\n
  (jao-insert-copyright-file)
  > \n
  > \n _)

(jao-provide-skel "\\.py$" 'jao-skel-python-module)

(provide 'python-skel)

