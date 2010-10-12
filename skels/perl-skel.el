;;; perl-skel.el

;; Copyright (C) 2004, 2005, 2009 Jose Antonio Ortega Ruiz

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

;; Perl skeletons

;;; Code:

(require 'common-skel)

(define-skeleton jao-skel-perl-script
  "Standard perl script header"
  "Brief file description: "
  "#! /usr/bin/perl -w"
  > \n
  "#"
  > \n
  "# $Id" "$"
  > \n
  "# " (file-name-nondirectory (buffer-file-name)) ": " str
  > \n
  "#"
  > ?\n
  (jao-copyright-line "# ")
  > ?\n
  (jao-insert-copyright-file)
  > \n
  "use strict;"
  > \n \n
  > \n _)

(define-skeleton jao-skel-perl-module
  "Standard perl module header"
  "Brief module description: "
  "#"
  > \n
  "# " (file-name-nondirectory (buffer-file-name)) ": "str
  > \n
  "#"
  > ?\n
  (jao-copyright-line "# ")
  > ?\n
  (jao-insert-copyright-file)
  "# "
  > \n \n
  "package "
  (read-string (concat "Module name (" (jao-basename) "): ")
	       nil nil (jao-basename))
  ";"
  > \n \n _ \n \n
  "1;"
  > \n)

(jao-provide-skel "\\.pl$" 'jao-skel-perl-script)
(jao-provide-skel "\\.pm$" 'jao-skel-perl-module)

(provide 'perl-skel)

