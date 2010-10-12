;;; scsh-skel.el --- skeleton for scsh scripts

;; Copyright (C) 2003, 2004, 2005, 2006, 2008  Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Keywords: abbrev

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

(require 'common-skel)

(define-skeleton jao-skel-scsh
  "Scsh script skeleton"
  "Brief description: "
  "#! " (executable-find "scsh") " \\" \n
  "-e " (file-name-nondirectory (buffer-file-name)) " -s" \n
  "!#" \n \n
  ";;;; " str \n
  "(define (" (file-name-nondirectory (buffer-file-name)) " args)"
  \n
  > _ " )"
  > \n \n \n
  ";; Local Variables:" \n
  ";; mode: scheme" \n
  ";; End:"
  '(hack-local-variables)
  \n \n)

(provide 'scsh-skel)


;;; scsh-skel.el ends here
