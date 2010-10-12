;;; texinfo-skel.el --- skeletons for texinfo files

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Keywords: languages

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

;; Skeletons to generate texinfo files templates.

;;; Code:

(require 'common-skel)

(defun jao-dir-entry ()
  "Read dir file entry"
  (let ((cat (read-string "Dir file category: "))
        (ent (read-string "Direntry name: "))
        (desc (read-string "Direntry description: ")))
    (concat "@dircategory " cat
            "\n@direntry\n" ent
            ": (" (jao-basename) ").        " desc "."
            "\n@end direntry\n")))

(define-skeleton jao-skel-main-texinfo
  "Main texinfo file skeleton"
  "Document title: "
  "\\input texinfo"
  \n "@ignore" >
  \n (jao-scm-line "")
  "@end ignore" >
  \n > "@c %**start of header"
  \n "@setfilename " (jao-basename) ".info" >
  \n "@settitle " str >
  \n "@syncodeindex pg cp" >
  \n "@setchapternewpage odd" >
  \n "@footnotestyle separate" >
  \n "@c %**end of header" >
  \n \n
  (jao-dir-entry)
  \n
  "@set UPDATED " (format-time-string "%B %Y")
  \n "@set EDITION 0.1"
  \n "@set VERSION 0.1"
  \n "@set AUTHOR " (user-full-name)
  \n \n "@copying"
  \n "This manual is for " str " (version @value{VERSION}, @value{UPDATED})."
  \n
  \n "Copyright @copyright{} " (format-time-string "%Y") " " jao-company-name
  \n
  \n "@quotation"
  \n "Permission is granted to copy, distribute and/or modify this document"
  \n "under the terms of the GNU Free Documentation License, Version 1.1 or"
  \n "any later version published by the Free Software Foundation; with no"
  \n "Invariant Sections, with the Front-Cover Texts being ``A GNU Manual,''"
  \n "and with the Back-Cover Texts as in (a) below.  A copy of the"
  \n "license is included in the section entitled ``GNU Free Documentation"
  \n "License.''"
  \n
  \n "(a) The FSF's Back-Cover Text is: ``You have freedom to copy and modify"
  \n "this GNU Manual, like GNU software.  Copies published by the Free"
  \n "Software Foundation raise funds for GNU development.''"
  \n "@end quotation"
  \n "@end copying"
  \n \n "@titlepage"
  \n "@title " str
  \n "@subtitle Edition @value{EDITION}, for version @value{VERSION}"
  \n "@subtitle @value{UPDATED}"
  \n "@author by @value{AUTHOR} (@email{jao@@gnu.org})"
  \n "@page"
  \n "@vskip 0pt plus 1filll"
  \n "@insertcopying"
  \n "@end titlepage"
  \n
  \n "@shortcontents"
  \n "@contents"
  \n
  \n "@ifnottex"
  \n "@node Top, , (dir), (dir)"
  \n \n "@insertcopying"
  \n "@end ifnottex"
  \n \n
  "@menu" > \n "@detailmenu" \n \n > "@end detailmenu" > \n "@end menu"
  \n \n \n
  "@include intro.texi" >
  \n \n \n
  "@bye"
  \n)

(define-skeleton jao-skel-child-texinfo
  "Template for child texinfo docs"
  "Node name: "
  "@node " str \n
  > "@chapter " str \n
  > "@ignore" \n
  (jao-scm-line "")
  > "@end ignore" \n \n
  _ \n \n
  "@c This is part of the " (read-string "Main doc title: ") \n
  "@c See the main file for copying conditions."
  \n \n)

(define-skeleton jao-skel-texinfo
  "Skeleton for texinfo files"
  nil
  '(if (y-or-n-p "Is this the main texinfo file? ")
       (jao-skel-main-texinfo)
     (jao-skel-child-texinfo))
  _)

(add-to-list 'auto-insert-alist '("\\.texi$" . jao-texinfo-skel))


(provide 'texinfo-skel)


;;; texinfo-skel.el ends here
