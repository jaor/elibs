;;; haskell-skel.el --- skeleton for haskell source files
;; Copyright (C) 2003, 2004, 2005, 2009, 2010, 2012 Jose A Ortega Ruiz

;; Author: Jose A Ortega Ruiz <jao@member.fsf.org>
;; Keywords: languages

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

;;

;;; Code:

(require 'common-skel)
(require 'jao-dominating-file)

;;; Auxiliar
(defun jao-skel--read-haskell-module ()
  (let* ((ddir (jao-relative-path "\\.cabal\\'"))
         (mbase (and ddir (concat (replace-regexp-in-string "/" "." ddir)
                                  ".")))
         (m (read-string "Module prefix (empty for no module): "
                         (concat (or mbase "") (jao-basename)))))
    (or m "")))

(defconst jao-skel--haskell-line (make-string 78 ?-))

;;; Skeletons
(define-skeleton jao-skel-haskell-file
  "Haskell hs file header"
  "Brief description: "
  '(setq v (jao-skel--read-haskell-module))
  jao-skel--haskell-line \n
  "-- |" \n
  "-- Module: " v \n
  (jao-copyright-line "-- Copyright: " "" t)
  "-- License: BSD3-style (see LICENSE)" \n
  "--" \n
  "-- Maintainer: " user-mail-address \n
  "-- Stability: unstable" \n
  "-- Portability: portable" \n
  "-- Created: " (format-time-string "%a %b %d, %Y %H:%M") \n
  "--" \n
  "--" \n
  "-- " str \n
  "--" \n
  jao-skel--haskell-line
  \n \n \n
  "module " v " where " \n \n \n)

(jao-provide-skel "\\.hs\\'"  'jao-skel-haskell-file)
;; (jao-provide-skel "\\.lhs\\'" 'jao-skel-lit-haskell-file)

(provide 'haskell-skel)

;;; haskell-skel.el ends here
