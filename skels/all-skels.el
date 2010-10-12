;;; all-skels.el --- Convenience package loading all skels

;; Copyright (C) 2008  Jose Ortega

;; Author: Jose Ortega <jao@google.com>
;; Keywords: languages

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

;;; Commentary:

;; Require this file to load all defined skels

;;; Code:

(require 'init-skel)

(require 'cpp-skel)
(require 'cppunit-skel)
(require 'perl-skel)
(require 'readme-skel)
(require 'make-skel)
(require 'caml-skel)
(require 'latex-skel)
(require 'noweb-skel)
(require 'lisp-skel)
(require 's48-skel)
(require 'haskell-skel)
(require 'scsh-skel)
(require 'lisa-skel)
(require 'texinfo-skel)
(require 'python-skel)
(require 'muse-skel)
(require 'asdf-skel)

(provide 'all-skels)

;;; all-skels.el ends here
