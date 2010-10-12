;;; lisa variants of c skeletons

;; Copyright (C) 2004, 2005, 2006 Jose Antonio Ortega Ruiz

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

;; Lisa variants for c skeletons

;;; Code:

(require 'common-skel)

(defun jao-lisa-filename ()
  (let* ((fname (expand-file-name (buffer-file-name)))
         (parts (split-string fname "/"))
         (dirs (member "src" parts)))
    (mapconcat 'identity dirs "/")))


(defun jao-lisa-guard ()
  (upcase (concat (jao-basename)
                  "_" (jao-extension) "_"
                  (format-time-string "%y%m%d%H%M"))))

(defun jao-lisa-header (&optional desc group prf)
  (concat "/**"
          "\n * @file     " (jao-lisa-filename)
          "\n * @author   " (user-full-name) " <"user-mail-address">"
          "\n * @date     " (format-time-string "%a %b %d, %Y %H:%M")
          "\n * @brief    " (or desc (read-string "Description: "))
          (if group (concat "\n * @ingroup  " group) "")
          (if prf (concat "\n * @uutrx    " prf) "")
          "\n **/\n\n"
          (jao-arch-line "// " "")))

(define-skeleton jao-skel-lisa-h
  "Standard lisa c header"
  nil
  '(setq guard (jao-lisa-guard))
  (jao-lisa-header)
  \n \n
  "#ifndef " guard \n
  "#define " guard \n
  \n \n "#include \"" _ "\"" \n \n \n
  "" \n "// Types" \n \n \n
  "" \n "// Constants" \n \n \n
  "" \n "// Functions" \n \n \n
  \n \n
  "#endif // " guard
  \n \n)

(define-skeleton jao-skel-lisa-c
  "Standard lisa c body"
  nil
  (jao-lisa-header (concat (jao-other-file-name "h" "c") " implementation"))
  \n \n
  "#include \""
  (jao-basename)
  ".h\"" >
  > \n \n \n
  "" \n "// Private" \n \n \n
  _
  "" \n "// Public" \n \n \n
  )

(define-skeleton jao-skel-lisa-test
  "Cantata++ test file"
  nil
  '(setq v1 (read-string "File under test (sans extension): "))
  '(setq v0 (read-string "Doxygen group under test: "))
  '(setq v2 (concat "UnitTest" v0))
  (jao-lisa-header (concat "Unit tests for " v1)
                   v2
                   (read-string "Prefix of functions being tested (e.g. 'rtos_?+'): "))
  \n \n
  "#include \"test/test.h\"" > \n
  "#include \"" v1 ".h\"" > \n \n \n
  "// Test name"  > \n
  "char const *test_name = \"" (concat v1 "_test") "\";" > \n \n
  "// Prototypes for test functions" > \n
  "/**" \n
  "* @defgroup " v2 " Unit tests" > \n
  "* @ingroup " v0 > \n
  "**/" > \n
  "//@{" > \n
  "//@}" > \n
  \n \n \n
  "void" > \n
  "run_tests (void)" > \n
  "{" > \n
  "}" > \n \n
  "// Test functions" > \n \n \n)

(defun jao-add-cantata-test ()
  "Call this function inside a test buffer to add a new test fun"
  (interactive)
  (let* ((fn (read-string "Function under test: "))
         (tfn (concat "test_" fn)))
    (goto-char (point-min))
    (if (not (search-forward-regexp "^// Prototypes for test functions$" nil t))
        (error "No beginning of test fun declarations found"))
    (if (not (search-forward-regexp "//@\\}$" nil t))
        (error "Missing doxygen group marks in prototype function decls"))
    (beginning-of-line)
    (open-line 1)
    (insert "/**\n * Unit tests for @ref " fn "\n */\n")
    (insert "static void " tfn " (void);\n")
    (if (not (search-forward-regexp "run_tests (void)$" nil t))
        (error "No run_tests() definition found"))
    (if (not (search-forward-regexp "^}" nil t))
        (error "End of run_tests() not found"))
    (beginning-of-line)
    (insert "\n")
    (previous-line 1)
    (insert tfn " ();")
    (indent-according-to-mode)
    (goto-char (point-max))
    (jao-insert-cantata-test-fun tfn)))

(defun jao-insert-cantata-test-fun (fn)
  (beginning-of-line)
  (insert "void\n" fn " (void)\n{\n")
  (insert "START_TEST (\"" fn
          "\", \"" (read-string "Test case description: ") "\");")
  (indent-according-to-mode)
  (insert "\n\n\nEND_TEST ();")
  (indent-according-to-mode)
  (insert "\n}\n"))


(defun jao-skel-lisa-activate ()
  (interactive)
  (jao-provide-skel "\\.c$" 'jao-skel-lisa-c)
  (jao-provide-skel "\\.h$" 'jao-skel-lisa-h)
  (jao-provide-skel "tests/.*\\.c$" 'jao-skel-lisa-test))


(provide 'lisa-skel)

