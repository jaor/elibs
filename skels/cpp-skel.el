;;; cpp-skel.el

;; Copyright (C) 2004, 2005, 2008, 2009 Jose Antonio Ortega Ruiz

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

;; C++ skeletons.

;;; Code:

(require 'common-skel)
(require 'thingatpt)

;;; Variables
(defvar jao-skel-cpp-root-namespace nil
  "The root C++ namespace")

(defvar jao-skel-cpp-brief-header-p nil
  "If non-nil, generate brief header comments")

(defvar jao-skel-cpp-make-guard-function #'jao-skel-cpp-make-guard-name
  "Function generating #include guards")

(defvar jao-skel-cpp-use-namespaces t
  "Whether to generate namespaces")

(defvar jao-skel-cpp-single-line-namespaces t
  "Whether to put consecutive namespace decls in a single line")

(defvar jao-skel-cpp-header-extension "hpp")

;;; Auxiliar functions
(defun jao-skel-cpp--find-other (ext)
  (file-name-nondirectory
   (or (ff-other-file-name)
       (concat (file-name-sans-extension (buffer-name)) "." ext))))

(defun jao-skel-cpp-make-guard-name (ns)
  "Create a standard include guard name"
  (upcase (mapconcat #'identity
                     `(,@ns ,(jao-basename) ,(jao-extension)
                            ,(user-login-name)
                            ,(format-time-string "%y%m%d%H%M"))
                     "_")))

;; namespaces
(defsubst jao-skel-cpp--read-ns (curr)
  (read-string (format "Add namespace (current: %s): " (or curr "[none]"))))

(defsubst jao-skel-cpp--ns2str (ns) (mapconcat 'identity ns "::"))

(defun jao-skel-cpp--get-ns-list (&optional acc)
  (do* ((result acc (cons next result))
        (next (jao-skel-cpp--read-ns (jao-skel-cpp--ns2str acc))
              (jao-skel-cpp--read-ns (jao-skel-cpp--ns2str (reverse result)))))
      ((string= next "") (reverse result))))

(defun jao-skel-cpp--insert-open-ns-list (ns)
  (dolist (n ns)
    (insert (format "namespace %s {%s"
                    n
                    (if jao-skel-cpp-single-line-namespaces " " "\n")))
    (indent-according-to-mode))
  (when jao-skel-cpp-single-line-namespaces
    (newline)
    (indent-according-to-mode)))

(defun jao-skel-cpp--insert-close-ns-list (ns)
  (if jao-skel-cpp-single-line-namespaces
      (insert (format "%s // namespace %s\n"
                      (make-string (length ns) ?})
                      (jao-skel-cpp--ns2str ns)))
    (dolist (n (reverse ns))
      (insert (format "}  // namespace %s\n" n)))))

(defun jao-skel-cpp--copy-ns-lines ()
  (let ((lines))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "namespace\\s-\\w+\\s-{\\|}+\\s-//\\s-namespace"
                                nil t)
        (push (thing-at-point 'line) lines)
        (next-line)))
    lines))

(defun jao-skel-cpp--copy-namespace ()
  (let* ((name (ff-other-file-name))
         (buff (and name (find-file-noselect name)))
         (nlines))
    (when buff
      (let ((lines (save-current-buffer
                     (set-buffer buff)
                     (jao-skel-cpp--copy-ns-lines))))
        (dolist (line lines)
          (push line nlines)
          (when (string-match "}" line)
            (push "\n\n\n\n" nlines)))))
    (mapconcat #'identity nlines "\n")))

(defsubst jao-skel-cpp--get-new-namespace ()
  (when jao-skel-cpp-use-namespaces
    (jao-skel-cpp--get-ns-list
     (and jao-skel-cpp-root-namespace (list jao-skel-cpp-root-namespace)))))

;; skeletons
(define-skeleton jao-skel-cpp-header-long
  "Initial file header blurb"
  "Brief file description: "
  "/**"
  > \n
  "* @file     " (file-name-nondirectory (buffer-file-name))
  > \n
  "* @brief    " str
  > \n
  "* @author   " (user-full-name) " <"user-mail-address">"
  > \n
  "* @date     " (format-time-string "%a %b %d, %Y %H:%M")
  > \n
  "*"
  > \n
  (jao-copyright-line "* " "")
  "*"
  > ?\n
  (jao-insert-copyright-file)
  > \n \n _)

(define-skeleton jao-skel-cpp-header-brief
  "Brief initial header blurb"
  nil
  (jao-copyright-line "/* " " */")
  \n)

(define-skeleton jao-skel-cpp-header-comment
  "Insert a standard comment block"
  nil
  '(if jao-skel-cpp-brief-header-p
       (jao-skel-cpp-header-brief)
     (jao-skel-cpp-header-long)))

;; source C/C++ file ------------------------------------------------------
(define-skeleton jao-skel-cpp-source-header
  "Insert a standard C++ source header"
  nil
  '(jao-skel-cpp-header-comment)
  ? \n
  "#include \"" (jao-skel-cpp--find-other jao-skel-cpp-header-extension) "\""
  > \n \n _
  (jao-skel-cpp--copy-namespace)
  \n)

(define-skeleton jao-skel-c-source-header
  "Insert a standard C source header"
  nil
  '(jao-skel-cpp-header-comment)
  "#include \"" (jao-skel-cpp--find-other "h") "\""
  > _ \n \n \n \n
  (jao-scm-line "/* " " */")
  > \n)


;; header C/C++ files ------------------------------------------------------
;; header guard

;; class definition
(define-skeleton jao-skel-cpp-class-def
  "Insert a class definition"
  nil
  '(setq v1 (jao-basename))
  > \n
  "/**"
  > \n
  "*"
  > \n
  "*"
  > \n
  "*/"
  > \n
  "class " v1
  > \n
  "{"
  > \n
  "public:"
  > \n
  "~" v1 "();"
  > \n
  v1 "();"
  > \n
  v1 "(const " v1 "& other);"
  > \n \n
  "private:"
  > \n
  "};"
  > \n)

(define-skeleton jao-skel-cpp-header
  "Insert a standard C++ header (hpp files)"
  nil
  '(setq v1 (jao-skel-cpp--get-new-namespace))
  '(setq v2 (funcall jao-skel-cpp-make-guard-function v1))
  '(jao-skel-cpp-header-comment)
  > \n
  "#ifndef " v2
  > \n
  "#define " v2
  > \n \n
  '(when v1 (jao-skel-cpp--insert-open-ns-list v1))
  _ '(jao-skel-cpp-class-def)
  > \n \n
  '(when v1 (jao-skel-cpp--insert-close-ns-list v1))
  > \n \n
  "#endif // " v2
  > \n)

(define-skeleton jao-skel-c-header
  "Insert a standard C header (.h files)"
  nil
  '(jao-skel-cpp-header-comment)
  > \n
  '(setq v1 (funcall jao-skel-cpp-make-guard-function nil))
  "#ifndef " v1
  > \n
  "#define " v1
  > _ \n \n \n \n
  "#endif /* " v1 " */"
  > \n \n
  (jao-scm-line "/* " " */")
  > \n)

(jao-skel-provide
 '(("\\.cpp$" jao-skel-cpp-source-header)
   ("\\.hpp$" jao-skel-cpp-header)
   ("\\.c$" jao-skel-c-source-header)
   ("\\.h$" jao-skel-c-header)))

(provide 'cpp-skel)

;;; cpp-skel.el ends here
