;; geiser-skel.el -- geiser skeletons

;; Copyright (C) 2009 Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Fri Sep 11, 2009 00:31

(require 'common-skel)

(defconst jao-skel-geiser--bsd
  ";; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.
")

(defsubst jao-skel-geiser--end-line ()
  (format ";;; %s ends here\n\n" (file-name-nondirectory (buffer-file-name))))

(define-skeleton jao-skel-geiser--common
  "Geiser elisp header"
  "Brief description: "
  ";;; " (file-name-nondirectory (buffer-file-name)) " -- " str ""
  \n \n
  (jao-copyright-line ";; ") \n
  jao-skel-geiser--bsd
  \n (jao-date-line ";; ") \n)

(define-skeleton jao-skel-geiser-elisp
  "Geiser elisp header"
  nil
  '(jao-skel-geiser--common)
  "" \n _ \n \n "" \n "(provide '" (jao-basename) ")" \n
  (jao-skel-geiser--end-line))

(jao-provide-skel "geiser/elisp/.+\\.el\\'" 'jao-skel-geiser-elisp)

(define-skeleton jao-skel-geiser-scheme
  "Geiser scheme header"
  nil
  '(jao-skel-geiser--common) _
  \n (jao-skel-geiser--end-line))

(jao-provide-skel "geiser/scheme/.+\\.\\(scm\\|ss\\|sls\\)\\'"
                  'jao-skel-geiser-scheme)



(provide 'geiser-skel)
;;; geiser-skel.el ends here
