;; jao-proton-bridge.el -- simple interaction with ProtonMail's bridge

;; Copyright (c) 2018 Jose Antonio Ortega Ruiz

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

;; Author: Jose Antonio Ortega Ruiz <mail@jao.io>
;; Start date: Fri Dec 21, 2018 23:56

;;; Comentary:

;;  This is a very simple comint-derived mode to run the CLI version
;;  of PM's Bridge within the comfort of emacs.

;;; Code:

(define-derived-mode proton-bridge-mode comint-mode "proton-bridge"
  "A very simple comint-based mode to run ProtonMail's bridge"
  (setq comint-prompt-read-only t)
  (setq comint-prompt-regexp "^>>> "))

(defun run-proton-bridge ()
  "Run or switch to an existing bridge process, using its CLI"
  (interactive)
  (pop-to-buffer (make-comint "proton-bridge" "Desktop-Bridge" nil "-c"))
  (unless (eq major-mode 'proton-bridge-mode)
    (proton-bridge-mode)))


(provide 'jao-proton-bridge)
;;; jao-proton-bridge.el ends here
