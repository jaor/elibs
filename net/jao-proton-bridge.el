;; jao-proton-bridge.el -- simple interaction with ProtonMail's bridge

;; Copyright (c) 2018 Jose Antonio Ortega Ruiz

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
