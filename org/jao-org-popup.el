;;; frame popups
;; http://metajack.im/2008/12/30/gtd-capture-with-emacs-orgmode/
(defsubst jao-remember--frame-p ()
  (equal "*Remember*" (frame-parameter nil 'name)))

(defadvice remember-finalize (after delete-remember-frame activate)
  "Advise remember-finalize to close the frame if it is the remember frame"
  (when (jao-remember--frame-p) (delete-frame)))

(defadvice remember-destroy (after delete-remember-frame activate)
  "Advise remember-destroy to close the frame if it is the remember frame"
  (when (jao-remember--frame-p) (delete-frame)))

;; make the frame contain a single window. by default org-remember
;; splits the window.
(defun jao-remember--delete-other-windows ()
  (when (jao-remember--frame-p) (delete-other-windows)))

(add-hook 'remember-mode-hook 'jao-remember--delete-other-windows)

(defun make-remember-frame ()
  "Create a new frame and run org-remember"
  (interactive)
  (make-frame-on-display (getenv "DISPLAY")
                         '((name . "*Remember*")
                           (width . 80)
                           (height . 10)))
  (select-frame-by-name "*Remember*")
  (org-remember nil ?x))

(provide 'jao-org-popup)