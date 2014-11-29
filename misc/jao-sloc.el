;; sloc.el -- LOC utilities

(defun count-sloc-region (beg end kind)
  "Count source lines of code in region (or (narrowed part of)
   the buffer when no region is active).  SLOC means that empty
   lines and comment-only lines are not taken into consideration.

   (function by Stefan Monnier).
  "
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end) 'region)
     (list (point-min) (point-max) 'buffer)))
  (save-excursion
    (goto-char beg)
    (let ((count 0))
      (while (< (point) end)
        (cond
         ((nth 4 (syntax-ppss)) ;; BOL is already inside a comment.
          (let ((pos (point)))
            (goto-char (nth 8 (syntax-ppss)))
            (forward-comment (point-max))
            (if (< (point) pos) (goto-char pos)))) ;; Just paranoia
         (t (forward-comment (point-max))))
        (setq count (1+ count))
        (forward-line))
      (when kind
        (message "SLOC in %s: %s." kind count)))))


(provide 'jao-sloc)
;;; sloc.el ends here
