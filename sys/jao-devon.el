;; DEVONthink interaction

(require 'jao-applescript)

(defconst *jao-devon-sep* "####")

(defun jao-devon-path (dvp)
  (car (split-string dvp *jao-devon-sep*)))
(defun jao-devon-url (dvp)
  (cadr (split-string dvp *jao-devon-sep*)))
(defun jao-devon-name (dvp)
  (car (last (split-string (jao-devon-path dvp) "/"))))

(defun jao-devon-make-dvp (path url) (concat path *jao-devon-sep* name))
(defun jao-devon-dvp-p (dvp)
  (and (stringp dvp)
       (string-match (concat "^/.+" *jao-devon-sep*) dvp)))

(defconst *jao-devon-sel-as*
 (concat "set rs to the selection
  set r to item 1 of rs
  set rn to the name of r
  set rl to the location of r
  set ru to the URL of r
  rl & rn & \"" *jao-devon-sep* "\" & ru"))

(defun jao-devon-selection ()
  (interactive)
  (jao-as-tell-app "DEVONThink Pro" *jao-devon-sel-as*))

(defun jao-devon-open-as (path)
  (concat "set r to get record at \"" path "\""
          "\n open window for record r\n activate"))

(defun jao-devon-open (dvp)
  (if (eq system-type 'darwin)
      (let ((path (jao-devon-path dvp)))
        (when path
          (jao-as-tell-app "DEVONThink Pro" (jao-devon-open-as path) t)))
    (browse-url (jao-devon-url dvp))))

(provide 'jao-devon)
