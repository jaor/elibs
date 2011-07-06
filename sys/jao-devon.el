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
  set rl to the reference URL of r
  set ru to the URL of r
  \"[[\" & rl & \"" *jao-devon-sep*
  "\" & ru & \"][\" & rn & \"]]\""))

(defun jao-devon-selection ()
  (jao-as-tell-app "DEVONThink Pro" *jao-devon-sel-as*))

;; (defun jao-devon-open-as (path)
;;   (concat "set r to get record at \"" path "\""
;;           "\n open window for record r\n activate"))

(defun jao-devon-open (dvp)
  (if (eq system-type 'darwin)
      (let* ((path (jao-devon-path dvp))
             (cmd (and path (format "open x-devonthink-item:%s" path))))
        (when cmd (shell-command-to-string cmd)))
    (browse-url (jao-devon-url dvp))))

(defun jao-devon-add-html-page (title url html)
  (let ((as (format "tell application id \"com.devon-technologies.thinkpro2\" to create record with {name:%S, type:html, URL:%S, source:%S}"
                    title url html)))
    (do-applescript as)))

(defun jao-devon-add-w3m-page ()
  "Add current w3m page to devonthink."
  (interactive)
  (let ((title (w3m-current-title))
        (url w3m-current-url))
    (when url
      (w3m-view-source)
      (let ((html (buffer-substring-no-properties (point-min) (point-max))))
        (jao-devon-add-html-page title url html))
      (w3m-view-source))))

(provide 'jao-devon)
