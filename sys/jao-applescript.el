;;; AppleScript and some macish bits
(autoload 'applescript-mode "applescript-mode"
   "major mode for editing AppleScript source." t)
(setq auto-mode-alist
      (cons '("\\.applescript$" . applescript-mode) auto-mode-alist))

(defun do-applescript (script)
  (with-temp-buffer
    (insert script)
    (shell-command-on-region (point-min) (point-max) "osascript" t)
    (buffer-string)))

(defun jao-as-tell-app (app something)
  (let ((res (do-applescript (format "tell application \"%s\"\n%s\nend tell"
                                     app something))))
    (or (and (stringp res) (substring res 0 -1)) "")))

(defmacro jao-as-get-doc (name application &optional doc)
  `(defun ,name ()
     (interactive)
     (let ((url (jao-as-tell-app ,application
                                 ,(format "get the URL of %s 1"
                                          (or doc "document"))))
           (name (jao-as-tell-app ,application "get the name of document 1")))
       (cons url name))))
(jao-as-get-doc jao-as-safari-doc "Safari")
(jao-as-get-doc jao-as-webkit-doc "WebKit")
(jao-as-get-doc jao-as-camino-doc "Camino" "window")

(defun jao-as-firefox-doc ()
  (interactive)
  (let ((url (shell-command-to-string
              (concat "osascript "
                      (expand-file-name "furl.applescript"
                                        (file-name-directory load-file-name)))))
        (name (jao-as-tell-app "Firefox" "get the name of window 1")))
    (cons (substring url 0 -1) name)))


;;; quicksilver
(defun jao-qs-buffer ()
  "Opens the current file in Quicksilver"
  (interactive)
  (cond ((and buffer-file-name (file-exists-p buffer-file-name))
         (call-process-shell-command (concat "qs \"" buffer-file-name "\"")))
        ;; dired handling
        ((eq major-mode 'dired-mode)
         (dired-do-shell-command "qs * "
                                 current-prefix-arg
                                 (dired-get-marked-files t current-prefix-arg)))
         ;; buffer-menu mode
        ((and (eq major-mode 'Buffer-menu-mode)
              (file-exists-p (buffer-file-name (Buffer-menu-buffer nil))))
         (call-process-shell-command
          (concat "qs \"" (buffer-file-name (Buffer-menu-buffer nil)) "\"")))
        (t
         (error "Not visiting a file or file doesn't exist"))))

 (defun jao-qs-region (start end)
   "Opens the contents of the region in Quicksilver as text."
   (interactive "r")
   (call-process-region start end "qs" nil 0 nil "-"))


(provide 'jao-applescript)
