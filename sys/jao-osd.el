;; candy
(defvar jao-osd-cat-color-fg "black")
(defvar jao-osd-cat-color-bg "white")
(defvar jao-osd-cat-font "Andika Basic 16")
;; (setq jao-osd-cat-font "Inconsolata 20")
(defun jao-osd-cat-font (&optional font)
  (or font jao-osd-cat-font))

(defun jao-osd-process-args (&optional font fg bg)
  `("-n" ,(jao-osd-cat-font font)
    "-R" ,(or bg jao-osd-cat-color-fg) "-B" ,(or fg jao-osd-cat-color-bg)
    "-b" "200" "-r" "255"
    "-e" "0" "-t" "2" "-d" "10" "-p" "0" "-x" "10" "-y" "10" "-u" "5000"))

(setq jao-osd-processes (make-hash-table))

(defsubst jao-osd--delete-process (name)
  (remhash name jao-osd-processes))

(defun jao-osd-process (name &optional font color)
  (let ((proc (gethash name jao-osd-processes)))
    (or (and proc (eq (process-status proc) 'run) proc)
        (puthash name
                 (apply 'start-process
                        `("notifications"
                          ,(format "*notifications/%s*" name)
                          "aosd_cat"
                          ,@(jao-osd-process-args)))
                 jao-osd-processes))))

(defun jao-osd-cat (name lines)
  (let* ((proc (jao-osd-process name))
         (lines (if (listp lines) lines (list lines)))
         (trail (- 5 (length lines))))
    (when proc
      (dolist (line lines)
        (send-string proc (format "%s\n" line))))))
      ; (when (> trail 0) (send-string proc (make-string trail ?\n))))))

(defun jao-osd--names ()
  (let (names)
    (maphash (lambda (n k) (push n names)) jao-osd-processes)
    (reverse names)))

(defun jao-osd-kill (name)
  (let ((proc (gethash name jao-osd-processes)))
    (when (processp proc)
      (kill-process proc))))

(defun jao-osd-kill-notifiers ()
  (interactive)
  (maphash (lambda (n p) (ignore-errors (kill-process p))) jao-osd-processes)
  (clrhash jao-osd-processes))

(defun jao-notify (msg &optional title icon)
  (let ((args (if title (format "'%s' '%s'" msg title) (format "'%s'" msg)))
        (iflag (if (and (stringp icon) (file-exists-p icon))
                   (format " -i %s" icon)
                 "")))
    (shell-command-to-string (format "notify-send %s%s" args iflag))))

(provide 'jao-osd)
