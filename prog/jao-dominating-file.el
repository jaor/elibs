(defun jao-locate-dominating-files (regexp &optional file)
  "Look up the directory hierarchy from FILE for a file matching REGEXP.
  Stop at the first parent where a matching file is found and return the list
  of files that that match in this directory."
  (catch 'found
    (let ((dir (file-name-as-directory (or file (buffer-file-name))))
          files)
      (while (and dir
                  (not (string-match locate-dominating-stop-dir-regexp
                                     dir)))
        (if (setq files (condition-case nil
                            (directory-files dir 'full regexp 'nosort)
                          (error nil)))
            (throw 'found files)
          (if (equal dir
                     (setq dir (file-name-directory
                                (directory-file-name dir))))
              (setq dir nil))))
      nil)))


(defun jao-locate-dominating-file (regexp &optional from)
  (car (jao-locate-dominating-files regexp from)))

(defun jao-relative-path (regexp &optional from)
  (let* ((from (or from (buffer-file-name)))
         (dfile (jao-locate-dominating-file regexp from))
         (ddir (and dfile (file-name-directory dfile)))
         (fdir (file-name-directory from)))
    (when ddir
      (and (string-match (format "%s\\(.+\\)/" (regexp-quote ddir)) fdir)
           (match-string-no-properties 1 fdir)))))

(provide 'jao-dominating-file)
