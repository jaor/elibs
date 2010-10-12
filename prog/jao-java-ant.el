(require 'jao-dominating-file)

(eval-after-load 'cc-mode
  '(progn
     (add-hook 'java-mode-hook
               (lambda ()
                 (set (make-local-variable 'compile-command) "ant")))
     (define-key java-mode-map "\C-cc" 'jao-java-ant-build)))

(defun jao-java-ant-build ()
  (interactive)
  (let ((build-file (jao-locate-dominating-file "build\\.xml")))
    (unless build-file
      (error "Couldn't find associated build file"))
    (let ((default-directory (file-name-directory build-file)))
      (call-interactively 'compile))))

(provide 'jao-java-ant)

;; End of jao-java-ant.el
