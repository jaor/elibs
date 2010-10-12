(require 'jao-dominating-file)

(defun jao-haskell-locate-cabal-file ()
  (jao-locate-dominating-file ".+\\.cabal"))

(eval-after-load 'haskell-mode
  '(add-hook 'haskell-mode-hook
             (lambda ()
               (set (make-local-variable 'compile-command) "cabal build"))))

(defun jao-haskell-cabal-build ()
  (interactive)
  (let ((cabal-file (jao-haskell-locate-cabal-file)))
    (unless cabal-file
      (error "Couldn't find associated cabal file"))
    (let ((default-directory (file-name-directory cabal-file)))
      (call-interactively 'compile))))

;;(eval-after-load 'haskell-mode
;;  '(define-key haskell-mode-map [?\C-c ?c] 'jao-haskell-cabal-build))

(provide 'jao-cabal)
