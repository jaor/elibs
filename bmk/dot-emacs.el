;;; sample initialisation file for bmk-mgr

(if (require 'bmk-mgr nil t)
    (progn
      (setq bmk-mgr-bookmark-file "~/.emacs.d/bookmarks")
      (setq bmk-mgr-inhibit-welcome-message nil)
      (setq bmk-mgr-inhibit-minibuffer t)
      (setq bmk-mgr-use-own-frame nil)
      (setq bmk-mgr-use-images t)
      (setq bmk-mgr-ignore-fold-state t)

      (define-key bmk-mgr-mode-map "g" 'bmk-mgr-browse-url)
      (define-key bmk-mgr-mode-map "G" 'bmk-mgr-browse-url-alt)
      (global-set-key "\C-cB" 'bmk-mgr-show-bookmarks)
      (global-set-key "\C-cA" 'bmk-mgr-add-url-at-point)

      ;;;; integration with emacs-w3m (optional)
      (when (require 'w3m nil t)
        (require 'bmk-mgr-w3m)
        (defun browse-bmk-w3m (url &rest ig)
          (goto-w3m-buffer)
          (w3m-goto-url url t))

        (defun browse-bmk-new-tab-w3m (url &rest ig)
          (goto-w3m-buffer)
          (w3m-goto-url-new-session url t))

        (defun goto-w3m-buffer () (interactive)
          (let ((display-buffer-reuse-frames 1)
                (pop-up-windows nil)
                (buffer (w3m-alive-p)))
            (if buffer (pop-to-buffer buffer))))

        (define-key bmk-mgr-mode-map "w" 'goto-w3m-buffer)
        (setq bmk-mgr-browser-function 'browse-bmk-w3m)
        (setq bmk-mgr-alt-browser-function 'browse-bmk-new-tab-w3m))

      ;; integration with w3
      (require 'bmk-mgr-w3)))

  (message "bookmark manager not available"))

