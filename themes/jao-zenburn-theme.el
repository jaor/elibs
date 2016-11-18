(require 'jao-themes)

(setq zenburn-override-colors-alist
      `(("zenburn-magenta" . "thistle")
        ("zenburn-cyan" . "LightSteelBlue1")
        ("zenburn-blue+1" . "LemonChiffon")
        ("zenburn-blue"   . "LemonChiffon1")
        ("zenburn-blue-1" . "LemonChiffon2")
        ("zenburn-blue-2" . "LemonChiffon3")
        ("zenburn-blue-3" . "LemonChiffon4")
        ("zenburn-blue-4" . "cadet blue")
        ("zenburn-blue-5" . "dark cyan")))

(require 'zenburn-theme)

;; (setq zenburn-colors-alist
;;   (append zenburn-default-colors-alist
;;           zenburn-override-colors-alist))

(load-theme 'zenburn t)

(zenburn-with-color-variables
  (let ((f (jao-themes-parse-faces
            `((circe-my-message-face (c "gray70"))
              (circe-originator-face (c ,zenburn-yellow-1))
              (compilation-info (c ,zenburn-yellow) nul)
              (compilation-error (c ,zenburn-red+1) nul)
              (diff-hl-change (c nil ,zenburn-blue-3))
              (diff-hl-delete (c nil ,zenburn-red-1))
              (diff-hl-insert (c nil ,zenburn-green-1))
              (diredp-date-time (c ,zenburn-yellow))
              (diredp-dir-name (c ,zenburn-blue-2) bf)
              (diredp-exec-priv (c ,zenburn-yellow-2))
              (diredp-write-priv (c ,zenburn-yellow-2))
              (font-lock-function-name-face (c ,zenburn-yellow) nbf)
              (fringe (c nil nil))
              (gnus-summary-cancelled (c ,zenburn-red) st)
              (header-line (c ,zenburn-fg ,zenburn-bg+1))
              (isearch (c nil ,zenburn-bg+1))
              (link (c ,zenburn-yellow) nbf nul)
              (link-visited (c ,zenburn-yellow-2) nbf nul)
              (lui-button-face (c ,zenburn-green+2))
              (lui-time-stamp-face (c ,zenburn-bg+3))
              (magit-diff-added-highlight (c ,zenburn-fg+1 ,zenburn-green))
              (magit-hash (c ,zenburn-green))
              (match (c ,zenburn-orange) nbf)
              (mm-uu-extract (c nil ,zenburn-bg+1))
              (mode-line (c ,zenburn-fg ,zenburn-bg+1))
              (mode-line-inactive (~ header-line))
              (powerline-active1 (c nil ,zenburn-bg+1))
              (powerline-active2 (c nil ,zenburn-bg-05))
              (spaceline-read-only (c "black" ,zenburn-blue-3))
              (spaceline-modified (c "black" ,zenburn-yellow-2))
              (spaceline-unmodified (c nil ,zenburn-green-1))
              (TeX-error-description-error (c ,zenburn-red))
              (vertical-border (c ,zenburn-bg+2))
              (w3m-anchor (~ link))
              (w3m-arrived-anchor (~ visited-link))
              (w3m-form-button (c ,zenburn-green+2 ,zenburn-bg+1))
              (w3m-header-line-location-content (c ,zenburn-yellow))
              (w3m-header-line-location-title nil)
              (w3m-image-anchor (~ w3m-anchor) (c nil ,zenburn-bg+2))
              (w3m-tab-background (~ mode-line))
              (w3m-tab-selected (c ,zenburn-red+1 ,zenburn-bg) bf bx)
              (w3m-tab-unselected (c ,zenburn-fg "grey30") bx)
              (w3m-tab-selected-background (~ w3m-tab-selected))
              (w3m-tab-unselected-unseen (~ w3m-tab-unselected))))))
    (apply 'custom-theme-set-faces (cons 'zenburn f))
    (custom-theme-set-variables 'zenburn `(fci-rule-color ,zenburn-bg+1))))

(provide 'jao-zenburn-theme)
