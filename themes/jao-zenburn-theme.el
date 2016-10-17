(require 'zenburn-theme)
(require 'jao-themes)

(load-theme 'zenburn t)

(zenburn-with-color-variables
  (let ((f (jao-themes-parse-faces
            `(
              (circe-originator-face (c ,zenburn-yellow-1))
              (diff-hl-change (c nil ,zenburn-blue-4))
              (diff-hl-delete (c nil ,zenburn-red-1))
              (diff-hl-insert (c nil ,zenburn-green))
              (fringe (c nil nil))
              (gnus-summary-cancelled (c ,zenburn-red) st)
              (link (c ,zenburn-yellow) nbf nul)
              (link-visited (c ,zenburn-yellow-2) nbf nul)
              (lui-button-face (c ,zenburn-green+2))
              (lui-time-stamp-face (c ,zenburn-bg+3))
              (mode-line (c ,zenburn-fg ,zenburn-bg-1))
              (mode-line-inactive (c ,zenburn-fg ,zenburn-bg+1))
              (vertical-border (c ,zenburn-bg+2))
              (w3m-anchor (~ link))
              (w3m-arrived-anchor (~ visited-link))
              (w3m-header-line-location-content (c ,zenburn-yellow))
              (w3m-header-line-location-title nil)
              (w3m-tab-background (~ mode-line))
              (w3m-tab-selected (c ,zenburn-red+1 ,zenburn-bg))
              (w3m-tab-unselected (c ,zenburn-fg "grey30") bx nil)
              (w3m-tab-unselected-unseen (~ w3m-tab-unselected))
              ))))
    (apply 'custom-theme-set-faces (cons 'zenburn f))))

(provide 'jao-zenburn-theme)
