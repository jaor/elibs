(jao-define-custom-theme jao-mono-dark
  (:palette (fg unspecified "grey60")
            (bg unspecified "grey2")
            (box "color-237" "grey30")
            (button ((c 240) nul))
            (hilite ((c nil 8)))
            (strike-through ((c 237)))
            (italic ((c 250) nul))
            (link ((c 108) nul))
            (visited-link ((c 107) nul))
            (tab-sel ((c 252 232) nbf))
            (tab-unsel ((c 245 232) bx))
            (comment ((c 244)))
            (keyword ((c 151) nbf nul))
            (function ((c 108) nul nbf))
            (type ((c 72) nbf))
            (variable-name ((c nil)))
            (constant ((c 66)))
            (string ((c 66)))
            (warning ((c 144)))
            (error ((c 95)))
            (dimm ((c 240)))
            (gnus-mail ((c 248 nil)))
            (gnus-news ((c 248 nil)))
            (outline ((c 247)))
            (f00 ((c 108)))
            (f01 ((c 29))) ;; 30
            (f02 ((c 102)))
            (f10 ((p f00)))
            (f11 ((p f01)))
            (f12 ((p f02))))
  (:faces (bold (c nil nil) nul)
          (button (c 66))
          (header-line (c 243 236))
          (font-lock-doc-face (c 244))
          (gnus-button (c nil) nul)
          (gnus-summary-selected (c 66 nil) nul nbf)
          (mm-uu-extract (c nil 234))
          (mode-line (c 253 240) nbf nul)
          (mode-line-inactive (c 245 235) nbf nul)
          (org-hide (c 0 nil))
          (rcirc-other-nick (c 108))
          (vertical-border (c 8 nil) :inherit nil)
          (w3m-image (c 144))
          (w3m-tab-background (c 0 0))
          (w3m-tab-line (c 0 0))
          (widget-button (c 196)))
  (:x-faces (gnus-button (c nil nil) nul)
            (gnus-summary-selected (c "grey40" nil) ul)
            (mode-line (c 14 8) nbf nul bx)
            (mode-line-inactive (c 3 8) nbf nul bx)
            (org-hide (c 0 nil))
            (font-lock-doc-face (c 10))
            (fringe (p dimm))
            (rcirc-other-nick (c 5))
            (vertical-border (c 8 nil) :inherit nil)
            (w3m-image (c 100))
            (w3m-tab-background (c 0 0))
            (widget-button (c nil nil) nul))
  (:x-colors "#050505"
             "lightgoldenrod3"
             "darkseagreen4"
             "grey40"
             "lightcyan4"
             "paleturquoise4"
             "grey7"
             "grey60"
             "grey20"
             "sienna4"
             "#44836e"
             "#648f81"
             "darkseagreen4"
             "aquamarine4"
             "azure4"
             "grey60"))

(provide 'jao-mono-dark-theme)
