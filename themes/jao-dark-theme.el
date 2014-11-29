(jao-define-custom-theme jao-dark
  (:palette (fg unspecified "grey60")
            (bg unspecified "grey2")
            (box "yellow" "grey30")
            (button ((c 11) nul))
            (hilite ((c nil 8)))
            (strike-through ((c 8)))
            (italic ((c 101) nul) (it :family "DejaVu Sans Mono" :height 100))
            (link ((c 2) nul))
            (visited-link ((c 2) nul))
            (tab-sel ((c 9 8) nbf))
            (tab-unsel ((c 15 6) bx))
            (comment ((c 3)) ((c 3)))
            (keyword ((c 12) nbf nul))
;;            (keyword ((c 151) nbf nul) (bf))
            (type ((c 11) nbf))
            (function ((c 108) nul nbf))
;;            (function ((c 13) nul bf))
            (variable-name ((c nil)))
            (constant ((c 4)))
            (string ((c 2)))
            (warning ((c 144)) ((c 1)))
            (error ((c 9)))
            (dimm ((c 3)))
            (gnus-mail ((c 15 nil)))
            (gnus-news ((c 15 nil)))
            (outline ((c 7)))
            (f00 ((c 11)))
            (f01 ((c 10)))
            (f02 ((c 23)) ((c "cadetblue4")))
            (f10 ((p f00)))
            (f11 ((p f01)))
            (f12 ((p f02))))
  (:faces (bold (c nil nil) nul)
          (font-lock-doc-face (c 10))
          (gnus-button (c nil nil) nul)
          (gnus-summary-selected (c nil nil) ul nbf)
          (mm-uu-extract (c nil 6))
          (mode-line (c 7 8) nbf nul)
          (mode-line-inactive (c 8 16) nbf nul)
          (org-hide (c 0 nil))
;;          (rcirc-other-nick (c 4) nbf)
          (vertical-border (c 8 nil) :inherit nil)
          (w3m-image (c 1))
          (w3m-tab-background (c 0 0))
          (w3m-tab-line (c 0 0))
          (widget-button (c nil nil) nul))
  (:x-faces (gnus-button (c nil nil) nul)
            (gnus-summary-selected (c "grey40" nil) ul)
            (mode-line (c 14 8) nbf nul bx)
            (mode-line-inactive (c 3 8) nbf nul bx)
            (org-hide (c 0 nil))
            (font-lock-doc-face (c 10))
            (fringe (p dimm))
            (rcirc-other-nick (c 5))
            (vertical-border (c 8 nil) :inherit nil)
            (w3m-image (c 9))
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

(provide 'jao-dark-theme)
