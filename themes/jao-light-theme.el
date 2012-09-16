(jao-define-custom-theme jao-light
  (:palette (fg unspecified "black")
            (bg unspecified "#efebe7")
            (box "magenta" "antiquewhite3")
            (button ((c 13 nil) nbf nul) (bx))
            (hilite ((c nil 5)))
            (strike-through ((c 1)) (st))
            (italic (it))
            (link ((c 10) ul nbf))
            (visited-link ((c 8) ul))
            (tab-sel ((~ mode-line)))
            (tab-unsel ((~ mode-line-inactive)))
            (comment ((c 239) it)) ;; italic
            (keyword ((c 14) nul bf) ((c 10) nul nbf))
            (type ((c 23) nbf nul) ((c 14) nbf))
            (function ((c 13 nil) bf) ((c 8 nil) nbf))
            (variable-name ((c 0)))
            (constant ((c 14)))
            (string ((c 13)))
            (warning ((c 9)))
            (error ((c 1)))
            (dimm ((c 12)))
            (gnus-mail ((c 0)))
            (gnus-news ((c 0)))
            (outline ((c 0)))
            (f00 ((c 8))) ;; 14
            (f01 ((c 14)))
            (f02 ((c 10))) ;; 8
            (f10 ((p f00)))
            (f11 ((p f01)))
            (f12 ((p f02))))
  (:faces (bold (c 0) bf)
          (company-tooltip-common (c 1 6) nbf)
          (company-tooltip-common-selection (~ company-tooltip-selection)
                                            (c nil 6) bf)
          (company-tooltip-selection (~ company-tooltip) bf nul)
          (compilation-info (c nil nil) bf)
          (diary (c 14) bf)
          (gnus-summary-selected (c 13 nil) nul)
          (gnus-summary-normal-unread (c 0 nil))
          (header-line (c nil 5) nul)
;;          (italic it)
          (mode-line (c 11 5) nbf nul)
          (mode-line-inactive (c 11 6) nbf nul)
          (org-hide (c 7 7))
          (vertical-border (c 11 nil) :inherit nil)
          (w3m-tab-selected (c 9 15) bf)
          (w3m-link-numbering (c 12))
          (w3m-bold (c nil nil) bf)
          (w3m-image (c 94))
          (w3m-tab-background (c 12 5)))
  (:x-faces (button (c 10 nil) nbf)
            (gnus-button (c nil nil) nbf)
            (company-tooltip-common (c 1 6) nbf)
            (company-tooltip-common-selection (~ company-tooltip-selection)
                                              (c nil 6) bf)
            (company-tooltip-selection (~ company-tooltip) bf nul)
            (compilation-info (c nil nil) bf)
            (cursor (c 1 1))
            (fringe (c 11 nil))
            (gnus-summary-selected (c nil "white"))
            (gnus-summary-cancelled (c 1) st)
            (header-line (c nil "#E0DACC"))
            (mode-line (c 0 7) :box (:line-width -1 :color "grey60"))
            (mode-line-inactive (~ mode-line) (c "grey30" "#E0DACC"))
            (org-hide (c 7 nil))
            (vertical-border (c 12 nil))
            (w3m-image (c 3))
            (w3m-tab-selected (c 9 15) bf)
            (w3m-tab-selected-retrieving (~ w3m-tab-selected) (c 1))
            (w3m-tab-background (c 15 15) nul))
  (:x-colors "black" "sienna3" "#597B59" "#D38108" "#3B3152"
             "#E0DACC" "lightyellow3"
             "#EFEBE7" "grey20" "orangered4" "darkslategray" "#59513A"
             "lemonchiffon4" "#386858" "#223142" "#EFEBE7"))

(provide 'jao-light-theme)
