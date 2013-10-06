(jao-define-custom-theme jao-light
  (:palette (fg unspecified "black")
            (bg unspecified "white")
            (box "magenta" "antiquewhite3")
            (button ((c 13 nil) nbf nul) (~ mode-line-inactive))
            (hilite ((c nil 15))) ;; 5
            (strike-through ((c 1)) (st))
            (italic (it) (it :family "Inconsolata LGC" :height 90))
            (link (ul nbf nit) ((c 29) nul nbf))
            (visited-link ((c 24) nul nbf))
            (tab-sel ((~ mode-line)))
            (tab-unsel ((~ mode-line-inactive)))
            (comment ((c 237) it :family "Inconsolata LGC" :height 90))
            (keyword ((c 24 nil) nbf))
            (type ((c 25) nbf nul) ((c 14) nbf))
            (function ((c 31 nil) nbf) ((c 30 nil) nbf))
            (variable-name ((c 0)))
            (constant ((c 14)))
            (string ((c 13)) ((c 29)))
            (warning ((c 9)))
            (error ((c 1)))
            (dimm ((c "grey69")) ((c 12)))
            (gnus-mail ((c 0)))
            (gnus-news ((c 0)))
            (outline ((c 0)))
            (f00 ((c 8))) ;; 14
            (f01 ((c 14)))
            (f02 ((c 10))) ;; 8
            (f10 ((p f00)))
            (f11 ((p f01)))
            (f12 ((p f02))))
  (:faces (bold (c 256) bf)
          (company-tooltip-common (c 1 6) nbf)
          (company-tooltip-common-selection (~ company-tooltip-selection)
                                            (c nil 6) bf)
          (company-tooltip-selection (~ company-tooltip) bf nul)
          (compilation-info (c nil nil) bf)
          (diary (c 14) bf)
          (gnus-summary-selected (c 13 nil) nul)
          (gnus-summary-normal-unread (c 0 nil))
          (header-line (c nil 15) nul)
          (italic it)
          (mode-line (c 0 "grey90") nbf nul)
          (mode-line-inactive (c 11 "grey90") nbf nul)
          (org-hide (c 15 15))
          (vertical-border (c "grey80" nil) :inherit nil)
          (w3m-tab-selected (c 9 nil) bf)
          (w3m-link-numbering (c 12))
          (w3m-bold (c nil nil) bf)
          (w3m-image (c 94))
          (w3m-tab-background (c 12 15)))
  (:x-faces (bold (c 14) bf)
            (button (c 10 nil) nbf)
            (gnus-button (c nil nil) nbf)
            (compilation-info (c 14 nil) nbf)
            (cursor (c 1 1))
            (diary (p error) nbf)
            (diff-hl-change (c "white" "light goldenrod yellow"))
            (diff-hl-insert (c "white" "honeydew2"))
            (diff-hl-delete (c "white" "moccasin"))
            (fringe (c "grey70" nil))
            (gnus-summary-selected (c 2) nbf)
            (gnus-summary-cancelled (c 1) st)
            (header-line (c nil "#efebe7"))
            (mode-line (c "grey15" 7) :box (:line-width -1 :color "grey90"))
            (mode-line-inactive (c "grey30" "grey97")
                                :box (:line-width -1 :color "grey90"))
            (org-hide (c "white" "white"))
            (vertical-border (c "grey70" nil))
            (w3m-image (c "midnightblue" "azure2"))
            (w3m-bold (c 10) bf)
            (w3m-tab-selected (c 9 15) bf)
            (w3m-tab-selected-retrieving (~ w3m-tab-selected) (c 1))
            (w3m-tab-background (c 15 15) nul))
  (:x-colors "black" "sienna3" "#597B59" "#D38108" "#3B3152"
             "#E0DACC" "lightyellow3"
             "#EFEBE7" "grey20" "orangered4" "darkslategray" "#59513A"
             "lemonchiffon4" "#386858" "#223142" "#EFEBE7"))

(provide 'jao-light-theme)
