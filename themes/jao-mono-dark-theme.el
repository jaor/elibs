(jao-define-custom-theme jao-mono-dark
  (:palette (fg unspecified "grey80")
            (bg unspecified "#3f3f3f")
            (box "color-237" "grey25")
            (button ((c 240) nul) ((c "lightskyblue1" "#4f4f4f")))
            (hilite ((c nil "#666666")))
            (strike-through ((c 237)) (st))
            (italic ((c 137) it) (it (c "lightyellow3")))
            (link ((c 108) nul) ((c "#F0DFAF") nit nul))
            (visited-link ((c 36) nul) ((c "#E0CF9F") nul))
            (tab-sel ((c 252 232) nbf))
            (tab-unsel ((c 245 232) bx))
            ;; (comment ((c 102) it) ((c nil) :height 100
            ;;                       :family "Inconsolata LGC" it))
            (comment ((c 102) it) ((c "grey70") it))
            (keyword ((c 151) nbf nul nit) ((c "darkseagreen3")))
            (function ((c 115) nul nbf) ((c "palegreen3"))) ;; 108
            (type ((c 72) nbf) ((c "honeydew3")))
            (variable-name ((c nil)))
            (constant ((c 72)) ((c "darkseagreen2")))
            (string ((c 36)) ((c "lemonchiffon3")))
            (warning ((c 144)) ((c "#F0DFAF")))
            (error ((c 95)) ((c "goldenrod3")))
            ;; (dimm ((c 240)))
            (dimm ((c 59)) ((c "#7f7f7f")))
            (gnus-mail ((c "gray70" nil)))
            (gnus-news ((c "gray70" nil)))
            (outline ((c 247)))
            (f00 ((c 29)) ((c "darkseagreen")))
            (f01 ((c 108)) ((c "darkseagreen2")))
            (f02 ((c 102)) ((c "lightcyan4"))) ;; ((c "paleturquoise4"))
            (f10 ((c "cornsilk3")))
            (f11 ((c "lemonchiffon3")))
            (f12 ((c "azure3"))))
  (:faces (bold (c nil nil) nul)
          (button (c 66))
          (font-lock-doc-face (c 30))
          (gnus-button (c nil) nul)
          (gnus-header-subject (p f01))
          (gnus-summary-selected (c 250))
          ;; (gnus-summary-selected (c 66 nil) nul nbf)
          (match ul)
          (magit-log-tag-label (c 95 240) nbf)
          (mm-uu-extract (c nil 234))
          (mode-line (c 248 235) nbf nul)
          (mode-line-inactive (c 243 235) nbf nul)
          (org-hide (c 0 nil))
          (rcirc-other-nick (c 108))
          (vertical-border (c 59 nil) :inherit nil)
          (w3m-image (c 144))
          (w3m-tab-background (c 0 0) ul)
          (w3m-tab-line (c 0 0) ul)
          (widget-button (c 196))
          (widget-field (c 143 236)))
  (:x-faces (diff-hl-change (c "#3f3f3f" "darkseagreen4"))
            (diff-hl-delete (c "#3f3f3f" "goldenrod4"))
            (diff-hl-insert (c "#3f3f3f" "aquamarine4"))
            (font-lock-doc-face (p visited-link))
            (fringe (p dimm))
            (gnus-button (c "lightyellow3") nul)
            (gnus-summary-cancelled  (c "dark slate gray" nil) st)
            (gnus-summary-selected (p warning) nul nbf)
            (header-line (p hilite))
            (mode-line (c "grey75" "grey21"))
            (mode-line-inactive (c "grey60" "grey31"))
            (org-hide (c 0 nil))
            (rcirc-other-nick (c 5))
            (spaceline-modified (c "black" "lemonchiffon3") niv)
            (spaceline-read-only (c "black" "lemonchiffon4") niv)
            (spaceline-unmodified (c nil "darkseagreen4") niv)
            (vertical-border (c "gray35") :inherit nil)
            (w3m-image (c "lightcyan2"))
            (w3m-tab-background (c nil nil))
            (w3m-tab-line (c 0 0) ul)
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
