(jao-define-custom-theme jao-mono-dark
  (:palette (fg unspecified "grey75")
            (bg unspecified "grey2")
            (box "color-237" "grey25")
            (button ((c 240) nul) ((c "aquamarine4")))
            (hilite ((c nil 238)))
            (strike-through ((c 237)) (st))
            (italic ((c 137) it) (it (c "lightyellow3")))
            (link ((c 108) nul) ((c "aquamarine3") nit nul))
            (visited-link ((c 36) nul) ((c "aquamarine3") nul))
            (tab-sel ((c 252 232) nbf))
            (tab-unsel ((c 245 232) bx))
            (comment ((c 29) nit) ((c "darkseagreen") it))
            (keyword ((c 151) nbf nul nit) ((c "darkseagreen2")))
            (function ((c 115) nul nbf) ((c "aquamarine3"))) ;; 108
            (type ((c 72) nbf) ((c "lightyellow3")))
            (variable-name ((c nil)))
            (constant ((c 72)) ((p function)))
            (string ((c 36)) ((c "aquamarine")))
            (warning ((c 144)) ((c "lightgoldenrod3")))
            (error ((c 95)) ((c "goldenrod3")))
            ;; (dimm ((c 240)))
            (dimm ((c 59)) ((c "darkslategray4")))
            (gnus-mail ((c 248 nil)))
            (gnus-news ((c 248 nil)))
            (outline ((c 247)))
            (f00 ((c 29)) ((c "aquamarine3")))
            (f01 ((c 108)) ((c "darkseagreen3")))
            (f02 ((c 102)) ((c "lightcyan4"))) ;; ((c "paleturquoise4"))
            (f10 ((p f01)))
            (f11 ((p f00)))
            (f12 ((p f02))))
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
  (:x-faces (gnus-button (c "lightyellow3") nul)
            (gnus-summary-selected ul)
            (gnus-summary-cancelled  (c "dark slate gray" nil) st)
            (header-line (c 243 235))
            (jabber-roster-user-dnd (p warning))
            (jabber-roster-user-error (p warning))
            (jabber-title-small-face (~ jabber-tittle-medium-face))
            (mode-line (c "grey75" "grey20") nbf nul bx)
            (mode-line-inactive (c "grey40" "grey11") nbf nul bx)
            (org-hide (c 0 nil))
            (font-lock-doc-face (c "darkseagreen"))
            (fringe (p dimm))
            (rcirc-other-nick (c 5))
            (vertical-border (c 8 nil) :inherit nil)
            (w3m-image (c "lightyellow4"))
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
