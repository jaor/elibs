(require 'cl)

;;; palette
(defvar jao-themes--face-family "Inconsolata")
(defvar jao-themes--fg "black")
(defvar jao-themes--bg "white")
(defvar jao-themes--box "grey75")
(defvar jao-themes--hilite nil)
(defvar jao-themes--italic '(it))
(defvar jao-themes--button '(ul))
(defvar jao-themes--strike-through '(:strike-through t))
(defvar jao-themes--outline '((c "darkslategrey")))
(defvar jao-themes--link '((c "darkgoldenrod4")))
(defvar jao-themes--visited-link '((c "darkolivegreen4") nul))
(defvar jao-themes--gnus-mail '(dfg))
(defvar jao-themes--gnus-news '(dfg))
(defvar jao-themes--tab-sel '((c nil "grey90") bx))
(defvar jao-themes--tab-unsel '((c "grey30" "grey85") nbf bx))
(defvar jao-themes--comment '((c "grey30")))
(defvar jao-themes--warning '((c "indianred3") nbf))
(defvar jao-themes--error '((c "indianred3") bf))
(defvar jao-themes--constant '((c "darkolivegreen") nbf))
(defvar jao-themes--function '((c "darkolivegreen") nbf))
(defvar jao-themes--keyword '((c "darkslategrey") nbf))
(defvar jao-themes--string '((c "skyblue4")))
(defvar jao-themes--type '((c "darkslategrey")))
(defvar jao-themes--variable-name '((c "DodgerBlue4")))
(defvar jao-themes--dimm '((c "grey30") nbf))
(defvar jao-themes--f00 '((c "dodgerblue4")))
(defvar jao-themes--f01 '((c "cadetblue4")))
(defvar jao-themes--f02 '((c "darkslategrey")))
(defvar jao-themes--f10 '((c "dodgerblue4")))
(defvar jao-themes--f11 '((c "cadetblue4")))
(defvar jao-themes--f12 '((c "darkslategrey")))

(defsubst jao-themes--palette-face (face)
  (intern (format "jao-themes--%s" face)))

(defun jao-themes--normalize-body (body)
  (dolist (p '(:inverse-video :underline :inherit) body)
    (unless (member p body)
      (setq body (append body (list p nil))))))

(defun jao-themes--parse-face-body (f)
  (cond ((null f) nil)
        ((listp f)
         (jao-themes--normalize-body
          (apply 'append (mapcar 'jao-themes--parse-face-sym f))))))

(defvar jao-themes--default-cidxs
  '("#000000" "#cd0000" "#00cd00" "#cdcd00"
    "#0000cd" "#cd00cd" "#00cdcd" "#e5e5e5"
    "#4d4d4d" "#ff0000" "#00ff00" "#ffff00"
    "#0000ff" "#ff00ff" "#00ffff" "#ffffff"
    "#000000" "#00002a" "#000055" "#000080"
    "#0000aa" "#0000d4" "#002a00" "#002a2a"
    "#002a55" "#002a80" "#002aaa" "#002ad4"
    "#005500" "#00552a" "#005555" "#005580"
    "#0055aa" "#0055d4" "#008000" "#00802a"
    "#008055" "#008080" "#0080aa" "#0080d4"
    "#00aa00" "#00aa2a" "#00aa55" "#00aa80"
    "#00aaaa" "#00aad4" "#00d400" "#00d42a"
    "#00d455" "#00d480" "#00d4aa" "#00d4d4"
    "#2a0000" "#2a002a" "#2a0055" "#2a0080"
    "#2a00aa" "#2a00d4" "#2a2a00" "#2a2a2a"
    "#2a2a55" "#2a2a80" "#2a2aaa" "#2a2ad4"
    "#2a5500" "#2a552a" "#2a5555" "#2a5580"
    "#2a55aa" "#2a55d4" "#2a8000" "#2a802a"
    "#2a8055" "#2a8080" "#2a80aa" "#2a80d4"
    "#2aaa00" "#2aaa2a" "#2aaa55" "#2aaa80"
    "#2aaaaa" "#2aaad4" "#2ad400" "#2ad42a"
    "#2ad455" "#2ad480" "#2ad4aa" "#2ad4d4"
    "#550000" "#55002a" "#550055" "#550080"
    "#5500aa" "#5500d4" "#552a00" "#552a2a"
    "#552a55" "#552a80" "#552aaa" "#552ad4"
    "#555500" "#55552a" "#555555" "#555580"
    "#5555aa" "#5555d4" "#558000" "#55802a"
    "#558055" "#558080" "#5580aa" "#5580d4"
    "#55aa00" "#55aa2a" "#55aa55" "#55aa80"
    "#55aaaa" "#55aad4" "#55d400" "#55d42a"
    "#55d455" "#55d480" "#55d4aa" "#55d4d4"
    "#800000" "#80002a" "#800055" "#800080"
    "#8000aa" "#8000d4" "#802a00" "#802a2a"
    "#802a55" "#802a80" "#802aaa" "#802ad4"
    "#805500" "#80552a" "#805555" "#805580"
    "#8055aa" "#8055d4" "#808000" "#80802a"
    "#808055" "#808080" "#8080aa" "#8080d4"
    "#80aa00" "#80aa2a" "#80aa55" "#80aa80"
    "#80aaaa" "#80aad4" "#80d400" "#80d42a"
    "#80d455" "#80d480" "#80d4aa" "#80d4d4"
    "#aa0000" "#aa002a" "#aa0055" "#aa0080"
    "#aa00aa" "#aa00d4" "#aa2a00" "#aa2a2a"
    "#aa2a55" "#aa2a80" "#aa2aaa" "#aa2ad4"
    "#aa5500" "#aa552a" "#aa5555" "#aa5580"
    "#aa55aa" "#aa55d4" "#aa8000" "#aa802a"
    "#aa8055" "#aa8080" "#aa80aa" "#aa80d4"
    "#aaaa00" "#aaaa2a" "#aaaa55" "#aaaa80"
    "#aaaaaa" "#aaaad4" "#aad400" "#aad42a"
    "#aad455" "#aad480" "#aad4aa" "#aad4d4"
    "#d40000" "#d4002a" "#d40055" "#d40080"
    "#d400aa" "#d400d4" "#d42a00" "#d42a2a"
    "#d42a55" "#d42a80" "#d42aaa" "#d42ad4"
    "#d45500" "#d4552a" "#d45555" "#d45580"
    "#d455aa" "#d455d4" "#d48000" "#d4802a"
    "#d48055" "#d48080" "#d480aa" "#d480d4"
    "#d4aa00" "#d4aa2a" "#d4aa55" "#d4aa80"
    "#d4aaaa" "#d4aad4" "#d4d400" "#d4d42a"
    "#d4d455" "#d4d480" "#d4d4aa" "#d4d4d4"
    "#080808" "#121212" "#1c1c1c" "#262626"
    "#303030" "#3a3a3a" "#444444" "#4e4e4e"
    "#585858" "#626262" "#6c6c6c" "#767676"
    "#808080" "#8a8a8a" "#949494" "#9e9e9e"
    "#a8a8a8" "#b2b2b2" "#bcbcbc" "#c6c6c6"
    "#d0d0d0" "#dadada" "#e4e4e4" "#eeeeee"))

(defvar jao-themes--cidxs nil)
(defvar jao-themes--x-colors nil)

(defun jao-themes--color (clr)
  (cond ((stringp clr) clr)
        ((numberp clr) (or (nth clr jao-themes--cidxs)
                           (nth clr jao-themes--default-cidxs)
                           (format "color-%s" clr)))
        (t 'unspecified)))

(defun jao-themes--parse-face-sym (s)
  (cond ((listp s)
         (case (car s)
           (c `(:foreground ,(jao-themes--color (cadr s))
                            :background ,(jao-themes--color (caddr s))))
           (p (let ((var (jao-themes--palette-face (cadr s))))
                (when (boundp var)
                  (let ((val (symbol-value var)))
                    (if (listp val)
                        (jao-themes--parse-face-body val)
                      val)))))
           (~ (cdr (assq (cadr s) *jao--parsed-faces*)))
           (t (list s))))
        ((atom s)
         (case s
           (~ '(:inherit))
           (dbg `(:background ,jao-themes--bg))
           (dfg `(:foreground ,jao-themes--fg))
           (link (jao-themes--parse-face-body jao-themes--link))
           (vlink (jao-themes--parse-face-body jao-themes--visited-link))
           (bf '(:bold t :weight bold))
           (nbf '(:bold t :weight normal))
           (it '(:italic t :slant italic))
           (nit '(:italic nil :slant normal))
           (niv '(:inverse-video nil))
           (ul '(:underline t))
           (nul '(:underline nil))
           (st '(:strike-through t))
           (bx `(:box (:line-width -1 :color ,jao-themes--box)))
           (t (list s))))))

(defun jao-themes--make-faces (fs &optional cidxs)
  (let ((*jao--parsed-faces* nil)
        (jao-themes--cidxs (or cidxs jao-themes--default-cidxs))
        (result nil))
    (dolist (f (sort (jao-themes--dfs fs) 'jao--cmp-faces) (reverse result))
      (let ((body (jao-themes--parse-face-body (cdr f))))
        (push (cons (car f) body) *jao--parsed-faces*)
        (push (list (car f) body) result)))))

(defun jao--cmp-faces (a b)
  (let ((ai (cadr (assq '~ a)))
        (bi (cadr (assq '~ b))))
    (cond ((and ai (not bi)) nil)
          ((and bi (not ai)) t)
          ((eq (car a) bi) t)
          ((eq (car b) ai) nil)
          (t (string< (symbol-name (car a))
                      (symbol-name (car b)))))))

(defun jao-themes--dfs (fs)
  (let ((dfs
         (append
          `((bbdb-company)
            (bbdb-field-name bf)
            (bbdb-field-value nil)
            (bbdb-name ul)
            (bmk-mgr-bookmark-face nil)
            (bmk-mgr-folder-face bf)
            (bmk-mgr-sel-bookmark-face link)
            (bmk-mgr-sel-folder-face bf)
            (bold bf)
            (bold-italic bf)
            (border (c nil nil))
            (buffer-menu-buffer bf)
            (button (p button)))
          `((calendar-holiday-marker (p f00))
            (circe-highlight-nick-face (p warning))
            (circe-originator-face (p f00))
            (circe-server-face (p dimm))
            (clojure-test-failure-face (p warning) ul)
            (clojure-test-error-face (p error) ul)
            (cursor (p error)))
          `((diredp-compressed-file-suffix (~ diredp-file-suffix))
            (diredp-date-time (p f01))
            (diredp-deletion (p error))
            (diredp-deletion-file-name (~ diredp-deletion))
            (diredp-dir-heading bf dfg dbg)
            (diredp-dir-priv dfg dbg bf)
            (diredp-display-msg (p f00))
            (diredp-exec-priv dfg dbg bf)
            (diredp-executable-tag (p error))
            (diredp-file-name dfg dbg)
            (diredp-file-suffix (~ diredp-file-name))
            (diredp-flag-mark (p f00) bf)
            (diredp-flag-mark-line (p hilite))
            (diredp-ignored-file-name (p dimm))
            (diredp-link-priv (~ diredp-symlink))
            (diredp-no-priv (~ diredp-read-priv))
            (diredp-other-priv dfg dbg)
            (diredp-rare-priv dfg dfg)
            (diredp-read-priv dfg dbg bf)
            (diredp-symlink (p warning))
            (diredp-write-priv dfg dbg bf))
          `((change-log-acknowledgement (p f02))
            (change-log-conditionals (p f02))
            (change-log-date (p f01))
            (change-log-email (p f00))
            (change-log-file (p f10))
            (change-log-function (p function))
            (change-log-list (p f11))
            (change-log-name (p keyword))
            (cider-test-error-face (p warning))
            (cider-test-failure-face  (p error))
            (cider-test-success-face (p f01))
            (comint-highlight-input (p f01) nbf)
            (comint-highlight-prompt (p f00))
            (company-tooltip (~ highlight))
            (company-tooltip-selection (~ company-tooltip) ul)
            (company-tooltip-common (~ company-tooltip) bf)
            (company-tooltip-common-selection
             (~ company-tooltip-selection) bf)
            (company-preview (~ highlight))
            (company-preview-common (~ company-preview) bf)
            (compilation-column-number (p f00) nul)
            (compilation-error nbf (p error) nul)
            (compilation-info nbf (p f02) nul)
            (compilation-line-number (p f01) nul)
            (compilation-mode-line-fail (p error))
            (compilation-mode-line-exit (p f01) nbf)
            (compilation-mode-line-run (p warning))
            (compilation-warning nbf (p warning) nul)
            (completions-common-part nbf :width normal)
            (completions-first-difference bf dfg dbg)
            (cursor dfg dbg)
            (custom-button (~ button))
            (custom-button-mouse (~ button))
            (custom-button-pressed (~ button))
            (custom-button-pressed-unraised (~ button))
            (custom-button-unraised (~ button))
            (custom-changed (p warning))
            (custom-comment (p string))
            (custom-comment-tag (p keyword))
            (custom-documentation (p string))
            (custom-face-tag nbf)
            (custom-group-tag bf (p f00) :height 11)
            (custom-group-tag-1 bf :family ,jao-themes--face-family
                                (p f00) :height 11)
            (custom-invalid (p error))
            (custom-link link)
            (custom-modified (p f10))
            (custom-rogue (p error))
            (custom-saved ul)
            (custom-set (p f11))
            (custom-state (p f12))
            (custom-themed (p f00))
            (custom-variable-button (~ button))
            (custom-variable-tag (p variable-name) bf)
            (cvs-handled (p dimm)))
          `((darcsum-change-line-face (p warning))
            (darcsum-filename-face (p f00))
            (darcsum-header-face (p f01))
            (darcsum-marked-face (p f00) bf)
            (darcsum-need-action-face (p warning))
            (darcsum-need-action-marked-face bf (p warning))
            (diary (p f02))
            (dictionary-button-face (p link))
            (dictionary-reference-face (p f11))
            (dictionary-word-entry-face (p f10))
            (diff-added (p warning))
            (diff-changed (p f02) nul)
            (diff-context (p dimm))
            (diff-file-header dfg dbg nbf)
            (diff-function (p function))
            (diff-header nbf dfg dbg)
            (diff-hl-change (p dimm))
            (diff-hl-insert (p dimm))
            (diff-hl-delete (p warn))
            (diff-hunk-header (~ diff-file-header))
            (diff-index bf dfg dbg)
            (diff-indicator-added (~ diff-added))
            (diff-indicator-changed (~ diff-changed))
            (diff-indicator-removed (~ diff-removed))
            (diff-nonexistent bf (p error))
            (diff-refine-added (~ diff-changed) ul)
            (diff-refine-change (~ diff-changed) ul)
            (diff-refine-removed (~ diff-removed) ul)
            (diff-removed (p error))
            (dired-directory (p f02))
            (dired-flagged bf)
            (dired-header (p f01))
            (dired-ignored (p dimm))
            (dired-mark (p f00) bf)
            (dired-marked bf (p f00))
            (dired-symlink (p f11))
            (dired-warn-writable (p warning))
            (dired-warning (p warning))
            (diredp-number (p f11)))
          `((ediff-current-diff-A (~ diff-added))
            (ediff-current-diff-Ancestor (c nil ,jao-themes--box))
            (ediff-current-diff-B (~ ediff-current-diff-A))
            (ediff-current-diff-C (~ ediff-current-diff-A))
            (ediff-even-diff-A (~ diff-added) bf)
            (ediff-even-diff-Ancestor (c nil ,jao-themes--box))
            (ediff-even-diff-B (~ ediff-even-diff-A))
            (ediff-even-diff-C (~ ediff-even-diff-A))
            (ediff-fine-diff-A (~ ediff-current-diff-A) nbf ul)
            (ediff-fine-diff-Ancestor (c nil ,jao-themes--box))
            (ediff-fine-diff-B (~ ediff-fine-diff-A))
            (ediff-fine-diff-C (~ ediff-fine-diff-A))
            (ediff-odd-diff-A (~ ediff-even-diff-A))
            (ediff-odd-diff-Ancestor (~ ediff-odd-diff-A) nbf)
            (ediff-odd-diff-B (~ ediff-odd-diff-A))
            (ediff-odd-diff-C (~ ediff-odd-diff-A))
            (emms-browser-album-face (p f00) :height 1.0)
            (emms-browser-artist-face (p f01) :height 1.0)
            (emms-browser-composer-face (p f02) :height 1.0)
            (emms-browser-track-face (p f10) :height 1.0)
            (emms-browser-year/genre-face (p f11) :height 1.0)
            (emms-metaplaylist-mode-current-face (p f00) bf)
            (emms-metaplaylist-mode-face (p f00))
            (emms-playlist-selected-face (p f00) bf)
            (emms-playlist-track-face (p f00) nbf)
            (emms-stream-name-face (p f00))
            (emms-stream-url-face link)
            (epa-field-body)
            (epa-field-name bf)
            (epa-mark bf (p f00))
            (epa-string (p f01))
            (epa-validity-disabled)
            (epa-validity-high bf)
            (epa-validity-low)
            (epa-validity-medium)
            (erc-action-face (p f02))
            (erc-button (p link))
            (erc-current-nick-face (p error))
            (erc-direct-msg-face (p warning))
            (erc-error-face (p error))
            (erc-header-line (~ header))
            (erc-input-face (p f01))
            (erc-my-nick-face (p warning))
            (erc-nick-default-face (p f02))
            (erc-nick-msg-face (p warning))
            (erc-notice-face (p dimm))
            (erc-pal-face (p warning))
            (erc-prompt-face (f 01))
            (erc-timestamp-face (p dimm))
            (escape-glyph (p dimm))
            (eshell-ls-archive (p f12))
            (eshell-ls-backup (p dimm))
            (eshell-ls-clutter (p dimm))
            (eshell-ls-directory (p f02))
            (eshell-ls-executable (p warning))
            (eshell-ls-missing (p dimm))
            (eshell-ls-product (p f01))
            (eshell-ls-readonly (p f01) bf)
            (eshell-ls-special bf (p f10))
            (eshell-ls-symlink bf (p f11))
            (eshell-ls-unreadable (p dimm))
            (eshell-prompt (p f00)))
          `((factor-font-lock-comment (~ font-lock-comment-face))
            (factor-font-lock-constructor (~ font-lock-function-name-face))
            (factor-font-lock-declaration (~ font-lock-type-face))
            (factor-font-lock-getter-word (~ font-lock-function-name-face))
            (factor-font-lock-parsing-word (~ font-lock-keyword-face))
            (factor-font-lock-setter-word (~ font-lock-function-name-face))
            (factor-font-lock-stack-effect (~ font-lock-comment-face))
            (factor-font-lock-string (~ font-lock-string-face))
            (factor-font-lock-symbol (~ font-lock-keyword-face))
            (factor-font-lock-symbol-definition (~ font-lock-builtin-face))
            (factor-font-lock-type-definition (~ font-lock-type-face))
            (factor-font-lock-type-name (~ font-lock-type-face))
            (factor-font-lock-vocabulary-name (~ font-lock-constant-face))
            (factor-font-lock-word (~ font-lock-function-name-face))
            (ffap)
            (file-name-shadow (p dimm))
            (fixed-pitch :family ,jao-themes--face-family)
            (flyspell-duplicate nbf (p warning))
            (flyspell-incorrect nbf (p error))
            (font-lock-builtin-face (p keyword))
            (font-lock-comment-delimiter-face (p comment))
            (font-lock-comment-face (p comment))
            (font-lock-constant-face (p constant))
            (font-lock-doc-face (p comment))
            (font-lock-function-name-face (p function))
            (font-lock-keyword-face (p keyword))
            (font-lock-negation-char-face nil)
            (font-lock-preprocessor-face (p constant))
            (font-lock-regexp-grouping-backslash bf)
            (font-lock-regexp-grouping-construct bf)
            (font-lock-string-face (p string))
            (font-lock-type-face (p type))
            (font-lock-variable-name-face (p variable-name))
            (font-lock-warning-face (p warning))
            (fringe (p dimm))
            (fuel-font-lock-debug-error (p error) nul)
            (fuel-font-lock-debug-info (p f01) nul)
            (fuel-font-lock-stack-region (p hilite))
            (fuel-font-lock-xref-link link nul)
            (fuel-font-lock-xref-vocab italic nul)
            (fuel-font-lock-markup-link link)
            (fuel-font-lock-markup-title (~ outline-1))
            (fuel-font-lock-markup-emphasis (~ italic))
            (fuel-font-lock-markup-heading (~ outline-1))
            (fuel-font-lock-markup-strong (~ bold)))
          `((geiser-font-lock-autodoc-current-arg (~ highlight))
            (geiser-font-lock-autodoc-identifier
             (~ font-lock-function-name-face))
            (geiser-font-lock-doc-button (~ button))
            (geiser-font-lock-doc-link link)
            (geiser-font-lock-doc-title bf)
            (geiser-font-lock-xref-header bf)
            (geiser-font-lock-xref-link link nul)
            (git-commit-summary-face (p f10))
            (git-gutter-fr:added (~ fringe) nbf)
            (git-gutter-fr:deleted (~ fringe) nbf)
            (git-gutter-fr:modified (~ fringe) nbf)
            (gnus-button (~ button))
            (gnus-cite-attribution nil)
            (gnus-cite-1 (p f10))
            (gnus-cite-2 (p f11))
            (gnus-cite-3 (p f12))
            (gnus-cite-4 (p dimm))
            (gnus-cite-5 (p dimm))
            (gnus-cite-6 (p dimm))
            (gnus-cite-7 (p dimm))
            (gnus-cite-8 (p dimm))
            (gnus-cite-9 (p dimm))
            (gnus-cite-10 (p dimm))
            (gnus-cite-11 (p dimm))
            (gnus-emphasis-bold bf)
            (gnus-emphasis-bold-italic bf)
            (gnus-emphasis-highlight-words (p hilite))
            (gnus-emphasis-italic nil)
            (gnus-emphasis-strikethru st)
            (gnus-emphasis-underline ul)
            (gnus-emphasis-underline-bold bf ul)
            (gnus-emphasis-underline-bold-italic bf ul)
            (gnus-emphasis-underline-italic ul)
            (gnus-group-mail-1 (p gnus-mail) bf)
            (gnus-group-mail-1-empty (p gnus-mail) nbf)
            (gnus-group-mail-2 (~ gnus-group-mail-1))
            (gnus-group-mail-2-empty (~ gnus-group-mail-1-empty))
            (gnus-group-mail-3 (~ gnus-group-mail-1))
            (gnus-group-mail-3-empty (~ gnus-group-mail-1-empty))
            (gnus-group-mail-4 (~ gnus-group-mail-1))
            (gnus-group-mail-4-empty (~ gnus-group-mail-1-empty))
            (gnus-group-mail-5 (p f00) bf)
            (gnus-group-mail-5-empty (p f00))
            (gnus-group-mail-6 (p dimm) bf)
            (gnus-group-mail-6-empty (p dimm))
            (gnus-group-mail-low bf (p dimm))
            (gnus-group-mail-low-empty (p dimm))
            (gnus-group-news-low bf (p dimm))
            (gnus-group-news-low-empty (p dimm))
            (gnus-group-news-1 (p gnus-news) bf)
            (gnus-group-news-1-empty (p gnus-news) nbf)
            (gnus-group-news-2 (~ gnus-group-news-1))
            (gnus-group-news-2-empty (~ gnus-group-news-1-empty))
            (gnus-group-news-3 (~ gnus-group-news-1))
            (gnus-group-news-3-empty (~ gnus-group-news-1-empty))
            (gnus-group-news-4 (~ gnus-group-news-1))
            (gnus-group-news-4-empty (~ gnus-group-news-1-empty))
            (gnus-group-news-5 (p f00) bf)
            (gnus-group-news-5-empty (p f00))
            (gnus-group-news-6 (p dimm) bf)
            (gnus-group-news-6-empty (p dimm))
            (gnus-header-content (p f02))
            (gnus-header-from (p f01))
            (gnus-header-name nbf (p f02))
            (gnus-header-newsgroups (p dimm))
            (gnus-header-subject (p f00) nbf)
            (gnus-mouse-face nil)
            (gnus-server-agent nbf)
            (gnus-server-closed (p warning))
            (gnus-server-denied bf (p error))
            (gnus-server-offline (p dimm))
            (gnus-server-opened bf)
            (gnus-signature nit (p f10))
            (gnus-splash dfg dbg)
            (gnus-summary-high-undownloaded bf nit dfg dbg)
            (gnus-summary-cancelled (p strike-through))
            (gnus-summary-high-unread bf nit)
            (gnus-summary-normal-ancient (p dimm))
            (gnus-summary-normal-read (p dimm))
            (gnus-summary-high-ticked bf nit dfg dbg)
            (gnus-summary-low-ancient (p dimm))
            (gnus-summary-low-read (p dimm) st)
            (gnus-summary-low-ticked (p dimm))
            (gnus-summary-low-unread (p dimm))
            (gnus-summary-low-undownloaded (p dimm))
            (gnus-summary-normal-ancient (p dimm))
            (gnus-summary-normal-read (p dimm))
            (gnus-summary-normal-ticked (p f10) nbf)
            (gnus-summary-normal-undownloaded bf dfg dbg)
            (gnus-summary-normal-unread dfg dbg)
            (gnus-summary-selected (p hilite))
            (gnus-x-face)
            (gui-button-face (~ button))
            (gui-element (~ gui-button-face)))
          `((header-line (~ mode-line-inactive))
            (help-argument-name)
            (helm-selection (p hilite))
            (helm-separator (p dimm))
            (helm-source-header (~ outline-1))
            (helm-ls-git-added-copied-face dfg dbg)
            (helm-ls-git-conflict-face (p error))
            (helm-ls-git-deleted-and-staged-face (p dimm))
            (helm-ls-git-deleted-not-staged-face dfg dbg)
            (helm-ls-git-modified-and-staged-face (p f10))
            (helm-ls-git-modified-not-staged-face (p warning))
            (helm-ls-git-renamed-modified-face (p warning))
            (helm-ls-git-untracked-face (p error))
            (highlight (p hilite)))
          `((ido-first-match (p warning))
            (ido-first-match-face (p warning))
            (ido-incomplete-regexp (p error))
            (ido-indicator (p error) nbf)
            (ido-only-match (p error))
            (ido-subdir (p f01))
            (info-header-node bf dfg)
            (info-header-xref dfg)
            (info-menu-header bf)
            (info-menu-star bf dfg)
            (info-node (p f00))
            (info-title-1 (~ outline-1) bf)
            (info-title-2 (~ outline-2) bf)
            (info-title-3 (~ outline-3) bf)
            (info-title-4 (~ outline-4) bf)
            (Info-quoted (p f01))
            (info-xref link)
            (info-xref-visited vlink)
            (isearch bf (p hilite))
            (isearch-fail (p error))
            (italic (p italic)))
          `((jabber-activity-face dbg dfg nbf)
            (jabber-activity-personal-face (p warning) nbf)
            (jabber-chat-error (p error))
            (jabber-chat-prompt-foreign (p f00) nbf)
            (jabber-chat-prompt-local (p f01) nbf)
            (jabber-chat-prompt-system (p f02) nbf)
            (jabber-rare-time-face (p dimm))
            (jabber-roster-user-away (p dimm))
            (jabber-roster-user-dnd (p dimm))
            (jabber-roster-user-chatty (p warning) nbf)
            (jabber-roster-user-offline (p dimm))
            (jabber-roster-user-online (p f01) nbf)
            (jabber-roster-user-xa (p dimm))
            (jabber-title-large (~ default) bf)
            (jabber-title-medium bf)
            (jabber-title-roster bf (p warning))
            (jao-emms-font-lock-album (p f01))
            (jao-emms-font-lock-artist (p f02))
            (jao-emms-font-lock-title (p f01))
            (jao-emms-font-lock-track dfg dbg)
            (jao-frm-from-face (p f00))
            (jao-frm-mailbox-face bf)
            (jao-frm-subject-face (p f01))
            (jao-frm-mailno-face bf)
            (jao-gnus-face-tree (p dimm))
            (jde-java-font-lock-constant-face (~ font-lock-constant-face))
            (jde-java-font-lock-doc-tag-face (p f02))
            (jde-java-font-lock-package-face (p f02))
            (jde-java-font-lock-link-face (p link))
            (jde-java-font-lock-number-face (~ font-lock-constant-face))
            (jde-java-font-lock-public-face (~ font-lock-keyword-face))
            (jde-java-font-lock-private-face (~ font-lock-keyword-face))
            (jde-java-font-lock-protected-face (~ font-lock-keyword-face))
            (jde-java-font-lock-modifier-face (~ font-lock-keyword-face)))
          `((lazy-highlight (p hilite))
            (link link nul)
            (link-visited vlink nul)
            (lui-button-face (p link))
            (lui-time-stamp-face (p dimm)))
          `((magit-branch (p f00))
            (magit-diff-add (~ diff-added))
            (magit-diff-del (~ diff-removed))
            (magit-diff-file-header (~ diff-file-header))
            (magit-diff-hunk-header (~ diff-hunk-header))
            (magit-diff-none (p dimm))
            (magit-item-highlight (~ mm-uu-extract))
            (magit-item-mark (p warning))
            (magit-log-head-label (p keyword) bf)
            (magit-log-head-label-head (p keyword) nbf ul)
            (magit-log-head-label-default (p keyword) nbf)
            (magit-log-head-label-local (p keyword) nbf)
            (magit-log-head-label-remote (p function) bf)
            (magit-log-head-label-tags (p warning) nbf)
            (magit-log-graph (p f11))
            (magit-log-tag-label (p keyword))
            (magit-section-title (~ outline-2))
            (match (p hilite))
            (markdown-italic-face (~ italic))
            (menu nil)
            (message-cited-text (p f01) nbf)
            (message-header-cc (p f00) nbf)
            (message-header-name (p f01) nbf)
            (message-header-newsgroups (p dimm) nbf)
            (message-header-other (p f00) nbf)
            (message-header-subject (p f00) nbf)
            (message-header-to (p f00) nbf)
            (message-header-xheader (p f00) nbf)
            (message-mml (p warning) nbf)
            (message-separator (p warning) nbf)
            (mm-uu-extract (p hilite))
            (minibuffer-prompt (p f00))
            (mode-line-buffer-id nbf (c nil nil))
            (mode-line-emphasis (p warning))
            (mode-line-highlight (~ mode-line))
            (modeline-mousable (~ mode-line-active))
            (modeline-mousable-minor-mode (~ modeline-mousable))
            (moinmoin-table-pi (p f02))
            (mouse dfg dbg ul)
            (muse-bad-link (p warning))
            (muse-header-1 (~ outline-1))
            (muse-header-2 (~ outline-2))
            (muse-header-3 (~ outline-3))
            (muse-header-4 (~ outline-4))
            (muse-header-5 (~ outline-5))
            (muse-link link)
            (muse-verbatim (p f02)))
          `((next-error (p hilite))
            (nobreak-space dbg dfg ul)
            (nrepl-error-face (p error))
            (nrepl-input-face (p f01))
            (nrepl-output-face (p f02))
            (nrepl-prompt-face (p f00))
            (nrepl-result-face nil))
          `((org-agenda-date-today (p warning))
            (org-agenda-date-weekend (p dimm))
            (org-agenda-done (p dimm))
            (org-agenda-restriction-lock (~ default))
            (org-agenda-structure (p f00))
            (org-archived (p dimm))
            (org-code (p f02))
            (org-column dfg dbg :height 1.0)
            (org-date (p f02) nul)
            (org-done (p dimm) nbf niv)
            (org-drawer (p f02))
            (org-ellipsis (p dimm))
            (org-formula (p f02))
            (org-headline-done (p dimm))
            (org-hide (c ,jao-themes--bg))
            (org-latex-and-export-specials (~ default))
            (org-level-1 (~ outline-1))
            (org-level-2 (~ outline-2))
            (org-level-3 (~ outline-3))
            (org-level-4 (~ outline-4))
            (org-level-5 (~ outline-5))
            (org-level-6 (~ outline-6))
            (org-level-7 (~ outline-7))
            (org-level-8 (~ outline-8))
            (org-link link)
            (org-property-value nil)
            (org-scheduled (p f01))
            (org-scheduled-previously (p warning) nbf)
            (org-scheduled-today (p f00))
            (org-sexp-date (p f01))
            (org-special-keyword (p keyword))
            (org-table (p f01))
            (org-tag (p dimm) nbf)
            (org-target ul)
            (org-time-grid dfg dbg)
            (org-todo bf niv (p warning))
            (org-upcoming-deadline (p f02))
            (org-verbatim  (p hilite))
            (org-warning bf (p warning))
            (outline-1 bf (p outline))
            (outline-2 bf (p outline))
            (outline-3 nbf (p outline))
            (outline-4 nbf (p outline))
            (outline-5 nbf (p outline))
            (outline-6 nbf (p outline))
            (outline-7 nbf (p outline))
            (outline-8 nbf (p outline)))
          `((query-replace bf (p hilite)))
          `((rcirc-bright-nick (p hilite))
            (rcirc-my-nick (p warning))
            (rcirc-nick-in-message (p warning))
            (rcirc-nick-in-message-full-line (~ rcirc-nick-in-message))
            (rcirc-other-nick (p keyword))
            (rcirc-prompt bf)
            (rcirc-server (p dimm))
            (rcirc-timestamp (p dimm))
            (rcirc-track-keyword (p warning))
            (rcirc-track-nick (~ rcirc-my-nick) niv)
            (rcirc-url nbf link)
            (reb-match-0 (p hilite))
            (reb-match-1 (~ secondary-selection))
            (reb-match-2 (~ secondary-selection) bf)
            (reb-match-3 (~ secondary-selection) ul)
            (region (p hilite))
            (rst-level-1-face (~ outline-1))
            (rst-level-2-face (~ outline-2))
            (rst-level-3-face (~ outline-3))
            (rst-level-4-face (~ outline-4))
            (rst-level-5-face (~ outline-5))
            (rst-level-6-face (~ outline-6))
            (rst-level-7-face (~ outline-7))
            (rst-level-8-face (~ outline-8)))
          `((secondary-selection (p hilite))
            (sh-quoted-exec (p f00))
            (show-paren-match (p hilite))
            (show-paren-mismatch (p error))
            (sldb-frame-line-face (p f00))
            (sldb-frame-label-face (p f01))
            (sldb-condition-face (p f02))
            (slime-repl-prompt-face (p f00))
            (slime-repl-input-face (p f00) bf)
            (slime-repl-inputed-output-face (p f02))
            (slime-repl-output-face (p string))
            (speedbar-directory-face (~ diredp-dir-heading))
            (speedbar-file-face (~ diredp-file-name))
            (speedbar-highlight-face (p hilite))
            (speedbar-selected-face ul)
            (speedbar-separator-face (p f00))
            (scroll-bar nil)
            (shadow nil))
          `((term nil)
            (tool-bar nil)
            (tooltip :family ,jao-themes--face-family (c nil "lightyellow"))
            (trailing-whitespace (p error))
            (twittering-uri-face (~ link))
            (twittering-username-face (p f01)))
          `((underline nul))
          `((variable-pitch :family ,jao-themes--face-family :height 11)
            (vertical-border (c ,jao-themes--box nil) :inherit default))
          `((w3m-anchor link)
            (w3m-arrived-anchor vlink)
            (w3m-bold bf dbg dfg)
            (w3m-current-anchor nbf ul)
            (w3m-form dfg dbg ul)
            (w3m-form-button (~ button))
            (w3m-form-button-mouse (~ custom-button-mouse))
            (w3m-form-button-pressed (~ custom-button-pressed))
            (w3m-header-line-location-content
             :box (:line-width 3 :color ,jao-themes--bg) dfg dbg)
            (w3m-header-line-location-title
             :box (:line-width 3 :color ,jao-themes--bg) dfg dbg)
            (w3m-history-current-url (c nil nil) ul)
            (w3m-image (p f10))
            (w3m-image-anchor (c nil nil))
            (w3m-insert (p f12))
            (w3m-italic (~ italic))
            (w3m-linknum-match (p warning))
            (w3m-linknum-minibuffer-prompt (~ minibuffer-prompt))
            (w3m-session-select (p f10))
            (w3m-session-selected bf nul (p f10))
            (w3m-strike-through st)
            (w3m-tab-background nul (c nil nil))
            (w3m-tab-mouse nil)
            (w3m-tab-selected (p tab-sel))
            (w3m-tab-selected-background nil)
            (w3m-tab-selected-retrieving (p tab-sel) it)
            (w3m-tab-unselected (p tab-unsel))
            (w3m-tab-unselected-retrieving (p tab-unsel) it)
            (w3m-tab-unselected-unseen (p tab-unsel))
            (w3m-underline ul)
            (wg-brace-face nil)
            (wg-command-face (p f00))
            (wg-current-workgroup-face (p f11) bf)
            (wg-divider-face nil)
            (wg-filename-face nil)
            (wg-frame-face nil)
            (wg-message-face (p string))
            (wg-mode-line-face nil)
            (wg-previous-workgroup-face (p f00))
            (widget-button (~ button))
            (widget-button-pressed nbf (~ custom-button-pressed))
            (widget-button-face (~ button))
            (widget-button-pressed-face (~ button))
            (widget-documentation (p dimm))
            (widget-field (p hilite) bx)
            (widget-inactive (p dimm))
            (Widget-single-line-field (~ widget-field))
            (woman-bold (p f00) bf)
            (woman-italic (p f01) nul nit)
            (woman-italic-no-ul (p f01) nul nit)))))
    (dolist (df dfs fs)
      (when (not (assq (car df) fs))
        (push df fs)))))

(defsubst jao-themes--let-palette (palette xp)
  (mapcar (lambda (f)
            `(,(jao-themes--palette-face (car f))
              ',(or (and xp (caddr f)) (cadr f))))
          palette))

(defun jao-themes--extract-faces (t-faces x-faces)
  (let ((result))
    (dolist (f t-faces (reverse result))
      (let ((xfb (cdr (assq (car f) x-faces))))
        (push `(,(car f) ((((type x ns)) ,@xfb)
                          (t ,@(cdr f)))) result)))))

(defun jao-themes--set-fbg (kind)
  (let* ((kvs (cdr (assoc kind window-system-default-frame-alist)))
         (f-alist (assq-delete-all 'background-color kvs))
         (f-alist (assq-delete-all 'foreground-color f-alist)))
    (when jao-themes--fg
      (push (cons 'foreground-color jao-themes--fg) f-alist))
    (when jao-themes--bg
      (push (cons 'background-color jao-themes--bg) f-alist))
    (setq window-system-default-frame-alist
          (cons
           (cons kind f-alist)
           (assq-delete-all kind window-system-default-frame-alist)))))

(defmacro jao-define-custom-theme (name &rest args)
  (let* ((t-faces (make-symbol "t-faces"))
         (xfaces (make-symbol "xfaces"))
         (tx-faces (make-symbol "tx-faces"))
         (palette (cdr (assoc :palette args)))
         (faces (cdr (assoc :faces args)))
         (x-faces (cdr (assoc :x-faces args)))
         (x-colors (cdr (assoc :x-colors args)))
         (ansi-colors (apply 'vector (butlast x-colors
                                              (- (length x-colors) 8)))))
    `(progn
       (setq ansi-color-names-vector ,ansi-colors)
       (ansi-color-map-update 'ansi-color-names-vector ,ansi-colors)
       (custom-make-theme-feature ',name)
       (deftheme ,name)
       (let* ,(jao-themes--let-palette palette nil)
         (jao-themes--set-fbg nil)
         (let ((,t-faces (jao-themes--make-faces ',faces)))
           (let* ,(jao-themes--let-palette palette t)
             (jao-themes--set-fbg 'x)
             (let* ((,xfaces (jao-themes--make-faces ',x-faces ',x-colors))
                    (,tx-faces (jao-themes--extract-faces ,t-faces ,xfaces)))
               (put ',name 'theme-immediate t)
               (apply 'custom-theme-set-faces (cons ',name ,tx-faces)))))
         (provide-theme ',name)))))

(put 'jao-define-custom-theme 'lisp-indent-function 1)

(add-to-list 'custom-theme-load-path (file-name-directory load-file-name))



(provide 'jao-themes)
