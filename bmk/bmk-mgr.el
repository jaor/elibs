;;; bmk-mgr.el --- Bookmark manager:

;; Copyright (C) 2003, 2004, 2006, 2007  Jose Antonio Ortega Ruiz.
;;

(defconst bmk-mgr-version "0.1.2")

;; Author: Jose A Ortega Ruiz <jao@gnu.org>
;; Keywords: hypermedia

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;;; INTRODUCTION:
;;;;  Emacs Bookmark Manager.
;;;;
;;;; INSTALLATION:
;;;;
;;;; CUSTOMIZATION:
;;;;
;;;; HISTORY:
;;;;  - 0.1.1 (May 2006). XBEL importing corrected.
;;;;
;;;; TODO:
;;;;   - Export: xbel, HTML, bmk
;;;;   - Add menu: display bookmarks as a menu
;;;;
;;;; THANKS:
;;;;   - David Magill, for lots of help in debugging.
;;;;

;;; Code:

;;;; Dependencies:

(require 'cl)
(require 'outline)
(require 'browse-url)

;;;; Compatibility:
(if (< emacs-major-version 22)
    (progn
      (defun substring-no-properties (x) x)
      (defsubst bmk-string-to-int (x) (string-to-int x)))
  (progn
    (defsubst bmk-string-to-int (x) (string-to-number x))))

;;;; Customization:

;;;;; Customization buffer:
(defgroup bmk-mgr nil
  "Bookmark manager"
  :group 'hypermedia
  :prefix "bmk-mgr-")

(defcustom bmk-mgr-bookmark-file "~/.emacs.bookmarks"
  "The file where bookmarks are stored."
  :group 'bmk-mgr
  :type 'file)

(defcustom bmk-mgr-autosave t
  "If on, save bookmarks whenever they are modified."
  :group 'bmk-mgr
  :type 'boolean)

(defcustom bmk-mgr-indent-width 2
  "The amount of indentation for evey new subfolder level."
  :group 'bmk-mgr
  :type 'number)

(defcustom bmk-mgr-link-mark ""
  "The string used to prefix link names."
  :group 'bmk-mgr
  :type 'string)

(defcustom bmk-mgr-open-mark "- "
  "The string used to prefix open folder names."
  :group 'bmk-mgr
  :type 'string)

(defcustom bmk-mgr-closed-mark "+ "
  "The string used to prefix closed folder names."
  :group 'bmk-mgr
  :type 'string)

(defconst bmk-mgr-available-browsers
  '(choice
    (function-item :tag "Default" :value nil)
    (function-item :tag "Emacs W3" :value  browse-url-w3)
    (function-item :tag "W3 in another Emacs via `gnudoit'"
                   :value  browse-url-w3-gnudoit)
    (function-item :tag "Mozilla" :value  browse-url-mozilla)
    (function-item :tag "Galeon" :value  browse-url-galeon)
    (function-item :tag "Netscape" :value  browse-url-netscape)
    (function-item :tag "Mosaic" :value  browse-url-mosaic)
    (function-item :tag "Mosaic using CCI" :value  browse-url-cci)
    (function-item :tag "IXI Mosaic" :value  browse-url-iximosaic)
    (function-item :tag "Lynx in an xterm window"
                   :value browse-url-lynx-xterm)
    (function-item :tag "Lynx in an Emacs window"
                   :value browse-url-lynx-emacs)
    (function-item :tag "Grail" :value  browse-url-grail)
    (function-item :tag "MMM" :value  browse-url-mmm)
    (function-item :tag "KDE" :value browse-url-kde)
    (function-item :tag "Specified by `Browse Url Generic Program'"
                   :value browse-url-generic)
    (function-item :tag "Default Windows browser"
                   :value browse-url-default-windows-browser)
    (function-item :tag "GNOME invoking Mozilla"
                   :value browse-url-gnome-moz)
    (function-item :tag "Default browser"
                   :value browse-url-default-browser)
    (function :tag "Your own function")
    (alist :tag "Regexp/function association list"
           :key-type regexp :value-type function)))

(defcustom bmk-mgr-browser-function nil
  "*Function to display the current bookmark in a WWW browser.

This has the same semantics as `browse-url''s `browse-url-browser-function'.
If you set this variable to nil, the latter will be used. Otherwise,
if the value is not a function it should be a list of pairs
\(REGEXP . FUNCTION).  In this case the function called will be the one
associated with the first REGEXP which matches the current URL.  The
function is passed the URL and any other args of `browse-url'.  The last
regexp should probably be \".\" to specify a default browser."
  :type  bmk-mgr-available-browsers
  :group 'bmk-mgr)

(defcustom bmk-mgr-alt-browser-function nil
  "Alternative function to display the current bookmark in a WWW browser.

This has the same semantics as `bmk-mgr-browser-function'. You can use
it to have a second browsing function available (activated by pressing
`shift-return' instead of just `return'). A typical application is to
have one to display the bookmark in the current tab, and another to
display the bookmark in a new tab."
  :type  bmk-mgr-available-browsers
  :group 'bmk-mgr)


(defcustom bmk-mgr-inhibit-welcome-message nil
  "When on, do not display a welcome message in the minibuffer upon
entering the bookmark manager."
  :group 'bmk-mgr
  :type 'boolean)

(defcustom bmk-mgr-inhibit-minibuffer nil
  "When on, do not automatically display info about the current folder
or bookmark in the minibuffer."
  :group 'bmk-mgr
  :type 'boolean)

(defcustom bmk-mgr-ignore-fold-state nil
  "Turn this variable on to display the initial tree with all
subfolders closed, instead of using their last state."
  :group 'bmk-mgr
  :type 'boolean)

(defcustom bmk-mgr-use-images nil
  "If on, images are used by default."
  :type 'boolean
  :group 'bmk-mgr)

(defcustom bmk-mgr-folder-open-image "folder-open.xpm"
  "Image to use for representing open folders."
  :type 'file
  :group 'bmk-mgr)

(defcustom bmk-mgr-folder-closed-image "folder-closed.xpm"
  "Image to use for representing closed folders."
  :type 'file
  :group 'bmk-mgr)

(defcustom bmk-mgr-bookmark-image "url.xpm"
  "Image to use for representing bookmarks."
  :type 'file
  :group 'bmk-mgr)

(defcustom bmk-mgr-use-own-frame nil
  "Whether the bookmars buffer should be displayed on its own frame."
  :type 'boolean
  :group 'bmk-mgr)

(defcustom bmk-mgr-frame-parameters '((width . 60))
  "Parameters of the bookmars buffer frame, when
`bmk-mgr-use-own-frame' has been set to non-nil"
  :type '(repeat (sexp :tag "Parameter:"))
  :group 'bmk-mgr)

(defface bmk-mgr-folder-face '((t (:bold t :foreground nil :weight bold)))
  "Face for folder names."
  :group 'bmk-mgr)

(defface bmk-mgr-sel-folder-face
  '((t (:bold t :foreground "IndianRed" :weight bold)))
  "Face for selected folder names."
  :group 'bmk-mgr)

(defface bmk-mgr-bookmark-face '((t ()))
  "Face for bookmark names."
  :group 'bmk-mgr)

(defface bmk-mgr-sel-bookmark-face '((t (:foreground "IndianRed")))
  "Face for selected bookmark names."
  :group 'bmk-mgr)

;;;;; Other variables:

(defvar bmk-mgr-bookmark-buffer-name "*Bookmarks*"
  "*Name of the bookmarks buffer.")

(defvar bmk-mgr-kill-ring-size 50
  "*Maximum number of killed bookmarks to be remembered.")

(defvar bmk-mgr-line-spacing 2
  "*Additional space to put between lines when displaying the
bookmarks buffer.

The space is measured in pixels, and put below lines on window
systems.")

(defvar bmk-mgr-document-title nil
  "Function variable returning the current document title.")

(defvar bmk-mgr-url-at-point nil
  "Function variable returning the value of the url under point.")

(defvar bmk-mgr-current-url nil
  "Function variable returning the value of the current document url.")

(make-variable-buffer-local 'bmk-mgr-document-title)
(make-variable-buffer-local 'bmk-mgr-url-at-point)
(make-variable-buffer-local 'bmk-mgr-current-url)

;;;; User interactive functions:

(defun bmk-mgr-create-bookmark-buffer ()
  (let ((tree (bmk-mgr-read-from-file bmk-mgr-bookmark-file)))
    (when tree
      (when bmk-mgr-use-own-frame
        (select-frame (make-frame bmk-mgr-frame-parameters)))
      (switch-to-buffer
       (get-buffer-create bmk-mgr-bookmark-buffer-name))
      (bmk-mgr-mode tree)
      (current-buffer))))

(defsubst bmk-mgr-get-bookmark-buffer ()
  (or (get-buffer bmk-mgr-bookmark-buffer-name)
      (bmk-mgr-create-bookmark-buffer)))

(defun bmk-mgr-show-bookmarks ()
  "Display the bookmarks buffer."
  (interactive)
  (let ((display-buffer-reuse-frames bmk-mgr-use-own-frame)
        (pop-up-frames bmk-mgr-use-own-frame))
    (switch-to-buffer (bmk-mgr-get-bookmark-buffer))))

(defun bmk-mgr-show-bookmarks-other-window ()
  "Display the bookmarks buffer in other window"
  (interactive)
  (let ((display-buffer-reuse-frames nil)
        (pop-up-frames nil))
    (split-window-horizontally (/ (* 2 (window-width)) 3))
    (other-window 1)
    (switch-to-buffer (bmk-mgr-get-bookmark-buffer))))

(defun bmk-mgr-add-url-at-point ()
  "Add URL at point to the bookmarks collection.
If there is no URL at point, this command asks for it."
  (interactive)
  (if bmk-mgr-url-at-point
      (bmk-mgr-add-bookmark-at-folder (funcall bmk-mgr-url-at-point))
    (progn
      (require 'ffap)
      (bmk-mgr-add-bookmark-at-folder (ffap-url-at-point)))))

;; the following 2 functions need to  be combined and generalized
(defun bmk-mgr-add-current-page ()
  "Adds the current page to the bookmark list."
  (interactive)
  (unless bmk-mgr-current-url
    (error "Current buffer has no associated URL."))
  ;; please leave these here, as I will need them later -- rdc
  ;; (message "bmk-mgr-current-url value as function is %s"
  ;;              bmk-mgr-current-url)
  ;; (message "bmk-mgr-current-url value as variable is %s"
  ;;             (funcall bmk-mgr-current-url))
  ;; (message "bmk-mgr-document-title value as function is %s"
  ;;              bmk-mgr-document-title)
  ;; (message "bmk-mgr-document-title value as variable is %s"
  ;;           (funcall bmk-mgr-document-title))
  (bmk-mgr-add-bookmark-at-folder
   (funcall bmk-mgr-current-url)
   (funcall bmk-mgr-document-title)))

;;;; Bookmark mode:

;;;;; Variables:

(defvar bmk-mgr-kill-ring nil "Killed nodes list")

(defmacro bmk-mgr-folder-or-url (ffun ufun)
  `(lambda ()
     (interactive)
     (if (bmk-mgr-node-folder-p (bmk-mgr-get-node-at-point))
         (funcall ',ffun)
       (funcall ',ufun))))

(defvar bmk-mgr-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map [(control ?a)] 'beginning-of-line)
    (define-key map [(control ?e)] 'end-of-line)
    (define-key map [(control ?k)] 'bmk-mgr-kill-bookmark)
    (define-key map [(control ?n)] 'bmk-mgr-next-line)
    (define-key map [(control ?p)] 'bmk-mgr-previous-line)
    (define-key map [(control ?y)] 'bmk-mgr-yank-bookmark)
    (define-key map [??] 'describe-mode)
    (define-key map [?A] 'bmk-mgr-add-folder)
    (define-key map [?I] 'bmk-mgr-toggle-images)
    (define-key map [?N] 'bmk-mgr-next-folder)
    (define-key map [?P] 'bmk-mgr-previous-folder)
    (define-key map [?Q] 'bmk-mgr-quit)
    (define-key map [?V] 'bmk-mgr-version)
    (define-key map [?a] 'bmk-mgr-add-bookmark)
    (define-key map [?c] 'bmk-mgr-close-children)
    (define-key map [?d] 'bmk-mgr-move-bookmark-down)
    (define-key map [?e] 'bmk-mgr-edit-bookmark)
    (define-key map [?f] 'bmk-mgr-find-folder)
    (define-key map [?h] 'describe-mode)
    (define-key map [?i] 'bmk-mgr-import)
    (define-key map [?n] 'bmk-mgr-next-line)
    (define-key map [?p] 'bmk-mgr-previous-line)
    (define-key map [?q] 'bmk-mgr-quit-ask)
    (define-key map [?s] 'bmk-mgr-save-bookmarks)
    (define-key map [?u] 'bmk-mgr-move-bookmark-up)
    (define-key map [?v] 'bmk-mgr-bookmark-info)
    (define-key map [?y] 'bmk-mgr-copy-url)
    (define-key map (kbd "<up>") 'bmk-mgr-previous-line)
    (define-key map (kbd "<down>") 'bmk-mgr-next-line)
    (define-key map (kbd "<left>") 'beginning-of-line)
    (define-key map (kbd "<right>") 'end-of-line)
    (define-key map (kbd "<mouse-1>") 'bmk-mgr-mouse-click)
    (define-key map (kbd "<mouse-2>") 'bmk-mgr-mouse-click-alt)
    (define-key map (kbd "<S-return>") 'bmk-mgr-browse-url-alt)
    (define-key map (kbd "M-RET") 'bmk-mgr-browse-url-alt)
    (define-key map (kbd "RET") 'bmk-mgr-browse-url)
    (define-key map (kbd "TAB") 'bmk-mgr-toggle-folder)
    map)
  "Keymap for `bmk-mgr-mode'.")

(defvar bmk-mgr-mode-syntax-table
  (let ((st (make-syntax-table)))
    st)
  "Syntax table for `bmk-mgr-mode'.")

;; regexps used by bmk-mgr-mode and other functions
(defvar bmk-mgr-outline-regexp nil)

;; images
(defvar bmk-mgr-url-img)
(defvar bmk-mgr-fopen-img)
(defvar bmk-mgr-fclosed-img)

;;;;; Mode definition:

;;;###autoload
(defun bmk-mgr-mode (&optional tree)
  "\\<bmk-mgr-mode-map>
   Major mode for displaying bookmark files.

Commands:

<DIGIT>+<key>\tRepeat command denoted by <key> the number of times
             \tpreviously typed. Commands accepting a prefix count are
             \tmarked with (*) below.

\\[bmk-mgr-next-line]\tGo to next visible line (*).
\\[bmk-mgr-previous-line]\tGo to previous visible line (*).
\\[bmk-mgr-next-folder]\tGo to next visible folder (*).
\\[bmk-mgr-previous-folder]\tGo to previous visible folder (*).
\\[beginning-of-line]\tGo to the beginning of text in current line.
\\[end-of-line]\tGo to the end of text in current line.
\\[bmk-mgr-toggle-folder]\tOpens or closes current folder.
\\[bmk-mgr-close-children]\tCloses all subfolders of current folder.
\\[bmk-mgr-bookmark-info]\tDisplay info about current bookmark or folder.
\\[bmk-mgr-copy-url]\tPut the current URL (if any) in the kill ring.
\\[bmk-mgr-find-folder]\tFind bookmarks folder.

\\[bmk-mgr-mouse-click]\tDisplay or toggle the clicked URL or folder.
\\[bmk-mgr-mouse-click-alt]\tDisplay or toggle the clicked URL or folder,
                           \tusing the alternate browser.
\\[bmk-mgr-browse-url]\tDisplay current URL in browser.
\\[bmk-mgr-browse-url-alt]\tDisplay current URL in alternate browser.

\\[bmk-mgr-move-bookmark-up]\tMoves current bookmark one line up (*).
\\[bmk-mgr-move-bookmark-down]\tMoves current bookmark one line down (*).
\\[bmk-mgr-edit-bookmark]\tEdit bookmark or folder in current line.
\\[bmk-mgr-add-bookmark]\tAdd a new bookmark (asks for its name and URL).
\\[bmk-mgr-add-folder]\tAdd a new bookmark folder (asks for its path).
\\[bmk-mgr-kill-bookmark]\tKills current bookmark or folder, putting it in the kill ring (*).
\\[bmk-mgr-yank-bookmark]\tYanks a previously killed bookmark or folder (*).

\\[bmk-mgr-import]\tImports an external bookmarks file (xbel, w3m, bmk).

\\[bmk-mgr-toggle-images]\tToggle display of images.

\\[bmk-mgr-save-bookmarks]\tSave current bookmarks.
\\[bmk-mgr-quit]\tQuit Bookmark Manager.
\\[bmk-mgr-quit-ask]\tQuit Bookmark Manager asking for confirmation.

\\[bmk-mgr-version]\tShow version.
\\[describe-mode]\tShows this help page.
"
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq major-mode 'bmk-mgr-mode)
  (setq mode-name "bmk")
  (use-local-map bmk-mgr-mode-map)

  (let ((prefix (make-string bmk-mgr-indent-width 32)))
    (setq bmk-mgr-outline-regexp (concat "\\(" prefix "\\)*.")))
  (set (make-local-variable 'outline-regexp) bmk-mgr-outline-regexp)
  (set (make-local-variable 'truncate-lines) t)
  (set (make-local-variable 'automatic-hscrolling) t)
  (set (make-local-variable 'line-spacing) bmk-mgr-line-spacing)
  (set (make-local-variable 'kill-whole-line) t)
  (set (make-local-variable 'next-line-add-newlines) t)
  (goto-char 1)
  (bmk-mgr-refresh tree)
  (toggle-read-only 1)
  (unless bmk-mgr-inhibit-welcome-message
    (message
     "Emacs Bookmark Manager, version %s. Type `h' for help." bmk-mgr-version)))

;;;;; Functions:

;;;;;; Helper macros:
(defmacro bmk-mgr-with-bookmarks-buffer (&rest body)
  `(with-current-buffer (bmk-mgr-get-bookmark-buffer)
     (unwind-protect
         (prog1
             (let ((inhibit-read-only t))
               (bmk-mgr-unmark-current)
               ,@body)
           (if (not bmk-mgr-inhibit-minibuffer) (bmk-mgr-bookmark-info)))
       (bmk-mgr-mark-current))))

(defmacro bmk-mgr-with-current-node (&rest body)
  `(bmk-mgr-with-bookmarks-buffer
    (beginning-of-line)
    (let ((bmk-node (bmk-mgr-get-node-at-point))
          (bmk-path (bmk-mgr-get-path-at-point)))
      ,@body)))

(defmacro bmk-mgr-with-current-node-save (&rest body)
  `(bmk-mgr-with-current-node
    (prog1
        (progn ,@body)
      (if bmk-mgr-autosave
          (progn
            (bmk-mgr-save-current-tree)
            (set-buffer-modified-p nil))))))

(defmacro bmk-mgr-repeat (&rest body)
  `(let ((count bmk-mgr-repeat-count))
     (while (> count 0)
       (decf count)
       ,@body)))

;;;;;; Helper functions:
(defun bmk-mgr-outline-level ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at bmk-mgr-outline-regexp)
        (length (match-string 0))
      0)))

(defun bmk-mgr-mark-current ()
  (let* ((inhibit-read-only 1)
         (node (bmk-mgr-get-node-at-point))
         (face (if (bmk-mgr-node-folder-p node) 'bmk-mgr-sel-folder-face
                 'bmk-mgr-sel-bookmark-face)))
    (beginning-of-line)
    (save-excursion
      (add-text-properties (progn (bmk-mgr-beginning) (point))
                           (progn (end-of-line) (point))
                       `(face ,face)))))

(defun bmk-mgr-unmark-current ()
  (let* ((inhibit-read-only 1)
         (node (bmk-mgr-get-node-at-point))
         (face (if (bmk-mgr-node-folder-p node) 'bmk-mgr-folder-face
                 'bmk-mgr-bookmark-face)))
    (add-text-properties (progn (bmk-mgr-beginning) (point))
                         (save-excursion (end-of-line) (point))
                         `(face ,face))))

(defun bmk-mgr-unmark-all ()
  (save-excursion
    (goto-char 1)
    (while (not (eobp))
      (bmk-mgr-unmark-current)
      (next-line 1))))

(defun bmk-mgr-push-to-kill-ring (node)
  (push (copy-list node) bmk-mgr-kill-ring)
  (when (> (length bmk-mgr-kill-ring) bmk-mgr-kill-ring-size)
    (setcdr (nthcdr (1- bmk-mgr-kill-ring-size) bmk-mgr-kill-ring) nil)))

(defsubst bmk-mgr-pop-kill-ring () (pop bmk-mgr-kill-ring))

(defsubst bmk-mgr-url-at-point ()
  "Get the URL of the current bookmark, if any."
  (bmk-mgr-node-url (bmk-mgr-get-node-at-point)))

(defun bmk-mgr-save-current-tree ()
  (bmk-mgr-save-to-file (bmk-mgr-get-root-node-in-buffer)
                        bmk-mgr-bookmark-file))

(defun bmk-mgr-ask-path (prompt &optional path)
  (let ((PC-word-delimiters ".")
        (bmk-mgr-inhibit-minibuffer t)
        (path (or path (list (bmk-mgr-node-name
                               (bmk-mgr-get-root-node-in-buffer))))))
    (bmk-mgr-string-to-path
     (completing-read prompt 'bmk-mgr-complete-path nil nil
                      (concat (bmk-mgr-path-to-string path) "/")))))

(defun bmk-mgr-complete-path (pstr fun flag)
  (bmk-mgr-with-bookmarks-buffer
   (let* ((root (bmk-mgr-get-root-node-in-buffer))
         (partial (not (string-match "/$" pstr)))
         (pc (split-string pstr "/"))
         (path (or pc (list (bmk-mgr-node-name root))))
         (ppath (if partial (bmk-mgr-path-parent path) path))
         (partstr (concat "^" (regexp-quote (bmk-mgr-path-leaf path))))
         (str (concat (bmk-mgr-path-to-string ppath) "/"))
         (children (bmk-mgr-node-child-folders root ppath))
         (comp (mapcar (lambda (x) (concat str (bmk-mgr-node-name x) "/"))
                        (if partial
                            (remove-if-not
                             (lambda (x)
                               (string-match partstr (bmk-mgr-node-name x)))
                             children)
                          children)))
         (len (length comp)))
     (case flag
       ((nil) (cond
               ((and (not partial) (zerop len)) t)
               ((= len 1) (car comp))
               ((zerop len) nil)
               (t pstr)))
       ((lambda) (not partial))
       (t comp)))))

;;;;;; Mode functions:

(defun bmk-mgr-version ()
  "Display version."
  (interactive)
  (message "Emacs Bookmark Manager, version %s" bmk-mgr-version))

(defun bmk-mgr-toggle-images ()
  "Toggle image display."
  (interactive)
  (setq bmk-mgr-use-images (not bmk-mgr-use-images))
  (bmk-mgr-refresh))

(defun bmk-mgr-refresh (&optional tree)
  "Refresh the bookmarks buffer."
  (interactive)
  (message "Redisplaying bookmarks...")
  (bmk-mgr-with-bookmarks-buffer
   (if window-system
       (progn
         (clear-image-cache
          (window-frame (get-buffer-window (current-buffer))))
         (setq bmk-mgr-url-img
               (find-image
                `((:file ,bmk-mgr-bookmark-image :type xpm :ascent center))))
         (setq bmk-mgr-fopen-img
               (find-image
                `((:file ,bmk-mgr-folder-open-image :type xpm :ascent 95))))
         (setq bmk-mgr-fclosed-img
               (find-image
                `((:file ,bmk-mgr-folder-closed-image :type xpm :ascent 95)))))
     (setq bmk-mgr-url-img nil bmk-mgr-fopen-img nil bmk-mgr-fclosed-img nil
           bmk-mgr-use-images nil))
   (let ((tree (or tree (bmk-mgr-get-root-node-in-buffer))))
     (save-excursion
       (erase-buffer)
       (if bmk-mgr-ignore-fold-state (bmk-mgr-node-close-all-children tree))
       (bmk-mgr-print-tree tree)
       (goto-char 1)
       (bmk-mgr-unmark-all)
       (bmk-mgr-refresh-open-close)))
  (message "Redisplaying bookmarks... done.")))

(defsubst bmk-mgr-beginning ()
  "Go to beginning of current bookmark."
  (interactive)
  (beginning-of-line)
  (re-search-forward "^ *"))

(defun bmk-mgr-next-line (arg)
  "Go to next visible bookmark line."
  (interactive "P")
  (bmk-mgr-with-bookmarks-buffer
   (outline-next-visible-heading (if arg (prefix-numeric-value arg) 1))
   (if (eobp) (outline-previous-visible-heading 1))))

(defun bmk-mgr-previous-line (arg)
  "Go to previous visible bookmark line."
  (interactive "P")
  (bmk-mgr-with-bookmarks-buffer
   (outline-previous-visible-heading (if arg (prefix-numeric-value arg) 1))))

(defun bmk-mgr-bookmark-info ()
  "Show info about current bookmark or folder."
  (interactive)
  (let ((node (bmk-mgr-get-node-at-point)))
    (if node
        (if (bmk-mgr-node-url-p node)
            (let ((url (bmk-mgr-node-url node)))
              (and url (message "%s" url)))
          (let ((children (bmk-mgr-node-child-folders node)))
            (if children
                (message
                 "%s"
                 (concat "Subfolders: "
                         (mapconcat 'bmk-mgr-node-name children ", ")))))))))

(defun bmk-mgr-copy-url ()
  "Put current URL in the kill ring."
  (interactive)
  (bmk-mgr-with-current-node
   (let ((url (bmk-mgr-node-url bmk-node)))
     (when url
       (kill-new url)
       (message "%s copied" url)))))

(defun bmk-mgr-next-folder (arg)
  "Go to next visible bookmark folder."
  (interactive "P")
  (bmk-mgr-with-bookmarks-buffer
   (let ((count (if arg (prefix-numeric-value arg) 1))
         (test (lambda () (bmk-mgr-node-url-p (bmk-mgr-get-node-at-point)))))
     (while (> count 0)
       (decf count)
       (if (not (funcall test)) (outline-next-visible-heading 1))
       (while (funcall test)
         (outline-next-visible-heading 1))))
   (if (eobp) (outline-previous-visible-heading 1))
   (bmk-mgr-beginning)))

(defun bmk-mgr-previous-folder (arg)
  "Go to previous visible bookmark folder."
  (interactive "P")
  (bmk-mgr-with-bookmarks-buffer
   (let ((count (if arg (prefix-numeric-value arg) 1))
         (test (lambda () (bmk-mgr-node-url-p (bmk-mgr-get-node-at-point)))))
     (while (> count 0)
       (decf count)
       (if (not (funcall test)) (outline-previous-visible-heading 1))
       (while (funcall test)
         (outline-previous-visible-heading 1))))
   (bmk-mgr-beginning)))

(defun bmk-mgr-browse-url ()
  "Display current bookmark in browser."
  (interactive)
  (let ((browse-url-browser-function
         (or bmk-mgr-browser-function browse-url-browser-function))
        (url (bmk-mgr-node-url (bmk-mgr-get-node-at-point))))
    (if url (browse-url url) (bmk-mgr-toggle-folder))))

(defun bmk-mgr-browse-url-alt ()
  "Display current bookmark in alternate browser."
  (interactive)
  (bmk-mgr-with-current-node
   (let ((browse-url-browser-function
          (or bmk-mgr-alt-browser-function browse-url-browser-function))
         (url (bmk-mgr-node-url (bmk-mgr-get-node-at-point))))
     (if url (browse-url url) (bmk-mgr-toggle-folder)))))

(defun bmk-mgr-mouse-click (event)
  "Visit the clicked bookmark or toogle the folder state."
  (interactive "e")
  (set-buffer (bmk-mgr-get-bookmark-buffer))
  (goto-char (posn-point (event-start event)))
  (let ((node (bmk-mgr-get-node-at-point)))
    (if (bmk-mgr-node-url-p node)
        (bmk-mgr-browse-url)
      (if (bmk-mgr-node-folder-p node)
          (bmk-mgr-toggle-folder)))))

(defun bmk-mgr-mouse-click-alt (event)
  "Visit the clicked bookmark or toogle the folder state."
  (interactive "e")
  (set-buffer (bmk-mgr-get-bookmark-buffer))
  (goto-char (posn-point (event-start event)))
  (let ((node (bmk-mgr-get-node-at-point)))
    (if (bmk-mgr-node-url-p node)
        (bmk-mgr-browse-url-alt)
      (if (bmk-mgr-node-folder-p node)
          (bmk-mgr-toggle-folder)))))

(defun bmk-mgr-toggle-folder ()
  "Toggle the open/closed status of folder at point, if any."
  (interactive)
  (bmk-mgr-with-current-node
   (when (bmk-mgr-node-folder-p bmk-node)
     (bmk-mgr-node-toggle-open-closed bmk-node)
     (bmk-mgr-redraw-node-at-point))))

(defun bmk-mgr-close-children ()
  "Close all subfolders of folder at point, if any."
  (interactive)
  (bmk-mgr-with-current-node
   (when (bmk-mgr-node-folder-p bmk-node)
     (bmk-mgr-node-close-all-children bmk-node)
     (bmk-mgr-update-tree-at-point)
     (bmk-mgr-refresh-open-close))))

(defun bmk-mgr-find-folder ()
  "Find a bookmarks folder."
  (interactive)
  (bmk-mgr-with-bookmarks-buffer
   (bmk-mgr-find-path-in-buffer (bmk-mgr-ask-path "Find folder: ") t)))

(defun bmk-mgr-save-bookmarks ()
  "Save current bookmars."
  (interactive)
  (when (y-or-n-p "Save current bookmarks? ")
    (with-current-buffer (bmk-mgr-get-bookmark-buffer)
      (bmk-mgr-save-current-tree)
      (set-buffer-modified-p nil))))

(defun bmk-mgr-edit-bookmark ()
  "Edit the current bookmark."
  (interactive)
  (bmk-mgr-with-current-node-save
   (when bmk-node
     (let ((newtitle (read-string "Name: " (bmk-mgr-node-title bmk-node))))
       (if (> (length newtitle) 0) (bmk-mgr-node-set-name bmk-node newtitle))
       (if (bmk-mgr-node-url-p bmk-node)
           (let ((newurl (read-string "URL: " (bmk-mgr-node-url bmk-node))))
             (if (> (length newurl) 0) (bmk-mgr-node-set-url bmk-node newurl))))
       (bmk-mgr-redraw-node-at-point
        (append (bmk-mgr-path-parent bmk-path) (list newtitle)))
       (if (bmk-mgr-node-folder-p bmk-node) ; update children paths
           (save-excursion
             (let ((cl (bmk-mgr-outline-level))
                   (pos (length (bmk-mgr-path-parent bmk-path))))
               (forward-line 1)
               (while (> (bmk-mgr-outline-level) cl)
                 (setf (nth pos (bmk-mgr-get-path-at-point)) newtitle)
                 (forward-line 1)))))
       (beginning-of-line)))))

(defun bmk-mgr-add-bookmark-at-folder (&optional url title)
  (let ((path
         (bmk-mgr-with-current-node
          (bmk-mgr-ask-path "Add bookmark to folder: "
                            (if (bmk-mgr-node-folder-p bmk-node) bmk-path
                              (bmk-mgr-path-parent bmk-path))))))
    (bmk-mgr-add-bookmark path nil url title t)))


(defun bmk-mgr-add-bookmark (&optional path node url title after)
  "Insert bookmark at a given path or current point."
  (interactive)
  (bmk-mgr-with-current-node-save
   (let* ((title (or title
                     (and node (bmk-mgr-node-name node))
                     (read-string "Name of new bookmark: ")))
          (url (or (and node "") url (read-string "URL: ")))
          (node (or node (bmk-mgr-node-url-new title url))))
     (if (and path (not (bmk-mgr-find-path-in-buffer path t)))
         (error "Folder %s does not exist"
                (bmk-mgr-path-to-string path)))
     (message "adding with path %S (%S)" path after)
     (if (not (or path bmk-path)) (outline-previous-visible-heading 1))
     (if (and (bmk-mgr-node-folder-p (bmk-mgr-get-node-at-point))
              (bmk-mgr-node-open-p (bmk-mgr-get-node-at-point)))
         (bmk-mgr-insert-child-at-point node (not after))
       (bmk-mgr-insert-sibling-at-point node nil))
     (while (not (or (eobp) (eq node (bmk-mgr-get-node-at-point))))
       (outline-next-visible-heading 1)))))

(defun bmk-mgr-add-folder ()
  "Insert new bookmarks folder."
  (interactive)
  (bmk-mgr-with-current-node-save
   (let* ((fpath (if (bmk-mgr-node-folder-p bmk-node) bmk-path
                   (bmk-mgr-path-parent bmk-path)))
          (npath (bmk-mgr-ask-path "New folder: " fpath))
          (pnpath (bmk-mgr-path-parent npath))
          (sibling (and (not (equal bmk-path fpath)) ; inserting besides a url
                        (equal fpath pnpath))))      ; in the same folder
     (if (and (not (equal fpath pnpath))
              (not (bmk-mgr-find-path-in-buffer pnpath t)))
         (error "Folder %s does not exist" (bmk-mgr-path-to-string pnpath)))
     (let ((node (bmk-mgr-node-folder-new (bmk-mgr-path-leaf npath))))
       (if sibling
           (bmk-mgr-insert-sibling-at-point node nil)
         (bmk-mgr-insert-child-at-point node t))
       (bmk-mgr-goto-node-around node)))))

(defun bmk-mgr-yank-bookmark (arg)
  "Yank last killed bookmark at point."
  (interactive "P")
  (let ((count (if arg (prefix-numeric-value arg) 1)))
    (while (> count 0)
      (decf count)
      (bmk-mgr-with-current-node-save
       (bmk-mgr-add-bookmark nil (bmk-mgr-pop-kill-ring))))))

(defun bmk-mgr-delete-node-at-point ()
  (let ((path (bmk-mgr-get-path-at-point)))
    (beginning-of-line)
    (hide-subtree)
    (let ((a (point))
          (b (save-excursion (outline-next-visible-heading 1) (point))))
      (if bmk-mgr-use-images (remove-images a b))
      (delete-region a b)
      (if (eobp) (outline-previous-visible-heading 1)))
    (bmk-mgr-delete-node (bmk-mgr-get-root-node-in-buffer) path)))

(defun bmk-mgr-kill-bookmark (arg)
  "Delete bookmark at point."
  (interactive "P")
  (let ((count (if arg (prefix-numeric-value arg) 1)))
    (while (> count 0)
      (decf count)
      (bmk-mgr-with-current-node-save
       (if (not (bmk-mgr-path-parent bmk-path))
           (error "Cannot kill root node"))
       (if (and (bmk-mgr-node-folder-p bmk-node)
                (not (null (bmk-mgr-node-children bmk-node)))
                (not (y-or-n-p
                      (format
                       "Killing `%s' and all its contents. Are you sure? "
                       (bmk-mgr-path-leaf bmk-path)))))
           (error "Cancelled"))
       (bmk-mgr-push-to-kill-ring bmk-node)
       (bmk-mgr-delete-node-at-point)))))

(defun bmk-mgr-transpose-lines (node path count &optional up)
  (beginning-of-line)
  (outline-next-visible-heading (if up count (* -1 count)))
  (let ((eol (save-excursion (end-of-line) (point))))
    (if bmk-mgr-use-images (remove-images (point) eol))
    (delete-region (point) (1+ eol))
    (outline-next-visible-heading (if up (* -1 count) count))
    (bmk-mgr-print-single-node-at-point node path t)))

(defun bmk-mgr-goto-node-around (node &optional width)
  (let ((width (or width 2)))
    (outline-previous-visible-heading (1+ width))
    (do ((max (1+ (* 2 width))) (n 0 (incf n)))
        ((or (> n max) (eq node (bmk-mgr-get-node-at-point))))
      (outline-next-visible-heading 1))))

(defun bmk-mgr-move-bookmark-up (arg)
  "Move bookmark at point one line up."
  (interactive "P")
  (bmk-mgr-with-current-node-save
   (let ((ppath (bmk-mgr-path-parent bmk-path))
         (count (if arg (prefix-numeric-value arg) 1)))
     (when (and (> count 0)
                (bmk-mgr-node-url-p bmk-node)
                (> (length bmk-path) 1))
       (beginning-of-line)
       (let ((p (point)))
         (outline-previous-visible-heading count)
         (if (= (bmk-mgr-outline-level) 1)
             (progn
               (outline-next-visible-heading 1)
               (when (not (equal bmk-node (bmk-mgr-get-node-at-point)))
                 (goto-char p)
                 (bmk-mgr-delete-node-at-point)
                 (goto-char 1)
                 (bmk-mgr-insert-child-at-point bmk-node t)))
           (let* ((current (bmk-mgr-get-node-at-point))
                  (iscl (bmk-mgr-node-closed-p current))
                  (isurl (bmk-mgr-node-url-p current))
                  (cpath (bmk-mgr-get-path-at-point))
                  (cppath (bmk-mgr-path-parent cpath)))
             (cond
              ((and (equal ppath cppath) (or isurl iscl))
               (bmk-mgr-node-swap-children-at-path
                (bmk-mgr-get-root-node-in-buffer) ppath bmk-node current)
               (bmk-mgr-transpose-lines bmk-node bmk-path count t))
              (t
               (outline-next-visible-heading count)
               (bmk-mgr-delete-node-at-point)
               (outline-previous-visible-heading count)
               (if (or isurl iscl (equal cpath ppath))
                   (bmk-mgr-insert-sibling-at-point bmk-node
                                                    (equal cpath ppath))
                 (bmk-mgr-insert-child-at-point bmk-node nil)))))))
       (bmk-mgr-goto-node-around bmk-node)))))

(defun bmk-mgr-move-bookmark-down (arg)
  "Move bookmark at point one line down."
  (interactive "P")
  (bmk-mgr-with-current-node-save
   (let ((ppath (bmk-mgr-path-parent bmk-path))
         (count (if arg (prefix-numeric-value arg) 1)))
     (when (and (> count 0)
                (bmk-mgr-node-url-p bmk-node)
                (> (length bmk-path) 1))
     (beginning-of-line)
     (let ((p (point)))
       (outline-next-visible-heading count)
       (if (null (bmk-mgr-get-node-at-point))
           (progn
             (outline-previous-visible-heading 1)
             (when (not (equal bmk-node (bmk-mgr-get-node-at-point)))
               (goto-char p)
               (bmk-mgr-delete-node-at-point)
               (goto-char 1)
               (bmk-mgr-insert-child-at-point bmk-node nil)
               (goto-char (point-max))))
         (let* ((current (bmk-mgr-get-node-at-point))
                (iscl (bmk-mgr-node-closed-p current))
                (isurl (bmk-mgr-node-url-p current))
                (cpath (bmk-mgr-get-path-at-point))
                (isout (< (length cpath) (length bmk-path)))
                (cppath (bmk-mgr-path-parent cpath)))
           (cond
            ((and (equal ppath cppath) (or isurl iscl))
             (bmk-mgr-node-swap-children-at-path
              (bmk-mgr-get-root-node-in-buffer) ppath bmk-node current)
             (bmk-mgr-transpose-lines bmk-node bmk-path count nil))
            (t
             (outline-previous-visible-heading count)
             (bmk-mgr-delete-node-at-point)
             (outline-next-visible-heading (1- count))
             (if (or isurl iscl isout)
                 (bmk-mgr-insert-sibling-at-point bmk-node isout)
               (bmk-mgr-insert-child-at-point bmk-node t)))))))
     (bmk-mgr-goto-node-around bmk-node)))))

(defvar bmk-mgr-import-formats '(("xbel" . bmk-mgr-import-xbel)
                                 ("bmk" . bmk-mgr-import-bmk)))

(defun bmk-mgr-import-add-formatter (name fun)
  (add-to-list 'bmk-mgr-import-formats `(,name . ,fun)))

(defun bmk-mgr-import ()
  "Import bookmarks file."
  (interactive)
  (let* ((formats bmk-mgr-import-formats)
         (names (mapcar 'car formats))
         (prompt (concat "Format (" (mapconcat 'identity names ", ") "): "))
         (sel (completing-read prompt formats nil 1))
         (fun (cdr (assoc sel formats))))
    (if fun
        (bmk-mgr-with-bookmarks-buffer
         (let* ((file (read-file-name "File: " nil nil t))
                (folder (bmk-mgr-ask-path "Import to folder: "))
                (ign (message "Reading %s..." file))
                (node (funcall fun file (bmk-mgr-path-leaf folder))))
           (when node
             (message "Importing bookmarks...")
             (if (bmk-mgr-find-path-in-buffer folder t)
                 (let ((parent (bmk-mgr-get-node-at-point))
                       (children (bmk-mgr-node-children node)))
                   (if (bmk-mgr-node-folder-p parent)
                       (progn
                         (mapc (lambda (x)
                                 (bmk-mgr-node-add-child parent x)) children)
                         (bmk-mgr-update-tree-at-point)
                         (bmk-mgr-refresh-open-close)
                         (message nil))
                     (message "`%s' is not a correct insertion point"
                              (bmk-mgr-node-name parent))))
               (if (bmk-mgr-find-path-in-buffer (bmk-mgr-path-parent folder) t)
                   (progn
                     (bmk-mgr-insert-child-at-point node nil)
                     (message nil))))))))))

(defun bmk-mgr-quit-ask ()
  "Quit bookmarks buffer, asking for confirmation."
  (interactive)
  (when (y-or-n-p "Close bookmarks browser? ") (bmk-mgr-quit)))

(defun bmk-mgr-quit ()
  "Quit bookmarks buffer."
  (interactive)
  (with-current-buffer (bmk-mgr-get-bookmark-buffer)
   (bmk-mgr-save-current-tree)
   (kill-buffer (current-buffer))))

;;;; Import/export:

;;;;; xbel:
(defun bmk-mgr-xbel-get-title (node def)
  (let* ((title-node (car (xml-get-children node 'title)))
         (title-body (or (and title-node (xml-node-children title-node))
                         '())))
    (bmk-mgr-filter-html
     (or (and title-body (stringp (car title-body)) (car title-body)) def))))

(defun bmk-mgr-xbel-to-bmk (xbel &optional name)
  (when (listp xbel)
    (case (xml-node-name xbel)
      (xbel (bmk-mgr-node-folder-new
             (or name "xbel") nil
             (mapcar 'bmk-mgr-xbel-to-bmk
                     (append (xml-get-children xbel 'bookmark)
                             (xml-get-children xbel 'folder)))))
      (folder (bmk-mgr-node-folder-new
               (bmk-mgr-xbel-get-title xbel "folder")
               (equal (xml-get-attribute xbel 'folded) "yes")
               (mapcar 'bmk-mgr-xbel-to-bmk
                       (append (xml-get-children xbel 'bookmark)
                               (xml-get-children xbel 'folder)))))
      (bookmark
       (let* ((href (bmk-mgr-filter-html (xml-get-attribute xbel 'href)))
              (title (bmk-mgr-xbel-get-title xbel href)))
         (bmk-mgr-node-url-new title href))))))

(defun bmk-mgr-import-xbel (file name)
  (save-current-buffer
    (if (not (file-readable-p file)) (error "Cannot read file"))
    (require 'xml)
    (message "Reading XBEL file...")
    (bmk-mgr-xbel-to-bmk
     (car (with-temp-buffer
            (insert-buffer (find-file-noselect file))
            (beginning-of-buffer)
            (while (re-search-forward "\n" nil t) (replace-match ""))
            (beginning-of-buffer)
            (while (re-search-forward "\"\"" nil t) (replace-match "\"empty\""))
            (beginning-of-buffer)
            (while (re-search-forward "> +<" nil t) (replace-match "><"))
            (xml-parse-region (point-min) (point-max))))
     name)))

;;;;; aux:
(defconst bmk-mgr-html-scp  "&#[0-9]+\\;")

(defun bmk-mgr-filter-html (str)
  (let* ((str (substring-no-properties str))
         (result "")
         (p0 0)
         (p1 (string-match bmk-mgr-html-scp str)))
    (while p1
      (let* ((p2 (match-end 0))
             (ch
              (char-to-string (bmk-string-to-int (substring
                                                  str (+ 2 p1) (1- p2))))))
        (setf result (concat result (substring str p0 p1) ch))
        (setf p0 p2)
        (setf p1 (string-match bmk-mgr-html-scp str p2))))
    (concat result (substring str p0))))

;;;; Bookmarks buffer:

;;;;; Functions:
(defun bmk-mgr-print-single-node-at-point (node path &optional insert)
  (beginning-of-line)
  (let ((kill-whole-line nil)
        (inhibit-read-only t)
        (depth (* (- (length path) 1) bmk-mgr-indent-width))
        (txt-mark "")
        (img))
    (if (bmk-mgr-node-folder-p node)
        (if (bmk-mgr-node-open-p node)
            (setq txt-mark bmk-mgr-open-mark img bmk-mgr-fopen-img)
          (setq txt-mark bmk-mgr-closed-mark img bmk-mgr-fclosed-img))
      (if (> (length (bmk-mgr-node-url node)) 0)
          (setq txt-mark bmk-mgr-link-mark img bmk-mgr-url-img)))
    (if insert
        (progn (newline)
               (forward-line -1))
      (kill-line))
    (delete-trailing-whitespace)
    (remove-images (point) (save-excursion (end-of-line) (point)))
    (insert (make-string depth 32))
    (if bmk-mgr-use-images
        (if img ; no image for separators
            (progn (put-image img (point)) (insert " ")))
      (insert txt-mark))
    (insert (bmk-mgr-node-title node))
    (bmk-mgr-set-path-at-point path)
    (bmk-mgr-set-node-at-point node)))

(defun bmk-mgr-print-tree (tree &optional path level)
  (let* ((kill-whole-line nil)
         (inhibit-read-only t)
         (next-line-add-newlines nil)
         (insertp
          (lambda (node path)
            (not
             (and (equal path (bmk-mgr-get-path-at-point))
                  (equal (bmk-mgr-node-type node)
                         (bmk-mgr-node-type (bmk-mgr-get-node-at-point)))))))
         (pfun
          (lambda (n w)
            (beginning-of-line)
            (let* ((title (bmk-mgr-node-title n))
                   (neww (append w (list title))))
              (bmk-mgr-print-single-node-at-point
               n neww (funcall insertp n neww))
              (if (eobp) (newline))
              (next-line 1)
              (cons neww t)))))
    (bmk-mgr-visit-tree tree pfun path)))

(defsubst bmk-mgr-update-tree-at-point ()
  (save-excursion
    (bmk-mgr-print-tree (bmk-mgr-get-node-at-point)
                        (bmk-mgr-path-parent (bmk-mgr-get-path-at-point)))))

(defsubst bmk-mgr-set-path-at-point (path &optional buffer)
  (let ((inhibit-field-text-motion t)
        (pos (save-excursion (end-of-line) (point))))
    (save-excursion
      (beginning-of-line)
      (add-text-properties (point) pos (list 'bmk-mgr-path path) buffer))))

(defsubst bmk-mgr-set-node-at-point (node &optional buffer)
  (let ((inhibit-field-text-motion t)
        (pos (save-excursion (end-of-line) (point))))
    (save-excursion
      (beginning-of-line)
      (add-text-properties (point) pos (list 'bmk-mgr-node node) buffer)
      (when (bmk-mgr-node-url-p node)
        (bmk-mgr-beginning)
        (add-text-properties (point) (1- pos)
                             (list 'mouse-face 'bmk-mgr-sel-bookmark-face)
                           buffer)))))

(defsubst bmk-mgr-get-path-at-point (&optional buffer)
  (get-text-property (point) 'bmk-mgr-path buffer))

(defsubst bmk-mgr-get-node-at-point (&optional buffer)
  (get-text-property (point) 'bmk-mgr-node buffer))

(defun bmk-mgr-get-root-node-in-buffer (&optional buffer)
  (save-current-buffer
    (if buffer (set-buffer buffer))
    (save-excursion
      (goto-char (point-min))
      (bmk-mgr-get-node-at-point))))

(defun bmk-mgr-refresh-open-close ()
  (save-excursion
    (let* ((node (bmk-mgr-get-node-at-point))
           (path (bmk-mgr-get-path-at-point))
           (cl (length path)))
      (unless (eobp)
        (bmk-mgr-unmark-current)
        (if (bmk-mgr-node-open-p node)
            (progn
              (show-children)
              (outline-next-visible-heading 1)
              (while (> (length (bmk-mgr-get-path-at-point)) cl)
                (bmk-mgr-refresh-open-close)
                (outline-next-visible-heading 1)))
          (hide-subtree))))))

(defun bmk-mgr-redraw-node-at-point (&optional path)
  (save-excursion
    (let ((node (bmk-mgr-get-node-at-point)))
      (when node
        (show-children)
        (bmk-mgr-print-single-node-at-point
         node (or path (bmk-mgr-get-path-at-point)))
        (beginning-of-line)
        (when (bmk-mgr-node-folder-p node)
          (if (bmk-mgr-node-open-p node)
              (bmk-mgr-refresh-open-close)
            (hide-subtree)))))))

(defun bmk-mgr-find-path-in-buffer (path &optional begin)
  (beginning-of-line)
  (let ((ip (point))
        (ppos)
        (found))
    (if begin (goto-char (point-min)))
    (while (not (or found (eobp)))
      (let* ((cp (bmk-mgr-get-path-at-point))
             (node (bmk-mgr-get-node-at-point))
             (isf (and node (bmk-mgr-node-folder-p node)))
             (isclf (and isf (bmk-mgr-node-closed-p node))))
        (cond
         ((equal path cp)
          (save-excursion
            (mapc (lambda (p)
                    (goto-char p)
                    (bmk-mgr-toggle-folder)
                    (bmk-mgr-unmark-current))
                  (reverse (if isclf (cons (point) ppos) ppos))))
          (setf found t))
         ((or (and isf (bmk-mgr-path-contains cp path))
              (and (not isf) (equal (bmk-mgr-path-parent cp)
                                    (bmk-mgr-path-parent path))))
          (if isclf (setf ppos (cons (point) ppos)))
          (forward-line 1))
         (t (let ((cl (bmk-mgr-outline-level)))
              (forward-line 1)
              (while (and (not (eobp))
                          (< cl (bmk-mgr-outline-level)))
                (forward-line 1)))))))
    (if (not found) (goto-char ip)
      (save-excursion (goto-char ip) (bmk-mgr-unmark-current)))
    (and found (point))))

(defun bmk-mgr-insert-sibling-at-point (node before)
  (let ((bmk-node (bmk-mgr-get-node-at-point))
        (bmk-path (bmk-mgr-get-path-at-point))
        (pos (point)))
    (save-excursion
      (if (and node
               (bmk-mgr-find-path-in-buffer (bmk-mgr-path-parent bmk-path) t))
          (let* ((path (list (bmk-mgr-path-leaf (bmk-mgr-get-path-at-point))
                             (bmk-mgr-path-leaf bmk-path)))
                 (newtree (bmk-mgr-insert-node (bmk-mgr-get-node-at-point)
                                               node path t before)))
            (if newtree
                (progn
                  (goto-char pos)
                  (when (not before)
                    (outline-next-visible-heading 1)
                    (if (eobp) (newline)))
                  (save-excursion
                    (bmk-mgr-print-tree node
                                        (bmk-mgr-path-parent bmk-path)))
                  (bmk-mgr-refresh-open-close))
              (error "Internal error")))
        (error "Path to node not found")))))


(defun bmk-mgr-insert-child-at-point (node before)
  (let ((bmk-node (bmk-mgr-get-node-at-point))
        (bmk-path (bmk-mgr-get-path-at-point)))
    (if (bmk-mgr-node-url-p bmk-node)
        (bmk-mgr-insert-sibling-at-point node before)
      (when node
        (let* ((path (list (bmk-mgr-path-leaf bmk-path)))
               (newtree (bmk-mgr-insert-node bmk-node node path nil before)))
          (if newtree
              (progn
                (if (bmk-mgr-node-closed-p bmk-node) (bmk-mgr-toggle-folder))
                (forward-line 1)
                (if (and (not before)
                         (> (length (bmk-mgr-node-children bmk-node)) 1))
                    (condition-case nil
                        (while (not (eobp)) (outline-forward-same-level 1))
                      (error (forward-line 1))))
                (save-excursion
                  (bmk-mgr-print-tree node bmk-path))
                (bmk-mgr-refresh-open-close))))))))

;;;; Bookmark tree datatype:

;;;;; paths:

(defsubst bmk-mgr-path-parent (path) (and (listp path) (subseq path 0 -1)))
(defsubst bmk-mgr-path-leaf (path) (and (listp path) (car (subseq path -1))))
(defsubst bmk-mgr-path-to-string (path)
  (mapconcat (lambda (x) (and (stringp x) x))
             (delete-if (lambda (x) (string= x "")) path) "/"))
(defsubst bmk-mgr-string-to-path (path)
  (delete-if (lambda (x) (string= x ""))
             (split-string path "/")))
(defsubst bmk-mgr-path-contains (parent child)
  (equal parent (subseq child 0 (length parent))))

;;;;; constructors:
(defsubst bmk-mgr-node-url-new (title url) (list title url))
(defsubst bmk-mgr-node-folder-new (name &optional closed children)
  (cons name (cons (if closed :closed :open) children)))

;;;;; accessors:
(defsubst bmk-mgr-node-children (n) (cddr n))
(defsubst bmk-mgr-node-name (n) (nth 0 n))
(defsubst bmk-mgr-node-folder-p (n) (and n (symbolp (nth 1 n))))
(defsubst bmk-mgr-node-open-p (n) (equal :open (nth 1 n)))
(defsubst bmk-mgr-node-closed-p (n) (equal :closed (nth 1 n)))
(defsubst bmk-mgr-node-url-p (n) (stringp (nth 1 n)))
(defsubst bmk-mgr-node-url (n) (and (stringp (nth 1 n)) (nth 1 n)))
(defsubst bmk-mgr-node-title (n) (nth 0 n))
(defsubst bmk-mgr-node-type (n) (if (bmk-mgr-node-url-p n) 'url 'folder))

(defun bmk-mgr-node-child-folders (node &optional path)
  (let ((node (or (and (null path) node)
                  (and node path (bmk-mgr-find-node node path)))))
    (when node
      (remove-if 'bmk-mgr-node-url-p
                 (bmk-mgr-node-children node)))))

(defun bmk-mgr-find-node (tree path)
  (let* ((node nil)
         (ffun (lambda (n p)
                 (if (equal (car p) (bmk-mgr-node-name n))
                     (if (null (cdr p))
                         (progn
                           (setq node n)
                           (cons nil nil))
                       (cons (cdr p) t))
                   (cons nil nil)))))
    (bmk-mgr-visit-tree tree ffun path)
    node))

(defun bmk-mgr-find-node-and-parent (tree path)
  (let* ((parent tree)
         (node nil)
         (fnode (lambda (n p)
                  (if (equal (car p) (bmk-mgr-node-name n))
                      (if (null (cdr p))
                          (progn (setq node n)
                                 (cons nil nil))
                        (progn (setq parent n)
                               (cons (cdr p) t)))
                    (cons nil nil)))))
    (bmk-mgr-visit-tree tree fnode path)
    (cons (and node parent) node)))

;;;;; modifiers:
(defsubst bmk-mgr-node-set-name (node name)
  (when (stringp name) (setf (car node) name)))

(defsubst bmk-mgr-node-set-url (node url)
  (when (and (bmk-mgr-node-url-p node) (stringp url)) (setf (nth 1 node) url)))

(defun bmk-mgr-node-toggle-open-closed (node)
  (when (bmk-mgr-node-folder-p node)
    (setf (nth 1 node) (if (bmk-mgr-node-closed-p node) :open :closed))))

(defsubst bmk-mgr-node-close (node)
  (when (bmk-mgr-node-folder-p node) (setf (nth 1 node) :closed)))

(defsubst bmk-mgr-node-close-all (tree)
  (when (bmk-mgr-node-folder-p tree)
    (bmk-mgr-node-close tree)
    (mapc #'bmk-mgr-node-close-all (bmk-mgr-node-children tree))))

(defsubst bmk-mgr-node-close-all-children (tree)
  (when (bmk-mgr-node-folder-p tree)
    (mapc #'bmk-mgr-node-close-all (bmk-mgr-node-children tree))))

(defun bmk-mgr-node-set-children (node children)
  (when (bmk-mgr-node-folder-p node)
    (setf (nthcdr 2 node) children)))

(defun bmk-mgr-node-swap-children (node c0 c1)
  "Swap the positions of C0 and C1, which are children of NODE.

If C0 is null, C1 gets promoted to the top of the children list.
Conversely, if C1 is null, C0 goes to the tail."
  (when (and (bmk-mgr-node-folder-p node) (or c0 c1))
    (let* ((children (bmk-mgr-node-children node))
           (p0 (position c0 children))
           (p1 (position c1 children)))
      (when (and children (or p0 p1))
        (if (and c1 p1) (setf (nth (or p0 2) children) c1))
        (if (and c0 p0)
            (setf (nth (or p1 (1- (length children))) children) c0))))))

(defun bmk-mgr-node-swap-children-at-path (tree path c0 c1)
  "Calls `bmk-mgr-node-swap-children' on the node of TREE denoted by
path."
  (when tree
    (let ((node (bmk-mgr-find-node tree path)))
      (if node (bmk-mgr-node-swap-children node c0 c1)))))

(defun bmk-mgr-node-add-child (tree node &optional prev before)
  "Add NODE as a new child of TREE, after (or before, if BEFORE is not
null) node PREV if it exists.

Returns the updated TREE if successful, nil otherwise."
  (when (bmk-mgr-node-folder-p tree)
    (let* ((pos (or (position prev tree) (if before 2)))
           (insp (if (not pos) (length tree) (if before pos (1+ pos)))))
      (setf (nthcdr insp tree) (cons node (nthcdr insp tree)))
      tree)))

(defun bmk-mgr-insert-node (tree node path &optional sibling before)
  "Insert the NODE at the given PATH of TREE.

If SIBLING is not null, the new node will be inserted as a sibling of
the one denoted by PATH. Otherwise, PATH is the path of NODE's parent.
If BEFORE is not null, NODE is inserter before or as the first child
denoted by path.

Returns the updated parent of NODE if successful, nil otherwise."
  (let* ((np (bmk-mgr-find-node-and-parent tree path))
         (parent (and np (car np)))
         (found (and np (cdr np))))
    (when found
      (if (or sibling (bmk-mgr-node-url-p found))
          (bmk-mgr-node-add-child parent node found before)
        (bmk-mgr-node-add-child found node nil before)))))

(defun bmk-mgr-delete-node (tree path)
  (let* ((np (bmk-mgr-find-node-and-parent tree path))
         (parent (and np (car np)))
         (found (and np (cdr np)))
         (children (and found (bmk-mgr-node-children parent))))
    (when children
      (bmk-mgr-node-set-children parent (remove found children)))))

;;;;; input/output:
(defun bmk-mgr-read-from-file (filename)
  (let ((rfname (expand-file-name filename)))
    (if (file-readable-p rfname)
        (with-temp-buffer
          (insert-file-contents rfname)
          (goto-char (point-min))
          (let ((sexp (read (current-buffer))))
            (and (bmk-mgr-node-folder-p sexp) sexp)))
      '("Bookmarks" :open
        ("Emacs bookmark manager"
         "http://www.emacswiki.org/cgi-bin/wiki/EmacsBmkMgr")))))

(defun bmk-mgr-save-to-file (bmks filename)
  (require 'pp)
  (when (bmk-mgr-node-folder-p bmks)
    (let ((rfname (expand-file-name filename))
          (b (if bmk-mgr-ignore-fold-state (subst :closed :open bmks) bmks)))
      (with-temp-buffer
        (insert ";;; File automatically generated by Emacs Bookmark Manager"
                "\n")
        (if bmk-mgr-ignore-fold-state (bmk-mgr-node-toggle-open-closed b))
        (pp b (current-buffer))
        (insert "\n;;; End of " (file-name-nondirectory rfname) "\n")
        (write-region (point-min) (point-max) rfname)))))


;;;;; aux functions:
(defun bmk-mgr-visit-tree (tree fun arg)
  "Visit a bookmarks tree aplying FUN to its nodes."
  (when tree
    (let ((arg (funcall fun tree arg)))
      (when (cdr arg)
        (mapc (lambda (n) (bmk-mgr-visit-tree n fun (car arg)))
              (bmk-mgr-node-children tree))))))



(provide 'bmk-mgr)





;;; Local stuff:
;;;; Local Variables: ;;
;;;; mode: emacs-lisp ;;
;;;; mode: outline-minor ;;
;;;; outline-regexp: ";;[;\f]+ " ;;
;;;; outline-heading-end-regexp: ":\n" ;;
;;;; indent-tabs-mode: nil ;;
;;;; End: ;;

;;; bmk-mgr.el ends here
