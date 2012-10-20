;;; w3m-session.el --- Persistent emacs-w3m sessions

;; Copyright (C) 2003, 2004, 2006, 2007, 2008, 2009, 2012  Jose A Ortega Ruiz

;; Author: Jose A Ortega Ruiz <jao@member.fsf.org>
;; Version: 0.3.6
;; Keywords: hypermedia, w3m, WWW

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

;; INTRODUCTION:
;;
;; jao-w3m-session provides persistent emacs-w3m browsing sessions. When
;; quitting w3m (or, if you request it, at any other time while using
;; it) you can save the current w3m session (that is, the set of open
;; tabs and the URLs they're visiting). Upon restarting emacs-w3m
;; (possibly after restarting Emacs itself) you'll have the possibity
;; of recovering the saved session (that is, of re-opening the saved
;; tabs and URLs). You also have at your disposal a command to recover
;; the saved session at any other time.
;;
;; INSTALLATION:
;;
;; Just put this file somewhere on your Emacs load path and add the
;; following line to your .emacs file:
;;
;;   (require 'jao-w3m-session)
;;
;; After restarting Emacs (or evaluating the form above), each time
;; you start emacs-w3m with 'w3m' you'll get a prompt asking whether
;; your last browsing session should be loaded. Likewise, when
;; quitting the browser, you'll have the possibility of saving your
;; current session (overwriting the previous one).
;;
;; In addition, two new interactive functions are defined:
;;
;;   jao-w3m-session-load  --  load the last stored session
;;   jao-w3m-session-save  --  save the current session
;;
;; These functions can be invoked at any time while running emacs-w3m.
;; Optionally, you can bind them to key shortcuts with the proper
;; variations of the following elisp magic in your .emacs:
;;  (defun w3m-add-keys ()
;;    (define-key w3m-mode-map "S" 'jao-w3m-session-save)
;;    (define-key w3m-mode-map "L" 'jao-w3m-session-load))
;;  (add-hook 'w3m-mode-hook 'w3m-add-keys)
;;
;; CUSTOMIZATION:
;;
;; A new customization group, jao-w3m-session, is available. There you can
;; customize the following variables:
;;
;;   jao-w3m-session-load-always     -- if t, `jao-w3m-session-load' will *not* ask
;;                                for confirmation (default nil)
;;   jao-w3m-session-save-always     -- if t, `jao-w3m-session-save' will *not* ask
;;                                for confirmation (default nil)
;;   jao-w3m-session-show-titles     -- if t, the load prompt will list the
;;                                session URL titles (default t)
;;   jao-w3m-session-duplicate-tabs  -- what to do when loading a session that
;;                                contains a URL already open
;;   jao-w3m-session-file            -- the file where w3m session info
;;                                is stored (default "~/.jao-w3m-session")
;;   jao-w3m-session-autosave-period -- the period, in seconds, for automatic
;;                                session backup file updating.
;;
;;
;; You can also customize them in your .emacs file, to wit:
;;
;;   (setq jao-w3m-session-file "~/.emacs.d/jao-w3m-session")
;;   (setq jao-w3m-session-save-always nil)
;;   (setq jao-w3m-session-load-always nil)
;;   (setq jao-w3m-session-show-titles t)
;;   (setq jao-w3m-session-duplicate-tabs 'ask) ;  'never, 'always, 'ask
;;
;; HISTORY:
;;
;;   Version 0.3.7 :
;;
;;     - `jao-w3m-session-deactivate-builtin-sessions', to do what it
;;       says.
;;
;;   Version 0.3.6 (Sat Apr 19, 2008):
;;
;;     - w3m-session -> jao-w3m-session to avoid collisions with
;;       emacs-w3m's session manager.
;;
;;   Version 0.3.5 (Sun Jan 14, 2007):
;;
;;     - automatic session backup every `jao-w3m-session-autosave-period'
;;       seconds.
;;
;;   Version 0.3.4 (Wed Jul 19, 2006):
;;
;;     - save session file on quitting Emacs (without using
;;       desktop.el)
;;
;;   Version 0.3.3 (Thu Jun  8, 2006):
;;
;;     - save session file with pretty print.
;;     - handle correctly multiple emacs-w3m (re)starts during a
;;       single emacs session.
;;     - save URLs in hexified form to allow & in them.
;;     - code cleanup.
;;
;;   Version 0.3.2 (Mon Sep 29, 2003):
;;
;;     - bug fix: when searching or going to home/bookmarks/etc,
;;       keep the current tab's focus.
;;
;;   Version 0.3.1 (Tue Aug 26, 2003):
;;
;;     - type of `jao-w3m-session-file' set to 'file' in customisation
;;       buffer.
;;     - bug fix: syntax error due to a typo in `jao-w3m-session-file'
;;
;;   Version 0.3 (Mon Aug 25, 2003):
;;
;;     - the load session tab lists the titles of the session's pages
;;       (customizable via 'jao-w3m-session-show-titles').
;;     - the duplicated tab prompt displays also the URL's title.
;;     - bug fix: active tab in session now is correctly saved.
;;
;;   Version 0.2 (Fri Aug 22, 2003):
;;
;;     - the session info now includes the active tab, which gets
;;       displayed when the session is reloaded.
;;     - when reloading a session in a running emacs-w3m, if the
;;       session contains a URL that is already being displayed by the
;;       browser, the tab can be reused or duplicated (customizable
;;       via `jao-w3m-session-duplicate-tabs').
;;
;;   Version 0.1 (Wed Aug 20, 2003) -- Initial release.
;;


;;; Code:

;;; Dependencies:

(require 'w3m)
(require 'advice)
(require 'url-util)

;;; Custom variables:

(defgroup jao-w3m-session nil
  "w3m - session saving in w3m."
  :group 'w3m
  :prefix "jao-w3m-session-")

(defcustom jao-w3m-session-save-always nil
  "If on, always save w3m session without asking."
  :group 'jao-w3m-session
  :type 'boolean)

(defcustom jao-w3m-session-load-always nil
  "If on, always load w3m session without asking."
  :group 'jao-w3m-session
  :type 'boolean)

(defcustom jao-w3m-session-show-titles t
  "If on, show URL titles in the load prompt."
  :group 'jao-w3m-session
  :type 'boolean)

(defcustom jao-w3m-session-duplicate-tabs 'never
  "How to treat session URL already being visited.

When loading a session with `jao-w3m-session-load', if one of the URLs in
the session is already displayed in a w3m tab, jao-w3m-session can:
- `never' create a new tab (just reload it), or
- `always' duplicate the URL in a new tab, or
- `ask' the user what to do."
  :group 'jao-w3m-session
  :type  '(choice (const :value never)
                  (const :value always)
                  (const :value ask)))

(defcustom jao-w3m-session-file "~/.jao-w3m-session"
  "File to save the w3m session data."
  :group 'jao-w3m-session
  :type 'file)

(defvar jao-w3m-session-autosave-period 180
  "A backup of the current session is saved with this period (in secs).")

(defvar jao-w3m-url-filters nil "URL filters.")

;;; Interactive functions:

(defun jao-w3m-session-save ()
  "Save the current w3m session."
  (interactive)
  (when (and (w3m-alive-p)
             (or jao-w3m-session-save-always
                 (y-or-n-p "Save current w3m session? ")))
    (jao-w3m-session-current-to-file)
    (jao-w3m-session--restart--autosave)))

(defun jao-w3m-session-load ()
  "Load last stored session into w3m."
  (interactive)
  (let ((s (jao-w3m-session-load-aux)))
    (when s
      (jao-w3m-session--restart--autosave)
      (let* ((urls (jao-w3m-session-url s))
             (offset (jao-w3m-session-offset s))
             (buffers (unless (equal jao-w3m-session-duplicate-tabs 'always)
                        (jao-w3m-session-find-duplicated urls))))
        (w3m-goto-url-new-session urls t)
        (when buffers (jao-w3m-session-close-buffers buffers))
        (unless (zerop offset) (w3m-next-buffer offset))))))

(defun jao-w3m-session-set-autosave-period (secs)
  "Set new value for the period between session backup autosaves."
  (interactive "p")
  (let ((secs (or secs (read-number "New period (secs): " 0))))
    (when (> secs 0)
      (setq jao-w3m-session-autosave-period secs)
      (jao-w3m-session--restart--autosave))))

(defun jao-w3m-session-deactivate-builtin-sessions ()
  "Deactivate emacs-w3m's builtin session handling."
  (setq w3m-session-deleted-save nil
        w3m-session-autosave nil
        w3m-session-deleted-keep-number 0
        w3m-session-crash-recovery nil))

;;; Internals:

;;;; advice w3m to use session management

(defadvice w3m (before jao-load-session activate)
  "Optionally load last w3m session on startup."
  (interactive
   (let ((s (jao-w3m-session-load-aux)))
     (list (or (and s (jao-w3m-session-url s)) w3m-home-page) t t))))

(defadvice w3m (after jao-select-tab activate)
  "Goto the saved focused tab"
  (interactive)
  (let ((offset (jao-w3m-session-offset)))
    (unless (zerop offset)
      (w3m-next-buffer offset))
    (ad-deactivate 'w3m)))

(defadvice w3m-quit (before jao-save-session activate)
  "Save session before quitting."
  (interactive)
  (jao-w3m-session-save)
  ;; this is a little hack: when quitting a w3m session with a tab
  ;; selected other than the first, the frame is not automatically
  ;; closed as should be when w3m-pop-up-frames is t:
  (switch-to-buffer (car (w3m-list-buffers)))
  (ad-activate 'w3m))

;;;; save session on exit
(add-to-list 'kill-emacs-query-functions
             (lambda () (jao-w3m-session-save) t))


;;;; auxiliary functions

(defvar jao-w3m-current-session '(jao-w3m-session 0 nil))

(defun jao-w3m-session--filter (url filters)
  (cond ((not filters) url)
        ((string-match-p (caar filters) url)
         (cond ((functionp (cdar filters)) (funcall (cadr filters) url))
               ((stringp (cdar filters)) (cdar filters))))
        (t (jao-w3m-session--filter url (cdr filters)))))

(defun jao-w3m-session--current-urls ()
  (let ((urls)
        (current-buffer (w3m-alive-p))
        (pos 0)
        (count 0))
    (dolist (b (w3m-list-buffers) (list pos (reverse urls)))
      (set-buffer b)
      (let ((url (jao-w3m-session--filter w3m-current-url jao-w3m-url-filters)))
        (when url
          (when (eq b current-buffer) (setq pos count))
          (setq count (1+ count))
          (push (cons (url-hexify-string url) (w3m-buffer-title b)) urls))))))

(defun jao-w3m-session-url (&optional s)
  (let ((s (or s jao-w3m-current-session)))
    (concat "group:"
            (mapconcat 'car (nth 2 s) "&"))))

(defun jao-w3m-session-offset (&optional s)
  (let ((s (or s jao-w3m-current-session)))
    (nth 1 s)))

(defun jao-w3m-session-titles (&optional s)
  (let ((s (or s jao-w3m-current-session)))
    (mapcar 'cdr (nth 2 s))))

(defun jao-w3m-session-current (&optional s)
  (save-current-buffer
    (setq jao-w3m-current-session
          (or s (cons 'jao-w3m-session (jao-w3m-session--current-urls))))))

(defun jao-w3m-session-current-url ()
  (when (w3m-alive-p)
    (save-current-buffer
      (concat "group:"
              (mapconcat (lambda  (b) (set-buffer b) w3m-current-url)
                         (w3m-list-buffers) "&")))))

(defun jao-w3m-session-find-duplicated (urls)
  (when (w3m-alive-p)
    (save-current-buffer
      (let* ((duplicate-p
              (lambda (b)
                (y-or-n-p
                 (format "'%s' (%s) is already open. Duplicate tab? "
                         (w3m-buffer-title b) w3m-current-url))))
             (test-b
              (lambda (b)
                (set-buffer b)
                (if (and
                     (string-match (regexp-quote w3m-current-url) urls)
                     (or (equal jao-w3m-session-duplicate-tabs 'never)
                         (not (funcall duplicate-p b))))
                    b 'not)))
             (buffers (mapcar test-b (w3m-list-buffers))))
        (delete 'not buffers)))))

(defun jao-w3m-session-close-buffers (buffers)
  (save-current-buffer
    (mapc 'kill-buffer buffers)))

(defun jao-w3m-session-load-aux ()
  (let ((new-session (jao-w3m-session-from-file
                      (expand-file-name jao-w3m-session-file))))
    (if (and new-session
             (or jao-w3m-session-load-always
                 (y-or-n-p
                  (if jao-w3m-session-show-titles
                      (format "Load last w3m session %S? "
                              (jao-w3m-session-titles new-session))
                    "Load last w3m session? "))))
        (jao-w3m-session-current new-session)
      nil)))

(defun jao-w3m-session-from-file (fname)
  (let ((fname (jao-w3m-session--check--backup fname)))
    (if (file-readable-p fname)
        (with-temp-buffer
          (insert-file-contents fname)
          (goto-char (point-min))
          (let ((sexp (read (current-buffer))))
            (and (equal 'jao-w3m-session (car sexp)) sexp)))
      nil)))

(defsubst jao-w3m-session-current-to-file ()
  (jao-w3m-session--to--file jao-w3m-session-file))

(defun jao-w3m-session--to--file (filename &optional is-auto)
  (require 'pp)
  (let ((msg (if is-auto (current-message))))
    (with-temp-buffer
      (insert ";;;; File generated by jao-w3m-session. DO NOT EDIT!\n")
      (pp (jao-w3m-session-current) (current-buffer))
      (insert "\n" ";;;; End of "
              (file-name-nondirectory jao-w3m-session-file) "\n")
      (write-region (point-min) (point-max) (expand-file-name filename)))
    (if is-auto (message msg))))

(defvar jao-w3m-session--timer nil)

(defun jao-w3m-session--backup-name (fname)
  (concat (expand-file-name fname) ".bak"))

(defun jao-w3m-session--check--backup (fname)
  (let ((bfname (jao-w3m-session--backup-name fname)))
    (if (and (file-newer-than-file-p bfname fname)
             (y-or-n-p "A newer autosaved session exists. Use it? "))
        bfname
      fname)))

(defun jao-w3m-session--restart--autosave ()
  (when (> jao-w3m-session-autosave-period 0)
    (if jao-w3m-session--timer (cancel-timer jao-w3m-session--timer))
    (setq jao-w3m-session--timer
          (run-at-time jao-w3m-session-autosave-period
                       jao-w3m-session-autosave-period
                       'jao-w3m-session--to--file
                       (jao-w3m-session--backup-name jao-w3m-session-file)
                       t))))

(provide 'jao-w3m-session)

;;; jao-w3m-session.el ends here
