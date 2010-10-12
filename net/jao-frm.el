;;; jao-frm.el --- use frm to show mailbox

;; Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009, 2010

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Keywords: mail

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

;;  Little hack to see the contents of your mailbox using GNU mailutils'
;;  `frm' program.
;;
;;  Just put (require 'jao-frm) in your .emacs, and M-x jao-frm will pop up a
;;  new window with your mailbox contents (from and subject) as
;;  printed by frm. In this buffer, use `n' and `p' to move, `q' to close
;;  the window. `g' will call Gnus.
;;

;;; Code:

;;;; Customisation:

(defgroup jao-frm nil
  "Frm-base mailbox checker"
  :prefix "jao-frm-")

(defcustom jao-frm-exec-path "frm"
  "frm executable path"
  :group 'jao-frm
  :type 'file)

(defcustom jao-frm-mail-command 'gnus
  "Emacs function to invoke when `g' is pressed on an frm buffer."
  :group 'jao-frm
  :type 'symbol)

(defcustom jao-frm-mailboxes nil
  "List of mailboxes to check, or directory containing them."
  :group 'jao-frm
  :type '(choice directory (repeat file)))

(defface jao-frm-mailno-face '((t (:foreground "dark slate grey")))
  "Face for the mail number."
  :group 'jao-frm)

(defface jao-frm-from-face '((t (:foreground "slate grey")))
  "Face for From: header."
  :group 'jao-frm)

(defface jao-frm-subject-face '((t (:foreground "slate blue")))
  "Face for Subject: header."
  :group 'jao-frm)

(defface jao-frm-mailbox-face '((t (:bold t :weight bold)))
  "Face for mailbox name."
  :group 'jao-frm)

;;;; Mode:

(defvar jao-frm-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map [?q] 'jao-frm-delete-window)
    (define-key map [?n] 'next-line)
    (define-key map [?p] 'previous-line)
    (define-key map [?g] jao-frm-mail-command)
    (define-key map [(control k)] 'jao-frm-delete-message)
    map))

(setq jao-frm-font-lock-keywords
 '(("^[^ :]+:" . 'jao-frm-mailbox-face)
   ("^\\([ 0-9]+\\):\t+\\([^\t]+\\)\t+\\([^\n]+$\\)"
    (1 'jao-frm-mailno-face)
    (2 'jao-frm-from-face)
    (3 'jao-frm-subject-face))))

(defvar jao-frm-mode-syntax-table
  (let ((st (make-syntax-table)))
    st))

(defun jao-frm-mode ()
  "Major mode for displaying frm output."
  (interactive)
  (kill-all-local-variables)
  (buffer-disable-undo)
  (use-local-map jao-frm-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       '(jao-frm-font-lock-keywords))
  (set (make-local-variable 'truncate-lines) t)
  (set (make-local-variable 'kill-whole-line) t)
  (set (make-local-variable 'next-line-add-newlines) nil)
  (setq major-mode 'jao-frm-mode)
  (setq mode-name "frm")
  (toggle-read-only 1)
  (goto-char 1))

;;;; Mode commands:
(defvar jao-frm-last-config nil)

(defun jao-frm-delete-window ()
  "Delete frm window and restore last win config"
  (interactive)
  (if (and (consp jao-frm-last-config)
           (window-configuration-p (car jao-frm-last-config)))
      (progn
        (set-window-configuration (car jao-frm-last-config))
        (goto-char (cadr jao-frm-last-config))
        (setq jao-frm-last-config nil))
    (bury-buffer)))

(defun jao-frm-delete-message ()
  "Delete message at point"
  (interactive)
  (when (eq (current-buffer) (get-buffer "*frm*"))
    (beginning-of-line)
    (when (search-forward-regexp "^ +\\([0-9]+\\):" nil t)
      (let ((mn (string-to-number (match-string 1))))
        (when (y-or-n-p (format "Delete message number %d? " mn))
          (toggle-read-only -1)
          (shell-command (format "echo 'd %d'|mail" mn) t)
          (jao-frm)
          (when (= (point-max) (point-min))
            (jao-frm-delete-window)
            (message "Mailbox is empty")))))))

;;;; Activate frm:
(defun jao-frm-mbox-mails (mbox)
  (let ((no (ignore-errors
              (substring
               (shell-command-to-string (format "frm %s|wc -l" mbox)) 0 -1))))
    (if (stringp no) (string-to-number no) 0)))

(defun jao-frm-mail-number ()
  (let ((no 0))
    (dolist (b (jao-frm-mboxes) no) (setq no (+ no (jao-frm-mbox-mails b))))))

(defun jao-frm-default-count-formatter (m n)
  (format "%s: %s" (file-name-sans-extension (file-name-nondirectory m)) n))

(defun jao-frm-mail-counts (fmt)
  (let ((fmt (or fmt 'jao-frm-default-count-formatter)))
    (remove nil
            (mapcar (lambda (m)
                      (let ((n (jao-frm-mbox-mails m)))
                        (unless (zerop n) (funcall fmt m n))))
                    (jao-frm-mboxes)))))

(defun jao-frm-display-mailbox (mbox)
  (when (not (zerop (jao-frm-mbox-mails mbox)))
    (insert (or (file-name-nondirectory mbox) mbox) ":\n\n")
    (apply 'call-process
           `(,jao-frm-exec-path nil ,(current-buffer) nil
                                "-n" "-t" ,@(and mbox (list mbox))))
    (newline 2)))

(defun jao-frm-mboxes ()
  (cond ((null jao-frm-mailboxes) (list (getenv "MAIL")))
        ((listp jao-frm-mailboxes) jao-frm-mailboxes)
        ((stringp jao-frm-mailboxes)
         (if (file-directory-p jao-frm-mailboxes)
             (directory-files jao-frm-mailboxes t "^[^.]")
           (list jao-frm-mailboxes)))
        (t (error "Error in mbox specification. Check `jao-frm-mailboxes'"))))

(defun jao-frm ()
  "Run frm."
  (interactive)
  (let ((fbuff (get-buffer-create "*frm*"))
        (inhibit-read-only t))
    (if (not (eq fbuff (current-buffer)))
        (setq jao-frm-last-config
              (list (current-window-configuration) (point-marker))))
    (with-current-buffer fbuff
      (delete-region (point-min) (point-max))
      (mapc 'jao-frm-display-mailbox (jao-frm-mboxes))
      (unless (eq major-mode 'jao-frm-mode)
        (jao-frm-mode))
      (goto-char (point-min))
      (if (= (point-min) (point-max))
          (message "Mailbox is empty.")
        (pop-to-buffer fbuff))
      (when (and (boundp 'display-time-mode) display-time-mode)
        (display-time-update)))))

(defun jao-frm-show-mail-numbers ()
  (interactive)
  (let ((counts (jao-frm-mail-counts nil)))
    (message (if counts (mapconcat 'identity counts ", ") "No mail"))))

(defun jao-frm-mail-string ()
  (let ((counts (jao-frm-mail-counts
                 (lambda (m n)
                   (let ((m (substring (file-name-nondirectory m) 0 1)))
                     (format "%s %s" m n))))))
    (mapconcat 'identity counts " ")))

(provide 'jao-frm)

;;; jao-frm.el ends here
