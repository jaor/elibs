;; jao-emms-lyrics.el -- simple show lyrics in emms

;; Copyright (C) 2009, 2010, 2017 Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sat Jul 04, 2009 13:41

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'emms)

(defvar jao-emms-lyrics-cache-dir "~/.emacs.d/emms-lyrics")

(defun jao-emms-lyrics--filename (artist title)
  (expand-file-name (format "%s.lyr" title)
                    (jao-emms-lyrics--ensure-dir artist)))

(defun jao-emms-lyrics--ensure-dir (artist)
  (let ((candidate (expand-file-name artist jao-emms-lyrics-cache-dir)))
    (unless (file-directory-p candidate)
      (make-directory candidate t))
    candidate))

(defun jao-emms-lyrics--get-cached (artist title)
  (let ((candidate (jao-emms-lyrics--filename artist title)))
    (and (file-exists-p candidate)
         (with-current-buffer (find-file-noselect candidate)
           (prog1
               (buffer-string)
             (kill-buffer))))))

(defun jao-emms-lyrics--cache (artist title lyrics)
  (with-current-buffer
      (find-file-noselect (jao-emms-lyrics--filename artist title))
    (delete-region (point-min) (point-max))
    (insert lyrics)
    (save-buffer)
    (kill-buffer)))

(make-variable-buffer-local
 (defvar jao-emms-lyrics--path nil))

(defvar jao-emms-lyrics-mode-map)
(setq jao-emms-lyrics-mode-map
      (let ((map (make-keymap)))
        (suppress-keymap map)
        (define-key map [?q] 'bury-buffer)
        (define-key map [?g] 'jao-emms-show-lyrics)
        (define-key map [?G] 'jao-emms-show-lyrics-force)
        (define-key map [?e] 'jao-emms-edit-lyrics)
        map))

(defvar jao-emms-font-lock-artist 'bold)
(defvar jao-emms-font-lock-title 'bold)

(defun jao-emms-lyrics-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map jao-emms-lyrics-mode-map)
  (setq major-mode 'jao-emms-lyrics-mode)
  (setq mode-name "lyrics")
  (toggle-read-only 1))

(defun jao-emms-lyrics-buffer ()
  (or (get-buffer "*Emms Lyrics*")
      (with-current-buffer (get-buffer-create "*Emms Lyrics*")
        (jao-emms-lyrics-mode)
        (current-buffer))))

(defun jao-emms-lyrics-track-data ()
  (let ((track (or (emms-playlist-current-selected-track)
                   (error "No playing track"))))
    (cons (or (emms-track-get track 'info-artist nil)
              (error "No artist"))
          (or (emms-track-get track 'info-title nil)
              (error "No artist")))))

(defun jao-emms-edit-lyrics ()
  (interactive)
  (unless jao-emms-lyrics--path
    (error "No track data available."))
  (find-file-other-window jao-emms-lyrics--path))



(defun jao-emms-lyrics--download (artist title)
  (message "Retrieving lyrics...")
  (let ((fn (jao-emms-lyrics--filename artist title)))
    (shell-command-to-string (format "glyrc lyrics -n 1-8 -Y -a %s -t %s -w %s"
                                     (shell-quote-argument artist)
                                     (shell-quote-argument title)
                                     (shell-quote-argument fn)))
    (prog1 (jao-emms-lyrics--get-cached artist title) (message nil))))

(defun jao-emms-show-lyrics (&optional force)
  (interactive "P")
  (let* ((a/t (jao-emms-lyrics-track-data))
         (artist (or (car a/t) ""))
         (title (or (cdr a/t) ""))
         (buffer (jao-emms-lyrics-buffer))
         (cached (and (not force) (jao-emms-lyrics--get-cached artist title)))
         (cached (and (not (zerop (length cached))) cached))
         (lyrics (or cached (jao-emms-lyrics--download artist title)))
         (inhibit-read-only t))
    (with-current-buffer buffer
      (delete-region (point-min) (point-max))
      (insert (format "♪ %s - %s\n\n"
                      (propertize artist 'face jao-emms-font-lock-artist)
                      (propertize title 'face jao-emms-font-lock-title)))
      (when lyrics (insert lyrics))
      (goto-char (point-min))
      (setq jao-emms-lyrics--path (jao-emms-lyrics--filename artist title)))
    (pop-to-buffer buffer)))

(defun jao-emms-show-lyrics-force ()
  (interactive)
  (jao-emms-show-lyrics t))


(provide 'jao-emms-lyrics)
;;; jao-emms-lyrics.el ends here
