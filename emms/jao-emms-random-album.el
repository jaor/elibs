;; jao-emms-random-album.el -- play random albums in emms

;; Copyright (C) 2009, 2010, 2017, 2018 Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sat Jul 04, 2009 13:06

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


(require 'emms)

(defvar jao-emms-random-album-p t)
(defvar jao-emms-random-lines nil)
(defvar jao-emms-random-lines-file
  (expand-file-name "~/.emacs.d/random-lines"))
(defvar jao-emms-random-album-notify-p t)
(defvar jao-emms-random-album-notify-icon nil)

(defun jao-emms-random-lines ()
  (or jao-emms-random-lines
      (and (file-exists-p jao-emms-random-lines-file)
           (with-current-buffer
               (find-file-noselect jao-emms-random-lines-file)
             (goto-char (point-min))
             (setq jao-emms-random-lines (read (current-buffer)))))
      (dotimes (n (1- (line-number-at-pos (point-max)))
                  jao-emms-random-lines)
        (push (1+ n) jao-emms-random-lines))))

(defun jao-emms-random-lines-save ()
  (with-current-buffer (find-file-noselect jao-emms-random-lines-file)
    (delete-region (point-min) (point-max))
    (insert (format "%s\n" jao-emms-random-lines))
    (save-buffer)))

(defun jao-emms-goto-random-album ()
  (let* ((pos (random (length (jao-emms-random-lines))))
         (line (nth pos jao-emms-random-lines)))
    (setq jao-emms-random-lines (remove line jao-emms-random-lines))
    (jao-emms-random-lines-save)
    (goto-line line)))

(defun jao-emms-next-noerror ()
  (interactive)
  (when emms-player-playing-p
    (error "A track is already being played"))
  (cond (emms-repeat-track
         (emms-start))
        ((condition-case nil
             (progn
               (emms-playlist-current-select-next)
               t)
           (error nil))
         (emms-start))
        (t
         (if jao-emms-random-album-p
             (jao-emms-random-album-next)
           (message "No next track in playlist")))))


;; User interface
(defun jao-emms-random-album-start ()
  (interactive)
  (setq jao-emms-random-album-p t)
  (jao-emms-random-album-next))

(defun jao-emms-random-album-stop ()
  (interactive)
  (setq jao-emms-random-album-p nil)
  (emms-stop))

(defun jao-emms-random-album-toggle ()
  (interactive)
  (setq jao-emms-random-album-p (not jao-emms-random-album-p))
  (message "Random album %s"
           (if jao-emms-random-album-p "enabled" "disabled")))

(defun jao-emms-random-album-next ()
  (interactive)
  (let ((buffer (emms-browser-get-buffer)))
    (save-excursion
      (if buffer (set-buffer buffer) (emms-browser))
      (ignore-errors (emms-browser-clear-playlist))
      (emms-browse-by-album)
      (jao-emms-goto-random-album)
      (let ((album (substring-no-properties (thing-at-point 'line) 0 -1)))
        (emms-browser-add-tracks-and-play)
        (when jao-emms-random-album-notify-p
          (jao-notify album "Next album" jao-emms-random-album-notify-icon)))
      (emms-browser-bury-buffer))))

(defun jao-emms-random-album-reset ()
  (interactive)
  (setq jao-emms-random-lines nil)
  (jao-emms-random-lines-save))

(defun jao-emms-random-album-setup ()
  (setq emms-player-next-function 'jao-emms-next-noerror))


(provide 'jao-emms-random-album)
;;; jao-emms-random-album.el ends here
