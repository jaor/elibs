;; jao-emms-info-track.el -- utilities to show tracks

;; Copyright (C) 2009, 2010 Jose Antonio Ortega Ruiz

;; Author: Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sat Jul 04, 2009 13:47

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
(require 'jao-osd)
(require 'jao-emms)

(defgroup jao-emms-faces nil "Faces"
  :group 'faces
  :group 'jao-emms)

(defface jao-emms-font-lock-album '((t (:foreground "lightgoldenrod2")))
  "Album name in EMMS track message."
  :group 'jao-emms-faces)

(defface jao-emms-font-lock-track '((t (:bold t)))
  "Track number in EMMS track message."
  :group 'jao-emms-faces)

(defface jao-emms-font-lock-title '((t (:foreground "dodgerblue2")))
  "Track title in EMMS track message."
  :group 'jao-emms-faces)

(defface jao-emms-font-lock-artist '((t (:foreground "dodgerblue3")))
  "Artist name in EMMS track message."
  :group 'jao-emms-faces)

(defcustom jao-emms-show-osd-p nil
  "Whether to show osd notices on track change"
  :group 'jao-emms)



(defun jao-emms-info-track-stream (track)
  "Return track info for streams"
  (let ((name (emms-track-name track))
        (title (or (emms-track-get track 'title nil)
                   (car (emms-track-get track 'metadata nil)))))
    (format "♪ %s (%s)" title (if title (emms-track-type track) name))))

(defsubst jao--put-face (str face)
  (put-text-property 0 (length str) 'face face str)
  str)

(defun jao-emms-info-track-file (track)
  "Return a description of the current track."
  (let ((no (string-to-number (emms-track-get track 'info-tracknumber "0")))
        (time (emms-track-get track 'info-playing-time))
        (artist (emms-track-get track 'info-artist ""))
        (composer (emms-track-get track 'info-composer nil))
        (title (emms-track-get track 'info-title ""))
        (album (emms-track-get track 'info-album))
        (last-played (or (emms-track-get track 'last-played) '(0 0 0)))
        (play-count (or (emms-track-get track 'play-count) 0)))
    (if (or (not title) (not album))
        (emms-track-simple-description track)
      (format "♪ %s%s%s%s%s %s"
              (if time (format "[%02d:%02d] " (/ time 60) (mod time 60)) "")
              (jao--put-face artist 'jao-emms-font-lock-artist)
              (jao--put-face (if composer (format " [%s]" composer) "")
                             'jao-emms-font-lock-artist)
              (jao--put-face (if album (format " (%s)" album) " *")
                             'jao-emms-font-lock-album)
              (jao--put-face (if (zerop no) "" (format " %02d." no))
                             'jao-emms-font-lock-track)
              (jao--put-face title
                             'jao-emms-font-lock-title)))))

(defun jao-emms-info-track-description (track)
  (if (memq (emms-track-type track) '(streamlist url))
      (jao-emms-info-track-stream track)
    (jao-emms-info-track-file track)))

(defun jao-emms-toggle-osd ()
  (interactive)
  (setq jao-emms-show-osd-p (not jao-emms-show-osd-p))
  (message "Emms OSD %s" (if jao-emms-show-osd-p "enabled" "disabled")))

(defsubst jao-emms-current-track-str ()
  (substring-no-properties (jao-emms-info-track-description
                            (emms-playlist-current-selected-track))))

(defun jao-emms-show-osd ()
  (interactive)
  (let ((str (jao-emms-current-track-str)))
    (when str (jao-osd-cat 'emms (substring str 2)))
    t))

(defun jao-emms-show-osd-hook ()
  (interactive)
  (when jao-emms-show-osd-p (jao-emms-show-osd))
  t)

(defun jao-emms-info-setup (&optional show-osd show-echo-line)
  (setq emms-track-description-function 'jao-emms-info-track-description)
  (setq jao-emms-show-osd-p show-osd)
  (add-hook 'emms-player-started-hook 'jao-emms-show-osd-hook)
  (unless show-echo-line
    (eval-after-load 'emms-player-mpd
      '(remove-hook 'emms-player-started-hook 'emms-player-mpd-show))))


(provide 'jao-emms-info-track)
;;; jao-emms-info-track.el ends here
