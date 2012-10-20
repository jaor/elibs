;;  Based on code by Thierry Volpiatto
;;  (http://mercurial.intuxication.org/hg/xml-weather)

(require 'xml)
(require 'derived)


;;; config:
(defvar jao-weather-format-id-url
  "http://xoap.weather.com/search/search?where=%s")

(defvar jao-weather-format-xml-from-id-url ; id, unit=m,day-forecast=5,login,key
  "http://xoap.weather.com/weather/local/%s?cc=*&unit=%s&dayf=%s&prod=xoap&par=%s&key=%s")

(defvar jao-weather-unit "m"
  "*m mean metric, you will have wind speed in km/h, temperature in °C and so on.")

(defvar jao-weather-login nil)
(defvar jao-weather-key nil)

(defvar jao-weather-day-forecast-num 5
  "*Number of days for forecast; Maximum 5.")

(defvar jao-weather-default-id "SPXX0015")

(defvar jao-weather-timer-delay 3600)

(defvar jao-weather-last-data nil)


;;; access:
(defun jao-weather-authentify ()
  "Authentify user from .authinfo file.
You have to setup correctly `auth-sources' to make this function
finding the path of your .authinfo file that is normally ~/.authinfo.
Entry in .authinfo should be:
machine xoap.weather.com port http login xxxxx password xxxxxx"
  (let ((auth (auth-source-user-or-password  '("login" "password")
                                             "xoap.weather.com"
                                             "http")))
    (setq jao-weather-login (car auth)
          jao-weather-key (cadr auth))))

(defun jao-weather--url (id)
  (unless (and jao-weather-login jao-weather-key)
    (jao-weather-authentify))
  (format jao-weather-format-xml-from-id-url
          (or id jao-weather-default-id)
          jao-weather-unit
          (min jao-weather-day-forecast-num 5)
          jao-weather-login
          jao-weather-key))

(defvar jao-weather-hook nil)

;; http://xoap.weather.com/weather/local/[locid]
;; Replace the [locid], of course, with the location ID obtained in the previous step.
;; Appended to this URL is a mix of other parameters,
;; some required and some optional. A typical example might be:
;; http://xoap.weather.com/weather/local/NLXX0002?cc=*&dayf=5&prod=xoap&par=[partner id]&key=[license key]
(defun jao-weather--get-info-async (&optional id)
  (let ((url (jao-weather--url id))
        (url-show-status nil))
    (url-retrieve url (lambda (res)
                        (when (not res)
                          (let ((data (jao-weather-get-alist)))
                            (when data
                              (setq jao-weather-last-data data)
                              (run-hooks 'jao-weather-hook))))
                        (kill-buffer (current-buffer))))))

(defun jao-weather--get-info-now (&optional id)
  (let* ((url (jao-weather--url id))
         (buffer (url-retrieve-synchronously url))
         (data (and buffer
                    (with-current-buffer buffer
                      (jao-weather-get-alist)))))
    (when buffer (kill-buffer buffer))
    (when data
      (setq jao-weather-last-data data)
      (run-hooks 'jao-weather-hook))
    data))


;;; formatting:
(defun jao-weather--flist (c fs)
  (when c
    (let (result)
      (dolist (f fs result)
        (let ((v (caddr (assoc (cadr f) c))))
          (when (and (stringp v) (not (string-equal v "N/A")))
            (push (cons (car f) v) result)))))))

(defun jao-weather--parse-cc (cc)
  (append (jao-weather--flist cc '((:date lsup)
                                   (:observatory obst)
                                   (:temperature tmp)
                                   (:condition t)
                                   (:pressure r)))
          (jao-weather--flist (assoc 'wind cc) '((:windir d)
                                                 (:wind-tilt t)
                                                 (:gust gust)))))

(defun jao-weather--parse-location (loc)
  (jao-weather--flist loc '((:city dnam)
                            (:time tm)
                            (:latitude lat)
                            (:longitude lon)
                            (:sunrise sunr)
                            (:sunset suns))))

(defun jao-weather--parse-day (d)
  (let ((p2 (assoc 'part
                   (remove (assoc 'part (cdr d))
                           (cdr d))))
        (wday (or (cdr (assoc 't (cadr d))) "day")))
    `(,(cdr (assoc 'dt (cadr d)))
      (:weekday . ,wday)
      (:weekday-abbrev . ,(substring wday 0 3))
      ,@(jao-weather--flist (cdr d) '((:max hi)
                                      (:min low)
                                      (:sunrise sunr)
                                      (:sunset suns)
                                      (:humidity hmid)))
      ,@(jao-weather--flist (assoc 'wind (assoc 'part (cdr d)))
                            '((:wind-dir 't) (:wind-speed 's)))
      ,@(jao-weather--flist (assoc 'wind p2) '((:night-wind-dir wea)
                                               (:night-wind-speed s)))
      ,@(jao-weather--flist p2
                            '((:night-condition t) (:night-humidity hmid))))))

(defun jao-weather-get-alist ()
  (let* ((pxml (car (xml-parse-region (point-min) (point-max))))
         (loc (car (xml-get-children pxml 'loc)))
         (cc (car (xml-get-children pxml 'cc)))
         (dayf (xml-get-children pxml 'dayf))
         (dayfs (xml-get-children (car dayf) 'day))
         (today (append (jao-weather--parse-cc cc)
                        (jao-weather--parse-location loc)))
         (forecast (mapcar 'jao-weather--parse-day dayfs)))
    `((today ,@today) (forecast ,@forecast))))

(defun jao-weather--format-fields (data fields sep)
  (if data
      (mapconcat (lambda (kv)
                   (let ((v (cdr (assoc (car kv) data))))
                     (if (not v) ""
                       (format (or (cdr kv) "%s") v))))
             fields
             sep)
    ""))

(defsubst jao-weather--today-string (fields sep)
  (jao-weather--format-fields (cdr (assoc 'today jao-weather-last-data))
                              fields sep))

(defun jao-weather--forecast-string (n fields sep)
  (jao-weather--format-fields (nth n (cdr (assoc 'forecast
                                                 jao-weather-last-data)))
                              fields sep))


;;; update daemon:
(defvar jao-weather--timer nil)
(defun jao-weather-start (&optional delay)
  (interactive)
  (jao-weather-stop)
  (setq jao-weather--timer
        (run-with-timer (or delay 0)
                        jao-weather-timer-delay
                        'jao-weather--get-info-async)))

(defun jao-weather-stop ()
  (interactive)
  (when jao-weather--timer
    (cancel-timer jao-weather--timer)
    (setq jao-weather--timer nil)))


;;; today
(defun jao-weather-today-msg (&optional arg)
  (interactive "p")
  (when (> arg 4) (jao-weather--get-info-now))
  (if (= 4 arg) (jao-weather-forecast-msg)
    (message "%s" (jao-weather--today-string '((:temperature . " %s°C")
                                               (:condition . "(%s)")
                                               (:sunrise . "↑ %s")
                                               (:sunset . "↓ %s")
                                               (:date . "[%s]"))
                                             " "))))

(defun jao-weather-forecast-msg (&optional arg)
  (interactive "P")
  (when arg (jao-weather--get-info-now))
  (message " %s" (mapconcat
                 (lambda (n)
                   (jao-weather--forecast-string n
                                                 '((:weekday-abbrev . "%s ")
                                                   (:max . "%s°/")
                                                   (:min . "%s°")
                                                   (:condition . ", %s")
                                                   (:night-condition . ", %s"))
                                                 ""))
                 '(1 2 3 4) " | ")))

(defun jao-weather-temperature ()
  (string-to-number (jao-weather--today-string '((:temperature)) "")))


(defun jao-weather-temperature* (&optional sep)
  (concat (jao-weather--today-string '((:temperature . "%s°")) "")
          (or sep " ")
          (jao-weather--forecast-string 1
                                        '((:max . "%s°/") (:min . "%s°")
                                          (:night-condition . " %s"))
                                        "")))

;; Provide
(provide 'jao-weather)
