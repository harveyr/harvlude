;;; harvey-sentry.el --- Harvey's sentry play-sound

;;; Commentary:

;;; Code:

(setq debug-on-error t)

(defvar sentry-timeseries-buffer "*sentry-timeseries*")
(defvar label-length-max 0)
(defvar should-poll-sentry nil)
(defvar poll-timer nil)

(defun get-current-timestamp ()
  "Return seconds since epoch as a number."
  (string-to-number (shell-command-to-string "date +%s")))


(defun assert-defined (name)
  "Assert that variable with NAME is defined."
  (unless (boundp name)
    (error "%s must be defined." name))
  t)

(defun fetch-sentry-timeseries (host org project)
  "Get sentry errors timeseries as list of lists.

ORGANIZATION is the organization-name string.

PROJECT is the project-name string."
  (require 'json)
  (require 'url)
  (let ((url-request-method "GET")
        (url (format "https://%s/api/0/projects/%s/%s/stats/"
                     host org project))
        (api-key (get-authinfo-pw host))
        auth-header
        response-buffer
        response-data
        url-request-extra-headers)
    (setq auth-header (create-auth-header api-key ""))
    (setq url-request-method "GET")
    (setq url-request-extra-headers `(("Authorization" . ,auth-header)))
    ;; http://stackoverflow.com/questions/16447266/http-request-in-emacs
    (with-current-buffer (url-retrieve-synchronously url)
      (progn
        (goto-char (point-min))
        (search-forward "[")
        (backward-char)
        (delete-region (point-min) (point))
        (json-read-from-string (buffer-string))
        ))))

(defun fake-sentry-data()
  (vector `[1437746400, 1] `[1437750000, 3] `[1437753600, 3] `[1437757200, 1] `[1437760800, 6] `[1437764400, 8] `[1437768000, 15] `[1437771600, 11] `[1437775200, 2] `[1437778800, 0] `[1437782400, 0] `[1437786000, 0] `[1437789600, 0] `[1437793200, 0] `[1437796800, 0] `[1437800400, 0] `[1437804000, 4] `[1437807600, 0] `[1437811200, 1] `[1437814800, 0] `[1437818400, 0] `[1437822000, 1] `[1437825600, 3] `[1437829200, 0]))

(defun process-sentry-timeseries (data)
  "Do something with the sentry timeseries data.  Not sure what yet.

DATA is a list of lists."

  (let (ts-pair
        val
        timestamp
        age
        age-hours
        value-buf
        timestamps
        (now (get-current-timestamp))
        (target-buffer "*sentry-timeseries*")
        (last-minute-total 0)
        (last-hour-total 0)
        (total-total 0))

    (with-current-buffer (get-buffer-create target-buffer)
      (dotimes (i (length data))
        (setq ts-pair (elt data i))
        (setq timestamp (elt ts-pair 0))
        (setq val (elt ts-pair 1))
        (setq age (- now timestamp))
        (setq age-hours (/ age 3600))
        (setq value-buf (format " %s " val))
        (if (> val 10)
            ;;http://stackoverflow.com/questions/2419225/printing-colored-characters-in-lisp-emacs
            (insert (propertize value-buf 'face
                                '(:foreground "red")))
          (insert value-buf))

        (if timestamps
            (setq timestamps (append timestamps (list timestamp)))
          (setq timestamps (list timestamp))))
      (insert-sentry-timeline timestamps)
      (display-buffer target-buffer))))

(defun insert-sentry-timeline (timestamps)
  "Insert a timeline row using list TIMESTAMPS."
  (let (age-hours
        (now (get-current-timestamp)))
    (with-current-buffer sentry-timeseries-buffer
      (newline)
      ;; see http://www.gnu.org/software/emacs/manual/html_node/elisp/Basic-Char-Syntax.html
      (insert (make-string label-length-max ?\s))
      (dolist (timestamp timestamps)
        (setq age-hours (/ (- now timestamp) 3600))
        (insert (format "%s | " age-hours))
        ))))

(defun refresh-one-sentry-timeseries (host org project)
  (message "Refreshing %s:%s (%s)" org project host)
  (with-current-buffer sentry-timeseries-buffer
    (let (label-length
          (label  (format "[%s] %s " org project)))
      (insert label)

      ;; Find the longest label
      (setq label-length (length label))
      (if (> label-length label-length-max)
          (setq label-length-max label-length)
        )))
  (process-sentry-timeseries (fake-sentry-data))
  )

(defun refresh-all-sentry-timeseries ()
  (assert-defined 'sentry-projects)
  (unless (vectorp sentry-projects)
    (error "sentry-projects must be a vector"))

  (with-current-buffer (get-buffer-create sentry-timeseries-buffer)
    (erase-buffer)
    (setq truncate-lines t)
    (setq truncate-partial-width-windows t)
    (insert (format "Sentry Error Count Timeseries (%s)" (format-time-string "%T")))
    (newline))

  (let (host org project)
    (dotimes (i (length sentry-projects))
      (let ((project-data (elt sentry-projects i)))
        (refresh-one-sentry-timeseries (elt project-data 0)
                                       (elt project-data 1)
                                       (elt project-data 2))))))

(refresh-all-sentry-timeseries)
;;(process-sentry-timeseries (fake-sentry-data))


;;(process-sentry-timeseries (fetch-sentry-timeseries "ua" "airship-py"))

(defun do-poll-sentry ()
  "Poll sentry."
  (refresh-all-sentry-timeseries)
  (setq poll-timer (run-at-time "5 sec" 5 'refresh-all-sentry-timeseries))
 )

(defun poll-sentry ()
  (interactive)
  (if poll-timer (cancel-timer poll-timer))
  (setq should-poll-sentry (if should-poll-sentry nil t))
  (if should-poll-sentry
      (do-poll-sentry)
    (message "Stopped polling Sentry")))

(provide 'harvey-sentry)
;;; harvey-sentry.el ends here
