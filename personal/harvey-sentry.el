;;; harvey-sentry.el --- Harvey's sentry play-sound

;;; Commentary:

;;; Code:

(defun get-current-timestamp ()
  "Return seconds since epoch as a number."
  (string-to-number (shell-command-to-string "date +%s")))


(defun fetch-sentry-timeseries (organization project)
  "Get sentry errors timeseries as list of lists.

ORGANIZATION is the organization-name string.

PROJECT is the project-name string."

  (require 'json)
  (require 'url)
  (let ((url-request-method "GET")
        (sentry-host "sentry.prod.urbanairship.com")
        (url (format "https://%s/api/0/projects/%s/%s/stats/"
                     sentry-host organization project))
        (api-key (get-authinfo-pw sentry-host))
        api-key
        response-buffer
        response-data
        url-request-extra-headers)
    (setq sentry-host "sentry.prod.urbanairship.com")
    (setq url (format "https://%s/api/0/projects/%s/%s/stats/"
                      sentry-host organization project))
    (setq api-key (get-authinfo-pw sentry-host))
    (setq auth-header (create-auth-header api-key ""))
    (setq url-request-method "GET")
    (setq url-request-extra-headers `(("Authorization" . ,auth-header)))
    ;; http://stackoverflow.com/questions/16447266/http-request-in-emacs
    (point)
    (with-current-buffer (url-retrieve-synchronously url)
      (progn
        (goto-char (point-min))
        (search-forward "[")
        (backward-char)
        (delete-region (point-min) (point))
        (json-read-from-string (buffer-string))
        ))))

(defun debug-type (x)
  (with-current-buffer (get-buffer-create "*debug-type*")
    (erase-buffer)
    (insert (format "Evaluating: %S" x))
    (newline)
    (newline)
    (insert "Types found:")
    (newline)
    (cond ((symbolp x) (insert "- symbol "))
          ((listp x) (insert "- list "))
          ((vectorp x) (insert "- vector "))
          ((arrayp x) (insert "- array "))
          ((bufferp x) (insert "- buffer "))
          ((consp x) (insert "- cons "))
          ((floatp x) (insert "- float "))
          ))
  (display-buffer "*debug-type*")
  )

(defun process-sentry-timeseries (data)
  "Do something with the sentry timeseries data.  Not sure what yet.

DATA is a list of lists."

  (let (ts-pair
        val
        timestamp
        age
        age-hours
        line-str
        (now (get-current-timestamp))
        (target-buffer "*sentry-timeseries*")
        (last-minute-total 0)
        (last-hour-total 0)
        (total-total 0))
    (with-current-buffer (get-buffer-create target-buffer)
      (erase-buffer)
      (dotimes (i (length data))
        (setq ts-pair (elt data i))
        (setq timestamp (elt ts-pair 0))
        (setq val (elt ts-pair 1))
        (setq age (- now timestamp))
        (setq age-hours (/ age 3600))
        (setq line-str (format "[%S] %s %s" ts-pair age-hours val))
        (if (> val 10)
            ;;http://stackoverflow.com/questions/2419225/printing-colored-characters-in-lisp-emacs
            (insert (propertize line-str 'face
                                '(:foreground "red")
                                ))
          (insert line-str))

        (newline)
        )
      (display-buffer target-buffer)
      )
    ))

(process-sentry-timeseries (vector `[1437746400, 1] `[1437750000, 3] `[1437753600, 3] `[1437757200, 1] `[1437760800, 6] `[1437764400, 8] `[1437768000, 15] `[1437771600, 11] `[1437775200, 2] `[1437778800, 0] `[1437782400, 0] `[1437786000, 0] `[1437789600, 0] `[1437793200, 0] `[1437796800, 0] `[1437800400, 0] `[1437804000, 4] `[1437807600, 0] `[1437811200, 1] `[1437814800, 0] `[1437818400, 0] `[1437822000, 1] `[1437825600, 3] `[1437829200, 0]))


;;(process-sentry-timeseries (fetch-sentry-timeseries "ua" "airship-py"))

(defun test-sentry-stuff ()
  (interactive)
  (fetch-sentry-timeseries "ua" "airship-py"))

(provide 'harvey-sentry)
;;; harvey-sentry.el ends here
