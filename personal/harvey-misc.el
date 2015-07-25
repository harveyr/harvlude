;;; harvey-misc.el --- Miscellaneous stuff that should probably go elsewhere

;;; Commentary:

;;; Code:
(server-start)

(prelude-require-package 'browse-at-remote)

;; smart-mode-line
(require 'smart-mode-line)
(sml/setup)

;; edit server, for chrome's edit-in-emacs
(require 'edit-server)
(edit-server-start)

;; Disable spell checking by default
(setq prelude-flyspell nil)

(setq python-shell-interpreter "/usr/local/bin/ipython")

;; Tags... sigh
(global-set-key (kbd "M-.") 'eshell-find-tag)
(setq tags-revert-without-query 1)
(add-to-list 'completion-at-point-functions 'semantic-completion-at-point-function)

;; An easier M-x
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; Commenting (use M-; instead)
;; (global-set-key (kbd "C-x C-/") 'comment-or-uncomment-region)

(defun get-authinfo-field (machine field)
  "Extract field from authinfo file."
  (require 'netrc)
  (let* ((parsed (netrc-parse "~/.authinfo"))
         (item (netrc-machine parsed machine)))
    (netrc-get item field)))


(defun get-authinfo-pw (machine)
  "Extract password from authinfo file."
  (get-authinfo-field machine "password")
  )

(defun create-auth-header (username password)
  "Create base64'd auth header for HTTP requests."
  (require 'base64)
  (concat "Basic " (base64-encode-string (concat username ":" password))))

(defun create-auth-header-for-authinfo-machine (machine)
  (let (username password)
    (setq username (get-authinfo-field machine "username"))
    (setq password (get-authinfo-field machine "password"))
    (create-auth-header username password)
    ))

(defvar jive-host "urbanairship.jiveon.com")

(defun h-make-jive-url (path)
  "Make a Jive URL with the given path."
  (format "https://%s/api/core/v3%s" jive-host path))

(defun h-jive-get-latest ()
  "Get latest Jive stuffs."
  (require 'request)
  (let (url auth-header)
    (setq url (h-make-jive-url '/activities))
    (setq auth-header (create-auth-header-for-authinfo-machine jive-host))
    (message url)
    (request
     url
     :parser  'json-read
     :headers '(("Authorization" . auth-header))
     :success (function*
               (lambda (&key data &allow-other-keys)
                 (message "here? %S" data)))
     :error (function*
             (lambda (&key error-thrown &allow-other-keys&rest _)
               (message "Got error: %S" error-thrown)))
     )))


(h-jive-get-latest)


(message "testing: %s" (h-make-jive-url "/activities"))
(message "testing: %s" (authinfo-field jive-host "login"))
(message "testing: %s"
         (create-auth-header-for-authinfo-machine
          "urbanairship.jiveon.com"))

(defun get-sentry-errors (organization project)
  "Get sentry errors. Experimental."
  (require 'json)
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
        (buffer-string)
        (beginning-of-buffer)
        (search-forward "[")
        (backward-char)
        (delete-region (point-min) (point))
        (json-read-from-string (buffer-string))
        ))))

(get-sentry-errors "ua" "airship-py")
(defun test-sentry-stuff ()
  (interactive)
  (get-sentry-errors "ua" "airship-py"))


(json-read-from-string "[[1437696000, 1]]")

(require 're-builder-x)
(setq reb-re-syntax 'perl)

(provide 'harvey-misc)
;;; harvey-misc.el ends here
