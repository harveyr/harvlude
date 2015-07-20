;;; utils.el --- Harvey's uncategorized emacs utils

;;; Commentary:
;; Don't know what I'm doooooing

;;; Code:

(defun get-current-line ()
  "Return current line as string."
  (buffer-substring-no-properties
   (line-beginning-position)
   (line-end-position))
  )

(defun h-s-trim-left (s)
  "Remove whitespace at the beginning of S."
  (if (string-match "\\`[ \t\n\r]+" s)
      (replace-match "" t t s)
    s)
  )

(defun h-s-trim-right (s)
  "Remove whitespace at the end of S."
  (if (string-match "[ \t\n\r]+\\'" s)
      (replace-match "" t t s)
    s)
  )

(defun h-s-trim (s)
  "Remove whitespace at the beginning and end of S."
  (h-s-trim-left (h-s-trim-right s))
  )

(defun h-kill-parens-inner ()
  "Kill content within parens."
  (let (r-start r-end)
    (search-backward "(")
    (forward-char)
    (setq r-start (point))
    (search-forward "):")
    (backward-char)
    (backward-char)
    (setq r-end (point))
    (message "killing region %s-%s" r-start r-end)
    (kill-region r-start r-end)
    )
  )

(defun h-move-buffer-other-window ()
  "Move current buffer to other window."
  (interactive)
  (let (current-buffer-name)
    ;; get current buffer name
    (setq current-buffer-name (buffer-name))

    ;; switch current window to last buffer
    (switch-to-buffer nil)

    ;; move to other window
    (other-window 1)

    ;; load the buffer
    (switch-to-buffer current-buffer-name)
    )
  )

(provide 'harvey-utils)
;;; harvey-utils.el ends here
