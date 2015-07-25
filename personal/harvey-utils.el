;;; utils.el --- Harvey's uncategorized emacs utils

;;; Commentary:
;; Don't know what I'm doooooing

;;; Code:

(defun goto-match-paren (arg)
  "Go to the matching parenthesis."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))


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


(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename
         (if (equal major-mode 'dired-mode)
             default-directory
           (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun debug-type (x)
  "Figure out what type X is.  Better way to do this?"
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
  (display-buffer "*debug-type*"))

(defun insert-current-date ()
  "Insert current date as a string."
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))

(defun h-regen-etags ()
  "Regenerate exuberant tags."
  (interactive)
  (error "I don't work yet")
  (let (root-dir tags-file file-pattern command)
    ;;(setq git-dir (h-s-trim (shell-command-to-string "git rev-parse --show-toplevel")))
    (if (string-match "python" (prin1-to-string major-mode))
        (setq file-pattern "*.py")
      )

    (unless file-pattern (error "I don't know how to handle %s" major-mode))

    (setq root-dir (projectile-project-root))
    (unless root-dir (error "You're not in a project"))

    (setq tags-file (concat root-dir "TAGS"))

    (let ((default-directory root-dir))
      "ctags -R -e --exclude=.git --exclude=node_modules --exclude=bin --exclude=lib"
      )
    (setq command
          (format "find %s -type f -name '%s' | xargs ctags -o %s --verbose"
                  root-dir file-pattern tags-file))
    (message "Running: %s" command)
    ;;(projectile-run-async-shell-command-in-root "ls")
    (kill-new command)
    (async-shell-command command)
    (visit-tags-table tags-file)
    )
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
