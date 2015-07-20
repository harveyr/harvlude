;;; harvey-python.el --- Harvey's misc python stuff

;;; Commentary:
;; Learning

;;; Code:

(require 'harvey-utils)

;;; Jedi

(prelude-require-package 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;;; Python util functions

(defun is-py-func-declaration-line ()
  "Return t if current line declares a Python function."
  (let (current-line current-line-words)
    (setq current-line (get-current-line))
    (setq current-line-words (split-string current-line))
    (string-equal (car current-line-words) "def")
    )
  )

(defun h-format-multiline-py-func ()
  "Reformat Python function declaration to multiline."
  (interactive)
  (let (args-str arg args args-count)
    ;;(setq currentWord (thing-at-point 'word))
    ;; (if (string-equal currentWord "def"))
    (if (is-py-func-declaration-line)
        (progn
          (beginning-of-line)
          (search-forward "(")
          (forward-char)
          )
      (progn
        (search-backward "(")
        (unless (is-py-func-declaration-line)
          (throw "Couldn't find start of function" t)
          )
        (forward-char)
        )
      )
    (h-kill-parens-inner)
    (setq args-str (car kill-ring-yank-pointer))
    (setq args (split-string args-str ","))
    (setq args-count (length args))

    (newline)

    (dotimes (i args-count)
      (setq arg (h-s-trim (pop args)))
      (message "arg %s" arg)
      (if arg
          (progn
            (indent-relative nil)
            (insert arg)
            (if (< i (- args-count 1))
                (insert ",")
              )
            ;;(indent-region (line-beginning-position) (line-end-position))
            (newline)
            )
        )
      )
    )
  )

(provide 'harvey-python)
;;; harvey-python.el ends here
