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

;; Tags... sigh
(global-set-key (kbd "M-.") 'eshell-find-tag)
(setq tags-revert-without-query 1)
(add-to-list 'completion-at-point-functions 'semantic-completion-at-point-function)

;; An easier M-x
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; Commenting
(global-set-key (kbd "C-x C-/") 'comment-or-uncomment-region)

(provide 'harvey-misc)
;;; harvey-misc.el ends here
