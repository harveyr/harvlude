;;; harvey-elfeed.el --- Harvey's elfeed init

;;; Commentary:

;;; Code:

(prelude-require-package 'elfeed)
(global-set-key (kbd "C-x w") 'elfeed)
(setq elfeed-feeds
      '("http://www.reddit.com/r/emacs/.rss"
        "https://www.reddit.com/r/portland/.rss"
        "http://hnrss.org/newest?points=200"
        "http://toastdriven.com/feeds/fresh_news/"))

(provide 'harvey-elfeed)
;;; harvey-elfeed.el ends here
