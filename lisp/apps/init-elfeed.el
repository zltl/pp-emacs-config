;;; init-elfeed.el --- RSS reader configuration -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Elfeed RSS/Atom reader configuration.
;; Feeds can be managed via M-x customize-variable elfeed-feeds
;; or through an org file with elfeed-org.
;;
;;; Code:

(use-package elfeed
  :defer t
  :commands elfeed
  :custom
  (elfeed-search-filter "@1-month-ago +unread")
  (elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)))

(use-package elfeed-org
  :after elfeed
  :custom
  (rmh-elfeed-org-files (list (expand-file-name "elfeed.org" user-emacs-directory)))
  :config
  (elfeed-org))

(provide 'init-elfeed)
;;; init-elfeed.el ends here
