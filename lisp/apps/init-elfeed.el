;;; init-elfeed.el --- RSS reader configuration -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Elfeed RSS/Atom reader configuration.
;; Feeds can be managed via M-x customize-variable elfeed-feeds
;; or through an org file with elfeed-org.
;;
;;; Code:

;; `elfeed' provides a local-first RSS/Atom reader so news/blog intake can
;; stay inside Emacs and under plain-file control.
(use-package elfeed
  :defer t
  :commands elfeed
  :custom
  ;; Start with a practical unread window instead of the entire archive.
  (elfeed-search-filter "@1-month-ago +unread")
  ;; Store the database inside the Emacs directory to keep state portable
  ;; and separate from unrelated home-directory files.
  (elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)))

;; `elfeed-org' lets feed subscriptions live in an Org file, which makes
;; them easier to curate, document, and sync than a raw Lisp list.
(use-package elfeed-org
  :after elfeed
  :custom
  (rmh-elfeed-org-files (list (expand-file-name "elfeed.org" user-emacs-directory)))
  :config
  (elfeed-org))

(provide 'init-elfeed)
;;; init-elfeed.el ends here
