;;; init-nolittering.el -- keep configure folder clean
;;; Commentary:
;;; Code:

;; (require 'init-package)
;; (require 'init-recentf)

;; no litterint, keep .emacs.d clean
(use-package no-littering
  :config
  (with-eval-after-load 'recentf
	(add-to-list 'recentf-exclude no-littering-var-directory)
	(add-to-list 'recentf-exclude no-littering-etc-directory)
	(add-to-list 'recentf-exclude (expand-file-name "elpa" user-emacs-directory))
	(add-to-list 'recentf-exclude (expand-file-name "cache" user-emacs-directory)))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

(provide 'init-nolittering)
;;; init-nolittering.el ends here
