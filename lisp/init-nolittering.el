;;; init-nolittering.el -- keep configure folder clean
;;; Commentary:
;;; Code:

;; (require 'init-package)
;; (require 'init-recentf)

;; no littering, keep .emacs.d clean
(use-package no-littering
  :config
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

(provide 'init-nolittering)
;;; init-nolittering.el ends here
