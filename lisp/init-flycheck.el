;;; init-flycheck.el --- Setting of flycheck

;; pip install pylint
;; npm install eslint

;;; Code:
(use-package flycheck
  :diminish nil
  :init (global-flycheck-mode))

(provide 'init-flycheck)
;;; init-flycheck.el ends here

