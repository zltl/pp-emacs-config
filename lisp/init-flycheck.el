;;; init-flycheck.el --- Setting of flycheck
;;; Commentary:
;;; Code:

;; pip install pylint
;; npm install eslint

;;; Code:
(use-package flycheck
  :diminish nil
  :config
  (global-flycheck-mode))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
