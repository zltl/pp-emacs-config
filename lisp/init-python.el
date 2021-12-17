;;; init-python.el --- configuration of python support
;;; Commentary:
;;; Code:

(use-package pip-requirements)

(use-package elpy
  :init
  (elpy-enable))

(provide 'init-python)
;;; init-python.el ends here
