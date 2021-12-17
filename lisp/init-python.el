;;; init-python.el --- configuration of python support
;;; Commentary:
;;; Code:

(use-package pip-requirements)

(use-package elpy
  :init
  (elpy-enable))

(add-hook 'python-mode-hook '(lambda () (setq python-indent 4)))

(provide 'init-python)
;;; init-python.el ends here
