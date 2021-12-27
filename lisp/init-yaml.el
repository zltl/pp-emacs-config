;;; init-yaml.el -- setting of yaml/yml file
;;; Commentary:
;;; Code:

(use-package yaml-mode
  :mode (("\\.yml\\'" . yaml-mode)
		 ("\\.yaml\\'" . yaml-mode)))

(provide 'init-yaml)
;;; init-yaml.el ends here
