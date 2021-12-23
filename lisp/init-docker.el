;;; init-docker.el --- docker and dockerfile setting
;;; Commentary:
;;; Code:

(use-package docker
  :bind ("C-c d" . docker))

(use-package dockerfile-mode
  :mode (("Dockerfile\\'" . dockerfile-mode)))

(provide 'init-docker)
;;; init-docker.el ends here
