;;; package --- init-modeline.el - fancy doom-modeline setting
;;; Commentary:
;;; Code:

;; NOTE: call all-the-icons-install-fonts manual.
(use-package all-the-icons)

(use-package doom-modeline
  :hook
  (after-init . doom-modeline-mode))

(provide 'init-modeline)
;;; init-modeline.el ends here
