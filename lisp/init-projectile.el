;;; init-projectile.el --- Use Projectile for navigation within projects -*- lexical-binding: t -*-

;;; Commentary:
;; neotree, lsp-mode ... build on top of projectile

;;; Code:

(use-package projectile
  :config
  (projectile-mode +1)
  (setq projectile-enable-caching t)
  (setq-default projectile-mode-line-prefix " Proj")
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))



(provide 'init-projectile)
;;; init-projectile.el ends here
