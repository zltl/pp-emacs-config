;;; init-html.el -- html/css/sass/scss
;;; commentary:
;;; code:

(use-package add-node-modules-path)

(use-package company-web
  :requires company)

(use-package css-mode)
(use-package less-css-mode)
(use-package sass-mode)
(use-package scss-mode)
(use-package slim-mode)

(use-package tagedit)

(use-package web-mode)

(use-package web-beautify)

(use-package js2-mode)

(use-package prettier-js)

(add-hook 'css-mode-hook #'add-node-modules-path)
(add-hook 'less-css-mode-hook #'add-node-modules-path)
(add-hook 'pug-mode-hook #'add-node-modules-path)
(add-hook 'sass-mode-hook #'add-node-modules-path)
(add-hook 'scss-mode-hook #'add-node-modules-path)
(add-hook 'slim-mode-hook #'add-node-modules-path)
(add-hook 'web-mode-hook #'add-node-modules-path)

(provide 'init-html)
;;; init-html.el ends here
