;;; init-js.el --- javascript
;;; commentary:
;;; code:

(use-package js-doc)
(use-package js2-mode)
(use-package js2-refactor)
(use-package nodejs-repl)
(use-package prettier-js)
(use-package skewer-mode)
(use-package tern)

(use-package typescript-mode)
(use-package import-js)

(defun my-js-hook ()
  "Js mode indent."
  (interactive)
  (setq	js-indent-level 2))

(add-hook 'js-mode-hook
		  #'my-js-hook)

(provide 'init-js)
;;; init-js.el ends here
