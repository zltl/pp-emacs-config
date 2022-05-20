;;; init-lisp.el --- config common lisp
;;; Commentary:
;;; Code:

;; (use-package slime
;;   :config
;;   (require 'slime-autoloads)
;;   (slime-setup '(slime-fancy slime-quicklisp slime-asdf))
;;   (setq inferior-lisp-program "sbcl"))

(use-package sly
  :config
  (setq inferior-lisp-program "sbcl"))

(provide 'init-lisp)
;;; init-lisp.el ends here
