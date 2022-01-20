;;; init-mmm.el --- enable multiple major mode coexists
;;; commentary:
;;; code:

(use-package mmm-mode)
(require 'mmm-auto)
(setq mmm-global-mode 'buffers-with-submode-classes)
(setq mmm-submode-decoration-level 2)

(provide 'init-mmm)
;;; init-mmm.el ends here
