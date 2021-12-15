;;; init-faster.el -- configure that make emacs faster

;;; Code:

;; A second, case-insensitive pass over `auto-mode-alist' is time wasted, and
;; indicates misconfiguration (don't rely on case insensitivity for file names).
(setq auto-mode-case-fold nil)

(provide 'init-faster)
;;; init-faster.el ends here
