;;; init-coding.el -- coding system (unicode utf-8)
;;; Commentary:
;;; Code:

;; make UTF-8 the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8-unix)
(setq locale-coding-system 'utf-8-unix)

(provide 'init-coding)
;;; init-coding.el ends here
