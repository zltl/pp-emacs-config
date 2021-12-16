;;; init-search.el --- searching setting

;;; Code:

;; use swiper to replace i-search
(use-package swiper
  :bind ("C-s" . swiper))

(use-package ag)

(provide 'init-search)
;;; init-search.el ends here
