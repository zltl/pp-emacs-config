

(use-package ag)

;; better search in buffer
(use-package swiper
  :config
  (global-set-key "\C-s" 'swiper))
(use-package counsel
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x))


(provide 'init-search)
