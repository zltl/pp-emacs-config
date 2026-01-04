;;; init-search.el --- Search and navigation tools -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This module configures search and navigation tools:
;; - ag: The Silver Searcher for fast code search
;; - swiper: Better in-buffer search with preview
;; - counsel: Enhanced Emacs commands with completion
;;
;;; Code:

(use-package ag
  :defer t
  :commands ag)

;; better search in buffer
(use-package swiper
  :config
  (global-set-key "\C-s" 'swiper))
(use-package counsel
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x))


(provide 'init-search)
