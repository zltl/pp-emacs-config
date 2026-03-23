;;; init-search.el --- Search and navigation tools -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This module configures search and navigation tools:
;; - ag: The Silver Searcher for fast code search
;; - consult-line: In-buffer search (replaces swiper, part of Vertico ecosystem)
;;
;;; Code:

(use-package ag
  :defer t
  :commands ag)

;; Use consult-line for in-buffer search (consistent with Vertico + Consult ecosystem)
;; Note: consult is loaded in init-complete.el
(with-eval-after-load 'consult
  (global-set-key (kbd "C-s") #'consult-line))


(provide 'init-search)
