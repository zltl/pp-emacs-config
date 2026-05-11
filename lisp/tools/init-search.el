;;; init-search.el --- Search and navigation tools -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This module configures search and navigation tools:
;; - ag: The Silver Searcher for fast code search
;; - consult-line: In-buffer search (replaces swiper, part of Vertico ecosystem)
;;
;;; Code:

;; `ag' keeps a direct entry point to The Silver Searcher for projects
;; where that tool is already installed and preferred.
(use-package ag
  :defer t
  :commands ag)

;; Use consult-line for in-buffer search (consistent with Vertico + Consult ecosystem)
;; Note: consult is loaded in init-complete.el
;; Rebinding C-s keeps search UI consistent with the rest of the
;; minibuffer completion stack and gives live preview of matches.
(with-eval-after-load 'consult
  (global-set-key (kbd "C-s") #'consult-line))


(provide 'init-search)
