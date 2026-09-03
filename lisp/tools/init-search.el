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

;; `C-s' is bound directly in the Consult declaration in
;; `init-complete.el'. Keeping the binding there installs its autoload
;; immediately; a `with-eval-after-load' here would leave C-s on isearch
;; until some other command happened to load Consult first.


(provide 'init-search)
