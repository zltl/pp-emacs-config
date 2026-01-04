;;; init-programing-web.el --- Web development languages -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Configuration for web development:
;; - JavaScript/TypeScript
;; - HTML/CSS
;; - Tailwind CSS
;; - Web frameworks
;;
;; Note: Web development typically uses 2-space indentation
;; Customize via ltl/web-indent-level variable
;;
;;; Code:

;;; TypeScript/JavaScript

(use-package tide
  :defer t
  :hook ((typescript-ts-mode . tide-setup)
         (tsx-ts-mode . tide-setup)
         (typescript-ts-mode . tide-hl-identifier-mode))
  :bind (:map typescript-ts-mode-map
              ("C-c C-r" . tide-rename-symbol)
              ("C-c C-f" . tide-format)
              ("C-c C-d" . tide-documentation-at-point)))

(provide 'init-programing-web)
;;; init-programing-web.el ends here
