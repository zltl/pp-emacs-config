;;; init-programming-web.el --- Web development languages -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Configuration for web development:
;; - JavaScript/TypeScript (via Eglot + typescript-language-server)
;; - HTML/CSS
;; - Tailwind CSS
;; - Web frameworks
;;
;; Note: Web development typically uses 2-space indentation
;; Customize via ltl/web-indent-level variable
;;
;;; Code:

;;; TypeScript/JavaScript — unified under Eglot (replaces tide)
;; Register TS/JS tree-sitter modes with the TypeScript language server so
;; the web stack shares one LSP backend and one set of editor commands.
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((typescript-ts-mode tsx-ts-mode) . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((js-ts-mode) . ("typescript-language-server" "--stdio"))))

;; Start Eglot automatically in common JS/TS tree-sitter modes so rename,
;; formatting, and diagnostics feel built in.
(dolist (hook '(typescript-ts-mode-hook tsx-ts-mode-hook js-ts-mode-hook))
  (add-hook hook #'eglot-ensure))

;; Keybindings consistent with the old tide setup, mapped to eglot equivalents
;; Preserving the old muscle memory makes the migration from Tide to Eglot
;; less disruptive for daily editing.
(with-eval-after-load 'typescript-ts-mode
  (define-key typescript-ts-mode-map (kbd "C-c C-r") #'eglot-rename)
  (define-key typescript-ts-mode-map (kbd "C-c C-f") #'eglot-format)
  (define-key typescript-ts-mode-map (kbd "C-c C-d") #'eldoc-doc-buffer))

(provide 'init-programming-web)
;;; init-programming-web.el ends here
