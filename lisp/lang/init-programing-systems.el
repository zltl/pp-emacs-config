;;; init-programing-systems.el --- Systems programming languages -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Configuration for systems programming languages:
;; - C/C++
;; - Rust
;; - Go
;; - Protobuf
;;
;;; Code:

;;; C/C++

(use-package cc-mode
  :ensure nil
  :hook ((c-mode c++-mode) . lsp))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-ts-mode))

(use-package protobuf-mode
  :defer t
  :mode "\\.proto\\'")

;;; Rust

(use-package rust-mode
  :defer t
  :mode "\\.rs\\'"
  :bind (:map rust-mode-map
              ("C-c C-r" . 'rust-run)
              ("C-c C-c" . 'rust-compile)
              ("C-c C-f" . 'rust-format-buffer)
              ("C-c C-t" . 'rust-test))
  :hook (rust-mode . prettify-symbols-mode)
  :hook (rust-mode . lsp))

;;; Go

(use-package go-mode
  :defer t
  :mode "\\.go\\'"
  :interpreter "go"
  :hook (go-mode . lsp))

(provide 'init-programing-systems)
;;; init-programing-systems.el ends here
