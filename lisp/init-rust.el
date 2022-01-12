;;; init-rust.el --- Rust language support
;;; Commentary:
;;; Code:

(use-package rust-mode)

(use-package rustic
  :hook (rustic-mode . lsp-deferred)
  :config
  (setq rustic-format-on-save t))

(provide 'init-rust)
;;; init-rust.el ends here
