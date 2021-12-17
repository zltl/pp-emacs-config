;;; init-rust.el --- Rust language support
;;; Commentary:
;;; Code:

(use-package rust-mode)

;; TODO: melpa not work for rustic?
;; (use-package rustic)

(defvar-local rustic-load-file
  (expand-file-name "vendor/rustic/rustic.el" user-emacs-directory))

(defun my/rustic-load-config ()
    "Load rustic and config."
    (require 'rustic)
    (setq rustic-format-on-save t)
    (add-hook 'rustic-mode-hook 'lsp-deferred))


(when (file-exists-p rustic-load-file)
  (my/rustic-load-config))


(provide 'init-rust)
;;; init-rust.el ends here
