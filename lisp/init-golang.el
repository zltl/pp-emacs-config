;;; init-golang.el --- golang config

;; install golang:
;;   https://go.dev/doc/install
;; set goproxy:
;;   export GOPROXY=https://goproxy.io,direct
;; intall tools
;;   go install golang.org/x/tools/gopls@latest

;;; Code:

(use-package go-mode
  :mode "\\.go\\'"
  :hook
  (go-mode . lsp-deferred)
  (before-save-hook . lsp-format-buffer)
  (before-save-hook . lsp-organize-imports))

(provide 'init-golang)
;;; init-golang.el ends here
