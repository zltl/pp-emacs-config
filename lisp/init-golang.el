;;; init-golang.el --- golang config
;;; Commentary:
;;; Code:

;; install golang:
;;   https://go.dev/doc/install
;; set goproxy:
;;   export GOPROXY=https://goproxy.io,direct
;; intall tools
;;   go install golang.org/x/tools/gopls@latest
;;   go get github.com/fatih/gomodifytags

;;; Code:

(use-package go-mode
  :mode "\\.go\\'"
  :hook
  (before-save . lsp-format-buffer)
  (before-save . lsp-organize-imports)
  :config
  (with-eval-after-load 'go-mode (setq-default tab-width 4)))

(use-package gotest)

(use-package go-tag
  :config
  (setq go-tag-args (list "-transform" "snakecase")))


(provide 'init-golang)
;;; init-golang.el ends here
