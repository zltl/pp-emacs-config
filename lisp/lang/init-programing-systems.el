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

(require 'seq)

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

(defvar ltl/go-tools-map
  '(("gopls" . "golang.org/x/tools/gopls@latest")
    ("goimports" . "golang.org/x/tools/cmd/goimports@latest")
    ("dlv" . "github.com/go-delve/delve/cmd/dlv@latest"))
  "Map of Go tool names to their install packages.")

(defvar ltl/go-tools-last-check-time nil
  "Timestamp of the last Go tools update check.")

(defun ltl/go-install-tools (&optional packages)
  "Install or update Go tools in the background."
  (interactive)
  (let ((pkgs (or packages (mapcar #'cdr ltl/go-tools-map))))
    (message "Installing Go tools in background...")
    (make-process
     :name "go-tools-install"
     :buffer "*go-tools-install*"
     :command (append '("go" "install") pkgs)
     :sentinel (lambda (proc event)
                 (when (string= event "finished\n")
                   (message "Go tools installation complete.")
                   (setq ltl/go-tools-last-check-time (current-time)))))))

(defun ltl/go-check-tools ()
  "Check for missing or outdated Go tools."
  (when (and (or (eq major-mode 'go-mode)
                 (eq major-mode 'go-ts-mode))
             (executable-find "go"))
    (let* ((missing (seq-filter (lambda (tool) (not (executable-find (car tool)))) ltl/go-tools-map))
           (should-check (or (not ltl/go-tools-last-check-time)
                             (> (float-time (time-since ltl/go-tools-last-check-time))
                                (* 60 60 24))))) ; Check daily
      (cond
       (missing
        (when (y-or-n-p (format "Missing Go tools (%s). Install now? "
                                (mapconcat #'car missing ", ")))
          (ltl/go-install-tools (mapcar #'cdr missing))))
       (should-check
        (when (y-or-n-p "Check for Go tool updates? ")
          (ltl/go-install-tools)))))))

(use-package go-mode
  :defer t
  :mode "\\.go\\'"
  :interpreter "go"
  :hook ((go-mode go-ts-mode) . lsp-deferred)
  :hook ((go-mode go-ts-mode) . ltl/go-check-tools))

(provide 'init-programing-systems)
;;; init-programing-systems.el ends here
