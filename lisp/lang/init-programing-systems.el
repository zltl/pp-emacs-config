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

(defun ltl/clang-format-get-style-setting (key)
  "Get a setting value from .clang-format file in project."
  (when-let* ((project-root (or (locate-dominating-file default-directory ".clang-format")
                                (and (fboundp 'project-current)
                                     (project-current)
                                     (project-root (project-current)))))
              (clang-format-file (expand-file-name ".clang-format" project-root)))
    (when (file-exists-p clang-format-file)
      (with-temp-buffer
        (insert-file-contents clang-format-file)
        (goto-char (point-min))
        (when (re-search-forward (format "^%s:\\s-*\\(.+\\)" key) nil t)
          (string-trim (match-string 1)))))))

(defun ltl/apply-clang-format-style ()
  "Apply .clang-format settings to cc-mode indentation."
  (when-let ((indent-width (ltl/clang-format-get-style-setting "IndentWidth")))
    (setq-local c-basic-offset (string-to-number indent-width)))
  (when-let ((use-tabs (ltl/clang-format-get-style-setting "UseTab")))
    (setq-local indent-tabs-mode (not (string= use-tabs "Never"))))
  (when-let ((tab-width (ltl/clang-format-get-style-setting "TabWidth")))
    (setq-local tab-width (string-to-number tab-width))))

(use-package cc-mode
  :ensure nil
  :hook ((c-mode c++-mode) . lsp)
  :hook ((c-mode c++-mode c-ts-mode c++-ts-mode) . ltl/apply-clang-format-style))

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
