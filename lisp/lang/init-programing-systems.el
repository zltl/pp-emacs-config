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

(defvar ltl/clang-format-base-styles
  '(("LLVM"       . (:indent-width 2 :use-tab nil :tab-width 8))
    ("Google"     . (:indent-width 2 :use-tab nil :tab-width 8))
    ("Chromium"   . (:indent-width 2 :use-tab nil :tab-width 8))
    ("Mozilla"    . (:indent-width 2 :use-tab nil :tab-width 8))
    ("WebKit"     . (:indent-width 4 :use-tab nil :tab-width 8))
    ("Microsoft"  . (:indent-width 4 :use-tab nil :tab-width 4))
    ("GNU"        . (:indent-width 2 :use-tab nil :tab-width 8)))
  "Predefined clang-format base styles with their default values.")

(defun ltl/clang-format-parse-file ()
  "Parse .clang-format file and return plist of settings.
Searches upward from current directory for .clang-format file."
  (when-let* ((clang-format-dir (locate-dominating-file default-directory ".clang-format"))
              (clang-format-file (expand-file-name ".clang-format" clang-format-dir)))
    (with-temp-buffer
      (insert-file-contents clang-format-file)
      (let (result base-style)
        ;; Parse BasedOnStyle first
        (goto-char (point-min))
        (when (re-search-forward "^BasedOnStyle:\\s-*\\(.+\\)" nil t)
          (setq base-style (string-trim (match-string 1)))
          (when-let ((defaults (cdr (assoc base-style ltl/clang-format-base-styles))))
            (setq result (copy-sequence defaults))))
        ;; Override with explicit values
        (goto-char (point-min))
        (when (re-search-forward "^IndentWidth:\\s-*\\([0-9]+\\)" nil t)
          (setq result (plist-put result :indent-width (string-to-number (match-string 1)))))
        (goto-char (point-min))
        (when (re-search-forward "^TabWidth:\\s-*\\([0-9]+\\)" nil t)
          (setq result (plist-put result :tab-width (string-to-number (match-string 1)))))
        (goto-char (point-min))
        (when (re-search-forward "^UseTab:\\s-*\\(.+\\)" nil t)
          (setq result (plist-put result :use-tab (not (string= (string-trim (match-string 1)) "Never")))))
        result))))

(defun ltl/apply-clang-format-style ()
  "Apply .clang-format settings to cc-mode indentation."
  (when-let ((settings (ltl/clang-format-parse-file)))
    (when-let ((indent-width (plist-get settings :indent-width)))
      (setq-local c-basic-offset indent-width))
    (when-let ((use-tab (plist-get settings :use-tab)))
      (setq-local indent-tabs-mode use-tab))
    (when-let ((tab-width (plist-get settings :tab-width)))
      (setq-local tab-width tab-width))))

(use-package cc-mode
  :ensure nil
  :hook ((c-mode c++-mode) . eglot-ensure)
  :hook ((c-mode c++-mode c-ts-mode c++-ts-mode) . ltl/apply-clang-format-style)
  :hook ((c-mode c++-mode c-ts-mode c++-ts-mode) . (lambda () (electric-indent-local-mode 1))))

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
  :hook (rust-mode . eglot-ensure))

;;; Go

(defvar ltl/go-tools-map
  '(("gopls" . "golang.org/x/tools/gopls@latest")
    ("goimports" . "golang.org/x/tools/cmd/goimports@latest")
    ("dlv" . "github.com/go-delve/delve/cmd/dlv@latest"))
  "Map of Go tool names to their install packages.")

(defvar ltl/go-tools-last-check-time nil
  "Timestamp of the last Go tools update check.")

(defun ltl/go-install-tools (&optional packages)
  "Install or update Go tools in the background.
Installs each package separately since they may be from different modules."
  (interactive)
  (let* ((pkgs (or packages (mapcar #'cdr ltl/go-tools-map)))
         (remaining (length pkgs))
         (failed nil))
    (message "Installing %d Go tool(s) in background..." remaining)
    (dolist (pkg pkgs)
      (make-process
       :name (format "go-install-%s" (file-name-base pkg))
       :buffer "*go-tools-install*"
       :command (list "go" "install" pkg)
       :sentinel (lambda (proc event)
                   (unless (string= event "finished\n")
                     (push (process-name proc) failed))
                   (cl-decf remaining)
                   (when (zerop remaining)
                     (if failed
                         (message "Go tools installation finished with errors: %s" failed)
                       (message "Go tools installation complete."))
                     (setq ltl/go-tools-last-check-time (current-time))))))))

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
  :hook ((go-mode go-ts-mode) . eglot-ensure)
  :hook ((go-mode go-ts-mode) . ltl/go-check-tools))

(provide 'init-programing-systems)
;;; init-programing-systems.el ends here
