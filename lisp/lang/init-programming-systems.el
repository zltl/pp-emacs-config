;;; init-programming-systems.el --- Systems programming languages -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Configuration for systems programming languages:
;; - C/C++
;; - Rust
;; - Go
;; - Protobuf
;;
;;; Code:

;; `seq' provides sequence helpers used below for filtering missing Go
;; tools; requiring it explicitly keeps the dependency obvious.
(require 'seq)
(declare-function ltl/eglot-ensure "init-programming-core")
(declare-function apheleia-format-buffer "apheleia")
(defvar eglot-workspace-configuration)

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
        ;; Parse BasedOnStyle first so project-specific overrides inherit
        ;; sane defaults from the upstream style they extend.
        (goto-char (point-min))
        (when (re-search-forward "^BasedOnStyle:\\s-*\\(.+\\)" nil t)
          (setq base-style (string-trim (match-string 1)))
          (when-let ((defaults (cdr (assoc base-style ltl/clang-format-base-styles))))
            (setq result (copy-sequence defaults))))
        ;; Override with explicit values so the local file wins over the
        ;; base style, matching clang-format's own precedence rules.
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
  ;; Mirroring .clang-format inside Emacs means TAB/RET indentation matches
  ;; formatter output, reducing surprise diffs after save/format.
  (when-let ((settings (ltl/clang-format-parse-file)))
    (when-let ((indent-width (plist-get settings :indent-width)))
      (setq-local c-basic-offset indent-width))
    (when-let ((use-tab (plist-get settings :use-tab)))
      (setq-local indent-tabs-mode use-tab))
    (when-let ((tab-width (plist-get settings :tab-width)))
      (setq-local tab-width tab-width))))

;; Built-in `cc-mode' remains the base for C/C++; the hooks here align
;; indentation and language-server startup with project conventions.
(use-package cc-mode
  :ensure nil
  :defer t
  :hook ((c-mode c++-mode c-ts-mode c++-ts-mode) . ltl/eglot-ensure)
  :hook ((c-mode c++-mode c-ts-mode c++-ts-mode) . ltl/apply-clang-format-style)
  ;; Ensure electric indentation is active in both classic and tree-sitter
  ;; C/C++ modes for immediate block-aware indentation on RET.
  :hook ((c-mode c++-mode c-ts-mode c++-ts-mode) . (lambda () (electric-indent-local-mode 1))))

;; Treat .h as C++ headers by default; in mixed C/C++ codebases this tends
;; to produce better highlighting and language-server behavior.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-ts-mode))

;; `protobuf-mode' gives syntax highlighting/editing support for .proto files.
;; MELPA clones all of protocolbuffers/protobuf for one .el file; the
;; emacsmirror copy is the editor excerpt only.
(use-package protobuf-mode
  :ensure (protobuf-mode :host github :repo "emacsmirror/protobuf-mode"
                         :depth 1 :inherit ignore)
  :defer t
  :mode "\\.proto\\'")

;;; Rust

;; `rust-mode' is kept for classic Rust buffers and command bindings even
;; though tree-sitter and Eglot provide most of the heavy lifting.
(use-package rust-mode
  :defer t
  :mode "\\.rs\\'"
  :bind (:map rust-mode-map
              ("C-c C-r" . #'rust-run)
              ("C-c C-c" . #'rust-compile)
              ("C-c C-f" . #'apheleia-format-buffer)
              ("C-c C-t" . #'rust-test))
  ;; Rust benefits from typographic symbol prettification and automatic LSP startup.
  :hook (rust-mode . prettify-symbols-mode)
  :hook ((rust-mode rust-ts-mode) . ltl/eglot-ensure))

;;; Go

(defvar ltl/go-tools-map
  '(("gopls" . "golang.org/x/tools/gopls@latest")
    ("goimports" . "golang.org/x/tools/cmd/goimports@latest")
    ("dlv" . "github.com/go-delve/delve/cmd/dlv@latest"))
  "Map of Go tool names to their install packages.")

(defvar ltl/go-exec-path-initialized nil
  "Non-nil after Go bin directories have been added to `exec-path'.")

(defvar ltl/go-tools-missing-warned nil
  "Non-nil after a missing-tools notice has been shown this session.")

(defun ltl/prepend-exec-path (dir)
  "Prepend DIR to `exec-path' and the PATH environment variable."
  (when (and dir (file-directory-p dir))
    (setq dir (directory-file-name (expand-file-name dir)))
    (setq exec-path (cons dir (delete dir exec-path)))
    (let ((parts (parse-colon-path (or (getenv "PATH") ""))))
      (unless (member dir parts)
        (setenv "PATH" (string-join (cons dir parts) path-separator))))))

(defun ltl/go-ensure-exec-path (&optional force)
  "Make the Go toolchain and `go install' binaries visible to Emacs.
GUI sessions often inherit ~/.profile rather than ~/.bashrc, so
~/go/bin and ~/.local/go/bin are missing even when the tools exist.
With FORCE, scan again (used after a fresh `go install')."
  (when (or force (not ltl/go-exec-path-initialized))
    (dolist (dir (list (expand-file-name ".local/go/bin" "~")
                       (expand-file-name "go/bin" "~")
                       (getenv "GOBIN")
                       (when-let* ((gopath (getenv "GOPATH")))
                         (expand-file-name "bin" (car (split-string gopath path-separator))))))
      (ltl/prepend-exec-path dir))
    (setq ltl/go-exec-path-initialized t)))

(defun ltl/go-missing-tools ()
  "Return alist entries from `ltl/go-tools-map' that are not on `exec-path'."
  (ltl/go-ensure-exec-path)
  (seq-filter (lambda (tool) (not (executable-find (car tool))))
              ltl/go-tools-map))

(defun ltl/go-install-tools (&optional packages)
  "Install or update Go tools in the background.
Installs each package separately since they may be from different modules."
  ;; Running installs asynchronously keeps Emacs usable while toolchains
  ;; update, which matters because `go install' can take a while.
  (interactive)
  (ltl/go-ensure-exec-path)
  (unless (executable-find "go")
    (user-error "Go toolchain not found. Install Go first"))
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
                     (ltl/go-ensure-exec-path t)
                     (if failed
                         (message "Go tools installation finished with errors: %s" failed)
                       (message "Go tools installation complete. Run M-x eglot if LSP did not start."))))))))

(defun ltl/go-check-tools ()
  "Report missing Go tools without blocking file-open.
Never prompt here: `y-or-n-p' in a mode hook freezes `find-file'
until the user answers, and a declined prompt used to repeat on
every subsequent Go buffer."
  (ltl/go-ensure-exec-path)
  (when (and (or (eq major-mode 'go-mode)
                 (eq major-mode 'go-ts-mode))
             (not ltl/go-tools-missing-warned)
             (executable-find "go"))
    (when-let* ((missing (ltl/go-missing-tools)))
      (setq ltl/go-tools-missing-warned t)
      (message "Missing Go tools (%s). Run M-x ltl/go-install-tools"
               (mapconcat #'car missing ", ")))))

(defun ltl/go-eglot-config ()
  "Ask gopls for a smaller first-load payload in this buffer.
Set before `eglot-ensure' so workspace/configuration sees it."
  (setq-local eglot-workspace-configuration
              '(:gopls (:semanticTokens :json-false
                        :staticcheck :json-false
                        :analysisProgressReporting :json-false
                        :directoryFilters ["-**/node_modules"
                                           "-**/.git"
                                           "-**/vendor"
                                           "-**/testdata"]))))

(defun ltl/go-setup ()
  "Ensure Go binaries are on PATH, then start LSP."
  ;; Path first so Eglot can see gopls that live in ~/go/bin.
  (ltl/go-ensure-exec-path)
  (ltl/go-check-tools)
  (ltl/go-eglot-config)
  (ltl/eglot-ensure))

;; `go-mode' handles classic Go buffers while tree-sitter remaps may
;; promote some files to go-ts-mode automatically.
(use-package go-mode
  :defer t
  :mode "\\.go\\'"
  :interpreter "go"
  :hook ((go-mode go-ts-mode) . ltl/go-setup))

(provide 'init-programming-systems)
;;; init-programming-systems.el ends here
