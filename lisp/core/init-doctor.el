;;; init-doctor.el --- Configuration health check -*- lexical-binding: t -*-
;;; Commentary:
;;
;; This module provides diagnostic functions to check if the Emacs
;; configuration is properly set up with all required dependencies.
;;
;;; Code:

;;;
;;; Executable Programs Check
;;;

(defvar ltl/required-executables
  '(("git" . "Version control system (required)")
    ("ag" . "The Silver Searcher (optional but recommended)")
    ("fd" . "Modern find alternative (optional)")
    ("rg" . "Ripgrep search tool (optional)")
    ("node" . "Node.js for Copilot and some LSP servers (optional)"))
  "List of programs to check for availability.")

(defun ltl/doctor-check-executables ()
  "Check if required external programs are installed."
  (let ((missing '())
        (found '()))
    (dolist (exe ltl/required-executables)
      (if (executable-find (car exe))
          (push exe found)
        (push exe missing)))
    (if missing
        (progn
          (message "⚠ Missing programs:")
          (dolist (exe missing)
            (message "  - %s: %s" (car exe) (cdr exe))))
      (message "✓ All required programs found"))
    (when found
      (message "✓ Found programs: %s" 
               (mapconcat #'car found ", ")))
    (null missing)))

;;;
;;; Font Check
;;;

(defvar ltl/required-fonts
  '("Source Code Pro" "Droid Sans Fallback")
  "List of fonts required by this configuration.")

(defun ltl/doctor-check-fonts ()
  "Check if required fonts are installed."
  (if (not (display-graphic-p))
      (progn
        (message "ℹ Running in terminal mode, font check skipped")
        t)
    (let ((missing '()))
      (dolist (font ltl/required-fonts)
        (unless (find-font (font-spec :name font))
          (push font missing)))
      (if missing
          (progn
            (message "⚠ Missing fonts: %s" (string-join missing ", "))
            (message "  Install fonts from:")
            (message "  - Source Code Pro: https://github.com/adobe-fonts/source-code-pro")
            (message "  - Droid Sans Fallback: Install via package manager")
            nil)
        (progn
          (message "✓ All required fonts installed")
          t)))))

;;;
;;; Language Server Check
;;;

(defvar ltl/language-servers
  '(("gopls" . "Go language server")
    ("clangd" . "C/C++ language server")
    ("rust-analyzer" . "Rust language server")
    ("pylsp" . "Python language server")
    ("pyright" . "Python type checker")
    ("typescript-language-server" . "TypeScript/JavaScript language server"))
  "List of language servers used by this configuration.")

(defun ltl/doctor-check-language-servers ()
  "Check if language servers are installed."
  (message "Checking language servers...")
  (let ((missing '())
        (found '()))
    (dolist (lsp ltl/language-servers)
      (if (executable-find (car lsp))
          (push lsp found)
        (push lsp missing)))
    (when found
      (message "✓ Found language servers:")
      (dolist (lsp found)
        (message "  - %s (%s)" (car lsp) (cdr lsp))))
    (when missing
      (message "ℹ Missing language servers (install as needed):")
      (dolist (lsp missing)
        (message "  - %s: %s" (car lsp) (cdr lsp))))
    t))

;;;
;;; Emacs Version Check
;;;

(defun ltl/doctor-check-emacs-version ()
  "Check if Emacs version meets requirements."
  (let ((required-version "29"))
    (if (version< emacs-version required-version)
        (progn
          (message "✗ Emacs version too old: %s (required: %s+)" 
                   emacs-version required-version)
          nil)
      (progn
        (message "✓ Emacs version: %s" emacs-version)
        (when (>= emacs-major-version 30)
          (message "  ℹ Using Emacs 30+ with native compilation support"))
        t))))

;;;
;;; Directory Structure Check
;;;

(defun ltl/doctor-check-directories ()
  "Check if required directories exist."
  (let ((dirs '("lisp" "etc" "var" "elpaca")))
    (message "Checking directory structure...")
    (dolist (dir dirs)
      (let ((path (expand-file-name dir user-emacs-directory)))
        (if (file-directory-p path)
            (message "✓ Directory exists: %s" dir)
          (message "⚠ Directory missing: %s (will be created)" dir))))
    t))

;;;
;;; Package Manager Check
;;;

(defun ltl/doctor-check-elpaca ()
  "Check if Elpaca is properly installed."
  (message "Checking Elpaca installation...")
  (if (featurep 'elpaca)
      (progn
        (message "✓ Elpaca is loaded")
        (message "  Installer version: %s" 
                 (if (boundp 'elpaca-installer-version)
                     elpaca-installer-version
                   "unknown"))
        t)
    (progn
      (message "✗ Elpaca is not loaded")
      nil)))

;;;
;;; Tree-sitter Check
;;;

(defun ltl/doctor-check-tree-sitter ()
  "Check if tree-sitter grammars are installed."
  (if (not (fboundp 'treesit-available-p))
      (progn
        (message "ℹ Tree-sitter not available in this Emacs build")
        t)
    (if (treesit-available-p)
        (progn
          (message "✓ Tree-sitter is available")
          (let ((langs '(go python rust c cpp typescript tsx javascript)))
            (dolist (lang langs)
              (if (treesit-language-available-p lang)
                  (message "  ✓ Grammar installed: %s" lang)
                (message "  ⚠ Grammar missing: %s" lang))))
          t)
      (progn
        (message "⚠ Tree-sitter not available")
        (message "  Run: M-x mp-setup-install-grammars")
        nil))))

;;;
;;; Main Doctor Command
;;;

(defun ltl/doctor ()
  "Run all configuration health checks."
  (interactive)
  (let ((results '())
        (start-time (current-time)))
    (message "")
    (message "═══════════════════════════════════════════════════")
    (message "  Emacs Configuration Health Check")
    (message "  %s" (format-time-string "%Y-%m-%d %H:%M:%S"))
    (message "═══════════════════════════════════════════════════")
    (message "")
    
    ;; Run all checks
    (push (cons "Emacs Version" (ltl/doctor-check-emacs-version)) results)
    (message "")
    (push (cons "Directories" (ltl/doctor-check-directories)) results)
    (message "")
    (push (cons "Elpaca" (ltl/doctor-check-elpaca)) results)
    (message "")
    (push (cons "Executables" (ltl/doctor-check-executables)) results)
    (message "")
    (push (cons "Fonts" (ltl/doctor-check-fonts)) results)
    (message "")
    (push (cons "Language Servers" (ltl/doctor-check-language-servers)) results)
    (message "")
    (push (cons "Tree-sitter" (ltl/doctor-check-tree-sitter)) results)
    
    ;; Summary
    (message "")
    (message "═══════════════════════════════════════════════════")
    (message "  Summary")
    (message "═══════════════════════════════════════════════════")
    (let ((passed 0)
          (failed 0))
      (dolist (result (reverse results))
        (if (cdr result)
            (progn
              (message "  ✓ %s" (car result))
              (setq passed (1+ passed)))
          (progn
            (message "  ✗ %s" (car result))
            (setq failed (1+ failed)))))
      (message "")
      (message "Checks passed: %d/%d" passed (+ passed failed))
      (message "Time elapsed: %.2f seconds" 
               (float-time (time-subtract (current-time) start-time)))
      (message "═══════════════════════════════════════════════════")
      (message "")
      
      (when (> failed 0)
        (message "⚠ Some checks failed. See messages above for details.")
        (message "  For help, see: README.md or ELPACA_GUIDE.md"))
      
      (when (= failed 0)
        (message "✓ All checks passed! Your configuration is healthy."))
      
      (= failed 0))))

;;;
;;; Quick Checks (non-interactive)
;;;

(defun ltl/doctor-quick-check ()
  "Perform quick health check and return status."
  (and (ltl/doctor-check-emacs-version)
       (ltl/doctor-check-elpaca)))

(provide 'init-doctor)
;;; init-doctor.el ends here
