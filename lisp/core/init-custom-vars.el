;;; init-custom-vars.el --- User customizable variables -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains user-customizable variables for the Emacs configuration.
;; Users can customize these values using M-x customize-group RET ltl RET
;; or by setting them directly in their init file.

;;; Code:

(defgroup ltl nil
  "Personal Emacs configuration."
  :group 'emacs
  :prefix "ltl/")

;;; UI Settings

(defcustom ltl/default-font "Source Code Pro"
  "Default font for programming and text editing."
  :type 'string
  :group 'ltl)

(defcustom ltl/font-size 110
  "Default font size (height * 10).
For example, 110 means size 11, 120 means size 12."
  :type 'integer
  :group 'ltl)

(defcustom ltl/chinese-font "Droid Sans Fallback"
  "Font for displaying Chinese characters."
  :type 'string
  :group 'ltl)

(defcustom ltl/theme 'spacemacs-dark
  "Default color theme."
  :type 'symbol
  :group 'ltl
  :options '(spacemacs-dark spacemacs-light doom-one doom-vibrant))

;;; Feature Toggles

(defcustom ltl/enable-copilot t
  "Whether to enable GitHub Copilot.
Requires proper Copilot authentication."
  :type 'boolean
  :group 'ltl)

(defcustom ltl/enable-lsp t
  "Whether to enable LSP (Eglot) for programming languages.
Disabling this can improve performance on slower machines."
  :type 'boolean
  :group 'ltl)

(defcustom ltl/enable-tree-sitter t
  "Whether to enable tree-sitter syntax highlighting.
Requires Emacs 29+ with tree-sitter support."
  :type 'boolean
  :group 'ltl)

(defcustom ltl/enable-dashboard t
  "Whether to show the dashboard on startup."
  :type 'boolean
  :group 'ltl)

(defcustom ltl/enable-reveal nil
  "Whether to enable reveal.js support in Org mode.
Includes org-re-reveal and oer-reveal packages."
  :type 'boolean
  :group 'ltl)

;;; Programming Settings

(defcustom ltl/indent-level 4
  "Default indentation level for programming modes."
  :type 'integer
  :group 'ltl)

(defcustom ltl/web-indent-level 2
  "Indentation level for web development (JS, TS, HTML, CSS)."
  :type 'integer
  :group 'ltl)

(defcustom ltl/use-tabs nil
  "Whether to use tabs instead of spaces for indentation."
  :type 'boolean
  :group 'ltl)

(defcustom ltl/show-trailing-whitespace t
  "Whether to highlight trailing whitespace in programming modes."
  :type 'boolean
  :group 'ltl)

;;; LSP Settings

(defcustom ltl/lsp-language-servers
  '((go . "gopls")
    (c . "clangd")
    (cpp . "clangd")
    (python . "pyright")
    (rust . "rust-analyzer")
    (typescript . "typescript-language-server")
    (javascript . "typescript-language-server"))
  "Alist of language to language server mappings."
  :type '(alist :key-type symbol :value-type string)
  :group 'ltl)

(defcustom ltl/lsp-enable-snippets t
  "Whether to enable snippet completion in LSP."
  :type 'boolean
  :group 'ltl)

(defcustom ltl/lsp-enable-on-type-formatting nil
  "Whether to enable automatic formatting while typing.
This can be distracting for some users."
  :type 'boolean
  :group 'ltl)

;;; Org Mode Settings

(defcustom ltl/org-directory "~/org"
  "Directory for org files."
  :type 'directory
  :group 'ltl)

(defcustom ltl/org-agenda-files '("~/org/agenda.org")
  "List of files to include in org agenda."
  :type '(repeat file)
  :group 'ltl)

;;; Performance Settings

(defcustom ltl/gc-cons-threshold (* 100 1024 1024)
  "Garbage collection threshold during normal operation (in bytes).
Default is 100MB. Increase if you experience stuttering."
  :type 'integer
  :group 'ltl)

(defcustom ltl/read-process-output-max (* 1024 1024)
  "Maximum bytes to read from subprocess in a single chunk.
Default is 1MB. Increase for better LSP/Eglot performance."
  :type 'integer
  :group 'ltl)

;;; Backup and Auto-save Settings

(defcustom ltl/backup-directory (expand-file-name "backups/" user-emacs-directory)
  "Directory for backup files."
  :type 'directory
  :group 'ltl)

(defcustom ltl/auto-save-directory (expand-file-name "auto-saves/" user-emacs-directory)
  "Directory for auto-save files."
  :type 'directory
  :group 'ltl)

(defcustom ltl/backup-by-copying t
  "Whether to backup files by copying instead of renaming.
Safer but slower."
  :type 'boolean
  :group 'ltl)

;;; Git Settings

(defcustom ltl/magit-auto-revert t
  "Whether to automatically revert buffers when files change in Git."
  :type 'boolean
  :group 'ltl)

;;; Shell Settings

(defcustom ltl/default-shell 'eshell
  "Default shell to use in Emacs."
  :type '(choice (const :tag "Eshell" eshell)
                 (const :tag "Vterm" vterm)
                 (const :tag "Shell" shell)
                 (const :tag "Ansi-term" ansi-term))
  :group 'ltl)

;;; Helper Functions

(defun ltl/apply-custom-settings ()
  "Apply custom settings from ltl/custom variables.
This function should be called after init-custom-vars is loaded."
  (interactive)
  
  ;; Apply performance settings
  (setq gc-cons-threshold ltl/gc-cons-threshold)
  (setq read-process-output-max ltl/read-process-output-max)
  
  ;; Apply font settings (if GUI)
  (when (display-graphic-p)
    (set-face-attribute 'default nil
                        :family ltl/default-font
                        :height ltl/font-size))
  
  ;; Apply programming settings
  (setq-default indent-tabs-mode ltl/use-tabs)
  (setq-default tab-width ltl/indent-level)
  
  (message "✓ Applied custom settings from ltl/ variables"))

(defun ltl/reset-custom-settings ()
  "Reset custom settings to their default values.
This is useful for troubleshooting."
  (interactive)
  (when (yes-or-no-p "Reset all ltl/ custom variables to defaults? ")
    (dolist (symbol (apropos-internal "^ltl/"))
      (when (custom-variable-p symbol)
        (custom-reevaluate-setting symbol)))
    (message "✓ Reset all ltl/ custom variables to defaults")
    (ltl/apply-custom-settings)))

(provide 'init-custom-vars)
;;; init-custom-vars.el ends here
