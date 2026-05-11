;;; init.el --- Initialization file -*- no-byte-compile: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose
;; startup issues
;;(setq debug-on-error t)

;; Avoid garbage collection during startup.
;; (setq gc-cons-threshold 402653184 gc-cons-percentage 0.6)

(let ((minver "29"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

;; Always load newest byte code
;; This makes Emacs prefer newer .el over stale .elc, so edits take
;; effect immediately and we avoid chasing "why is the old behavior
;; still here?" problems after a config change.
(setq load-prefer-newer t)

;; preload the settings from `lisp'
;; Keep all hand-written modules under a dedicated lisp/ tree so init.el
;; stays focused on bootstrap and module ordering.
(defvar ltl/lisp-path (expand-file-name "lisp" user-emacs-directory))
;; Store Customize output separately so hand-written config is easier to
;; review and merge.
(defvar ltl/custom-file (expand-file-name "custom.el" user-emacs-directory))
;; Reuse no-littering-friendly directories so caches and generated files
;; do not spread across the repo root.
(defvar ltl/etc-directory (expand-file-name "etc/" user-emacs-directory))
(defvar ltl/var-directory (expand-file-name "var/" user-emacs-directory))
(defvar sys/user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))
(message "Emacs is powering up... Be patient, Master %s!" sys/user)

(when (file-exists-p  ltl/lisp-path)
  (message "Loading configuration files in %s..." ltl/lisp-path)
  (add-to-list 'load-path ltl/lisp-path)
  ;; Add subdirectories to load-path so `require' can find modules by
  ;; feature name instead of using brittle relative file paths.
  (add-to-list 'load-path (expand-file-name "core" ltl/lisp-path))
  (add-to-list 'load-path (expand-file-name "ui" ltl/lisp-path))
  (add-to-list 'load-path (expand-file-name "editing" ltl/lisp-path))
  (add-to-list 'load-path (expand-file-name "tools" ltl/lisp-path))
  (add-to-list 'load-path (expand-file-name "lang" ltl/lisp-path))
  (add-to-list 'load-path (expand-file-name "apps" ltl/lisp-path)))

;; I'll add an extra note here since user customizations are important.
;; Emacs actually offers a UI-based customization menu, "M-x customize".
;; You can use this menu to change variable values across Emacs. By default,
;; changing a variable will write to your init.el automatically, mixing
;; your hand-written Emacs Lisp with automatically-generated Lisp from the
;; customize menu. The following setting instead writes customizations to a
;; separate file, custom.el, to keep your init.el clean.
(when (and ltl/custom-file
           (file-exists-p ltl/custom-file))
  (message "Loading custom file %s..." ltl/custom-file)
  (load ltl/custom-file))



;; Load core constants and package manager first
;; `init-const' centralizes OS/version predicates and shared constants,
;; which keeps later modules declarative and avoids repeating the same
;; environment checks everywhere.
(require 'init-const)
;; `init-custom-vars' defines user-facing defcustom options early so
;; later modules can read those settings during initialization.
(require 'init-custom-vars)

;; compilations, enhance elisp.
;; `bytecomp' exposes compiler helpers and warnings used by modern Emacs
;; Lisp code; loading it early gives later modules consistent compiler
;; behavior.
(require 'bytecomp)

;; `init-elpa' bootstraps Elpaca and `use-package', so every package
;; declaration below can rely on automatic install/lazy-load behavior.
(require 'init-elpa)
;; Only surface real problems during startup; otherwise optional package
;; chatter tends to drown out actionable errors in *Messages*.
(setq warning-minimum-level :error)

(defun ltl/ensure-dir (pathname)
  "Ensure PATHNAME exists."
  (or (file-directory-p pathname)
      (make-directory pathname t))
  pathname)

(defun ltl/safe-require (module)
  "Safely load MODULE with error handling and logging."
  (condition-case err
      (progn
        (require module)
        (message "✓ Loaded %s" module)
        t)
    (error
     (message "✗ Failed to load %s: %s" module (error-message-string err))
     nil)))

;; Utility libraries used across the config.
;; `async' lets commands or helpers offload work so the UI stays
;; responsive during heavy tasks.
(use-package async
  :commands (async-start))
;; `dash' adds list combinators that make non-trivial Elisp pipelines
;; shorter and easier to read when a module needs them.
(use-package dash)
;; `s' provides practical string helpers so later modules do not need to
;; reimplement trimming, joining, or formatting glue code.
(use-package s)

;;; No littering
;; Many packages leave crumbs in user-emacs-directory or even
;; $HOME. Finding and configuring them individually is a hassle, so we
;; rely on the community configuration of no-littering. Run this
;; early, because many of the crumb droppers are configured below!
(use-package no-littering
  :init
  (setq no-littering-etc-directory ltl/etc-directory
	no-littering-var-directory ltl/var-directory))

;; gc
;; `gcmh' raises the GC threshold while you are actively typing and runs
;; collection during idle time, which reduces random pauses in normal
;; editing without giving up memory cleanup entirely.
(use-package gcmh
  :diminish
  :config
  (gcmh-mode)
  :custom
  ;; A higher threshold means fewer collections during bursts of LSP,
  ;; minibuffer, or package-manager activity.
  (gcmh-high-cons-threshold 33554432)
  ;; :custom
  ;; Set the delay to 20s instead of the default 15. I tried using `auto', but
  ;; with the default 20 of `gcmh-auto-idle-delay-factor', it triggers GC each
  ;; 1s on my machine. Setting the factor to a higher value should solve the
  ;; issue on my machine, but I don't think it is right to assume it will work
  ;; the same way on other machines. So we switch back to a fixed delay of 20s.
  (gcmh-idle-delay 20))

;; warn when opening files bigger than 100MB
;; Large files can make Emacs appear frozen; warn first so the user can
;; choose a more appropriate tool when needed.
(setq large-file-warning-threshold 100000000)

;; Increase the amount of data which Emacs reads from the process.
;; The emacs default is too low (4k) considering that some of the
;; language server responses are in 800k - 3M range.
;; Value is controlled by ltl/read-process-output-max in init-custom-vars.el
;; Raising this avoids truncation and reduces the number of round-trips
;; needed for LSP responses, which noticeably helps Eglot on large
;; projects.
(setq read-process-output-max (or (bound-and-true-p ltl/read-process-output-max)
                                   (* 1024 1024 4)))



;; `scratch' creates persistent named scratch buffers, which is handy
;; for temporary notes/snippets without polluting real project files.
(use-package scratch
  :ensure (scratch :type git :protocol https
                   :host github :repo "ffevotte/scratch.el"
                   :inherit nil))
;; Show event history and command history of some or all buffers.
;; (use-package command-log-mode)
;; load PATH from shell
;; Graphical Emacs does not always inherit the same PATH as an interactive
;; shell. Importing it avoids "tool not found" surprises for Git, LSP
;; servers, formatters, and language runtimes.
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (when (daemonp)
    (exec-path-from-shell-initialize)))

;; Load configuration modules with error handling
(message "Loading configuration modules...")

;; Core modules (init-const and init-custom-vars already loaded above)
;; `init-doctor' provides a diagnostic entry point so startup can report
;; environment problems in a user-friendly way later on.
(ltl/safe-require 'init-doctor)

;; UI modules
;; Theme and startup-screen choices are loaded early so the first frame
;; already reflects the intended visual style.
(ltl/safe-require 'init-themes)
(ltl/safe-require 'init-dashboard)

;; Editing modules
;; Editing behavior comes before tools/languages so every major mode gets
;; the same base ergonomics for selection, completion, and text editing.
(ltl/safe-require 'init-editor)
(ltl/safe-require 'init-text)
(ltl/safe-require 'init-complete)

;; Tool modules
;; Tooling modules wire in global capabilities such as keys, file/project
;; navigation, Git, and search.
(ltl/safe-require 'init-bindings)
(ltl/safe-require 'init-files)
(ltl/safe-require 'init-git)
(ltl/safe-require 'init-search)

;; Programming modules
;; Programming support is split by concern so core infra (LSP,
;; tree-sitter, formatting) loads before language-specific tweaks.
(ltl/safe-require 'init-programming-core)
(ltl/safe-require 'init-programming-systems)
(ltl/safe-require 'init-programming-web)
(ltl/safe-require 'init-programming-scripting)
(ltl/safe-require 'init-programming-misc)

;; Application modules
;; These modules add workflows beyond plain editing: notes, shell, AI,
;; RSS, and small standalone utilities.
(ltl/safe-require 'init-org)
(ltl/safe-require 'init-shell)
;; (ltl/safe-require 'init-copilot)  ; Editor Copilot — disabled in favor of init-copilot-cli
(ltl/safe-require 'init-copilot-cli)
(ltl/safe-require 'init-ai)
(ltl/safe-require 'init-elfeed)
(ltl/safe-require 'init-applications)


;;; Report loading status
(message "")
(message "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
(message "✓ Emacs configuration loaded successfully!")
(message "  Run M-x ltl/doctor to check your configuration health")
(message "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
(message "")

(provide 'init)
;;; init.el ends here
