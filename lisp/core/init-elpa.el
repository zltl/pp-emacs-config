;;; init-elpa.el --- Package manager configuration (Elpaca) -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This module configures the Elpaca package manager (v0.11).
;; Elpaca is a modern, async package manager for Emacs that:
;; - Supports multiple package sources (MELPA, GNU ELPA, GitHub, etc.)
;; - Provides fast parallel package installation
;; - Integrates seamlessly with use-package
;; - Offers a powerful UI for package management
;;
;; For more details, see: https://github.com/progfolio/elpaca
;;
;;; Code:

;;;
;;; Packages, .emacs.d folders
;;;


;;; elpaca

(defvar elpaca-installer-version 0.12)
;; Keep all Elpaca state under ~/.emacs.d/elpaca so package source,
;; builds, and generated autoloads stay isolated from the rest of the
;; config.
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  ;; Prefer the built directory when available so startup uses compiled
  ;; package code; fall back to the source tree during first bootstrap.
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    ;; `subr-x' backfills convenience macros/functions for older Emacs
    ;; releases so the bootstrap expression keeps working across versions.
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ;; Load Elpaca immediately so it can generate its own
                  ;; autoloads and finish bootstrapping in one pass.
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  ;; Prefer generated autoloads when present because they make package
  ;; activation cheaper on subsequent starts.
  (unless (require 'elpaca-autoloads nil t)
    ;; Fall back to loading Elpaca directly if autoloads have not been
    ;; generated yet, then generate them for the next startup.
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
;; Process any queued package operations after init so startup can remain
;; responsive while Elpaca handles installs/updates asynchronously.
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca (elpaca-use-package :wait t)
  ;; Enable use-package :ensure support for Elpaca.
  ;; `require' here wires `use-package' into Elpaca so every `use-package`
  ;; form in the rest of the config can install packages declaratively.
  (require 'elpaca-use-package)
  (elpaca-use-package-mode))
;; Make package declarations terse: most modules can just say
;; `(use-package foo)' and let Elpaca ensure installation automatically.
(setq use-package-always-ensure t)

;; Windows can have trouble with symlink-heavy package layouts; disabling
;; symlinks trades a bit of disk usage for a more reliable package tree.
(and sys/win32p
     (elpaca-no-symlink-mode))


;;; Security
;; For the love of all that is holy, do not continue with untrusted
;; connections!
;; `gnutls' is built in, but declaring it here makes the security policy
;; explicit: fail loudly on TLS verification errors instead of silently
;; accepting bad certificates.
(use-package gnutls
  :ensure nil
  :custom
  (gnutls-verify-error t))

;; ;; utility hooks and functions from Doom Emacs
;; `on' provides readable hook/event helpers used by several modules to
;; express lazy loading conditions more clearly.
(use-package on
  :ensure (on :repo "ajgrf/on.el" :host github))
;; We also want to “diminish” most minor-mode indicators on the mode
;; line. They’re only interesting if they’re in an unexpected state.
;; `diminish' keeps the mode line concise so signal-heavy status items
;; stay visible instead of being crowded out by minor mode names.
(use-package diminish)

;;; Benchmark
;; benchmark-init is a simple package that may or may not carry its
;; weight versus usepackage-compute-statistics. Run
;; benchmark-init/show-durations-tabulated to check this one out.
;; Deferred — activate manually with M-x benchmark-init/activate when profiling.
;; Keeping it deferred means there is zero overhead during normal startup,
;; but profiling is one command away when init performance regresses.
(use-package benchmark-init
  :defer t
  :commands (benchmark-init/activate
             benchmark-init/show-durations-tabulated
             benchmark-init/show-durations-tree))


;; This Emacs library provides a global mode which displays ugly form
;; feed characters as tidy horizontal rules.
;;
;; I use ^L to break sections on lisp
;; Enabling it globally makes section separators readable in code and docs
;; without changing the underlying file contents.
(use-package page-break-lines
  :ensure (page-break-lines :host github :repo "purcell/page-break-lines")
  :diminish
  :config
  (global-page-break-lines-mode))

(provide 'init-elpa)
;;; init-elpa.el ends here
