;;; init.el --- Load the full configuration
;;; Commentary:
;;; Code:

(when (version< emacs-version "26.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

;; Minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)
;; Lower threshold back to 32 MiB (default is 800kB)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 26))))
;; emacs default is too low 4k considering that the some of the language server
;; responses are in 800k - 3M range.
(setq read-process-output-max (* 1024 1024 3)) ;; 1mb

;; add ./lisp folder into load-path, so we can split the configure files
;; into this directory.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; from purcel/emacs.d/, Measure startup time
(require 'init-benchmarking)

(require 'init-cachedir)

(require 'init-coding)

(eval-when-compile
  ;; Import some common lisp functions and macros
  (require 'cl-lib)
  ;; subr-x has a ton of useful macros and functions, and it would be nice
  ;; if it were available to packages that support emacs versions down to
  ;; 24.1.
  (eval-when-compile (require 'subr-x)))

(require 'init-package)

(require 'init-nolittering)

(use-package benchmark-init
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; theme
(use-package spacemacs-theme
  :defer t
  :init (load-theme 'spacemacs-dark t))

;; feel free to forget shortkeys
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

(require 'init-fonts)
(require 'init-editor)
(require 'init-modeline)
(require 'init-recentf)
(require 'init-help)
(require 'eldoc)

(use-package multiple-cursors)

(require 'init-completion)
(require 'init-flycheck)
(require 'init-search)
;; find file, switch buffer
(require 'init-ido)

(require 'init-mmm)

;; Projectile is a really nifty package, that “teaches” Emacs the concept of
;; project.
(require 'init-projectile)

;; git
(use-package magit)

(require 'init-org)

(require 'init-lsp)
(require 'init-golang)
(require 'init-cc)
(require 'init-python)
(require 'init-rust)
(require 'init-protobuf)
(require 'init-yaml)
(require 'init-csv)
(require 'init-html)
(require 'init-js)
(require 'init-json)
(require 'init-lisp)
(require 'init-haskell)
(use-package lua-mode)

(require 'init-undo)

(require 'init-neotree)

(provide 'init)
;;; init.el ends here

