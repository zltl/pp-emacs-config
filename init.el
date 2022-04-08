;;; init.el --- Load the full configuration
;;; Commentary:
;;; Code:

(when (version< emacs-version "26.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

;; add ./lisp folder into load-path, so we can split the configure files
;; into this directory.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; from purcel/emacs.d/, Measure startup time
;; (require 'init-benchmarking)

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
(use-package lua-mode)

(require 'init-undo)

(require 'init-neotree)

(use-package page-break-lines)

(use-package dashboard
  :config
  (dashboard-setup-startup-hook))

(provide 'init)
;;; init.el ends here

