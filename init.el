;;; init.el --- Load the full configuration
;;; Commentary:
;;; Code:

;; add ./lisp folder into load-path, so we can split the configure files
;; into this directory.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq custom-file (locate-user-emacs-file "custom.el"))

(require 'init-cachedir)

(require 'init-coding)

;; subr-x has a ton of useful macros and functions, and it would be nice
;; if it were available to packages that support emacs versions down to
;; 24.1.
(require 'subr-x)
;; Import some common lisp functions and macros
(require 'cl-lib)

(require 'init-package)

;; theme
(use-package spacemacs-theme
  :defer t
  :init (load-theme 'spacemacs-dark t))

;; feel free to forget shortkeys
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

(require 'init-editor)
(require 'init-modeline)
(require 'init-recentf)
(require 'init-help)

(require 'init-completion)
(require 'init-flycheck)
(require 'init-search)
;; find file, switch buffer
(require 'init-ido)

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

(require 'init-docker)

(require 'init-undo)

(require 'init-neotree)

;; Variables configured via the interactive 'customize' interface
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here

