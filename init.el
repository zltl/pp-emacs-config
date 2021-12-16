;;; init.el --- Load the full configuration

;;; Code:

;; add ./lisp folder into load-path, so we can split the configure files
;; into this directory.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; dir to save temp data
(defvar my-cache-dir (concat user-emacs-directory "cache/"))
(unless (file-exists-p my-cache-dir)
    (make-directory my-cache-dir t))

(require 'init-faster)
(require 'init-coding)

;; subr-x has a ton of useful macros and functions, and it would be nice
;; if it were available to packages that support emacs versions down to
;; 24.1.
(require 'subr-x)

;; Import some common lisp functions and macros
(require 'cl-lib)

;; set custom.el as local configure, it should not commit to git repo.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'init-package)
;; hide eldoc-mode
(diminish 'eldoc-mode)

;; theme
(use-package spacemacs-theme
  :defer t
  :init (load-theme 'spacemacs-dark t))


(require 'init-editor)
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

(require 'init-undo)

(require 'init-neotree)

(require 'init-keybind)

;; load custom.el
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here
