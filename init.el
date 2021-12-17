;;; init.el --- Load the full configuration
;;; Commentary:
;;; Code:

;; add ./lisp folder into load-path, so we can split the configure files
;; into this directory.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

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
(require 'init-cc)
(require 'init-python)
(require 'init-rust)

(require 'init-undo)

(require 'init-neotree)

(require 'init-keybind)

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (company-box eldoc-box company-quickhelp company-quick-help ace-window yasnippet-snippets xterm-color which-key use-package undo-tree spacemacs-theme smartparens rust-mode rainbow-delimiters pyenv-mode pip-requirements org-bullets neotree modern-cpp-font-lock magit lsp-ui ido-vertical-mode ibuffer-projectile helpful google-c-style go-mode flycheck-google-cpplint fill-column-indicator elpy diminish counsel-projectile company-jedi cmake-mode ccls anaconda-mode ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
