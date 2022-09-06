;;; early-init.el -*- lexical-binding: t; -*-

;; Before Emacs 27, the init file was responsible for initializing the package
;; manager by calling `package-initialize'. Emacs 27 changed the default
;; behavior: It now calls `package-initialize' before loading the init file.
;; We want to own package initialization from running.
(setq package-enable-at-startup nil)

;;; early-init.el ends here
