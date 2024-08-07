;;; early-init.el --- Early Init File -*- no-byte-compile: t -*-

;; Before Emacs 27, the init file was responsible for initializing the
;; package manager by calling `package-initialize`. Emacs 27 changed
;; the default behavior: It now calls `package-initialize` before
;; loading the init file. This behavior would prevent my own package
;; initialization from running. However, Emacs 27 also loads the
;; "early init" file (this file) before it initializes the packages
;; manager, and we use this early init file to prevent Emacs from
;; initializing the package manager. (See
;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=24acb31c04b4048b85311d794e600ecd7ce60d3b)
;;
;; Earlier Emacs version do not load the early init file and do not
;; initialize the package manager before loading the init file, so
;; this file is neither needed nor loaded on those versions.
(setq package-enable-at-startup nil)

;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setenv "LSP_USE_PLISTS" "true")

;; UX: Respect DEBUG envvar as an alternative to --debug-init, and to make are
;;   startup sufficiently verbose from this point on.
(when (getenv-internal "DEBUG")
  (setq init-file-debug t
        debug-on-error t))





