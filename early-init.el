;;; early-init.el --- Early Init File -*- no-byte-compile: t -*-

;; This file is loaded before the package manager and UI are initialized.
;; We use it for critical performance optimizations that must happen early.

;; Prevent package.el from initializing (we use Elpaca)
(setq package-enable-at-startup nil)

;; Startup performance: temporarily maximize GC threshold.
;; This will be reset by gcmh after init completes.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Startup performance: disable file-name-handler during init.
;; This speeds up file loading significantly.
(defvar ltl/--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist ltl/--file-name-handler-alist)))

;; Prevent UI elements from appearing and then disappearing (visual flash).
;; Setting these in default-frame-alist takes effect before the first frame.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Native compilation: suppress warnings buffer during normal use
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors 'silent))

;; UX: Respect DEBUG envvar as an alternative to --debug-init
(when (getenv-internal "DEBUG")
  (setq init-file-debug t
        debug-on-error t))





