;;; early-init.el --- Early Init File -*- no-byte-compile: t -*-

;; This file is loaded before the package manager and UI are initialized.
;; We use it for critical performance optimizations that must happen early.

;; Prevent package.el from initializing (we use Elpaca)
;; Skipping package.el avoids double-initialization, duplicate package
;; work, and startup time spent on a package manager this config does not use.
(setq package-enable-at-startup nil)

;; Startup performance: temporarily maximize GC threshold.
;; This will be reset by gcmh after init completes.
;; The effect is fewer garbage collections during bootstrap, which makes
;; first-frame startup noticeably smoother.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Startup performance: disable file-name-handler during init.
;; This speeds up file loading significantly.
;; File handlers are useful for TRAMP/compression, but they are also
;; consulted for every file access; temporarily disabling them trims a
;; lot of startup overhead.
(defvar ltl/--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist ltl/--file-name-handler-alist)))

;; Prevent UI elements from appearing and then disappearing (visual flash).
;; Setting these in default-frame-alist takes effect before the first frame.
;; Doing this here avoids the "startup flicker" where menus or scrollbars
;; are briefly visible and then removed by later UI modules.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Native compilation: suppress warnings buffer during normal use
;; Native-comp warnings are still available when needed, but hiding the
;; popup keeps routine startup from feeling noisy.
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors 'silent))

;; UX: Respect DEBUG envvar as an alternative to --debug-init
;; This is convenient when launching Emacs from shell scripts or desktop
;; launchers where changing the full command line is inconvenient.
(when (getenv-internal "DEBUG")
  (setq init-file-debug t
        debug-on-error t))




