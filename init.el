;;; init.el --- Initialization file -*- no-byte-compile: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose
;; startup issues
;;(setq debug-on-error t)

;; Avoid garbage collection during startup.
;; (setq gc-cons-threshold 402653184 gc-cons-percentage 0.6)

(let ((minver "29"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

;; Always load newest byte code
(setq load-prefer-newer t)

;; preload the settings from `lisp'
(defvar ltl/lisp-path (expand-file-name "lisp" user-emacs-directory))
(defvar ltl/custom-file (expand-file-name "custom.el" user-emacs-directory))
(defvar ltl/etc-directory (expand-file-name "etc/" user-emacs-directory))
(defvar ltl/var-directory (expand-file-name "var/" user-emacs-directory))
(defvar sys/user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))
(message "Emacs is powering up... Be patient, Master %s!" sys/user)

(when (file-exists-p  ltl/lisp-path)
  (message "Loading configuration files in %s..." ltl/lisp-path)
  (add-to-list 'load-path ltl/lisp-path)
  ;; Add subdirectories to load-path
  (add-to-list 'load-path (expand-file-name "core" ltl/lisp-path))
  (add-to-list 'load-path (expand-file-name "ui" ltl/lisp-path))
  (add-to-list 'load-path (expand-file-name "editing" ltl/lisp-path))
  (add-to-list 'load-path (expand-file-name "tools" ltl/lisp-path))
  (add-to-list 'load-path (expand-file-name "lang" ltl/lisp-path))
  (add-to-list 'load-path (expand-file-name "apps" ltl/lisp-path)))

;; I'll add an extra note here since user customizations are important.
;; Emacs actually offers a UI-based customization menu, "M-x customize".
;; You can use this menu to change variable values across Emacs. By default,
;; changing a variable will write to your init.el automatically, mixing
;; your hand-written Emacs Lisp with automatically-generated Lisp from the
;; customize menu. The following setting instead writes customizations to a
;; separate file, custom.el, to keep your init.el clean.
(when (and ltl/custom-file
           (file-exists-p ltl/custom-file))
  (message "Loading custom file %s..." ltl/custom-file)
  (load ltl/custom-file))



;; Load core constants and package manager first
(require 'init-const)
(require 'init-custom-vars)

;; compilations, enhence elisp.
(require 'cl-lib)
(require 'subr-x)
(require 'bytecomp)

(require 'init-elpa)
(setq create-lockfiles nil)
(setq warning-minimum-level :error)

(defun ltl/ensure-dir (pathname)
  "Ensure PATHNAME exists."
  (or (file-directory-p pathname)
      (make-directory pathname t))
  pathname)

(defun ltl/safe-require (module)
  "Safely load MODULE with error handling and logging."
  (condition-case err
      (progn
        (require module)
        (message "✓ Loaded %s" module)
        t)
    (error
     (message "✗ Failed to load %s: %s" module (error-message-string err))
     nil)))

;; some usefull libraies
(use-package async
  :commands (async-start))
(use-package dash)
(use-package s)

;;; No littering
;; Many packages leave crumbs in user-emacs-directory or even
;; $HOME. Finding and configuring them individually is a hassle, so we
;; rely on the community configuration of no-littering. Run this
;; early, because many of the crumb droppers are configured below!
(use-package no-littering
  :init
  (setq no-littering-etc-directory ltl/etc-directory
	no-littering-var-directory ltl/var-directory))

;; gc
(use-package gcmh
  :diminish
  :config
  (gcmh-mode)
  :custom
  (gcmh-high-cons-threshold 33554432)
  ;; :custom
  ;; Set the delay to 20s instead of the default 15. I tried using `auto', but
  ;; with the default 20 of `gcmh-auto-idle-delay-factor', it triggers GC each
  ;; 1s on my machine. Setting the factor to a higher value should solve the
  ;; issue on my machine, but I don't think it is right to assume it will work
  ;; the same way on other machines. So we switch back to a fixed delay of 20s.
  (gcmh-idle-delay 20))

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; Increase the amount of data which Emacs reads from the process#
;; Again the emacs default is too low 4k considering that the some of

;; the language server responses are in 800k - 3M range.
(setq read-process-output-max (* 1024 1024 4))



(use-package scratch
  :ensure (scratch :host github :repo "ffevotte/scratch.el" :inherit nil))
;; Show event history and command history of some or all buffers.
;; (use-package command-log-mode)
;; load PATH from shell
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (when (daemonp)
    (exec-path-from-shell-initialize)))

;; Load configuration modules with error handling
(message "Loading configuration modules...")

;; Core modules (init-const and init-custom-vars already loaded above)
(ltl/safe-require 'init-doctor)

;; UI modules
(ltl/safe-require 'init-themes)
(ltl/safe-require 'init-dashboard)

;; Editing modules
(ltl/safe-require 'init-editor)
(ltl/safe-require 'init-text)
(ltl/safe-require 'init-complete)

;; Tool modules
(ltl/safe-require 'init-bindings)
(ltl/safe-require 'init-files)
(ltl/safe-require 'init-git)
(ltl/safe-require 'init-search)

;; Programming modules
(ltl/safe-require 'init-programing-core)
(ltl/safe-require 'init-programing-systems)
(ltl/safe-require 'init-programing-web)
(ltl/safe-require 'init-programing-scripting)
(ltl/safe-require 'init-programing-misc)

;; Application modules
(ltl/safe-require 'init-org)
(ltl/safe-require 'init-shell)
(ltl/safe-require 'init-copilot)
(ltl/safe-require 'init-copilot-cli)
(ltl/safe-require 'init-applications)

;;; Report loading status
(message "")
(message "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
(message "✓ Emacs configuration loaded successfully!")
(message "  Run M-x ltl/doctor to check your configuration health")
(message "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
(message "")

(provide 'init)
;;; init.el ends here

(provide 'init)
;;; init.el ends here


;;;
;;; Applications
;;;

;; Dictionary
;; The M-# keybinding is dubious because it’s not reserved, but it’s
;; good enough for Mickey Petersen.
(use-package dictionary
  :bind
  ("M-#" . dictionary-lookup-definition)
  :if (memq window-system '(mac ns x))
  :custom
  ;; Until I find a working dictd for MacOS on Nix, we’ll sigh heavily
  ;; and use dict.org.
  (dictionary-server "dict.org"))

;; UUID Generation
(use-package uuidgen
  :defer t)

;; rime
;; install librime/librime-dev
(use-package rime
  :custom
  (default-input-method "rime")
  (rime-show-candidate 'popup))

(require 'init-copilot)




(provide 'init)
;;; init.el ends here
