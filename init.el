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

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; I'll add an extra note here since user customizations are important.
;; Emacs actually offers a UI-based customization menu, "M-x customize".
;; You can use this menu to change variable values across Emacs. By default,
;; changing a variable will write to your init.el automatically, mixing
;; your hand-written Emacs Lisp with automatically-generated Lisp from the
;; customize menu. The following setting instead writes customizations to a
;; separate file, custom.el, to keep your init.el clean.
(setf custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
           (file-exists-p custom-file))
  (load custom-file nil :nomessage))



(require 'init-const)
(require 'init-elpa)


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

;; Increase the amount of data which Emacs reads from the process#
;; Again the emacs default is too low 4k considering that the some of
;; the language server responses are in 800k - 3M range.
(setq read-process-output-max (* 1024 1024 4))



(use-package scratch)
;; Show event history and command history of some or all buffers.
(use-package command-log-mode)
;; load PATH from shell
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (when (daemonp)
    (exec-path-from-shell-initialize)))

(require 'init-themes)
(require 'init-bindings)
(require 'init-files)
(require 'init-text)
(require 'init-org)
(require 'init-git)
(require 'init-editor)
(require 'init-complete)
(require 'init-search)
(require 'init-programing)


;;;
;;; Applications
;;;

;; Dictionary
;; The M-# keybinding is dubious because it’s not reserved, but it’s
;; good enough for Mickey Petersen.
(use-package dictionary
  :bind
  ("M-#" . dictionary-lookup-definition))
;; Until I find a working dictd for MacOS on Nix, we’ll sigh heavily
;; and use dict.org.
(use-package dictionary
  :if (memq window-system '(mac ns x))
  :custom
  (dictionary-server "dict.org"))

;; UUID Generation
(use-package uuidgen
  :defer t)



(require 'init-copilot)




(provide 'init)
;;; init.el ends here
