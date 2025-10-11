;;; init-const.el --- Define constants and system detection -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This module defines constants and detects the operating system.
;; It provides variables for:
;; - System type detection (Linux, macOS, Windows, BSD)
;; - Emacs version checks
;; - Platform-specific configurations
;;
;; All system detection variables use the 'sys/' prefix for consistency.
;;
;;; Code:

;;; Operating System Detection
(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a Windows system?")

(defconst sys/bsdp
  (memq system-type '(berkeley-unix gnu/kfreebsd))
  "Are we running on a BSD system?")

(defconst sys/cygwinp
  (eq system-type 'cygwin)
  "Are we running on a Cygwin system?")

;;; System Architecture
(defconst sys/arch (intern (car (split-string system-configuration "-")))
  "The system's architecture read from `system-configuration'.
It return a symbol like `x86_64', `aarch64', `armhf', ...")

(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp)
  "Are we running under X on a Mac system?")

(defconst sys/mac-ns-p
  (eq window-system 'ns)
  "Are we running on a GNUstep or Macintosh Cocoa display?")

(defconst sys/mac-cocoa-p
  (featurep 'cocoa)
  "Are we running with Cocoa on a Mac system?")

(defconst sys/mac-port-p
  (eq window-system 'mac)
  "Are we running a macport build on a Mac system?")

(defconst sys/linux-x-p
  (and (display-graphic-p) sys/linuxp)
  "Are we running under X on a GNU/Linux system?")

;;; User Detection
(defconst sys/rootp
  (string-equal "root" (getenv "USER"))
  "Are you using ROOT user?")

(defconst emacs/>=27p
  (>= emacs-major-version 27)
  "Emacs is 27 or above.")

(defconst emacs/>=28p
  (>= emacs-major-version 28)
  "Emacs is 28 or above.")

(defconst emacs/>=29p
  (>= emacs-major-version 29)
  "Emacs is 29 or above.")

(defconst emacs/>=30p
  (>= emacs-major-version 30)
  "Emacs is 30 or above.")

(provide 'init-const)
;;; init-const.el ends here


