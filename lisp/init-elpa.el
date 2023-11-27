;;; init-elpa.el --- Settings and helpers for package.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;;
;;; Packages, .emacs.d folders
;;;

;; For Chinese user, elpa may blocked by the Great-Fucking-Wirewall
(require 'package)
(setq package-archives '(("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

;; Work-around for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
(when (and (version< emacs-version "26.3") (boundp 'libgnutls-version) (>= libgnutls-version 30604))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; Unless we've already fetched (and cached) the package archives,
;; refresh them.
(package-initialize)
(package-refresh-contents)

(setq epg-gpg-program "gpg2")
(fset 'epg-wait-for-status 'ignore)
(setq auth-source-debug t)

;; emacs 29 make use-package build in
(require 'use-package)
(setq use-package-always-ensure t)
(use-package quelpa
  :init
  (setq quelpa-upgrade-p nil
        quelpa-update-melpa-p nil
        quelpa-checkout-melpa-p nil))
  
(quelpa
 '(quelpa-use-package
   :fetcher github-ssh
   :repo "quelpa/quelpa-use-package"))
(require 'quelpa-use-package)

;; (setq use-package-ensure-function 'quelpa)

;;; Security
;; For the love of all that is holy, do not continue with untrusted
;; connections!
(use-package gnutls
  :defer t
  :custom
  (gnutls-verify-error t))

;; utility hooks and functions from Doom Emacs
(use-package on
  :quelpa (on :repo "ajgrf/on.el" :fetcher github-ssh))
;; We also want to “diminish” most minor-mode indicators on the mode
;; line. They’re only interesting if they’re in an unexpected state.
(use-package diminish)
;; compilations, enhence elisp.
(require 'cl-lib)
(require 'subr-x)
(require 'bytecomp)

;; Benchmark
;; benchmark-init is a simple package that may or may not carry its
;; weight versus usepackage-compute-statistics. Run
;; benchmark-init/show-durations-tabulated to check this one out.
(use-package benchmark-init
  :demand t
  :hook (after-init . benchmark-init/deactivate)
  :config
  (benchmark-init/activate))

;; This Emacs library provides a global mode which displays ugly form
;; feed characters as tidy horizontal rules.
;;
;; I use ^L to break sections on lisp
(use-package page-break-lines
  :diminish
  :hook ((lisp-mode . page-break-lines-mode)
         (emacs-lisp-mode . page-break-lines-mode)))

;;; No littering
;; Many packages leave crumbs in user-emacs-directory or even
;; $HOME. Finding and configuring them individually is a hassle, so we
;; rely on the community configuration of no-littering. Run this
;; early, because many of the crumb droppers are configured below!
;; (use-package no-littering
;;   :init
;;   (setq no-littering-etc-directory "~/.cache/emacs/etc/"
;; 	no-littering-var-directory "~/.cache/emacs/var/"))
;; (use-package recentf)
;; (add-to-list 'recentf-exclude
;; 	     (recentf-expand-file-name no-littering-var-directory))
;; (add-to-list 'recentf-exclude
;; 	     (recentf-expand-file-name no-littering-etc-directory))

;; I get a bunch of asynchronous warnings from native compilation in a
;; *Warnings* popup. It’s nice that they’re there, but unless they’re
;; an error, I don’t need them all up in my business.
;; (use-package comp
;;   :custom
;;   (native-comp-async-report-warnings-errors 'silent))

(provide 'init-elpa)
;;; init-elpa.el ends here
