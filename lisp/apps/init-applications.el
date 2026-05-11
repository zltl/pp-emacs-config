;;; init-applications.el --- Miscellaneous applications -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configuration for standalone applications and utilities that don't
;; fit into other categories.
;;
;;; Code:

;;;
;;; Dictionary
;;;

;; Dictionary lookup using dict protocol
;; The M-# keybinding is dubious because it's not reserved, but it's
;; good enough for Mickey Petersen.
;; `dictionary' is handy for reading/writing without leaving the editor.
(use-package dictionary
  :bind
  ("M-#" . dictionary-lookup-definition)
  :if (memq window-system '(mac ns x))
  :custom
  ;; Until I find a working dictd for MacOS on Nix, we'll sigh heavily
  ;; and use dict.org.
  (dictionary-server "dict.org"))

;;;
;;; UUID Generation
;;;

;; `uuidgen' is tiny but practical for test data, request IDs, and schema work.
(use-package uuidgen
  :defer t
  :commands (uuidgen-1 uuidgen-4))

;;;
;;; Chinese Input Method (Rime)
;;;

;; Rime is a powerful input method engine for typing Chinese.
;; Requires: librime or librime-dev to be installed
;;
;; Installation:
;;   Gentoo: sudo emerge --ask app-i18n/librime
;;   Ubuntu: sudo apt install librime-dev
;;   macOS:  brew install librime
;;
;; Usage:
;;   C-\ - Toggle input method
;;   M-x rime-select-schema - Switch input schema
(use-package rime
  :defer t
  :custom
  ;; Make Rime the default IME so C-\ toggles directly into Chinese input.
  (default-input-method "rime")
  ;; Show candidates near point instead of in a detached UI.
  (rime-show-candidate 'popup))

;;;
;;; Time Tracking (Optional)
;;;

;; (use-package org-clock
;;   :ensure nil
;;   :commands (org-clock-in org-clock-out)
;;   :custom
;;   (org-clock-persist 'history)
;;   :config
;;   (org-clock-persistence-insinuate))

;;;
;;; PDF Tools (Optional)
;;;

;; Better PDF viewing in Emacs
;; (use-package pdf-tools
;;   :defer t
;;   :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
;;   :config
;;   (pdf-tools-install :no-query))

;;;
;;; EWW (Emacs Web Wowser)
;;;

;; Built-in web browser configuration
;; `eww' is sufficient for quick docs/search lookups where opening a full
;; external browser would be unnecessary context switching.
(use-package eww
  :ensure nil
  :defer t
  :custom
  ;; Reuse a familiar search engine for ad-hoc queries from inside Emacs.
  (eww-search-prefix "https://www.google.com/search?q=")
  ;; Put downloads in the standard user Downloads directory.
  (eww-download-directory "~/Downloads/"))

;;;
;;; Calendar
;;;

;; Built-in `calendar' mainly needs locale-style tweaks to match ISO dates
;; and Monday-first work weeks.
(use-package calendar
  :ensure nil
  :defer t
  :custom
  (calendar-week-start-day 1) ; Week starts on Monday
  (calendar-date-style 'iso)) ; ISO 8601 date format

;; auth-source-pass - Password store integration
;; Provides secrets to forge, gptel, and other packages via ~/.password-store
;; Enable one shared credential backend so auth-aware packages can reuse
;; pass entries instead of each storing secrets their own way.
(use-package auth-source-pass
  :ensure nil
  :defer t
  :config
  (auth-source-pass-enable))

(provide 'init-applications)
;;; init-applications.el ends here
