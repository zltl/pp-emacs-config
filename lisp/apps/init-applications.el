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
  :custom
  (default-input-method "rime")
  (rime-show-candidate 'popup)
  :config
  (when (and (boundp 'rime-librime-root)
             (not (file-exists-p rime-librime-root)))
    (message "⚠ Rime: librime not found. Install librime-dev to use Chinese input.")))

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
(use-package eww
  :ensure nil
  :defer t
  :custom
  (eww-search-prefix "https://www.google.com/search?q=")
  (eww-download-directory "~/Downloads/"))

;;;
;;; Calendar
;;;

(use-package calendar
  :ensure nil
  :defer t
  :custom
  (calendar-week-start-day 1) ; Week starts on Monday
  (calendar-date-style 'iso)) ; ISO 8601 date format

(provide 'init-applications)
;;; init-applications.el ends here
