;;; init-elpa.el --- Package manager configuration (Elpaca) -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This module configures the Elpaca package manager (v0.11).
;; Elpaca is a modern, async package manager for Emacs that:
;; - Supports multiple package sources (MELPA, GNU ELPA, GitHub, etc.)
;; - Provides fast parallel package installation
;; - Integrates seamlessly with use-package
;; - Offers a powerful UI for package management
;;
;; For more details, see: https://github.com/progfolio/elpaca
;;
;;; Code:

;;;
;;; Packages, .emacs.d folders
;;;

;; Work-around for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
(when (and (version< emacs-version "26.3") (boundp 'libgnutls-version) (>= libgnutls-version 30604))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))


;;; elpaca

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))
(setq elpaca-use-package-by-default t)

(and sys/win32p
     (elpaca-no-symlink-mode))


;;; Security
;; For the love of all that is holy, do not continue with untrusted
;; connections!
(use-package gnutls
  :ensure nil
  :custom
  (gnutls-verify-error t))

;; ;; utility hooks and functions from Doom Emacs
(use-package on
  :ensure (on :repo "ajgrf/on.el" :host github))
;; We also want to “diminish” most minor-mode indicators on the mode
;; line. They’re only interesting if they’re in an unexpected state.
(use-package diminish)

;;; Benchmark
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
  :ensure (page-break-lines :host github :repo "purcell/page-break-lines")
  :diminish
  :config
  (global-page-break-lines-mode))

(provide 'init-elpa)
;;; init-elpa.el ends here
