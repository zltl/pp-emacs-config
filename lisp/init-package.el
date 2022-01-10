;;; init-package.el --- Settings for package.el, and use-package
;;; Commentary:
;;; Code:

(require 'package)

(require 'init-cachedir)
;; Install into separate package dirs for each Emacs version, to prevent
;; bytecode incompatibility
(let ((versioned-package-dir
       (expand-file-name (format "elpa-%s.%s" emacs-major-version
				 emacs-minor-version)
			 my-cache-dir)))
  (setq package-user-dir versioned-package-dir))

;; Fast mirror for Chinese mainland
(setq package-archives
      '(("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
	("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
	("org-cn" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

(unless (file-exists-p (expand-file-name "elpa" user-emacs-directory))
  (package-refresh-contents))

(setq package-enable-at-startup nil)
(package-initialize)

;; Initialize use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
;; this i only needed once
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;; hiding mode line diplay of minor-modes
(use-package diminish)

;; load packages cloned into vendor/...
(defun add-subdirs-to-load-path (parent-dir)
  "Add every non-hidden subdir of PARENT-DIR to `load-path'."
  (let ((default-directory parent-dir))
    (setq load-path
          (append
           (cl-remove-if-not
            #'file-directory-p
            (directory-files (expand-file-name parent-dir) t "^[^\\.]"))
           load-path))))
(add-subdirs-to-load-path
 (expand-file-name "vendor/" user-emacs-directory))

;; Handier way to add modes to auto-mode-alist
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(provide 'init-package)
;;; init-package.el ends here
