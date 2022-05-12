;;; init-cc.el --- config C/C++
;;; Commentary:
;;; Code:

(use-package cc-mode
  ;; TODO: open these hooks.
  ;; Sadly I work with a dirty team, i will change the whole code base if
  ;; uncommend config below.
  ;; (before-save-hook . lsp-format-buffer)
  ;; (before-save-hook . lsp-organize-imports)
  :config
  ;; (define-key c-mode-base-map (kbd "M-/") 'ff-find-related-file)
  ;; Open a header file in C++ mode by defaults
  (add-auto-mode 'c++-mode "\\.h\\'"))

(use-package company-c-headers
  :config
  (add-to-list 'company-backends 'company-c-headers))

(use-package cpp-auto-include)

;; disassemble C/C++ code under cursor
(use-package disaster)

(use-package clang-format+
  :config
  (add-hook 'c-mode-common-hook #'clang-format+-mode))

(use-package gdb-mi
  :config
  (setq
   gdb-many-windows t
   gdb-show-main t))

(use-package semantic
  :config
  (add-hook 'c-mode-common-hook #'semantic-mode))
(use-package stickyfunc-enhance
  :config
  (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode))

(use-package cmake-mode
  :mode
  (("CMakeLists\\.txt\\'" . cmake-mode)
   ("\\.cmake\\'" . cmake-mode)))

;; C++20 highlighting
(use-package modern-cpp-font-lock
  :diminish nil
  :hook
  (c++-mode . modern-c++-font-lock-mode)
  (modern-c++-font-lock-mode
   .
   (lambda () (diminish
			   'modern-c++-font-lock-mode))))

;; google cpplint
(use-package flycheck-google-cpplint
  :config
  (with-eval-after-load 'flycheck
    (require 'flycheck-google-cpplint)
    ;; Add Google C++ Style checker.
    ;; In default, syntax checked by Clang and Cppcheck.
    (flycheck-add-next-checker 'c/c++-cppcheck
			       '(warning . c/c++-googlelint))))

;; google style, but with 4 space indent.
(defun google-set-c-style-with-4-indent ()
  "Set current buffer to google style, but with 4 space indent."
  (interactive)
  (make-local-variable 'c-tab-always-indent)
  (setq c-tab-always-indent t)
  (c-add-style "Google" google-c-style t)
  (setq tab-width 8
        c-indent-tabs-mode t
        c-indent-level 4
        c-basic-offset 4))

(use-package google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style-with-4-indent)

(provide 'init-cc)
;;; init-cc.el ends here
