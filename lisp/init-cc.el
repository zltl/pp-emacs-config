;;; init-cc.el --- config C/C++
;;; Commentary:
;;; Code:

(use-package cc-mode
  :hook
  (c-mode . lsp)
  (c++-mode . lsp)
  :config
  (define-key c-mode-base-map (kbd "M-/") 'ff-find-related-file))
                                        

;; Open a header file in C++ mode by defaults
(add-auto-mode 'c++-mode "\\.h\\'")

(use-package cmake-mode
  :init
  (setq auto-mode-alist
        (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                  ("\\.cmake\\'" . cmake-mode))
                auto-mode-alist)))

;; C++20 highlighting
(use-package modern-cpp-font-lock)

;; indentation
(setq-default indent-tabs-mode nil
      tab-width 4
      c-indent-tabs-mode t
      c-indent-level 4
      c-argdecl-indent 0
      c-tab-always-indent t
      backward-delete-function nil
      c-basic-offset 4)

(provide 'init-cc)
;;; init-cc.el ends here
