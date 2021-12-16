;;; init-cc.el --- config C/C++
;;; Commentary:
;;; Code:

(use-package cc-mode
  :hook
  (c-mode . lsp)
  (c++-mode . lsp)
  ;; TODO: open these hooks.
  ;; Sadly I work with a dirty team, i will change the whole code base if open
  ;; codes below.
  ;; (before-save-hook . lsp-format-buffer)
  ;; (before-save-hook . lsp-organize-imports)
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

;; google cpplint
(use-package flycheck-google-cpplint
  :config
  (eval-after-load 'flycheck
  '(progn
     (require 'flycheck-google-cpplint)
     ;; Add Google C++ Style checker.
     ;; In default, syntax checked by Clang and Cppcheck.
     (flycheck-add-next-checker 'c/c++-cppcheck
                                '(warning . c/c++-googlelint)))))

;; google style, but with 4 space indent.
(defun google-set-c-style-with-4-indent ()
  "Set current buffer to google style, but with 4 space indent."
  (interactive)
  (make-local-variable 'c-tab-always-indent)
  (setq c-tab-always-indent t)
  (c-add-style "Google" google-c-style t)
  (setq tab-width 4
        c-indent-tabs-mode t
        c-indent-level 4
        c-basic-offset 4))

(use-package google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style-with-4-indent)


(provide 'init-cc)
;;; init-cc.el ends here
