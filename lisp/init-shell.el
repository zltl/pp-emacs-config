;;; package --- terminal emulator
;;; Commentary:

;;; Code:

(use-package eat
  :ensure (eat :fetcher git
               :url "https://codeberg.org/akib/emacs-eat"
               :files ("*.el" ("term" "term/*.el") "*.texi"
                       "*.ti" ("terminfo/e" "terminfo/e/*")
                       ("terminfo/65" "terminfo/65/*")
                       ("integration" "integration/*")
                       (:exclude ".dir-locals.el" "*-tests.el")))
  :config
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  )


(provide 'init-shell)
;;; init-shell.el ends here
