;;; init-ido.el --- Interactively do things is a very convenient way to find
;;; files and switch buffers

;;; Commentary:

;;; Code:

(use-package ido
  :config
  (ido-mode 1)
  )

(use-package ido-vertical-mode
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  (setq ido-vertical-show-count t)
  (setq ido-use-faces t)
  (set-face-attribute 'ido-vertical-first-match-face nil
                      :background nil
                      :foreground "orange")
  (set-face-attribute 'ido-vertical-only-match-face nil
                      :background nil
                      :foreground nil)
  (set-face-attribute 'ido-vertical-match-face nil
                      :foreground nil))

(provide 'init-ido)
;;; init-ido.el ends here
