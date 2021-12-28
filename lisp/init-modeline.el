;;; package --- init-modeline.el - fancy doom-modeline setting
;;; Commentary:
;;; Code:

;;(defun my/font-installed-p (font-name)
;;  "Check if font with FONT-NAME is available."
;;  (if (find-font (font-spec :name font-name))
;;      t
;;    nil))

;; call all-the-icons-install-fonts manual.
(use-package all-the-icons)

;;(defun my/start-doom-modeline-if-fonts-exists ()
;;  "Start doom-modeline only in graphic window."
;;  (if (my/font-installed-p "all-the-icons")
;;	  (doom-modeline-mode 1)))

(use-package doom-modeline
  :hook
  (after-init . doom-modeline-mode))

(provide 'init-modeline)
;;; init-modeline.el ends here
