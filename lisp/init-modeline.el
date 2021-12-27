;;; package --- init-modeline.el - fancy doom-modeline setting
;;; Commentary:
;;; Code:

(defun my/font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (if (find-font (font-spec :name font-name))
      t
    nil))

(use-package all-the-icons
;;  :config
;; INSTAL FONT MANUAL!
;;  (when (and (eq system-type 'gnu/linux)
;;			 (not (my/font-installed-p "all-the-icons")))
;; 	(all-the-icons-install-fonts t))
  )

(defun my/start-doom-modeline-only-graphic ()
  "Start doom-modeline only in graphic window."
  (if (and (display-graphic-p)
		   (my/font-installed-p "all-the-icons"))
	  (doom-modeline-mode 1)))

(use-package doom-modeline
  :hook
  (after-init . my/start-doom-modeline-only-graphic))

(provide 'init-modeline)
;;; init-modeline.el ends here
