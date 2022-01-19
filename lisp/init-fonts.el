;;; init-fonts.el --- setting fonts
;;; Commentary:
;;; Code:

;; default
(let ((f "Source Code Pro"))
  (when (member f (font-family-list))
    (set-face-attribute 'default nil :font f)))

(when (eq system-type 'darwin)
  (setq fonts '("SF Mono" "冬青黑体简体中文"))
  (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend)
  (set-face-attribute 'default nil :font
                      (format "%s:pixelsize=%d" (car fonts) 14)))

(when (eq system-type 'windows-nt)
  (setq fonts '("Source Code Pro" "思源黑体"))
  (set-fontset-font t 'unicode "Segoe UI Emoji" nil 'prepend)
  (set-face-attribute 'default nil :font
                      (format "%s:pixelsize=%d" (car fonts) 20)))

(when (eq system-type 'gnu/linux)
  (setq fonts '("Source Code Pro" "思源黑体"))
  (set-fontset-font t 'unicode "Noto Color Emoji" nil 'prepend)
  (set-face-attribute 'default nil :font
                      (format "%s:pixelsize=%d" (car fonts) 20)))

(provide 'init-fonts)
;;; init-fonts.el ends here


