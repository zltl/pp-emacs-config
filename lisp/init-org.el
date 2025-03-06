;;; init-org.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; Org
(use-package org
  :custom
  (org-format-latex-options
   '(:foreground default
                 :background default
                 :scale 2.0
                 :html-foreground "Black"
                 :html-background "Transparent"
                 :html-scale 2.0
                 :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))))

;; get rid of the extra white space that’s added inside code blocks.
(setq org-src-preserve-indentation t)
;; (setq org-preview-latex-default-process 'dvisvgm) ;No blur when scaling
(defun my/text-scale-adjust-latex-previews ()
  "Adjust the size of latex preview fragments when changing the
buffer's text scale."
  (pcase major-mode
    ('latex-mode
     (dolist (ov (overlays-in (point-min) (point-max)))
       (if (eq (overlay-get ov 'category)
               'preview-overlay)
           (my/text-scale--resize-fragment ov))))
    ('org-mode
     (dolist (ov (overlays-in (point-min) (point-max)))
       (if (eq (overlay-get ov 'org-overlay-type)
               'org-latex-overlay)
           (my/text-scale--resize-fragment ov))))))

(defun my/text-scale--resize-fragment (ov)
  (overlay-put
   ov 'display
   (cons 'image
         (plist-put
          (cdr (overlay-get ov 'display))
          :scale (+ 1.0 (* 1.25 text-scale-mode-amount))))))

(add-hook 'text-scale-mode-hook #'my/text-scale-adjust-latex-previews)


(use-package org-contrib
  :after org
  :demand t)

(use-package engrave-faces
  :after org)

;; Org export
(use-package ox-hugo
  :after ox
  :demand t)


;; (use-package ox-extra
;;   :after ox
;;   :demand t
;;   :config
;;   (ox-extras-activate '(latex-header-blocks ignore-headlines)))

;; Other Org features
(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-inside-latex t)
  (org-appear-autokeywords t)
  (org-appear-autoentities t)
  (org-appear-autoemphasis t)
  (org-appear-autosubmarkers t)
  (org-appear-autolinks 'just-brackets)
  :config
  ;; For proper first-time setup, `org-appear--set-elements' needs to be run after other hooks have acted.
  (run-at-time nil nil #'org-appear--set-elements))

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :hook (org-agenda-finalize . org-modern-agenda)
  :custom-face
  ;; Force monospaced font for tags
  (org-modern-tag ((t (:inherit org-verbatim :weight regular :foreground "black" :background "LightGray" :box "black"))))
  :custom
  ;; (org-modern-star '("◉" "○" "◈" "◇" "✳" "◆" "✸" "▶"))
  (org-modern-table-vertical 5)
  (org-modern-table-horizontal 2)
  (org-modern-list '((?+ . "➤") (?- . "–") (?* . "•")))
  (org-modern-block-fringe nil)
  (org-modern-checkbox nil) ;; Not that interesting! Maybe it depends on the used font
  (org-modern-todo-faces
   ;; Tweak colors, and force it to be monospaced, useful when using `mixed-pitch-mode'.
   '(("IDEA" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "goldenrod"))
     ("NEXT" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "IndianRed1"))
     ("STRT" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "OrangeRed"))
     ("WAIT" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "coral"))
     ("KILL" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "DarkGreen"))
     ("PROJ" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "LimeGreen"))
     ("HOLD" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "orange"))
     ("DONE" . (:inherit org-verbatim :weight semi-bold :foreground "black" :background "LightGray")))))

;; For latex fragments
(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode)
  :custom
  (org-fragtog-preview-delay 0.2))

(use-package org-re-reveal
  :ensure t)

(use-package oer-reveal
  :config (oer-reveal-setup-submodules))

(require 'org)
(require 'ox-publish)
(use-package toc-org)
(require 'org-id)

(setq org-startup-folded 'show2levels)
(add-hook 'org-mode-hook #'org-indent-mode)


;; `org-mode' is great but Denote makes it even better by adding
;; features that you'd find in something like Obsidian (like
;; backlinks!). You can write your notes in org, markdown, or plain
;; text, though I recommend giving `org-mode' a try if you've never
;; used it before. The Denote manual is also excellent:
;; https://protesilaos.com/emacs/denote
(use-package denote
  :custom
  (denote-known-keywords '("emacs" "journal"))
  ;; This is the directory where your notes live.
  (denote-directory (expand-file-name "~/denote/"))
  :bind
  (("C-c n n" . denote)
   ("C-c n f" . denote-open-or-create)
   ("C-c n i" . denote-link)))

(provide 'init-org)
;;; init-org.el ends here

