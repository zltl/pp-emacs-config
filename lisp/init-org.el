;;; init-org.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; Org

(use-package org-contrib
  :after org
  :demand t)

(use-package engrave-faces
  :after org)

;; Org export
(use-package ox-hugo
  :after ox
  :demand t)


(use-package ox-extra
  :after ox
  :demand t
  :config
  (ox-extras-activate '(latex-header-blocks ignore-headlines)))

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
  (org-modern-star '("◉" "○" "◈" "◇" "✳" "◆" "✸" "▶"))
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
  :straight t
  :hook (org-mode . org-fragtog-mode)
  :custom
  (org-fragtog-preview-delay 0.2))

(use-package org-re-reveal
  :straight t)

(use-package oer-reveal
  :straight t
  :hook (minemacs-build-functions . oer-reveal-setup-submodules))

(require 'org)
(require 'ox-publish)
(use-package toc-org)
(require 'org-id)

;; ox-slack
;; Mostly useful for org-slack-export-to-clipboard-as-slack.
(use-package ox-slack
  :after org
  :bind
  (:map org-mode-map
   :prefix-map ltl/org-mode-map
   :prefix "C-c m"
   ("w" . org-slack-export-to-clipboard-as-slack)))

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

