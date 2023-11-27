;;; init-org.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; Org
;; Org Mode’s timestamps are sadly not aware of time zones, but we can
;; crudely approximate support by setting org-time-stamp-formats.
(use-package org
  :hook (org-mode . org-indent-mode)
  :config
  (setf org-src-preserve-indentation nil
        org-edit-src-content-indentation 0
        org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-fontify-quote-and-verse-blocks t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 2
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-startup-folded 'content
        org-cycle-separator-lines 2)
        ;; make title look better
  (org-bullets-mode 1)
  (setq org-time-stamp-formats '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M %Z>"))
  (setf org-startup-folded 'show2levels)
  (setq org-modules
    '(org-crypt
        org-habit
        org-bookmark
        org-eshell
        org-irc))

  ;; This is needed as of Org 9.2
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
  (add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("go" . "src go"))
  (add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
  (add-to-list 'org-structure-template-alist '("json" . "src json")))

(require 'ox-publish)
(use-package toc-org)
(require 'org-id)

(use-package org-bullets
  :hook (org-mode . (lambda ()  org-bullets-mode 1)))

;; ox-hugo
;; We use ox-hugo for publishing.
;;
;; ltl/ox-hugo-update-lastmod can be used to update the timestamp of
;; the exported tree at the current point.
(use-package ox-hugo
  :after org
  :config
  (defun ltl/ox-hugo-update-lastmod ()
    "Updates the EXPORT_HUGO_LAST_MOD property of the nearest element
with EXPORT_FILE_NAME."
    (interactive)
    (save-excursion
      (when-let* ((elem (car (org-hugo--get-elem-with-prop :EXPORT_FILE_NAME)))
                  (begin (org-element-property :begin elem))
                  (time (format-time-string (org-time-stamp-format t) (current-time))))
        (org-entry-put begin "EXPORT_HUGO_LASTMOD" time)))))

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

;; Whitespace butler
(use-package ws-butler
  :hook (on-first-buffer . ws-butler-global-mode)
  :diminish)

(provide 'init-org)
;;; init-org.el ends here

