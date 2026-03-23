;;; init-git.el --- Git integration and version control -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This module configures Git integration using:
;; - Magit: The legendary Git porcelain for Emacs
;; - Git-gutter: Show Git diff in the fringe
;; - Git-timemachine: Browse historic versions of files
;; - Transient: Command interface framework (required by Magit)
;;
;; Magit is considered one of the best Git interfaces available,
;; even attracting users who don't otherwise use Emacs.
;;
;;; Code:

;; Magit
;; I have known people to leave Emacs, but continuing to use Magit for
;; version control. It's that good.
;;
;; I am giving built-ins the benefit of the doubt in this config, and
;; would like to get into vc-mode. But I'm an advanced enough Git user
;; that something tailor-made carries its weight here.

(use-package transient
  :defer t)


(use-package magit
  :functions ltl/magit-clone-read-args-a
  :after org
  :bind
  ("C-x g" . magit-status)
  :custom
  (magit-clone-default-directory "~/src/")
  (magit-no-message (list "Turning on magit-auto-revert-mode..."))
  (magit-save-repository-buffers 'dontask)
  :config
  (defun ltl/magit-clone-read-args-a (orig-fun &rest args)
    "Sets `vertico-preselect' to `prompt' when cloning repos, so we
clone to the default prompted directory, and not some random
existing directory under `magit-clone-default-directory'."
    (let ((vertico-preselect 'prompt))
      (apply orig-fun args)))
  (advice-add 'magit-clone-read-args :around #'ltl/magit-clone-read-args-a))
;; show todos
(use-package magit-todos
  :after magit
  :config (magit-todos-mode 1))

;; Forge - GitHub/GitLab integration for Magit
(use-package forge
  :after magit
  :defer t)

;; Git-Link
;; git-link grabs links to lines, regions, commits, or home pages.
(use-package git-link
  :custom
  (git-link-use-commit t)
  (git-link-use-single-line-number t)
  :commands (git-link git-link-commit git-link-homepage))

;; Ediff - Side-by-side diff (built-in, improved defaults)
(use-package ediff
  :ensure nil
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-merge-split-window-function 'split-window-horizontally))

;; diff-hl - Highlight uncommitted changes in the fringe
(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (text-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :config
  (diff-hl-flydiff-mode)
  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

(provide 'init-git)
