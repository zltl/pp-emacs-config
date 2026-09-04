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

;; `transient' powers Magit's popup menus. Declaring it explicitly keeps
;; command loading predictable and makes the dependency obvious to readers.
(use-package transient
  :defer t)


(use-package magit
  :ensure (:host github :repo "magit/magit")
  :functions ltl/magit-clone-read-args-a
  :commands (magit-status magit-blame magit-log-current magit-log-buffer-file)
  :custom
  ;; Default clones into ~/src so repositories do not pile up in random
  ;; working directories chosen by the current buffer.
  (magit-clone-default-directory "~/src/")
  ;; Suppress a noisy status message that is informational but not useful
  ;; once auto-revert is part of the expected workflow.
  (magit-no-message (list "Turning on magit-auto-revert-mode..."))
  ;; Save dirty repository buffers automatically before Git operations so
  ;; status/diff views reflect the current buffer contents.
  (magit-save-repository-buffers 'dontask)
  :config
  (defun ltl/magit-clone-read-args-a (orig-fun &rest args)
    "Sets `vertico-preselect' to `prompt' when cloning repos, so we
clone to the default prompted directory, and not some random
existing directory under `magit-clone-default-directory'."
    (let ((vertico-preselect 'prompt))
      (apply orig-fun args)))
  (advice-add 'magit-clone-read-args :around #'ltl/magit-clone-read-args-a))

;; Bind the shared Git prefix to autoloads so Magit loads only when used.
(bind-keys :map ltl/git-map
  ("g" . magit-status)
  ("b" . magit-blame)
  ("l" . magit-log-current)
  ("f" . magit-log-buffer-file))
(bind-key "C-x g" #'magit-status)

;; Scan the repo for TODO/FIXME only when asked. Enabling
;; `magit-todos-mode' globally made every `magit-status' grep the tree.
(use-package magit-todos
  :commands magit-todos-list
  :bind (:map ltl/git-map
              ("o" . magit-todos-list)))

;; Forge - GitHub/GitLab integration for Magit
;; Requires magit >= 4.5; ensure latest magit is installed
;; Load this on demand because network-backed forge features are useful,
;; but not part of every Git interaction.
(use-package forge
  :after magit
  :ensure (:host github :repo "magit/forge")
  :defer t)

;; Git-Link
;; git-link grabs links to lines, regions, commits, or home pages.
;; Using commit-based URLs makes shared links stable even after branches
;; move, which is better for code review and issue discussions.
(use-package git-link
  :custom
  (git-link-use-commit t)
  ;; Single-line anchors generate cleaner links when you only need to
  ;; point at one exact location.
  (git-link-use-single-line-number t)
  :commands (git-link git-link-commit git-link-homepage)
  :bind
  (:map ltl/git-map
        ("k" . git-link)))

;; `git-timemachine' walks a file's Git history one revision at a time.
(use-package git-timemachine
  :defer t
  :commands git-timemachine
  :bind
  (:map ltl/git-map
        ("t" . git-timemachine)))

;; Ediff - Side-by-side diff (built-in, improved defaults)
;; Prefer the current frame and horizontal splits so diffs stay visible
;; without creating extra windows or swapping to a vertical layout.
(use-package ediff
  :ensure nil
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-merge-split-window-function 'split-window-horizontally))

;; diff-hl - Highlight uncommitted changes in the fringe
;; This gives constant, low-noise awareness of local edits right in the
;; fringe, which complements Magit instead of replacing it.
(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (text-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :config
  ;; Flydiff recomputes the hunk on every edit. Update on save instead;
  ;; Magit hooks below still refresh after Git operations.
  (diff-hl-flydiff-mode -1)
  ;; Newer diff-hl can run `git' off the command loop. Ignore the
  ;; variable on older builds.
  (when (boundp 'diff-hl-update-async)
    (setq diff-hl-update-async t))
  (with-eval-after-load 'magit
    ;; Refresh diff-hl around Magit updates so fringe markers do not lag
    ;; behind the repository state after staging/reverting.
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

(provide 'init-git)
