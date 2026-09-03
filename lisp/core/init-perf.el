;;; init-perf.el --- Runtime responsiveness -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Emacs Lisp runs on a single command loop. Native threads exist, but
;; they share a global lock and cannot make typing or redisplay faster.
;; Responsiveness comes from doing less work on that loop, skipping
;; redisplay while keys are pending, and letting subprocesses (LSP,
;; formatters, Git) run out of process.
;;
;; This module holds the low-level editor/runtime knobs. Package-level
;; decisions (when to start Eglot, Flycheck, Corfu, etc.) live in the
;; modules that own those packages.
;;
;;; Code:

;; Bidirectional text's bracket algorithm is expensive on long lines and
;; rarely useful for source code. Paragraph direction stays LTR so CJK
;; still renders, without paying the full bidi scan on every redisplay.
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Font cache compaction can pause Emacs for hundreds of milliseconds
;; after using many faces (themes, icons, mixed scripts).
(setq inhibit-compacting-font-caches t)

;; If the user is still typing, skip fontification/redisplay work that
;; would immediately become stale. This is the single most effective
;; built-in way to keep the command loop feeling live.
(setq redisplay-skip-fontification-on-input t
      fast-but-imprecise-scrolling t
      auto-window-vscroll nil
      scroll-conservatively 101
      scroll-margin 0
      scroll-step 1)

;; Fontify the visible window first; finish the rest while idle so
;; opening or paging a large file does not hitch the next keystroke.
(setq jit-lock-defer-time 0
      jit-lock-stealth-time 1.25
      jit-lock-stealth-nice 0.5
      jit-lock-stealth-load 200
      jit-lock-chunk-size 1024)

;; LSP and other chatty subprocesses already raise `read-process-output-max'
;; in init.el. Disabling adaptive buffering lets Emacs drain those pipes
;; in larger chunks instead of waking the command loop for every 4K.
(setq process-adaptive-read-buffering nil)

;; Remote / VC file operations should not block the first visit for long.
(setq remote-file-name-inhibit-locks t
      vc-handled-backends '(Git)
      auto-mode-case-fold nil)

;; `so-long' disables expensive minor modes in minified, generated, or
;; single-line files that would otherwise lock the UI.
(use-package so-long
  :ensure nil
  :hook (after-init . global-so-long-mode)
  :custom
  (so-long-threshold 1000)
  (so-long-max-lines 500))

(defun ltl/profile-cpu (&optional seconds)
  "Profile CPU for SECONDS (default 8) and show the report.
Use this while reproducing a hitch: type, scroll, or wait for the
freeze, then read the report to see which functions owned the loop."
  (interactive "P")
  (require 'profiler)
  (let ((secs (if (numberp seconds) seconds 8)))
    (profiler-reset)
    (profiler-start 'cpu)
    (run-with-timer
     secs nil
     (lambda ()
       (profiler-stop)
       (profiler-report)
       (message "CPU profile ready (sampled %ds)" secs)))
    (message "Profiling CPU for %ds — reproduce the hitch now..." secs)))

(defun ltl/profile-memory ()
  "Take a memory profile snapshot and show the report."
  (interactive)
  (require 'profiler)
  (profiler-reset)
  (profiler-start 'mem)
  (run-with-timer
   5 nil
   (lambda ()
     (profiler-stop)
     (profiler-report)
     (message "Memory profile ready")))
  (message "Profiling memory for 5s..."))

(provide 'init-perf)
;;; init-perf.el ends here
