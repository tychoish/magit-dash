;;; test-magit-dash-which-key.el --- Which-key rendering regressions -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise rendered descriptions using which-key's actual replacement engine.

;;; Code:

(require 'ert)
(require 'seq)
(require 'magit-dash-global)

(defvar savehist-additional-variables)
(defvar which-key-replacement-alist)
(defvar which-key-allow-multiple-replacements)
(defvar which-key-mode-hook)
(declare-function which-key--maybe-replace "which-key")

(defvar magit-mode-hook)
(defvar magit-post-refresh-hook)
(defvar magit-post-worktree-add-hook)

(ert-deftest magit-dash-which-key-trims-all-dashboard-modes ()
  "Every dashboard major mode trims package prefixes from descriptions."
  (skip-unless (locate-library "which-key"))
  (require 'which-key)
  (let ((which-key-replacement-alist nil)
        (which-key-allow-multiple-replacements nil)
        (magit-dash-which-key-abbreviate-commands 'strip))
    (magit-dash--which-key-register-replacements)
    (seq-do
     (lambda (mode)
       (with-temp-buffer
         (setq major-mode mode)
         (should (equal '("m" . "menu")
                        (which-key--maybe-replace '("m" . "magit-dash-menu"))))
         (should (equal '("g" . "refresh")
                        (which-key--maybe-replace
                         '("g" . "magit-dash-pr-dashboard-refresh"))))
         (should (equal '("o" . "open")
                        (which-key--maybe-replace '("o" . "magit-dash-pr-open"))))
         (should (equal '("p" . "group:views")
                        (which-key--maybe-replace
                         '("p" . "group:magit-dash-pr-views"))))))
     '(magit-dash-mode magit-dash-actionable-mode
       magit-dash-pr-dashboard-mode magit-dash-stack-mode
       magit-dash-gh-actions-log-mode))))

(ert-deftest magit-dash-which-key-preserves-unrelated-modes-and-bindings ()
  "Package descriptions outside dashboard modes remain unchanged."
  (skip-unless (locate-library "which-key"))
  (require 'which-key)
  (let ((which-key-replacement-alist '(((nil . "other-command") . (nil . "other"))))
        (magit-dash-which-key-abbreviate-commands 'strip))
    (magit-dash--which-key-register-replacements)
    (with-temp-buffer
      (emacs-lisp-mode)
      (should (equal '("g" . "magit-dash-pr-dashboard-refresh")
                     (which-key--maybe-replace
                      '("g" . "magit-dash-pr-dashboard-refresh"))))
      (should (equal '("o" . "other")
                     (which-key--maybe-replace '("o" . "other-command")))))))

(ert-deftest magit-dash-which-key-respects-md-preference ()
  "The md preference abbreviates every supported namespace."
  (skip-unless (locate-library "which-key"))
  (require 'which-key)
  (let ((which-key-replacement-alist nil)
        (magit-dash-which-key-abbreviate-commands 'md)
        (major-mode 'magit-dash-pr-dashboard-mode))
    (magit-dash--which-key-register-replacements)
    (should (equal '("g" . "md-refresh")
                   (which-key--maybe-replace
                    '("g" . "magit-dash-pr-dashboard-refresh"))))))

(ert-deftest magit-dash-which-key-disabled-keeps-preferences ()
  "Disabling abbreviation leaves the existing replacement list intact."
  (skip-unless (locate-library "which-key"))
  (require 'which-key)
  (let* ((existing '(((nil . "user-command") . (nil . "user"))))
         (which-key-replacement-alist existing)
         (magit-dash-which-key-abbreviate-commands nil))
    (magit-dash--which-key-register-replacements)
    (should (eq existing which-key-replacement-alist))))

(ert-deftest magit-dash-which-key-global-mode-registers-immediately ()
  "Enabling global mode registers when which-key was enabled first."
  (skip-unless (locate-library "which-key"))
  (require 'which-key)
  (let ((magit-dash-global-mode nil)
        (magit-dash-global-managed-settings nil)
        (magit-dash-global--saved-settings nil)
        (which-key-mode t)
        (which-key-replacement-alist nil)
        (which-key-mode-hook nil)
        (magit-mode-hook nil)
        (magit-post-refresh-hook nil)
        (magit-post-worktree-add-hook nil)
        (after-load-functions nil)
        (savehist-additional-variables nil))
    (unwind-protect
        (progn
          (magit-dash-global-mode 1)
          (should (assq 'magit-dash-pr-dashboard-mode which-key-replacement-alist))
          (let ((once (copy-tree which-key-replacement-alist)))
            (magit-dash--which-key-register-replacements)
            (should (equal once which-key-replacement-alist))))
      (magit-dash-global-mode -1))))

(ert-deftest magit-dash-which-key-registers-on-later-enable ()
  "The mode hook registers replacements when which-key is enabled later."
  (skip-unless (locate-library "which-key"))
  (require 'which-key)
  (let ((which-key-replacement-alist nil)
        (which-key-mode-hook '(magit-dash--which-key-register-replacements))
        (magit-dash-which-key-abbreviate-commands 'strip))
    (run-hooks 'which-key-mode-hook)
    (should (assq 'magit-dash-pr-dashboard-mode which-key-replacement-alist))))

(provide 'test-magit-dash-which-key)
;;; test-magit-dash-which-key.el ends here
