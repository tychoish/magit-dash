;;; test-helper.el --- ERT test infrastructure for magit-dash -*- lexical-binding: t -*-

;;; Commentary:
;; Loaded by ert-runner before any test files.
;; Adds the magit-dash repo root to load-path so test files can require
;; local modules.  Cask manages all external dependencies.

;;; Code:

(let* ((test-file (or load-file-name buffer-file-name))
       (test-dir (file-name-directory test-file))
       (root (file-name-directory (directory-file-name test-dir))))
  (add-to-list 'load-path root))

(require 'magit-dash)

(defmacro magit-dash-test--with-refresh-stubs (&rest body)
  "Execute BODY with refresh-infrastructure functions stubbed out.
Stubs discover-worktrees, discover-submodules, populate-stats-async,
tabulated-list-print, and tabulated-list-init-header so tests can call
`magit-dash-refresh' without a live Emacs dashboard buffer or real repos."
  `(cl-letf (((symbol-function 'magit-dash--discover-worktrees) (lambda () nil))
             ((symbol-function 'magit-dash--discover-submodules) (lambda () nil))
             ((symbol-function 'magit-dash--populate-stats-async) (lambda (_) nil))
             ((symbol-function 'tabulated-list-print) (lambda (&rest _) nil))
             ((symbol-function 'tabulated-list-init-header) (lambda () nil)))
     ,@body))

(provide 'test-helper)
;;; test-helper.el ends here
