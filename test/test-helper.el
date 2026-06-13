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

(provide 'test-helper)
;;; test-helper.el ends here
