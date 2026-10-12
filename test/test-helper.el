;;; test-helper.el --- ERT test infrastructure for magit-dash -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Loaded before other test files by both the test runner and manual
;; `emacs -batch -l' invocations.
;; Adds the magit-dash repo root to load-path so test files can require
;; local modules.

;;; Code:

(let* ((test-file (or load-file-name buffer-file-name))
       (test-dir (file-name-directory test-file))
       (root (file-name-directory (directory-file-name test-dir)))
       (gen-dir (expand-file-name "../gen.el" root)))
  (add-to-list 'load-path root)
  (when (file-directory-p gen-dir)
    (add-to-list 'load-path gen-dir)))

(provide 'test-helper)
;;; test-helper.el ends here
