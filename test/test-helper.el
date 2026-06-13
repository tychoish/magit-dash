;;; test-helper.el --- ERT test infrastructure for magit-dash -*- lexical-binding: t -*-

;;; Commentary:
;; Loaded by ert-runner before any test files.
;; Adds the magit-dash repo root and emacs.d elpa/external dirs to
;; load-path so test files can require local modules and their deps.

;;; Code:

(let* ((test-file (or load-file-name buffer-file-name))
       (test-dir (file-name-directory test-file))
       (root (file-name-directory (directory-file-name test-dir)))
       (emacs-d (expand-file-name "~/.emacs.d")))
  ;; Package root — all magit-dash modules live here
  (add-to-list 'load-path root)
  ;; elpa packages: magit, transient, and other deps
  (dolist (dir (directory-files (expand-file-name "elpa" emacs-d) t "\\`[^.]"))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir)))
  ;; external packages: annotated-completing-read, sprite, xtdlib, etc.
  (dolist (dir (directory-files (expand-file-name "external" emacs-d) t "\\`[^.]"))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir))))

(provide 'test-helper)
;;; test-helper.el ends here
