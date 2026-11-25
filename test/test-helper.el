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

;; Throwaway git repositories (always under `make-temp-file' directories).

(defun magit-dash-test--git (dir &rest args)
  "Run git ARGS in DIR, signalling on failure; return trimmed output."
  (with-temp-buffer
    (let ((exit (apply #'call-process "git" nil t nil "-C" (expand-file-name dir)
                       "-c" "user.name=t" "-c" "user.email=t@t"
                       "-c" "commit.gpgsign=false" args)))
      (unless (zerop exit)
        (error "git %S failed in %s: %s" args dir (buffer-string)))
      (string-trim (buffer-string)))))

(defun magit-dash-test--make-repo ()
  "Return a new throwaway git repository on branch main with one commit."
  (let ((dir (file-name-as-directory (make-temp-file "mdtest-repo-" t))))
    (magit-dash-test--git dir "init" "-q" "-b" "main")
    (with-temp-file (expand-file-name "a.txt" dir) (insert "a\n"))
    (with-temp-file (expand-file-name ".gitignore" dir) (insert "keep.secret\nnode_modules/\n"))
    (magit-dash-test--git dir "add" ".")
    (magit-dash-test--git dir "commit" "-q" "-m" "init")
    dir))

(defun magit-dash-test--commit (dir name)
  "Commit a new file NAME in DIR and return its hash."
  (with-temp-file (expand-file-name name dir) (insert name "\n"))
  (magit-dash-test--git dir "add" name)
  (magit-dash-test--git dir "commit" "-q" "-m" name)
  (magit-dash-test--git dir "rev-parse" "HEAD"))

(provide 'test-helper)
;;; test-helper.el ends here
