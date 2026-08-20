;;; test-magit-dash-worktree.el --- Tests for magit-dash-worktree.el -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'magit-dash)
(require 'magit-dash-worktree)

;;; Customization & resolution tests

(ert-deftest magit-dash-worktree/global-symlinks-default ()
  "Global symlinks defaults to common project dependencies/environment files."
  (should (member "node_modules" magit-dash-worktree-global-symlinks))
  (should (member ".venv" magit-dash-worktree-global-symlinks))
  (should (member ".env" magit-dash-worktree-global-symlinks)))

(ert-deftest magit-dash-worktree/symlinks-for-global-only ()
  "Returns global symlinks when no repo-specific configuration exists."
  (let ((magit-dash-worktree-global-symlinks '("node_modules" ".venv"))
        (magit-dash-worktree-repo-symlinks nil)
        (magit-dash-repo-list nil))
    (should (equal '("node_modules" ".venv")
                   (magit-dash-worktree-symlinks-for "/tmp/some-repo")))))

(ert-deftest magit-dash-worktree/symlinks-for-repo-map ()
  "Merges global symlinks with matching entries in magit-dash-worktree-repo-symlinks."
  (let ((magit-dash-worktree-global-symlinks '("node_modules"))
        (magit-dash-worktree-repo-symlinks '(("backend" . (".venv" "target"))
                                             ("frontend" . ("dist"))))
        (magit-dash-repo-list nil))
    ;; Matching "backend"
    (let ((links (magit-dash-worktree-symlinks-for "/tmp/backend")))
      (should (member "node_modules" links))
      (should (member ".venv" links))
      (should (member "target" links))
      (should-not (member "dist" links)))))

(ert-deftest magit-dash-worktree/symlinks-for-struct-field ()
  "Merges global, repo-map, and repo struct :worktree-symlinks."
  (let* ((magit-dash-worktree-global-symlinks '("node_modules"))
         (magit-dash-worktree-repo-symlinks '(("my-app" . ("dist"))))
         (repo (magit-dash-repo--make :name "my-app"
                                      :path "/tmp/my-app"
                                      :worktree-symlinks '(".env.local" "build")))
         (magit-dash-repo-list (list repo)))
    (let ((links (magit-dash-worktree-symlinks-for repo)))
      (should (member "node_modules" links))
      (should (member "dist" links))
      (should (member ".env.local" links))
      (should (member "build" links)))))

(ert-deftest magit-dash-worktree/register-worktree-symlinks ()
  "magit-dash-register stores :worktree-symlinks and alias :symlinks in struct."
  (let ((magit-dash-repo-list nil))
    (magit-dash-register :name "r1" :path "/tmp/r1" :worktree-symlinks '("node_modules" ".env"))
    (should (equal '("node_modules" ".env")
                   (magit-dash-repo-worktree-symlinks (car magit-dash-repo-list))))
    (magit-dash-register :name "r2" :path "/tmp/r2" :symlinks '(".venv" "build"))
    (should (equal '(".venv" "build")
                   (magit-dash-repo-symlinks (car magit-dash-repo-list))))))

;;; Symlink creation tests

(ert-deftest magit-dash-worktree/symlink-worktree-creates-links ()
  "symlink-worktree creates symbolic links from main worktree to linked worktree."
  (let ((main-dir (make-temp-file "magit-dash-main-" t))
        (wt-dir (make-temp-file "magit-dash-wt-" t)))
    (unwind-protect
        (progn
          ;; Create source files/directories in main worktree
          (make-directory (expand-file-name "node_modules" main-dir) t)
          (with-temp-file (expand-file-name ".env" main-dir)
            (insert "SECRET=123\n"))
          ;; .venv does not exist in main-dir, should be skipped
          (let ((created (magit-dash-worktree-symlink-worktree
                          wt-dir main-dir '("node_modules" ".venv" ".env"))))
            ;; Check returned alist
            (should (assoc "node_modules" created))
            (should (assoc ".env" created))
            (should-not (assoc ".venv" created))
            ;; Check files in wt-dir are actual symlinks pointing to main-dir
            (should (file-symlink-p (expand-file-name "node_modules" wt-dir)))
            (should (file-symlink-p (expand-file-name ".env" wt-dir)))
            (should (equal (expand-file-name "node_modules" main-dir)
                           (file-truename (expand-file-name "node_modules" wt-dir))))
            (should (equal (expand-file-name ".env" main-dir)
                           (file-truename (expand-file-name ".env" wt-dir))))))
      (delete-directory main-dir t)
      (delete-directory wt-dir t))))

(ert-deftest magit-dash-worktree/symlink-worktree-skips-main-root ()
  "symlink-worktree skips symlinking when worktree-path is the main worktree itself."
  (let ((main-dir (make-temp-file "magit-dash-main-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "node_modules" main-dir) t)
          (let ((created (magit-dash-worktree-symlink-worktree
                          main-dir main-dir '("node_modules"))))
            (should-not created)
            ;; node_modules in main-dir should remain a real directory, not symlink
            (should-not (file-symlink-p (expand-file-name "node_modules" main-dir)))))
      (delete-directory main-dir t))))

(ert-deftest magit-dash-worktree/symlink-worktree-idempotent ()
  "Running symlink-worktree multiple times preserves existing valid symlinks."
  (let ((main-dir (make-temp-file "magit-dash-main-" t))
        (wt-dir (make-temp-file "magit-dash-wt-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name ".env" main-dir) (insert "A=1\n"))
          (let ((first-run (magit-dash-worktree-symlink-worktree wt-dir main-dir '(".env"))))
            (should (= 1 (length first-run)))
            (let ((second-run (magit-dash-worktree-symlink-worktree wt-dir main-dir '(".env"))))
              (should (= 1 (length second-run)))
              (should (file-symlink-p (expand-file-name ".env" wt-dir))))))
      (delete-directory main-dir t)
      (delete-directory wt-dir t))))

(ert-deftest magit-dash-worktree/post-add-hook-triggers-symlinking ()
  "magit-post-worktree-add-hook executes magit-dash-worktree-post-add-hook-function."
  (let ((main-dir (make-temp-file "magit-dash-main-" t))
        (wt-dir (make-temp-file "magit-dash-wt-" t))
        (magit-dash-worktree-auto-symlink t)
        (magit-dash-worktree-global-symlinks '(".env")))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name ".env" main-dir) (insert "KEY=val\n"))
          (cl-letf (((symbol-function 'magit-dash-worktree-main-dir) (lambda (_) main-dir)))
            (magit-dash-worktree-post-add-hook-function wt-dir "feat-branch")
            (should (file-symlink-p (expand-file-name ".env" wt-dir)))))
      (delete-directory main-dir t)
      (delete-directory wt-dir t))))

(provide 'test-magit-dash-worktree)
;;; test-magit-dash-worktree.el ends here
