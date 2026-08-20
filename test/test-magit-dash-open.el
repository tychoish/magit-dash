;;; test-magit-dash-open.el --- ERT tests for magit-dash-open -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Run inside a live Emacs session with full config loaded:
;;   M-x ert RET t RET
;; or filtered:
;;   (ert "^magit-dash-open/")
;;
;; Batch run:
;;   emacs --batch -l test/test-helper.el \
;;     -l test/test-magit-dash-open.el \
;;     --eval '(ert-run-tests-batch-and-exit "magit-dash-open/")'

(require 'ert)
(require 'cl-lib)
(require 'magit-dash-open)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-dash-open--git-p

(ert-deftest magit-dash-open/git-p-detects-git-dir ()
  "Returns non-nil for a directory containing a .git entry."
  (let ((dir (make-temp-file "ert-magit-git-p-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" dir))
          (should (magit-dash-open--git-p dir)))
      (delete-directory dir t))))

(ert-deftest magit-dash-open/git-p-rejects-plain-dir ()
  "Returns nil for a directory without a .git entry."
  (let ((dir (make-temp-file "ert-magit-git-p-plain-" t)))
    (unwind-protect
        (should-not (magit-dash-open--git-p dir))
      (delete-directory dir t))))

(ert-deftest magit-dash-open/git-p-git-file-counts ()
  "Returns non-nil when .git is a file (worktree) rather than a directory."
  (let ((dir (make-temp-file "ert-magit-git-p-file-" t)))
    (unwind-protect
        (progn
          (write-region "gitdir: /some/path" nil (expand-file-name ".git" dir))
          (should (magit-dash-open--git-p dir)))
      (delete-directory dir t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-dash-open--subdirs

(ert-deftest magit-dash-open/subdirs-returns-directories ()
  "Returns only directories, not plain files."
  (let ((dir (make-temp-file "ert-magit-subdirs-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "subdir" dir))
          (write-region "" nil (expand-file-name "file.txt" dir))
          (let ((result (magit-dash-open--subdirs dir)))
            (should (= 1 (length result)))
            (should (string-suffix-p "subdir" (car result)))))
      (delete-directory dir t))))

(ert-deftest magit-dash-open/subdirs-excludes-hidden ()
  "Hidden directories (starting with .) are excluded."
  (let ((dir (make-temp-file "ert-magit-subdirs-hidden-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".hidden" dir))
          (make-directory (expand-file-name "visible" dir))
          (let ((result (magit-dash-open--subdirs dir)))
            (should (= 1 (length result)))
            (should (string-suffix-p "visible" (car result)))))
      (delete-directory dir t))))

(ert-deftest magit-dash-open/subdirs-empty-dir ()
  "Returns nil for an empty directory."
  (let ((dir (make-temp-file "ert-magit-subdirs-empty-" t)))
    (unwind-protect
        (should-not (magit-dash-open--subdirs dir))
      (delete-directory dir t))))

(ert-deftest magit-dash-open/subdirs-returns-absolute-paths ()
  "All returned paths are absolute."
  (let ((dir (make-temp-file "ert-magit-subdirs-abs-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "child" dir))
          (should (seq-every-p #'file-name-absolute-p (magit-dash-open--subdirs dir))))
      (delete-directory dir t))))

(ert-deftest magit-dash-open/subdirs-inaccessible-returns-nil ()
  "Returns nil for a path that is not a directory."
  (should-not (magit-dash-open--subdirs "/no/such/path/xyz")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-dash-open--find-open-buffer

(ert-deftest magit-dash-open/find-open-buffer-matches-exact ()
  "Returns the buffer when path matches a key in open-buffers."
  (let* ((dir (make-temp-file "ert-magit-find-buf-" t))
         (norm (file-name-as-directory (expand-file-name dir)))
         (buf (get-buffer-create " *ert-magit-find-buf*"))
         (open-buffers (list (cons norm buf))))
    (unwind-protect
        (should (eq buf (magit-dash-open--find-open-buffer dir open-buffers)))
      (kill-buffer buf)
      (delete-directory dir t))))

(ert-deftest magit-dash-open/find-open-buffer-trailing-slash-insensitive ()
  "Matches regardless of whether path has a trailing slash."
  (let* ((dir (make-temp-file "ert-magit-find-slash-" t))
         (norm (file-name-as-directory (expand-file-name dir)))
         (buf (get-buffer-create " *ert-magit-find-slash*"))
         (open-buffers (list (cons norm buf))))
    (unwind-protect
        (progn
          (should (eq buf (magit-dash-open--find-open-buffer dir open-buffers)))
          (should (eq buf (magit-dash-open--find-open-buffer (directory-file-name dir) open-buffers))))
      (kill-buffer buf)
      (delete-directory dir t))))

(ert-deftest magit-dash-open/find-open-buffer-no-match ()
  "Returns nil when no buffer matches the path."
  (let* ((dir (make-temp-file "ert-magit-find-none-" t))
         (other "/tmp/completely-different-path/"))
    (unwind-protect
        (should-not (magit-dash-open--find-open-buffer dir (list (cons other nil))))
      (delete-directory dir t))))

(ert-deftest magit-dash-open/find-open-buffer-empty-list ()
  "Returns nil for an empty open-buffers alist."
  (let ((dir (make-temp-file "ert-magit-find-empty-" t)))
    (unwind-protect
        (should-not (magit-dash-open--find-open-buffer dir nil))
      (delete-directory dir t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-dash-open--worktree-paths

(ert-deftest magit-dash-open/worktree-paths-extracts-additional-worktrees ()
  "Returns paths of additional worktrees, excluding the main worktree."
  (cl-letf (((symbol-function 'magit-dash-open--run-git)
             (lambda (&rest _)
               '("worktree /repo/main"
                 "HEAD abc123"
                 "branch refs/heads/main"
                 ""
                 "worktree /repo/feature"
                 "HEAD def456"
                 "branch refs/heads/feature"
                 ""))))
    (should (equal '("/repo/feature") (magit-dash-open--worktree-paths "/repo/main")))))

(ert-deftest magit-dash-open/worktree-paths-single-worktree ()
  "Returns nil when only the main worktree exists."
  (cl-letf (((symbol-function 'magit-dash-open--run-git)
             (lambda (&rest _)
               '("worktree /repo/main"
                 "HEAD abc123"
                 "branch refs/heads/main"
                 ""))))
    (should-not (magit-dash-open--worktree-paths "/repo/main"))))

(ert-deftest magit-dash-open/worktree-paths-no-git-output ()
  "Returns nil when git returns nil (not a repo or git error)."
  (cl-letf (((symbol-function 'magit-dash-open--run-git) (lambda (&rest _) nil)))
    (should-not (magit-dash-open--worktree-paths "/no/repo"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-dash-open--submodule-paths

(ert-deftest magit-dash-open/submodule-paths-extracts-paths ()
  "Returns absolute submodule paths from git submodule status output."
  (cl-letf (((symbol-function 'magit-dash-open--run-git)
             (lambda (&rest _)
               '(" abc123 sub/module1 (v1.0)"
                 " def456 sub/module2 (v2.0)"))))
    (let ((result (magit-dash-open--submodule-paths "/repo")))
      (should (= 2 (length result)))
      (should (string-suffix-p "sub/module1" (car result)))
      (should (string-suffix-p "sub/module2" (cadr result))))))

(ert-deftest magit-dash-open/submodule-paths-empty ()
  "Returns nil when there are no submodules."
  (cl-letf (((symbol-function 'magit-dash-open--run-git) (lambda (&rest _) nil)))
    (should-not (magit-dash-open--submodule-paths "/repo"))))

(ert-deftest magit-dash-open/submodule-paths-returns-absolute ()
  "All returned paths are absolute."
  (cl-letf (((symbol-function 'magit-dash-open--run-git)
             (lambda (&rest _) '(" abc123 vendor/lib (v1)"))))
    (should (seq-every-p #'file-name-absolute-p
                         (magit-dash-open--submodule-paths "/repo")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-dash-open--annotation with dash cache

(ert-deftest magit-dash-open/annotation-directory-kind ()
  "Directory kind returns \"directory\"."
  (should (equal (magit-dash-open--annotation "/tmp/dir" 'dir) "directory")))

(ert-deftest magit-dash-open/annotation-incorporates-dash-cache ()
  "magit-dash-open--annotation enriches annotation with cached CI and PR info."
  (let ((magit-dash-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-dash-open--git-info)
               (lambda (_) (list :branch "feature" :ahead 1 :behind 0 :dirty t))))
      (magit-dash-gh--cache-set "/tmp/myrepo" :ci-status (list :conclusion "success" :status "completed"))
      (magit-dash-gh--cache-set "/tmp/myrepo" :pr-counts (cons 2 0))
      (let ((ann (magit-dash-open--annotation "/tmp/myrepo" 'repo)))
        (should (string-match-p "repo" ann))
        (should (string-match-p "feature" ann))
        (should (string-match-p "\\+1" ann))
        (should (string-match-p "\\*" ann))
        (should (string-match-p "CI:✓" ann))
        (should (string-match-p "2 PRs" ann))))))

(ert-deftest magit-dash-open/open-repo-merges-registered-repos ()
  "magit-dash-open-repo includes registered repos from magit-dash-repo-list."
  (let ((magit-dash-repo-list (list (magit-dash-repo--make :name "reg-repo" :path "/tmp/registered-repo")))
        (offered-paths nil))
    (cl-letf (((symbol-function 'magit-dash-open--collect-deep) (lambda (&rest _) nil))
              ((symbol-function 'magit-dash-open--git-p) (lambda (_) nil))
              ((symbol-function 'magit-dash-open--open-status-buffers) (lambda () nil))
              ((symbol-function 'annotated-completing-read)
               (lambda (table &rest _)
                 (setq offered-paths (map-keys table))
                 (cdr (map-elt table (abbreviate-file-name "/tmp/registered-repo")))))
              ((symbol-function 'magit-status-setup-buffer) (lambda (_path) t)))
      (magit-dash-open-repo)
      (should (member (abbreviate-file-name "/tmp/registered-repo") offered-paths)))))
(provide 'test-magit-dash-open)
;;; test-magit-dash-open.el ends here
