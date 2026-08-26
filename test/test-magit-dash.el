;;; test-magit-dash.el --- ERT tests for magit-dash -*- lexical-binding: t; no-byte-compile: t; -*-

;; Run inside a live Emacs session:
;;   (ert "^magit-dash/")
;;
;; Batch run:
;;   emacs --batch -l test/test-helper.el \
;;     -l test/test-magit-dash.el \
;;     --eval '(ert-run-tests-batch-and-exit "magit-dash/")'

(require 'ert)
(require 'cl-lib)
(require 'map)
(require 'magit-dash)

;;;; magit-dash-register

(ert-deftest magit-dash/register-adds-to-list ()
  "register creates a struct and prepends it to `magit-dash-repo-list'."
  (let ((magit-dash-repo-list nil))
    (magit-dash-register :name "foo" :path "/tmp/foo")
    (should (= 1 (length magit-dash-repo-list)))
    (let ((r (car magit-dash-repo-list)))
      (should (magit-dash-repo-p r))
      (should (equal "foo" (magit-dash-repo-name r)))
      (should (equal "/tmp/foo" (magit-dash-repo-path r))))))

(ert-deftest magit-dash/register-expands-path ()
  "register expands the path with `expand-file-name'."
  (let ((magit-dash-repo-list nil))
    (magit-dash-register :name "bar" :path "~/bar")
    (should (string-prefix-p "/" (magit-dash-repo-path (car magit-dash-repo-list))))))

(ert-deftest magit-dash/register-replaces-existing ()
  "Registering the same name replaces the previous entry."
  (let ((magit-dash-repo-list nil))
    (magit-dash-register :name "dup" :path "/tmp/dup1")
    (magit-dash-register :name "dup" :path "/tmp/dup2")
    (should (= 1 (length magit-dash-repo-list)))
    (should (equal "/tmp/dup2" (magit-dash-repo-path (car magit-dash-repo-list))))))

(ert-deftest magit-dash/register-multiple ()
  "Multiple distinct names accumulate in the list."
  (let ((magit-dash-repo-list nil))
    (magit-dash-register :name "a" :path "/tmp/a")
    (magit-dash-register :name "b" :path "/tmp/b")
    (should (= 2 (length magit-dash-repo-list)))))

(ert-deftest magit-dash/register-defaults ()
  "Registering with only name+path yields correct defaults for new fields."
  (let ((magit-dash-repo-list nil))
    (magit-dash-register :name "r" :path "/tmp/r")
    (let ((r (car magit-dash-repo-list)))
      (should (null (magit-dash-repo-include-prs r)))
      (should (null (magit-dash-repo-auto-fetch r)))
      (should (null (magit-dash-repo-auto-pull r)))
      (should (null (magit-dash-repo-auto-commit r)))
      (should (null (magit-dash-repo-auto-push r)))
      (should (null (magit-dash-repo-hooks r)))
      (should (null (magit-dash-repo-tags r)))
      (should (null (magit-dash-repo-commands r)))
      (should (null (magit-dash-repo-sort-hint r)))
      (should (null (magit-dash-repo-worktree r))))))

(ert-deftest magit-dash/register-worktree ()
  ":worktree t is stored correctly."
  (let ((magit-dash-repo-list nil))
    (magit-dash-register :name "r" :path "/tmp/r" :worktree t)
    (should (eq t (magit-dash-repo-worktree (car magit-dash-repo-list))))))

(ert-deftest magit-dash/register-include-prs-true ()
  ":include-prs t is stored correctly."
  (let ((magit-dash-repo-list nil))
    (magit-dash-register :name "r" :path "/tmp/r" :include-prs t)
    (should (eq t (magit-dash-repo-include-prs (car magit-dash-repo-list))))))

(ert-deftest magit-dash/register-auto-fetch ()
  ":auto-fetch t is stored correctly."
  (let ((magit-dash-repo-list nil))
    (magit-dash-register :name "r" :path "/tmp/r" :auto-fetch t)
    (should (magit-dash-repo-auto-fetch (car magit-dash-repo-list)))))

(ert-deftest magit-dash/register-auto-pull ()
  ":auto-pull t is stored correctly."
  (let ((magit-dash-repo-list nil))
    (magit-dash-register :name "r" :path "/tmp/r" :auto-pull t)
    (should (magit-dash-repo-auto-pull (car magit-dash-repo-list)))))

(ert-deftest magit-dash/register-auto-push ()
  ":auto-push t is stored correctly."
  (let ((magit-dash-repo-list nil))
    (magit-dash-register :name "r" :path "/tmp/r" :auto-push t)
    (should (magit-dash-repo-auto-push (car magit-dash-repo-list)))))

(ert-deftest magit-dash/auto-sync-steps-fetch-only ()
  ":auto-fetch produces a single fetch step."
  (let ((magit-dash-repo-list nil))
    (magit-dash-register :name "r" :path "/tmp/r" :auto-fetch t)
    (let ((steps (magit-dash--auto-sync-steps (car magit-dash-repo-list))))
      (should (= 1 (length steps)))
      (should (equal "fetch" (caar steps))))))

(ert-deftest magit-dash/auto-sync-steps-pull-implies-fetch ()
  ":auto-pull produces fetch then pull steps."
  (let ((magit-dash-repo-list nil))
    (magit-dash-register :name "r" :path "/tmp/r" :auto-pull t)
    (let ((steps (magit-dash--auto-sync-steps (car magit-dash-repo-list))))
      (should (= 2 (length steps)))
      (should (equal "fetch" (caar steps)))
      (should (equal "pull" (car (cadr steps)))))))

(ert-deftest magit-dash/register-tags ()
  ":tags list of symbols is stored correctly."
  (let ((magit-dash-repo-list nil))
    (magit-dash-register :name "r" :path "/tmp/r" :tags '(work personal))
    (should (equal '(work personal) (magit-dash-repo-tags (car magit-dash-repo-list))))))

(ert-deftest magit-dash/register-auto-commit-bool ()
  ":auto-commit t is stored correctly."
  (let ((magit-dash-repo-list nil))
    (magit-dash-register :name "r" :path "/tmp/r" :auto-commit t)
    (should (eq t (magit-dash-repo-auto-commit (car magit-dash-repo-list))))))

(ert-deftest magit-dash/register-auto-commit-function ()
  ":auto-commit function is stored correctly."
  (let ((magit-dash-repo-list nil))
    (magit-dash-register :name "r" :path "/tmp/r" :auto-commit #'ignore)
    (should (eq #'ignore (magit-dash-repo-auto-commit (car magit-dash-repo-list))))))

(ert-deftest magit-dash/register-commands ()
  ":commands alist is stored correctly."
  (let ((magit-dash-repo-list nil)
        (cmds '(("run tests" . my-test-fn) ("lint" . my-lint-fn))))
    (magit-dash-register :name "r" :path "/tmp/r" :commands cmds)
    (should (equal cmds (magit-dash-repo-commands (car magit-dash-repo-list))))))

(ert-deftest magit-dash/register-sort-hint ()
  ":sort-hint number is stored correctly."
  (let ((magit-dash-repo-list nil))
    (magit-dash-register :name "r" :path "/tmp/r" :sort-hint 10)
    (should (= 10 (magit-dash-repo-sort-hint (car magit-dash-repo-list))))))

(ert-deftest magit-dash/register-clone-url ()
  ":clone-url and :remote-url are stored in magit-dash-repo-clone-url."
  (let ((magit-dash-repo-list nil))
    (magit-dash-register :name "r1" :path "/tmp/r1" :clone-url "https://github.com/user/r1.git")
    (should (equal "https://github.com/user/r1.git" (magit-dash-repo-clone-url (car magit-dash-repo-list))))
    (magit-dash-register :name "r2" :path "/tmp/r2" :remote-url "git@github.com:user/r2.git")
    (should (equal "git@github.com:user/r2.git" (magit-dash-repo-remote-url (car magit-dash-repo-list))))))

(ert-deftest magit-dash/register-repo-upstream ()
  ":repo stores the path/url of the upstream repository."
  (let ((magit-dash-repo-list nil))
    (magit-dash-register :name "r1" :path "/tmp/r1" :repo "/srv/git/r1.git")
    (should (equal "/srv/git/r1.git" (magit-dash-repo-repo (car magit-dash-repo-list))))
    (should (equal "/srv/git/r1.git" (magit-dash-repo-upstream-repo (car magit-dash-repo-list))))
    (should (equal "/srv/git/r1.git" (magit-dash-repo-upstream (car magit-dash-repo-list))))))

(ert-deftest magit-dash/register-expands-local-repo-path ()
  ":repo expands local file paths but preserves URLs."
  (let ((magit-dash-repo-list nil))
    (magit-dash-register :name "r1" :path "/tmp/r1" :repo "~/upstream/r1")
    (should (string-prefix-p "/" (magit-dash-repo-repo (car magit-dash-repo-list))))
    (magit-dash-register :name "r2" :path "/tmp/r2" :repo "git@github.com:user/r2.git")
    (should (equal "git@github.com:user/r2.git" (magit-dash-repo-repo (car magit-dash-repo-list))))
    (magit-dash-register :name "r3" :path "/tmp/r3" :repo "https://github.com/user/r3.git")
    (should (equal "https://github.com/user/r3.git" (magit-dash-repo-repo (car magit-dash-repo-list))))))

;;;; magit-dash--sorted-repos

(ert-deftest magit-dash/sorted-repos-by-hint ()
  "Repos with sort-hints are ordered numerically."
  (let* ((r1 (magit-dash-repo--make :name "r1" :path "/tmp/r1" :sort-hint 20))
         (r2 (magit-dash-repo--make :name "r2" :path "/tmp/r2" :sort-hint 5))
         (r3 (magit-dash-repo--make :name "r3" :path "/tmp/r3" :sort-hint 10))
         (sorted (magit-dash--sorted-repos (list r1 r2 r3))))
    (should (equal "r2" (magit-dash-repo-name (nth 0 sorted))))
    (should (equal "r3" (magit-dash-repo-name (nth 1 sorted))))
    (should (equal "r1" (magit-dash-repo-name (nth 2 sorted))))))

(ert-deftest magit-dash/sorted-repos-hinted-before-unhinted ()
  "Repos with sort-hints appear before those without."
  (let* ((r1 (magit-dash-repo--make :name "r1" :path "/tmp/r1"))
         (r2 (magit-dash-repo--make :name "r2" :path "/tmp/r2" :sort-hint 1))
         (sorted (magit-dash--sorted-repos (list r1 r2))))
    (should (equal "r2" (magit-dash-repo-name (nth 0 sorted))))
    (should (equal "r1" (magit-dash-repo-name (nth 1 sorted))))))

(ert-deftest magit-dash/sorted-repos-unhinted-preserve-order ()
  "Repos without sort-hints preserve their relative order."
  (let* ((r1 (magit-dash-repo--make :name "r1" :path "/tmp/r1"))
         (r2 (magit-dash-repo--make :name "r2" :path "/tmp/r2"))
         (sorted (magit-dash--sorted-repos (list r1 r2))))
    (should (equal "r1" (magit-dash-repo-name (nth 0 sorted))))
    (should (equal "r2" (magit-dash-repo-name (nth 1 sorted))))))

;;;; magit-dash--format-age

(ert-deftest magit-dash/format-age-nil ()
  (should (equal "┄" (magit-dash--format-age nil))))

(ert-deftest magit-dash/format-age-seconds ()
  (should (equal "30s" (magit-dash--format-age 30.0))))

(ert-deftest magit-dash/format-age-minutes ()
  (should (equal "5m" (magit-dash--format-age 300.0))))

(ert-deftest magit-dash/format-age-hours ()
  (should (equal "2h" (magit-dash--format-age 7200.0))))

(ert-deftest magit-dash/format-age-days ()
  (should (equal "3d" (magit-dash--format-age (* 3 86400.0)))))

(ert-deftest magit-dash/format-age-boundary-59s ()
  "59 seconds formats as seconds."
  (should (equal "59s" (magit-dash--format-age 59.0))))

(ert-deftest magit-dash/format-age-boundary-60s ()
  "60 seconds formats as 1 minute."
  (should (equal "1m" (magit-dash--format-age 60.0))))

;;;; magit-dash--format-status

(ert-deftest magit-dash/format-status-all-clean ()
  "All-zero/nil returns empty string."
  (should (equal "" (magit-dash--format-status 0 0 nil))))

(ert-deftest magit-dash/format-status-dirty-only ()
  "Dirty with no divergence shows only \"!\"."
  (should (equal "!" (substring-no-properties
                      (magit-dash--format-status 0 0 t)))))

(ert-deftest magit-dash/format-status-ahead-only ()
  "Ahead count shows ↑N."
  (should (equal "↑3" (substring-no-properties
                       (magit-dash--format-status 3 0 nil)))))

(ert-deftest magit-dash/format-status-behind-only ()
  "Behind count shows ↓N."
  (should (equal "↓5" (substring-no-properties
                       (magit-dash--format-status 0 5 nil)))))

(ert-deftest magit-dash/format-status-all-set ()
  "All three indicators appear in order ↑ ↓ ! separated by spaces."
  (should (equal "↑2 ↓3 !" (substring-no-properties
                             (magit-dash--format-status 2 3 t)))))

;;;; magit-dash--head-hash

(ert-deftest magit-dash/head-hash-nonexistent-path ()
  "head-hash returns nil for a path with no .git/HEAD."
  (should (null (magit-dash--head-hash "/tmp/nonexistent-no-git-here"))))

;;;; gitlink resolution: worktrees and submodules
;;
;; A linked worktree's PATH/.git is a *file* containing "gitdir: ..." that
;; points at a private directory (own HEAD) whose "commondir" file points
;; back at the main repo's real .git (shared FETCH_HEAD/refs).  A
;; submodule's PATH/.git is likewise a file, pointing at a self-contained
;; module directory with no commondir.  `magit-dash--fetch-age' and
;; `magit-dash--head-hash' used to assume PATH/.git was always a plain
;; directory, so both silently returned nil for worktrees and submodules.

(defun magit-dash-test--write-file (path content)
  "Write CONTENT to PATH, creating parent directories as needed."
  (make-directory (file-name-directory path) t)
  (with-temp-file path (insert content)))

(ert-deftest magit-dash/resolve-git-dir-plain-repo ()
  "resolve-git-dir returns PATH/.git as-is when it is already a directory."
  (let ((root (make-temp-file "magit-dash-test-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" root))
          (should (equal (expand-file-name ".git" root)
                         (magit-dash--resolve-git-dir root))))
      (delete-directory root t))))

(ert-deftest magit-dash/resolve-git-dir-follows-worktree-gitlink ()
  "resolve-git-dir follows a worktree's \"gitdir: ...\" pointer file."
  (let ((root (make-temp-file "magit-dash-test-" t)))
    (unwind-protect
        (let* ((private-dir (expand-file-name "main/.git/worktrees/wt" root))
               (wt-checkout (expand-file-name "wt-checkout" root)))
          (make-directory private-dir t)
          (magit-dash-test--write-file
           (expand-file-name ".git" wt-checkout)
           (format "gitdir: %s\n" private-dir))
          (should (equal private-dir (magit-dash--resolve-git-dir wt-checkout))))
      (delete-directory root t))))

(ert-deftest magit-dash/resolve-common-git-dir-follows-commondir ()
  "resolve-common-git-dir follows a worktree's private dir to the shared common dir."
  (let ((root (make-temp-file "magit-dash-test-" t)))
    (unwind-protect
        (let* ((main-git-dir (expand-file-name "main/.git" root))
               (private-dir (expand-file-name "main/.git/worktrees/wt" root))
               (wt-checkout (expand-file-name "wt-checkout" root)))
          (make-directory private-dir t)
          (magit-dash-test--write-file
           (expand-file-name ".git" wt-checkout)
           (format "gitdir: %s\n" private-dir))
          (magit-dash-test--write-file
           (expand-file-name "commondir" private-dir)
           "../..\n")
          (should (equal main-git-dir
                         (magit-dash--resolve-common-git-dir wt-checkout))))
      (delete-directory root t))))

(ert-deftest magit-dash/resolve-common-git-dir-no-commondir-returns-git-dir ()
  "resolve-common-git-dir returns the resolved git-dir as-is when there is no commondir.
Covers plain repos and submodules, whose module directory is self-contained."
  (let ((root (make-temp-file "magit-dash-test-" t)))
    (unwind-protect
        (let ((module-dir (expand-file-name "main/.git/modules/subm" root))
              (subm-checkout (expand-file-name "subm-checkout" root)))
          (make-directory module-dir t)
          (magit-dash-test--write-file
           (expand-file-name ".git" subm-checkout)
           (format "gitdir: %s\n" module-dir))
          (should (equal module-dir
                         (magit-dash--resolve-common-git-dir subm-checkout))))
      (delete-directory root t))))

(ert-deftest magit-dash/fetch-age-non-nil-for-worktree ()
  "fetch-age reads the shared FETCH_HEAD via the worktree's commondir, not PATH/.git."
  (let ((root (make-temp-file "magit-dash-test-" t)))
    (unwind-protect
        (let* ((main-git-dir (expand-file-name "main/.git" root))
               (private-dir (expand-file-name "main/.git/worktrees/wt" root))
               (wt-checkout (expand-file-name "wt-checkout" root)))
          (make-directory private-dir t)
          (magit-dash-test--write-file
           (expand-file-name ".git" wt-checkout)
           (format "gitdir: %s\n" private-dir))
          (magit-dash-test--write-file (expand-file-name "commondir" private-dir) "../..\n")
          (magit-dash-test--write-file (expand-file-name "FETCH_HEAD" main-git-dir) "irrelevant\n")
          (should (numberp (magit-dash--fetch-age wt-checkout))))
      (delete-directory root t))))

(ert-deftest magit-dash/fetch-age-non-nil-for-submodule ()
  "fetch-age reads FETCH_HEAD from a submodule's resolved module directory."
  (let ((root (make-temp-file "magit-dash-test-" t)))
    (unwind-protect
        (let ((module-dir (expand-file-name "main/.git/modules/subm" root))
              (subm-checkout (expand-file-name "subm-checkout" root)))
          (make-directory module-dir t)
          (magit-dash-test--write-file
           (expand-file-name ".git" subm-checkout)
           (format "gitdir: %s\n" module-dir))
          (magit-dash-test--write-file (expand-file-name "FETCH_HEAD" module-dir) "irrelevant\n")
          (should (numberp (magit-dash--fetch-age subm-checkout))))
      (delete-directory root t))))

(ert-deftest magit-dash/head-hash-resolves-worktree-private-head-via-common-refs ()
  "head-hash reads a worktree's own HEAD but resolves its ref against the common dir."
  (let ((root (make-temp-file "magit-dash-test-" t)))
    (unwind-protect
        (let* ((main-git-dir (expand-file-name "main/.git" root))
               (private-dir (expand-file-name "main/.git/worktrees/wt" root))
               (wt-checkout (expand-file-name "wt-checkout" root)))
          (make-directory private-dir t)
          (magit-dash-test--write-file
           (expand-file-name ".git" wt-checkout)
           (format "gitdir: %s\n" private-dir))
          (magit-dash-test--write-file (expand-file-name "commondir" private-dir) "../..\n")
          (magit-dash-test--write-file (expand-file-name "HEAD" private-dir) "ref: refs/heads/feature\n")
          (magit-dash-test--write-file
           (expand-file-name "refs/heads/feature" main-git-dir) "1111222233334444\n")
          (should (equal "1111222233334444" (magit-dash--head-hash wt-checkout))))
      (delete-directory root t))))

(ert-deftest magit-dash/head-hash-resolves-submodule-head ()
  "head-hash resolves a submodule's HEAD and ref from its own module directory."
  (let ((root (make-temp-file "magit-dash-test-" t)))
    (unwind-protect
        (let ((module-dir (expand-file-name "main/.git/modules/subm" root))
              (subm-checkout (expand-file-name "subm-checkout" root)))
          (make-directory module-dir t)
          (magit-dash-test--write-file
           (expand-file-name ".git" subm-checkout)
           (format "gitdir: %s\n" module-dir))
          (magit-dash-test--write-file (expand-file-name "HEAD" module-dir) "ref: refs/heads/main\n")
          (magit-dash-test--write-file (expand-file-name "refs/heads/main" module-dir) "deadbeef\n")
          (should (equal "deadbeef" (magit-dash--head-hash subm-checkout))))
      (delete-directory root t))))

;;;; magit-dash--commit-timestamp-to-age

(ert-deftest magit-dash/commit-timestamp-to-age-empty-is-nil ()
  "commit-timestamp-to-age returns nil for an empty timestamp (no commits yet)."
  (should (null (magit-dash--commit-timestamp-to-age ""))))

(ert-deftest magit-dash/commit-timestamp-to-age-converts-unix-seconds ()
  "commit-timestamp-to-age converts a unix-seconds string to a seconds-ago float."
  (let ((age (magit-dash--commit-timestamp-to-age
              (number-to-string (round (- (float-time) 120))))))
    (should (numberp age))
    (should (> age 100))
    (should (< age 140))))

;;;; magit-dash--display-branch-name

(ert-deftest magit-dash/display-branch-name-unchanged-when-disabled ()
  "display-branch-name returns BRANCH unchanged when the basename toggle is off."
  (let ((magit-dash-render-branch-name-as-basename nil))
    (should (equal "feature/some/thing" (magit-dash--display-branch-name "feature/some/thing")))))

(ert-deftest magit-dash/display-branch-name-trims-to-basename-when-enabled ()
  "display-branch-name trims everything up to the final \"/\" when the toggle is on."
  (let ((magit-dash-render-branch-name-as-basename t))
    (should (equal "thing" (magit-dash--display-branch-name "feature/some/thing")))))

(ert-deftest magit-dash/display-branch-name-no-slash-unaffected ()
  "display-branch-name returns a branch with no slash unchanged, toggle on or off."
  (let ((magit-dash-render-branch-name-as-basename t))
    (should (equal "main" (magit-dash--display-branch-name "main")))))

;;;; magit-dash--collect-stats (via mock)

(ert-deftest magit-dash/collect-stats-extracts-fields ()
  "collect-stats populates all stat fields using magit-git functions."
  (let ((repo (magit-dash-repo--make :name "test" :path "/tmp/test")))
    (cl-letf (((symbol-function 'magit-git-string)
               (lambda (&rest args)
                 (cond
                  ((member "branch" args) "main")
                  ((member "rev-list" args) "2")
                  ((and (member "config" args) (member "remote.origin.url" args))
                   "git@github.com:user/test.git")
                  (t nil))))
              ((symbol-function 'magit-git-lines)
               (lambda (&rest args)
                 (cond
                  ((member "status" args) '(" M foo.el"))
                  ((member "log" args) '("abc123 fix foo" "def456 add bar"))
                  (t nil))))
              ((symbol-function 'magit-dash--fetch-age)
               (lambda (_) 3600.0))
              ((symbol-function 'magit-dash--head-hash)
               (lambda (_) "abc123def456")))
      (let ((magit-dash--stats-cache (make-hash-table :test #'equal)))
        (let ((stats (magit-dash--collect-stats repo)))
          (should (equal "main" (plist-get stats :branch)))
          (should (equal "git@github.com:user/test.git" (plist-get stats :remote-origin)))
          (should (= 2 (plist-get stats :behind)))
          (should (eq t (plist-get stats :dirty)))
          (should (equal '(" M foo.el") (plist-get stats :uncommitted-files)))
          (should (= 3600.0 (plist-get stats :fetch-age)))
          (should (equal "abc123def456" (plist-get stats :head-hash)))
          (should (string-match-p "fix foo" (plist-get stats :recent-log))))))))

(ert-deftest magit-dash/collect-stats-clean-workdir ()
  "collect-stats sets :dirty nil and :uncommitted-files nil when porcelain is empty."
  (let ((repo (magit-dash-repo--make :name "test" :path "/tmp/test")))
    (cl-letf (((symbol-function 'magit-git-string)
               (lambda (&rest args)
                 (cond
                  ((member "branch" args) "feat")
                  ((member "rev-list" args) "0")
                  (t nil))))
              ((symbol-function 'magit-get)
               (lambda (&rest _) nil))
              ((symbol-function 'magit-git-lines)
               (lambda (&rest _) nil))
              ((symbol-function 'magit-dash--fetch-age)
               (lambda (_) nil))
              ((symbol-function 'magit-dash--head-hash)
               (lambda (_) "deadbeef")))
      (let ((magit-dash--stats-cache (make-hash-table :test #'equal)))
        (let ((stats (magit-dash--collect-stats repo)))
          (should (eq nil (plist-get stats :dirty)))
          (should (null (plist-get stats :uncommitted-files)))
          (should (null (plist-get stats :remote-origin)))
          (should (= 0 (plist-get stats :behind))))))))

;;;; magit-dash--get-stats (cache invalidation)

(ert-deftest magit-dash/get-stats-uses-cache-on-same-hash ()
  "get-stats returns cached stats when HEAD hash matches."
  (let* ((repo (magit-dash-repo--make :name "test" :path "/tmp/test"))
         (cached (list :branch "main" :remote-origin nil :behind 0
                       :dirty nil :uncommitted-files nil
                       :fetch-age 60.0 :head-hash "abc123" :recent-log ""))
         (magit-dash-gh--cache (make-hash-table :test #'equal)))
    (magit-dash-gh--cache-set "/tmp/test" :stats cached)
    (cl-letf (((symbol-function 'magit-dash--head-hash)
               (lambda (_) "abc123")))
      (should (equal cached (magit-dash--get-stats repo))))))

(ert-deftest magit-dash/get-stats-invalidates-on-new-hash ()
  "get-stats collects fresh stats when HEAD hash has changed."
  (let* ((repo (magit-dash-repo--make :name "test" :path "/tmp/test"))
         (cached (list :branch "main" :remote-origin nil :behind 0
                       :dirty nil :uncommitted-files nil
                       :fetch-age 60.0 :head-hash "old123" :recent-log ""))
         (magit-dash-gh--cache (make-hash-table :test #'equal)))
    (magit-dash-gh--cache-set "/tmp/test" :stats cached)
    (cl-letf (((symbol-function 'magit-dash--head-hash)
               (lambda (_) "new456"))
              ((symbol-function 'magit-dash--collect-stats)
               (lambda (_) (list :branch "feat" :remote-origin nil :behind 1
                                 :dirty t :uncommitted-files nil
                                 :fetch-age nil :head-hash "new456" :recent-log ""))))
      (let ((result (magit-dash--get-stats repo)))
        (should (equal "feat" (plist-get result :branch)))
        (should (= 1 (plist-get result :behind)))))))

;;;; magit-dash--get-stats-fast (non-blocking cache access)

(ert-deftest magit-dash/get-stats-fast-returns-cached-stats ()
  "get-stats-fast returns cached stats without validation."
  (let* ((repo (magit-dash-repo--make :name "test" :path "/tmp/test-fast"))
         (cached (list :branch "main" :remote-origin nil :behind 0 :ahead 0
                       :dirty nil :uncommitted-files nil
                       :fetch-age 60.0 :head-hash "abc123" :recent-log ""))
         (magit-dash-gh--cache (make-hash-table :test #'equal)))
    (magit-dash-gh--cache-set "/tmp/test-fast" :stats cached)
    (should (equal cached (magit-dash--get-stats-fast repo)))))

(ert-deftest magit-dash/get-stats-fast-returns-placeholder-when-empty ()
  "get-stats-fast returns a loading placeholder when no cache entry exists."
  (let* ((repo (magit-dash-repo--make :name "test" :path "/tmp/test-nocache"))
         (magit-dash-gh--cache (make-hash-table :test #'equal))
         (result (magit-dash--get-stats-fast repo)))
    (should (equal "…" (plist-get result :branch)))
    (should (null (plist-get result :head-hash)))))

(ert-deftest magit-dash/get-stats-fast-returns-empty-for-missing-submodule ()
  "get-stats-fast returns empty branch string for missing submodules."
  (let* ((repo (magit-dash-repo--make :name "p<s>" :path "/tmp/p/s" :submodule 'missing))
         (magit-dash-gh--cache (make-hash-table :test #'equal))
         (result (magit-dash--get-stats-fast repo)))
    (should (equal "" (plist-get result :branch)))))

(ert-deftest magit-dash/get-stats-fast-does-not-validate-stale-cache ()
  "get-stats-fast returns cached stats even when HEAD hash has changed."
  (let* ((repo (magit-dash-repo--make :name "test" :path "/tmp/test-stale"))
         (stale (list :branch "old" :remote-origin nil :behind 0 :ahead 0
                      :dirty nil :uncommitted-files nil
                      :fetch-age nil :head-hash "old-hash" :recent-log ""))
         (magit-dash-gh--cache (make-hash-table :test #'equal)))
    (magit-dash-gh--cache-set "/tmp/test-stale" :stats stale)
    (cl-letf (((symbol-function 'magit-dash--head-hash) (lambda (_) "new-hash")))
      (should (equal stale (magit-dash--get-stats-fast repo))))))

;;;; magit-dash--auto-commit

(ert-deftest magit-dash/auto-commit-uses-default-message ()
  "With :auto-commit t, calls magit-call-git add then commit with the default message."
  (let ((repo (magit-dash-repo--make :name "test" :path "/tmp/test" :auto-commit t))
        (git-calls nil))
    (cl-letf (((symbol-function 'magit-call-git)
               (lambda (&rest args)
                 (push args git-calls)
                 0)))
      (should (magit-dash--auto-commit repo)))
    ;; git-calls is (commit-args add-args) — commit was pushed last
    (should (= 2 (length git-calls)))
    (should (member "add" (cadr git-calls)))
    (let ((commit-args (car git-calls)))
      (should (equal "commit" (car commit-args)))
      (should (member (magit-dash--default-commit-message repo)
                      commit-args)))))

(ert-deftest magit-dash/auto-commit-calls-message-function ()
  "With :auto-commit as a function, calls it to produce the commit message."
  (let* ((custom-msg "custom: my message")
         (repo (magit-dash-repo--make :name "test" :path "/tmp/test"
                                    :auto-commit (lambda (_r) custom-msg)))
         (commit-msg nil))
    (cl-letf (((symbol-function 'magit-call-git)
               (lambda (&rest args)
                 (when (equal "commit" (car args))
                   (setq commit-msg (cadr (member "-m" args))))
                 0)))
      (magit-dash--auto-commit repo)
      (should (equal custom-msg commit-msg)))))

(ert-deftest magit-dash/commit-user-error-when-not-configured ()
  "commit signals user-error when :auto-commit is nil."
  (let ((repo (magit-dash-repo--make :name "test" :path "/tmp/test")))
    (cl-letf (((symbol-function 'magit-dash--repo-at-point)
               (lambda () repo)))
      (should-error (magit-dash-commit) :type 'user-error))))

;;;; magit-dash-sync-all

(ert-deftest magit-dash/sync-all-user-error-when-none-configured ()
  "sync-all signals user-error when no repos have :auto-sync."
  (let ((magit-dash-repo-list (list (magit-dash-repo--make :name "r" :path "/tmp/r"))))
    (should-error (magit-dash-sync-all) :type 'user-error)))

(ert-deftest magit-dash/sync-all-skips-repos-without-auto-sync ()
  "sync-all only runs on repos that have auto operations configured."
  (let* ((r1 (magit-dash-repo--make :name "r1" :path "/tmp/r1" :auto-fetch t))
         (r2 (magit-dash-repo--make :name "r2" :path "/tmp/r2"))
         (magit-dash-repo-list (list r1 r2))
         (batch-repos nil))
    (cl-letf (((symbol-function 'magit-dash--batch-run)
               (lambda (repos _op _label &optional _done)
                 (setq batch-repos repos))))
      (magit-dash-sync-all))
    (should (= 1 (length batch-repos)))
    (should (equal "r1" (magit-dash-repo-name (car batch-repos))))))

;;;; magit-dash-sync-repo

(ert-deftest magit-dash/sync-repo-errors-when-no-repos ()
  "Signals user-error when no repos have auto-sync configured."
  (cl-letf (((symbol-value 'magit-dash-repo-list) nil)
            ((symbol-function 'magit-dash--auto-sync-steps) #'ignore))
    (should-error (magit-dash-sync-repo) :type 'user-error)))

(ert-deftest magit-dash/sync-repo-errors-when-none-configured ()
  "Signals user-error when repos exist but none have auto-sync steps."
  (cl-letf (((symbol-value 'magit-dash-repo-list)
             (list (magit-dash-repo--make :name "fake" :path "/tmp/fake")))
            ((symbol-function 'magit-dash--auto-sync-steps) (lambda (_) nil)))
    (should-error (magit-dash-sync-repo) :type 'user-error)))

(ert-deftest magit-dash/sync-repo-calls-async-on-selected ()
  "Calls `magit-dash--auto-sync-async' with the repo matching the selection.
`annotated-completing-read' resolves the picked candidate's triple-form
target directly to the repo struct."
  (let* ((fake-repo (magit-dash-repo--make :name "my-repo" :path "/tmp/my-repo"))
         (called-with nil))
    (cl-letf (((symbol-value 'magit-dash-repo-list) (list fake-repo))
              ((symbol-function 'magit-dash--auto-sync-steps) (lambda (_) '(fetch)))
              ((symbol-function 'annotated-completing-read)
               (lambda (_table &rest _) fake-repo))
              ((symbol-function 'magit-dash--auto-sync-async)
               (lambda (repo _cb) (setq called-with repo)))
              ((symbol-function 'magit-dash--maybe-refresh) #'ignore))
      (magit-dash-sync-repo)
      (should (eq called-with fake-repo)))))

;;;; magit-dash--run-command-for

(ert-deftest magit-dash/run-command-user-error-when-no-commands ()
  "run-command-for signals user-error when repo has no commands."
  (let ((repo (magit-dash-repo--make :name "test" :path "/tmp/test")))
    (should-error (magit-dash--run-command-for repo) :type 'user-error)))

(ert-deftest magit-dash/normalize-command-op-formats ()
  "normalize-command-op extracts target and background flag correctly."
  ;; Plain string
  (should (equal (magit-dash--normalize-command-op "make test") '("make test" . nil)))
  ;; Plain symbol/function
  (should (equal (magit-dash--normalize-command-op 'my-func) '(my-func . nil)))
  ;; List with keyword
  (should (equal (magit-dash--normalize-command-op '("make test" :background t)) '("make test" . t)))
  (should (equal (magit-dash--normalize-command-op '(my-func :background t)) '(my-func . t)))
  ;; Plist
  (should (equal (magit-dash--normalize-command-op '(:command "make test" :background t)) '("make test" . t)))
  (should (equal (magit-dash--normalize-command-op '(:command "make test" :background nil)) '("make test" . nil))))

(ert-deftest magit-dash/run-command-for-foreground-starts-compilation ()
  "run-command-for in foreground calls compilation-start with command-named buffer."
  (let ((repo (magit-dash-repo--make :name "test" :path "/tmp/test"
                                     :commands '(("build" . "make build"))))
        (started nil))
    (cl-letf (((symbol-function 'annotated-completing-read)
               (lambda (_choices &rest _args) '("build" . "make build")))
              ((symbol-function 'compilation-start)
               (lambda (cmd &optional _mode name-fn)
                 (setq started (list cmd (funcall name-fn nil)))
                 (get-buffer-create (funcall name-fn nil)))))
      (magit-dash--run-command-for repo nil)
      (should (equal started '("make build" "*test-dash-cmd-build*"))))))

(ert-deftest magit-dash/run-command-for-background-suppresses-window ()
  "run-command-for in background suppresses window display using display-buffer-no-window."
  (let ((repo (magit-dash-repo--make :name "test" :path "/tmp/test"
                                     :commands '(("build" . "make build"))))
        (started nil)
        (has-no-window-rule nil))
    (cl-letf (((symbol-function 'annotated-completing-read)
               (lambda (_choices &rest _args) '("build" . "make build")))
              ((symbol-function 'compilation-start)
               (lambda (cmd &optional _mode name-fn)
                 (setq started cmd)
                 (let ((bname (funcall name-fn nil)))
                   (setq has-no-window-rule
                         (seq-some (lambda (rule)
                                     (and (stringp (car rule))
                                          (string-match-p (car rule) bname)
                                          (eq (cadr rule) #'display-buffer-no-window)))
                                   display-buffer-alist))
                   (get-buffer-create bname)))))
      (magit-dash--run-command-for repo t)
      (should (equal started "make build"))
      (should has-no-window-rule))))

(ert-deftest magit-dash/run-command-for-per-command-background ()
  "run-command-for respects :background t on registered command entry."
  (let ((repo (magit-dash-repo--make :name "test" :path "/tmp/test"
                                     :commands '(("bg-test" . ("make bg-test" :background t))))))
    (cl-letf (((symbol-function 'annotated-completing-read)
               (lambda (_choices &rest _args) '("bg-test" . ("make bg-test" :background t))))
              ((symbol-function 'compilation-start)
               (lambda (cmd &optional _mode name-fn)
                 (should (equal cmd "make bg-test"))
                 (let ((bname (funcall name-fn nil)))
                   (should (equal bname "*test-dash-cmd-bg-test*"))
                   (should (seq-some (lambda (rule)
                                       (and (stringp (car rule))
                                            (string-match-p (car rule) bname)
                                            (eq (cadr rule) #'display-buffer-no-window)))
                                     display-buffer-alist))
                   (get-buffer-create bname)))))
      (magit-dash--run-command-for repo nil))))
(ert-deftest magit-dash/run-command-delegates-prefix-arg ()
  "magit-dash-run-command passes prefix argument as background flag."
  (let ((repo (magit-dash-repo--make :name "test" :path "/tmp/test"))
        (called-bg nil))
    (cl-letf (((symbol-function 'magit-dash--repo-at-point) (lambda () repo))
              ((symbol-function 'magit-dash--run-command-for)
               (lambda (_repo &optional bg) (setq called-bg bg))))
      (let ((current-prefix-arg '(4)))
        (call-interactively #'magit-dash-run-command))
      (should (equal called-bg '(4))))))

(ert-deftest magit-dash/run-command-background-invokes-bg ()
  "magit-dash-run-command-background explicitly passes t for background."
  (let ((repo (magit-dash-repo--make :name "test" :path "/tmp/test"))
        (called-bg nil))
    (cl-letf (((symbol-function 'magit-dash--repo-at-point) (lambda () repo))
              ((symbol-function 'magit-dash--run-command-for)
               (lambda (_repo &optional bg) (setq called-bg bg))))
      (magit-dash-run-command-background)
      (should (eq called-bg t)))))
(ert-deftest magit-dash/run-command-includes-rebuild-when-ci-enabled ()
  "run-command-for offers 'rebuild' command when repo has :include-ci t."
  (let ((repo (magit-dash-repo--make :name "test" :path "/tmp/test" :include-ci t))
        (offered nil))
    (cl-letf (((symbol-function 'annotated-completing-read)
               (lambda (choices &rest _args)
                 (setq offered (mapcar #'car choices))
                 '("rebuild" . magit-dash-gh-workflow-run)))
              ((symbol-function 'magit-dash-gh-workflow-run) #'ignore))
      (magit-dash--run-command-for repo nil)
      (should (member "rebuild" offered)))))

(ert-deftest magit-dash/has-commands-p-true-when-ci-enabled ()
  "has-commands-p returns t for a repo with :include-ci t even without :commands."
  (let ((repo (magit-dash-repo--make :name "ci-repo" :path "/tmp/ci-repo" :include-ci t)))
    (cl-letf (((symbol-function 'magit-dash--repo-at-point) (lambda () repo)))
      (should (magit-dash--has-commands-p)))))

(defmacro magit-dash-test--with-refresh-stubs (&rest body)
  "Run BODY with `magit-dash-refresh' infrastructure stubbed out.
Stubs discover-worktrees/submodules, populate-stats-async, and the
tabulated-list render functions so tests can call `magit-dash-refresh'
without real git repos or a live dashboard buffer."
  `(cl-letf (((symbol-function 'magit-dash--discover-worktrees) (lambda () nil))
             ((symbol-function 'magit-dash--discover-submodules) (lambda () nil))
             ((symbol-function 'magit-dash--populate-stats-async) (lambda (_) nil))
             ((symbol-function 'tabulated-list-print) (lambda (&rest _) nil))
             ((symbol-function 'tabulated-list-init-header) (lambda () nil)))
     ,@body))

(ert-deftest magit-dash/filter-by-tag-reduces-entries ()
  "Refresh with a tag filter shows only matching repos."
  (let* ((r1 (magit-dash-repo--make :name "r1" :path "/tmp/r1" :tags '(work)))
         (r2 (magit-dash-repo--make :name "r2" :path "/tmp/r2" :tags '(personal)))
         (magit-dash-repo-list (list r1 r2))
         (built nil))
    (magit-dash-test--with-refresh-stubs
      (cl-letf (((symbol-function 'magit-dash--build-entry)
                 (lambda (r) (push (magit-dash-repo-name r) built) nil)))
        (with-temp-buffer
          (setq-local magit-dash--tag-filter 'work)
          (magit-dash-refresh))))
    (should (= 1 (length built)))
    (should (member "r1" built))
    (should-not (member "r2" built))))

(ert-deftest magit-dash/filter-nil-shows-all ()
  "Refresh with no tag filter shows all repos."
  (let* ((r1 (magit-dash-repo--make :name "r1" :path "/tmp/r1" :tags '(work)))
         (r2 (magit-dash-repo--make :name "r2" :path "/tmp/r2" :tags '(personal)))
         (magit-dash-repo-list (list r1 r2))
         (built nil))
    (magit-dash-test--with-refresh-stubs
      (cl-letf (((symbol-function 'magit-dash--build-entry)
                 (lambda (r) (push (magit-dash-repo-name r) built) nil)))
        (with-temp-buffer
          (setq-local magit-dash--tag-filter nil)
          (magit-dash-refresh))))
    (should (= 2 (length built)))))

;;;; magit-dash--build-format

(ert-deftest magit-dash/build-format-elastic-width ()
  "Name column width is one wider than the longest repo name."
  (let* ((r1 (magit-dash-repo--make :name "short" :path "/tmp/r1"))
         (r2 (magit-dash-repo--make :name "a-much-longer-name" :path "/tmp/r2"))
         (fmt (magit-dash--build-format (list r1 r2))))
    (should (= (1+ (length "a-much-longer-name")) (cadr (aref fmt 0))))))

(ert-deftest magit-dash/build-format-minimum-width ()
  "Name column is at least as wide as the header label \"Name\"."
  (let* ((r (magit-dash-repo--make :name "x" :path "/tmp/r"))
         (fmt (magit-dash--build-format (list r))))
    (should (>= (cadr (aref fmt 0)) (length "Name")))))

(ert-deftest magit-dash/build-format-empty-list ()
  "Empty repo list yields the minimum column width (12)."
  (let ((fmt (magit-dash--build-format nil)))
    (should (= 12 (cadr (aref fmt 0))))))

;;;; magit-dash--build-entry

(ert-deftest magit-dash/build-entry-structure ()
  "build-entry returns (REPO VECTOR) with columns matching active column count."
  (let ((repo (magit-dash-repo--make :name "myrep" :path "/tmp/myrep"))
        (magit-dash--stats-cache (make-hash-table :test #'equal))
        (magit-dash-columns
         '((name . t) (branch . t) (fetched . t) (updated . nil) (ci . nil) (status . t)
           (worktree . t) (sync . nil) (cached . nil)))
        (magit-dash--worktree-map (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-dash--get-stats-fast)
               (lambda (_)
                 (list :branch "main" :ahead 0 :behind 0 :dirty nil :fetch-age 120.0
                       :head-hash "abc" :recent-log ""))))
      (let* ((entry (magit-dash--build-entry repo))
             (id (car entry))
             (vec (cadr entry)))
        (should (magit-dash-repo-p id))
        (should (= 5 (length vec)))
        (should (string-match-p "myrep" (aref vec 0)))
        (should (string-match-p "main" (aref vec 1)))
        (should (equal "2m" (aref vec 2)))
        (should (equal "" (aref vec 3)))))))

(ert-deftest magit-dash/build-entry-updated-column-shows-commit-age ()
  "build-entry's Updated column formats :updated-age like :fetch-age does."
  (let ((repo (magit-dash-repo--make :name "myrep" :path "/tmp/myrep"))
        (magit-dash-columns '((name . t) (branch . nil) (fetched . nil) (updated . t)
                              (ci . nil) (status . nil) (worktree . nil)
                              (sync . nil) (cached . nil))))
    (cl-letf (((symbol-function 'magit-dash--get-stats-fast)
               (lambda (_)
                 (list :branch "main" :ahead 0 :behind 0 :dirty nil
                       :fetch-age nil :updated-age 3600.0
                       :head-hash "abc" :recent-log ""))))
      (let* ((entry (magit-dash--build-entry repo))
             (vec (cadr entry)))
        (should (equal "1h" (aref vec 1)))))))

(ert-deftest magit-dash/build-entry-updated-column-nil-age-shows-placeholder ()
  "build-entry's Updated column shows the \"never\" placeholder when :updated-age is nil."
  (let ((repo (magit-dash-repo--make :name "myrep" :path "/tmp/myrep"))
        (magit-dash-columns '((name . t) (branch . nil) (fetched . nil) (updated . t)
                              (ci . nil) (status . nil) (worktree . nil)
                              (sync . nil) (cached . nil))))
    (cl-letf (((symbol-function 'magit-dash--get-stats-fast)
               (lambda (_)
                 (list :branch "main" :ahead 0 :behind 0 :dirty nil
                       :fetch-age nil :updated-age nil
                       :head-hash "abc" :recent-log ""))))
      (let* ((entry (magit-dash--build-entry repo))
             (vec (cadr entry)))
        (should (equal "┄" (aref vec 1)))))))

;;;; magit-dash-gh-pr-dashboard--build-args

(ert-deftest magit-dash/pr-build-args-default ()
  "Default filters aggregate all open PRs without author filtering."
  (let ((filters (list :state "open" :author nil :repo nil :org nil)))
    (let ((args (magit-dash-gh-pr-dashboard--build-args filters)))
      (should (equal "search" (nth 0 args)))
      (should (equal "prs" (nth 1 args)))
      (should (member "--state" args))
      (should (member "open" args))
      (should-not (member "--author" args)))))

(ert-deftest magit-dash/pr-build-args-with-author ()
  "Explicit author filter adds --author to the search command."
  (let ((filters (list :state "open" :author "@me" :repo nil :org nil)))
    (let ((args (magit-dash-gh-pr-dashboard--build-args filters)))
      (should (equal "search" (nth 0 args)))
      (should (member "--author" args))
      (should (member "@me" args)))))
(ert-deftest magit-dash/pr-build-args-with-repo ()
  "When :repo is set, uses gh pr list -R REPO."
  (let ((filters (list :state "open" :author nil :repo "owner/myrepo" :org nil)))
    (let ((args (magit-dash-gh-pr-dashboard--build-args filters)))
      (should (equal "pr" (nth 0 args)))
      (should (equal "list" (nth 1 args)))
      (should (member "-R" args))
      (should (member "owner/myrepo" args)))))

(ert-deftest magit-dash/pr-build-args-with-org ()
  "When :org is set (no :repo), adds --owner to the search command."
  (let ((filters (list :state "open" :author "@me" :repo nil :org "myorg")))
    (let ((args (magit-dash-gh-pr-dashboard--build-args filters)))
      (should (equal "search" (nth 0 args)))
      (should (member "--owner" args))
      (should (member "myorg" args)))))

(ert-deftest magit-dash/pr-build-args-state-closed ()
  "State \"closed\" is passed through for both search and per-repo modes."
  (let ((filters (list :state "closed" :author nil :repo nil :org nil)))
    (let ((args (magit-dash-gh-pr-dashboard--build-args filters)))
      (should (member "closed" args)))))

(ert-deftest magit-dash/pr-build-args-state-unknown ()
  "Unknown state falls back to \"open\" in search mode."
  (let ((filters (list :state "merged" :author nil :repo nil :org nil)))
    (let ((args (magit-dash-gh-pr-dashboard--build-args filters)))
      (should (equal "search" (nth 0 args)))
      (should (member "open" args))
      (should-not (member "merged" args)))))

(ert-deftest magit-dash/pr-build-args-no-author ()
  "Nil :author omits --author from the command."
  (let ((filters (list :state "open" :author nil :repo nil :org nil)))
    (let ((args (magit-dash-gh-pr-dashboard--build-args filters)))
      (should-not (member "--author" args)))))

;;;; magit-dash-gh-pr-dashboard--format-ci

(ert-deftest magit-dash/format-ci-success ()
  (should (equal "pass" (substring-no-properties
                         (magit-dash-gh-pr-dashboard--format-ci "SUCCESS")))))

(ert-deftest magit-dash/format-ci-failure ()
  (should (equal "fail" (substring-no-properties
                         (magit-dash-gh-pr-dashboard--format-ci "FAILURE")))))

(ert-deftest magit-dash/format-ci-pending ()
  (should (equal "pending" (substring-no-properties
                            (magit-dash-gh-pr-dashboard--format-ci "PENDING")))))

(ert-deftest magit-dash/format-ci-nil ()
  "Nil or unknown CI state produces a dash."
  (should (equal "—" (magit-dash-gh-pr-dashboard--format-ci nil)))
  (should (equal "—" (magit-dash-gh-pr-dashboard--format-ci "UNKNOWN"))))

;;;; magit-dash-gh-pr-dashboard--format-review

(ert-deftest magit-dash/format-review-approved ()
  (should (equal "approved" (substring-no-properties
                             (magit-dash-gh-pr-dashboard--format-review "APPROVED")))))

(ert-deftest magit-dash/format-review-changes ()
  (should (equal "changes req" (substring-no-properties
                                (magit-dash-gh-pr-dashboard--format-review "CHANGES_REQUESTED")))))

(ert-deftest magit-dash/format-review-needed ()
  (should (equal "needed" (substring-no-properties
                           (magit-dash-gh-pr-dashboard--format-review "REVIEW_REQUIRED")))))

(ert-deftest magit-dash/format-review-nil ()
  "Nil review decision returns empty string."
  (should (equal "" (magit-dash-gh-pr-dashboard--format-review nil))))

;;;; magit-dash-gh-pr-dashboard--comments-count

(ert-deftest magit-dash/comments-count-integer ()
  "When comments field is already an integer, return it directly."
  (let ((pr '((comments . 7))))
    (should (= 7 (magit-dash-gh-pr-dashboard--comments-count pr)))))

(ert-deftest magit-dash/comments-count-alist ()
  "When comments is an alist with totalCount, extract it."
  (let ((pr `((comments . ((totalCount . 3))))))
    (should (= 3 (magit-dash-gh-pr-dashboard--comments-count pr)))))

(ert-deftest magit-dash/comments-count-nil ()
  "Nil comments field returns 0."
  (let ((pr '((title . "test"))))
    (should (= 0 (magit-dash-gh-pr-dashboard--comments-count pr)))))

;;;; magit-dash-gh-pr-dashboard--parse-output

(defun magit-dash-test--make-pr-json (&rest overrides)
  "Build a minimal PR JSON object string, optionally overriding fields."
  (let ((pr (append
             (list (cons 'number 42)
                   (cons 'title "Test PR")
                   (cons 'state "OPEN")
                   (cons 'author '((login . "alice")))
                   (cons 'updatedAt "2024-01-15T10:30:00Z")
                   (cons 'comments 5)
                   (cons 'reviewDecision "APPROVED")
                   (cons 'isDraft :false)
                   (cons 'url "https://github.com/owner/repo/pull/42"))
             overrides)))
    pr))

(ert-deftest magit-dash/parse-output-empty-array ()
  "Empty JSON array produces nil."
  (should (null (magit-dash-gh-pr-dashboard--parse-output
                 "[]"
                 (list :repo "owner/myrepo")))))

(ert-deftest magit-dash/parse-output-non-json ()
  "Non-JSON output (e.g., error message) produces nil."
  (should (null (magit-dash-gh-pr-dashboard--parse-output
                 "error: not found"
                 (list :state "open")))))

(ert-deftest magit-dash/parse-output-per-repo-mode ()
  "Per-repo mode uses :repo filter value as the repo-name."
  (let* ((pr (magit-dash-test--make-pr-json))
         (json (concat "[" (json-serialize pr) "]"))
         (filters (list :state "open" :repo "owner/myrepo"))
         (entries (magit-dash-gh-pr-dashboard--parse-output json filters)))
    (should (= 1 (length entries)))
    (let* ((entry (car entries))
           (id (car entry))
           (vec (cadr entry)))
      (should (equal "owner/myrepo" (plist-get id :repo)))
      (should (= 42 (plist-get id :number)))
      (should (equal "owner/myrepo" (aref vec 0)))
      (should (equal "42" (aref vec 1))))))

(ert-deftest magit-dash/parse-output-search-mode ()
  "Search mode extracts repo name from the `repository' field."
  (let* ((pr (append (magit-dash-test--make-pr-json)
                     (list (cons 'repository '((nameWithOwner . "org/other"))))))
         (json (concat "[" (json-serialize pr) "]"))
         (filters (list :state "open" :author "@me"))
         (entries (magit-dash-gh-pr-dashboard--parse-output json filters)))
    (should (= 1 (length entries)))
    (let* ((entry (car entries))
           (id (car entry)))
      (should (equal "org/other" (plist-get id :repo))))))

(ert-deftest magit-dash/parse-output-multiple-prs ()
  "Multiple PRs produce multiple entries."
  (let* ((pr1 (magit-dash-test--make-pr-json))
         (pr2 (append (magit-dash-test--make-pr-json) (list (cons 'number 99))))
         (json (concat "[" (json-serialize pr1) "," (json-serialize pr2) "]"))
         (filters (list :repo "owner/repo"))
         (entries (magit-dash-gh-pr-dashboard--parse-output json filters)))
    (should (= 2 (length entries)))))

;;;; magit-dash-gh-pr-dashboard--build-entry (column structure)

(ert-deftest magit-dash/build-entry-columns ()
  "build-entry produces a 7-element vector matching the column format."
  (let* ((pr '((number . 10)
               (title . "My PR")
               (updatedAt . "2024-06-01T12:00:00Z")
               (comments . 2)
               (reviewDecision . "APPROVED")
               (statusCheckRollup . ((state . "SUCCESS")))
               (url . "https://example.com/10")))
         (entry (magit-dash-gh-pr-dashboard--build-entry pr "myorg/myrepo"))
         (id (car entry))
         (vec (cadr entry)))
    (should (= 7 (length vec)))
    (should (equal "myorg/myrepo" (aref vec 0)))
    (should (equal "10" (aref vec 1)))
    (should (equal "My PR" (aref vec 2)))
    (should (equal "pass" (substring-no-properties (aref vec 3))))
    (should (equal "approved" (substring-no-properties (aref vec 6))))
    (should (equal 10 (plist-get id :number)))
    (should (equal "myorg/myrepo" (plist-get id :repo)))))

(ert-deftest magit-dash/build-entry-truncates-long-title ()
  "Titles longer than 36 chars are truncated with an ellipsis."
  (let* ((pr `((number . 1)
               (title . ,(make-string 50 ?X))
               (updatedAt . "")
               (comments . 0)))
         (entry (magit-dash-gh-pr-dashboard--build-entry pr "r"))
         (vec (cadr entry)))
    (should (<= (length (aref vec 2)) 37))))

;;;; magit-dash-mode-map RET binding

(ert-deftest magit-dash/ret-opens-magit-status ()
  "RET in `magit-dash-mode-map' is bound to `magit-dash-magit-status'."
  (should (eq (lookup-key magit-dash-mode-map (kbd "RET"))
              #'magit-dash-magit-status)))
;;;; magit-dash--batch-run

(ert-deftest magit-dash/batch-run-collects-ok ()
  "batch-run calls on-all-done with all results when each op returns 'ok."
  (let* ((repos (list (magit-dash-repo--make :name "r1" :path "/tmp/r1")
                      (magit-dash-repo--make :name "r2" :path "/tmp/r2")))
         (all-results nil))
    (magit-dash--batch-run
     repos
     (lambda (repo cb) (funcall cb 'ok))
     "test"
     (lambda (results) (setq all-results results)))
    (should (= 2 (length all-results)))
    (should (seq-every-p (lambda (r) (eq 'ok (cdr r))) all-results))))

(ert-deftest magit-dash/batch-run-mixed-statuses ()
  "batch-run handles a mix of 'ok, 'skipped, and 'error results."
  (let* ((statuses '(ok skipped error))
         (repos (list (magit-dash-repo--make :name "r1" :path "/tmp/r1")
                      (magit-dash-repo--make :name "r2" :path "/tmp/r2")
                      (magit-dash-repo--make :name "r3" :path "/tmp/r3")))
         (all-results nil)
         (idx 0))
    (magit-dash--batch-run
     repos
     (lambda (repo cb)
       (funcall cb (nth idx statuses))
       (setq idx (1+ idx)))
     "test"
     (lambda (results) (setq all-results results)))
    (should (= 1 (seq-count (lambda (r) (eq 'ok (cdr r))) all-results)))
    (should (= 1 (seq-count (lambda (r) (eq 'skipped (cdr r))) all-results)))
    (should (= 1 (seq-count (lambda (r) (eq 'error (cdr r))) all-results)))))

(ert-deftest magit-dash/batch-run-calls-on-all-done ()
  "batch-run calls on-all-done exactly once after the last repo completes."
  (let* ((repos (list (magit-dash-repo--make :name "r1" :path "/tmp/r1")))
         (call-count 0))
    (magit-dash--batch-run
     repos
     (lambda (_repo cb) (funcall cb 'ok))
     "test"
     (lambda (_) (setq call-count (1+ call-count))))
    (should (= 1 call-count))))

;;;; magit-dash--auto-commit-async

(ert-deftest magit-dash/auto-commit-async-skipped-when-clean ()
  "auto-commit-async returns 'skipped when git status --porcelain is empty."
  (let* ((repo (magit-dash-repo--make :name "r" :path "/tmp/r" :auto-commit t))
         (result nil))
    (cl-letf (((symbol-function 'magit-dash--run-git)
               (lambda (_path args on-success &optional _on-error)
                 (when (member "status" args)
                   (funcall on-success "")))))
      (magit-dash--auto-commit-async repo (lambda (s) (setq result s))))
    (should (eq 'skipped result))))

(ert-deftest magit-dash/auto-commit-async-ok-when-dirty ()
  "auto-commit-async returns 'ok after successful add+commit."
  (let* ((repo (magit-dash-repo--make :name "r" :path "/tmp/r" :auto-commit t))
         (result nil)
         (git-calls nil))
    (cl-letf (((symbol-function 'magit-dash--run-git)
               (lambda (_path args on-success &optional _on-error)
                 (push (car args) git-calls)
                 (cond
                  ((member "status" args) (funcall on-success " M foo.el"))
                  ((member "add" args) (funcall on-success ""))
                  ((member "commit" args) (funcall on-success ""))))))
      (magit-dash--auto-commit-async repo (lambda (s) (setq result s))))
    (should (eq 'ok result))
    (should (member "status" git-calls))
    (should (member "add" git-calls))
    (should (member "commit" git-calls))))

(ert-deftest magit-dash/auto-commit-async-error-on-add-failure ()
  "auto-commit-async returns 'error when git add fails."
  (let* ((repo (magit-dash-repo--make :name "r" :path "/tmp/r" :auto-commit t))
         (result nil))
    (cl-letf (((symbol-function 'magit-dash--run-git)
               (lambda (_path args on-success on-error)
                 (cond
                  ((member "status" args) (funcall on-success " M foo.el"))
                  ((member "add" args) (funcall on-error "error" 1))))))
      (magit-dash--auto-commit-async repo (lambda (s &optional _) (setq result s))))
    (should (eq 'error result))))

(ert-deftest magit-dash/auto-commit-async-uses-message-function ()
  "auto-commit-async uses the :auto-commit function to generate the commit message."
  (let* ((custom-msg "custom: my message")
         (repo (magit-dash-repo--make :name "r" :path "/tmp/r"
                                    :auto-commit (lambda (_r) custom-msg)))
         (commit-msg nil))
    (cl-letf (((symbol-function 'magit-dash--run-git)
               (lambda (_path args on-success &optional _on-error)
                 (cond
                  ((member "status" args) (funcall on-success " M foo.el"))
                  ((member "add" args) (funcall on-success ""))
                  ((member "commit" args)
                   (setq commit-msg (cadr (member "-m" args)))
                   (funcall on-success ""))))))
      (magit-dash--auto-commit-async repo #'ignore))
    (should (equal custom-msg commit-msg))))

;;;; magit-dash-commit-all (async)

(ert-deftest magit-dash/commit-all-async-user-error-when-none ()
  "commit-all signals user-error when no repos have :auto-commit configured."
  (let ((magit-dash-repo-list (list (magit-dash-repo--make :name "r" :path "/tmp/r"))))
    (should-error (magit-dash-commit-all) :type 'user-error)))

(ert-deftest magit-dash/commit-all-async-runs-batch ()
  "commit-all dispatches --batch-run for repos with :auto-commit set."
  (let* ((magit-dash-repo-list
          (list (magit-dash-repo--make :name "r1" :path "/tmp/r1" :auto-commit t)
                (magit-dash-repo--make :name "r2" :path "/tmp/r2")))
         (batched-repos nil))
    (cl-letf (((symbol-function 'magit-dash--batch-run)
               (lambda (repos _op _label &optional _done)
                 (setq batched-repos (seq-map #'magit-dash-repo-name repos)))))
      (magit-dash-commit-all))
    (should (= 1 (length batched-repos)))
    (should (equal "r1" (car batched-repos)))))

;;;; magit-dash-auto-sync

(ert-deftest magit-dash/auto-sync-user-error-when-none ()
  "autosync signals user-error when no repos have :auto-commit or :auto-sync."
  (let ((magit-dash-repo-list (list (magit-dash-repo--make :name "r" :path "/tmp/r"))))
    (should-error (magit-dash-auto-sync) :type 'user-error)))

(ert-deftest magit-dash/auto-sync-dispatches-single-batch ()
  "autosync runs one batch for all repos with any auto operation configured."
  (let* ((magit-dash-repo-list
          (list (magit-dash-repo--make :name "c1" :path "/tmp/c1" :auto-commit t)
                (magit-dash-repo--make :name "f1" :path "/tmp/f1" :auto-fetch t)
                (magit-dash-repo--make :name "n1" :path "/tmp/n1")))
         (batch-repos nil)
         (batch-labels nil))
    (cl-letf (((symbol-function 'magit-dash--batch-run)
               (lambda (repos _op label &optional _done)
                 (push label batch-labels)
                 (setq batch-repos repos))))
      (magit-dash-auto-sync))
    (should (= 1 (length batch-labels)))
    (should (equal "magit-dash autosync" (car batch-labels)))
    (should (= 2 (length batch-repos)))))

(ert-deftest magit-dash/auto-sync-commit-only-repo-included ()
  "autosync includes a repo with only :auto-commit in the single batch."
  (let* ((magit-dash-repo-list
          (list (magit-dash-repo--make :name "c1" :path "/tmp/c1" :auto-commit t)))
         (batch-repos nil))
    (cl-letf (((symbol-function 'magit-dash--batch-run)
               (lambda (repos _op _label &optional _done)
                 (setq batch-repos repos))))
      (magit-dash-auto-sync))
    (should (= 1 (length batch-repos)))))

;;;; magit-dash-register :hooks

(ert-deftest magit-dash/register-hooks-stored ()
  ":hooks plist is stored verbatim on the struct."
  (let ((magit-dash-repo-list nil)
        (hooks '(:fetch (:post (docs)) :sync (:operation orchestrate))))
    (magit-dash-register :name "r" :path "/tmp/r" :hooks hooks)
    (should (equal hooks (magit-dash-repo-hooks (car magit-dash-repo-list))))))

(ert-deftest magit-dash/register-hooks-rejects-unknown-operation ()
  ":hooks with an operation outside fetch/pull/commit/push/sync signals user-error."
  (let ((magit-dash-repo-list nil))
    (should-error
     (magit-dash-register :name "r" :path "/tmp/r" :hooks '(:rebase (:pre (foo))))
     :type 'user-error)))

(ert-deftest magit-dash/register-hooks-rejects-unknown-slot ()
  ":hooks with a slot outside :pre/:post/:operation signals user-error."
  (let ((magit-dash-repo-list nil))
    (should-error
     (magit-dash-register :name "r" :path "/tmp/r" :hooks '(:fetch (:during (foo))))
     :type 'user-error)))

(ert-deftest magit-dash/register-auto-sync-command-deprecation-warning ()
  ":auto-sync-command emits a one-time `display-warning'."
  (let ((magit-dash-repo-list nil)
        (warned nil))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (&rest _) (setq warned t))))
      (magit-dash-register :name "r" :path "/tmp/r" :auto-sync-command "make sync"))
    (should warned)))

(ert-deftest magit-dash/register-auto-sync-command-aliases-to-sync-operation ()
  ":auto-sync-command translates to :hooks (:sync (:operation VALUE))."
  (let ((magit-dash-repo-list nil))
    (cl-letf (((symbol-function 'display-warning) #'ignore))
      (magit-dash-register :name "r" :path "/tmp/r" :auto-sync-command "make sync"))
    (should (equal "make sync"
                    (magit-dash--repo-operation (car magit-dash-repo-list) :sync)))))

(ert-deftest magit-dash/register-auto-sync-command-merges-with-explicit-hooks ()
  "Explicit :hooks and the :auto-sync-command alias merge rather than clobber."
  (let ((magit-dash-repo-list nil))
    (cl-letf (((symbol-function 'display-warning) #'ignore))
      (magit-dash-register :name "r" :path "/tmp/r"
                            :auto-sync-command "make sync"
                            :hooks '(:sync (:pre (notify)))))
    (let ((hooks (magit-dash-repo-hooks (car magit-dash-repo-list))))
      (should (equal "make sync" (magit-dash--hook-slot hooks :sync :operation)))
      (should (equal '(notify) (magit-dash--hook-slot hooks :sync :pre))))))

;;;; magit-dash-set-global-hooks

(ert-deftest magit-dash/set-global-hooks-stores-value ()
  (let (magit-dash-global-hooks)
    (magit-dash-set-global-hooks '(:fetch (:post (docs))))
    (should (equal '(:fetch (:post (docs))) magit-dash-global-hooks))))

(ert-deftest magit-dash/set-global-hooks-rejects-operation ()
  ":operation is not valid in `magit-dash-global-hooks'."
  (let (magit-dash-global-hooks)
    (should-error
     (magit-dash-set-global-hooks '(:sync (:operation orchestrate)))
     :type 'user-error)))

;;;; magit-dash--run-target

(ert-deftest magit-dash/run-target-string-runs-shell-command ()
  (let ((repo (magit-dash-repo--make :name "r" :path "/tmp/r"))
        (ran nil))
    (cl-letf (((symbol-function 'magit-dash--run-shell-string-async)
               (lambda (cmd _path cb) (setq ran cmd) (funcall cb 'ok))))
      (magit-dash--run-target repo "echo hi" (lambda (status &optional _) (should (eq 'ok status)))))
    (should (equal "echo hi" ran))))

(ert-deftest magit-dash/run-target-function-called-with-repo-and-callback ()
  (let* ((repo (magit-dash-repo--make :name "r" :path "/tmp/r"))
         (called-with nil))
    (magit-dash--run-target
     repo (lambda (r cb) (setq called-with r) (funcall cb 'ok))
     #'ignore)
    (should (eq repo called-with))))

(ert-deftest magit-dash/run-target-function-zero-args-called ()
  (let* ((repo (magit-dash-repo--make :name "r" :path "/tmp/r"))
         (called nil)
         (got-dir nil))
    (magit-dash--run-target
     repo (lambda () (setq called t got-dir default-directory))
     (lambda (status &optional _)
       (should (eq 'ok status))))
    (should called)
    (should (string-suffix-p "tmp/r/" got-dir))))

(ert-deftest magit-dash/run-target-function-one-arg-called ()
  (let* ((repo (magit-dash-repo--make :name "r" :path "/tmp/r"))
         (called-with nil))
    (magit-dash--run-target
     repo (lambda (r) (setq called-with r))
     (lambda (status &optional _)
       (should (eq 'ok status))))
    (should (eq repo called-with))))

(ert-deftest magit-dash/save-project-buffers-falls-back-to-project-el ()
  (let* ((projectile-mode nil)
         (project-current-called nil)
         (project-buffers-called nil)
         (saved nil)
         (buf (get-buffer-create "*magit-dash-test-buf*")))
    (with-current-buffer buf
      (setq-local buffer-file-name "/tmp/fake-file")
      (set-buffer-modified-p t))
    (cl-letf (((symbol-function 'project-current)
               (lambda (&optional _maybe-prompt _dir) (setq project-current-called t) 'fake-project))
              ((symbol-function 'project-buffers)
               (lambda (p)
                 (should (eq p 'fake-project))
                 (setq project-buffers-called t)
                 (list buf)))
              ((symbol-function 'save-buffer)
               (lambda () (setq saved t))))
      (unwind-protect
          (cl-letf (((symbol-function 'magit-dash--resolve-repo-path)
                     (lambda () "/tmp/")))
            (magit-dash-save-project-buffers)
            (should project-current-called)
            (should project-buffers-called)
            (should saved))
        (with-current-buffer buf
          (set-buffer-modified-p nil))
        (kill-buffer buf)))))

(ert-deftest magit-dash/drop-stashes-calls-magit-stash-clear ()
  "drop-stashes resolves repo path and calls magit-stash-clear interactively."
  (let ((resolved nil)
        (stash-cleared nil))
    (cl-letf (((symbol-function 'magit-dash--resolve-repo-path)
               (lambda () (setq resolved t) "/tmp/test-repo"))
              ((symbol-function 'magit-stash-clear)
               (lambda ()
                 (interactive)
                 (setq stash-cleared (equal default-directory "/tmp/test-repo/")))))
      (magit-dash-drop-stashes)
      (should resolved)
      (should stash-cleared))))

(ert-deftest magit-dash/run-target-symbol-resolves-via-commands ()
  (let ((repo (magit-dash-repo--make :name "r" :path "/tmp/r"
                                     :commands '((docs . "make docs"))))
        (ran nil))
    (cl-letf (((symbol-function 'magit-dash--run-shell-string-async)
               (lambda (cmd _path cb) (setq ran cmd) (funcall cb 'ok))))
      (magit-dash--run-target repo 'docs #'ignore))
    (should (equal "make docs" ran))))

(ert-deftest magit-dash/run-target-symbol-resolves-via-commands-with-background ()
  (let ((repo (magit-dash-repo--make :name "r" :path "/tmp/r"
                                     :commands '((docs . ("make docs" :background t)))))
        (ran nil))
    (cl-letf (((symbol-function 'magit-dash--run-shell-string-async)
               (lambda (cmd _path cb) (setq ran cmd) (funcall cb 'ok))))
      (magit-dash--run-target repo 'docs #'ignore))
    (should (equal "make docs" ran))))

(ert-deftest magit-dash/run-target-symbol-not-in-commands-errors ()
  (let ((repo (magit-dash-repo--make :name "r" :path "/tmp/r"))
        (result nil))
    (magit-dash--run-target repo 'missing (lambda (status &optional text) (setq result (cons status text))))
    (should (eq 'error (car result)))))

(ert-deftest magit-dash/run-target-unsupported-type-errors ()
  (let ((repo (magit-dash-repo--make :name "r" :path "/tmp/r"))
        (result nil))
    (magit-dash--run-target repo 42 (lambda (status &optional _) (setq result status)))
    (should (eq 'error result))))

;;;; magit-dash--run-operation: hooks and operation resolution

(ert-deftest magit-dash/run-operation-runs-pre-then-operation-then-post ()
  "pre, operation, and post run in that order for a single op."
  (let* ((order nil)
         (repo (magit-dash-repo--make
                :name "r" :path "/tmp/r"
                :hooks (list :fetch (list :pre (list (lambda (_r cb) (push 'pre order) (funcall cb 'ok)))
                                          :post (list (lambda (_r cb) (push 'post order) (funcall cb 'ok)))
                                          :operation (lambda (_r cb) (push 'op order) (funcall cb 'ok)))))))
    (magit-dash--run-operation repo :fetch (lambda (&rest _) (push 'done order)))
    (should (equal '(done post op pre) order))))

(ert-deftest magit-dash/run-operation-global-outermost-for-pre ()
  "Global :pre hooks run before repo :pre hooks."
  (let* ((order nil)
         (magit-dash-global-hooks
          (list :fetch (list :pre (list (lambda (_r cb) (push 'global order) (funcall cb 'ok))))))
         (repo (magit-dash-repo--make
                :name "r" :path "/tmp/r"
                :hooks (list :fetch (list :pre (list (lambda (_r cb) (push 'local order) (funcall cb 'ok)))
                                          :operation (lambda (_r cb) (funcall cb 'ok)))))))
    (magit-dash--run-operation repo :fetch #'ignore)
    (should (equal '(local global) order))))

(ert-deftest magit-dash/run-operation-repo-outermost-for-post ()
  "Repo :post hooks run before global :post hooks (global closes last)."
  (let* ((order nil)
         (magit-dash-global-hooks
          (list :fetch (list :post (list (lambda (_r cb) (push 'global order) (funcall cb 'ok))))))
         (repo (magit-dash-repo--make
                :name "r" :path "/tmp/r"
                :hooks (list :fetch (list :post (list (lambda (_r cb) (push 'local order) (funcall cb 'ok)))
                                          :operation (lambda (_r cb) (funcall cb 'ok)))))))
    (magit-dash--run-operation repo :fetch #'ignore)
    (should (equal '(global local) order))))

(ert-deftest magit-dash/run-operation-multiple-pre-targets-run-in-order ()
  (let* ((order nil)
         (repo (magit-dash-repo--make
                :name "r" :path "/tmp/r"
                :hooks (list :fetch (list :pre (list (lambda (_r cb) (push 'a order) (funcall cb 'ok))
                                                      (lambda (_r cb) (push 'b order) (funcall cb 'ok)))
                                          :operation (lambda (_r cb) (funcall cb 'ok)))))))
    (magit-dash--run-operation repo :fetch #'ignore)
    (should (equal '(b a) order))))

(ert-deftest magit-dash/run-operation-pre-failure-skips-operation ()
  "A `:pre' hook signaling `error' skips the operation; ON-COMPLETE gets `skipped'."
  (let* ((op-ran nil)
         (result nil)
         (repo (magit-dash-repo--make
                :name "r" :path "/tmp/r"
                :hooks (list :fetch (list :pre (list (lambda (_r cb) (funcall cb 'error "boom")))
                                          :operation (lambda (_r cb) (setq op-ran t) (funcall cb 'ok)))))))
    (magit-dash--run-operation repo :fetch (lambda (status &optional text) (setq result (cons status text))))
    (should (null op-ran))
    (should (eq 'skipped (car result)))))

(ert-deftest magit-dash/run-operation-post-failure-does-not-fail-overall-status ()
  "A `:post' hook failure is not reflected in the status passed to ON-COMPLETE."
  (let* ((result nil)
         (repo (magit-dash-repo--make
                :name "r" :path "/tmp/r"
                :hooks (list :fetch (list :post (list (lambda (_r cb) (funcall cb 'error "boom")))
                                          :operation (lambda (_r cb) (funcall cb 'ok)))))))
    (magit-dash--run-operation repo :fetch (lambda (status &optional _) (setq result status)))
    (should (eq 'ok result))))

(ert-deftest magit-dash/run-operation-post-does-not-run-after-error ()
  (let* ((post-ran nil)
         (repo (magit-dash-repo--make
                :name "r" :path "/tmp/r"
                :hooks (list :fetch (list :post (list (lambda (_r cb) (setq post-ran t) (funcall cb 'ok)))
                                          :operation (lambda (_r cb) (funcall cb 'error "op failed")))))))
    (magit-dash--run-operation repo :fetch #'ignore)
    (should (null post-ran))))

(ert-deftest magit-dash/run-operation-step-uses-operation-override-when-auto-flag-set ()
  "A step's `:operation' override runs instead of the default while the step
is still gated by its own auto-* flag."
  (let* ((default-ran nil) (override-ran nil)
         (repo (magit-dash-repo--make
                :name "r" :path "/tmp/r" :auto-fetch t
                :hooks (list :fetch (list :operation (lambda (_r cb) (setq override-ran t) (funcall cb 'ok)))))))
    (cl-letf (((symbol-function 'magit-dash--auto-fetch-async)
               (lambda (_r cb) (setq default-ran t) (funcall cb 'ok))))
      (magit-dash--run-operation repo :fetch #'ignore))
    (should override-ran)
    (should (null default-ran))))

(ert-deftest magit-dash/sync-operation-override-bypasses-entire-default-pipeline ()
  "A `:sync' `:operation' override replaces fetch/pull/commit/push entirely —
they are not consulted even when their own auto-* flags are set."
  (let* ((step-ran nil) (override-ran nil)
         (repo (magit-dash-repo--make
                :name "r" :path "/tmp/r"
                :auto-fetch t :auto-pull t :auto-commit t :auto-push t
                :hooks (list :sync (list :operation (lambda (_r cb) (setq override-ran t) (funcall cb 'ok)))))))
    (cl-letf (((symbol-function 'magit-dash--auto-fetch-async)
               (lambda (_r cb) (setq step-ran t) (funcall cb 'ok)))
              ((symbol-function 'magit-dash--auto-pull-async)
               (lambda (_r cb) (setq step-ran t) (funcall cb 'ok)))
              ((symbol-function 'magit-dash--auto-commit-async)
               (lambda (_r cb) (setq step-ran t) (funcall cb 'ok)))
              ((symbol-function 'magit-dash--auto-push-async)
               (lambda (_r cb) (setq step-ran t) (funcall cb 'ok))))
      (magit-dash--auto-sync-async repo #'ignore))
    (should override-ran)
    (should (null step-ran))))

;;;; magit-dash--auto-sync-pipeline-async: default `:sync' implementation

(ert-deftest magit-dash/auto-sync-pipeline-matches-legacy-behavior-fetch-pull ()
  "The migrated default pipeline runs fetch then pull for :auto-pull, same as before."
  (let* ((order nil)
         (repo (magit-dash-repo--make :name "r" :path "/tmp/r" :auto-pull t)))
    (cl-letf (((symbol-function 'magit-dash--auto-fetch-async)
               (lambda (_r cb) (push 'fetch order) (funcall cb 'ok)))
              ((symbol-function 'magit-dash--auto-pull-async)
               (lambda (_r cb) (push 'pull order) (funcall cb 'ok))))
      (magit-dash--auto-sync-async repo #'ignore))
    (should (equal '(pull fetch) order))))

(ert-deftest magit-dash/auto-sync-pipeline-aborts-on-step-error ()
  (let* ((order nil)
         (repo (magit-dash-repo--make :name "r" :path "/tmp/r" :auto-fetch t :auto-commit t)))
    (cl-letf (((symbol-function 'magit-dash--auto-fetch-async)
               (lambda (_r cb) (push 'fetch order) (funcall cb 'error "boom")))
              ((symbol-function 'magit-dash--auto-commit-async)
               (lambda (_r cb) (push 'commit order) (funcall cb 'ok))))
      (magit-dash--auto-sync-async repo #'ignore))
    (should (equal '(fetch) order))))

(ert-deftest magit-dash/auto-sync-pipeline-skipped-when-no-steps-configured ()
  (let ((repo (magit-dash-repo--make :name "r" :path "/tmp/r"))
        (result nil))
    (magit-dash--auto-sync-async repo (lambda (status &optional _) (setq result status)))
    (should (eq 'skipped result))))

;;;; magit-dash--has-sync-configured-p

(ert-deftest magit-dash/has-sync-configured-p-true-for-sync-operation-only ()
  "A repo with only a `:sync' `:operation' override (no auto-* flags) counts as configured."
  (let ((repo (magit-dash-repo--make
               :name "r" :path "/tmp/r"
               :hooks (list :sync (list :operation "make sync")))))
    (should (magit-dash--has-sync-configured-p repo))))

(ert-deftest magit-dash/has-sync-configured-p-false-when-nothing-set ()
  (let ((repo (magit-dash-repo--make :name "r" :path "/tmp/r")))
    (should (null (magit-dash--has-sync-configured-p repo)))))

;;;; Builder and agent-shell commands

(ert-deftest magit-dash/builder-delegates-to-builder ()
  "builder command invokes builder-compile-project in the repo directory."
  (let ((called-in nil))
    (cl-letf (((symbol-function 'magit-dash--repo-at-point)
               (lambda () (magit-dash-repo--make :name "r" :path "/tmp/r")))
              ((symbol-function 'builder-compile-project)
               (lambda () (interactive) (setq called-in default-directory))))
      (magit-dash-builder))
    (should (string-prefix-p "/tmp/r" (or called-in "")))))

(ert-deftest magit-dash/agent-shell-queue-callable ()
  "agent-shell-queue command calls agent-shell-queue-buffer-open."
  (let ((called nil))
    (cl-letf (((symbol-function 'magit-dash--repo-at-point)
               (lambda () (magit-dash-repo--make :name "r" :path "/tmp/r")))
              ((symbol-function 'agent-shell-queue-buffer-open)
               (lambda () (interactive) (setq called t))))
      (magit-dash-agent-shell-queue))
    (should called)))

;;;; magit-dash--parse-worktrees

(defconst magit-dash-test--worktree-output
  '("worktree /tmp/main"
    "HEAD abc123def456"
    "branch refs/heads/main"
    ""
    "worktree /tmp/wt1"
    "HEAD 111111aaaaaa"
    "branch refs/heads/feature-1"
    ""
    "worktree /tmp/wt2"
    "HEAD 222222bbbbbb"
    "detached"
    "")
  "Sample `git worktree list --porcelain' output as a list of lines.")

(ert-deftest magit-dash/parse-worktrees-skips-main ()
  "parse-worktrees omits the main worktree (first block)."
  (let ((result (magit-dash--parse-worktrees
                 "/tmp/main"
                 magit-dash-test--worktree-output)))
    (should (= 2 (length result)))
    (should-not (seq-find (lambda (r) (equal "/tmp/main" (magit-dash-repo-path r)))
                          result))))

(ert-deftest magit-dash/parse-worktrees-paths ()
  "parse-worktrees sets correct paths on returned structs."
  (let ((result (magit-dash--parse-worktrees
                 "/tmp/main"
                 magit-dash-test--worktree-output)))
    (should (equal "/tmp/wt1" (magit-dash-repo-path (nth 0 result))))
    (should (equal "/tmp/wt2" (magit-dash-repo-path (nth 1 result))))))

(ert-deftest magit-dash/parse-worktrees-names ()
  "parse-worktrees constructs names from main-repo basename and branch."
  (let ((result (magit-dash--parse-worktrees
                 "/tmp/main"
                 magit-dash-test--worktree-output)))
    (should (equal "main@feature-1" (magit-dash-repo-name (nth 0 result))))
    (should (equal "main@detached" (magit-dash-repo-name (nth 1 result))))))

(ert-deftest magit-dash/parse-worktrees-worktree-flag ()
  "parse-worktrees sets :worktree t on all returned structs."
  (let ((result (magit-dash--parse-worktrees
                 "/tmp/main"
                 magit-dash-test--worktree-output)))
    (should (seq-every-p #'magit-dash-repo-worktree result))))

(ert-deftest magit-dash/parse-worktrees-empty-output ()
  "parse-worktrees returns nil when only the main worktree is listed."
  (let ((result (magit-dash--parse-worktrees
                 "/tmp/main"
                 '("worktree /tmp/main" "HEAD abc123" "branch refs/heads/main" ""))))
    (should (null result))))

(ert-deftest magit-dash/parse-worktrees-uses-registered-name ()
  "parse-worktrees uses the registered repo's :name, not the path basename, when they differ."
  (let* ((magit-dash-repo-list
          (list (magit-dash-repo--make :name "custom-name" :path "/tmp/main")))
         (result (magit-dash--parse-worktrees
                  "/tmp/main"
                  magit-dash-test--worktree-output)))
    (should (equal "custom-name@feature-1" (magit-dash-repo-name (nth 0 result))))
    (should (equal "custom-name@detached" (magit-dash-repo-name (nth 1 result))))))

(ert-deftest magit-dash/parse-worktrees-inherits-parent-sort-hint ()
  "parse-worktrees copies the registered parent's :sort-hint onto each worktree."
  (let* ((magit-dash-repo-list
          (list (magit-dash-repo--make :name "main" :path "/tmp/main" :sort-hint 7)))
         (result (magit-dash--parse-worktrees
                  "/tmp/main"
                  magit-dash-test--worktree-output)))
    (should (seq-every-p (lambda (r) (= 7 (magit-dash-repo-sort-hint r))) result))))

(ert-deftest magit-dash/parse-worktrees-no-sort-hint-when-unregistered ()
  "parse-worktrees leaves :sort-hint nil when the parent isn't a registered repo."
  (let* ((magit-dash-repo-list nil)
         (result (magit-dash--parse-worktrees
                  "/tmp/main"
                  magit-dash-test--worktree-output)))
    (should (seq-every-p (lambda (r) (null (magit-dash-repo-sort-hint r))) result))))

(ert-deftest magit-dash/parse-worktrees-inherits-parent-include-ci ()
  "parse-worktrees copies the registered parent's :include-ci onto each worktree."
  (let* ((magit-dash-repo-list
          (list (magit-dash-repo--make :name "main" :path "/tmp/main" :include-ci t)))
         (result (magit-dash--parse-worktrees
                  "/tmp/main"
                  magit-dash-test--worktree-output)))
    (should (seq-every-p #'magit-dash-repo-include-ci result))))

(ert-deftest magit-dash/parse-worktrees-no-include-ci-when-parent-disabled ()
  "parse-worktrees leaves :include-ci nil when the parent doesn't have it set."
  (let* ((magit-dash-repo-list
          (list (magit-dash-repo--make :name "main" :path "/tmp/main")))
         (result (magit-dash--parse-worktrees
                  "/tmp/main"
                  magit-dash-test--worktree-output)))
    (should (seq-every-p (lambda (r) (null (magit-dash-repo-include-ci r))) result))))

;;;; magit-dash--sorted-repos with worktrees

(ert-deftest magit-dash/sorted-repos-appends-worktrees ()
  "sorted-repos places discovered worktrees immediately after their parent."
  (let* ((main (magit-dash-repo--make :name "main" :path "/tmp/main"))
         (wt (magit-dash-repo--make :name "main@feat" :path "/tmp/wt" :worktree t))
         (magit-dash-gh--cache (make-hash-table :test #'equal)))
    (magit-dash-gh--cache-set "/tmp/main" :worktrees (list wt))
    (let ((result (magit-dash--sorted-repos (list main))))
      (should (= 2 (length result)))
      (should (equal "main" (magit-dash-repo-name (nth 0 result))))
      (should (equal "main@feat" (magit-dash-repo-name (nth 1 result)))))))

;;;; Column configuration

(ert-deftest magit-dash/column-enabled-defaults ()
  "CI is enabled by default; upstream is disabled by default in magit-dash-columns."
  (should (eq t (alist-get 'ci magit-dash-columns)))
  (should (eq nil (alist-get 'upstream magit-dash-columns))))

(ert-deftest magit-dash/savehist-additional-variables ()
  "magit-dash-columns is included in savehist-additional-variables."
  (should (memq 'magit-dash-columns savehist-additional-variables)))

(ert-deftest magit-dash/column-disabled ()
  "A disabled column is excluded from active-columns."
  (let ((magit-dash-columns
         '((name . t) (branch . nil) (fetched . t) (status . t) (worktree . t))))
    (should-not (magit-dash--column-enabled-p 'branch))
    (should-not (member 'branch (magit-dash--active-columns)))))

(ert-deftest magit-dash/build-format-omits-disabled-columns ()
  "build-format produces a vector that excludes disabled columns."
  (let ((magit-dash-columns
         '((name . t) (branch . nil) (fetched . t) (updated . nil) (ci . nil) (status . t)
           (worktree . nil) (sync . nil) (cached . nil)))
        (repos (list (magit-dash-repo--make :name "r" :path "/tmp/r"))))
    (let ((fmt (magit-dash--build-format repos)))
      (should (= 3 (length fmt)))
      (should-not (seq-find (lambda (col) (equal "Branch" (car col))) (append fmt nil)))
      (should-not (seq-find (lambda (col) (equal "Type" (car col))) (append fmt nil))))))

(ert-deftest magit-dash/build-entry-matches-format ()
  "build-entry vector length matches active-column count."
  (let ((magit-dash-columns
         '((name . t) (branch . t) (fetched . nil) (status . t) (worktree . nil)))
        (magit-dash--worktree-map (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-dash--get-stats-fast)
               (lambda (_)
                 (list :branch "main" :ahead 0 :behind 0 :dirty nil :fetch-age 60.0
                       :head-hash "abc" :recent-log "" :remote-origin nil
                       :uncommitted-files nil))))
      (let* ((repo (magit-dash-repo--make :name "r" :path "/tmp/r"))
             (entry (magit-dash--build-entry repo))
             (active (magit-dash--active-columns)))
        (should (= (length active) (length (cadr entry))))))))

;;;; New command user-error conditions

(ert-deftest magit-dash/visit-buffer-user-error-when-none ()
  "visit-buffer signals user-error when no buffers visit the repo."
  (let ((magit-dash-repo-list nil))
    (cl-letf (((symbol-function 'magit-dash--repo-at-point)
               (lambda () (magit-dash-repo--make :name "r" :path "/nonexistent/path")))
              ((symbol-function 'buffer-list) (lambda (&optional _) nil)))
      (should-error (magit-dash-visit-buffer) :type 'user-error))))

(ert-deftest magit-dash/worktree-delete-user-error-when-not-worktree ()
  "worktree-delete signals user-error when the entry is not a worktree."
  (cl-letf (((symbol-function 'magit-dash--repo-at-point)
             (lambda () (magit-dash-repo--make :name "r" :path "/tmp/r"))))
    (should-error (magit-dash-worktree-delete) :type 'user-error)))

(ert-deftest magit-dash/worktree-add-user-error-when-at-worktree ()
  "worktree-add signals user-error when the entry is itself a worktree."
  (cl-letf (((symbol-function 'magit-dash--repo-at-point)
             (lambda () (magit-dash-repo--make :name "r@feat" :path "/tmp/wt" :worktree t))))
    (should-error (magit-dash-worktree-add) :type 'user-error)))

;;;; worktree branch field

(ert-deftest magit-dash/parse-worktrees-stores-branch ()
  "parse-worktrees stores branch name in the :branch struct slot."
  (let ((result (magit-dash--parse-worktrees
                 "/tmp/main"
                 magit-dash-test--worktree-output)))
    (should (equal "feature-1" (magit-dash-repo-branch (nth 0 result))))
    (should (equal "detached"  (magit-dash-repo-branch (nth 1 result))))))

(ert-deftest magit-dash/build-entry-branch-falls-back-to-struct ()
  "build-entry uses struct :branch when stats return empty string."
  (let ((magit-dash-columns
         '((name . t) (branch . t) (fetched . nil) (ci . nil) (status . nil)
           (worktree . nil) (sync . nil) (cached . nil)))
        (magit-dash-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-dash--get-stats-fast)
               (lambda (_) (list :branch "" :ahead 0 :behind 0 :dirty nil :fetch-age nil
                                 :head-hash "abc" :recent-log "" :remote-origin nil
                                 :uncommitted-files nil))))
      (let* ((repo (magit-dash-repo--make :name "r@feat" :path "/tmp/wt"
                                        :worktree t :branch "feat"))
             (entry (magit-dash--build-entry repo))
             (branch-cell (aref (cadr entry) 1)))
        (should (equal "feat" (substring-no-properties branch-cell)))))))

(ert-deftest magit-dash/build-entry-branch-uses-stats-when-available ()
  "build-entry uses stats :branch when non-empty, even for worktrees."
  (let ((magit-dash-columns
         '((name . t) (branch . t) (fetched . nil) (ci . nil) (status . nil)
           (worktree . nil) (sync . nil) (cached . nil)))
        (magit-dash-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-dash--get-stats-fast)
               (lambda (_) (list :branch "live-branch" :ahead 0 :behind 0 :dirty nil :fetch-age nil
                                 :head-hash "abc" :recent-log "" :remote-origin nil
                                 :uncommitted-files nil))))
      (let* ((repo (magit-dash-repo--make :name "r@old" :path "/tmp/wt"
                                        :worktree t :branch "old"))
             (entry (magit-dash--build-entry repo))
             (branch-cell (aref (cadr entry) 1)))
        (should (equal "live-branch" (substring-no-properties branch-cell)))))))

(ert-deftest magit-dash/build-entry-name-replaces-branch-with-placeholder ()
  "build-entry's Name column shows <B> in place of the branch for worktree rows,
since the Branch column already displays it."
  (let ((magit-dash-columns
         '((name . t) (branch . t) (fetched . nil) (ci . nil) (status . nil)
           (worktree . nil) (sync . nil) (cached . nil)))
        (magit-dash-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-dash--get-stats-fast)
               (lambda (_) (list :branch nil :ahead 0 :behind 0 :dirty nil :fetch-age nil
                                 :head-hash "abc" :recent-log "" :remote-origin nil
                                 :uncommitted-files nil))))
      (let* ((repo (magit-dash-repo--make :name "main@feature-1" :path "/tmp/wt"
                                        :worktree t :branch "feature-1"))
             (entry (magit-dash--build-entry repo))
             (name-cell (aref (cadr entry) 0)))
        (should (equal "main@<B>" (substring-no-properties name-cell)))))))

(ert-deftest magit-dash/build-entry-name-unchanged-for-non-worktree ()
  "build-entry's Name column is untouched for a plain (non-worktree) repo."
  (let ((magit-dash-columns
         '((name . t) (branch . t) (fetched . nil) (ci . nil) (status . nil)
           (worktree . nil) (sync . nil) (cached . nil)))
        (magit-dash-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-dash--get-stats-fast)
               (lambda (_) (list :branch "main" :ahead 0 :behind 0 :dirty nil :fetch-age nil
                                 :head-hash "abc" :recent-log "" :remote-origin nil
                                 :uncommitted-files nil))))
      (let* ((repo (magit-dash-repo--make :name "main" :path "/tmp/main"))
             (entry (magit-dash--build-entry repo))
             (name-cell (aref (cadr entry) 0)))
        (should (equal "main" (substring-no-properties name-cell)))))))

;;;; magit-dash--parse-submodules

(defconst magit-dash-test--submodule-output
  '(" abc1234def5 vendor/lib (v1.2)"
    " 0000000000a sub/other (HEAD)"
    "-deadbeef00b uninit-mod"
    "+cafebabe001 modified-sub (HEAD)")
  "Sample `git submodule status' output as a list of lines.")

(ert-deftest magit-dash/parse-submodules-count ()
  "parse-submodules returns one struct per accessible submodule."
  (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t)))
    (let ((result (magit-dash--parse-submodules
                   "/tmp/main"
                   magit-dash-test--submodule-output)))
      (should (= 4 (length result))))))

(ert-deftest magit-dash/parse-submodules-paths ()
  "parse-submodules sets absolute paths on returned structs."
  (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t)))
    (let ((result (magit-dash--parse-submodules
                   "/tmp/main"
                   magit-dash-test--submodule-output)))
      (should (equal "/tmp/main/vendor/lib" (magit-dash-repo-path (nth 0 result))))
      (should (equal "/tmp/main/sub/other"  (magit-dash-repo-path (nth 1 result)))))))

(ert-deftest magit-dash/parse-submodules-names ()
  "parse-submodules formats names as \"parent<submod>\"."
  (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t)))
    (let ((result (magit-dash--parse-submodules
                   "/tmp/main"
                   magit-dash-test--submodule-output)))
      (should (equal "main<lib>"      (magit-dash-repo-name (nth 0 result))))
      (should (equal "main<other>"    (magit-dash-repo-name (nth 1 result))))
      (should (equal "main<uninit-mod>"  (magit-dash-repo-name (nth 2 result))))
      (should (equal "main<modified-sub>" (magit-dash-repo-name (nth 3 result)))))))

(ert-deftest magit-dash/parse-submodules-uses-registered-name ()
  "parse-submodules uses registered repo names for both parent and submodule."
  (let ((magit-dash-repo-list
         (list (magit-dash-repo--make :name "my-parent" :path "/tmp/main")
               (magit-dash-repo--make :name "my-lib" :path "/tmp/main/vendor/lib"))))
    (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t)))
      (let ((result (magit-dash--parse-submodules "/tmp/main"
                      magit-dash-test--submodule-output)))
        (should (equal "my-parent<my-lib>" (magit-dash-repo-name (nth 0 result))))
        (should (equal "my-parent<other>"  (magit-dash-repo-name (nth 1 result))))))))

(ert-deftest magit-dash/parse-submodules-submodule-flag ()
  "parse-submodules sets :submodule t on all returned structs."
  (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t)))
    (let ((result (magit-dash--parse-submodules
                   "/tmp/main"
                   magit-dash-test--submodule-output)))
      (should (seq-every-p #'magit-dash-repo-submodule result)))))

(ert-deftest magit-dash/parse-submodules-skips-missing-paths ()
  "parse-submodules marks entries with non-existent paths as :submodule \\='missing."
  (cl-letf (((symbol-function 'file-directory-p) (lambda (_) nil)))
    (let ((result (magit-dash--parse-submodules
                   "/tmp/main"
                   magit-dash-test--submodule-output)))
      (should (= 4 (length result)))
      (should (seq-every-p (lambda (r) (eq 'missing (magit-dash-repo-submodule r)))
                           result)))))

(ert-deftest magit-dash/parse-submodules-empty-output ()
  "parse-submodules returns nil for empty output."
  (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t)))
    (should (null (magit-dash--parse-submodules "/tmp/main" '())))))

;;;; --sorted-repos with submodules

(ert-deftest magit-dash/sorted-repos-appends-submodules ()
  "sorted-repos places discovered submodules after their parent."
  (let* ((main (magit-dash-repo--make :name "main" :path "/tmp/main"))
         (mod (magit-dash-repo--make :name "main<lib>" :path "/tmp/main/lib" :submodule t))
         (magit-dash-gh--cache (make-hash-table :test #'equal))
         (magit-dash-show-discovered-submodules t))
    (magit-dash-gh--cache-set "/tmp/main" :worktrees nil)
    (magit-dash-gh--cache-set "/tmp/main" :submodules (list mod))
    (let ((result (magit-dash--sorted-repos (list main))))
      (should (= 2 (length result)))
      (should (equal "main"      (magit-dash-repo-name (nth 0 result))))
      (should (equal "main<lib>" (magit-dash-repo-name (nth 1 result)))))))

(ert-deftest magit-dash/sorted-repos-appends-worktrees-and-submodules ()
  "sorted-repos appends both worktrees and submodules after parent, in that order."
  (let* ((main (magit-dash-repo--make :name "main" :path "/tmp/main"))
         (wt   (magit-dash-repo--make :name "main@feat" :path "/tmp/wt" :worktree t))
         (mod  (magit-dash-repo--make :name "main<lib>" :path "/tmp/main/lib" :submodule t))
         (magit-dash-gh--cache (make-hash-table :test #'equal))
         (magit-dash-show-discovered-worktrees t)
         (magit-dash-show-discovered-submodules t))
    (magit-dash-gh--cache-set "/tmp/main" :worktrees (list wt))
    (magit-dash-gh--cache-set "/tmp/main" :submodules (list mod))
    (let ((result (magit-dash--sorted-repos (list main))))
      (should (= 3 (length result)))
      (should (equal "main"      (magit-dash-repo-name (nth 0 result))))
      (should (equal "main@feat" (magit-dash-repo-name (nth 1 result))))
      (should (equal "main<lib>" (magit-dash-repo-name (nth 2 result)))))))

(ert-deftest magit-dash/sorted-repos-deduplicates-registered-submodules ()
  "sorted-repos suppresses auto-discovered submodule rows whose path is already
in magit-dash-repo-list, preventing duplicate rows."
  (let* ((main (magit-dash-repo--make :name "main" :path "/tmp/main"))
         (lib  (magit-dash-repo--make :name "lib"  :path "/tmp/main/lib"))
         (mod  (magit-dash-repo--make :name "main<lib>" :path "/tmp/main/lib" :submodule t))
         (magit-dash-gh--cache (make-hash-table :test #'equal))
         (magit-dash-repo-list (list main lib))
         (magit-dash-show-discovered-submodules t))
    (magit-dash-gh--cache-set "/tmp/main" :worktrees nil)
    (magit-dash-gh--cache-set "/tmp/main" :submodules (list mod))
    (let ((result (magit-dash--sorted-repos (list main lib))))
      (should (= 2 (length result)))
      (should (equal "main" (magit-dash-repo-name (nth 0 result))))
      (should (equal "lib"  (magit-dash-repo-name (nth 1 result)))))))

(ert-deftest magit-dash/sorted-repos-keeps-unregistered-submodules ()
  "sorted-repos still appends auto-discovered submodules that are not in magit-dash-repo-list."
  (let* ((main (magit-dash-repo--make :name "main" :path "/tmp/main"))
         (mod  (magit-dash-repo--make :name "main<lib>" :path "/tmp/main/lib" :submodule t))
         (magit-dash-gh--cache (make-hash-table :test #'equal))
         (magit-dash-repo-list (list main))
         (magit-dash-show-discovered-submodules t))
    (magit-dash-gh--cache-set "/tmp/main" :worktrees nil)
    (magit-dash-gh--cache-set "/tmp/main" :submodules (list mod))
    (let ((result (magit-dash--sorted-repos (list main))))
      (should (= 2 (length result)))
      (should (equal "main"      (magit-dash-repo-name (nth 0 result))))
      (should (equal "main<lib>" (magit-dash-repo-name (nth 1 result)))))))

;;;; magit-dash--resolve-repo-path

(ert-deftest magit-dash/resolve-repo-path-prefers-dashboard-point ()
  "resolve-repo-path uses the repo at point over `magit-toplevel' when both are available."
  (let ((repo (magit-dash-repo--make :name "r" :path "/tmp/at-point")))
    (cl-letf (((symbol-function 'magit-dash--repo-at-point-p) (lambda () t))
              ((symbol-function 'magit-dash--repo-at-point) (lambda () repo))
              ((symbol-function 'magit-toplevel) (lambda () "/tmp/toplevel")))
      (should (equal "/tmp/at-point" (magit-dash--resolve-repo-path))))))

(ert-deftest magit-dash/resolve-repo-path-falls-back-to-toplevel ()
  "resolve-repo-path uses `magit-toplevel' when there's no dashboard repo at point."
  (cl-letf (((symbol-function 'magit-dash--repo-at-point-p) (lambda () nil))
            ((symbol-function 'magit-toplevel) (lambda () "/tmp/toplevel")))
    (should (equal "/tmp/toplevel" (magit-dash--resolve-repo-path)))))

(ert-deftest magit-dash/repo-annotation-formats-path-and-cache ()
  "magit-dash--repo-annotation formats path and enriches with cached stats, CI, and PRs."
  (let ((magit-dash-gh--cache (make-hash-table :test #'equal))
        (r (magit-dash-repo--make :name "alpha" :path "/tmp/alpha")))
    ;; 1. Plain path when no extra cache data
    (should (equal (magit-dash--repo-annotation r) "/tmp/alpha"))
    ;; 2. With cached stats (ahead/behind/dirty)
    (magit-dash-gh--cache-set "/tmp/alpha" :stats
                              (list :branch "main" :ahead 2 :behind 1 :dirty t))
    (should (equal (magit-dash--repo-annotation r) "/tmp/alpha  (main +2/-1*)"))
    ;; 3. With cached CI status
    (magit-dash-gh--cache-set "/tmp/alpha" :ci-status
                              (list :conclusion "success" :status "completed"))
    (should (equal (magit-dash--repo-annotation r) "/tmp/alpha  (main +2/-1* · CI:✓)"))
    ;; 4. With cached PR counts
    (magit-dash-gh--cache-set "/tmp/alpha" :pr-counts (cons 3 1))
    (should (equal (magit-dash--repo-annotation r) "/tmp/alpha  (main +2/-1* · CI:✓ · 3 PRs)"))))

(ert-deftest magit-dash/resolve-repo-path-prompts-when-nothing-obvious ()
  "resolve-repo-path prompts among registered repos via annotated-completing-read."
  (let ((magit-dash-repo-list
         (list (magit-dash-repo--make :name "alpha" :path "/tmp/alpha")
               (magit-dash-repo--make :name "beta" :path "/tmp/beta"))))
    (cl-letf (((symbol-function 'magit-dash--repo-at-point-p) (lambda () nil))
              ((symbol-function 'magit-toplevel) (lambda () nil))
              ((symbol-function 'annotated-completing-read)
               (lambda (table &key prompt require-match &rest _)
                 (should require-match)
                 (should (equal prompt "Repository: "))
                 (cdr (map-elt table "beta")))))
      (should (equal "/tmp/beta" (magit-dash--resolve-repo-path))))))

(ert-deftest magit-dash/open-registered-repo-opens-status-buffer ()
  "open-registered-repo prompts for a repo and calls magit-status-setup-buffer."
  (let ((magit-dash-repo-list
         (list (magit-dash-repo--make :name "alpha" :path "/tmp/alpha")))
        (opened nil))
    (cl-letf (((symbol-function 'magit-dash--prompt-for-repo-path)
               (lambda (&optional _prompt) "/tmp/alpha"))
              ((symbol-function 'magit-status-setup-buffer)
               (lambda (path) (setq opened path))))
      (magit-dash-open-registered-repo)
      (should (equal opened "/tmp/alpha")))))
(ert-deftest magit-dash/resolve-repo-path-user-error-when-no-registered-repos ()
  "prompt-for-repo-path signals `user-error' when there is nothing to choose from."
  (let ((magit-dash-repo-list nil))
    (cl-letf (((symbol-function 'magit-dash--repo-at-point-p) (lambda () nil))
              ((symbol-function 'magit-toplevel) (lambda () nil)))
      (should-error (magit-dash--resolve-repo-path) :type 'user-error))))

;;;; Transient menu key conflict detection

(defun test-magit-dash--transient-keys (prefix)
  "Collect all key strings from transient PREFIX layout.
Layout shape: [2 nil (ROW)] where ROW = [transient-columns nil (COL...)]
and each COL = [transient-column PROPS ((transient-suffix :key K ...) ...)]."
  (when-let* ((layout (get prefix 'transient--layout))
              ((vectorp layout))
              (rows (aref layout 2))
              (row (car rows))
              ((vectorp row))
              (cols (aref row 2)))
    (thread-last (if (vectorp cols) (append cols nil) cols)
      (seq-filter #'vectorp)
      (seq-mapcat (lambda (col) (aref col 2)))
      (seq-filter (lambda (s) (and (listp s) (eq (car s) 'transient-suffix))))
      (seq-map (lambda (s) (plist-get (cdr s) :key)))
      (seq-remove #'null))))

(ert-deftest magit-dash/transient-predicates-safe-with-no-repo ()
  "All transient :inapt-if-not predicates return nil (not error) with no repo at point."
  (let ((magit-dash-repo-list nil)
        (magit-dash-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () nil))
              ((symbol-function 'derived-mode-p) (lambda (&rest _) t)))
      (should (null (magit-dash--repo-at-point-p)))
      (should (null (magit-dash--dirty-or-unknown-p)))
      (should (null (magit-dash--has-auto-commit-p)))
      (should (null (magit-dash--has-commands-p)))
      (should (null (magit-dash--repo-at-point-behind-p)))
      (should (null (magit-dash--at-worktree-p)))
      (should (null (magit-dash--can-add-worktree-p))))))

(ert-deftest magit-dash/menu-no-key-prefix-conflicts ()
  "No key in the dashboard transient menu is a string prefix of another key.
A conflict (e.g. \"b\" and \"bp\" coexisting) causes transient to raise
\"Wrong type argument: command, (keymap ...), command\" when the menu is opened."
  (let ((keys (test-magit-dash--transient-keys
               'magit-dash-menu)))
    (should (> (length keys) 0))
    (seq-do
     (lambda (k)
       (let ((conflict (seq-find (lambda (other)
                                   (and (not (equal k other))
                                        (string-prefix-p k other)))
                                 keys)))
         (should (equal nil conflict))))
     keys)))

;;;; Ephemeral tag tests

(ert-deftest magit-dash/all-tags-for-permanent-only ()
  "all-tags-for returns permanent tags when no ephemeral tags exist."
  (let ((magit-dash-repo-list nil)
        (magit-dash--ephemeral-tags (make-hash-table :test #'equal)))
    (magit-dash-register :name "r" :path "/tmp/r" :tags '(work personal))
    (should (equal '(work personal)
                   (magit-dash--all-tags-for (car magit-dash-repo-list))))))

(ert-deftest magit-dash/all-tags-for-combines-ephemeral ()
  "all-tags-for appends ephemeral tags after permanent ones."
  (let ((magit-dash-repo-list nil)
        (magit-dash--ephemeral-tags (make-hash-table :test #'equal)))
    (magit-dash-register :name "r" :path "/tmp/r" :tags '(work))
    (puthash "/tmp/r" '(temp) magit-dash--ephemeral-tags)
    (should (equal '(work temp)
                   (magit-dash--all-tags-for (car magit-dash-repo-list))))))

(ert-deftest magit-dash/all-tags-for-ephemeral-only ()
  "all-tags-for returns ephemeral tags for a repo with no permanent tags."
  (let ((magit-dash-repo-list nil)
        (magit-dash--ephemeral-tags (make-hash-table :test #'equal)))
    (magit-dash-register :name "r" :path "/tmp/r")
    (puthash "/tmp/r" '(draft) magit-dash--ephemeral-tags)
    (should (equal '(draft)
                   (magit-dash--all-tags-for (car magit-dash-repo-list))))))

(ert-deftest magit-dash/permanent-tag-set-collects-all ()
  "permanent-tag-set returns deduplicated symbols from all repo :tags fields."
  (let ((magit-dash-repo-list nil)
        (magit-dash--ephemeral-tags (make-hash-table :test #'equal)))
    (magit-dash-register :name "a" :path "/tmp/a" :tags '(work))
    (magit-dash-register :name "b" :path "/tmp/b" :tags '(work personal))
    (let ((tags (magit-dash--permanent-tag-set)))
      (should (memq 'work tags))
      (should (memq 'personal tags))
      (should (= 2 (length tags))))))

(ert-deftest magit-dash/permanent-tag-set-excludes-ephemeral ()
  "permanent-tag-set does not include ephemeral-only tags."
  (let ((magit-dash-repo-list nil)
        (magit-dash--ephemeral-tags (make-hash-table :test #'equal)))
    (magit-dash-register :name "r" :path "/tmp/r" :tags '(work))
    (puthash "/tmp/r" '(ephemeral-only) magit-dash--ephemeral-tags)
    (let ((tags (magit-dash--permanent-tag-set)))
      (should (memq 'work tags))
      (should (null (memq 'ephemeral-only tags))))))

(ert-deftest magit-dash/build-tag-table-format ()
  "build-tag-table returns a triple-form alist of (name annotation . tag)."
  (let ((magit-dash-repo-list nil)
        (magit-dash--ephemeral-tags (make-hash-table :test #'equal)))
    (magit-dash-register :name "alpha" :path "/tmp/a" :tags '(work))
    (magit-dash-register :name "beta"  :path "/tmp/b" :tags '(work personal))
    (let* ((table (magit-dash--build-tag-table))
           (work-entry (seq-find (lambda (e) (equal (car e) "work")) table))
           (personal-entry (seq-find (lambda (e) (equal (car e) "personal")) table)))
      (should (consp work-entry))
      (should (string-match-p "2 repos" (car (cdr work-entry))))
      (should (eq (cdr (cdr work-entry)) 'work))
      (should (consp personal-entry))
      (should (string-match-p "1 repo:" (car (cdr personal-entry))))
      (should (eq (cdr (cdr personal-entry)) 'personal)))))

(ert-deftest magit-dash/build-tag-table-permanent-before-ephemeral ()
  "build-tag-table lists permanent tags before ephemeral-only tags."
  (let ((magit-dash-repo-list nil)
        (magit-dash--ephemeral-tags (make-hash-table :test #'equal)))
    (magit-dash-register :name "r" :path "/tmp/r" :tags '(permanent))
    (puthash "/tmp/r" '(ephemeral) magit-dash--ephemeral-tags)
    (let* ((table (magit-dash--build-tag-table))
           (names (seq-map #'car table)))
      (should (< (seq-position names "permanent")
                 (seq-position names "ephemeral"))))))

(ert-deftest magit-dash/transient-predicates-include-auto-sync ()
  "has-auto-sync-p returns nil when no repo is at point."
  (let ((magit-dash-repo-list nil)
        (magit-dash-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () nil))
              ((symbol-function 'derived-mode-p) (lambda (&rest _) t)))
      (should (null (magit-dash--has-auto-sync-p))))))

(provide 'test-magit-dash)

;;; test-magit-dash.el ends here

;;;; Cache reset and rendering (regression tests)

(ert-deftest magit-dash/cache-reset-rendering ()
  "Regression: rendering after cache reset should not fail.
Simulates the user's issue where a cache reset caused rendering problems."
  (let* ((repo (magit-dash-repo--make :name "test" :path "/tmp/test"))
         (magit-dash-repo-list (list repo))
         (magit-dash-gh--cache (make-hash-table :test #'equal))
         (build-entry-called nil))
    ;; Pre-populate cache with valid stats
    (magit-dash-gh--cache-set "/tmp/test" :stats
                         (list :branch "main"
                               :remote-origin "git@github.com:user/test.git"
                               :behind 0
                               :ahead 0
                               :dirty nil
                               :uncommitted-files nil
                               :fetch-age 3600.0
                               :head-hash "abc123"
                               :recent-log "abc123 initial commit"))
    ;; Reset cache (this is what user did)
    (clrhash magit-dash-gh--cache)
    (cl-letf (((symbol-function 'magit-dash--discover-worktrees) (lambda () nil))
              ((symbol-function 'magit-dash--discover-submodules) (lambda () nil))
              ((symbol-function 'magit-dash--populate-stats-async) (lambda (_) nil))
              ((symbol-function 'magit-dash--build-entry)
               (lambda (r)
                 (setq build-entry-called t)
                 (list r (vector "test" "main" "1h" "" "REPO"))))
              ((symbol-function 'tabulated-list-print) (lambda (&rest _) nil))
              ((symbol-function 'tabulated-list-init-header) (lambda () nil)))
      (with-temp-buffer
        (magit-dash-mode)
        (magit-dash-refresh)
        (should build-entry-called)))))

(ert-deftest magit-dash/cache-reset-all-repopulates ()
  "cache-reset-all clears the entire cache."
  (let* ((r1 (magit-dash-repo--make :name "r1" :path "/tmp/r1"))
         (r2 (magit-dash-repo--make :name "r2" :path "/tmp/r2"))
         (magit-dash-repo-list (list r1 r2))
         (magit-dash-gh--cache (make-hash-table :test #'equal)))
    (magit-dash-gh--cache-set "/tmp/r1" :stats '(:branch "main"))
    (magit-dash-gh--cache-set "/tmp/r2" :stats '(:branch "feat"))
    (should (= 2 (hash-table-count magit-dash-gh--cache)))
    (cl-letf (((symbol-function 'magit-dash--maybe-refresh) (lambda () nil)))
      (magit-dash-cache-reset-all))
    (should (= 0 (hash-table-count magit-dash-gh--cache)))))

(ert-deftest magit-dash/cache-diagnose-finds-missing-stats ()
  "cache-diagnose should detect repos with missing stats."
  (let* ((repo (magit-dash-repo--make :name "r1" :path "/tmp/r1"))
         (magit-dash-repo-list (list repo))
         (magit-dash-gh--cache (make-hash-table :test #'equal))
         (messages nil))
    ;; No stats in cache
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages)))
              ((symbol-function 'pop-to-buffer) (lambda (_) nil))
              ((symbol-function 'view-mode) (lambda (_) nil)))
      (magit-dash-cache-diagnose)
      ;; Should report 1 warning
      (should (seq-some (lambda (msg) (string-match-p "1 warning" msg)) messages)))))

(ert-deftest magit-dash/cache-diagnose-finds-malformed-stats ()
  "cache-diagnose should detect stats missing required fields."
  (let* ((repo (magit-dash-repo--make :name "r1" :path "/tmp/r1"))
         (magit-dash-repo-list (list repo))
         (magit-dash-gh--cache (make-hash-table :test #'equal))
         (messages nil))
    ;; Add malformed stats (missing :head-hash)
    (magit-dash-gh--cache-set "/tmp/r1" :stats (list :branch "main" :dirty nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages)))
              ((symbol-function 'pop-to-buffer) (lambda (_) nil))
              ((symbol-function 'view-mode) (lambda (_) nil)))
      (magit-dash-cache-diagnose)
      ;; Should report errors
      (should (seq-some (lambda (msg) (string-match-p "error" msg)) messages)))))

(ert-deftest magit-dash/cache-stats-shows-summary ()
  "cache-stats should display a summary of cached data."
  (let* ((repo (magit-dash-repo--make :name "r1" :path "/tmp/r1"))
         (magit-dash-repo-list (list repo))
         (magit-dash-gh--cache (make-hash-table :test #'equal))
         (buffer-shown nil))
    (magit-dash-gh--cache-set "/tmp/r1" :stats
                         (list :branch "main" :head-hash "abc123"
                               :dirty nil :behind 0 :ahead 0))
    (magit-dash-gh--cache-set "/tmp/r1" :pr-counts (cons 5 2))
    (cl-letf (((symbol-function 'pop-to-buffer)
               (lambda (buf) (setq buffer-shown buf)))
              ((symbol-function 'view-mode) (lambda (_) nil)))
      (magit-dash-cache-stats)
      (should buffer-shown)
      (with-current-buffer buffer-shown
        (let ((text (buffer-string)))
          (should (string-match-p "Repository: r1" text))
          (should (string-match-p "Stats cached: yes" text))
          (should (string-match-p "PR counts cached: yes" text)))))))

(ert-deftest magit-dash/build-entry-handles-missing-stats ()
  "build-entry should handle repos with no cached stats gracefully."
  (let* ((repo (magit-dash-repo--make :name "test" :path "/tmp/test"))
         (magit-dash-gh--cache (make-hash-table :test #'equal))
         (magit-dash--submodule-path-set (make-hash-table :test #'equal))
         (magit-dash-columns
          '((name . t) (branch . t) (fetched . t) (updated . nil) (ci . nil) (status . t)
            (worktree . t) (sync . nil) (cached . nil))))
    (cl-letf (((symbol-function 'magit-dash--get-stats-fast)
               (lambda (_)
                 (list :branch "main" :ahead 0 :behind 0 :dirty nil
                       :fetch-age nil :head-hash "abc" :recent-log ""))))
      (let* ((entry (magit-dash--build-entry repo))
             (vec (cadr entry)))
        (should (= 5 (length vec)))
        (should (string-match-p "test" (aref vec 0)))
        (should (equal "main" (substring-no-properties (aref vec 1))))
        (should (equal "┄" (aref vec 2)))))))

(ert-deftest magit-dash/cache-reset-at-point-refreshes ()
  "cache-reset-at-point should clear cache for one repo and refresh."
  (let* ((repo (magit-dash-repo--make :name "test" :path "/tmp/test"))
         (magit-dash-gh--cache (make-hash-table :test #'equal))
         (refresh-called nil)
         (collect-called nil))
    ;; Pre-populate cache
    (magit-dash-gh--cache-set "/tmp/test" :stats (list :branch "main" :dirty nil))
    (cl-letf (((symbol-function 'magit-dash--repo-at-point)
               (lambda () repo))
              ((symbol-function 'magit-dash--collect-stats)
               (lambda (r)
                 (setq collect-called t)
                 (list :branch "feat" :dirty t :head-hash "new")))
              ((symbol-function 'magit-dash--maybe-refresh)
               (lambda () (setq refresh-called t))))
      (magit-dash-cache-reset-at-point)
      (should collect-called)
      (should refresh-called)
      ;; Stats should be updated
      (let ((stats (magit-dash-gh--cache-get "/tmp/test" :stats)))
        (should (equal "feat" (plist-get stats :branch)))))))

(ert-deftest magit-dash/format-age-consistent-after-cache-reset ()
  "Regression: format-age should work consistently after cache reset."
  ;; Test that formatting functions don't depend on cache state
  (let ((magit-dash-gh--cache (make-hash-table :test #'equal)))
    (should (equal "┄" (magit-dash--format-age nil)))
    (should (equal "1h" (magit-dash--format-age 3600.0)))
    (clrhash magit-dash-gh--cache)
    (should (equal "┄" (magit-dash--format-age nil)))
    (should (equal "1h" (magit-dash--format-age 3600.0)))))

(ert-deftest magit-dash/format-status-consistent-after-cache-reset ()
  "Regression: format-status should work consistently after cache reset."
  (let ((magit-dash-gh--cache (make-hash-table :test #'equal)))
    (should (equal "" (magit-dash--format-status 0 0 nil)))
    (should (equal "↑2 ↓3 !" (substring-no-properties
                              (magit-dash--format-status 2 3 t))))
    (clrhash magit-dash-gh--cache)
    (should (equal "" (magit-dash--format-status 0 0 nil)))
    (should (equal "↑2 ↓3 !" (substring-no-properties
                              (magit-dash--format-status 2 3 t))))))

(ert-deftest magit-dash/build-entry-name-returns-string ()
  "Regression: name column must return a string, not the result of add-text-properties.
The bug was that add-text-properties returns t, not the modified string."
  (let* ((repo (magit-dash-repo--make :name "test" :path "/tmp/test"))
         (magit-dash-gh--cache (make-hash-table :test #'equal))
         (magit-dash--submodule-path-set (make-hash-table :test #'equal))
         (magit-dash--marked-paths nil)
         (magit-dash-columns
          '((name . t))))
    (cl-letf (((symbol-function 'magit-dash--get-stats-fast)
               (lambda (_)
                 (list :branch "main" :ahead 0 :behind 0 :dirty nil
                       :fetch-age nil :head-hash "abc" :recent-log ""))))
      (let* ((entry (magit-dash--build-entry repo))
             (vec (cadr entry))
             (name-col (aref vec 0)))
        (should (stringp name-col))
        (should (equal "test" (substring-no-properties name-col)))))))

(ert-deftest magit-dash/build-entry-all-columns-are-strings ()
  "All column values must be strings for tabulated-list-mode."
  (let* ((repo (magit-dash-repo--make :name "test" :path "/tmp/test"))
         (magit-dash-gh--cache (make-hash-table :test #'equal))
         (magit-dash--submodule-path-set (make-hash-table :test #'equal))
         (magit-dash-columns
          '((name . t) (branch . t) (fetched . t) (updated . nil) (ci . nil) (status . t)
            (worktree . t) (sync . nil) (cached . nil))))
    (cl-letf (((symbol-function 'magit-dash--get-stats-fast)
               (lambda (_)
                 (list :branch "main" :ahead 0 :behind 0 :dirty nil
                       :fetch-age 3600.0 :head-hash "abc123" :recent-log ""))))
      (let* ((entry (magit-dash--build-entry repo))
             (vec (cadr entry)))
        (should (= 5 (length vec)))
        (dotimes (i (length vec))
          (let ((val (aref vec i)))
            (should (stringp val))))))))

(ert-deftest magit-dash/head-hash-nil-is-valid ()
  "Repos with no commits can have nil :head-hash without being malformed."
  (let* ((repo (magit-dash-repo--make :name "empty" :path "/tmp/empty"))
         (magit-dash-repo-list (list repo))
         (magit-dash-gh--cache (make-hash-table :test #'equal))
         (messages nil))
    ;; Set up stats with nil head-hash (valid for repos with no commits)
    (magit-dash-gh--cache-set "/tmp/empty" :stats
                         (list :branch "" :head-hash nil :dirty nil
                               :behind 0 :ahead 0 :uncommitted-files nil
                               :fetch-age nil :recent-log ""))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages)))
              ((symbol-function 'pop-to-buffer) (lambda (_) nil))
              ((symbol-function 'view-mode) (lambda (_) nil))
              ((symbol-function 'magit-dash--head-hash)
               (lambda (_) nil)))
      (magit-dash-cache-diagnose)
      ;; Should not report this as an error (nil is in the plist, just has nil value)
      (should (seq-some (lambda (msg) (string-match-p "0 error" msg)) messages)))))

(ert-deftest magit-dash/parse-submodules-includes-missing ()
  "parse-submodules should include uninitialized (missing) submodules."
  (cl-letf (((symbol-function 'file-directory-p)
             (lambda (path)
               ;; Only "present-mod" directory exists
               (string-match-p "present-mod" path))))
    (let* ((lines '("-abc123 external/missing-mod"
                    " def456 external/present-mod"))
           (repos (magit-dash--parse-submodules "/tmp/parent" lines)))
      (should (= 2 (length repos)))
      (let ((missing (seq-find (lambda (r) (string-match-p "missing-mod" (magit-dash-repo-name r))) repos))
            (present (seq-find (lambda (r) (string-match-p "present-mod" (magit-dash-repo-name r))) repos)))
        (should (eq 'missing (magit-dash-repo-submodule missing)))
        (should (eq t (magit-dash-repo-submodule present)))))))

(ert-deftest magit-dash/format-worktree-missing-submodule ()
  "Missing submodules should display as SUBM.EMPTY with warning face."
  (let ((repo (magit-dash-repo--make :name "test<sub>" :path "/tmp/missing"
                                   :submodule 'missing)))
    (let ((result (magit-dash--format-worktree repo)))
      (should (equal "SUBM.EMPTY" (substring-no-properties result)))
      (should (equal 'warning (get-text-property 0 'face result))))))

(ert-deftest magit-dash/format-worktree-present-submodule ()
  "Initialized submodules should display as SUBM."
  (let ((repo (magit-dash-repo--make :name "test<sub>" :path "/tmp/present"
                                   :submodule t)))
    (let ((result (magit-dash--format-worktree repo)))
      (should (equal "SUBM" (substring-no-properties result)))
      (should (equal 'magit-dash-repo-branch-face (get-text-property 0 'face result))))))

(ert-deftest magit-dash/get-stats-missing-submodule ()
  "Missing submodules should return placeholder stats without calling git."
  (let ((repo (magit-dash-repo--make :name "test<sub>" :path "/nonexistent"
                                   :submodule 'missing))
        (magit-dash-gh--cache (make-hash-table :test #'equal))
        (collect-called nil))
    (cl-letf (((symbol-function 'magit-dash--collect-stats)
               (lambda (_) (setq collect-called t) (error "Should not be called"))))
      (let ((stats (magit-dash--get-stats repo)))
        (should-not collect-called)
        (should (plist-get stats :branch))
        (should (equal "" (plist-get stats :branch)))
        (should (eq nil (plist-get stats :head-hash)))))))

(ert-deftest magit-dash/build-entry-missing-submodule-strikethrough ()
  "Missing submodules should have strikethrough face on the name."
  (let* ((repo (magit-dash-repo--make :name "parent<missing>" :path "/tmp/missing"
                                    :submodule 'missing))
         (magit-dash-gh--cache (make-hash-table :test #'equal))
         (magit-dash--submodule-path-set (make-hash-table :test #'equal))
         (magit-dash--marked-paths nil)
         (magit-dash-columns '((name . t) (branch . nil) (fetched . nil) (updated . nil) (ci . nil)
                              (status . nil) (worktree . t) (sync . nil) (cached . nil))))
    ;; Add to submodule path set so it gets the special display name
    (puthash "/tmp/missing" "parent<missing>" magit-dash--submodule-path-set)
    (cl-letf (((symbol-function 'magit-dash--get-stats-fast)
               (lambda (_)
                 (list :branch "" :ahead 0 :behind 0 :dirty nil
                       :fetch-age nil :head-hash nil :recent-log ""))))
      (let* ((entry (magit-dash--build-entry repo))
             (vec (cadr entry))
             (name-col (aref vec 0))
             (type-col (aref vec 1)))
        ;; Check name has strikethrough
        (should (stringp name-col))
        (should (equal "parent<missing>" (substring-no-properties name-col)))
        (let ((face (get-text-property 0 'face name-col)))
          (should (listp face))
          (should (member '(:strike-through t) face)))
        ;; Check type is SUBM.EMPTY
        (should (equal "SUBM.EMPTY" (substring-no-properties type-col)))))))

(ert-deftest magit-dash/parse-submodules-prefix-detection ()
  "parse-submodules should detect missing submodules by - prefix."
  (let* ((lines '("-abc123 missing/sub1"
                  "+def456 modified/sub2"
                  " 123abc current/sub3"
                  "Uabc123 conflict/sub4"))
         (repos (magit-dash--parse-submodules "/tmp/test" lines)))
    (should (= 4 (length repos)))
    ;; - prefix means missing
    (let ((missing (seq-find (lambda (r) (string-match-p "sub1" (magit-dash-repo-name r))) repos)))
      (should (eq 'missing (magit-dash-repo-submodule missing))))
    ;; Other prefixes should still create repos, marked as missing if dir doesn't exist
    (should (seq-every-p #'magit-dash-repo-p repos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; magit-dash--repo-type-rank

(ert-deftest magit-dash/repo-type-rank-plain-repo ()
  "Plain repo (no worktree, no submodule) gets rank 0."
  (let ((r (magit-dash-repo--make :name "r" :path "/tmp/r")))
    (should (= 0 (magit-dash--repo-type-rank r)))))

(ert-deftest magit-dash/repo-type-rank-worktree ()
  "Worktree gets rank 1."
  (let ((r (magit-dash-repo--make :name "r" :path "/tmp/r" :worktree t)))
    (should (= 1 (magit-dash--repo-type-rank r)))))

(ert-deftest magit-dash/repo-type-rank-tracked-submodule ()
  "Tracked submodule (non-missing :submodule) gets rank 2."
  (let ((r (magit-dash-repo--make :name "r" :path "/tmp/r" :submodule "/tmp/parent")))
    (should (= 2 (magit-dash--repo-type-rank r)))))

(ert-deftest magit-dash/repo-type-rank-missing-submodule ()
  "Missing submodule gets rank 3."
  (let ((r (magit-dash-repo--make :name "r" :path "/tmp/r" :submodule 'missing)))
    (should (= 3 (magit-dash--repo-type-rank r)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; magit-dash--sorted-repos type-based ordering

(ert-deftest magit-dash/sorted-repos-type-order-no-hints ()
  "Without sort-hints, repos are ordered by type: repo < wt < subm < missing."
  (let* ((repo (magit-dash-repo--make :name "repo" :path "/tmp/repo"))
         (wt (magit-dash-repo--make :name "wt" :path "/tmp/wt" :worktree t))
         (subm (magit-dash-repo--make :name "subm" :path "/tmp/subm" :submodule "/p"))
         (miss (magit-dash-repo--make :name "miss" :path "/tmp/miss" :submodule 'missing))
         (magit-dash-gh--cache (make-hash-table :test #'equal))
         (result (magit-dash--sorted-repos (list miss wt repo subm))))
    (should (equal "repo" (magit-dash-repo-name (nth 0 result))))
    (should (equal "wt"   (magit-dash-repo-name (nth 1 result))))
    (should (equal "subm" (magit-dash-repo-name (nth 2 result))))
    (should (equal "miss" (magit-dash-repo-name (nth 3 result))))))

(ert-deftest magit-dash/sorted-repos-type-order-within-same-hint ()
  "Repos sharing the same sort-hint are ordered by type as secondary key."
  (let* ((repo (magit-dash-repo--make :name "repo" :path "/tmp/repo" :sort-hint 5))
         (wt (magit-dash-repo--make :name "wt" :path "/tmp/wt" :worktree t :sort-hint 5))
         (magit-dash-gh--cache (make-hash-table :test #'equal))
         (result (magit-dash--sorted-repos (list wt repo))))
    (should (equal "repo" (magit-dash-repo-name (nth 0 result))))
    (should (equal "wt"   (magit-dash-repo-name (nth 1 result))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; magit-dash--push-async

(ert-deftest magit-dash/push-async-calls-run-git-with-push ()
  "Calls --run-git with the repo path and (\"push\") args."
  (let* ((repo (magit-dash-repo--make :name "r" :path "/tmp/r"))
         (captured-path nil)
         (captured-args nil))
    (cl-letf (((symbol-function 'magit-dash--run-git)
               (lambda (path args _on-success &optional _on-error)
                 (setq captured-path path
                       captured-args args))))
      (magit-dash--push-async repo #'ignore))
    (should (equal "/tmp/r" captured-path))
    (should (equal '("push") captured-args))))

(ert-deftest magit-dash/push-async-calls-callback-ok-on-success ()
  "Callback receives `ok' when --run-git calls on-success."
  (let* ((repo (magit-dash-repo--make :name "r" :path "/tmp/r"))
         (result nil))
    (cl-letf (((symbol-function 'magit-dash--run-git)
               (lambda (_path _args on-success &optional _on-error)
                 (funcall on-success ""))))
      (magit-dash--push-async repo (lambda (status &rest _) (setq result status))))
    (should (eq 'ok result))))

(ert-deftest magit-dash/push-async-calls-callback-error-on-failure ()
  "Callback receives `error' when --run-git calls on-error."
  (let* ((repo (magit-dash-repo--make :name "r" :path "/tmp/r"))
         (result nil))
    (cl-letf (((symbol-function 'magit-dash--run-git)
               (lambda (_path _args _on-success &optional on-error)
                 (funcall on-error "remote error" 1))))
      (magit-dash--push-async repo (lambda (status &rest _) (setq result status))))
    (should (eq 'error result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; magit-dash-push-all

(ert-deftest magit-dash/push-all-errors-when-no-repos ()
  "Signals user-error when --effective-repos returns nil."
  (cl-letf (((symbol-function 'magit-dash--effective-repos)
             (lambda () nil)))
    (should-error (magit-dash-push-all) :type 'user-error)))

(ert-deftest magit-dash/push-all-calls-batch-run-with-push-async ()
  "Calls --batch-run with the effective repos and --push-async as op-fn."
  (let* ((repo (magit-dash-repo--make :name "r" :path "/tmp/r"))
         (batch-repos nil)
         (batch-op nil))
    (cl-letf (((symbol-function 'magit-dash--effective-repos)
               (lambda () (list repo)))
              ((symbol-function 'magit-dash--batch-run)
               (lambda (repos op-fn _label &optional _done)
                 (setq batch-repos repos
                       batch-op op-fn))))
      (magit-dash-push-all))
    (should (equal (list repo) batch-repos))
    (should (eq #'magit-dash--push-async batch-op))))

;;;; Missing repos and git clone operations

(ert-deftest magit-dash/repo-missing-p-detects-missing-dir ()
  "repo-missing-p returns t for non-existent directory."
  (let ((repo (magit-dash-repo--make :name "nonexistent" :path "/tmp/definitely-not-here-12345")))
    (should (magit-dash--repo-missing-p repo))))

(ert-deftest magit-dash/repo-missing-p-detects-missing-submodule ()
  "repo-missing-p returns t for missing submodule."
  (let ((repo (magit-dash-repo--make :name "p<s>" :path "/tmp/some-path" :submodule 'missing)))
    (should (magit-dash--repo-missing-p repo))))

(ert-deftest magit-dash/format-worktree-shows-missing ()
  "format-worktree returns MISSING for un-cloned repos."
  (let ((repo (magit-dash-repo--make :name "missing-repo" :path "/tmp/definitely-not-here-12345")))
    (should (equal (substring-no-properties (magit-dash--format-worktree repo)) "MISSING"))))

(ert-deftest magit-dash/clone-repo-errors-when-destination-exists ()
  "clone-repo signals user-error when repository already exists at path."
  (let ((repo (magit-dash-repo--make :name "exists" :path default-directory)))
    (should-error (magit-dash-clone-repo repo) :type 'user-error)))

(ert-deftest magit-dash/clone-repo-spawns-git-clone ()
  "clone-repo runs git clone with the specified URL and path."
  (let* ((repo (magit-dash-repo--make :name "newrepo" :path "/tmp/new-repo-test-12345"
                                      :clone-url "git@github.com:user/newrepo.git"))
         (spawned-args nil)
         (magit-dash-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-dash--resolve-git-dir) (lambda (_) nil))
              ((symbol-function 'make-directory) #'ignore)
              ((symbol-function 'make-process)
               (lambda (&rest plist)
                 (setq spawned-args (plist-get plist :command))
                 'dummy-proc)))
      (magit-dash-clone-repo repo "git@github.com:user/newrepo.git")
      (should (member "clone" spawned-args))
      (should (member "git@github.com:user/newrepo.git" spawned-args))
      (should (member (expand-file-name "/tmp/new-repo-test-12345") spawned-args)))))

(ert-deftest magit-dash/clone-all-missing-clones-all ()
  "clone-all-missing calls clone-repo on every missing repository."
  (let* ((r1 (magit-dash-repo--make :name "m1" :path "/tmp/missing-1"))
         (r2 (magit-dash-repo--make :name "m2" :path "/tmp/missing-2"))
         (magit-dash-repo-list (list r1 r2))
         (cloned nil))
    (cl-letf (((symbol-function 'magit-dash--repo-missing-p) (lambda (_) t))
              ((symbol-function 'magit-dash-clone-repo)
               (lambda (repo &optional _url) (push (magit-dash-repo-name repo) cloned))))
      (magit-dash-clone-all-missing)
      (should (member "m1" cloned))
      (should (member "m2" cloned)))))


(ert-deftest magit-dash/update-default-directory-existing-repo ()
  "update-default-directory sets default-directory to repo path when repo exists."
  (let ((repo (magit-dash-repo--make :name "exists" :path default-directory)))
    (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () repo))
              ((symbol-function 'magit-dash--repo-missing-p) (lambda (_) nil)))
      (with-temp-buffer
        (insert (propertize " " 'tabulated-list-id repo))
        (goto-char (point-min))
        (magit-dash--update-default-directory)
        (should (equal default-directory (file-name-as-directory (expand-file-name default-directory))))))))

(ert-deftest magit-dash/update-default-directory-missing-repo ()
  "update-default-directory sets default-directory to parent directory when repo is missing."
  (let* ((missing-path "/tmp/nonexistent-parent-dir-12345/missing-repo-dir")
         (repo (magit-dash-repo--make :name "missing" :path missing-path)))
    (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () repo))
              ((symbol-function 'magit-dash--repo-missing-p) (lambda (_) t)))
      (with-temp-buffer
        (insert (propertize " " 'tabulated-list-id repo))
        (goto-char (point-min))
        (magit-dash--update-default-directory)
        (should (equal default-directory "/tmp/nonexistent-parent-dir-12345/"))))))

(ert-deftest magit-dash/update-default-directory-fallback ()
  "update-default-directory falls back to ~/ when no repo at point."
  (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () nil)))
    (with-temp-buffer
      (magit-dash--update-default-directory)
      (should (equal default-directory (file-name-as-directory (expand-file-name "~/")))))))

(ert-deftest magit-dash/bootstrap-repo-errors-when-no-upstream ()
  "bootstrap-repo signals user-error when repo has no upstream :repo."
  (let ((repo (magit-dash-repo--make :name "no-up" :path "/tmp/no-up-test")))
    (cl-letf (((symbol-function 'magit-dash--repo-at-point) (lambda () repo))
              ((symbol-function 'magit-dash--repo-at-point-p) (lambda () t)))
      (should-error (magit-dash-bootstrap-repo repo) :type 'user-error))))

(ert-deftest magit-dash/bootstrap-repo-errors-when-repo-exists ()
  "bootstrap-repo signals user-error when destination repository already exists."
  (let ((repo (magit-dash-repo--make :name "exists" :path default-directory :repo "/srv/git/exists.git")))
    (should-error (magit-dash-bootstrap-repo repo) :type 'user-error)))

(ert-deftest magit-dash/bootstrap-repo-delegates-to-clone-repo ()
  "bootstrap-repo calls clone-repo with upstream repo and destination."
  (let* ((repo (magit-dash-repo--make :name "boot" :path "/tmp/boot-target" :repo "/srv/git/boot.git"))
         (cloned-repo nil)
         (cloned-url nil))
    (cl-letf (((symbol-function 'magit-dash--resolve-git-dir) (lambda (_) nil))
              ((symbol-function 'magit-dash-clone-repo)
               (lambda (r url &optional _on-complete)
                 (setq cloned-repo r)
                 (setq cloned-url url))))
      (magit-dash-bootstrap-repo repo)
      (should (eq cloned-repo repo))
      (should (equal cloned-url "/srv/git/boot.git")))))

(ert-deftest magit-dash/bootstrap-marked-errors-when-none-missing ()
  "bootstrap-marked signals user-error when no missing repos with upstream :repo exist."
  (let* ((r1 (magit-dash-repo--make :name "exists" :path "/tmp/r1" :repo "/srv/git/r1.git"))
         (r2 (magit-dash-repo--make :name "no-upstream" :path "/tmp/r2"))
         (magit-dash-repo-list (list r1 r2)))
    (cl-letf (((symbol-function 'magit-dash--repo-missing-p)
               (lambda (r) (equal (magit-dash-repo-name r) "no-upstream"))))
      (should-error (magit-dash-bootstrap-marked) :type 'user-error))))

(ert-deftest magit-dash/bootstrap-marked-clones-missing-repos ()
  "bootstrap-marked batch clones all missing repositories that have :repo set."
  (let* ((r1 (magit-dash-repo--make :name "m1" :path "/tmp/m1" :repo "/srv/git/m1.git"))
         (r2 (magit-dash-repo--make :name "m2" :path "/tmp/m2" :repo "/srv/git/m2.git"))
         (r3 (magit-dash-repo--make :name "m3" :path "/tmp/m3")) ;; no :repo
         (r4 (magit-dash-repo--make :name "e4" :path "/tmp/e4" :repo "/srv/git/e4.git")) ;; not missing
         (magit-dash-repo-list (list r1 r2 r3 r4))
         (batch-repos nil))
    (cl-letf (((symbol-function 'magit-dash--repo-missing-p)
               (lambda (r) (member (magit-dash-repo-name r) '("m1" "m2" "m3"))))
              ((symbol-function 'magit-dash--batch-run)
               (lambda (repos _fn _label _cb)
                 (setq batch-repos (mapcar #'magit-dash-repo-name repos)))))
      (magit-dash-bootstrap-marked)
      (should (equal (sort batch-repos #'string<) '("m1" "m2"))))))


(ert-deftest magit-dash/has-missing-bootstrap-repos-p-behavior ()
  "has-missing-bootstrap-repos-p requires batch to be enabled and matching missing repos."
  (let* ((r1 (magit-dash-repo--make :name "m1" :path "/tmp/m1" :repo "/srv/git/m1.git"))
         (r2 (magit-dash-repo--make :name "m2" :path "/tmp/m2"))
         (magit-dash-repo-list (list r1 r2)))
    (cl-letf (((symbol-function 'magit-dash--repo-missing-p)
               (lambda (r) (equal (magit-dash-repo-name r) "m1"))))
      ;; No marks and batch-all nil -> nil
      (let ((magit-dash--marked-paths nil)
            (magit-dash--batch-all nil))
        (should-not (magit-dash--has-missing-bootstrap-repos-p)))
      ;; batch-all t with missing repo having :repo -> t
      (let ((magit-dash--marked-paths nil)
            (magit-dash--batch-all t))
        (should (magit-dash--has-missing-bootstrap-repos-p)))
      ;; Marked matching repo -> t
      (let ((magit-dash--marked-paths '("/tmp/m1"))
            (magit-dash--batch-all nil))
        (should (magit-dash--has-missing-bootstrap-repos-p)))
      ;; Marked non-matching repo -> nil
      (let ((magit-dash--marked-paths '("/tmp/m2"))
            (magit-dash--batch-all nil))
        (should-not (magit-dash--has-missing-bootstrap-repos-p))))))

(ert-deftest magit-dash/bootstrap-marked-targets-marked-only ()
  "bootstrap-marked targets only marked missing repositories when batch-all is nil."
  (let* ((r1 (magit-dash-repo--make :name "m1" :path "/tmp/m1" :repo "/srv/git/m1.git"))
         (r2 (magit-dash-repo--make :name "m2" :path "/tmp/m2" :repo "/srv/git/m2.git"))
         (magit-dash-repo-list (list r1 r2))
         (magit-dash--marked-paths '("/tmp/m1"))
         (magit-dash--batch-all nil)
         (batch-repos nil))
    (cl-letf (((symbol-function 'magit-dash--repo-missing-p) (lambda (_) t))
              ((symbol-function 'magit-dash--batch-run)
               (lambda (repos _fn _label _cb)
                 (setq batch-repos (mapcar #'magit-dash-repo-name repos)))))
      (magit-dash-bootstrap-marked)
      (should (equal batch-repos '("m1"))))))


(ert-deftest magit-dash/validate-remote-sync-rejects-unknown-slot ()
  "validate-remote-sync signals user-error when unknown slots are present."
  (should-error (magit-dash--validate-remote-sync '(:hosts ("h1") :unknown-slot t))
                :type 'user-error)
  (should-error (magit-dash--validate-remote-sync "not-a-plist")
                :type 'user-error)
  ;; Valid slots do not signal error
  (magit-dash--validate-remote-sync '(:hosts ("h1" "h2") :branch "main" :path "/foo/bar")))

(ert-deftest magit-dash/remote-sync-target-generates-pipeline ()
  "magit-dash-remote-sync-target constructs the expected ssh and git sync pipeline."
  ;; With defaults:
  (let ((cmd (magit-dash-remote-sync-target :host "spinoza")))
    (should (string-prefix-p "ssh spinoza 'cd . && git add -A && git fetch origin && git rebase origin/$(git rev-parse --abbrev-ref HEAD)" cmd))
    (should (string-match-p "sync\.REMOTE(spinoza)" cmd))
    (should (string-match-p "git ls-files -d | xargs -r git rm --ignore-unmatch --quiet --" cmd))
    (should (string-suffix-p "git fetch origin && git rebase origin/$(git rev-parse --abbrev-ref HEAD)" cmd)))
  ;; With explicit branch, path, and symbol host:
  (let ((cmd (magit-dash-remote-sync-target :host 'deleuze :path "/home/tychoish/src/blog" :branch "master")))
    (should (string-prefix-p "ssh deleuze 'cd /home/tychoish/src/blog && git add -A && git fetch origin && git rebase origin/master" cmd))
    (should (string-match-p "sync\.REMOTE(deleuze)" cmd))
    (should (string-suffix-p "git fetch origin && git rebase origin/master" cmd))))

(ert-deftest magit-dash/register-with-remote-sync-generates-commands ()
  "magit-dash-register splices sync-HOST commands and stores :remote-sync."
  (let ((magit-dash-repo-list nil))
    (magit-dash-register
     :name "test-rs"
     :path "/tmp/test-rs"
     :remote-sync '(:hosts ("deleuze" "spinoza") :branch "main")
     :commands '((custom-cmd . "echo hello")))
    (let ((repo (car magit-dash-repo-list)))
      (should repo)
      (should (equal (magit-dash-repo-remote-sync repo)
                     '(:hosts ("deleuze" "spinoza") :branch "main")))
      ;; Check commands contains custom-cmd, sync-deleuze, and sync-spinoza
      (let ((cmds (magit-dash-repo-commands repo)))
        (should (assq 'custom-cmd cmds))
        (should (assq 'sync-deleuze cmds))
        (should (assq 'sync-spinoza cmds))
        (let ((del-cmd (cdr (assq 'sync-deleuze cmds))))
          (should (string-match-p "ssh deleuze" del-cmd))
          (should (string-match-p "origin/main" del-cmd)))
        (let ((spin-cmd (cdr (assq 'sync-spinoza cmds))))
          (should (string-match-p "ssh spinoza" spin-cmd))
          (should (string-match-p "origin/main" spin-cmd)))))))

(ert-deftest magit-dash/register-rejects-invalid-remote-sync ()
  "magit-dash-register signals user-error when invalid :remote-sync is passed."
  (let ((magit-dash-repo-list nil))
    (should-error
     (magit-dash-register
      :name "invalid-rs"
      :path "/tmp/invalid-rs"
      :remote-sync '(:bad-slot t))
     :type 'user-error)))


(ert-deftest magit-dash/validate-remote-sync-merge-method ()
  "validate-remote-sync verifies :merge-method is 'merge or 'rebase."
  (should-error (magit-dash--validate-remote-sync '(:hosts ("h1") :merge-method squash))
                :type 'user-error)
  (magit-dash--validate-remote-sync '(:hosts ("h1") :merge-method merge))
  (magit-dash--validate-remote-sync '(:hosts ("h1") :merge-method rebase)))

(ert-deftest magit-dash/remote-sync-target-with-merge-method ()
  "magit-dash-remote-sync-target uses git merge when merge-method is 'merge."
  (let ((cmd (magit-dash-remote-sync-target :host "spinoza" :branch "main" :merge-method 'merge)))
    (should (string-prefix-p "ssh spinoza 'cd . && git add -A && git fetch origin && git merge origin/main" cmd))
    (should (string-match-p "git add -A && git fetch origin && git merge origin/main" cmd))
    (should (string-suffix-p "git fetch origin && git merge origin/main" cmd))))

(ert-deftest magit-dash/clone-repo-missing-dir-runs-in-parent ()
  "magit-dash-clone-repo executes make-process with default-directory set to parent dir."
  (let* ((tmp-parent (make-temp-file "magit-dash-test-parent-" t))
         (repo-path (expand-file-name "nonexistent-repo" tmp-parent))
         (repo (magit-dash-repo--make :name "test-clone" :path repo-path))
         (captured-dir nil))
    (unwind-protect
        (cl-letf (((symbol-function 'magit-dash--resolve-git-dir) (lambda (_) nil))
                  ((symbol-function 'magit-git-executable) (lambda () "git"))
                  ((symbol-function 'make-process)
                   (lambda (&rest plist)
                     (setq captured-dir default-directory)
                     (generate-new-buffer " *mock-proc*"))))
          (magit-dash-clone-repo repo "git@github.com:foo/bar.git")
          (should (equal (file-name-as-directory tmp-parent)
                         (file-name-as-directory (or captured-dir "")))))
      (delete-directory tmp-parent t))))
