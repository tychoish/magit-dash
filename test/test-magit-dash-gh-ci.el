;;; test-magit-dash-gh-ci.el --- ERT tests for magit-dash-gh-ci -*- lexical-binding: t; no-byte-compile: t; -*-

;; Run inside a live Emacs session:
;;   (ert "^magit-dash-gh-ci/")
;;
;; Batch run:
;;   emacs --batch -l test/test-helper.el \
;;     -l test/test-magit-dash-gh-ci.el \
;;     --eval '(ert-run-tests-batch-and-exit "magit-dash-gh-ci/")'

(require 'ert)
(require 'cl-lib)
(require 'map)
(require 'magit-dash)
(require 'magit-dash-status)
(require 'magit-dash-gh-ci)

;;; Test helpers

(cl-defun magit-dash-gh-ci-test/make-repo (&optional name path branch (include-ci t))
  "Return a fake `magit-dash-repo' struct for NAME at PATH on BRANCH.
INCLUDE-CI defaults to t so CI-gated operations are enabled unless a test
passes nil explicitly to exercise the disabled case."
  (magit-dash-repo--make :name (or name "test") :path (or path "/tmp/test")
                          :branch branch
                          :include-ci include-ci))

;;; magit-dash-gh-ci-fetch

(ert-deftest magit-dash-gh-ci/fetch-ignores-older-results ()
  "An older CI response cannot overwrite a more recent response."
  (let* ((repo (magit-dash-repo--make :name "r" :path "/tmp/r" :include-ci t))
         (magit-dash-gh--cache (make-hash-table :test #'equal))
         (magit-dash-gh--cache-requests (make-hash-table :test #'equal))
         callbacks)
    (cl-letf (((symbol-function 'magit-dash--current-branch) (lambda (_) "main"))
              ((symbol-function 'magit-dash-gh--run-process)
               (lambda (_args _path success _failure) (push success callbacks)))
              ((symbol-function 'magit-dash-gh-ci--parse-runs) #'identity))
      (magit-dash-gh-ci-fetch repo #'ignore)
      (magit-dash-gh-ci-fetch repo #'ignore)
      (funcall (car callbacks) "[{\"name\":\"new\"}]")
      (funcall (cadr callbacks) "[{\"name\":\"old\"}]")
      (should (equal (alist-get 'name (car (magit-dash-gh--cache-get "/tmp/r" :ci-status)))
                     "new")))))

(ert-deftest magit-dash-gh-ci/cache-invalidation-rejects-pending-result ()
  "Clearing a field invalidates requests that were already in flight."
  (let ((magit-dash-gh--cache (make-hash-table :test #'equal))
        (magit-dash-gh--cache-requests (make-hash-table :test #'equal)))
    (let ((token (magit-dash-gh--cache-request "/tmp/r" :stats)))
      (magit-dash-gh--cache-remove "/tmp/r" :stats)
      (magit-dash-gh--cache-set-if-current "/tmp/r" :stats 'stale token)
      (should-not (magit-dash-gh--cache-get "/tmp/r" :stats)))))

(ert-deftest magit-dash-gh-ci/fetch-uses-live-checked-out-branch ()
  "Queries the branch currently checked out at REPO's path, not a stale cached one."
  (let* ((repo (magit-dash-gh-ci-test/make-repo "test" "/tmp/test" "stale-cached-branch"))
         (queried-branch nil))
    (magit-dash-gh--cache-set "/tmp/test" :stats (list :branch "stale-cached-branch"))
    (cl-letf (((symbol-function 'magit-dash--current-branch)
               (lambda (_path) "live-branch"))
              ((symbol-function 'magit-dash-gh--run-process)
               (lambda (args _dir on-success &optional _on-error)
                 (setq queried-branch (nth (1+ (seq-position args "--branch")) args))
                 (funcall on-success "[]"))))
      (magit-dash-gh-ci-fetch repo #'ignore)
      (should (equal "live-branch" queried-branch)))))

(ert-deftest magit-dash-gh-ci/fetch-falls-back-to-repo-branch-when-detached ()
  "Falls back to the repo struct's :branch when the current branch is detached (empty)."
  (let* ((repo (magit-dash-gh-ci-test/make-repo "test" "/tmp/test" "fallback-branch"))
         (queried-branch nil))
    (cl-letf (((symbol-function 'magit-dash--current-branch)
               (lambda (_path) ""))
              ((symbol-function 'magit-dash-gh--run-process)
               (lambda (args _dir on-success &optional _on-error)
                 (setq queried-branch (nth (1+ (seq-position args "--branch")) args))
                 (funcall on-success "[]"))))
      (magit-dash-gh-ci-fetch repo #'ignore)
      (should (equal "fallback-branch" queried-branch)))))

(ert-deftest magit-dash-gh-ci/fetch-does-nothing-when-ci-disabled ()
  "Does not query anything when REPO's :include-ci is nil."
  (let ((repo (magit-dash-gh-ci-test/make-repo "test" "/tmp/test" "main" nil))
        (called nil))
    (cl-letf (((symbol-function 'magit-dash-gh--run-process)
               (lambda (&rest _) (setq called t))))
      (magit-dash-gh-ci-fetch repo #'ignore)
      (should-not called))))

;;; magit-dash-gh--owner-repo-from-url

(ert-deftest magit-dash-gh-ci/owner-repo-from-url-parses-ssh-and-https ()
  (should (equal '("owner" . "repo") (magit-dash-gh--owner-repo-from-url "git@github.com:owner/repo.git")))
  (should (equal '("owner" . "repo") (magit-dash-gh--owner-repo-from-url "https://github.com/owner/repo.git")))
  (should (equal '("owner" . "repo") (magit-dash-gh--owner-repo-from-url "https://github.com/owner/repo")))
  (should (equal '("owner" . "repo") (magit-dash-gh--owner-repo-from-url "owner/repo")))
  (should-not (magit-dash-gh--owner-repo-from-url "plain-name")))

;;; magit-dash-ci--repo-slug

(ert-deftest magit-dash-gh-ci/repo-slug-resolves-via-repo-info ()
  (let ((repo (magit-dash-gh-ci-test/make-repo "myrepo" "/tmp/myrepo")))
    (cl-letf (((symbol-function 'magit-dash-gh--repo-info)
               (lambda () '(:owner "owner" :repo "myrepo" :branch "main"))))
      (should (equal "owner/myrepo" (magit-dash-ci--repo-slug repo))))))

(ert-deftest magit-dash-gh-ci/repo-slug-prefers-repo-over-upstream ()
  "Fork's `:repo' attribute takes precedence over `:upstream-repo'."
  (let ((repo (magit-dash-repo--make
               :name "myfork"
               :path "/tmp/myfork"
               :repo "git@github.com:fork/myrepo.git"
               :upstream-repo "git@github.com:upstream/myrepo.git"
               :include-ci t)))
    (should (equal "fork/myrepo" (magit-dash-ci--repo-slug repo)))))

(ert-deftest magit-dash-gh-ci/parse-runs-prefers-target-sha ()
  "When target SHA is provided, runs for that commit are selected over newer ones."
  (let ((runs '(((databaseId . 200) (headSha . "sha-newer") (conclusion . "success") (status . "completed"))
                ((databaseId . 100) (headSha . "sha-target") (conclusion . "failure") (status . "completed")))))
    (should (equal 100 (plist-get (magit-dash-gh-ci--parse-runs runs "sha-target") :run-id)))
    (should (equal "failure" (plist-get (magit-dash-gh-ci--parse-runs runs "sha-target") :conclusion)))))

(ert-deftest magit-dash-gh-ci/repo-slug-errors-when-unresolvable ()
  (let ((repo (magit-dash-gh-ci-test/make-repo "bare-name" "/tmp/bare-name")))
    (cl-letf (((symbol-function 'magit-dash-gh--repo-info)
               (lambda () '(:owner nil :repo nil :branch "main"))))
      (should-error (magit-dash-ci--repo-slug repo) :type 'user-error))))

;;; magit-dash-ci-dispatch-fix-operation

(ert-deftest magit-dash-gh-ci/dispatch-fix-operation-errors-when-ci-disabled ()
  "Signals user-error when the repo does not have :include-ci set."
  (let ((repo (magit-dash-gh-ci-test/make-repo "disabled" "/tmp/disabled-repo" nil nil)))
    (should-error (magit-dash-ci-dispatch-fix-operation repo) :type 'user-error)))

(ert-deftest magit-dash-gh-ci/dispatch-fix-operation-dispatches-fix-ci ()
  "Dispatches fix-ci via agent-shell-workflow-dispatch with resolved slug."
  (let* ((repo (magit-dash-gh-ci-test/make-repo "myrepo" "/tmp/myrepo" "main"))
         (captured-prompt nil)
         (captured-kwargs nil))
    (cl-letf (((symbol-function 'magit-dash-ci--repo-slug) (lambda (_) "owner/myrepo"))
              ((symbol-function 'agent-shell-workflow-dispatch)
               (lambda (prompt &rest kwargs)
                 (setq captured-prompt prompt
                       captured-kwargs kwargs))))
      (magit-dash-ci-dispatch-fix-operation repo)
      (should (eq 'fix-ci captured-prompt))
      (should (equal "owner/myrepo" (plist-get (plist-get captured-kwargs :args) :repo))))))

(ert-deftest magit-dash-gh-ci/download-and-dispatch-dispatches-fix-ci-with-run-id ()
  "Dispatches fix-ci via agent-shell-workflow-dispatch with slug and run-id."
  (let* ((repo (magit-dash-gh-ci-test/make-repo "myrepo" "/tmp/myrepo" "main"))
         (captured-prompt nil)
         (captured-kwargs nil))
    (cl-letf (((symbol-function 'magit-dash-ci--repo-slug) (lambda (_) "owner/myrepo"))
              ((symbol-function 'agent-shell-workflow-dispatch)
               (lambda (prompt &rest kwargs)
                 (setq captured-prompt prompt
                       captured-kwargs kwargs))))
      (magit-dash-ci--download-and-dispatch repo 12345)
      (should (eq 'fix-ci captured-prompt))
      (should (equal "owner/myrepo" (plist-get (plist-get captured-kwargs :args) :repo)))
      (should (= 12345 (plist-get (plist-get captured-kwargs :args) :run-id))))))

(ert-deftest magit-dash-gh-ci/parse-runs-failure-in-any-workflow-wins ()
  (let ((ci (magit-dash-gh-ci--parse-runs
             '(((headSha . "a") (status . "completed") (conclusion . "success")
                (databaseId . 1) (url . "u1"))
               ((headSha . "a") (status . "completed") (conclusion . "failure")
                (databaseId . 2) (url . "u2"))
               ((headSha . "old") (status . "completed") (conclusion . "failure")
                (databaseId . 3) (url . "u3"))))))
    (should (equal "failure" (plist-get ci :conclusion)))
    (should (equal 2 (plist-get ci :run-id)))
    (should (equal 2 (plist-get ci :total)))))

(ert-deftest magit-dash-gh-ci/parse-runs-ignores-older-commits ()
  (let ((ci (magit-dash-gh-ci--parse-runs
             '(((headSha . "a") (status . "completed") (conclusion . "success")
                (databaseId . 1))
               ((headSha . "old") (status . "completed") (conclusion . "failure")
                (databaseId . 3))))))
    (should (equal "success" (plist-get ci :conclusion)))))

(ert-deftest magit-dash-gh-ci/parse-runs-pending-when-any-running ()
  (let ((ci (magit-dash-gh-ci--parse-runs
             '(((headSha . "a") (status . "completed") (conclusion . "success")
                (databaseId . 1))
               ((headSha . "a") (status . "in_progress") (conclusion . "")
                (databaseId . 2))))))
    (should (equal "in_progress" (plist-get ci :status)))
    (should-not (plist-get ci :conclusion))))

(ert-deftest magit-dash-gh-ci/dispatch-fix-operation-includes-cached-run-id ()
  "Dispatches fix-ci with cached run-id and context-dir when available."
  (let* ((repo (magit-dash-gh-ci-test/make-repo "myrepo" "/tmp/myrepo" "main"))
         (captured-prompt nil)
         (captured-kwargs nil))
    (cl-letf (((symbol-function 'magit-dash-ci--repo-slug) (lambda (_) "owner/myrepo"))
              ((symbol-function 'magit-dash-gh--cache-get)
               (lambda (path key)
                 (when (and (equal path "/tmp/myrepo") (eq key :ci-status))
                   (list :run-id 99999 :conclusion "failure"))))
              ((symbol-function 'agent-shell-workflow-dispatch)
               (lambda (prompt &rest kwargs)
                 (setq captured-prompt prompt
                       captured-kwargs kwargs))))
      (magit-dash-ci-dispatch-fix-operation repo)
      (should (eq 'fix-ci captured-prompt))
      (should (equal "owner/myrepo" (plist-get (plist-get captured-kwargs :args) :repo)))
      (should (= 99999 (plist-get (plist-get captured-kwargs :args) :run-id)))
      (should (equal "/tmp/myrepo/" (plist-get captured-kwargs :context-dir))))))

(ert-deftest magit-dash-status/ci-fix-falls-back-to-workflow-dispatch ()
  "magit-dash-status-ci-fix falls back to agent-shell-workflow-dispatch for unregistered repos."
  (let ((captured-prompt nil)
        (captured-kwargs nil))
    (cl-letf (((symbol-function 'magit-dash-status--repo-root)
               (lambda () "/tmp/unregistered-repo"))
              ((symbol-function 'magit-dash-gh--cache-get)
               (lambda (path key)
                 (when (and (equal path "/tmp/unregistered-repo") (eq key :ci-status))
                   (list :run-id 88888))))
              ((symbol-function 'agent-shell-workflow-dispatch)
               (lambda (prompt &rest kwargs)
                 (setq captured-prompt prompt
                       captured-kwargs kwargs))))
      (let ((magit-dash-repo-list nil))
        (magit-dash-status-ci-fix)
        (should (eq 'fix-ci captured-prompt))
        (should (= 88888 (plist-get (plist-get captured-kwargs :args) :run-id)))
        (should (equal "/tmp/unregistered-repo/" (plist-get captured-kwargs :context-dir)))))))

(ert-deftest magit-dash-gh-ci/fetch-passes-repo-slug ()
  "Includes --repo <slug> when repo slug can be resolved."
  (let* ((repo (magit-dash-gh-ci-test/make-repo "myrepo" "/tmp/myrepo" "main"))
         (captured-args nil))
    (cl-letf (((symbol-function 'magit-dash--current-branch) (lambda (_) "main"))
              ((symbol-function 'magit-dash-ci--repo-slug) (lambda (_) "owner/myrepo"))
              ((symbol-function 'magit-dash-gh--run-process)
               (lambda (args _dir on-success &optional _on-error)
                 (setq captured-args args)
                 (funcall on-success "[]"))))
      (magit-dash-gh-ci-fetch repo #'ignore)
      (should (member "--repo" captured-args))
      (should (equal "owner/myrepo" (nth (1+ (seq-position captured-args "--repo")) captured-args))))))

(ert-deftest magit-dash-gh-ci/fetch-empty-runs-caches-none ()
  "When gh returns 0 runs, cache :none status and format as —."
  (let* ((repo (magit-dash-gh-ci-test/make-repo "myrepo" "/tmp/myrepo" "main"))
         (magit-dash-gh--cache (make-hash-table :test #'equal))
         (magit-dash-gh--cache-requests (make-hash-table :test #'equal))
         (callback-result 'not-called))
    (cl-letf (((symbol-function 'magit-dash--current-branch) (lambda (_) "main"))
              ((symbol-function 'magit-dash-ci--repo-slug) (lambda (_) "owner/myrepo"))
              ((symbol-function 'magit-dash-gh--run-process)
               (lambda (_args _dir on-success &optional _on-error)
                 (funcall on-success "[]"))))
      (magit-dash-gh-ci-fetch repo (lambda (res) (setq callback-result res)))
      ;; Callback receives nil (no error, but no active runs)
      (should (null callback-result))
      ;; Cache holds (:none t) so it is distinguished from uncached
      (let ((cached (magit-dash-gh--cache-get "/tmp/myrepo" :ci-status)))
        (should (equal '(:none t) cached))
        (should (equal "—" (substring-no-properties (magit-dash-gh-ci--format-status cached)))))
      ;; magit-dash--repo-has-ci-status-p returns nil for :none
      (cl-letf (((symbol-function 'magit-dash--repo-at-point) (lambda () repo)))
        (should-not (magit-dash--repo-has-ci-status-p))))))

(ert-deftest magit-dash-gh-ci/repo-slug-resolves-from-string-path ()
  "magit-dash-ci--repo-slug accepts a raw path string."
  (cl-letf (((symbol-function 'magit-dash-gh--repo-info)
             (lambda () (list :owner "owner" :repo "myrepo" :branch "main"))))
    (should (equal "owner/myrepo" (magit-dash-ci--repo-slug "/tmp/myrepo")))))

(ert-deftest magit-dash-gh-ci/build-batch-graphql-query ()
  "Test GraphQL batch query generation for repositories."
  (let* ((r1 (magit-dash-repo--make :name "r1" :path "/tmp/r1"))
         (entries (list (list "r1" r1 "owner/repo1" "main")))
         (q (magit-dash-gh-ci--build-batch-graphql-query entries)))
    (should (string-match-p "r1: repository(owner: \"owner\", name: \"repo1\")" q))
    (should (string-match-p "refs/heads/main" q))))

(ert-deftest magit-dash-gh-ci/parse-graphql-rollup-states ()
  "Test parsing GraphQL statusCheckRollup data."
  (let* ((rollup-success '((state . "SUCCESS")
                           (contexts (nodes ((__typename . "CheckRun")
                                             (name . "test")
                                             (conclusion . "SUCCESS")
                                             (status . "COMPLETED"))))))
         (res-success (magit-dash-gh-ci--parse-graphql-rollup rollup-success))
         (rollup-fail '((state . "FAILURE")
                        (contexts (nodes ((__typename . "CheckRun")
                                          (name . "test")
                                          (conclusion . "FAILURE")
                                          (status . "COMPLETED"))))))
         (res-fail (magit-dash-gh-ci--parse-graphql-rollup rollup-fail)))
    (should (equal (plist-get res-success :conclusion) "success"))
    (should (equal (plist-get res-fail :conclusion) "failure"))
    ;; Ensure :null and nil rollups do not trigger map-elt method errors
    (should (null (magit-dash-gh-ci--parse-graphql-rollup :null)))
    (should (null (magit-dash-gh-ci--parse-graphql-rollup nil)))
    (should (null (magit-dash-gh-ci--parse-graphql-rollup :false)))))

(ert-deftest magit-dash-gh-ci/graphql-rollup-aggregates-all-checks ()
  "Count every check and report a failure even when it is not first."
  (let* ((rollup '((state . "FAILURE")
                   (contexts (nodes ((__typename . "CheckRun") (name . "ok")
                                     (conclusion . "SUCCESS") (status . "COMPLETED"))
                                    ((__typename . "CheckRun") (name . "failed")
                                     (conclusion . "FAILURE") (status . "COMPLETED")
                                     (databaseId . 888)
                                     (detailsUrl . "https://github.com/o/r/actions/runs/123/job/888"))))))
         (ci (magit-dash-gh-ci--parse-graphql-rollup rollup)))
    (should (= 2 (plist-get ci :total)))
    (should (= 1 (plist-get ci :pass)))
    (should (= 1 (plist-get ci :fail)))
    (should (equal "failure" (plist-get ci :conclusion)))
    (should (= 123 (plist-get ci :run-id)))))

(ert-deftest magit-dash-gh-ci/graphql-rollup-cancelled-and-pending ()
  "A cancelled check is visible and a pending check remains pending."
  (let* ((rollup '((state . "FAILURE")
                   (contexts (nodes ((__typename . "CheckRun") (conclusion . "SUCCESS")
                                     (status . "COMPLETED"))
                                    ((__typename . "CheckRun") (conclusion . "CANCELLED")
                                     (status . "COMPLETED"))))))
         (ci (magit-dash-gh-ci--parse-graphql-rollup rollup)))
    (should (= 2 (plist-get ci :total)))
    (should (equal "cancelled" (plist-get ci :conclusion)))
    (should-not (equal "—" (substring-no-properties (magit-dash-gh-ci--format-status ci)))))
  (let* ((rollup '((state . "PENDING")
                   (contexts (nodes ((__typename . "CheckRun") (conclusion . "SUCCESS")
                                     (status . "COMPLETED"))
                                    ((__typename . "CheckRun") (conclusion . :null)
                                     (status . "IN_PROGRESS"))))))
         (ci (magit-dash-gh-ci--parse-graphql-rollup rollup)))
    (should (= 2 (plist-get ci :total)))
    (should (equal "in_progress" (plist-get ci :status)))))

(ert-deftest magit-dash-gh-ci/graphql-rollup-respects-unseen-checks ()
  "A failure outside the first page must never be reported as passing."
  (let* ((rollup '((state . "FAILURE")
                   (contexts (totalCount . 21)
                             (nodes ((__typename . "CheckRun") (conclusion . "SUCCESS")
                                     (status . "COMPLETED"))))))
         (ci (magit-dash-gh-ci--parse-graphql-rollup rollup)))
    (should (equal "failure" (plist-get ci :conclusion)))
    (should (plist-get ci :truncated))
    (should-not (plist-get ci :run-id))))

(ert-deftest magit-dash-gh-ci/requested-check-displays-pending ()
  "Requested checks remain pending when another check succeeds."
  (let* ((rollup '((state . "PENDING")
                   (contexts (nodes ((__typename . "CheckRun") (conclusion . "SUCCESS")
                                     (status . "COMPLETED"))
                                    ((__typename . "CheckRun") (conclusion . :null)
                                     (status . "REQUESTED"))))))
         (ci (magit-dash-gh-ci--parse-graphql-rollup rollup)))
    (should (equal "requested" (plist-get ci :status)))
    (should (equal "⟳" (substring-no-properties (magit-dash-gh-ci--format-status ci))))))

(provide 'test-magit-dash-gh-ci)
;;; test-magit-dash-gh-ci.el ends here
