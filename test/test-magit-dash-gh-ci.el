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

(provide 'test-magit-dash-gh-ci)
;;; test-magit-dash-gh-ci.el ends here
