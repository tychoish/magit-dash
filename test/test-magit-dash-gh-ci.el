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
  "Dispatches fix-ci via agent-shell-prompt-exec with resolved slug."
  (let* ((repo (magit-dash-gh-ci-test/make-repo "myrepo" "/tmp/myrepo" "main"))
         (captured-prompt nil)
         (captured-args nil))
    (cl-letf (((symbol-function 'magit-dash-ci--repo-slug) (lambda (_) "owner/myrepo"))
              ((symbol-function 'agent-shell-prompt-exec)
               (lambda (prompt args)
                 (setq captured-prompt prompt
                       captured-args args))))
      (magit-dash-ci-dispatch-fix-operation repo)
      (should (eq 'fix-ci captured-prompt))
      (should (equal "owner/myrepo" (plist-get captured-args :repo))))))
(provide 'test-magit-dash-gh-ci)
;;; test-magit-dash-gh-ci.el ends here
