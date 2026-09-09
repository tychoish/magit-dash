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

;;; magit-dash-ci--build-fix-prompt

(ert-deftest magit-dash-gh-ci/build-fix-prompt-mentions-workflow-and-branch ()
  (let* ((repo (magit-dash-gh-ci-test/make-repo "myrepo" "/tmp/myrepo"))
         (ctx (list :dir "/tmp/myrepo/plans/ci-feature-1"
                    :run-info '((databaseId . 1) (workflowName . "CI")
                                (conclusion . "failure") (headBranch . "feature"))
                    :files (list '(:path "run-info.json" :type "metadata")
                                 '(:path "run-logs.ghlog" :type "logs"))))
         (prompt (magit-dash-ci--build-fix-prompt repo ctx)))
    (should (string-match-p "CI" prompt))
    (should (string-match-p "failed" prompt))
    (should (string-match-p "feature" prompt))
    (should (string-match-p "myrepo" prompt))
    (should (string-match-p "/tmp/myrepo" prompt))))

(ert-deftest magit-dash-gh-ci/build-fix-prompt-links-every-file ()
  (let* ((repo (magit-dash-gh-ci-test/make-repo))
         (ctx (list :dir "/tmp/test/plans/ci-main-2"
                    :run-info '((databaseId . 2) (workflowName . "CI")
                                (conclusion . "failure") (headBranch . "main"))
                    :files (list '(:path "run-info.json" :type "metadata")
                                 '(:path "run-logs.ghlog" :type "logs")
                                 '(:path "run-failed-logs.ghlog" :type "failed-logs"))))
         (prompt (magit-dash-ci--build-fix-prompt repo ctx)))
    (should (string-match-p (regexp-quote "/tmp/test/plans/ci-main-2/run-info.json") prompt))
    (should (string-match-p (regexp-quote "/tmp/test/plans/ci-main-2/run-logs.ghlog") prompt))
    (should (string-match-p (regexp-quote "/tmp/test/plans/ci-main-2/run-failed-logs.ghlog") prompt))))

(ert-deftest magit-dash-gh-ci/build-fix-prompt-non-failure-wording ()
  (let* ((repo (magit-dash-gh-ci-test/make-repo))
         (ctx (list :dir "/tmp/test/plans/ci-main-3"
                    :run-info '((databaseId . 3) (workflowName . "CI")
                                (conclusion . "cancelled") (headBranch . "main"))
                    :files nil))
         (prompt (magit-dash-ci--build-fix-prompt repo ctx)))
    (should (string-match-p "did not complete successfully" prompt))))

;;; magit-dash-ci--dispatch-prompt

(defun magit-dash-gh-ci-test--call-with-unbound (symbols thunk)
  "Call THUNK with each function symbol in SYMBOLS temporarily unbound.
Only symbols that are currently `fboundp' are unbound; each is restored to
its original definition afterward.  Used to simulate an environment where an
optional package (e.g. agent-shell-menu) isn't loaded, so a test exercises
the intended fallback branch regardless of what the running Emacs session
happens to have loaded — a `cl-letf' mock of the symbol's function cell
isn't enough here, since the dispatch code branches on `fboundp', not on
what the function does."
  (let* ((bound (seq-filter #'fboundp symbols))
         (saved (seq-map #'symbol-function bound)))
    (unwind-protect
        (progn
          (seq-do #'fmakunbound bound)
          (funcall thunk))
      (seq-mapn (lambda (s f) (fset s f)) bound saved))))

(ert-deftest magit-dash-gh-ci/dispatch-prompt-sends-to-open-shell ()
  "Prefers an existing agent-shell buffer for the repo's project directory."
  (let* ((repo (magit-dash-gh-ci-test/make-repo "test" "/tmp/test"))
         (inserted nil)
         (fake-buf (generate-new-buffer " *fake-agent-shell*")))
    (unwind-protect
        (progn
          (with-current-buffer fake-buf
            (setq default-directory "/tmp/test/"))
          (cl-letf (((symbol-function 'agent-shell-buffers)
                     (lambda () (list fake-buf)))
                    ((symbol-function 'agent-shell-subscribe-to) #'ignore)
                    ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                    ((symbol-function 'agent-shell-insert)
                     (cl-function
                      (lambda (&key text submit shell-buffer)
                        (setq inserted (list text submit shell-buffer))))))
            (magit-dash-ci--dispatch-prompt repo "fix it please")
            (should (equal "fix it please" (nth 0 inserted)))
            (should (nth 1 inserted))
            (should (eq fake-buf (nth 2 inserted)))))
      (kill-buffer fake-buf))))

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
