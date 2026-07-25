;;; test-magit-dash-gh-ci.el --- ERT tests for magit-dash-gh-ci -*- lexical-binding: t -*-

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

(ert-deftest magit-dash-gh-ci/dispatch-prompt-queues-when-no-shell-open ()
  "Falls back to the unassigned queue bucket when no shell is open and no
agent-shell-menu is loaded."
  (magit-dash-gh-ci-test--call-with-unbound
   '(agent-shell-menu-new-shell-in-dir)
   (lambda ()
     (let* ((repo (magit-dash-gh-ci-test/make-repo))
            (queued nil))
       (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () nil))
                 ((symbol-function 'agent-shell-queue-add-unassigned)
                  (lambda (prompt &optional _background) (setq queued prompt))))
         (magit-dash-ci--dispatch-prompt repo "fix it please")
         (should (equal "fix it please" queued)))))))

(ert-deftest magit-dash-gh-ci/dispatch-prompt-falls-back-to-kill-ring ()
  "Copies to the kill ring when neither agent-shell-menu nor the queue is available."
  (magit-dash-gh-ci-test--call-with-unbound
   '(agent-shell-menu-new-shell-in-dir agent-shell-queue-add-unassigned)
   (lambda ()
     (let ((repo (magit-dash-gh-ci-test/make-repo)))
       (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () nil)))
         (kill-new "unrelated")
         (magit-dash-ci--dispatch-prompt repo "fix it please")
         (should (equal "fix it please" (current-kill 0))))))))

;;; magit-dash-ci-dispatch-fix-operation

(ert-deftest magit-dash-gh-ci/dispatch-fix-operation-errors-when-ci-disabled ()
  "Signals user-error when the repo does not have :include-ci set."
  (let ((repo (magit-dash-gh-ci-test/make-repo "disabled" "/tmp/disabled-repo" nil nil)))
    (should-error (magit-dash-ci-dispatch-fix-operation repo) :type 'user-error)))

(ert-deftest magit-dash-gh-ci/dispatch-fix-operation-uses-cached-run-id ()
  "Passes the cached run-id through to the download pipeline without fetching."
  (let* ((repo (magit-dash-gh-ci-test/make-repo "cached" "/tmp/cached-repo" "main"))
         (captured-ctx nil))
    (magit-dash-gh--cache-set "/tmp/cached-repo" :ci-status (list :run-id 123 :url "https://example.com"))
    (cl-letf (((symbol-function 'magit-dash-gh--check-gh) (lambda () nil))
              ((symbol-function 'magit-dash-gh-ci-fetch)
               (lambda (&rest _) (error "should not fetch when status is already cached")))
              ((symbol-function 'magit-dash-gh-actions--step-run-info)
               (lambda (ctx) (setq captured-ctx ctx))))
      (magit-dash-ci-dispatch-fix-operation repo)
      (should (= 123 (plist-get captured-ctx :run-id)))
      (should (equal "/tmp/cached-repo" (plist-get captured-ctx :repo-dir)))
      (should (equal "main" (plist-get captured-ctx :branch)))
      (should (functionp (plist-get captured-ctx :on-complete))))))

(ert-deftest magit-dash-gh-ci/dispatch-fix-operation-fetches-when-uncached ()
  "Fetches CI status first when none is cached, then proceeds with its run-id."
  (let* ((repo (magit-dash-gh-ci-test/make-repo "uncached" "/tmp/uncached-repo" "main"))
         (fetch-called nil)
         (captured-ctx nil))
    (cl-letf (((symbol-function 'magit-dash-gh--check-gh) (lambda () nil))
              ((symbol-function 'magit-dash-gh-ci-fetch)
               (lambda (r callback)
                 (setq fetch-called r)
                 (funcall callback (list :run-id 456 :url "https://example.com"))))
              ((symbol-function 'magit-dash-gh-actions--step-run-info)
               (lambda (ctx) (setq captured-ctx ctx))))
      (magit-dash-ci-dispatch-fix-operation repo)
      (should (eq repo fetch-called))
      (should (= 456 (plist-get captured-ctx :run-id))))))

(ert-deftest magit-dash-gh-ci/dispatch-fix-operation-messages-when-fetch-finds-no-run ()
  "Does not error or download when the fetch callback finds no run."
  (let* ((repo (magit-dash-gh-ci-test/make-repo "empty" "/tmp/empty-repo" "main"))
         (download-called nil))
    (cl-letf (((symbol-function 'magit-dash-gh--check-gh) (lambda () nil))
              ((symbol-function 'magit-dash-gh-ci-fetch)
               (lambda (_r callback) (funcall callback nil)))
              ((symbol-function 'magit-dash-gh-actions--step-run-info)
               (lambda (&rest _) (setq download-called t))))
      (magit-dash-ci-dispatch-fix-operation repo)
      (should-not download-called))))

(provide 'test-magit-dash-gh-ci)
;;; test-magit-dash-gh-ci.el ends here
