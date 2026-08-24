;;; test-magit-dash-status.el --- Tests for magit-dash-status -*- lexical-binding: t; no-byte-compile: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'magit)
(require 'magit-dash-status)

;;; Test Helpers

(defun test-magit-dash-status--fake-ci-run (&rest kvs)
  "Create a test CI run plist with defaults."
  (let ((defaults (list :database-id 12345
                        :name "CI Workflow"
                        :workflow-name "build"
                        :status "completed"
                        :conclusion "success"
                        :created-at "2026-08-23T10:00:00Z"
                        :updated-at "2026-08-23T10:05:00Z"
                        :url "https://github.com/owner/repo/actions/runs/12345"
                        :head-sha "abc1234"
                        :event "push")))
    (while kvs
      (setq defaults (plist-put defaults (pop kvs) (pop kvs))))
    defaults))

(defun test-magit-dash-status--fake-pr (&rest kvs)
  "Create a test PR plist with defaults."
  (let ((defaults (list :number 42
                        :title "Add GitHub status to magit-status"
                        :state "OPEN"
                        :review-decision "APPROVED"
                        :url "https://github.com/owner/repo/pull/42"
                        :mergeable "MERGEABLE"
                        :head-ref "feature-status"
                        :base-ref "main"
                        :updated-at "2026-08-23T09:30:00Z"
                        :status-rollup '((conclusion . "SUCCESS")))))
    (while kvs
      (setq defaults (plist-put defaults (pop kvs) (pop kvs))))
    defaults))

;;; CI Symbol Tests

(ert-deftest magit-dash-status/ci-symbol-success ()
  (let ((sym (magit-dash-status--ci-symbol "success" "completed")))
    (should (string-match-p "✓ pass" sym))))

(ert-deftest magit-dash-status/ci-symbol-failure ()
  (let ((sym (magit-dash-status--ci-symbol "failure" "completed")))
    (should (string-match-p "✗ fail" sym)))
  (let ((sym (magit-dash-status--ci-symbol "timed_out" "completed")))
    (should (string-match-p "✗ fail" sym))))

(ert-deftest magit-dash-status/ci-symbol-running ()
  (let ((sym (magit-dash-status--ci-symbol nil "in_progress")))
    (should (string-match-p "● running" sym)))
  (let ((sym (magit-dash-status--ci-symbol nil "queued")))
    (should (string-match-p "● running" sym))))

(ert-deftest magit-dash-status/ci-symbol-cancelled ()
  (let ((sym (magit-dash-status--ci-symbol "cancelled" "completed")))
    (should (string-match-p "cancelled" sym))))

(ert-deftest magit-dash-status/ci-symbol-nil ()
  (let ((sym (magit-dash-status--ci-symbol nil nil)))
    (should (equal "—" (substring-no-properties sym)))))

;;; Rollup Status Check Tests

(ert-deftest magit-dash-status/rollup-symbol-states ()
  (should (equal "✓" (substring-no-properties (magit-dash-status--rollup-ci-symbol '((conclusion . "SUCCESS"))))))
  (should (equal "✗" (substring-no-properties (magit-dash-status--rollup-ci-symbol '((conclusion . "FAILURE"))))))
  (should (equal "●" (substring-no-properties (magit-dash-status--rollup-ci-symbol '((status . "IN_PROGRESS"))))))
  (should (equal "—" (substring-no-properties (magit-dash-status--rollup-ci-symbol nil)))))

;;; Review Decision Tests

(ert-deftest magit-dash-status/review-decision-formatting ()
  (should (string-match-p "Approved" (magit-dash-status--format-review-decision "APPROVED")))
  (should (string-match-p "Changes Requested" (magit-dash-status--format-review-decision "CHANGES_REQUESTED")))
  (should (string-match-p "Review Required" (magit-dash-status--format-review-decision "REVIEW_REQUIRED")))
  (should (equal "" (magit-dash-status--format-review-decision nil))))

;;; PR Row Formatting Tests

(ert-deftest magit-dash-status/format-pr-row ()
  (let* ((pr (test-magit-dash-status--fake-pr))
         (row (magit-dash-status--format-pr-row pr)))
    (should (string-match-p "#42" row))
    (should (string-match-p "Add GitHub status to magit-status" row))
    (should (string-match-p "Approved" row))))

;;; Parsing Tests

(ert-deftest magit-dash-status/parse-ci-run ()
  (let* ((json-alist '((databaseId . 999)
                       (name . "Test Workflow")
                       (workflowName . "test")
                       (status . "completed")
                       (conclusion . "success")
                       (createdAt . "2026-08-23T08:00:00Z")
                       (updatedAt . "2026-08-23T08:05:00Z")
                       (url . "https://github.com/foo/bar/actions/runs/999")
                       (headSha . "1234567")
                       (event . "pull_request")))
         (parsed (magit-dash-status--parse-ci-run json-alist)))
    (should (= 999 (plist-get parsed :database-id)))
    (should (equal "test" (plist-get parsed :workflow-name)))
    (should (equal "success" (plist-get parsed :conclusion)))
    (should (equal "https://github.com/foo/bar/actions/runs/999" (plist-get parsed :url)))))

(ert-deftest magit-dash-status/parse-pr ()
  (let* ((json-alist '((number . 101)
                       (title . "Refactor cache system")
                       (state . "OPEN")
                       (reviewDecision . "APPROVED")
                       (url . "https://github.com/foo/bar/pull/101")
                       (mergeable . "MERGEABLE")
                       (headRefName . "refactor-cache")
                       (baseRefName . "main")
                       (updatedAt . "2026-08-23T11:00:00Z")))
         (parsed (magit-dash-status--parse-pr json-alist)))
    (should (= 101 (plist-get parsed :number)))
    (should (equal "Refactor cache system" (plist-get parsed :title)))
    (should (equal "APPROVED" (plist-get parsed :review-decision)))
    (should (equal "refactor-cache" (plist-get parsed :head-ref)))))

;;; Status Buffer Inserters in Temp Buffer

(ert-deftest magit-dash-status/insert-ci-header-cached ()
  (with-temp-buffer
    (let* ((top "/tmp/fake-repo")
           (ci-run (test-magit-dash-status--fake-ci-run)))
      (cl-letf (((symbol-function 'magit-dash-status--github-repo-p) (lambda (&rest _) t))
                ((symbol-function 'magit-dash-status--repo-root) (lambda (&rest _) top)))
        (magit-dash-gh--cache-set top :status-ci ci-run)
        (magit-dash-gh--cache-set top :status-ci-time (float-time))
        (magit-dash-status-insert-ci-header)
        (let ((content (buffer-string)))
          (should (string-match-p "CI: " content))
          (should (string-match-p "✓ pass" content))
          (should (string-match-p "build" content)))))))

(ert-deftest magit-dash-status/insert-pr-header-cached ()
  (with-temp-buffer
    (let* ((top "/tmp/fake-repo")
           (pr (test-magit-dash-status--fake-pr)))
      (cl-letf (((symbol-function 'magit-dash-status--github-repo-p) (lambda (&rest _) t))
                ((symbol-function 'magit-dash-status--repo-root) (lambda (&rest _) top)))
        (magit-dash-gh--cache-set top :status-branch-pr pr)
        (magit-dash-gh--cache-set top :status-branch-pr-time (float-time))
        (magit-dash-status-insert-pr-header)
        (let ((content (buffer-string)))
          (should (string-match-p "PR: " content))
          (should (string-match-p "#42" content))
          (should (string-match-p "Add GitHub status to magit-status" content))
          (should (string-match-p "Approved" content)))))))

(ert-deftest magit-dash-status/insert-authored-prs-section ()
  (with-temp-buffer
    (let* ((top "/tmp/fake-repo")
           (prs (list (test-magit-dash-status--fake-pr :number 10 :title "PR 10")
                      (test-magit-dash-status--fake-pr :number 20 :title "PR 20"))))
      (cl-letf (((symbol-function 'magit-dash-status--github-repo-p) (lambda (&rest _) t))
                ((symbol-function 'magit-dash-status--repo-root) (lambda (&rest _) top)))
        (magit-dash-gh--cache-set top :status-authored-prs prs)
        (magit-dash-gh--cache-set top :status-authored-prs-time (float-time))
        (magit-dash-status-insert-authored-prs)
        (let ((content (buffer-string)))
          (should (string-match-p "Pull Requests (author: @me, 2)" content))
          (should (string-match-p "#10" content))
          (should (string-match-p "PR 10" content))
          (should (string-match-p "#20" content))
          (should (string-match-p "PR 20" content)))))))

(ert-deftest magit-dash-status/insert-authored-prs-omitted-when-empty ()
  "Section is omitted entirely when no open authored PRs exist."
  (with-temp-buffer
    (let ((top "/tmp/fake-repo"))
      (cl-letf (((symbol-function 'magit-dash-status--github-repo-p) (lambda (&rest _) t))
                ((symbol-function 'magit-dash-status--repo-root) (lambda (&rest _) top)))
        (magit-dash-gh--cache-set top :status-authored-prs nil)
        (magit-dash-gh--cache-set top :status-authored-prs-time (float-time))
        (magit-dash-status-insert-authored-prs)
        (should (equal "" (buffer-string)))))))
;;; In-Flight and Recursion Safety Tests

(ert-deftest magit-dash-status/in-flight-deduplication ()
  "Do not launch duplicate process when fetch is already in flight."
  (let* ((top (make-temp-file "magit-test-repo-" t))
         (spawn-count 0))
    (unwind-protect
        (cl-letf (((symbol-function 'magit-dash-status--repo-root) (lambda (&rest _) top))
                  ((symbol-function 'magit-get-current-branch) (lambda (&rest _) "main"))
                  ((symbol-function 'magit-dash-gh--run-process)
                   (lambda (&rest _) (setq spawn-count (1+ spawn-count)) nil)))
          ;; Mark in-flight
          (magit-dash-status--set-in-flight top :status-ci t)
          (magit-dash-status--fetch-ci top #'ignore)
          (should (= spawn-count 0))
          ;; Clear in-flight -> process can spawn
          (magit-dash-status--set-in-flight top :status-ci nil)
          (magit-dash-status--fetch-ci top #'ignore)
          (should (= spawn-count 1)))
      (delete-directory top t))))

(ert-deftest magit-dash-status/detached-head-safe ()
  "When branch is nil or empty, set empty cache without infinite recursion."
  (let* ((top (make-temp-file "magit-test-repo-" t))
         (called-back nil))
    (unwind-protect
        (cl-letf (((symbol-function 'magit-dash-status--repo-root) (lambda (&rest _) top))
                  ((symbol-function 'magit-get-current-branch) (lambda (&rest _) nil)))
          (magit-dash-status--fetch-ci
           top
           (lambda (res)
             (setq called-back t)
             (should (null res))))
          (should called-back)
          (should (eq (magit-dash-gh--cache-get top :status-ci) :empty))
          (should (numberp (magit-dash-gh--cache-get top :status-ci-time))))
      (delete-directory top t))))
;;; Keymap & Setup Tests

(ert-deftest magit-dash-status/setup-installs-hooks-and-keys ()
  (magit-dash-status-setup)
  (should (memq #'magit-dash-status-insert-ci-header magit-status-headers-hook))
  (should (memq #'magit-dash-status-insert-pr-header magit-status-headers-hook))
  (should (memq #'magit-dash-status-insert-authored-prs magit-status-sections-hook))
  (when (boundp 'magit-mode-map)
    (should (eq (lookup-key magit-mode-map ".") #'magit-dash-dispatch))))

(ert-deftest magit-dash-status/pr-checks-calls-fetch-for-pr ()
  "pr-checks calls magit-dash-gh-actions-fetch-for-pr when a PR is present."
  (let ((pr-args nil))
    (cl-letf (((symbol-function 'fboundp)
               (lambda (sym) (if (eq sym 'magit-gh-pr-checks) nil (cl-typep sym '(or symbol function)))))
              ((symbol-function 'magit-dash-status--current-pr)
               (lambda () '(:number 99)))
              ((symbol-function 'magit-dash-status--repo-root)
               (lambda () "/tmp/fake-repo"))
              ((symbol-function 'magit-dash-gh-actions-fetch-for-pr)
               (lambda (num dir) (setq pr-args (list num dir)))))
      (magit-dash-status-pr-checks)
      (should (equal '(99 "/tmp/fake-repo") pr-args)))))
(provide 'test-magit-dash-status)
;;; test-magit-dash-status.el ends here
