;;; test-magit-dash-gh-ci-strict.el --- Tests for strict CI observation -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for strict exact-SHA CI observation and policy resolution in
;; `magit-dash-gh-ci'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'test-helper)
(require 'magit-dash-gh-ci)

(defconst test-md-strict--sha-a "1111111111111111111111111111111111111111")
(defconst test-md-strict--sha-b "2222222222222222222222222222222222222222")

(ert-deftest test-md-strict-ci-ignores-wrong-sha-and-never-passes-vacuously ()
  ;; Wrong SHA success must NOT count toward requested SHA
  (let* ((runs `(((head_sha . ,test-md-strict--sha-b)
                  (name . "ert") (status . "completed") (conclusion . "success")
                  (run_id . 100) (run_attempt . 1))))
         (obs-before (magit-dash-gh-ci-evaluate-strict-observation
                      test-md-strict--sha-a runs nil
                      :repository "tychoish/foo" :ref "refs/heads/fix"
                      :policy 'all-at-head :observed-at 1100 :created-at 1000
                      :discovery-deadline 300))
         (obs-after (magit-dash-gh-ci-evaluate-strict-observation
                     test-md-strict--sha-a runs nil
                     :repository "tychoish/foo" :ref "refs/heads/fix"
                     :policy 'all-at-head :observed-at 1500 :created-at 1000
                     :discovery-deadline 300)))
    (should (= (plist-get obs-before :ignored-wrong-sha) 1))
    (should (eq (plist-get obs-before :verdict) 'pending))
    (should (plist-get obs-before :best-effort))
    (should (eq (plist-get obs-after :verdict) 'missing-checks))))

(ert-deftest test-md-strict-ci-rerun-attempt-dedup-and-commit-status ()
  ;; Attempt 1 failed, attempt 2 succeeded on same SHA + commit status succeeded
  (let* ((runs `(((head_sha . ,test-md-strict--sha-a)
                  (name . "ert") (app . ((slug . "github-actions")))
                  (status . "completed") (conclusion . "failure")
                  (run_id . 10) (run_attempt . 1))
                 ((head_sha . ,test-md-strict--sha-a)
                  (name . "ert") (app . ((slug . "github-actions")))
                  (status . "completed") (conclusion . "success")
                  (run_id . 10) (run_attempt . 2))))
         (statuses `(((sha . ,test-md-strict--sha-a)
                      (context . "lint") (state . "success"))))
         (obs (magit-dash-gh-ci-evaluate-strict-observation
               test-md-strict--sha-a runs statuses
               :repository "tychoish/foo" :ref "refs/heads/fix"
               :policy 'explicit-checks :required-checks '("ert" "lint")
               :observed-at 1100)))
    (should (eq (plist-get obs :verdict) 'succeeded))
    (should (= (plist-get obs :ignored-obsolete) 1))
    (should (equal (plist-get obs :actual-sha) test-md-strict--sha-a))))

(ert-deftest test-md-strict-ci-missing-required-check ()
  (let* ((runs `(((head_sha . ,test-md-strict--sha-a)
                  (name . "ert") (status . "completed") (conclusion . "success")
                  (run_id . 10) (run_attempt . 1))))
         (obs (magit-dash-gh-ci-evaluate-strict-observation
               test-md-strict--sha-a runs nil
               :repository "tychoish/foo" :ref "refs/heads/fix"
               :policy 'required-at-pr-head :required-checks '("ert" "package-lint")
               :observed-at 2000 :created-at 1000 :discovery-deadline 300)))
    (should (eq (plist-get obs :verdict) 'missing-checks))
    (should (equal (plist-get obs :missing-checks) '("package-lint")))))

(ert-deftest test-md-strict-ci-observe-main-workflows-and-errors ()
  ;; Main workflows discovery uses main only for workflow names, evaluates at requested SHA
  (cl-letf (((symbol-function 'magit-dash-gh--call-process)
             (lambda (args _dir)
               (cond
                ((equal (car args) "run")
                 (cons 0 "[{\"workflowName\":\"CI\"}]"))
                ((string-match-p "check-runs" (cadr args))
                 (cons 0 (format "{\"check_runs\":[{\"head_sha\":\"%s\",\"name\":\"CI\",\"status\":\"completed\",\"conclusion\":\"failure\",\"run_id\":77,\"run_attempt\":1}]}"
                                 test-md-strict--sha-a)))
                (t (cons 0 "{\"statuses\":[]}"))))))
    (let ((obs (magit-dash-gh-ci-observe-strict
                "tychoish/foo" "refs/heads/topic" test-md-strict--sha-a
                :policy-override '(:policy main-workflows))))
      (should (eq (plist-get obs :verdict) 'failed))
      (should (equal (plist-get obs :policy-source) "main-workflows"))
      (should (= (length (plist-get obs :failing-checks)) 1))))
  ;; Inaccessible branch protection returns unknown-policy
  (cl-letf (((symbol-function 'magit-dash-gh--call-process)
             (lambda (_args _dir) (cons 1 "HTTP 403 Forbidden"))))
    (let ((obs (magit-dash-gh-ci-observe-strict
                "tychoish/foo" "refs/heads/topic" test-md-strict--sha-a
                :policy-override '(:policy required-at-pr-head))))
      (should (eq (plist-get obs :verdict) 'unknown-policy))))
  ;; Rate limit returns rate-limited
  (cl-letf (((symbol-function 'magit-dash-gh--call-process)
             (lambda (_args _dir) (cons 1 "HTTP 429 API rate limit exceeded"))))
    (let ((obs (magit-dash-gh-ci-observe-strict
                "tychoish/foo" "refs/heads/topic" test-md-strict--sha-a
                :policy-override '(:policy all-at-head))))
      (should (eq (plist-get obs :verdict) 'rate-limited)))))

(provide 'test-magit-dash-gh-ci-strict)
;;; test-magit-dash-gh-ci-strict.el ends here
