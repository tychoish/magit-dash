;;; test-magit-dash-pr-dashboard-status.el --- Dashboard status tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Deterministic combined status and asynchronous comparison regression tests.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'magit-dash-pr)

(ert-deftest magit-dash-pr-dashboard-status-known ()
  (let* ((magit-dash-pr-dashboard--comparisons (make-hash-table :test #'equal))
         (pr '((number . 7) (baseRefOid . "base") (headRefOid . "head")
               (mergeable . "CONFLICTING")
               (statusCheckRollup . ((state . "SUCCESS"))))))
    (setf (map-elt magit-dash-pr-dashboard--comparisons '("o/r" "base" "head"))
          '(3 . 2))
    (let ((status (magit-dash-pr-dashboard--format-status pr "o/r" nil))
          (drift (magit-dash-pr-dashboard--format-drift pr "o/r")))
      (should (string-match-p "✓" status))
      (should (string-match-p "conflict" status))
      (should-not (string-match-p "stack-" status))
      (should-not (string-match-p "↑" status))
      (should (equal "↑3↓2" (substring-no-properties drift))))))

(ert-deftest magit-dash-pr-dashboard-status-stack-is-repository-scoped ()
  (let* ((pr '((number . 7) (baseRefName . "parent") (headRefName . "child")))
         (tabulated-list-entries
          (list (list (list :repo "o/r" :pr '((number . 6) (headRefName . "parent"))) []))))
    (should (string-match-p "stack#6"
                            (magit-dash-pr-dashboard--format-status pr "o/r" nil)))
    (should-not (string-match-p "stack"
                                (magit-dash-pr-dashboard--format-status pr "other/r" nil)))))

(ert-deftest magit-dash-pr-dashboard-status-stack-root ()
  (let* ((pr '((number . 6) (baseRefName . "main") (headRefName . "parent")
               (mergeable . "MERGEABLE")))
         (tabulated-list-entries
          (list (list (list :repo "o/r" :pr '((number . 7) (baseRefName . "parent"))) [])))
         (status (magit-dash-pr-dashboard--format-status pr "o/r" nil)))
    (should (string-match-p "stack#7" status))
    (should-not (string-match-p "clean" status))))

(ert-deftest magit-dash-pr-dashboard-status-unknown ()
  (let ((status (magit-dash-pr-dashboard--format-status '((number . 7)) "o/r" nil))
        (drift (magit-dash-pr-dashboard--format-drift '((number . 7)) "o/r")))
    (should-not (string-match-p "merge\\?" status))
    (should-not (string-match-p "clean" status))
    (should-not (string-match-p "stack-" status))
    (should (equal "" drift))))

(ert-deftest magit-dash-pr-dashboard-comparison-async-cache-and-stale-results ()
  (with-temp-buffer
    (let* ((pr '((number . 7) (baseRefOid . "base") (headRefOid . "head")))
           (tabulated-list-entries (list (list (list :repo "o/r" :pr pr) [])))
           (calls nil)
           (prints 0))
      (cl-letf (((symbol-function 'magit-dash-gh--run-process)
                 (lambda (args _dir ok fail) (push (list args ok fail) calls)))
                ((symbol-function 'magit-dash-pr-dashboard--rebuild-entries)
                 (lambda () (setq prints (1+ prints)))))
        (magit-dash-pr-dashboard--enrich-comparisons (current-buffer) default-directory)
        (should (= 1 (length calls)))
        (should (member "repos/o/r/compare/base...head" (caar calls)))
        (magit-dash-pr-dashboard--enrich-comparisons (current-buffer) default-directory)
        (should (= 1 (length calls)))
        (funcall (cadar calls) "{\"ahead_by\":3,\"behind_by\":2}")
        (should (equal '(3 . 2)
                       (map-elt magit-dash-pr-dashboard--comparisons '("o/r" "base" "head"))))
        (should (= 1 prints))
        (magit-dash-pr-dashboard--enrich-comparisons (current-buffer) default-directory)
        (should (= 1 (length calls)))
        ;; Changing heads must request a new comparison; old counts never match.
        (setf (map-elt pr 'headRefOid) "new")
        (magit-dash-pr-dashboard--enrich-comparisons (current-buffer) default-directory)
        (should (= 2 (length calls)))
        (should (equal ""
                       (magit-dash-pr-dashboard--format-drift pr "o/r")))
        (funcall (caddar calls) "failure" 1)
        (should-not (map-elt magit-dash-pr-dashboard--comparisons '("o/r" "base" "new")))))))

(ert-deftest magit-dash-pr-dashboard-comparison-failure-continues ()
  (with-temp-buffer
    (let* ((pr '((number . 7) (baseRefOid . "base") (headRefOid . "head")))
           (other '((number . 8) (baseRefOid . "base") (headRefOid . "other")))
           (tabulated-list-entries
            (list (list (list :repo "o/r" :pr pr) [])
                  (list (list :repo "o/r" :pr other) [])))
           calls)
      (cl-letf (((symbol-function 'magit-dash-gh--run-process)
                 (lambda (_args _dir ok fail) (push (cons ok fail) calls)))
                ((symbol-function 'magit-dash-pr-dashboard--rebuild-entries) #'ignore))
        (magit-dash-pr-dashboard--enrich-comparisons (current-buffer) default-directory)
        (funcall (cdar calls) "failure" 1)
        (should (= 2 (length calls)))
        ;; Malformed replies must not strand the queue or invent zero counts.
        (funcall (caar calls) "not JSON")
        (should (= 2 (length calls)))
        (should (zerop (map-length magit-dash-pr-dashboard--comparison-pending)))
        (should-not (map-elt magit-dash-pr-dashboard--comparisons '("o/r" "base" "other")))))))

(ert-deftest magit-dash-pr-dashboard-comparison-changed-head-while-pending ()
  (with-temp-buffer
    (let* ((pr '((number . 7) (baseRefOid . "base") (headRefOid . "head")))
           (tabulated-list-entries (list (list (list :repo "o/r" :pr pr) [])))
           callbacks)
      (cl-letf (((symbol-function 'magit-dash-gh--run-process)
                 (lambda (_args _dir ok _fail) (push ok callbacks)))
                ((symbol-function 'magit-dash-pr-dashboard--rebuild-entries) #'ignore))
        (magit-dash-pr-dashboard--enrich-comparisons (current-buffer) default-directory)
        (setf (map-elt pr 'headRefOid) "new")
        (funcall (car callbacks) "{\"ahead_by\":3,\"behind_by\":2}")
        (should (= 2 (length callbacks)))
        (should (equal ""
                       (magit-dash-pr-dashboard--format-drift pr "o/r")))
        (funcall (car callbacks) "{\"ahead_by\":0,\"behind_by\":0}")
        (should (equal ""
                       (magit-dash-pr-dashboard--format-drift pr "o/r")))))))


(ert-deftest magit-dash-pr-dashboard-comparison-launch-failure-continues ()
  "A synchronous process launch failure must not strand later comparisons."
  (with-temp-buffer
    (let* ((tabulated-list-entries
            '(((:repo "o/r" :pr ((number . 1) (baseRefOid . "b") (headRefOid . "h"))) [])
              ((:repo "o/r" :pr ((number . 2) (baseRefOid . "b") (headRefOid . "h2"))) [])))
           (calls 0)
           finish)
      (cl-letf (((symbol-function 'magit-dash-gh--run-process)
                 (lambda (_args _dir ok _fail)
                   (setq calls (1+ calls))
                   (if (= calls 1)
                       (error "Cannot launch gh")
                     (setq finish ok))))
                ((symbol-function 'magit-dash-pr-dashboard--rebuild-entries) #'ignore))
        (magit-dash-pr-dashboard--enrich-comparisons (current-buffer) default-directory)
        (should (= calls 2))
        (should (map-contains-key magit-dash-pr-dashboard--comparisons '("o/r" "b" "h")))
        (should-not (map-elt magit-dash-pr-dashboard--comparisons '("o/r" "b" "h")))
        (funcall finish "{\"ahead_by\":1,\"behind_by\":2}")
        (should (zerop (map-length magit-dash-pr-dashboard--comparison-pending)))))))

(ert-deftest magit-dash-pr-dashboard-comparison-nonobject-json ()
  "Valid JSON with the wrong shape is an unsuccessful comparison."
  (with-temp-buffer
    (let ((magit-dash-pr-dashboard--comparisons (make-hash-table :test #'equal))
          (pending (make-hash-table :test #'equal))
          (key '("o/r" "base" "head")))
      (cl-letf (((symbol-function 'magit-dash-pr-dashboard--rebuild-entries) #'ignore)
                ((symbol-function 'magit-dash-pr-dashboard--enrich-comparisons) #'ignore))
        (seq-do
         (lambda (reply)
           (setf (map-elt pending key) t)
           (magit-dash-pr-dashboard--finish-comparison
            (current-buffer) default-directory pending key reply)
           (should (zerop (map-length pending)))
           (should-not (map-elt magit-dash-pr-dashboard--comparisons key)))
         '("null" "123" "[]" "\"text\""))))))

(ert-deftest magit-dash-pr-dashboard-status-refresh-one-enriches-comparisons ()
  "Refreshing a row also updates comparison data for its new SHA pair."
  (with-temp-buffer
    (magit-dash-pr-dashboard-mode)
    (let ((entry '(:repo "o/r" :number 7 :pr ((number . 7))))
          (buf (current-buffer))
          enriched)
      (cl-letf (((symbol-function 'magit-dash-gh--repo-dir) (lambda () default-directory))
                ((symbol-function 'magit-dash-gh--run-process)
                 (lambda (_args _dir ok &rest _) (funcall ok "{}")))
                ((symbol-function 'magit-dash-pr-dashboard--replace-row) #'ignore)
                ((symbol-function 'magit-dash-pr-dashboard--refresh-status)
                 (lambda (_buf _key _pr _dir done) (funcall done)))
                ((symbol-function 'magit-dash-pr-dashboard--enrich-comparisons)
                 (lambda (target _dir) (push target enriched))))
        (magit-dash-pr-dashboard-refresh-entries (list entry))
        (should (equal enriched (list buf)))))))

(ert-deftest magit-dash-pr-dashboard-status-discovery-rebuild-is-buffer-scoped ()
  "An asynchronous discovery callback must not rebuild another table buffer."
  (with-temp-buffer
    (magit-dash-pr-dashboard-mode)
    (setq magit-dash-pr-dashboard--filters '(:repo "o/r" :state "open"))
    (let ((buf (current-buffer))
          callback rebuilt)
      (cl-letf (((symbol-function 'magit-dash-gh--repo-dir) (lambda () default-directory))
                ((symbol-function 'magit-dash-gh--run-process)
                 (lambda (_args _dir ok &rest _) (setq callback ok)))
                ((symbol-function 'magit-dash-pr--prime-account-logins) #'ignore)
                ((symbol-function 'magit-dash-pr-dashboard--cached-entries) #'ignore)
                ((symbol-function 'magit-dash-pr-dashboard--cache-row) #'ignore)
                ((symbol-function 'magit-dash-pr-dashboard--evict-ghosts) #'ignore)
                ((symbol-function 'magit-dash-pr-dashboard--collect-statuses) #'ignore)
                ((symbol-function 'magit-dash-pr-dashboard--enrich-comparisons) #'ignore)
                ((symbol-function 'magit-dash-pr-dashboard--print) #'ignore)
                ((symbol-function 'magit-dash-pr-dashboard--rebuild-entries)
                 (lambda () (push (current-buffer) rebuilt))))
        (magit-dash-pr-dashboard-refresh)
        (with-temp-buffer
          (tabulated-list-mode)
          (funcall callback "[{\"number\":7,\"title\":\"PR\"}]"))
        (should (equal rebuilt (list buf)))))))

(ert-deftest magit-dash-pr-dashboard-status-default-replaces-ci ()
  "Combined Status replaces default CI while explicit CI remains available."
  (let ((magit-dash-pr-columns (copy-tree magit-dash-pr--default-columns)))
    (should-not (memq 'ci (magit-dash-pr--active-columns)))
    (should (< (seq-position (magit-dash-pr--active-columns) 'status)
               (seq-position (magit-dash-pr--active-columns) 'title)))
    (setf (map-elt magit-dash-pr-columns 'ci) t)
    (should (memq 'ci (magit-dash-pr--active-columns)))))

(ert-deftest magit-dash-pr-dashboard-status-repo-min-width-and-appr-fallback ()
  "Repo keeps at least 5 cols (4 visible chars + ellipsis) and Appr falls back to reviewDecision."
  (let* ((entries '(((:repo "owner/long-repo-name") [])))
         (fmt (magit-dash-pr-dashboard--build-format 60 entries)))
    (should (>= (cadr (aref fmt 0)) 5)))
  (should (equal "✓" (substring-no-properties
                      (magit-dash-pr-dashboard--format-approvals
                       nil '((reviewDecision . "APPROVED"))))))
  (should (equal "!" (substring-no-properties
                      (magit-dash-pr-dashboard--format-approvals
                       nil '((reviewDecision . "CHANGES_REQUESTED"))))))
  (should (equal "0/?" (substring-no-properties
                        (magit-dash-pr-dashboard--format-approvals
                         nil '((reviewDecision . "REVIEW_REQUIRED")))))))

(ert-deftest magit-dash-pr-dashboard-status-collect-fills-missing-rows ()
  "After author batch completes, missing rows are fetched individually to populate Appr/Thr."
  (let ((fetched nil)
        batch-done)
    (cl-letf (((symbol-function 'magit-dash-pr-status-collect-all-async)
               (lambda (on-done &rest _) (setq batch-done on-done) #'ignore))
              ((symbol-function 'magit-dash-pr-dashboard--refresh-status)
               (lambda (_buf key _pr _dir done)
                 (push key fetched)
                 (funcall done))))
      (with-temp-buffer
        (magit-dash-pr-dashboard-mode)
        (setq tabulated-list-entries
              (list (magit-dash-pr-dashboard--build-entry '((number . 1)) "o/r")
                    (magit-dash-pr-dashboard--build-entry '((number . 2)) "o/r")))
        (magit-dash-pr-dashboard--set-status "o/r" 1 '(:approvals 1 :required 1))
        (magit-dash-pr-dashboard--collect-statuses (current-buffer) "/d")
        (funcall batch-done nil)
        (should (equal '(("o/r" . 2)) fetched))))))

(provide 'test-magit-dash-pr-dashboard-status)
;;; test-magit-dash-pr-dashboard-status.el ends here
