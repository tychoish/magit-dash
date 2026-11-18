;;; test-magit-dash-pr-status-org.el --- ERT tests for magit-dash-pr-status-org -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:

;; ERT test suite for `magit-dash-pr-status-org'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'magit-dash-pr)
(require 'magit-dash-pr-status)
(require 'magit-dash-pr-status-org)

(defconst md-test/fixture
  "#+title: Open PR status (@me)
#+filetags: :agent:prs:

* Summary
Last refreshed: [2026-11-06 Fri 12:00]

| PR | Repo | Branch | Approvals | CI failing | Unresolved | Bucket |
|-
| 1 | casaphq/go-monorepo | b1 | 1 | 0 | 0 | ready |
| 2 | casaphq/go-monorepo | b2 | 0 | 0 | 1 | threads |

* Tasks
** TODO 1: 1: ready
:PROPERTIES:
:REPO: casaphq/go-monorepo
:PR: 1
:STEP: ready
:LAST_SEEN: [2026-11-06 Fri 12:00]
:BRANCH: b1
:URL: https://github.com/casaphq/go-monorepo/pull/1
:END:
- PR one
*** TODO merge
:PROPERTIES:
:ACTION: merge
:END:

** TODO 2: 2: respond to 1 open review thread
:PROPERTIES:
:REPO: casaphq/go-monorepo
:PR: 2
:STEP: threads
:LAST_SEEN: [2026-11-06 Fri 12:00]
:BRANCH: b2
:URL: https://github.com/casaphq/go-monorepo/pull/2
:END:
- PR two
*** TODO respond to 1 open review thread
:PROPERTIES:
:ACTION: threads
:END:
")

(defun md-test/pr (n &rest kvs)
  "Build a mock PR plist for PR number N with KVS overrides."
  (let ((base (list :number n
                    :repo "casaphq/go-monorepo"
                    :branch (format "b%d" n)
                    :url (format "https://github.com/casaphq/go-monorepo/pull/%d" n)
                    :title (format "PR %d" n)
                    :bucket 'ready
                    :approvals 1
                    :threads-open 0
                    :tracking (list :last-seen "[2026-11-18 Wed 10:00]"
                                    :action-summary (format "%d: ready" n)))))
    (while kvs
      (setq base (plist-put base (pop kvs) (pop kvs))))
    base))

(defmacro md-test/with-file (sym initial &rest body)
  "Create temp file SYM holding INITIAL, execute BODY, and delete file."
  (declare (indent 2))
  `(let ((,sym (make-temp-file "md-test-pr-status-" nil ".org")))
     (unwind-protect
         (progn
           (when ,initial
             (with-temp-file ,sym (insert ,initial)))
           ,@body)
       (when (file-exists-p ,sym) (delete-file ,sym)))))

(defun md-test/slurp (f)
  "Read contents of file F."
  (with-temp-buffer
    (insert-file-contents f)
    (buffer-string)))

(ert-deftest md-test/sync-file-skeleton-created ()
  (md-test/with-file f nil
    (magit-dash-pr-status-sync-file (list (md-test/pr 1)) f)
    (let ((content (md-test/slurp f)))
      (should (string-match-p "^#\\+title: Open PR status" content))
      (should (string-match-p "^\\* Summary" content))
      (should (string-match-p "^\\* Tasks" content))
      (should (string-match-p "^\\*\\* TODO 1:" content)))))

(ert-deftest md-test/summary-table ()
  (md-test/with-file f nil
    (let ((prs (list (md-test/pr 1) (md-test/pr 2 :bucket 'threads :threads-open 1 :approvals 0))))
      (magit-dash-pr-status-sync-file prs f)
      (let ((content (md-test/slurp f)))
        (should (string-match-p "| PR +| Repo +| Branch +| Approvals +| CI failing +| Unresolved +| Bucket" content))
        (should (string-match-p "| +1 | casaphq/go-monorepo | b1 +| +1 | +0 | +0 | ready" content))
        (should (string-match-p "| +2 | casaphq/go-monorepo | b2 +| +0 | +0 | +1 | threads" content))))))

(ert-deftest md-test/step-change-updates-heading ()
  (md-test/with-file f md-test/fixture
    (let ((pr (md-test/pr 1 :bucket 'fix-ci
                          :failing '(("ci/build" . "url"))
                          :tracking (list :last-seen "[2026-11-18 Wed 10:05]"
                                          :action-summary "1: fix failing tests: ci/build"
                                          :failing-checks '("ci/build")))))
      (magit-dash-pr-status-sync-file (list pr) f)
      (let ((content (md-test/slurp f)))
        (should (string-match-p "^\\*\\* TODO 1: 1: fix failing tests: ci/build" content))
        (should (string-match-p ":STEP: +fix-ci" content))))))

(ert-deftest md-test/closed-pr-done ()
  (md-test/with-file f md-test/fixture
    (let ((magit-dash-pr-status-max-prs 50))
      ;; PR 2 is omitted; should be marked DONE with CLOSED planning info.
      (magit-dash-pr-status-sync-file (list (md-test/pr 1)) f)
      (let ((content (md-test/slurp f)))
        (should (string-match-p "^\\*\\* TODO 1:" content))
        (should (string-match-p "^\\*\\* DONE 2:" content))
        (should (string-match-p "CLOSED: \\[" content))))))

(ert-deftest md-test/truncated-list-does-not-close ()
  (md-test/with-file f md-test/fixture
    (let ((magit-dash-pr-status-max-prs 1))
      ;; Since length(prs) >= max-prs (1 >= 1), close-allowed-p is false!
      (magit-dash-pr-status-sync-file (list (md-test/pr 1)) f)
      (let ((content (md-test/slurp f)))
        (should (string-match-p "^\\*\\* TODO 2:" content))))))

(ert-deftest md-test/done-task-reopens-when-pr-present ()
  (md-test/with-file f md-test/fixture
    (let ((magit-dash-pr-status-max-prs 50))
      ;; First close PR 2
      (magit-dash-pr-status-sync-file (list (md-test/pr 1)) f)
      (should (string-match-p "^\\*\\* DONE 2:" (md-test/slurp f)))
      ;; Now reopen PR 2
      (magit-dash-pr-status-sync-file (list (md-test/pr 1) (md-test/pr 2)) f)
      (should (string-match-p "^\\*\\* TODO 2:" (md-test/slurp f))))))

(ert-deftest md-test/sync-pure-elisp-synchronous ()
  (md-test/with-file f nil
    (let ((magit-dash-pr-status-file f)
          (magit-dash-pr-status-org--fingerprint nil)
          (magit-dash-pr-status-org--last-count 0))
      (cl-letf (((symbol-function 'magit-dash-pr-status-collect-all)
                 (lambda (&rest _) (list (md-test/pr 1)))))
        (magit-dash-pr-status-sync t f)
        (should (string-match-p "^\\*\\* TODO 1:" (md-test/slurp f)))
        (should magit-dash-pr-status-org--fingerprint)
        (should (= magit-dash-pr-status-org--last-count 1))))))

(ert-deftest md-test/sync-pure-elisp-asynchronous ()
  (md-test/with-file f nil
    (let ((magit-dash-pr-status-file f)
          (magit-dash-pr-status-org--fingerprint nil)
          (magit-dash-pr-status-org--last-count 0)
          (done nil))
      (cl-letf (((symbol-function 'magit-dash-pr-status-collect-all-async)
                 (lambda (on-success _on-err &rest _)
                   (funcall on-success (list (md-test/pr 1))))))
        (magit-dash-pr-status-sync 'async f (lambda (_file _status) (setq done t)))
        (should done)
        (should (string-match-p "^\\*\\* TODO 1:" (md-test/slurp f)))))))

(ert-deftest md-test/sync-fingerprint-unchanged-skips ()
  (md-test/with-file f nil
    (let ((magit-dash-pr-status-file f)
          (magit-dash-pr-status-org--fingerprint nil)
          (magit-dash-pr-status-org--last-count 0))
      (cl-letf (((symbol-function 'magit-dash-pr-status-collect-all)
                 (lambda (&rest _) (list (md-test/pr 1)))))
        (magit-dash-pr-status-sync t f)
        (let ((before (md-test/slurp f)))
          (magit-dash-pr-status-sync t f
                                     (lambda (_file status)
                                       (should (eq status 'unchanged))))
          (should (equal before (md-test/slurp f))))))))

(ert-deftest md-test/empty-after-nonempty-skips ()
  (md-test/with-file f nil
    (let ((magit-dash-pr-status-file f)
          (magit-dash-pr-status-org--fingerprint nil)
          (magit-dash-pr-status-org--last-count 0))
      (cl-letf (((symbol-function 'magit-dash-pr-status-collect-all)
                 (lambda (&rest _) (list (md-test/pr 1)))))
        (magit-dash-pr-status-sync t f)
        (let ((before (md-test/slurp f)))
          ;; PR collection returns empty list:
          (cl-letf (((symbol-function 'magit-dash-pr-status-collect-all)
                     (lambda (&rest _) nil)))
            (magit-dash-pr-status-sync t f
                                       (lambda (_file status)
                                         (should (eq status 'empty-after-nonempty))))
            (should (equal before (md-test/slurp f)))))))))

(ert-deftest md-test/failure-warning-escalation ()
  (md-test/with-file f nil
    (let ((magit-dash-pr-status-file f)
          (magit-dash-pr-status-org--failures 0)
          (warnings nil))
      (cl-letf (((symbol-function 'magit-dash-pr-status-collect-all)
                 (lambda (&rest _) (error "504 Gateway Timeout")))
                ((symbol-function 'display-warning)
                 (lambda (type msg &rest _) (push (list type msg) warnings))))
        (magit-dash-pr-status-sync t f)
        (should (= magit-dash-pr-status-org--failures 1))
        (should-not warnings)
        (magit-dash-pr-status-sync t f)
        (should (= magit-dash-pr-status-org--failures 2))
        (should-not warnings)
        (magit-dash-pr-status-sync t f)
        (should (= magit-dash-pr-status-org--failures 3))
        (should warnings)
        (should (string-match-p "3 consecutive collection failures" (cadr (car warnings))))))))

(ert-deftest md-test/timer-lifecycle ()
  (let ((magit-dash-pr-status-org--timer nil)
        (magit-dash-pr-status-timer-interval 120))
    (unwind-protect
        (progn
          (magit-dash-pr-status-start-timer)
          (should (timerp magit-dash-pr-status-org--timer))
          (should (= (timer--repeat-delay magit-dash-pr-status-org--timer) 120))
          ;; Calling start again restarts cleanly without leak:
          (magit-dash-pr-status-start-timer 60)
          (should (= (timer--repeat-delay magit-dash-pr-status-org--timer) 60))
          (magit-dash-pr-status-stop-timer)
          (should-not magit-dash-pr-status-org--timer)
          ;; Stop is idempotent:
          (magit-dash-pr-status-stop-timer)
          (should-not magit-dash-pr-status-org--timer))
      (when (timerp magit-dash-pr-status-org--timer)
        (cancel-timer magit-dash-pr-status-org--timer)))))

(provide 'test-magit-dash-pr-status-org)
;;; test-magit-dash-pr-status-org.el ends here
