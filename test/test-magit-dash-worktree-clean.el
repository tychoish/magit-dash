;;; test-magit-dash-worktree-clean.el --- ERT tests for magit-dash-worktree-clean -*- lexical-binding: t; no-byte-compile: t; -*-

;; Batch run:
;;   emacs --batch -l test/test-helper.el -l test/test-magit-dash-worktree-clean.el \
;;     --eval '(ert-run-tests-batch-and-exit "magit-dash-worktree-clean/")'

(require 'ert)
(require 'cl-lib)
(require 'magit-dash-worktree-clean)

(defmacro magit-dash-worktree-clean-test--with-store (&rest body)
  "Run BODY with plans stored in a throwaway directory."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "mdwc-test-" t))
          (magit-dash-worktree-clean-plan-directory dir))
     (unwind-protect (progn ,@body)
       (delete-directory dir t))))

(defun magit-dash-worktree-clean-test--target (name dirty)
  (list :worktree (concat "/tmp/" name) :repo-dir "/tmp/repo" :slug "o/r"
        :number 7 :branch name :state "merged" :dirty dirty))

(ert-deftest magit-dash-worktree-clean/dirty-steps-are-preskipped ()
  (magit-dash-worktree-clean-test--with-store
    (let* ((plan (hitl-plan-create
                  :domain 'magit-dash-worktree-clean
                  :targets (list (magit-dash-worktree-clean-test--target "clean" nil)
                                 (magit-dash-worktree-clean-test--target "dirty" t))
                  :ops '(delete-worktree)))
           (statuses (seq-map #'hitl-plan-step-status (hitl-plan-steps plan))))
      (should (equal '(pending skipped) statuses)))))

(ert-deftest magit-dash-worktree-clean/execute-deletes-only-pending ()
  (magit-dash-worktree-clean-test--with-store
    (let ((deleted nil))
      (cl-letf (((symbol-function 'magit-dash-worktree-uncommitted-changes-p) #'ignore)
                ((symbol-function 'magit-dash-pr-delete-worktree)
                 (lambda (path &optional _repo) (push path deleted) t)))
        (let ((plan (hitl-plan-create
                     :domain 'magit-dash-worktree-clean
                     :targets (list (magit-dash-worktree-clean-test--target "clean" nil)
                                    (magit-dash-worktree-clean-test--target "dirty" t))
                     :ops '(delete-worktree))))
          (hitl-plan-execute plan)
          (should (equal '("/tmp/clean") deleted))
          (should (equal '(done skipped)
                         (seq-map #'hitl-plan-step-status (hitl-plan-steps plan)))))))))

(ert-deftest magit-dash-worktree-clean/apply-refuses-newly-dirty-worktree ()
  (magit-dash-worktree-clean-test--with-store
    (cl-letf (((symbol-function 'magit-dash-worktree-uncommitted-changes-p) (lambda (_) t))
              ((symbol-function 'magit-dash-pr-delete-worktree)
               (lambda (&rest _) (error "must not delete"))))
      (let ((plan (hitl-plan-create
                   :domain 'magit-dash-worktree-clean
                   :targets (list (magit-dash-worktree-clean-test--target "late" nil))
                   :ops '(delete-worktree))))
        (hitl-plan-execute plan)
        (should (eq 'failed (hitl-plan-step-status (car (hitl-plan-steps plan)))))))))

(ert-deftest magit-dash-worktree-clean/empty-targets-make-no-plan ()
  (should-not (magit-dash-worktree-clean-plan-targets nil)))

(defun magit-dash-worktree-clean-test--match (name branch pr)
  "Match a worktree called NAME on BRANCH against merged PR, fetching PR."
  (let* ((dir (make-temp-file "mdwc-wt-" t))
         (path (expand-file-name name dir)))
    (make-directory path)
    (unwind-protect
        (cl-letf (((symbol-function 'magit-dash-pr--fetch-prs-for-branch) #'ignore)
                  ((symbol-function 'magit-dash-pr--fetch-pr) (lambda (&rest _) pr)))
          (magit-dash-worktree-clean--match
           `((path . ,path) (branch . ,branch)) nil "o/r" dir))
      (delete-directory dir t))))

(ert-deftest magit-dash-worktree-clean/number-only-match-needs-corroboration ()
  (let ((pr '((number . 5) (headRefName . "feat") (state . "MERGED"))))
    (should-not (magit-dash-worktree-clean-test--match "cr-ticket-5" "my-work" pr))
    (should-not (magit-dash-worktree-clean-test--match "xcr-5" "my-work" pr))
    (should (magit-dash-worktree-clean-test--match "cr-5" "my-work" pr))
    (should (magit-dash-worktree-clean-test--match "cr-ticket-5" "wt/feat" pr))))

(provide 'test-magit-dash-worktree-clean)
;;; test-magit-dash-worktree-clean.el ends here
