;;; test-magit-dash-gh.el --- ERT tests for magit-dash-gh -*- lexical-binding: t; no-byte-compile: t; -*-

;; Run inside a live Emacs session with the full config loaded:
;;   M-x ert RET t RET
;; or filtered:
;;   (ert "^magit-dash-gh/")
;;
;; Batch run:
;;   emacs --batch -l test/test-helper.el \
;;     -l test/test-magit-dash-gh.el \
;;     --eval '(ert-run-tests-batch-and-exit "magit-dash-gh/")'

(require 'ert)
(require 'cl-lib)
(require 'magit-dash)
(require 'magit-dash-gh)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-dash-gh--pr-closed-p (pure)

(ert-deftest magit-dash-gh/pr-closed-p-merged ()
  (should (magit-dash-gh--pr-closed-p '((state . "MERGED")))))

(ert-deftest magit-dash-gh/pr-closed-p-closed ()
  (should (magit-dash-gh--pr-closed-p '((state . "CLOSED")))))

(ert-deftest magit-dash-gh/pr-closed-p-open ()
  (should-not (magit-dash-gh--pr-closed-p '((state . "OPEN")))))

(ert-deftest magit-dash-gh/pr-closed-p-draft ()
  (should-not (magit-dash-gh--pr-closed-p '((state . "DRAFT")))))

(ert-deftest magit-dash-gh/pr-closed-p-missing-state ()
  (should-not (magit-dash-gh--pr-closed-p '((number . 42)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-dash-gh--prune-format-annotation (pure)

(ert-deftest magit-dash-gh/format-annotation ()
  (let ((pr '((number . 42) (state . "MERGED") (title . "Add feature X"))))
    (should (equal "PR #42 MERGED: Add feature X"
                   (magit-dash-gh--prune-format-annotation pr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-dash-gh--prune-build-menu (pure)

(ert-deftest magit-dash-gh/build-menu-empty ()
  "With no candidates, menu has only exit + refresh."
  (let ((table (magit-dash-gh--prune-build-menu nil nil)))
    (should (map-contains-key table "exit menu"))
    (should (map-contains-key table "refresh"))
    (should-not (map-contains-key table "prune all branches (no prompt)"))
    (should-not (map-contains-key table "prune all branches (with prompt)"))
    (should-not (map-contains-key table "mark branch for pruning"))
    (should-not (map-contains-key table "prune marked branches"))
    (should (= 2 (hash-table-count table)))))

(ert-deftest magit-dash-gh/build-menu-with-candidates ()
  "With candidates, bulk options appear plus one entry per branch."
  (let* ((candidates '(("a" . ((number . 1) (state . "MERGED") (title . "A")))
                       ("b" . ((number . 2) (state . "CLOSED") (title . "B")))))
         (table (magit-dash-gh--prune-build-menu candidates nil)))
    (should (map-contains-key table "exit menu"))
    (should (map-contains-key table "refresh"))
    (should (map-contains-key table "prune all branches (no prompt)"))
    (should (map-contains-key table "prune all branches (with prompt)"))
    (should (map-contains-key table "mark branch for pruning"))
    (should (map-contains-key table "prune: a"))
    (should (map-contains-key table "prune: b"))
    (should-not (map-contains-key table "prune marked branches"))
    (should-not (map-contains-key table "prune: a [marked]"))))

(ert-deftest magit-dash-gh/build-menu-with-marked ()
  "Marked branches show [marked] suffix and surface the prune-marked entry."
  (let* ((candidates '(("a" . ((number . 1) (state . "MERGED") (title . "A")))
                       ("b" . ((number . 2) (state . "CLOSED") (title . "B")))))
         (table (magit-dash-gh--prune-build-menu candidates '("a"))))
    (should (map-contains-key table "prune marked branches"))
    (should (map-contains-key table "prune: a [marked]"))
    (should (map-contains-key table "prune: b"))
    (should-not (map-contains-key table "prune: a"))))

(ert-deftest magit-dash-gh/build-menu-annotations-have-pr-info ()
  (let* ((candidates '(("feat" . ((number . 99) (state . "MERGED") (title . "Big change")))))
         (table (magit-dash-gh--prune-build-menu candidates nil)))
    (should (equal "PR #99 MERGED: Big change"
                   (car (map-elt table "prune: feat"))))))

(ert-deftest magit-dash-gh/build-menu-branch-entry-carries-branch-as-target ()
  "Each `prune: BRANCH' entry carries BRANCH itself as its ACR target."
  (let* ((candidates '(("feat" . ((number . 99) (state . "MERGED") (title . "Big change")))))
         (table (magit-dash-gh--prune-build-menu candidates nil)))
    (should (equal "feat" (cdr (map-elt table "prune: feat"))))))

(ert-deftest magit-dash-gh/build-menu-marked-branch-entry-target-has-no-suffix ()
  "The target for a marked branch entry is the bare branch name, not `BRANCH [marked]'."
  (let* ((candidates '(("feat" . ((number . 99) (state . "MERGED") (title . "Big change")))))
         (table (magit-dash-gh--prune-build-menu candidates '("feat"))))
    (should (equal "feat" (cdr (map-elt table "prune: feat [marked]"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-dash-gh--prune-scan

(defun magit-dash-gh-extras-test--make-pr-table (alist)
  "Build a hash table of branch→pr-alist from ALIST for use in scan mocks."
  (let ((table (make-hash-table :test #'equal)))
    (seq-do (lambda (pair) (puthash (car pair) (cdr pair) table)) alist)
    table))

(ert-deftest magit-dash-gh/scan-collects-closed-prs ()
  "scan keeps only branches whose PR is merged or closed."
  (let ((prs (magit-dash-gh-extras-test--make-pr-table
              '(("a" . ((number . 1) (state . "MERGED")))
                ("c" . ((number . 3) (state . "CLOSED"))))))
        (magit-dash-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-dash-gh--branch-verified) (lambda (&rest _) 'merged))
              ((symbol-function 'magit-dash-gh--commit-verified) (lambda (&rest _) 'merged))
              ((symbol-function 'magit-dash-gh--repo-dir) (lambda () "/tmp/r"))
              ((symbol-function 'magit-dash-gh--default-branch) (lambda () "main"))
              ((symbol-function 'magit-get-current-branch) (lambda () "current"))
              ((symbol-function 'magit-dash-gh--fetch-closed-prs) (lambda (&optional _) prs))
              ((symbol-function 'magit-list-local-branch-names) (lambda () '("a" "b" "c" "d"))))
      (let ((result (magit-dash-gh--prune-scan)))
        (should (equal '("a" "c") (seq-map #'car result)))
        (should (equal '("a" "c")
                       (seq-map #'car (plist-get (magit-dash-gh--cache-get "/tmp/r" :prune-state) :candidates))))))))

(ert-deftest magit-dash-gh/scan-drops-stale-marked ()
  "Marked branches no longer in candidate set are dropped."
  (let ((prs (magit-dash-gh-extras-test--make-pr-table
              '(("a" . ((number . 1) (state . "MERGED")))
                ("c" . ((number . 3) (state . "CLOSED"))))))
        (magit-dash-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-dash-gh--branch-verified) (lambda (&rest _) 'merged))
              ((symbol-function 'magit-dash-gh--commit-verified) (lambda (&rest _) 'merged))
              ((symbol-function 'magit-dash-gh--repo-dir) (lambda () "/tmp/r"))
              ((symbol-function 'magit-dash-gh--default-branch) (lambda () "main"))
              ((symbol-function 'magit-get-current-branch) (lambda () "current"))
              ((symbol-function 'magit-dash-gh--fetch-closed-prs) (lambda (&optional _) prs))
              ((symbol-function 'magit-list-local-branch-names) (lambda () '("a" "b" "c"))))
      ;; 'gone' is stale (not a branch); 'b' has no closed PR (not a candidate)
      (magit-dash-gh--cache-set "/tmp/r" :prune-state
                          (list :candidates nil :marked '("a" "gone" "b")))
      (magit-dash-gh--prune-scan)
      (should (equal '("a") (plist-get (magit-dash-gh--cache-get "/tmp/r" :prune-state) :marked))))))

(ert-deftest magit-dash-gh/scan-empty ()
  "scan with no closed PRs yields empty candidates."
  (let ((prs (make-hash-table :test #'equal))
        (magit-dash-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-dash-gh--repo-dir) (lambda () "/tmp/r"))
              ((symbol-function 'magit-dash-gh--default-branch) (lambda () "main"))
              ((symbol-function 'magit-get-current-branch) (lambda () "current"))
              ((symbol-function 'magit-dash-gh--fetch-closed-prs) (lambda (&optional _) prs))
              ((symbol-function 'magit-list-local-branch-names) (lambda () '("a"))))
      (let ((result (magit-dash-gh--prune-scan)))
        (should-not result)
        (let ((state (magit-dash-gh--cache-get "/tmp/r" :prune-state)))
          (should-not (plist-get state :candidates))
          (should-not (plist-get state :marked)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-dash-gh--prune-delete-branches

(ert-deftest magit-dash-gh/delete-no-prompt ()
  "With PROMPT-P nil, all branches are deleted without read-char-choice."
  (let ((deleted nil)
        (magit-dash-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-dash-gh--branch-verified) (lambda (&rest _) 'merged))
              ((symbol-function 'magit-branch-delete)
               (lambda (branches &optional _)
                 (setq deleted (append deleted branches))))
              ((symbol-function 'read-char-choice)
               (lambda (&rest _) (error "should not be called")))
              ((symbol-function 'magit-dash-gh--prune-scan)
               (lambda () nil)))
      (let ((result (magit-dash-gh--prune-delete-branches '("a" "b") "/tmp/r" nil)))
        (should (equal '("a" "b") deleted))
        (should (= 2 (plist-get result :deleted)))
        (should (= 0 (plist-get result :skipped)))
        (should-not (plist-get result :quit))))))

(ert-deftest magit-dash-gh/delete-with-prompt-yes ()
  (let ((deleted nil)
        (magit-dash-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-dash-gh--branch-verified) (lambda (&rest _) 'merged))
              ((symbol-function 'magit-branch-delete)
               (lambda (branches &optional _)
                 (setq deleted (append deleted branches))))
              ((symbol-function 'read-char-choice)
               (lambda (&rest _) ?y))
              ((symbol-function 'magit-dash-gh--prune-scan)
               (lambda () nil)))
      (let ((result (magit-dash-gh--prune-delete-branches '("a" "b") "/tmp/r" t)))
        (should (equal '("a" "b") deleted))
        (should (= 2 (plist-get result :deleted)))))))

(ert-deftest magit-dash-gh/delete-with-prompt-no-skips ()
  "All ?n answers leave the branches untouched."
  (let ((deleted nil)
        (magit-dash-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-dash-gh--branch-verified) (lambda (&rest _) 'merged))
              ((symbol-function 'magit-branch-delete)
               (lambda (branches &optional _)
                 (setq deleted (append deleted branches))))
              ((symbol-function 'read-char-choice)
               (lambda (&rest _) ?n))
              ((symbol-function 'magit-dash-gh--prune-scan)
               (lambda () nil)))
      (let ((result (magit-dash-gh--prune-delete-branches '("a" "b") "/tmp/r" t)))
        (should-not deleted)
        (should (= 0 (plist-get result :deleted)))
        (should (= 2 (plist-get result :skipped)))))))

(ert-deftest magit-dash-gh/delete-with-prompt-quit ()
  "Quit terminates the loop after the current branch."
  (let ((deleted nil)
        (calls 0)
        (magit-dash-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-dash-gh--branch-verified) (lambda (&rest _) 'merged))
              ((symbol-function 'magit-branch-delete)
               (lambda (branches &optional _)
                 (setq deleted (append deleted branches))))
              ((symbol-function 'read-char-choice)
               (lambda (&rest _) (setq calls (1+ calls)) (if (= calls 1) ?y ?q)))
              ((symbol-function 'magit-dash-gh--prune-scan)
               (lambda () nil)))
      (let ((result (magit-dash-gh--prune-delete-branches '("a" "b" "c") "/tmp/r" t)))
        (should (equal '("a") deleted))
        (should (plist-get result :quit))
        (should (= 1 (plist-get result :deleted)))))))

(ert-deftest magit-dash-gh/delete-yes-to-all ()
  "Bang answer enables yes-to-all for the remainder."
  (let ((deleted nil)
        (calls 0)
        (magit-dash-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-dash-gh--branch-verified) (lambda (&rest _) 'merged))
              ((symbol-function 'magit-branch-delete)
               (lambda (branches &optional _)
                 (setq deleted (append deleted branches))))
              ((symbol-function 'read-char-choice)
               (lambda (&rest _) (setq calls (1+ calls)) ?!))
              ((symbol-function 'magit-dash-gh--prune-scan)
               (lambda () nil)))
      (let ((result (magit-dash-gh--prune-delete-branches '("a" "b" "c") "/tmp/r" t)))
        (should (equal '("a" "b" "c") deleted))
        (should (= 3 (plist-get result :deleted)))
        (should (= 1 calls))))))

(ert-deftest magit-dash-gh/delete-rescans-after ()
  "A re-scan is performed for PATH after deletion."
  (let ((scanned 0)
        (scanned-in-path nil)
        (magit-dash-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-dash-gh--branch-verified) (lambda (&rest _) 'merged))
              ((symbol-function 'magit-branch-delete) (lambda (&rest _) nil))
              ((symbol-function 'magit-dash-gh--prune-scan)
               (lambda () (setq scanned (1+ scanned)) (setq scanned-in-path (magit-dash-gh--repo-dir)) nil))
              ((symbol-function 'magit-dash-gh--repo-dir) (lambda () "/tmp/r")))
      (magit-dash-gh--prune-delete-branches '("a") "/tmp/r" nil)
      (should (= 1 scanned))
      (should (equal "/tmp/r" scanned-in-path)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-dash-gh--prune-toggle-mark

(ert-deftest magit-dash-gh/toggle-mark-adds ()
  "Selecting an unmarked branch adds it to :marked."
  (let ((magit-dash-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'annotated-completing-read)
               (lambda (&rest _) "feat")))
      (magit-dash-gh--cache-set "/tmp/r" :prune-state
                          (list :candidates '(("feat" . ((number . 1) (state . "MERGED") (title . "F"))))
                                :marked nil))
      (magit-dash-gh--prune-toggle-mark "/tmp/r")
      (should (equal '("feat") (plist-get (magit-dash-gh--cache-get "/tmp/r" :prune-state) :marked))))))

(ert-deftest magit-dash-gh/toggle-mark-removes ()
  "Selecting a marked branch removes it from :marked."
  (let ((magit-dash-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'annotated-completing-read)
               (lambda (&rest _) "feat")))
      (magit-dash-gh--cache-set "/tmp/r" :prune-state
                          (list :candidates '(("feat" . ((number . 1) (state . "MERGED") (title . "F"))))
                                :marked '("feat")))
      (magit-dash-gh--prune-toggle-mark "/tmp/r")
      (should-not (plist-get (magit-dash-gh--cache-get "/tmp/r" :prune-state) :marked)))))

(ert-deftest magit-dash-gh/toggle-mark-preserves-other-marks ()
  "Toggling one branch leaves other marked branches in place."
  (let ((magit-dash-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'annotated-completing-read)
               (lambda (&rest _) "b")))
      (magit-dash-gh--cache-set "/tmp/r" :prune-state
                          (list :candidates '(("a" . ((number . 1) (state . "MERGED") (title . "A")))
                                              ("b" . ((number . 2) (state . "CLOSED") (title . "B"))))
                                :marked '("a")))
      (magit-dash-gh--prune-toggle-mark "/tmp/r")
      (let ((marked (plist-get (magit-dash-gh--cache-get "/tmp/r" :prune-state) :marked)))
        (should (member "a" marked))
        (should (member "b" marked))))))

(ert-deftest magit-dash-gh/toggle-mark-errors-when-empty ()
  (let ((magit-dash-gh--cache (make-hash-table :test #'equal)))
    (magit-dash-gh--cache-set "/tmp/r" :prune-state (list :candidates nil :marked nil))
    (should-error (magit-dash-gh--prune-toggle-mark "/tmp/r")
                  :type 'user-error)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-dash-gh--prune-dispatch

(ert-deftest magit-dash-gh/dispatch-exit-throws ()
  "exit menu throws magit-dash-gh--prune-exit so the loop terminates."
  (let ((magit-dash-gh--cache (make-hash-table :test #'equal)))
    (magit-dash-gh--cache-set "/tmp/r" :prune-state (list :candidates nil :marked nil))
    (let ((after-throw 'untouched))
      (catch 'magit-dash-gh--prune-exit
        (magit-dash-gh--prune-dispatch "exit menu" "/tmp/r")
        (setq after-throw 'reached))
      (should (eq 'untouched after-throw)))))

(ert-deftest magit-dash-gh/dispatch-refresh-calls-scan ()
  (let ((scanned 0)
        (magit-dash-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-dash-gh--prune-scan)
               (lambda () (setq scanned (1+ scanned)) nil)))
      (magit-dash-gh--cache-set "/tmp/r" :prune-state (list :candidates nil :marked nil))
      (magit-dash-gh--prune-dispatch "refresh" "/tmp/r")
      (should (= 1 scanned)))))

(ert-deftest magit-dash-gh/dispatch-prune-selected ()
  "A branch name resolved via ACR's target support deletes that branch only."
  (let ((deleted nil)
        (magit-dash-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-dash-gh--branch-verified) (lambda (&rest _) 'merged))
              ((symbol-function 'magit-branch-delete)
               (lambda (branches &optional _)
                 (setq deleted (append deleted branches))))
              ((symbol-function 'magit-dash-gh--prune-scan)
               (lambda () nil)))
      (magit-dash-gh--cache-set "/tmp/r" :prune-state
                          (list :candidates '(("feat" . ((number . 1) (state . "MERGED") (title . "F")))
                                              ("other" . ((number . 2) (state . "CLOSED") (title . "O"))))
                                :marked nil))
      (magit-dash-gh--prune-dispatch "feat" "/tmp/r")
      (should (equal '("feat") deleted)))))

(ert-deftest magit-dash-gh/dispatch-prune-selected-marked ()
  "A marked branch's target is still the bare branch name, so dispatch
deletes it the same as an unmarked selection."
  (let ((deleted nil)
        (magit-dash-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-dash-gh--branch-verified) (lambda (&rest _) 'merged))
              ((symbol-function 'magit-branch-delete)
               (lambda (branches &optional _)
                 (setq deleted (append deleted branches))))
              ((symbol-function 'magit-dash-gh--prune-scan)
               (lambda () nil)))
      (magit-dash-gh--cache-set "/tmp/r" :prune-state
                          (list :candidates '(("feat" . ((number . 1) (state . "MERGED") (title . "F"))))
                                :marked '("feat")))
      (magit-dash-gh--prune-dispatch "feat" "/tmp/r")
      (should (equal '("feat") deleted)))))

(ert-deftest magit-dash-gh/dispatch-prune-all-no-prompt ()
  (let ((deleted nil)
        (read-calls 0)
        (magit-dash-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-dash-gh--branch-verified) (lambda (&rest _) 'merged))
              ((symbol-function 'magit-branch-delete)
               (lambda (branches &optional _)
                 (setq deleted (append deleted branches))))
              ((symbol-function 'read-char-choice)
               (lambda (&rest _) (cl-incf read-calls) ?y))
              ((symbol-function 'magit-dash-gh--prune-scan)
               (lambda () nil)))
      (magit-dash-gh--cache-set "/tmp/r" :prune-state
                          (list :candidates '(("a" . ((number . 1) (state . "MERGED") (title . "A")))
                                              ("b" . ((number . 2) (state . "CLOSED") (title . "B"))))
                                :marked nil))
      (magit-dash-gh--prune-dispatch "prune all branches (no prompt)" "/tmp/r")
      (should (equal '("a" "b") deleted))
      (should (= 0 read-calls)))))

(ert-deftest magit-dash-gh/dispatch-prune-all-with-prompt ()
  (let ((deleted nil)
        (read-calls 0)
        (magit-dash-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-dash-gh--branch-verified) (lambda (&rest _) 'merged))
              ((symbol-function 'magit-branch-delete)
               (lambda (branches &optional _)
                 (setq deleted (append deleted branches))))
              ((symbol-function 'read-char-choice)
               (lambda (&rest _) (cl-incf read-calls) ?y))
              ((symbol-function 'magit-dash-gh--prune-scan)
               (lambda () nil)))
      (magit-dash-gh--cache-set "/tmp/r" :prune-state
                          (list :candidates '(("a" . ((number . 1) (state . "MERGED") (title . "A")))
                                              ("b" . ((number . 2) (state . "CLOSED") (title . "B"))))
                                :marked nil))
      (magit-dash-gh--prune-dispatch "prune all branches (with prompt)" "/tmp/r")
      (should (equal '("a" "b") deleted))
      (should (= 2 read-calls)))))

(ert-deftest magit-dash-gh/dispatch-prune-marked ()
  "prune marked branches deletes only the marked subset."
  (let ((deleted nil)
        (magit-dash-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-dash-gh--branch-verified) (lambda (&rest _) 'merged))
              ((symbol-function 'magit-branch-delete)
               (lambda (branches &optional _)
                 (setq deleted (append deleted branches))))
              ((symbol-function 'magit-dash-gh--prune-scan)
               (lambda () nil)))
      (magit-dash-gh--cache-set "/tmp/r" :prune-state
                          (list :candidates '(("a" . ((number . 1) (state . "MERGED") (title . "A")))
                                              ("b" . ((number . 2) (state . "CLOSED") (title . "B"))))
                                :marked '("b")))
      (magit-dash-gh--prune-dispatch "prune marked branches" "/tmp/r")
      (should (equal '("b") deleted)))))

(ert-deftest magit-dash-gh/dispatch-mark-delegates ()
  "mark branch for pruning delegates to toggle-mark."
  (let ((called nil)
        (magit-dash-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-dash-gh--prune-toggle-mark)
               (lambda (path) (setq called path))))
      (magit-dash-gh--cache-set "/tmp/r" :prune-state
                          (list :candidates '(("a" . ((number . 1) (state . "MERGED") (title . "A"))))
                                :marked nil))
      (magit-dash-gh--prune-dispatch "mark branch for pruning" "/tmp/r")
      (should (equal "/tmp/r" called)))))

(ert-deftest magit-dash-gh/dispatch-unknown-errors ()
  (let ((magit-dash-gh--cache (make-hash-table :test #'equal)))
    (magit-dash-gh--cache-set "/tmp/r" :prune-state (list :candidates nil :marked nil))
    (should-error (magit-dash-gh--prune-dispatch "bogus" "/tmp/r")
                  :type 'user-error)))

(ert-deftest magit-dash-gh/dispatch-errors-when-label-not-a-real-candidate ()
  "A label that is not a fixed action and not among :candidates errors,
even when other real candidates exist."
  (let ((magit-dash-gh--cache (make-hash-table :test #'equal)))
    (magit-dash-gh--cache-set "/tmp/r" :prune-state
                        (list :candidates '(("feat" . ((number . 1) (state . "MERGED") (title . "F"))))
                              :marked nil))
    (should-error (magit-dash-gh--prune-dispatch "not-a-real-branch" "/tmp/r")
                  :type 'user-error)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-dash-gh--cache-get, magit-dash-gh--cache-set, magit-dash-gh--cache-remove

(ert-deftest magit-dash-gh/cache-set-and-get ()
  "set stores a value; get retrieves it."
  (let ((magit-dash-gh--cache (make-hash-table :test #'equal)))
    (magit-dash-gh--cache-set "/tmp/r" :foo "bar")
    (should (equal "bar" (magit-dash-gh--cache-get "/tmp/r" :foo)))))

(ert-deftest magit-dash-gh/cache-get-missing-key ()
  "get returns nil for a key that was never set."
  (let ((magit-dash-gh--cache (make-hash-table :test #'equal)))
    (magit-dash-gh--cache-set "/tmp/r" :x 1)
    (should (null (magit-dash-gh--cache-get "/tmp/r" :y)))))

(ert-deftest magit-dash-gh/cache-remove-key ()
  "remove with a key deletes only that key."
  (let ((magit-dash-gh--cache (make-hash-table :test #'equal)))
    (magit-dash-gh--cache-set "/tmp/r" :a 1)
    (magit-dash-gh--cache-set "/tmp/r" :b 2)
    (magit-dash-gh--cache-remove "/tmp/r" :a)
    (should (null (magit-dash-gh--cache-get "/tmp/r" :a)))
    (should (= 2 (magit-dash-gh--cache-get "/tmp/r" :b)))))

(ert-deftest magit-dash-gh/cache-remove-all ()
  "remove without a key deletes all data for the repo."
  (let ((magit-dash-gh--cache (make-hash-table :test #'equal)))
    (magit-dash-gh--cache-set "/tmp/r" :x 99)
    (magit-dash-gh--cache-remove "/tmp/r")
    (should (null (magit-dash-gh--cache-get "/tmp/r" :x)))))

(ert-deftest magit-dash-gh/cache-isolated-by-path ()
  "Different repo paths have independent cache entries."
  (let ((magit-dash-gh--cache (make-hash-table :test #'equal)))
    (magit-dash-gh--cache-set "/tmp/a" :k "a-value")
    (magit-dash-gh--cache-set "/tmp/b" :k "b-value")
    (should (equal "a-value" (magit-dash-gh--cache-get "/tmp/a" :k)))
    (should (equal "b-value" (magit-dash-gh--cache-get "/tmp/b" :k)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-dash-gh--add-file

(ert-deftest magit-dash-gh/add-file-empty ()
  "Adding to empty :files list produces a single-entry list."
  (let* ((ctx (list :files nil))
         (ctx2 (magit-dash-gh--add-file ctx "pr-info.json" "metadata")))
    (should (equal '((:path "pr-info.json" :type "metadata"))
                   (plist-get ctx2 :files)))))

(ert-deftest magit-dash-gh/add-file-accumulates ()
  "Each add-file appends; order is preserved."
  (let* ((ctx (list :files nil))
         (ctx2 (magit-dash-gh--add-file ctx "a.json" "x"))
         (ctx3 (magit-dash-gh--add-file ctx2 "b.json" "y")))
    (should (= 2 (length (plist-get ctx3 :files))))
    (should (equal "a.json" (plist-get (nth 0 (plist-get ctx3 :files)) :path)))
    (should (equal "b.json" (plist-get (nth 1 (plist-get ctx3 :files)) :path)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-dash-gh--branch-slug

(ert-deftest magit-dash-gh/branch-slug-simple ()
  (should (equal "main" (magit-dash-gh--branch-slug "main"))))

(ert-deftest magit-dash-gh/branch-slug-slash ()
  (should (equal "fix-the-thing" (magit-dash-gh--branch-slug "fix/the-thing"))))

(ert-deftest magit-dash-gh/branch-slug-uppercase ()
  (should (equal "feature-foo-123" (magit-dash-gh--branch-slug "Feature/Foo-123"))))

(ert-deftest magit-dash-gh/branch-slug-multiple-separators ()
  "Consecutive non-alphanumeric chars collapse to a single hyphen."
  (should (equal "a-b" (magit-dash-gh--branch-slug "a_/_b"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-dash-gh--make-error-handler

(ert-deftest magit-dash-gh/make-error-handler-formats-message ()
  "The returned lambda emits a correctly formatted message."
  (let (msg)
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (let ((handler (magit-dash-gh--make-error-handler "magit-dash-gh-test" "my-step")))
        (funcall handler "oops\n" 1)))
    (should (string-match-p "magit-dash-gh-test" msg))
    (should (string-match-p "my-step" msg))
    (should (string-match-p "exit 1" msg))
    (should (string-match-p "oops" msg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-dash-gh--index-table, magit-dash-gh--file-table, magit-dash-gh--write-index

(ert-deftest magit-dash-gh/index-table-basic ()
  "index-table builds a hash-table from alternating key-value pairs."
  (let ((ht (magit-dash-gh--index-table "type" "ci" "count" 3)))
    (should (equal "ci" (gethash "type" ht)))
    (should (= 3 (gethash "count" ht)))))

(ert-deftest magit-dash-gh/file-table-basic ()
  "file-table builds a two-key hash-table."
  (let ((ht (magit-dash-gh--file-table "run-info.json" "metadata")))
    (should (equal "run-info.json" (gethash "path" ht)))
    (should (equal "metadata" (gethash "type" ht)))))

(ert-deftest magit-dash-gh/write-index-json ()
  "write-index writes valid JSON to index.json inside DIR."
  (let ((dir (make-temp-file "magit-dash-gh-extras-test" t)))
    (unwind-protect
        (let ((data (magit-dash-gh--index-table "type" "test" "n" 7)))
          (magit-dash-gh--write-index dir data)
          (let* ((file (expand-file-name "index.json" dir))
                 (raw (with-temp-buffer
                        (insert-file-contents file)
                        (buffer-string)))
                 (parsed (json-parse-string raw :object-type 'alist)))
            (should (file-exists-p file))
            (should (equal "test" (map-elt parsed 'type)))
            (should (= 7 (map-elt parsed 'n)))))
      (delete-directory dir t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-dash-gh--collect-default-name

(ert-deftest magit-dash-gh/collect-default-name-basic ()
  (should (equal "ci-main-12345"
                 (magit-dash-gh--collect-default-name 'ci "main" 12345))))

(ert-deftest magit-dash-gh/collect-default-name-slugifies ()
  (should (equal "ci-feature-my-thing-99"
                 (magit-dash-gh--collect-default-name 'ci "feature/my-thing" 99))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GitHub Account Selection & Token Management

(ert-deftest magit-dash-gh/account-token-success ()
  "account-token returns trimmed token on gh auth token exit 0."
  (let ((magit-dash-gh--token-cache (make-hash-table :test #'equal))
        (magit-dash-gh--token-pending (make-hash-table :test #'equal)))
    (magit-dash-gh-test--with-fake-gh
     "echo ghp_mock_token_12345"
     (lambda (_dir)
       (should (equal "ghp_mock_token_12345"
                      (magit-dash-gh--account-token "alice" "github.com")))))))

(ert-deftest magit-dash-gh/account-token-failure ()
  "account-token signals user-error on gh auth token exit non-zero."
  (let ((magit-dash-gh--token-cache (make-hash-table :test #'equal))
        (magit-dash-gh--token-pending (make-hash-table :test #'equal)))
    (magit-dash-gh-test--with-fake-gh
     "echo 'no token found for user'; exit 1"
     (lambda (_dir)
       (should-error (magit-dash-gh--account-token "bob" "github.com")
                     :type 'user-error)))))

(ert-deftest magit-dash-gh/with-account-nil ()
  "magit-dash-gh-with-account nil executes body without altering environment."
  (let ((token-before (getenv "GH_TOKEN")))
    (should (equal "result"
                   (magit-dash-gh-with-account nil
                     (should (equal token-before (getenv "GH_TOKEN")))
                     "result")))))

(ert-deftest magit-dash-gh/legacy-account-macro ()
  "The old account macro still expands and binds the account environment."
  (cl-letf (((symbol-function 'magit-dash-gh--account-token)
             (lambda (_user _host) "legacy-token")))
    (should (equal "legacy-token"
                   (with-magit-gh-account '(:user "alice" :host "github.com")
                     (getenv "GH_TOKEN"))))))

(ert-deftest magit-dash-gh/with-account-github-com ()
  "magit-dash-gh-with-account binds GH_TOKEN for github.com."
  (cl-letf (((symbol-function 'magit-dash-gh--account-token)
             (lambda (user host)
               (format "token-for-%s-on-%s" user host))))
    (magit-dash-gh-with-account '(:user "alice" :host "github.com")
      (should (equal "token-for-alice-on-github.com" (getenv "GH_TOKEN")))
      (should-not (getenv "GH_ENTERPRISE_TOKEN")))))

(ert-deftest magit-dash-gh/with-account-enterprise ()
  "magit-dash-gh-with-account binds GH_ENTERPRISE_TOKEN and GH_HOST for enterprise hosts."
  (cl-letf (((symbol-function 'magit-dash-gh--account-token)
             (lambda (user host)
               (format "ent-token-for-%s-on-%s" user host))))
    (magit-dash-gh-with-account '(:user "corp-dev" :host "ghe.myorg.internal")
      (should (equal "ent-token-for-corp-dev-on-ghe.myorg.internal" (getenv "GH_ENTERPRISE_TOKEN")))
      (should (equal "ghe.myorg.internal" (getenv "GH_HOST"))))))

(ert-deftest magit-dash-gh/with-account-string ()
  "magit-dash-gh-with-account supports plain username string defaulting to github.com."
  (cl-letf (((symbol-function 'magit-dash-gh--account-token)
             (lambda (user host)
               (format "token-%s-%s" user host))))
    (magit-dash-gh-with-account "dev1"
      (should (equal "token-dev1-github.com" (getenv "GH_TOKEN"))))))

(ert-deftest magit-dash-gh/repo-account-struct ()
  "repo-account extracts account and host from a magit-dash-repo struct."
  (let ((r-account (magit-dash-repo--make :name "r1" :path "/tmp/r1"
                                         :gh-account "octo" :gh-host "github.corp.com"))
        (r-no-account (magit-dash-repo--make :name "r2" :path "/tmp/r2")))
    (should (equal '(:user "octo" :host "github.corp.com")
                   (magit-dash-gh--repo-account r-account)))
    (should (null (magit-dash-gh--repo-account r-no-account)))))

(ert-deftest magit-dash-gh/repo-account-lookup-path-and-name ()
  "repo-account resolves repo via path string or name string in repo list."
  (let* ((r1 (magit-dash-repo--make :name "repo-one" :path "/tmp/repo-one"
                                   :gh-account "user1"))
         (r2 (magit-dash-repo--make :name "repo-two" :path "/tmp/repo-two"))
         (magit-dash-repo-list (list r1 r2)))
    ;; By expanded path
    (should (equal '(:user "user1" :host "github.com")
                   (magit-dash-gh--repo-account "/tmp/repo-one")))
    ;; By repo name
    (should (equal '(:user "user1" :host "github.com")
                   (magit-dash-gh--repo-account "repo-one")))
    ;; Repo without account
    (should (null (magit-dash-gh--repo-account "/tmp/repo-two")))
    ;; Unregistered path
    (should (null (magit-dash-gh--repo-account "/tmp/unregistered")))))

(ert-deftest magit-dash-gh/parse-auth-hosts-json ()
  "parse-auth-hosts-json parses JSON status structure."
  (let* ((json "{\"hosts\":{\"github.com\":[{\"active\":true,\"host\":\"github.com\",\"login\":\"u1\"},{\"active\":false,\"host\":\"github.com\",\"login\":\"u2\"}],\"ghe.internal\":[{\"active\":true,\"host\":\"ghe.internal\",\"login\":\"corp-u1\"}]}}")
         (accounts (magit-dash-gh--parse-auth-hosts-json json)))
    (should (= 3 (length accounts)))
    (should (equal '(:host "github.com" :user "u1" :active t) (nth 0 accounts)))
    (should (equal '(:host "github.com" :user "u2" :active nil) (nth 1 accounts)))
    (should (equal '(:host "ghe.internal" :user "corp-u1" :active t) (nth 2 accounts)))))

(ert-deftest magit-dash-gh/auth-accounts-json-and-fallback ()
  "auth-accounts parses JSON when available and falls back to regex."
  (cl-letf (((symbol-function 'executable-find) (lambda (_) "/usr/bin/gh")))
    ;; 1. JSON success
    (cl-letf (((symbol-function 'call-process)
               (lambda (_cmd _in dest _disp &rest _args)
                 (with-current-buffer (car dest)
                   (insert "{\"hosts\":{\"github.com\":[{\"active\":true,\"host\":\"github.com\",\"login\":\"json-user\"}]}}"))
                 0)))
      (let ((accounts (magit-dash-gh--auth-accounts)))
        (should (= 1 (length accounts)))
        (should (equal "json-user" (plist-get (car accounts) :user)))))
    ;; 2. JSON failure, fallback to regex
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest _) 1))
              ((symbol-function 'shell-command-to-string)
               (lambda (_)
                 "Logged in to github.com account fallback-user (/config)\n  - Active account: true\n")))
      (let ((accounts (magit-dash-gh--auth-accounts)))
        (should (= 1 (length accounts)))
        (should (equal "fallback-user" (plist-get (car accounts) :user)))))))

(ert-deftest magit-dash-gh/validate-accounts ()
  "validate-accounts checks token resolution and reports status alist."
  (let* ((r-good (magit-dash-repo--make :name "good" :path "/tmp/good" :gh-account "good-user"))
         (r-bad (magit-dash-repo--make :name "bad" :path "/tmp/bad" :gh-account "bad-user"))
         (r-none (magit-dash-repo--make :name "none" :path "/tmp/none")))
    (cl-letf (((symbol-function 'magit-dash-gh--account-token)
               (lambda (user &optional _host)
                 (if (string= user "good-user")
                     "token-ok"
                   (user-error "auth failed for %s" user)))))
      (let ((results (magit-dash-gh-validate-accounts (list r-good r-bad r-none))))
        (should (= 2 (length results)))
        (should (eq 'ok (cdr (assoc "good" results))))
        (should (string-match-p "auth failed" (cdr (assoc "bad" results))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Prune menu & Worktree cleanup tests

(ert-deftest magit-dash-gh/build-menu-with-worktrees ()
  "Worktree options appear when merged worktrees are present."
  (let* ((wts '(("/tmp/r/.wt/pr-42" . ("pr-42" . ((number . 42) (state . "MERGED") (title . "Fix"))))))
         (table (magit-dash-gh--prune-build-menu nil nil wts)))
    (should (map-contains-key table "exit menu"))
    (should (map-contains-key table "refresh"))
    (should (map-contains-key table "clean merged worktrees"))
    (should (map-contains-key table "clean worktree: pr-42"))
    (should (equal "/tmp/r/.wt/pr-42" (cdr (map-elt table "clean worktree: pr-42"))))))

(ert-deftest magit-dash-gh/build-menu-with-candidates-and-worktrees ()
  "Combined worktree and branch prune option appears when both exist."
  (let* ((candidates '(("feat" . ((number . 1) (state . "MERGED") (title . "F")))))
         (wts '(("/tmp/r/.wt/feat" . ("feat" . ((number . 1) (state . "MERGED") (title . "F"))))))
         (table (magit-dash-gh--prune-build-menu candidates nil wts)))
    (should (map-contains-key table "clean worktrees & prune all branches"))
    (should (map-contains-key table "clean merged worktrees"))
    (should (map-contains-key table "prune: feat"))))

(ert-deftest magit-dash-gh/scan-finds-git-merged-branches ()
  "scan includes local branches merged into default branch via git."
  (let ((magit-dash-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-dash-gh--repo-dir) (lambda () "/tmp/r"))
              ((symbol-function 'magit-dash-gh--default-branch) (lambda () "main"))
              ((symbol-function 'magit-get-current-branch) (lambda () "main"))
              ((symbol-function 'magit-dash-gh--fetch-closed-prs) (lambda (&optional _) (make-hash-table :test #'equal)))
              ((symbol-function 'magit-dash-gh--git-merged-branches) (lambda (&rest _) '("merged-locally")))
              ((symbol-function 'magit-dash-gh--git-worktrees) (lambda (&rest _) nil))
              ((symbol-function 'magit-list-local-branch-names) (lambda () '("main" "merged-locally" "unmerged"))))
      (let ((result (magit-dash-gh--prune-scan)))
        (should (equal '("merged-locally") (seq-map #'car result)))
        (let ((pr (cdar result)))
          (should (equal "MERGED" (map-elt pr 'state)))
          (should (string-match-p "git" (map-elt pr 'title))))))))

(ert-deftest magit-dash-gh/scan-finds-pr-number-and-wt-branches ()
  "scan matches branches named pr-NUM or wt/name."
  (let* ((prs (magit-dash-gh-extras-test--make-pr-table
               '(("pr-99" . ((number . 99) (state . "MERGED") (title . "Feature 99")))
                 ("wt/feat" . ((number . 100) (state . "CLOSED") (title . "Feature 100"))))))
         (magit-dash-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-dash-gh--branch-verified) (lambda (&rest _) 'merged))
              ((symbol-function 'magit-dash-gh--commit-verified) (lambda (&rest _) 'merged))
              ((symbol-function 'magit-dash-gh--repo-dir) (lambda () "/tmp/r"))
              ((symbol-function 'magit-dash-gh--default-branch) (lambda () "main"))
              ((symbol-function 'magit-get-current-branch) (lambda () "main"))
              ((symbol-function 'magit-dash-gh--fetch-closed-prs) (lambda (&optional _) prs))
              ((symbol-function 'magit-dash-gh--git-merged-branches) (lambda (&rest _) nil))
              ((symbol-function 'magit-dash-gh--git-worktrees) (lambda (&rest _) nil))
              ((symbol-function 'magit-list-local-branch-names) (lambda () '("main" "pr-99" "wt/feat" "other"))))
      (let ((result (magit-dash-gh--prune-scan)))
        (should (equal '("pr-99" "wt/feat") (seq-map #'car result)))))))

(ert-deftest magit-dash-gh/scan-finds-merged-worktrees ()
  "scan collects worktrees for merged PRs or branches into :merged-worktrees."
  (let* ((prs (magit-dash-gh-extras-test--make-pr-table
               '(("feat" . ((number . 5) (state . "MERGED") (title . "Feat 5"))))))
         (wts '(((path . "/tmp/r") (branch . "main") (head . "abc"))
                ((path . "/tmp/r/.wt/feat") (branch . "feat") (head . "def"))))
         (magit-dash-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-dash-gh--branch-verified) (lambda (&rest _) 'merged))
              ((symbol-function 'magit-dash-gh--commit-verified) (lambda (&rest _) 'merged))
              ((symbol-function 'magit-dash-gh--repo-dir) (lambda () "/tmp/r"))
              ((symbol-function 'magit-dash-gh--default-branch) (lambda () "main"))
              ((symbol-function 'magit-get-current-branch) (lambda () "main"))
              ((symbol-function 'magit-dash-gh--fetch-closed-prs) (lambda (&optional _) prs))
              ((symbol-function 'magit-dash-gh--git-merged-branches) (lambda (&rest _) nil))
              ((symbol-function 'magit-dash-gh--git-worktrees) (lambda (&rest _) wts))
              ((symbol-function 'file-directory-p) (lambda (_) t))
              ((symbol-function 'magit-list-local-branch-names) (lambda () '("main" "feat"))))
      (magit-dash-gh--prune-scan)
      (let* ((state (magit-dash-gh--cache-get "/tmp/r" :prune-state))
             (merged-wts (plist-get state :merged-worktrees)))
        (should (= 1 (length merged-wts)))
        (should (equal "/tmp/r/.wt/feat" (caar merged-wts)))))))

(ert-deftest magit-dash-gh/delete-branch-removes-clean-worktree-first ()
  "Deleting a branch with a clean worktree removes worktree before deleting branch."
  (let ((wt-removed nil)
        (branch-deleted nil))
    (cl-letf (((symbol-function 'magit-dash-gh--branch-verified) (lambda (&rest _) 'merged))
              ((symbol-function 'magit-dash-gh--commit-verified) (lambda (&rest _) 'merged))
              ((symbol-function 'magit-dash-gh--branch-worktree)
               (lambda (branch _path) (when (equal branch "feat") "/tmp/r/.wt/feat")))
              ((symbol-function 'magit-dash-worktree-uncommitted-changes-p)
               (lambda (_path) nil))
              ((symbol-function 'call-process)
               (lambda (_cmd _in _dest _disp &rest args)
                 (when (and (member "worktree" args) (member "remove" args))
                   (setq wt-removed t))
                 0))
              ((symbol-function 'magit-branch-delete)
               (lambda (branches &optional _force)
                 (setq branch-deleted (car branches))))
              ((symbol-function 'magit-dash-gh--prune-scan) #'ignore))
      (let ((res (magit-dash-gh--prune-delete-branches '("feat") "/tmp/r" nil)))
        (should (= 1 (plist-get res :deleted)))
        (should wt-removed)
        (should (equal "feat" branch-deleted))))))

(ert-deftest magit-dash-gh/delete-branch-refuses-when-worktree-dirty ()
  "Deleting a branch with a dirty worktree skips deletion."
  (let ((wt-removed nil)
        (branch-deleted nil))
    (cl-letf (((symbol-function 'magit-dash-gh--branch-verified) (lambda (&rest _) 'merged))
              ((symbol-function 'magit-dash-gh--commit-verified) (lambda (&rest _) 'merged))
              ((symbol-function 'magit-dash-gh--branch-worktree)
               (lambda (branch _path) (when (equal branch "feat") "/tmp/r/.wt/feat")))
              ((symbol-function 'magit-dash-worktree-uncommitted-changes-p)
               (lambda (_path) t))
              ((symbol-function 'call-process)
               (lambda (&rest _) (setq wt-removed t) 0))
              ((symbol-function 'magit-branch-delete)
               (lambda (branches &optional _) (setq branch-deleted (car branches))))
              ((symbol-function 'magit-dash-gh--prune-scan) #'ignore))
      (let ((res (magit-dash-gh--prune-delete-branches '("feat") "/tmp/r" nil)))
        (should (= 0 (plist-get res :deleted)))
        (should (= 1 (plist-get res :skipped)))
        (should-not wt-removed)
        (should-not branch-deleted)))))

(ert-deftest magit-dash-gh/dispatch-clean-merged-worktrees ()
  "Dispatch clean merged worktrees delegates to magit-dash-pr-clean-merged-worktrees."
  (let ((called nil)
        (scanned nil))
    (cl-letf (((symbol-function 'magit-dash-pr-clean-merged-worktrees)
               (lambda (path prompt)
                 (setq called (list path prompt))))
              ((symbol-function 'magit-dash-gh--prune-scan)
               (lambda () (setq scanned t))))
      (magit-dash-gh--prune-dispatch "clean merged worktrees" "/tmp/r")
      (should (equal '("/tmp/r" t) called))
      (should scanned))))

(ert-deftest magit-dash-gh/dispatch-clean-single-worktree ()
  "Dispatching a worktree path target deletes that worktree."
  (let ((called-with nil)
        (scanned nil)
        (magit-dash-gh--cache (make-hash-table :test #'equal)))
    (magit-dash-gh--cache-set "/tmp/r" :prune-state
                              (list :merged-worktrees '(("/tmp/r/.wt/pr-42" . ("pr-42" . nil)))))
    (cl-letf (((symbol-function 'magit-dash-pr-delete-worktree)
               (lambda (path repo) (setq called-with (list path repo))))
              ((symbol-function 'magit-dash-gh--prune-scan)
               (lambda () (setq scanned t))))
      (magit-dash-gh--prune-dispatch "/tmp/r/.wt/pr-42" "/tmp/r")
      (should (equal '("/tmp/r/.wt/pr-42" "/tmp/r") called-with))
      (should scanned))))

(ert-deftest magit-dash-gh/dispatch-clean-worktrees-and-prune-all ()
  "Dispatching clean worktrees & prune all branches cleans worktrees then prunes branches."
  (let ((wts-cleaned nil)
        (branches-pruned nil)
        (magit-dash-gh--cache (make-hash-table :test #'equal)))
    (magit-dash-gh--cache-set "/tmp/r" :prune-state
                              (list :candidates '(("a" . nil) ("b" . nil))))
    (cl-letf (((symbol-function 'magit-dash-pr-clean-merged-worktrees)
               (lambda (path prompt) (setq wts-cleaned (list path prompt))))
              ((symbol-function 'magit-dash-gh--prune-delete-branches)
               (lambda (branches path prompt) (setq branches-pruned (list branches path prompt))))
              ((symbol-function 'magit-dash-gh--prune-scan) #'ignore))
      (magit-dash-gh--prune-dispatch "clean worktrees & prune all branches" "/tmp/r")
      (should (equal '("/tmp/r" t) wts-cleaned))
      (should (equal '(("a" "b") "/tmp/r" t) branches-pruned)))))

;;;; run-process hardening

(defun magit-dash-gh-test--with-fake-gh (script fn)
  "Call FN with a directory whose `gh' is the shell SCRIPT, PATH set to it."
  (let ((dir (make-temp-file "fake-gh" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "gh" dir)
            (insert "#!/bin/sh\n" script "\n"))
          (set-file-modes (expand-file-name "gh" dir) #o755)
          (let ((exec-path (cons dir exec-path))
                (process-environment (cons (concat "PATH=" dir ":" (getenv "PATH"))
                                           process-environment)))
            (cl-letf (((symbol-function 'magit-dash-gh--repo-account) #'ignore))
              (funcall fn dir))))
      (delete-directory dir t))))

(defun magit-dash-gh-test--wait-until (pred &optional timeout)
  "Run the event loop until PRED is non-nil or TIMEOUT (default 10) seconds pass."
  (let ((deadline (+ (float-time) (or timeout 10))))
    (while (and (not (funcall pred)) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (funcall pred)))

(defun magit-dash-gh-test--new-live-procs (before)
  "Return live processes named magit-gh that were not in the list BEFORE."
  (seq-filter (lambda (p)
                (and (not (memq p before))
                     (equal (process-name p) "magit-gh")
                     (process-live-p p)))
              (process-list)))

(defun magit-dash-gh-test--run (dir)
  "Run the fake gh in DIR; return (:ok OUT) or (:err OUT CODE)."
  (let (result)
    (magit-dash-gh--run-process
     '("x") dir
     (lambda (out) (setq result (list :ok out)))
     (lambda (out code) (setq result (list :err out code))))
    (magit-dash-gh-test--wait-until (lambda () result) 5)
    result))

(defun magit-dash-gh-test--proc-buffers (before)
  "Return process buffers created since the buffer list BEFORE."
  (seq-filter (lambda (b)
                (and (not (memq b before))
                     (string-prefix-p " *magit-dash-proc" (buffer-name b))))
              (buffer-list)))

(ert-deftest magit-dash-gh/run-process-success-excludes-stderr ()
  (magit-dash-gh-test--with-fake-gh
   "echo 'warning: noise' >&2; echo '[1]'; echo 'late noise' >&2"
   (lambda (dir)
     (should (equal '(:ok "[1]\n") (magit-dash-gh-test--run dir))))))

(ert-deftest magit-dash-gh/run-process-error-includes-stderr ()
  (magit-dash-gh-test--with-fake-gh
   "echo partial; echo 'HTTP 401 nope' >&2; exit 3"
   (lambda (dir)
     (let ((result (magit-dash-gh-test--run dir)))
       (should (eq :err (car result)))
       (should (string-match-p "HTTP 401 nope" (cadr result)))
       (should (= 3 (nth 2 result)))))))

(ert-deftest magit-dash-gh/run-process-leaves-no-buffers ()
  (magit-dash-gh-test--with-fake-gh
   "echo ok"
   (lambda (dir)
     (let ((before (buffer-list)))
       (magit-dash-gh-test--run dir)
       (should (null (magit-dash-gh-test--proc-buffers before)))))))

(ert-deftest magit-dash-gh/run-process-make-process-failure-leaks-nothing ()
  (let ((dir (make-temp-file "gh-nomake" t))
        (before (buffer-list)))
    (unwind-protect
        (cl-letf (((symbol-function 'make-process)
                   (lambda (&rest _) (error "no fork")))
                  ((symbol-function 'magit-dash-gh--repo-account) #'ignore))
          (should-error (magit-dash-gh--run-process '("x") dir #'ignore))
          (should (null (magit-dash-gh-test--proc-buffers before))))
      (delete-directory dir t))))

(ert-deftest magit-dash-gh/run-process-cancelled-runs-no-callbacks ()
  (magit-dash-gh-test--with-fake-gh
   "echo '[1]'"
   (lambda (dir)
     (let ((called nil))
       (magit-dash-gh--run-process '("x") dir
                                   (lambda (_) (setq called t))
                                   (lambda (&rest _) (setq called t))
                                   nil (lambda () t))
       (should-not called)))))

(ert-deftest magit-dash-gh/run-process-waits-for-late-stderr-before-retry ()
  "Stderr arriving after the stdout sentinel still triggers the 5xx retry."
  (let ((counter (make-temp-file "gh-count")))
    (unwind-protect
        (magit-dash-gh-test--with-fake-gh
         (format "n=$(cat %s 2>/dev/null); n=$((${n:-0}+1)); echo $n > %s
if [ $n -eq 1 ]; then
  (sleep 0.4; echo 'gh: HTTP 503' >&2) >/dev/null &
  exit 1
fi
echo done" counter counter)
         (lambda (dir)
           (let ((magit-dash-gh-retry-delay 0)
                 (magit-dash-gh-retry-attempts 3)
                 (result nil))
             (magit-dash-gh--run-process
              '("x") dir
              (lambda (out) (setq result (list :ok out)))
              (lambda (out code) (setq result (list :err out code))))
             (should (magit-dash-gh-test--wait-until (lambda () result) 5))
             (should (equal '(:ok "done\n") result)))))
      (delete-file counter))))

(ert-deftest magit-dash-gh/run-process-handle-tracks-retry-process ()
  (let ((counter (make-temp-file "gh-count")))
    (unwind-protect
        (magit-dash-gh-test--with-fake-gh
         (format "n=$(cat %s 2>/dev/null); n=$((${n:-0}+1)); echo $n > %s
if [ $n -eq 1 ]; then echo 'HTTP 502' >&2; exit 1; fi
exec sleep 30" counter counter)
         (lambda (dir)
           (let ((magit-dash-gh-retry-delay 0)
                 (handle (list nil))
                 (first nil))
             (unwind-protect
                 (progn
                   (setq first (magit-dash-gh--run-process '("x") dir #'ignore #'ignore
                                                           nil nil handle))
                   (should (eq first (car handle)))
                   (should (magit-dash-gh-test--wait-until
                            (lambda () (and (car handle) (not (eq (car handle) first))))
                            5))
                   (should (process-live-p (car handle)))
                   (let ((second (car handle)))
                     (magit-dash-gh--handle-cancel handle)
                     (should (magit-dash-gh-test--wait-until
                              (lambda () (not (process-live-p second)))
                              2))
                     (should (eq 'signal (process-status second)))))
               (magit-dash-gh--handle-cancel handle)))))
      (delete-file counter))))


;;;; token cache, stderr hygiene, retry start failures

(ert-deftest magit-dash-gh/account-token-is-cached-per-user-and-host ()
  (let ((counter (make-temp-file "tok-cache-count"))
        (magit-dash-gh--token-cache (make-hash-table :test #'equal))
        (magit-dash-gh--token-pending (make-hash-table :test #'equal))
        (magit-dash-gh-token-ttl 60))
    (unwind-protect
        (magit-dash-gh-test--with-fake-gh
         (format "n=$(cat %s 2>/dev/null); n=$((${n:-0}+1)); echo $n > %s
echo \"tok-$4-$n\"" counter counter)
         (lambda (_dir)
           (should (equal "tok-alice-1" (magit-dash-gh--account-token "alice" "github.com")))
           (should (equal "tok-alice-1" (magit-dash-gh--account-token "alice" "github.com")))
           (should (equal "1\n" (with-temp-buffer (insert-file-contents counter) (buffer-string))))
           (magit-dash-gh--account-token "bob" "github.com")
           (magit-dash-gh--account-token "alice" "ghe.example")
           (should (equal "3\n" (with-temp-buffer (insert-file-contents counter) (buffer-string))))
           (magit-dash-gh-reset-token-cache)
           (magit-dash-gh--account-token "alice" "github.com")
           (should (equal "4\n" (with-temp-buffer (insert-file-contents counter) (buffer-string))))))
      (delete-file counter))))

(ert-deftest magit-dash-gh/account-token-cache-expires ()
  (let ((counter (make-temp-file "tok-exp-count"))
        (magit-dash-gh--token-cache (make-hash-table :test #'equal))
        (magit-dash-gh--token-pending (make-hash-table :test #'equal))
        (magit-dash-gh-token-ttl 60))
    (unwind-protect
        (magit-dash-gh-test--with-fake-gh
         (format "n=$(cat %s 2>/dev/null); n=$((${n:-0}+1)); echo $n > %s
echo tok" counter counter)
         (lambda (_dir)
           (magit-dash-gh--account-token "alice" "github.com")
           (setf (cdr (map-elt magit-dash-gh--token-cache '("alice" . "github.com")))
                 (time-subtract (current-time) 120))
           (magit-dash-gh--account-token "alice" "github.com")
           (should (equal "2\n" (with-temp-buffer (insert-file-contents counter) (buffer-string))))
           (setq magit-dash-gh-token-ttl nil)
           (magit-dash-gh--account-token "alice" "github.com")
           (should (equal "3\n" (with-temp-buffer (insert-file-contents counter) (buffer-string))))))
      (delete-file counter))))

(ert-deftest magit-dash-gh/account-token-failure-not-cached-and-redacted ()
  (let ((counter (make-temp-file "tok-fail-count"))
        (magit-dash-gh--token-cache (make-hash-table :test #'equal))
        (magit-dash-gh--token-pending (make-hash-table :test #'equal)))
    (unwind-protect
        (magit-dash-gh-test--with-fake-gh
         (format "n=$(cat %s 2>/dev/null); n=$((${n:-0}+1)); echo $n > %s
echo 'bad ghp_SECRET123abc token'
exit 1" counter counter)
         (lambda (_dir)
           (let ((err (should-error (magit-dash-gh--account-token "bob" "github.com")
                                    :type 'user-error)))
             (should-not (string-match-p "SECRET123" (error-message-string err))))
           (should-error (magit-dash-gh--account-token "bob" "github.com") :type 'user-error)
           (should (equal "2\n" (with-temp-buffer (insert-file-contents counter) (buffer-string))))))
      (delete-file counter))))

(ert-deftest magit-dash-gh/sanitize-redacts-and-truncates ()
  (let ((magit-dash-gh-message-max-length 40))
    (dolist (secret '("ghp_abcDEF123" "gho_abc123" "github_pat_11ABC_xyz" "Bearer abc.def-1"))
      (let ((out (magit-dash-gh--sanitize (format "boom %s end" secret))))
        (should-not (string-match-p (regexp-quote (car (last (split-string secret " ")))) out))
        (should (string-match-p "\\[redacted\\]" out))))
    (let ((out (magit-dash-gh--sanitize (make-string 100 ?x))))
      (should (< (length out) 60))
      (should (string-suffix-p "[truncated]" out)))
    (should (equal "" (magit-dash-gh--sanitize nil)))))

(ert-deftest magit-dash-gh/run-process-error-output-is-redacted-and-bounded ()
  (magit-dash-gh-test--with-fake-gh
   "echo 'auth failed ghp_TOPSECRET99 Bearer abcdef' >&2; yes xxxxxxxxxx | head -n 400 >&2; exit 4"
   (lambda (dir)
     (let ((result (magit-dash-gh-test--run dir)))
       (should (eq :err (car result)))
       (should-not (string-match-p "TOPSECRET99\\|abcdef" (cadr result)))
       (should (<= (length (cadr result))
                   (+ magit-dash-gh-message-max-length 20)))))))

(ert-deftest magit-dash-gh/run-process-retry-start-failure-calls-on-error ()
  "A make-process failure on the retry reaches ON-ERROR instead of the timer."
  (let ((counter (make-temp-file "gh-count"))
        (real-make-process (symbol-function 'make-process)))
    (unwind-protect
        (magit-dash-gh-test--with-fake-gh
         (format "n=$(cat %s 2>/dev/null); n=$((${n:-0}+1)); echo $n > %s
echo 'HTTP 503' >&2; exit 1" counter counter)
         (lambda (dir)
           (let ((magit-dash-gh-retry-delay 0)
                 (magit-dash-gh-retry-attempts 3)
                 (starts 0)
                 (result nil))
             (cl-letf (((symbol-function 'make-process)
                        (lambda (&rest args)
                          (setq starts (1+ starts))
                          (if (> starts 1)
                              (error "no fork")
                            (apply real-make-process args)))))
               (magit-dash-gh--run-process
                '("x") dir
                (lambda (out) (setq result (list :ok out)))
                (lambda (out code) (setq result (list :err out code))))
               (should (magit-dash-gh-test--wait-until (lambda () result)))
               (should (eq :err (car result)))
               (should (string-match-p "no fork" (cadr result)))
               (should (= 2 starts))))))
      (delete-file counter))))

(ert-deftest magit-dash-gh/run-process-retry-start-failure-without-on-error-messages ()
  (let ((real-make-process (symbol-function 'make-process))
        (starts 0)
        (msgs nil))
    (magit-dash-gh-test--with-fake-gh
     "echo 'HTTP 502' >&2; exit 1"
     (lambda (dir)
       (let ((magit-dash-gh-retry-delay 0))
         (cl-letf (((symbol-function 'make-process)
                    (lambda (&rest args)
                      (setq starts (1+ starts))
                      (if (> starts 1)
                          (error "no fork")
                        (apply real-make-process args))))
                   ((symbol-function 'message)
                    (lambda (fmt &rest args) (push (apply #'format fmt args) msgs))))
           (magit-dash-gh--run-process '("x") dir #'ignore)
           (should (magit-dash-gh-test--wait-until
                    (lambda () (seq-find (lambda (m) (string-match-p "retry failed" m)) msgs))))))))))

(ert-deftest magit-dash-gh/run-process-survives-killed-buffers ()
  "Killing the process buffers before the exit does not break the finish step."
  (magit-dash-gh-test--with-fake-gh
   "exec sleep 30"
   (lambda (dir)
     (let ((before (process-list))
           (result nil)
           (magit-dash-gh-stderr-grace 0.1)
           (proc nil))
       (unwind-protect
           (progn
             (setq proc (magit-dash-gh--run-process
                         '("x") dir
                         (lambda (out) (setq result (list :ok out)))
                         (lambda (out code) (setq result (list :err out code)))))
             (let ((buf (process-buffer proc)))
               (when (buffer-live-p buf)
                 (let ((kill-buffer-query-functions nil))
                   (kill-buffer buf))))
             (when (process-live-p proc)
               (delete-process proc))
             (should (magit-dash-gh-test--wait-until (lambda () result) 3))
             (should (eq :err (car result)))
             (should (null (magit-dash-gh-test--new-live-procs before))))
         (when (process-live-p proc)
           (delete-process proc)))))))

(ert-deftest magit-dash-gh/fetch-closed-prs-and-prune-squash-merged ()
  "fetch-closed-prs preserves headRefOid and prune deletes squash-merged cleanly."
  (let ((repo (magit-dash-test--make-repo))
        (cache-dir (make-temp-file "mdgh-prune-cache-" t)))
    (unwind-protect
        (let* ((wt-match (expand-file-name ".wt/feat" repo))
               (wt-div (expand-file-name ".wt/diverged" repo))
               (magit-dash-prune-cache-dir cache-dir)
               (magit-dash-gh--cache (make-hash-table :test #'equal)))
          (magit-dash-test--git repo "worktree" "add" "-q" "-b" "feat" wt-match "HEAD")
          (magit-dash-test--git repo "worktree" "add" "-q" "-b" "diverged" wt-div "HEAD")
          (let* ((oid-match (magit-dash-test--commit wt-match "m.txt"))
                 (oid-div-pr (magit-dash-test--commit wt-div "d1.txt"))
                 (_oid-div-tip (magit-dash-test--commit wt-div "d2.txt"))
                 (prs (vector `((number . 101) (state . "MERGED")
                                (headRefName . "feat") (headRefOid . ,oid-match)
                                (title . "Squash merged"))
                              `((number . 102) (state . "MERGED")
                                (headRefName . "diverged") (headRefOid . ,oid-div-pr)
                                (title . "Diverged local"))))
                 (stale-table (make-hash-table :test #'equal)))
            ;; Seed stale cache entry lacking headRefOid to verify healing.
            (setf (map-elt stale-table "feat")
                  '((number . 101) (state . "MERGED") (headRefName . "feat")))
            (cl-letf (((symbol-function 'magit-dash-gh--repo-dir) (lambda () repo))
                      ((symbol-function 'magit-dash-gh--repo-account) (lambda (_) nil))
                      ((symbol-function 'magit-dash-gh--default-branch) (lambda () "main"))
                      ((symbol-function 'shell-command-to-string)
                       (lambda (cmd)
                         (if (string-match-p "headRefOid" cmd)
                             (json-serialize prs)
                           "[]"))))
              (let ((table (magit-dash-gh--fetch-closed-prs stale-table)))
                (should (equal oid-match (map-elt (map-elt table "feat") 'headRefOid)))
                (should (equal oid-match (map-elt (map-elt (magit-dash-gh--prune-load-cache) "feat")
                                                  'headRefOid))))
              (let ((candidates (magit-dash-gh--prune-scan)))
                (should (equal '("feat") (seq-map #'car candidates)))
                (should (equal `(head-oid)
                               (list (car (magit-dash-gh--prune-delete-risks
                                           "feat" wt-match repo)))))
                (should-not (cdr (magit-dash-gh--prune-delete-risks
                                  "feat" wt-match repo)))
                (should-not (car (magit-dash-gh--prune-delete-risks
                                  "diverged" wt-div repo)))
                (should (cdr (magit-dash-gh--prune-delete-risks
                              "diverged" wt-div repo))))
              ;; Dispatching single worktree clean passes headRefOid (no risk prompt).
              (cl-letf (((symbol-function 'magit-dash-worktree-confirm-risks)
                         (lambda (&rest _) (error "must not prompt for risk"))))
                (magit-dash-gh--prune-dispatch wt-match repo)
                (should-not (file-directory-p wt-match))
                ;; Recreate wt-match and verify prune-delete-branches cleans both cleanly.
                (magit-dash-test--git repo "worktree" "add" "-q" wt-match "feat")
                (let ((res (magit-dash-gh--prune-delete-branches '("feat") repo nil)))
                  (should (= 1 (plist-get res :deleted)))
                  (should-not (file-directory-p wt-match))
                  (should-not (magit-dash-gh--branch-tip "feat" repo)))))))
      (delete-directory repo t)
      (delete-directory cache-dir t))))

(ert-deftest magit-dash-gh/account-token-cache-hit-spawns-zero-processes ()
  "Cache hit returns immediately without spawning any process."
  (let ((magit-dash-gh--token-cache (make-hash-table :test #'equal))
        (magit-dash-gh-token-ttl 60)
        (procs 0))
    (setf (map-elt magit-dash-gh--token-cache '("alice" . "github.com"))
          (cons "tok-cached" (current-time)))
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest _) (setq procs (1+ procs)) 1))
              ((symbol-function 'make-process)
               (lambda (&rest _) (setq procs (1+ procs)) (error "unexpected make-process"))))
      (should (equal "tok-cached" (magit-dash-gh--account-token "alice" "github.com")))
      (let ((got nil))
        (magit-dash-gh--account-token "alice" "github.com"
                                      (lambda (tok err) (setq got (list tok err))))
        (should (equal '("tok-cached" nil) got)))
      (should (= 0 procs)))))

(ert-deftest magit-dash-gh/prime-account-token-async-dedupes-and-caches-ttl ()
  "Async token priming deduplicates in-flight lookups and caches per TTL."
  (let ((counter (make-temp-file "tok-count"))
        (magit-dash-gh--token-cache (make-hash-table :test #'equal))
        (magit-dash-gh--token-pending (make-hash-table :test #'equal))
        (magit-dash-gh-token-ttl 60)
        (magit-dash-gh-token-lookup-timeout 5))
    (unwind-protect
        (magit-dash-gh-test--with-fake-gh
         (format "n=$(cat %s 2>/dev/null); n=$((${n:-0}+1)); echo $n > %s
sleep 0.05
echo tok-async-$n" counter counter)
         (lambda (_dir)
           (let ((res1 nil)
                 (res2 nil))
             (magit-dash-gh--prime-account-token-async
              "alice" "github.com" (lambda (tok err) (setq res1 (list tok err))))
             (magit-dash-gh--prime-account-token-async
              "alice" "github.com" (lambda (tok err) (setq res2 (list tok err))))
             (should (magit-dash-gh-test--wait-until (lambda () (and res1 res2))))
             (should (equal '("tok-async-1" nil) res1))
             (should (equal '("tok-async-1" nil) res2))
             (should (equal "1\n" (with-temp-buffer
                                    (insert-file-contents counter)
                                    (buffer-string))))
             ;; Warm cache hit spawns 0 processes.
             (should (equal "tok-async-1"
                            (magit-dash-gh--account-token "alice" "github.com")))
             (should (equal "1\n" (with-temp-buffer
                                    (insert-file-contents counter)
                                    (buffer-string))))
             ;; After TTL expires, next prime re-fetches.
             (setf (cdr (map-elt magit-dash-gh--token-cache '("alice" . "github.com")))
                   (time-subtract (current-time) 120))
             (let ((res3 nil))
               (magit-dash-gh--prime-account-token-async
                "alice" "github.com" (lambda (tok err) (setq res3 (list tok err))))
               (should (magit-dash-gh-test--wait-until (lambda () res3)))
               (should (equal '("tok-async-2" nil) res3))))))
      (delete-file counter))))

(ert-deftest magit-dash-gh/account-token-bounded-timeout-on-hung-gh ()
  "Hung gh auth token fails within bounded timeout on both sync and async paths."
  (let ((magit-dash-gh--token-cache (make-hash-table :test #'equal))
        (magit-dash-gh--token-pending (make-hash-table :test #'equal))
        (magit-dash-gh-token-lookup-timeout 0.15)
        (before (process-list)))
    (unwind-protect
        (magit-dash-gh-test--with-fake-gh
         "exec sleep 30"
         (lambda (_dir)
           (let ((start (float-time)))
             (should-error (magit-dash-gh--account-token "alice" "github.com")
                           :type 'user-error)
             (should (< (- (float-time) start) 2.0)))
           (let ((async-res nil)
                 (start (float-time)))
             (magit-dash-gh--prime-account-token-async
              "bob" "github.com" (lambda (tok err) (setq async-res (list tok err))))
             (should (magit-dash-gh-test--wait-until (lambda () async-res) 2))
             (should (< (- (float-time) start) 2.0))
             (should-not (car async-res))
             (should (string-match-p "timed out" (cadr async-res))))
           (should (magit-dash-gh-test--wait-until
                    (lambda () (null (magit-dash-gh-test--new-live-procs before)))
                    2))))
      (seq-do (lambda (p) (when (process-live-p p) (delete-process p)))
              (seq-filter (lambda (p) (and (not (memq p before)) (process-live-p p)))
                          (process-list))))))

(ert-deftest magit-dash-gh/run-process-primes-cold-token-async ()
  "run-process primes a cold account token asynchronously without blocking."
  (let ((magit-dash-gh--token-cache (make-hash-table :test #'equal))
        (magit-dash-gh--token-pending (make-hash-table :test #'equal))
        (magit-dash-gh-token-ttl 60)
        (result nil))
    (magit-dash-gh-test--with-fake-gh
     "if [ \"$1\" = auth ] && [ \"$2\" = token ]; then
  echo tok-async-env
else
  echo \"tok=$GH_TOKEN\"
fi"
     (lambda (dir)
       (cl-letf (((symbol-function 'magit-dash-gh--repo-account)
                  (lambda (_) '(:user "alice" :host "github.com")))
                 ((symbol-function 'magit-dash-gh--account-token)
                  (lambda (&rest _) (error "must not resolve token synchronously"))))
         (magit-dash-gh--run-process
          '("pr" "list") dir
          (lambda (out) (setq result (list :ok (string-trim out))))
          (lambda (out code) (setq result (list :err out code))))
         (should (magit-dash-gh-test--wait-until (lambda () result) 2))
         (should (equal '(:ok "tok=tok-async-env") result))
         (should (equal "tok-async-env"
                        (car (map-elt magit-dash-gh--token-cache
                                      '("alice" . "github.com"))))))))))

(ert-deftest magit-dash-gh/reset-token-cache-settles-pending-run-process ()
  "reset-token-cache settles in-flight async token lookups and notifies callers."
  (let ((magit-dash-gh--token-cache (make-hash-table :test #'equal))
        (magit-dash-gh--token-pending (make-hash-table :test #'equal))
        (magit-dash-gh-token-lookup-timeout 5)
        (before (process-list))
        (result nil))
    (unwind-protect
        (magit-dash-gh-test--with-fake-gh
         "exec sleep 30"
         (lambda (dir)
           (cl-letf (((symbol-function 'magit-dash-gh--repo-account)
                      (lambda (_) '(:user "alice" :host "github.com"))))
             (magit-dash-gh--run-process
              '("pr" "list") dir
              (lambda (out) (setq result (list :ok out)))
              (lambda (out code) (setq result (list :err out code))))
             (should (map-elt magit-dash-gh--token-pending '("alice" . "github.com")))
             (magit-dash-gh-reset-token-cache)
             (should (zerop (hash-table-count magit-dash-gh--token-pending)))
             (should (eq :err (car result)))
             (should (string-match-p "cancelled" (cadr result)))
             (should (magit-dash-gh-test--wait-until
                      (lambda () (null (magit-dash-gh-test--new-live-procs before)))
                      2)))))
      (seq-do (lambda (p) (when (process-live-p p) (delete-process p)))
              (seq-filter (lambda (p) (and (not (memq p before)) (process-live-p p)))
                          (process-list))))))

(ert-deftest magit-dash-gh/prime-account-token-async-isolates-callback-errors ()
  "One failing callback does not drop subsequent waiters on the same key."
  (let ((magit-dash-gh--token-cache (make-hash-table :test #'equal))
        (magit-dash-gh--token-pending (make-hash-table :test #'equal))
        (magit-dash-gh-token-ttl 60)
        (second-res nil))
    (magit-dash-gh-test--with-fake-gh
     "sleep 0.05; echo tok-shared"
     (lambda (_dir)
       (magit-dash-gh--prime-account-token-async
        "alice" "github.com"
        (lambda (_tok _err) (error "boom in first callback")))
       (magit-dash-gh--prime-account-token-async
        "alice" "github.com"
        (lambda (tok err) (setq second-res (list tok err))))
       (should (magit-dash-gh-test--wait-until (lambda () second-res) 2))
       (should (equal '("tok-shared" nil) second-res))))))

(ert-deftest magit-dash-gh/account-token-ignores-stderr-warnings ()
  "Warnings on stderr do not contaminate the cached stdout token or leak buffers."
  (let ((magit-dash-gh--token-cache (make-hash-table :test #'equal))
        (magit-dash-gh--token-pending (make-hash-table :test #'equal))
        (before (buffer-list)))
    (magit-dash-gh-test--with-fake-gh
     "echo 'warning: gh upgrade available' >&2; echo ghp_clean_token"
     (lambda (_dir)
       (should (equal "ghp_clean_token"
                      (magit-dash-gh--account-token "alice" "github.com")))
       (should (null (seq-filter
                      (lambda (b)
                        (and (not (memq b before))
                             (string-prefix-p " *magit-dash-gh-token" (buffer-name b))))
                      (buffer-list))))))))

(ert-deftest magit-dash-gh/run-process-cancelled-during-async-token-prime ()
  "Cancelling while awaiting a cold async token skips spawning gh and callbacks."
  (let ((magit-dash-gh--token-cache (make-hash-table :test #'equal))
        (magit-dash-gh--token-pending (make-hash-table :test #'equal))
        (called nil)
        (cancelled nil)
        (spawned-main nil))
    (magit-dash-gh-test--with-fake-gh
     (format "if [ \"$1\" = auth ]; then
  sleep 0.05
  echo tok-late
else
  echo main-ran
fi")
     (lambda (dir)
       (cl-letf (((symbol-function 'magit-dash-gh--repo-account)
                  (lambda (_) '(:user "alice" :host "github.com")))
                 ((symbol-function 'magit-dash-gh--spawn-process)
                  (lambda (&rest _) (setq spawned-main t) nil)))
         (magit-dash-gh--run-process
          '("pr" "list") dir
          (lambda (out) (setq called (list :ok out)))
          (lambda (out code) (setq called (list :err out code)))
          nil
          (lambda () cancelled))
         (setq cancelled t)
         (should (magit-dash-gh-test--wait-until
                  (lambda ()
                    (map-elt magit-dash-gh--token-cache '("alice" . "github.com")))
                  2))
         (should-not spawned-main)
         (should-not called))))))

(ert-deftest magit-dash-gh/branch-verified-real-git-cases ()
  "Real git verification distinguishes merged, head-oid, remote, and unverified."
  (let ((repo (magit-dash-test--make-repo)))
    (unwind-protect
        (progn
          (magit-dash-test--git repo "checkout" "-q" "-b" "merged-b")
          (magit-dash-test--commit repo "merged.txt")
          (magit-dash-test--git repo "checkout" "-q" "main")
          (magit-dash-test--git repo "merge" "-q" "--no-ff" "merged-b" "-m" "merge")
          (magit-dash-test--git repo "checkout" "-q" "-b" "head-oid-b" "main")
          (let ((head-oid (magit-dash-test--commit repo "head-oid.txt")))
            (magit-dash-test--git repo "checkout" "-q" "-b" "remote-b" "main")
            (let ((remote-oid (magit-dash-test--commit repo "remote.txt")))
              (magit-dash-test--git repo "update-ref" "refs/remotes/origin/remote-b" remote-oid)
              (magit-dash-test--git repo "checkout" "-q" "-b" "unverified-b" "main")
              (magit-dash-test--commit repo "unverified.txt")
              (magit-dash-test--git repo "checkout" "-q" "main")
              (should (eq 'merged (magit-dash-gh--branch-verified "merged-b" nil repo "main")))
              (should (eq 'head-oid
                          (magit-dash-gh--branch-verified
                           "head-oid-b" `((headRefOid . ,head-oid)) repo "main")))
              (should (eq 'remote (magit-dash-gh--branch-verified "remote-b" nil repo "main")))
              (should-not (magit-dash-gh--branch-verified
                           "unverified-b" '((headRefOid . "deadbeef")) repo "main")))))
      (delete-directory repo t))))

(ert-deftest magit-dash-gh/prune-scan-and-delete-without-branch-verified-stub ()
  "Unverified reused branches are excluded from scan and refused by non-prompt prune."
  (let ((repo (magit-dash-test--make-repo))
        (cache-dir (make-temp-file "mdgh-verify-cache-" t)))
    (unwind-protect
        (let ((magit-dash-prune-cache-dir cache-dir)
              (magit-dash-gh--cache (make-hash-table :test #'equal)))
          (magit-dash-test--git repo "checkout" "-q" "-b" "reused" "main")
          (let ((old-oid (magit-dash-test--commit repo "old.txt")))
            (magit-dash-test--commit repo "new-unpushed.txt")
            (magit-dash-test--git repo "checkout" "-q" "-b" "squash-ok" "main")
            (let ((ok-oid (magit-dash-test--commit repo "ok.txt"))
                  (prs (make-hash-table :test #'equal)))
              (magit-dash-test--git repo "checkout" "-q" "main")
              (setf (map-elt prs "reused")
                    `((number . 10) (state . "MERGED") (headRefName . "reused")
                      (headRefOid . ,old-oid) (title . "Old PR")))
              (setf (map-elt prs "squash-ok")
                    `((number . 11) (state . "MERGED") (headRefName . "squash-ok")
                      (headRefOid . ,ok-oid) (title . "Squashed PR")))
              (cl-letf (((symbol-function 'magit-dash-gh--repo-dir) (lambda () repo))
                        ((symbol-function 'magit-dash-gh--default-branch) (lambda () "main"))
                        ((symbol-function 'magit-dash-gh--fetch-closed-prs) (lambda (&optional _) prs)))
                (let ((candidates (magit-dash-gh--prune-scan)))
                  (should (equal '("squash-ok") (seq-map #'car candidates))))
                (let ((res (magit-dash-gh--prune-delete-branches '("reused") repo nil)))
                  (should (= 0 (plist-get res :deleted)))
                  (should (= 1 (plist-get res :skipped)))
                  (should (magit-dash-gh--branch-tip "reused" repo)))))))
      (delete-directory repo t)
      (delete-directory cache-dir t))))

;;; test-magit-dash-gh.el ends here
