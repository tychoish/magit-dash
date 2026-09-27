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
    (cl-letf (((symbol-function 'magit-dash-gh--repo-dir) (lambda () "/tmp/r"))
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
    (cl-letf (((symbol-function 'magit-dash-gh--repo-dir) (lambda () "/tmp/r"))
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
    (cl-letf (((symbol-function 'magit-branch-delete)
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
    (cl-letf (((symbol-function 'magit-branch-delete)
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
    (cl-letf (((symbol-function 'magit-branch-delete)
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
    (cl-letf (((symbol-function 'magit-branch-delete)
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
    (cl-letf (((symbol-function 'magit-branch-delete)
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
    (cl-letf (((symbol-function 'magit-branch-delete) (lambda (&rest _) nil))
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
    (cl-letf (((symbol-function 'magit-branch-delete)
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
    (cl-letf (((symbol-function 'magit-branch-delete)
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
    (cl-letf (((symbol-function 'magit-branch-delete)
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
    (cl-letf (((symbol-function 'magit-branch-delete)
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
    (cl-letf (((symbol-function 'magit-branch-delete)
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
  (cl-letf (((symbol-function 'call-process)
             (lambda (_cmd _infile destination _display &rest _args)
               (with-current-buffer (if (listp destination) (car destination) (current-buffer))
                 (insert "ghp_mock_token_12345
"))
               0)))
    (should (equal "ghp_mock_token_12345"
                   (magit-dash-gh--account-token "alice" "github.com")))))

(ert-deftest magit-dash-gh/account-token-failure ()
  "account-token signals user-error on gh auth token exit non-zero."
  (cl-letf (((symbol-function 'call-process)
             (lambda (_cmd _infile destination _display &rest _args)
               (with-current-buffer (if (listp destination) (car destination) (current-buffer))
                 (insert "no token found for user
"))
               1)))
    (should-error (magit-dash-gh--account-token "bob" "github.com")
                  :type 'user-error)))

(ert-deftest magit-dash-gh/with-account-nil ()
  "with-magit-gh-account nil executes body without altering environment."
  (let ((token-before (getenv "GH_TOKEN")))
    (should (equal "result"
                   (with-magit-gh-account nil
                     (should (equal token-before (getenv "GH_TOKEN")))
                     "result")))))

(ert-deftest magit-dash-gh/with-account-github-com ()
  "with-magit-gh-account binds GH_TOKEN for github.com."
  (cl-letf (((symbol-function 'magit-dash-gh--account-token)
             (lambda (user host)
               (format "token-for-%s-on-%s" user host))))
    (with-magit-gh-account '(:user "alice" :host "github.com")
      (should (equal "token-for-alice-on-github.com" (getenv "GH_TOKEN")))
      (should-not (getenv "GH_ENTERPRISE_TOKEN")))))

(ert-deftest magit-dash-gh/with-account-enterprise ()
  "with-magit-gh-account binds GH_ENTERPRISE_TOKEN and GH_HOST for enterprise hosts."
  (cl-letf (((symbol-function 'magit-dash-gh--account-token)
             (lambda (user host)
               (format "ent-token-for-%s-on-%s" user host))))
    (with-magit-gh-account '(:user "corp-dev" :host "ghe.myorg.internal")
      (should (equal "ent-token-for-corp-dev-on-ghe.myorg.internal" (getenv "GH_ENTERPRISE_TOKEN")))
      (should (equal "ghe.myorg.internal" (getenv "GH_HOST"))))))

(ert-deftest magit-dash-gh/with-account-string ()
  "with-magit-gh-account supports plain username string defaulting to github.com."
  (cl-letf (((symbol-function 'magit-dash-gh--account-token)
             (lambda (user host)
               (format "token-%s-%s" user host))))
    (with-magit-gh-account "dev1"
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

;;; test-magit-dash-gh.el ends here
