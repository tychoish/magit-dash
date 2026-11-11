;;; test-magit-dash-gh-pr-status.el --- ERT tests for magit-dash-gh-pr-status -*- lexical-binding: t; no-byte-compile: t; -*-

;; The fixture is reconstructed from the 2026-11-04 snapshot of open PRs
;; in casaphq/go-monorepo (no PRs were open to capture live on 2026-11-06).
;;
;; Batch run:
;;   emacs --batch -l test/test-helper.el \
;;     -l test/test-magit-dash-gh-pr-status.el \
;;     --eval '(ert-run-tests-batch-and-exit "magit-dash-gh-pr-status/")'

(require 'ert)
(require 'cl-lib)
(require 'magit-dash)
(require 'magit-dash-gh-pr-status)

(defconst magit-dash-gh-pr-status-test/fixture
  (expand-file-name "fixtures/pr-status-2026-11-04.json"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defconst magit-dash-gh-pr-status-test/slug "casaphq/go-monorepo")

(defconst magit-dash-gh-pr-status-test/required 2
  "Approvals required by the shared render helpers.")

(defun magit-dash-gh-pr-status-test/response ()
  "Return the fixture parsed as a GraphQL response."
  (with-temp-buffer
    (insert-file-contents magit-dash-gh-pr-status-test/fixture)
    (json-parse-string (buffer-string) :object-type 'alist
                       :array-type 'list :null-object nil)))

(defun magit-dash-gh-pr-status-test/prs ()
  "Return the PR plists parsed from the fixture."
  (magit-dash-gh-pr-status--parse (magit-dash-gh-pr-status-test/response)
                                  magit-dash-gh-pr-status-test/required))

(defun magit-dash-gh-pr-status-test/pr (number &optional prs)
  (seq-find (lambda (pr) (= number (plist-get pr :number)))
            (or prs (magit-dash-gh-pr-status-test/prs))))

(defun magit-dash-gh-pr-status-test/pr-with (pr &rest props)
  "Return a copy of PR with PROPS set and its `:bucket' recomputed."
  (let ((new (copy-sequence pr)))
    (while props
      (setq new (plist-put new (pop props) (pop props))))
    (plist-put new :bucket
               (magit-dash-gh-pr-status--bucket
                new magit-dash-gh-pr-status-test/required))))

(defun magit-dash-gh-pr-status-test/replace-pr (prs number &rest props)
  "Return PRS with PR NUMBER updated by `--pr-with' using PROPS."
  (mapcar (lambda (pr)
            (if (= number (plist-get pr :number))
                (apply #'magit-dash-gh-pr-status-test/pr-with pr props)
              pr))
          prs))

(defun magit-dash-gh-pr-status-test/without (prs number)
  (seq-remove (lambda (pr) (= number (plist-get pr :number))) prs))

(defun magit-dash-gh-pr-status-test/user (login state)
  `((state . ,state) (author . ((login . ,login) (__typename . "User")))))

;;; Classification

(ert-deftest magit-dash-gh-pr-status/bucket-fixture ()
  (let ((prs (magit-dash-gh-pr-status-test/prs)))
    (should (= 10 (length prs)))
    (dolist (expected '((9921 . ready) (11005 . threads)
                        (11051 . fix-ci) (11006 . fix-ci) (11192 . fix-ci) (11193 . fix-ci)
                        (11202 . reviews) (11055 . reviews) (11052 . reviews) (11053 . reviews)))
      (let ((pr (magit-dash-gh-pr-status-test/pr (car expected) prs)))
        (should (eq (cdr expected) (plist-get pr :bucket)))
        (should (eq (cdr expected) (magit-dash-gh-pr-status--bucket pr 2)))))))

(ert-deftest magit-dash-gh-pr-status/bucket-required-approvals ()
  (let ((pr (magit-dash-gh-pr-status-test/pr 11005)))
    (setq pr (plist-put (plist-put (plist-put (copy-sequence pr) :threads-open 0) :approvals 1) :merge-state "CLEAN"))
    (should (eq 'ready (magit-dash-gh-pr-status--bucket pr 1)))
    (should (eq 'reviews (magit-dash-gh-pr-status--bucket pr 2)))))

(ert-deftest magit-dash-gh-pr-status/bucket-changes-requested-is-not-ready ()
  (let ((pr (magit-dash-gh-pr-status-test/pr 9921)))
    (should (eq 'ready (magit-dash-gh-pr-status--bucket pr 0)))
    (should (eq 'threads (magit-dash-gh-pr-status--bucket
                          (plist-put (copy-sequence pr) :changes-requested 1) 0)))))

(ert-deftest magit-dash-gh-pr-status/changes-requested-counted-per-latest-review ()
  (let ((reviews (list (magit-dash-gh-pr-status-test/user "a" "CHANGES_REQUESTED")
                       (magit-dash-gh-pr-status-test/user "b" "CHANGES_REQUESTED")
                       (magit-dash-gh-pr-status-test/user "b" "APPROVED")
                       (magit-dash-gh-pr-status-test/user "c" "CHANGES_REQUESTED")
                       (magit-dash-gh-pr-status-test/user "c" "COMMENTED")
                       (magit-dash-gh-pr-status-test/user "claude" "CHANGES_REQUESTED"))))
    (should (= 2 (magit-dash-gh-pr-status--changes-requested reviews)))
    (should (= 1 (magit-dash-gh-pr-status--approvals reviews)))))

(ert-deftest magit-dash-gh-pr-status/changes-requested-is-threads-bucket ()
  (let* ((node `((number . 5) (title . "t") (headRefName . "b") (baseRefName . "main")
                 (reviews . ((nodes . (,(magit-dash-gh-pr-status-test/user "a" "CHANGES_REQUESTED")
                                       ,(magit-dash-gh-pr-status-test/user "b" "APPROVED")
                                       ,(magit-dash-gh-pr-status-test/user "c" "APPROVED")))))
                 (reviewThreads . ((nodes . nil)))))
         (pr (car (magit-dash-gh-pr-status--parse
                   `((data . ((search . ((nodes . (,node))))))) 1))))
    (should (= 1 (plist-get pr :changes-requested)))
    (should (eq 'threads (plist-get pr :bucket)))
    (should (string-match-p "changes requested by 1 reviewer"
                            (plist-get (plist-get (magit-dash-gh-pr-attach-tracking pr) :tracking)
                                       :action-summary)))))

(ert-deftest magit-dash-gh-pr-status/required-approvals-per-repo ()
  (let ((magit-dash-repo-list
         (list (magit-dash-repo--make :name "m" :path "/tmp/m"
                                      :repo "casaphq/go-monorepo"
                                      :pr '(:required-approvals 3))))
        (magit-dash-gh-pr-status-required-approvals 1))
    (should (= 3 (magit-dash-gh-pr-status--required-approvals "casaphq/go-monorepo")))
    (should (= 1 (magit-dash-gh-pr-status--required-approvals "other/repo")))))

(ert-deftest magit-dash-gh-pr-status/required-approvals-validated ()
  (let ((magit-dash-gh-pr-status-required-approvals 1))
    (dolist (value '(nil "2" -1 1.5 :x))
      (let ((magit-dash-repo-list
             (list (magit-dash-repo--make :name "m" :path "/tmp/m" :repo "o/r"
                                          :pr (list :required-approvals value)))))
        (should (= 1 (magit-dash-gh-pr-status--required-approvals "o/r")))))
    (let ((magit-dash-repo-list
           (list (magit-dash-repo--make :name "m" :path "/tmp/m" :repo "o/r"
                                        :pr '(:required-approvals 0)))))
      (should (= 0 (magit-dash-gh-pr-status--required-approvals "o/r"))))))

(defmacro magit-dash-gh-pr-status-test/capturing-warnings (var &rest body)
  (declare (indent 1))
  `(let ((,var nil))
     (cl-letf (((symbol-function 'display-warning)
                (lambda (&rest args) (push args ,var))))
       ,@body)))

(ert-deftest magit-dash-gh-pr-status/check-options-warns-on-unknown-key ()
  (magit-dash-gh-pr-status-test/capturing-warnings warned
    (magit-dash-gh-pr-check-options "m" '(:bogus 1 :required-approvals 2))
    (should (= 1 (length warned)))))

(ert-deftest magit-dash-gh-pr-status/check-options-warns-on-bad-approvals ()
  (dolist (bad '(nil "2" -1))
    (magit-dash-gh-pr-status-test/capturing-warnings warned
      (magit-dash-gh-pr-check-options "m" (list :required-approvals bad))
      (should (= 1 (length warned)))))
  (magit-dash-gh-pr-status-test/capturing-warnings warned
    (magit-dash-gh-pr-check-options "m" '(:required-approvals 0))
    (should-not warned)))

(ert-deftest magit-dash-gh-pr-status/register-validates-pr-plist ()
  (let ((magit-dash-repo-list nil))
    (magit-dash-gh-pr-status-test/capturing-warnings warned
      (magit-dash-register :name "m" :path "/tmp/m" :repo "o/r" :pr '(:bogus 1))
      (should warned)
      (should (equal '(:bogus 1) (magit-dash-repo-pr (car magit-dash-repo-list)))))))

(ert-deftest magit-dash-gh-pr-status/repo-option-does-not-warn ()
  (let ((magit-dash-repo-list
         (list (magit-dash-repo--make :name "m" :path "/tmp/m" :repo "o/r"
                                      :pr '(:bogus 1)))))
    (magit-dash-gh-pr-status-test/capturing-warnings warned
      (should (eq 'dflt (magit-dash-gh-pr-repo-option "o/r" :required-approvals 'dflt)))
      (should (eq 1 (magit-dash-gh-pr-repo-option "o/r" :bogus 'dflt)))
      (should-not warned))))

(ert-deftest magit-dash-gh-pr-status/approvals-ignore-bots-and-comments ()
  (let ((pr (magit-dash-gh-pr-status-test/pr 11005)))
    ;; brian approved then commented; claude and a Bot approved.
    (should (= 1 (plist-get pr :approvals)))
    (should (equal '("james" "shilpam8") (plist-get pr :requested)))
    (should (= 5 (plist-get pr :threads-open)))
    (should (= 1 (plist-get pr :threads-outdated)))))

(ert-deftest magit-dash-gh-pr-status/approvals-changes-requested-retracts ()
  (should (= 0 (magit-dash-gh-pr-status--approvals
                (list (magit-dash-gh-pr-status-test/user "a" "APPROVED")
                      (magit-dash-gh-pr-status-test/user "a" "CHANGES_REQUESTED"))))))

(ert-deftest magit-dash-gh-pr-status/failing-checks-carry-urls ()
  (let ((pr (magit-dash-gh-pr-status-test/pr 11006)))
    (should (equal '("koalaty") (mapcar #'car (plist-get pr :failing))))
    (should (string-prefix-p "https://" (cdar (plist-get pr :failing))))
    (should (equal "FAILURE" (plist-get pr :ci)))))

(ert-deftest magit-dash-gh-pr-status/review-decision-empty ()
  (should (null (plist-get (magit-dash-gh-pr-status-test/pr 9921) :review-decision)))
  (should (plist-get (magit-dash-gh-pr-status-test/pr 11005) :review-decision)))

;;; Stacks

(ert-deftest magit-dash-gh-pr-status/stack-parents ()
  (let ((prs (magit-dash-gh-pr-status-test/prs)))
    (dolist (expected '((11006 . 11005) (11193 . 11192) (11051 . 11193) (11053 . 11052)
                        (9921) (11005) (11192) (11202) (11055) (11052)))
      (should (eq (cdr expected)
                  (plist-get (magit-dash-gh-pr-status-test/pr (car expected) prs)
                             :stack-parent))))))

(ert-deftest magit-dash-gh-pr-status/stack-parent-duplicate-head-branch ()
  (let* ((a (list :number 1 :branch "shared" :base "main"))
         (b (list :number 2 :branch "shared" :base "main"))
         (child (list :number 3 :branch "c" :base "shared"))
         (prs (list a b child)))
    (should (= 1 (magit-dash-gh-pr-status--stack-parent child prs)))
    (should (null (magit-dash-gh-pr-status--stack-parent a prs)))))

(ert-deftest magit-dash-gh-pr-status/ticket-ids ()
  (should (equal "FND-1269" (magit-dash-gh-pr-status--ticket "tycho/fnd-1269-top")))
  (should (null (magit-dash-gh-pr-status--ticket "tycho/cleanup"))))

(ert-deftest magit-dash-gh-pr-status/pending-statuscontext-and-no-rollup ()
  (let* ((node '((number . 1) (title . "t") (headRefName . "b") (baseRefName . "main")
                 (commits . ((nodes . (((commit . ((statusCheckRollup . nil))))))))
                 (reviewThreads . ((nodes . nil)))))
         (pr (magit-dash-gh-pr-status--node-to-pr node)))
    (should (null (plist-get pr :failing)))
    (should (null (plist-get pr :ci))))
  (should (equal '(("lint" . "u"))
                 (magit-dash-gh-pr-status--failing
                  '(((__typename . "StatusContext") (context . "lint")
                     (state . "FAILURE") (targetUrl . "u"))
                    ((__typename . "StatusContext") (context . "slow")
                     (state . "PENDING")))))))

;;; Fetching

(defun magit-dash-gh-pr-status-test/fetch-with (code out &optional slug)
  (cl-letf (((symbol-function 'magit-dash-gh--call-process)
             (lambda (_args _dir) (cons code out))))
    (magit-dash-gh-pr-status--fetch (or slug "o/r") "/tmp")))

(ert-deftest magit-dash-gh-pr-status/fetch-gh-failure ()
  (should-error (magit-dash-gh-pr-status-test/fetch-with 1 "boom") :type 'user-error))

(ert-deftest magit-dash-gh-pr-status/fetch-non-json ()
  (should-error (magit-dash-gh-pr-status-test/fetch-with 0 "not json") :type 'user-error)
  (should-error (magit-dash-gh-pr-status-test/fetch-with 0 "") :type 'user-error))

(ert-deftest magit-dash-gh-pr-status/fetch-graphql-errors ()
  (should-error (magit-dash-gh-pr-status-test/fetch-with
                 0 "{\"errors\":[{\"message\":\"bad\"}]}")
                :type 'user-error))

(ert-deftest magit-dash-gh-pr-status/fetch-success ()
  (should (magit-dash-gh-pr-status-test/fetch-with
           0 "{\"data\":{\"search\":{\"nodes\":[]}}}")))

(ert-deftest magit-dash-gh-pr-status/fetch-empty-object-is-valid-json ()
  (let ((err (should-error (magit-dash-gh-pr-status--parse-json "oops")
                           :type 'user-error)))
    (should (string-match-p "non-JSON" (cadr err))))
  (should-error (magit-dash-gh-pr-status-test/fetch-with 0 "{}") :type 'user-error))

(ert-deftest magit-dash-gh-pr-status/fetch-rejects-bad-slug ()
  (dolist (slug '("" "o" "o/r --repo:evil" "o/r extra" "a/b/c" "/tmp/x"
                  "o/r\nx" "../etc" "o/.." "o/."))
    (should-error (magit-dash-gh-pr-status-test/fetch-with
                   0 "{\"data\":{}}" slug)
                  :type 'user-error)))

(ert-deftest magit-dash-gh-pr-status/slug-accepts-real-names ()
  (dolist (slug '("o/r" "casaphq/go-monorepo" "o/.github" "o/a.b_c-d"))
    (magit-dash-gh-pr-status--check-slug slug)))

(ert-deftest magit-dash-gh-pr-status/collect-rejects-bad-slug-before-fetching ()
  (cl-letf (((symbol-function 'magit-dash-gh-pr-status--fetch-search)
             (lambda (&rest _) (ert-fail "fetched"))))
    (should-error (magit-dash-gh-pr-status-collect "/tmp/path" "/tmp") :type 'user-error)))

(ert-deftest magit-dash-gh-pr-status/registered-checkout-override ()
  (let ((magit-dash-repo-list nil)
        (magit-dash-gh-pr-status-required-approvals 1))
    (magit-dash-register :name "example" :path "/tmp/example" :repo "org/repo"
                         :pr '(:required-approvals 2))
    (should (= 2 (magit-dash-gh-pr-status--required-approvals "org/repo" "/tmp/example")))))

(ert-deftest magit-dash-gh-pr-status/blocked-prs-are-not-ready ()
  (let ((pr '(:approvals 1 :threads-open 0 :changes-requested 0)))
    (dolist (property '((:draft . t) (:ci . "PENDING") (:merge-state . "DIRTY")
                        (:merge-state . "BLOCKED") (:merge-state . "BEHIND")))
      (should (eq 'blocked (magit-dash-gh-pr-status--bucket
                            (plist-put (copy-sequence pr) (car property) (cdr property)) 1))))))

(ert-deftest magit-dash-gh-pr-status/fetch-rejects-incomplete-results ()
  (dolist (json '("{\"data\":{\"search\":{\"nodes\":[],\"pageInfo\":{\"hasNextPage\":true}}}}"
                  "{\"data\":{\"search\":{\"nodes\":[{\"number\":1,\"reviewThreads\":{\"pageInfo\":{\"hasNextPage\":true}}}]}}}"))
    (should-error (magit-dash-gh-pr-status-test/fetch-with 0 json) :type 'user-error)))

;;; Multi-repo collection and tracking

(defun magit-dash-gh-pr-status-test/node (repo number &rest extra)
  "Return a minimal GraphQL PR node for REPO and NUMBER, with EXTRA alist entries."
  `(,@extra
    (number . ,number) (title . "t") (headRefName . ,(format "b%d" number))
    (baseRefName . "main") (mergeStateStatus . "CLEAN")
    (repository . ((nameWithOwner . ,repo)))
    (reviews . ((nodes . nil)))
    (reviewThreads . ((nodes . nil)))))

(defun magit-dash-gh-pr-status-test/search (&rest nodes)
  `((data . ((search . ((nodes . ,nodes)))))))

(defun magit-dash-gh-pr-status-test/approved-by (&rest logins)
  `(reviews . ((nodes . ,(mapcar (lambda (l) (magit-dash-gh-pr-status-test/user l "APPROVED"))
                                 logins)))))

(ert-deftest magit-dash-gh-pr-status/query-includes-repository ()
  (should (string-match-p "repository{ nameWithOwner }" (magit-dash-gh-pr-status--query))))

(ert-deftest magit-dash-gh-pr-status/node-has-repo ()
  (let ((pr (magit-dash-gh-pr-status--node-to-pr
             (magit-dash-gh-pr-status-test/node "o/r" 1))))
    (should (equal "o/r" (plist-get pr :repo)))))

(ert-deftest magit-dash-gh-pr-status/collect-all-multi-repo ()
  (let ((magit-dash-repo-list
         (list (magit-dash-repo--make :name "a" :path "/tmp/a" :repo "o/a"
                                      :pr '(:required-approvals 2))))
        (magit-dash-gh-pr-status-required-approvals 1)
        (seen nil))
    (cl-letf (((symbol-function 'magit-dash-gh--call-process)
               (lambda (args _dir)
                 (setq seen args)
                 (cons 0 (json-encode
                          (magit-dash-gh-pr-status-test/search
                           (magit-dash-gh-pr-status-test/node
                            "o/b" 4 (magit-dash-gh-pr-status-test/approved-by "x"))
                           (magit-dash-gh-pr-status-test/node
                            "o/a" 9 (magit-dash-gh-pr-status-test/approved-by "x"))))))))
      (let ((prs (magit-dash-gh-pr-status-collect-all "/tmp")))
        (should (cl-some (lambda (a) (string-match-p "is:pr is:open author:@me\\'" a)) seen))
        (should-not (cl-some (lambda (a) (string-match-p "repo:" a)) seen))
        (should (equal '("o/b" "o/a") (mapcar (lambda (p) (plist-get p :repo)) prs)))
        ;; o/a requires 2 approvals, o/b falls back to 1.
        (should (eq 'ready (plist-get (nth 0 prs) :bucket)))
        (should (eq 'reviews (plist-get (nth 1 prs) :bucket)))))))

(ert-deftest magit-dash-gh-pr-status/collect-single-repo-sets-repo ()
  (cl-letf (((symbol-function 'magit-dash-gh-pr-status--fetch)
             (lambda (_s _d)
               (magit-dash-gh-pr-status-test/search
                (let ((n (magit-dash-gh-pr-status-test/node "x/y" 3)))
                  (assq-delete-all 'repository n))))))
    (should (equal "o/r" (plist-get (car (magit-dash-gh-pr-status-collect "o/r" "/tmp"))
                                    :repo)))))

(ert-deftest magit-dash-gh-pr-status/collect-is-pure-and-failure-preserves-cache ()
  (let ((magit-dash-gh-pr--tracking-cache (make-hash-table :test #'equal)))
    (puthash "o/r#1" '(:bucket ready) magit-dash-gh-pr--tracking-cache)
    (cl-letf (((symbol-function 'magit-dash-gh--call-process)
               (lambda (_a _d) (cons 1 "network down"))))
      (should-error (magit-dash-gh-pr-status-collect-all "/tmp") :type 'user-error))
    (should (= 1 (hash-table-count magit-dash-gh-pr--tracking-cache)))
    (cl-letf (((symbol-function 'magit-dash-gh--call-process)
               (lambda (_a _d)
                 (cons 0 (json-encode (magit-dash-gh-pr-status-test/search
                                       (magit-dash-gh-pr-status-test/node "o/r" 2)))))))
      (let ((prs (magit-dash-gh-pr-status-collect-all "/tmp")))
        (should-not (plist-member (car prs) :tracking))
        (should (= 1 (hash-table-count magit-dash-gh-pr--tracking-cache)))))))

(defmacro magit-dash-gh-pr-status-test/with-clean-cache (&rest body)
  (declare (indent 0))
  `(let ((magit-dash-gh-pr--tracking-cache (make-hash-table :test #'equal)))
     ,@body))

(defun magit-dash-gh-pr-status-test/tracking (pr)
  (plist-get (magit-dash-gh-pr-attach-tracking pr) :tracking))

(ert-deftest magit-dash-gh-pr-status/tracking-new-then-unchanged ()
  (magit-dash-gh-pr-status-test/with-clean-cache
    (let ((pr (magit-dash-gh-pr-status-test/pr 11006)))
      (let ((tr (magit-dash-gh-pr-status-test/tracking pr)))
        (should (eq 'new (plist-get tr :delta-type)))
        (should (null (plist-get tr :previous-bucket)))
        (should (string-match-p "\\`\\[[0-9-]+ [A-Za-z]+ [0-9:]+\\]\\'" (plist-get tr :last-seen)))
        (should (equal "fix failing CI: koalaty" (plist-get tr :action-summary)))
        (should (equal '("koalaty") (plist-get tr :failing-checks))))
      ;; Attaching alone does not commit.
      (should (eq 'new (plist-get (magit-dash-gh-pr-status-test/tracking pr) :delta-type)))
      (should-not (plist-member pr :tracking))
      (magit-dash-gh-pr-commit-tracking (list pr))
      (should (eq 'unchanged (plist-get (magit-dash-gh-pr-status-test/tracking pr) :delta-type))))))

(ert-deftest magit-dash-gh-pr-status/tracking-deltas ()
  (magit-dash-gh-pr-status-test/with-clean-cache
    (let* ((base (list :number 1 :repo "o/r" :bucket 'threads :threads-open 2
                       :approvals 0 :failing nil :required 1 :changes-requested 0))
           (delta (lambda (&rest props)
                    (let ((pr (copy-sequence base)))
                      (while props (setq pr (plist-put pr (pop props) (pop props))))
                      (plist-get (magit-dash-gh-pr-status-test/tracking pr) :delta-type)))))
      (magit-dash-gh-pr-commit-tracking (list base))
      (should (eq 'unchanged (funcall delta)))
      (should (eq 'threads-changed (funcall delta :threads-open 1)))
      (should (eq 'bucket-changed (funcall delta :bucket 'reviews :threads-open 0)))
      (should (eq 'ci-changed (funcall delta :failing '(("a" . "u")))))
      (should (equal "address 2 open review threads"
                     (plist-get (magit-dash-gh-pr-status-test/tracking base) :action-summary)))
      (should (equal "address 1 open review thread"
                     (plist-get (magit-dash-gh-pr-status-test/tracking
                                 (plist-put (copy-sequence base) :threads-open 1))
                                :action-summary)))
      (should (equal "threads"
                     (symbol-name (plist-get (magit-dash-gh-pr-status-test/tracking
                                              (plist-put (copy-sequence base) :bucket 'reviews))
                                             :previous-bucket)))))))

(ert-deftest magit-dash-gh-pr-status/tracking-action-summaries ()
  (magit-dash-gh-pr-status-test/with-clean-cache
    (let ((pr (list :number 1 :repo "o/r" :threads-open 0 :approvals 0 :required 1)))
      (dolist (case '((reviews . "needs 1 review") (blocked . "check merge blockers")
                      (ready . "ready to merge")))
        (should (equal (cdr case)
                       (plist-get (magit-dash-gh-pr-status-test/tracking
                                   (plist-put (copy-sequence pr) :bucket (car case)))
                                  :action-summary)))))))

(ert-deftest magit-dash-gh-pr-status/tracking-fingerprint ()
  (let* ((prs (magit-dash-gh-pr-status-test/prs))
         (fp (magit-dash-gh-pr-tracking-fingerprint prs)))
    (should (stringp fp))
    (should (= 32 (length fp)))
    (should (equal fp (magit-dash-gh-pr-tracking-fingerprint (reverse prs))))
    ;; Non-volatile fields do not matter.
    (should (equal fp (magit-dash-gh-pr-tracking-fingerprint
                       (mapcar (lambda (p) (plist-put (copy-sequence p) :title "other")) prs))))
    (dolist (change '((:threads-open . 99) (:approvals . 9) (:bucket . blocked)
                      (:state . "MERGED") (:failing . (("z" . "u"))) (:repo . "o/z")))
      (should-not (equal fp (magit-dash-gh-pr-tracking-fingerprint
                             (cons (plist-put (copy-sequence (car prs)) (car change) (cdr change))
                                   (cdr prs))))))))

;;; End-to-end mock GraphQL

(defun magit-dash-gh-pr-status-test/e2e-response (a1-threads a2-fail)
  "Mock GraphQL response: o/a#1 (A1-THREADS unresolved threads), o/a#2, o/b#7.
When A2-FAIL, o/a#2 has a failing check run."
  (let* ((threads (lambda (n)
                    `(reviewThreads . ((nodes . ,(cl-loop repeat n collect
                                                          '((isResolved . :false) (isOutdated . :false))))))))
         (checks (lambda (conclusion)
                   `(commits . ((nodes . (((commit . ((statusCheckRollup .
                                   ((contexts . ((nodes . (((__typename . "CheckRun") (name . "lint")
                                                            (conclusion . ,conclusion) (status . "COMPLETED")
                                                            (detailsUrl . "http://ci/lint"))))))))))))))))))
    (magit-dash-gh-pr-status-test/search
     (magit-dash-gh-pr-status-test/node "o/a" 1 (funcall threads a1-threads))
     (magit-dash-gh-pr-status-test/node "o/a" 2 (funcall checks (if a2-fail "FAILURE" "SUCCESS")))
     (magit-dash-gh-pr-status-test/node "o/b" 7 (magit-dash-gh-pr-status-test/approved-by "x")))))

(defun magit-dash-gh-pr-status-test/e2e-cycle ()
  "Collect, attach tracking to every PR, commit, and return the attached PRs."
  (let ((prs (magit-dash-gh-pr-status-collect-all "/tmp")))
    (prog1 (mapcar #'magit-dash-gh-pr-attach-tracking prs)
      (magit-dash-gh-pr-commit-tracking prs))))

(defun magit-dash-gh-pr-status-test/delta (prs repo number)
  (plist-get (plist-get (cl-find-if (lambda (p) (and (equal repo (plist-get p :repo))
                                                     (= number (plist-get p :number))))
                                    prs)
                        :tracking)
             :delta-type))

(ert-deftest magit-dash-gh-pr-status/e2e-collect-attach-commit-recollect ()
  (magit-dash-gh-pr-status-test/with-clean-cache
    (let ((magit-dash-repo-list nil)
          (magit-dash-gh-pr-status-required-approvals 1)
          (resp (magit-dash-gh-pr-status-test/e2e-response 2 t)))
      (cl-letf (((symbol-function 'magit-dash-gh--call-process)
                 (lambda (_a _d) (cons 0 (json-encode resp)))))
        (let ((first (magit-dash-gh-pr-status-test/e2e-cycle)))
          (should (equal '(threads fix-ci ready) (mapcar (lambda (p) (plist-get p :bucket)) first)))
          (should (equal '("lint") (mapcar #'car (plist-get (nth 1 first) :failing))))
          (should (= 2 (plist-get (nth 0 first) :threads-open)))
          (dolist (p first) (should (eq 'new (plist-get (plist-get p :tracking) :delta-type))))
          ;; Same data: all unchanged.
          (let ((second (magit-dash-gh-pr-status-test/e2e-cycle)))
            (dolist (p second) (should (eq 'unchanged (plist-get (plist-get p :tracking) :delta-type))))
            ;; o/a#1 resolves its threads (threads -> reviews), o/a#2 CI goes green.
            (setq resp (magit-dash-gh-pr-status-test/e2e-response 0 nil))
            (let ((third (magit-dash-gh-pr-status-test/e2e-cycle)))
              (should (eq 'bucket-changed (magit-dash-gh-pr-status-test/delta third "o/a" 1)))
              (should (eq 'bucket-changed (magit-dash-gh-pr-status-test/delta third "o/a" 2)))
              (should (eq 'unchanged (magit-dash-gh-pr-status-test/delta third "o/b" 7)))
              (should (eq 'threads (plist-get (plist-get (nth 0 third) :tracking) :previous-bucket))))))))))

(ert-deftest magit-dash-gh-pr-status/e2e-same-number-different-repo-tracked-separately ()
  (magit-dash-gh-pr-status-test/with-clean-cache
    (let ((magit-dash-repo-list nil))
      (cl-letf (((symbol-function 'magit-dash-gh--call-process)
                 (lambda (_a _d)
                   (cons 0 (json-encode (magit-dash-gh-pr-status-test/search
                                         (magit-dash-gh-pr-status-test/node "o/a" 5)))))))
        (magit-dash-gh-pr-status-test/e2e-cycle))
      (cl-letf (((symbol-function 'magit-dash-gh--call-process)
                 (lambda (_a _d)
                   (cons 0 (json-encode (magit-dash-gh-pr-status-test/search
                                         (magit-dash-gh-pr-status-test/node "o/b" 5)))))))
        (should (eq 'new (magit-dash-gh-pr-status-test/delta
                          (mapcar #'magit-dash-gh-pr-attach-tracking
                                  (magit-dash-gh-pr-status-collect-all "/tmp"))
                          "o/b" 5)))))))

(defconst magit-dash-gh-pr-status-test/root
  (file-name-directory (directory-file-name
                        (file-name-directory (or load-file-name buffer-file-name)))))

(defun magit-dash-gh-pr-status-test/emacs-eval (form)
  "Run FORM in a fresh `emacs -Q --batch' with this tree on the load path; return stdout."
  (let* ((root magit-dash-gh-pr-status-test/root)
         (args (append '("-Q" "--batch" "-L") (list root)
                       (cl-loop for p in load-path
                                unless (equal p root) append (list "-L" p))
                       (list "--eval" (prin1-to-string form)))))
    (with-temp-buffer
      (apply #'call-process (expand-file-name invocation-name invocation-directory)
             nil t nil args)
      (buffer-string))))

(ert-deftest magit-dash-gh-pr-status/does-not-load-org-or-render ()
  (let* ((root magit-dash-gh-pr-status-test/root)
         (src (expand-file-name "magit-dash-gh-pr-status.el" root)))
    ;; Source never requires org.
    (with-temp-buffer
      (insert-file-contents src)
      (should-not (re-search-forward "(require 'org\\b" nil t)))
    (let ((out (magit-dash-gh-pr-status-test/emacs-eval
                '(progn (require 'magit-dash-gh-pr-status)
                        (princ (format "RESULT org=%s render=%s"
                                       (featurep 'org)
                                       (and (apropos-internal "\\`magit-dash-gh-pr-status--render") t)))))))
      (should (string-match "RESULT org=\\([a-z]+\\) render=\\([a-z]+\\)" out))
      (should (equal "nil" (match-string 2 out)))
      (should (equal "nil" (match-string 1 out))))))

(provide 'test-magit-dash-gh-pr-status)
;;; test-magit-dash-gh-pr-status.el ends here
