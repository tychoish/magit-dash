;;; test-magit-dash-pr-status.el --- ERT tests for magit-dash-pr-status -*- lexical-binding: t; no-byte-compile: t; -*-

;; The fixture is reconstructed from the 2026-11-04 snapshot of open PRs
;; in casaphq/go-monorepo (no PRs were open to capture live on 2026-11-06).
;;
;; Batch run:
;;   emacs --batch -l test/test-helper.el \
;;     -l test/test-magit-dash-pr-status.el \
;;     --eval '(ert-run-tests-batch-and-exit "magit-dash-pr-status/")'

(require 'ert)
(require 'cl-lib)
(require 'magit-dash)
(require 'magit-dash-pr-status)

(defconst magit-dash-pr-status-test/fixture
  (expand-file-name "fixtures/pr-status-2026-11-04.json"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defconst magit-dash-pr-status-test/slug "casaphq/go-monorepo")

(defconst magit-dash-pr-status-test/required 2
  "Approvals required by the shared render helpers.")

(defun magit-dash-pr-status-test/response ()
  "Return the fixture parsed as a GraphQL response."
  (with-temp-buffer
    (insert-file-contents magit-dash-pr-status-test/fixture)
    (json-parse-string (buffer-string) :object-type 'alist
                       :array-type 'list :null-object nil)))

(defun magit-dash-pr-status-test/prs ()
  "Return the PR plists parsed from the fixture."
  (magit-dash-pr-status--parse (magit-dash-pr-status-test/response)
                                  magit-dash-pr-status-test/required))

(defun magit-dash-pr-status-test/pr (number &optional prs)
  (seq-find (lambda (pr) (= number (plist-get pr :number)))
            (or prs (magit-dash-pr-status-test/prs))))

(defun magit-dash-pr-status-test/pr-with (pr &rest props)
  "Return a copy of PR with PROPS set and its `:bucket' recomputed."
  (let ((new (copy-sequence pr)))
    (while props
      (setq new (plist-put new (pop props) (pop props))))
    (plist-put new :bucket
               (magit-dash-pr-status--bucket
                new magit-dash-pr-status-test/required))))

(defun magit-dash-pr-status-test/replace-pr (prs number &rest props)
  "Return PRS with PR NUMBER updated by `--pr-with' using PROPS."
  (mapcar (lambda (pr)
            (if (= number (plist-get pr :number))
                (apply #'magit-dash-pr-status-test/pr-with pr props)
              pr))
          prs))

(defun magit-dash-pr-status-test/without (prs number)
  (seq-remove (lambda (pr) (= number (plist-get pr :number))) prs))

(defun magit-dash-pr-status-test/user (login state)
  `((state . ,state) (author . ((login . ,login) (__typename . "User")))))

;;; Classification

(ert-deftest magit-dash-pr-status/bucket-fixture ()
  (let ((prs (magit-dash-pr-status-test/prs)))
    (should (= 10 (length prs)))
    (dolist (expected '((9921 . ready) (11005 . threads)
                        (11051 . fix-ci) (11006 . fix-ci) (11192 . fix-ci) (11193 . fix-ci)
                        (11202 . reviews) (11055 . reviews) (11052 . reviews) (11053 . reviews)))
      (let ((pr (magit-dash-pr-status-test/pr (car expected) prs)))
        (should (eq (cdr expected) (plist-get pr :bucket)))
        (should (eq (cdr expected) (magit-dash-pr-status--bucket pr 2)))))))

(ert-deftest magit-dash-pr-status/bucket-required-approvals ()
  (let ((pr (magit-dash-pr-status-test/pr 11005)))
    (setq pr (plist-put (plist-put (plist-put (copy-sequence pr) :threads-open 0) :approvals 1) :merge-state "CLEAN"))
    (should (eq 'ready (magit-dash-pr-status--bucket pr 1)))
    (should (eq 'reviews (magit-dash-pr-status--bucket pr 2)))))

(ert-deftest magit-dash-pr-status/bucket-changes-requested-is-not-ready ()
  (let ((pr (magit-dash-pr-status-test/pr 9921)))
    (should (eq 'ready (magit-dash-pr-status--bucket pr 0)))
    (should (eq 'threads (magit-dash-pr-status--bucket
                          (plist-put (copy-sequence pr) :changes-requested 1) 0)))))

(ert-deftest magit-dash-pr-status/changes-requested-counted-per-latest-review ()
  (let ((reviews (list (magit-dash-pr-status-test/user "a" "CHANGES_REQUESTED")
                       (magit-dash-pr-status-test/user "b" "CHANGES_REQUESTED")
                       (magit-dash-pr-status-test/user "b" "APPROVED")
                       (magit-dash-pr-status-test/user "c" "CHANGES_REQUESTED")
                       (magit-dash-pr-status-test/user "c" "COMMENTED")
                       (magit-dash-pr-status-test/user "claude" "CHANGES_REQUESTED"))))
    (should (= 2 (magit-dash-pr-status--changes-requested reviews)))
    (should (= 1 (magit-dash-pr-status--approvals reviews)))))

(ert-deftest magit-dash-pr-status/changes-requested-is-threads-bucket ()
  (let* ((node `((number . 5) (title . "t") (headRefName . "b") (baseRefName . "main")
                 (reviews . ((nodes . (,(magit-dash-pr-status-test/user "a" "CHANGES_REQUESTED")
                                       ,(magit-dash-pr-status-test/user "b" "APPROVED")
                                       ,(magit-dash-pr-status-test/user "c" "APPROVED")))))
                 (reviewThreads . ((nodes . nil)))))
         (pr (car (magit-dash-pr-status--parse
                   `((data . ((search . ((nodes . (,node))))))) 1))))
    (should (= 1 (plist-get pr :changes-requested)))
    (should (eq 'threads (plist-get pr :bucket)))
    (should (string-match-p "changes requested by 1 reviewer"
                            (plist-get (plist-get (magit-dash-pr-attach-tracking pr) :tracking)
                                       :action-summary)))))

(ert-deftest magit-dash-pr-status/required-approvals-per-repo ()
  (let ((magit-dash-repo-list
         (list (magit-dash-repo--make :name "m" :path "/tmp/m"
                                      :repo "casaphq/go-monorepo"
                                      :pr '(:required-approvals 3))))
        (magit-dash-pr-status-required-approvals 1))
    (should (= 3 (magit-dash-pr-status--required-approvals "casaphq/go-monorepo")))
    (should (= 1 (magit-dash-pr-status--required-approvals "other/repo")))))

(ert-deftest magit-dash-pr-status/required-approvals-validated ()
  (let ((magit-dash-pr-status-required-approvals 1))
    (dolist (value '(nil "2" -1 1.5 :x))
      (let ((magit-dash-repo-list
             (list (magit-dash-repo--make :name "m" :path "/tmp/m" :repo "o/r"
                                          :pr (list :required-approvals value)))))
        (should (= 1 (magit-dash-pr-status--required-approvals "o/r")))))
    (let ((magit-dash-repo-list
           (list (magit-dash-repo--make :name "m" :path "/tmp/m" :repo "o/r"
                                        :pr '(:required-approvals 0)))))
      (should (= 0 (magit-dash-pr-status--required-approvals "o/r"))))))

(defmacro magit-dash-pr-status-test/capturing-warnings (var &rest body)
  (declare (indent 1))
  `(let ((,var nil))
     (cl-letf (((symbol-function 'display-warning)
                (lambda (&rest args) (push args ,var))))
       ,@body)))

(ert-deftest magit-dash-pr-status/check-options-warns-on-unknown-key ()
  (magit-dash-pr-status-test/capturing-warnings warned
    (magit-dash-pr-check-options "m" '(:bogus 1 :required-approvals 2))
    (should (= 1 (length warned)))))

(ert-deftest magit-dash-pr-status/check-options-warns-on-bad-approvals ()
  (dolist (bad '(nil "2" -1))
    (magit-dash-pr-status-test/capturing-warnings warned
      (magit-dash-pr-check-options "m" (list :required-approvals bad))
      (should (= 1 (length warned)))))
  (magit-dash-pr-status-test/capturing-warnings warned
    (magit-dash-pr-check-options "m" '(:required-approvals 0))
    (should-not warned)))

(ert-deftest magit-dash-pr-status/register-validates-pr-plist ()
  (let ((magit-dash-repo-list nil))
    (magit-dash-pr-status-test/capturing-warnings warned
      (magit-dash-register :name "m" :path "/tmp/m" :repo "o/r" :pr '(:bogus 1))
      (should warned)
      (should (equal '(:bogus 1) (magit-dash-repo-pr (car magit-dash-repo-list)))))))

(ert-deftest magit-dash-pr-status/repo-option-does-not-warn ()
  (let ((magit-dash-repo-list
         (list (magit-dash-repo--make :name "m" :path "/tmp/m" :repo "o/r"
                                      :pr '(:bogus 1)))))
    (magit-dash-pr-status-test/capturing-warnings warned
      (should (eq 'dflt (magit-dash-pr-repo-option "o/r" :required-approvals 'dflt)))
      (should (eq 1 (magit-dash-pr-repo-option "o/r" :bogus 'dflt)))
      (should-not warned))))

(ert-deftest magit-dash-pr-status/approvals-ignore-bots-and-comments ()
  (let ((pr (magit-dash-pr-status-test/pr 11005)))
    ;; brian approved then commented; claude and a Bot approved.
    (should (= 1 (plist-get pr :approvals)))
    (should (equal '("james" "shilpam8") (plist-get pr :requested)))
    (should (= 5 (plist-get pr :threads-open)))
    (should (= 1 (plist-get pr :threads-outdated)))))

(ert-deftest magit-dash-pr-status/approvals-changes-requested-retracts ()
  (should (= 0 (magit-dash-pr-status--approvals
                (list (magit-dash-pr-status-test/user "a" "APPROVED")
                      (magit-dash-pr-status-test/user "a" "CHANGES_REQUESTED"))))))

(ert-deftest magit-dash-pr-status/failing-checks-carry-urls ()
  (let ((pr (magit-dash-pr-status-test/pr 11006)))
    (should (equal '("koalaty") (mapcar #'car (plist-get pr :failing))))
    (should (string-prefix-p "https://" (cdar (plist-get pr :failing))))
    (should (equal "FAILURE" (plist-get pr :ci)))))

(ert-deftest magit-dash-pr-status/review-decision-empty ()
  (should (null (plist-get (magit-dash-pr-status-test/pr 9921) :review-decision)))
  (should (plist-get (magit-dash-pr-status-test/pr 11005) :review-decision)))

;;; Stacks

(ert-deftest magit-dash-pr-status/stack-parents ()
  (let ((prs (magit-dash-pr-status-test/prs)))
    (dolist (expected '((11006 . 11005) (11193 . 11192) (11051 . 11193) (11053 . 11052)
                        (9921) (11005) (11192) (11202) (11055) (11052)))
      (should (eq (cdr expected)
                  (plist-get (magit-dash-pr-status-test/pr (car expected) prs)
                             :stack-parent))))))

(ert-deftest magit-dash-pr-status/stack-parent-duplicate-head-branch ()
  (let* ((a (list :number 1 :branch "shared" :base "main"))
         (b (list :number 2 :branch "shared" :base "main"))
         (child (list :number 3 :branch "c" :base "shared"))
         (prs (list a b child)))
    (should (= 1 (magit-dash-pr-status--stack-parent child prs)))
    (should (null (magit-dash-pr-status--stack-parent a prs)))))

(ert-deftest magit-dash-pr-status/ticket-ids ()
  (should (equal "FND-1269" (magit-dash-pr-status--ticket "tycho/fnd-1269-top")))
  (should (null (magit-dash-pr-status--ticket "tycho/cleanup"))))

(ert-deftest magit-dash-pr-status/pending-statuscontext-and-no-rollup ()
  (let* ((node '((number . 1) (title . "t") (headRefName . "b") (baseRefName . "main")
                 (commits . ((nodes . (((commit . ((statusCheckRollup . nil))))))))
                 (reviewThreads . ((nodes . nil)))))
         (pr (magit-dash-pr-status--node-to-pr node)))
    (should (null (plist-get pr :failing)))
    (should (null (plist-get pr :ci))))
  (should (equal '(("lint" . "u"))
                 (magit-dash-pr-status--failing
                  '(((__typename . "StatusContext") (context . "lint")
                     (state . "FAILURE") (targetUrl . "u"))
                    ((__typename . "StatusContext") (context . "slow")
                     (state . "PENDING")))))))

;;; Fetching

(defun magit-dash-pr-status-test/fetch-with (code out &optional slug)
  (cl-letf (((symbol-function 'magit-dash-gh--call-process)
             (lambda (_args _dir) (cons code out))))
    (magit-dash-pr-status--fetch (or slug "o/r") "/tmp")))

(ert-deftest magit-dash-pr-status/fetch-gh-failure ()
  (should-error (magit-dash-pr-status-test/fetch-with 1 "boom") :type 'user-error))

(ert-deftest magit-dash-pr-status/fetch-non-json ()
  (should-error (magit-dash-pr-status-test/fetch-with 0 "not json") :type 'user-error)
  (should-error (magit-dash-pr-status-test/fetch-with 0 "") :type 'user-error))

(ert-deftest magit-dash-pr-status/fetch-graphql-errors ()
  (should-error (magit-dash-pr-status-test/fetch-with
                 0 "{\"errors\":[{\"message\":\"bad\"}]}")
                :type 'user-error))

(ert-deftest magit-dash-pr-status/fetch-success ()
  (should (magit-dash-pr-status-test/fetch-with
           0 "{\"data\":{\"search\":{\"nodes\":[]}}}")))

(ert-deftest magit-dash-pr-status/fetch-empty-object-is-valid-json ()
  (let ((err (should-error (magit-dash-pr-status--parse-json "oops")
                           :type 'user-error)))
    (should (string-match-p "non-JSON" (cadr err))))
  (should-error (magit-dash-pr-status-test/fetch-with 0 "{}") :type 'user-error))

(ert-deftest magit-dash-pr-status/fetch-rejects-bad-slug ()
  (dolist (slug '("" "o" "o/r --repo:evil" "o/r extra" "a/b/c" "/tmp/x"
                  "o/r\nx" "../etc" "o/.." "o/."))
    (should-error (magit-dash-pr-status-test/fetch-with
                   0 "{\"data\":{}}" slug)
                  :type 'user-error)))

(ert-deftest magit-dash-pr-status/slug-accepts-real-names ()
  (dolist (slug '("o/r" "casaphq/go-monorepo" "o/.github" "o/a.b_c-d"))
    (magit-dash-pr-status--check-slug slug)))

(ert-deftest magit-dash-pr-status/collect-rejects-bad-slug-before-fetching ()
  (cl-letf (((symbol-function 'magit-dash-pr-status--fetch-search)
             (lambda (&rest _) (ert-fail "fetched"))))
    (should-error (magit-dash-pr-status-collect "/tmp/path" "/tmp") :type 'user-error)))

(ert-deftest magit-dash-pr-status/registered-checkout-override ()
  (let ((magit-dash-repo-list nil)
        (magit-dash-pr-status-required-approvals 1))
    (magit-dash-register :name "example" :path "/tmp/example" :repo "org/repo"
                         :pr '(:required-approvals 2))
    (should (= 2 (magit-dash-pr-status--required-approvals "org/repo" "/tmp/example")))))

(ert-deftest magit-dash-pr-status/blocked-prs-are-not-ready ()
  (let ((pr '(:approvals 1 :threads-open 0 :changes-requested 0)))
    (dolist (property '((:draft . t) (:ci . "PENDING") (:merge-state . "DIRTY")
                        (:merge-state . "BLOCKED") (:merge-state . "BEHIND")))
      (should (eq 'blocked (magit-dash-pr-status--bucket
                            (plist-put (copy-sequence pr) (car property) (cdr property)) 1))))))

(ert-deftest magit-dash-pr-status/fetch-rejects-incomplete-results ()
  (should-error (magit-dash-pr-status-test/fetch-with
                 0 "{\"data\":{\"search\":{\"nodes\":[],\"pageInfo\":{\"hasNextPage\":true}}}}")
                :type 'user-error))

(ert-deftest magit-dash-pr-status/truncated-nested-lists-degrade-to-partial ()
  (let ((messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (let ((response (magit-dash-pr-status--validate-response
                       "{\"data\":{\"search\":{\"nodes\":[{\"number\":1,\"reviewThreads\":{\"pageInfo\":{\"hasNextPage\":true}}}]}}}")))
        (should (= 1 (length (map-nested-elt response '(data search nodes)))))
        (should (cl-some (lambda (m) (string-match-p "#1" m)) messages))))))

(ert-deftest magit-dash-pr-status/summarize-counts-truncated-and-sorts ()
  "Summarize sorts by repo then number and notes truncated PRs."
  (let* ((summary (magit-dash-pr-status--summarize
                   (list (cons '(:label nil)
                               (list :prs (list '(:repo "o/b" :number 1)
                                                '(:repo "o/a" :number 9 :truncated t)
                                                '(:repo "o/a" :number 2))))
                         (cons '(:label "work") (list :prs (list '(:repo "o/a" :number 2))))))))
    (should (equal '(("o/a" . 2) ("o/a" . 9) ("o/b" . 1))
                   (seq-map (lambda (pr) (cons (plist-get pr :repo) (plist-get pr :number)))
                            (plist-get summary :prs))))
    (should (equal '("1 PR with truncated review/check data") (plist-get summary :notes)))
    (should-not (plist-get summary :errors))
    (should-not (plist-get summary :all-failed))))

(ert-deftest magit-dash-pr-status/summarize-all-failed ()
  (let ((summary (magit-dash-pr-status--summarize
                  (list (cons '(:label nil) '(:error "x"))
                        (cons '(:label "work") '(:error "y"))))))
    (should (plist-get summary :all-failed))
    (should (equal '("default gh account: x" "work: y") (plist-get summary :errors)))))

(ert-deftest magit-dash-pr-status/node-marks-truncated ()
  (should (plist-get (magit-dash-pr-status--node-to-pr
                      '((number . 1) (reviewThreads (pageInfo (hasNextPage . t)))))
                     :truncated))
  (should-not (plist-get (magit-dash-pr-status--node-to-pr '((number . 1))) :truncated)))

(ert-deftest magit-dash-pr-status/query-uses-lightweight-limits ()
  (let ((query (magit-dash-pr-status--query)))
    (should (string-match-p "reviews(last:30)" query))
    (should (string-match-p "contexts(first:50)" query))
    (should (string-match-p "reviewThreads(first:50)" query)))
  (should (= 10 magit-dash-pr-status-page-size)))

;;; Multi-repo collection and tracking

(defun magit-dash-pr-status-test/node (repo number &rest extra)
  "Return a minimal GraphQL PR node for REPO and NUMBER, with EXTRA alist entries."
  ;; Drop defaults that EXTRA overrides: duplicate JSON keys are resolved
  ;; differently by `json-parse-string' across Emacs versions.
  (append extra
          (cl-remove-if (lambda (cell) (assq (car cell) extra))
                        `((number . ,number) (title . "t")
                          (headRefName . ,(format "b%d" number))
                          (baseRefName . "main") (mergeStateStatus . "CLEAN")
                          (repository . ((nameWithOwner . ,repo)))
                          (reviews . ((nodes . nil)))
                          (reviewThreads . ((nodes . nil)))))))

(defun magit-dash-pr-status-test/search (&rest nodes)
  `((data . ((search . ((nodes . ,nodes)))))))

(defun magit-dash-pr-status-test/approved-by (&rest logins)
  `(reviews . ((nodes . ,(mapcar (lambda (l) (magit-dash-pr-status-test/user l "APPROVED"))
                                 logins)))))

(ert-deftest magit-dash-pr-status/query-includes-repository ()
  (should (string-match-p "repository{ nameWithOwner }" (magit-dash-pr-status--query))))

(ert-deftest magit-dash-pr-status/node-has-repo ()
  (let ((pr (magit-dash-pr-status--node-to-pr
             (magit-dash-pr-status-test/node "o/r" 1))))
    (should (equal "o/r" (plist-get pr :repo)))))

(ert-deftest magit-dash-pr-status/collect-all-multi-repo ()
  (let ((magit-dash-repo-list
         (list (magit-dash-repo--make :name "a" :path "/tmp/a" :repo "o/a"
                                      :pr '(:required-approvals 2))))
        (magit-dash-pr-status-required-approvals 1)
        (seen nil))
    (cl-letf (((symbol-function 'magit-dash-gh--call-process)
               (lambda (args _dir)
                 (setq seen args)
                 (cons 0 (json-encode
                          (magit-dash-pr-status-test/search
                           (magit-dash-pr-status-test/node
                            "o/b" 4 (magit-dash-pr-status-test/approved-by "x"))
                           (magit-dash-pr-status-test/node
                            "o/a" 9 (magit-dash-pr-status-test/approved-by "x"))))))))
      (let ((prs (magit-dash-pr-status-collect-all "/tmp")))
        (should (cl-some (lambda (a) (string-match-p "is:pr is:open archived:false author:@me\\'" a)) seen))
        (should-not (cl-some (lambda (a) (string-match-p "repo:" a)) seen))
        (should (equal '("o/a" "o/b") (mapcar (lambda (p) (plist-get p :repo)) prs)))
        ;; o/a requires 2 approvals, o/b falls back to 1.
        (should (eq 'reviews (plist-get (nth 0 prs) :bucket)))
        (should (eq 'ready (plist-get (nth 1 prs) :bucket)))))))

(defun magit-dash-pr-status-test/account-repos ()
  "Return registered repos spanning two accounts and the default."
  (list (magit-dash-repo--make :name "w1" :path "/tmp/w1" :gh-account "work")
        (magit-dash-repo--make :name "w2" :path "/tmp/w2" :gh-account "work")
        (magit-dash-repo--make :name "p1" :path "/tmp/p1" :gh-account "me" :gh-host "ghe.example")
        (magit-dash-repo--make :name "d1" :path "/tmp/d1")))

(ert-deftest magit-dash-pr-status/targets-one-per-account ()
  (let* ((magit-dash-repo-list (magit-dash-pr-status-test/account-repos))
         (targets (magit-dash-pr-status--targets "/tmp/here")))
    (should (equal '(nil "work@github.com" "me@ghe.example")
                   (seq-map (lambda (tg) (plist-get tg :label)) targets)))
    (should (equal "/tmp/here" (plist-get (car targets) :dir)))))

(ert-deftest magit-dash-pr-status/targets-omit-default-when-all-accounted ()
  (let* ((magit-dash-repo-list (seq-take (magit-dash-pr-status-test/account-repos) 3)))
    (should (equal '("work@github.com" "me@ghe.example")
                   (seq-map (lambda (tg) (plist-get tg :label))
                            (magit-dash-pr-status--targets "/tmp"))))))

(ert-deftest magit-dash-pr-status/collect-all-fans-out-and-survives-one-failure ()
  (let ((magit-dash-repo-list (magit-dash-pr-status-test/account-repos))
        (dirs nil)
        (messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages)))
              ((symbol-function 'magit-dash-gh--call-process)
               (lambda (_args dir)
                 (push dir dirs)
                 (cond
                  ((equal dir "/tmp/w1") (cons 1 "gh: HTTP 401"))
                  ((equal dir "/tmp/p1")
                   (cons 0 (json-encode (magit-dash-pr-status-test/search
                                         (magit-dash-pr-status-test/node "o/p" 7)))))
                  (t (cons 0 (json-encode (magit-dash-pr-status-test/search
                                           (magit-dash-pr-status-test/node "o/p" 7)
                                           (magit-dash-pr-status-test/node "o/d" 2)))))))))
      (let ((prs (magit-dash-pr-status-collect-all "/tmp/here")))
        (should (equal '("/tmp/here" "/tmp/w1" "/tmp/p1") (reverse dirs)))
        ;; o/p#7 appears under two accounts but is deduplicated.
        (should (equal '(2 7) (seq-map (lambda (p) (plist-get p :number)) prs)))
        (should (cl-some (lambda (m) (string-match-p "work@github.com" m)) messages))))))

(ert-deftest magit-dash-pr-status/collect-all-errors-naming-accounts-when-all-fail ()
  (let ((magit-dash-repo-list (seq-take (magit-dash-pr-status-test/account-repos) 3)))
    (cl-letf (((symbol-function 'magit-dash-gh--call-process)
               (lambda (_a _d) (cons 1 "gh: HTTP 401"))))
      (let ((err (should-error (magit-dash-pr-status-collect-all "/tmp") :type 'user-error)))
        (should (string-match-p "work@github.com" (cadr err)))
        (should (string-match-p "me@ghe.example" (cadr err)))))))

(ert-deftest magit-dash-pr-status/collect-all-async-reports-partial-failures ()
  (let ((magit-dash-repo-list (magit-dash-pr-status-test/account-repos))
        (result 'unset)
        (partial nil)
        (failure nil))
    (cl-letf (((symbol-function 'magit-dash-gh--run-process)
               (lambda (_args dir on-success on-error &rest _)
                 (if (equal dir "/tmp/w1")
                     (funcall on-error "gh: HTTP 401" 1)
                   (funcall on-success
                            (json-encode (magit-dash-pr-status-test/search
                                          (magit-dash-pr-status-test/node "o/d" 2))))))))
      (magit-dash-pr-status-collect-all-async
       (lambda (prs) (setq result prs))
       (lambda (err) (setq failure err))
       "/tmp/here"
       (lambda (errs) (setq partial errs)))
      (should-not failure)
      (should (equal '(2) (seq-map (lambda (p) (plist-get p :number)) result)))
      (should (= 1 (length partial)))
      (should (string-match-p "^work@github.com: " (car partial))))))

(ert-deftest magit-dash-pr-status/sync-and-async-report-identically ()
  "Both collectors give ON-PARTIAL the same notes and record them."
  (let ((magit-dash-repo-list (magit-dash-pr-status-test/account-repos))
        (sync-notes nil)
        (async-notes nil)
        (sync-recorded nil)
        (magit-dash-pr-status-timeout nil)
        (response (lambda (dir on-ok on-err)
                    (if (equal dir "/tmp/w1")
                        (funcall on-err)
                      (funcall on-ok (json-encode
                                      (magit-dash-pr-status-test/search
                                       (magit-dash-pr-status-test/node "o/d" 2)))))))
        (magit-dash-pr-status-last-notes nil))
    (cl-letf (((symbol-function 'magit-dash-gh--call-process)
               (lambda (_args dir)
                 (funcall response dir (lambda (out) (cons 0 out))
                          (lambda () (cons 1 "gh: HTTP 401")))))
              ((symbol-function 'magit-dash-gh--run-process)
               (lambda (_args dir on-success on-error &rest _)
                 (funcall response dir on-success
                          (lambda () (funcall on-error "gh: HTTP 401" 1))))))
      (magit-dash-pr-status-collect-all "/tmp/here" (lambda (n) (setq sync-notes n)))
      (setq sync-recorded magit-dash-pr-status-last-notes)
      (setq magit-dash-pr-status-last-notes nil)
      (magit-dash-pr-status-collect-all-async
       #'ignore nil "/tmp/here" (lambda (n) (setq async-notes n)))
      (should sync-notes)
      (should (string-match-p "^work@github.com: " (car sync-notes)))
      (should (equal sync-notes async-notes))
      (should (equal sync-notes sync-recorded))
      (should (equal sync-notes magit-dash-pr-status-last-notes)))))

(ert-deftest magit-dash-pr-status/last-notes-cleared-on-complete-collection ()
  (let ((magit-dash-repo-list nil)
        (magit-dash-pr-status-last-notes '("stale")))
    (cl-letf (((symbol-function 'magit-dash-gh--call-process)
               (lambda (_args _dir)
                 (cons 0 (json-encode (magit-dash-pr-status-test/search
                                       (magit-dash-pr-status-test/node "o/b" 4)))))))
      (magit-dash-pr-status-collect-all "/tmp")
      (should (null magit-dash-pr-status-last-notes)))))

(ert-deftest magit-dash-pr-status/collect-all-async-errors-when-all-fail ()
  (let ((magit-dash-repo-list (seq-take (magit-dash-pr-status-test/account-repos) 3))
        (failure nil)
        (succeeded nil))
    (cl-letf (((symbol-function 'magit-dash-gh--run-process)
               (lambda (_args _dir _on-success on-error &rest _)
                 (funcall on-error "boom" 1))))
      (magit-dash-pr-status-collect-all-async
       (lambda (_) (setq succeeded t))
       (lambda (err) (setq failure err))
       "/tmp")
      (should-not succeeded)
      (should (string-match-p "work@github.com: .*boom" failure)))))

(defun magit-dash-pr-status-test/wait (pred)
  "Run the event loop until PRED is non-nil, for at most about two seconds."
  (let ((tries 0))
    (while (and (not (funcall pred)) (< tries 40))
      (accept-process-output nil 0.05)
      (setq tries (1+ tries)))))

(ert-deftest magit-dash-pr-status/collect-all-async-progress-and-timeout ()
  "Results render as accounts answer; a silent account times out and is killed."
  (let ((magit-dash-repo-list (seq-take (magit-dash-pr-status-test/account-repos) 3))
        (magit-dash-pr-status-timeout 0.1)
        (hung (make-process :name "asw-hung" :command '("sleep" "30") :noquery t))
        (progress nil)
        (result 'unset)
        (partial nil))
    (unwind-protect
        (cl-letf (((symbol-function 'magit-dash-gh--run-process)
                   (lambda (_args dir on-success _on-error &rest _)
                     (if (equal dir "/tmp/w1")
                         hung
                       (funcall on-success
                                (json-encode (magit-dash-pr-status-test/search
                                              (magit-dash-pr-status-test/node "o/d" 2))))
                       nil))))
          (magit-dash-pr-status-collect-all-async
           (lambda (prs) (setq result prs))
           nil "/tmp/here"
           (lambda (notes) (setq partial notes))
           (lambda (prs notes pending) (push (list prs notes pending) progress)))
          ;; The two answering accounts arrived before the timeout fired.
          (should (eq result 'unset))
          (should (equal '(1) (seq-map #'cl-third progress)))
          (magit-dash-pr-status-test/wait (lambda () (not (eq result 'unset))))
          (should (equal '(2) (seq-map (lambda (p) (plist-get p :number)) result)))
          (should (string-match-p "^work@github.com: no response within" (car partial)))
          (magit-dash-pr-status-test/wait (lambda () (not (process-live-p hung))))
          (should-not (process-live-p hung)))
      (when (process-live-p hung)
        (delete-process hung)))))

(ert-deftest magit-dash-pr-status/collect-all-async-cancel-stops-callbacks ()
  "The returned canceller kills processes and silences later callbacks."
  (let ((magit-dash-repo-list (seq-take (magit-dash-pr-status-test/account-repos) 3))
        (magit-dash-pr-status-timeout nil)
        (hung (make-process :name "asw-hung2" :command '("sleep" "30") :noquery t))
        (late nil)
        (called nil))
    (unwind-protect
        (cl-letf (((symbol-function 'magit-dash-gh--run-process)
                   (lambda (_args _dir on-success _on-error &rest _)
                     (setq late on-success)
                     hung)))
          (let ((cancel (magit-dash-pr-status-collect-all-async
                         (lambda (_) (setq called t))
                         (lambda (_) (setq called t))
                         "/tmp/here")))
            (funcall cancel)
            (magit-dash-pr-status-test/wait (lambda () (not (process-live-p hung))))
            (should-not (process-live-p hung))
            (funcall late (json-encode (magit-dash-pr-status-test/search
                                        (magit-dash-pr-status-test/node "o/d" 2))))
            (should-not called)))
      (when (process-live-p hung)
        (delete-process hung)))))

(ert-deftest magit-dash-pr-status/collect-all-async-start-failure-counts-as-failure ()
  "A signal while starting `gh' fails that account instead of leaving it pending."
  (let ((magit-dash-repo-list (seq-take (magit-dash-pr-status-test/account-repos) 3))
        (magit-dash-pr-status-timeout nil)
        (result 'unset)
        (partial nil))
    (cl-letf (((symbol-function 'magit-dash-gh--run-process)
               (lambda (_args dir on-success _on-error &rest _)
                 (if (equal dir "/tmp/w1")
                     (error "cannot start gh")
                   (funcall on-success
                            (json-encode (magit-dash-pr-status-test/search
                                          (magit-dash-pr-status-test/node "o/d" 2))))
                   nil))))
      (magit-dash-pr-status-collect-all-async
       (lambda (prs) (setq result prs))
       nil "/tmp/here" (lambda (notes) (setq partial notes)))
      (should (equal '(2) (seq-map (lambda (p) (plist-get p :number)) result)))
      (should (string-match-p "^work@github.com: cannot start gh" (car partial))))))

(ert-deftest magit-dash-pr-status/include-archived-option ()
  (let ((seen nil)
        (magit-dash-pr-status-include-archived t))
    (cl-letf (((symbol-function 'magit-dash-gh--call-process)
               (lambda (args _dir)
                 (setq seen args)
                 (cons 0 (json-encode
                          (magit-dash-pr-status-test/search
                           (magit-dash-pr-status-test/node "o/b" 4)))))))
      (magit-dash-pr-status-collect-all "/tmp")
      (should (cl-some (lambda (a) (string-match-p "is:pr is:open author:@me\\'" a)) seen))
      (should-not (cl-some (lambda (a) (string-match-p "archived:false" a)) seen)))))

(ert-deftest magit-dash-pr-status/collect-all-async ()
  (let ((seen nil)
        (result nil)
        (error-result nil))
    (cl-letf (((symbol-function 'magit-dash-gh--run-process)
               (lambda (args _dir on-success _on-error &rest _)
                 (setq seen args)
                 (funcall on-success
                          (json-encode
                           (magit-dash-pr-status-test/search
                            (magit-dash-pr-status-test/node "o/b" 4)))))))
      (magit-dash-pr-status-collect-all-async
       (lambda (prs) (setq result prs))
       (lambda (err) (setq error-result err))
       "/tmp")
      (should (cl-some (lambda (a) (string-match-p "is:pr is:open archived:false author:@me\\'" a)) seen))
      (should-not error-result)
      (should (equal '("o/b") (mapcar (lambda (p) (plist-get p :repo)) result))))))

(ert-deftest magit-dash-pr-status/collect-single-repo-sets-repo ()
  (cl-letf (((symbol-function 'magit-dash-pr-status--fetch)
             (lambda (_s _d)
               (magit-dash-pr-status-test/search
                (let ((n (magit-dash-pr-status-test/node "x/y" 3)))
                  (assq-delete-all 'repository n))))))
    (should (equal "o/r" (plist-get (car (magit-dash-pr-status-collect "o/r" "/tmp"))
                                    :repo)))))

(ert-deftest magit-dash-pr-status/collect-is-pure-and-failure-preserves-cache ()
  (let ((magit-dash-pr--tracking-cache (make-hash-table :test #'equal)))
    (puthash "o/r#1" '(:bucket ready) magit-dash-pr--tracking-cache)
    (cl-letf (((symbol-function 'magit-dash-gh--call-process)
               (lambda (_a _d) (cons 1 "network down"))))
      (should-error (magit-dash-pr-status-collect-all "/tmp") :type 'user-error))
    (should (= 1 (hash-table-count magit-dash-pr--tracking-cache)))
    (cl-letf (((symbol-function 'magit-dash-gh--call-process)
               (lambda (_a _d)
                 (cons 0 (json-encode (magit-dash-pr-status-test/search
                                       (magit-dash-pr-status-test/node "o/r" 2)))))))
      (let ((prs (magit-dash-pr-status-collect-all "/tmp")))
        (should-not (plist-member (car prs) :tracking))
        (should (= 1 (hash-table-count magit-dash-pr--tracking-cache)))))))

(ert-deftest magit-dash-pr-status/retries-transient-502 ()
  (let ((calls 0)
        (magit-dash-pr-status-retry-delay 0))
    (cl-letf (((symbol-function 'magit-dash-gh--call-process)
               (lambda (_a _d)
                 (setq calls (1+ calls))
                 (if (< calls 3)
                     (cons 1 "<title>502 Bad Gateway</title>\ngh: HTTP 502")
                   (cons 0 (json-encode (magit-dash-pr-status-test/search
                                         (magit-dash-pr-status-test/node "o/r" 2))))))))
      (should (magit-dash-pr-status-collect "o/r" "/tmp"))
      (should (= 3 calls)))))

(ert-deftest magit-dash-pr-status/does-not-retry-non-transient-failure ()
  (let ((calls 0)
        (magit-dash-pr-status-retry-delay 0))
    (cl-letf (((symbol-function 'magit-dash-gh--call-process)
               (lambda (_a _d)
                 (setq calls (1+ calls))
                 (cons 1 "gh: HTTP 401"))))
      (should-error (magit-dash-pr-status-collect "o/r" "/tmp") :type 'user-error)
      (should (= 1 calls)))))

(ert-deftest magit-dash-pr-status/paginates-and-merges-pages ()
  (let ((afters nil)
        (magit-dash-pr-status-page-size 2))
    (cl-letf (((symbol-function 'magit-dash-gh--call-process)
               (lambda (args _d)
                 (let ((after (seq-find (lambda (a) (string-prefix-p "after=" a)) args)))
                   (push after afters)
                   (cons 0 (if (equal after "after=null")
                               "{\"data\":{\"search\":{\"nodes\":[{\"number\":1},{\"number\":2}],\"pageInfo\":{\"hasNextPage\":true,\"endCursor\":\"c1\"}}}}"
                             "{\"data\":{\"search\":{\"nodes\":[{\"number\":3}],\"pageInfo\":{\"hasNextPage\":false,\"endCursor\":\"c2\"}}}}"))))))
      (let ((response (magit-dash-pr-status--fetch-search "q" "/tmp")))
        (should (equal '(1 2 3)
                       (seq-map (lambda (n) (map-elt n 'number))
                                (map-nested-elt response '(data search nodes)))))
        (should (equal '("after=null" "after=c1") (nreverse afters)))))))

(defmacro magit-dash-pr-status-test/with-clean-cache (&rest body)
  (declare (indent 0))
  `(let ((magit-dash-pr--tracking-cache (make-hash-table :test #'equal)))
     ,@body))

(defun magit-dash-pr-status-test/tracking (pr)
  (plist-get (magit-dash-pr-attach-tracking pr) :tracking))

(ert-deftest magit-dash-pr-status/tracking-new-then-unchanged ()
  (magit-dash-pr-status-test/with-clean-cache
    (let ((pr (magit-dash-pr-status-test/pr 11006)))
      (let ((tr (magit-dash-pr-status-test/tracking pr)))
        (should (eq 'new (plist-get tr :delta-type)))
        (should (null (plist-get tr :previous-bucket)))
        (should (string-match-p "\\`\\[[0-9-]+ [A-Za-z]+ [0-9:]+\\]\\'" (plist-get tr :last-seen)))
        (should (equal "fix failing CI: koalaty" (plist-get tr :action-summary)))
        (should (equal '("koalaty") (plist-get tr :failing-checks))))
      ;; Attaching alone does not commit.
      (should (eq 'new (plist-get (magit-dash-pr-status-test/tracking pr) :delta-type)))
      (should-not (plist-member pr :tracking))
      (magit-dash-pr-commit-tracking (list pr))
      (should (eq 'unchanged (plist-get (magit-dash-pr-status-test/tracking pr) :delta-type))))))

(ert-deftest magit-dash-pr-status/tracking-deltas ()
  (magit-dash-pr-status-test/with-clean-cache
    (let* ((base (list :number 1 :repo "o/r" :bucket 'threads :threads-open 2
                       :approvals 0 :failing nil :required 1 :changes-requested 0))
           (delta (lambda (&rest props)
                    (let ((pr (copy-sequence base)))
                      (while props (setq pr (plist-put pr (pop props) (pop props))))
                      (plist-get (magit-dash-pr-status-test/tracking pr) :delta-type)))))
      (magit-dash-pr-commit-tracking (list base))
      (should (eq 'unchanged (funcall delta)))
      (should (eq 'threads-changed (funcall delta :threads-open 1)))
      (should (eq 'bucket-changed (funcall delta :bucket 'reviews :threads-open 0)))
      (should (eq 'ci-changed (funcall delta :failing '(("a" . "u")))))
      (should (equal "address 2 open review threads"
                     (plist-get (magit-dash-pr-status-test/tracking base) :action-summary)))
      (should (equal "address 1 open review thread"
                     (plist-get (magit-dash-pr-status-test/tracking
                                 (plist-put (copy-sequence base) :threads-open 1))
                                :action-summary)))
      (should (equal "threads"
                     (symbol-name (plist-get (magit-dash-pr-status-test/tracking
                                              (plist-put (copy-sequence base) :bucket 'reviews))
                                             :previous-bucket)))))))

(ert-deftest magit-dash-pr-status/tracking-action-summaries ()
  (magit-dash-pr-status-test/with-clean-cache
    (let ((pr (list :number 1 :repo "o/r" :threads-open 0 :approvals 0 :required 1)))
      (dolist (case '((reviews . "needs 1 review") (blocked . "check merge blockers")
                      (ready . "ready to merge")))
        (should (equal (cdr case)
                       (plist-get (magit-dash-pr-status-test/tracking
                                   (plist-put (copy-sequence pr) :bucket (car case)))
                                  :action-summary)))))))

(ert-deftest magit-dash-pr-status/tracking-fingerprint ()
  (let* ((prs (magit-dash-pr-status-test/prs))
         (fp (magit-dash-pr-tracking-fingerprint prs)))
    (should (stringp fp))
    (should (= 32 (length fp)))
    (should (equal fp (magit-dash-pr-tracking-fingerprint (reverse prs))))
    ;; Non-volatile fields do not matter.
    (should (equal fp (magit-dash-pr-tracking-fingerprint
                       (mapcar (lambda (p) (plist-put (copy-sequence p) :title "other")) prs))))
    (dolist (change '((:threads-open . 99) (:approvals . 9) (:bucket . blocked)
                      (:state . "MERGED") (:failing . (("z" . "u"))) (:repo . "o/z")))
      (should-not (equal fp (magit-dash-pr-tracking-fingerprint
                             (cons (plist-put (copy-sequence (car prs)) (car change) (cdr change))
                                   (cdr prs))))))))

;;; End-to-end mock GraphQL

(defun magit-dash-pr-status-test/e2e-response (a1-threads a2-fail)
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
    (magit-dash-pr-status-test/search
     (magit-dash-pr-status-test/node "o/a" 1 (funcall threads a1-threads))
     (magit-dash-pr-status-test/node "o/a" 2 (funcall checks (if a2-fail "FAILURE" "SUCCESS")))
     (magit-dash-pr-status-test/node "o/b" 7 (magit-dash-pr-status-test/approved-by "x")))))

(defun magit-dash-pr-status-test/e2e-cycle ()
  "Collect, attach tracking to every PR, commit, and return the attached PRs."
  (let ((prs (magit-dash-pr-status-collect-all "/tmp")))
    (prog1 (mapcar #'magit-dash-pr-attach-tracking prs)
      (magit-dash-pr-commit-tracking prs))))

(defun magit-dash-pr-status-test/delta (prs repo number)
  (plist-get (plist-get (cl-find-if (lambda (p) (and (equal repo (plist-get p :repo))
                                                     (= number (plist-get p :number))))
                                    prs)
                        :tracking)
             :delta-type))

(ert-deftest magit-dash-pr-status/e2e-collect-attach-commit-recollect ()
  (magit-dash-pr-status-test/with-clean-cache
    (let ((magit-dash-repo-list nil)
          (magit-dash-pr-status-required-approvals 1)
          (resp (magit-dash-pr-status-test/e2e-response 2 t)))
      (cl-letf (((symbol-function 'magit-dash-gh--call-process)
                 (lambda (_a _d) (cons 0 (json-encode resp)))))
        (let ((first (magit-dash-pr-status-test/e2e-cycle)))
          (should (equal '(threads fix-ci ready) (mapcar (lambda (p) (plist-get p :bucket)) first)))
          (should (equal '("lint") (mapcar #'car (plist-get (nth 1 first) :failing))))
          (should (= 2 (plist-get (nth 0 first) :threads-open)))
          (dolist (p first) (should (eq 'new (plist-get (plist-get p :tracking) :delta-type))))
          ;; Same data: all unchanged.
          (let ((second (magit-dash-pr-status-test/e2e-cycle)))
            (dolist (p second) (should (eq 'unchanged (plist-get (plist-get p :tracking) :delta-type))))
            ;; o/a#1 resolves its threads (threads -> reviews), o/a#2 CI goes green.
            (setq resp (magit-dash-pr-status-test/e2e-response 0 nil))
            (let ((third (magit-dash-pr-status-test/e2e-cycle)))
              (should (eq 'bucket-changed (magit-dash-pr-status-test/delta third "o/a" 1)))
              (should (eq 'bucket-changed (magit-dash-pr-status-test/delta third "o/a" 2)))
              (should (eq 'unchanged (magit-dash-pr-status-test/delta third "o/b" 7)))
              (should (eq 'threads (plist-get (plist-get (nth 0 third) :tracking) :previous-bucket))))))))))

(ert-deftest magit-dash-pr-status/e2e-same-number-different-repo-tracked-separately ()
  (magit-dash-pr-status-test/with-clean-cache
    (let ((magit-dash-repo-list nil))
      (cl-letf (((symbol-function 'magit-dash-gh--call-process)
                 (lambda (_a _d)
                   (cons 0 (json-encode (magit-dash-pr-status-test/search
                                         (magit-dash-pr-status-test/node "o/a" 5)))))))
        (magit-dash-pr-status-test/e2e-cycle))
      (cl-letf (((symbol-function 'magit-dash-gh--call-process)
                 (lambda (_a _d)
                   (cons 0 (json-encode (magit-dash-pr-status-test/search
                                         (magit-dash-pr-status-test/node "o/b" 5)))))))
        (should (eq 'new (magit-dash-pr-status-test/delta
                          (mapcar #'magit-dash-pr-attach-tracking
                                  (magit-dash-pr-status-collect-all "/tmp"))
                          "o/b" 5)))))))

(defconst magit-dash-pr-status-test/root
  (file-name-directory (directory-file-name
                        (file-name-directory (or load-file-name buffer-file-name)))))

(defun magit-dash-pr-status-test/emacs-eval (form)
  "Run FORM in a fresh `emacs -Q --batch' with this tree on the load path; return stdout."
  (let* ((root magit-dash-pr-status-test/root)
         (args (append '("-Q" "--batch" "-L") (list root)
                       (cl-loop for p in load-path
                                unless (equal p root) append (list "-L" p))
                       (list "--eval" (prin1-to-string form)))))
    (with-temp-buffer
      (apply #'call-process (expand-file-name invocation-name invocation-directory)
             nil t nil args)
      (buffer-string))))

(ert-deftest magit-dash-pr-status/does-not-load-org-or-render ()
  (let* ((root magit-dash-pr-status-test/root)
         (src (expand-file-name "magit-dash-pr-status.el" root)))
    ;; Source never requires org.
    (with-temp-buffer
      (insert-file-contents src)
      (should-not (re-search-forward "(require 'org\\b" nil t)))
    (let ((out (magit-dash-pr-status-test/emacs-eval
                '(progn (require 'magit-dash-pr-status)
                        (princ (format "RESULT org=%s render=%s"
                                       (featurep 'org)
                                       (and (apropos-internal "\\`magit-dash-pr-status--render") t)))))))
      (should (string-match "RESULT org=\\([a-z]+\\) render=\\([a-z]+\\)" out))
      (should (equal "nil" (match-string 2 out)))
      (should (equal "nil" (match-string 1 out))))))

;;;; fetch-one

(cl-defun magit-dash-pr-status-test/fetch-one-with-fake-gh
    (failures &key body (stderr "gh: HTTP 504") (slug magit-dash-pr-status-test/slug)
              (number 9921) on-success on-error (attempts 3) (wait 40)
              warn (sleep 0))
  "Run `fetch-one' for fixture PR 9921 against a fake `gh'.
The fake exits with STDERR FAILURES times, then prints BODY (default: the
fixture node).  ON-SUCCESS and ON-ERROR are extra callbacks called after
recording.  Return a plist of :result :error :calls :successes :errors."
  (let* ((dir (make-temp-file "fetch-one" t))
         (counter (expand-file-name "count" dir))
         (node (car (map-nested-elt (magit-dash-pr-status-test/response)
                                    '(data search nodes))))
         (body (or body
                   (json-encode `((data . ((repository . ((pullRequest . ,node))))))))))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "body.json" dir) (insert body))
          (with-temp-file (expand-file-name "gh" dir)
            (insert (format "#!/bin/sh
n=$(cat %s 2>/dev/null || echo 0); n=$((n+1)); echo $n > %s
if [ $n -le %d ]; then echo '%s' >&2; exit 1; fi
[ -n '%s' ] && echo '%s' >&2
%s
cat %s
" counter counter failures stderr (or warn "") (or warn "")
                         (if (and (numberp sleep) (> sleep 0))
                             (format "exec sleep %s" sleep)
                           "")
                         (expand-file-name "body.json" dir))))
          (set-file-modes (expand-file-name "gh" dir) #o755)
          (let ((exec-path (cons dir exec-path))
                (process-environment (cons (concat "PATH=" dir ":" (getenv "PATH"))
                                           process-environment))
                (magit-dash-gh-retry-delay 0)
                (magit-dash-gh-retry-attempts attempts)
                (magit-dash-pr-status-required-approvals 3)
                (successes nil)
                (errors nil)
                (tries 0))
            (cl-letf (((symbol-function 'magit-dash-gh--repo-account) #'ignore))
              (magit-dash-pr-status-fetch-one
               slug number dir
               (lambda (r)
                 (push r successes)
                 (when on-success (funcall on-success r)))
               (lambda (e)
                 (push e errors)
                 (when on-error (funcall on-error e))))
              (let ((deadline (+ (float-time) (/ (* wait 0.05) 1.0))))
                (while (and (not successes) (not errors) (< (float-time) deadline))
                  (accept-process-output nil 0.02)))
              ;; Let any stray duplicate callback arrive.
              (accept-process-output nil 0.1))
            (list :result (car successes)
                  :error (car errors)
                  :successes successes
                  :errors errors
                  :calls (if (file-exists-p counter)
                             (string-to-number
                              (string-trim (with-temp-buffer
                                             (insert-file-contents counter)
                                             (buffer-string))))
                           0))))
      (delete-directory dir t))))

(ert-deftest magit-dash-pr-status/fetch-one-returns-status-plist ()
  (let* ((out (magit-dash-pr-status-test/fetch-one-with-fake-gh 0))
         (result (plist-get out :result))
         (expected (magit-dash-pr-status-test/pr 9921)))
    (should result)
    (should-not (plist-get out :error))
    (should (= 1 (plist-get out :calls)))
    (should (= 1 (length (plist-get out :successes))))
    (seq-do (lambda (key) (should (plist-member result key)))
            '(:ci :approvals :required :requested :changes-requested
              :threads-open :threads-outdated :failing :truncated :bucket))
    (should (equal (plist-get expected :ci) (plist-get result :ci)))
    (should (= (plist-get expected :approvals) (plist-get result :approvals)))
    (should (= (plist-get expected :threads-open) (plist-get result :threads-open)))
    (should (equal (plist-get expected :requested) (plist-get result :requested)))
    (should (= (plist-get expected :changes-requested)
               (plist-get result :changes-requested)))
    (should (equal (plist-get expected :failing) (plist-get result :failing)))
    (should (= (plist-get expected :threads-outdated)
               (plist-get result :threads-outdated)))
    (should (= 3 (plist-get result :required)))
    (should (eq (magit-dash-pr-status--bucket expected 3) (plist-get result :bucket)))))

(ert-deftest magit-dash-pr-status/fetch-one-retries-504 ()
  (let ((out (magit-dash-pr-status-test/fetch-one-with-fake-gh 1)))
    (should (plist-get out :result))
    (should-not (plist-get out :error))
    (should (= 2 (plist-get out :calls)))
    (should (plist-get (plist-get out :result) :bucket))))

(ert-deftest magit-dash-pr-status/fetch-one-retries-exhausted ()
  (let ((out (magit-dash-pr-status-test/fetch-one-with-fake-gh 99 :attempts 2)))
    (should-not (plist-get out :result))
    (should (= 2 (plist-get out :calls)))
    (should (= 1 (length (plist-get out :errors))))
    (should (string-match-p "HTTP 504" (plist-get out :error)))))

(ert-deftest magit-dash-pr-status/fetch-one-non-5xx-failure ()
  (let ((out (magit-dash-pr-status-test/fetch-one-with-fake-gh
              99 :stderr "gh: HTTP 401 Bad credentials")))
    (should-not (plist-get out :result))
    (should (= 1 (plist-get out :calls)))
    (should (= 1 (length (plist-get out :errors))))
    (should (string-match-p "Bad credentials" (plist-get out :error)))))

(ert-deftest magit-dash-pr-status/fetch-one-graphql-errors ()
  (let ((out (magit-dash-pr-status-test/fetch-one-with-fake-gh
              0 :body "{\"errors\":[{\"message\":\"boom\"}]}")))
    (should-not (plist-get out :result))
    (should (= 1 (length (plist-get out :errors))))
    (should (string-match-p "boom" (plist-get out :error)))))

(ert-deftest magit-dash-pr-status/fetch-one-null-pull-request ()
  (let ((out (magit-dash-pr-status-test/fetch-one-with-fake-gh
              0 :body "{\"data\":{\"repository\":{\"pullRequest\":null}}}")))
    (should-not (plist-get out :result))
    (should (= 1 (length (plist-get out :errors))))
    (should (string-match-p "not found" (plist-get out :error)))))

(ert-deftest magit-dash-pr-status/fetch-one-malformed-json ()
  (let ((out (magit-dash-pr-status-test/fetch-one-with-fake-gh 0 :body "not json {")))
    (should-not (plist-get out :result))
    (should (= 1 (length (plist-get out :errors))))
    (should (string-match-p "JSON\\|pars" (plist-get out :error)))))

;; Hardening

(ert-deftest magit-dash-pr-status/fetch-one-ignores-stderr-noise ()
  (let ((out (magit-dash-pr-status-test/fetch-one-with-fake-gh
              0 :warn "warning: new gh release available")))
    (should (plist-get out :result))
    (should-not (plist-get out :error))))

(ert-deftest magit-dash-pr-status/parse-json-skips-leading-noise-and-bad-utf8 ()
  (should (equal '((a . 1))
                 (magit-dash-pr-status--parse-json "warn: x\n{\"a\":1}")))
  (should-error (magit-dash-pr-status--parse-json "just noise")
                :type 'user-error)
  (should-error (magit-dash-pr-status--parse-json (string-to-multibyte "{\"a\":\"\x80\"}"))
                :type 'user-error)
  ;; Emacs 29 does not define this condition; it cannot signal it either.
  (when (get 'json-utf8-decode-error 'error-conditions)
    (cl-letf (((symbol-function 'json-parse-string)
               (lambda (&rest _) (signal 'json-utf8-decode-error '("bad")))))
      (should-error (magit-dash-pr-status--parse-json "{}") :type 'user-error))))

(defun magit-dash-pr-status-test/wait-until (pred &optional timeout)
  "Run the event loop until PRED is non-nil or TIMEOUT (default 10) seconds pass.
Return PRED's final value."
  (let ((deadline (+ (float-time) (or timeout 10))))
    (while (and (not (funcall pred)) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (funcall pred)))

(defun magit-dash-pr-status-test/new-live-gh-procs (before)
  "Return live `magit-gh' processes created since BEFORE."
  (seq-filter (lambda (p)
                (and (not (memq p before))
                     (equal (process-name p) "magit-gh")
                     (process-live-p p)))
              (process-list)))

(ert-deftest magit-dash-pr-status/fetch-one-times-out-once ()
  (let* ((magit-dash-pr-status-fetch-one-timeout 0.3)
         (out (magit-dash-pr-status-test/fetch-one-with-fake-gh 0 :sleep 30 :wait 60)))
    (should-not (plist-get out :result))
    (should (= 1 (length (plist-get out :errors))))
    (should (string-match-p "timed out" (plist-get out :error)))
    (should-not (plist-get out :successes))))

(ert-deftest magit-dash-pr-status/fetch-one-timeout-cancels-process ()
  "The timeout kills the process and flips the CANCELLED-P it handed over."
  (let ((magit-dash-pr-status-fetch-one-timeout 0.2)
        (cancelled-p nil)
        (errors nil)
        (hung (make-process :name "fo-hung" :command '("sleep" "30") :noquery t)))
    (unwind-protect
        (cl-letf (((symbol-function 'magit-dash-gh--run-process)
                   (lambda (_args _dir _ok &optional _err _attempt cancelled _handle)
                     (setq cancelled-p cancelled)
                     hung)))
          (magit-dash-pr-status-fetch-one
           magit-dash-pr-status-test/slug 1 default-directory #'ignore
           (lambda (e) (push e errors)))
          (should-not (funcall cancelled-p))
          (should (magit-dash-pr-status-test/wait-until
                   (lambda () (and errors (not (process-live-p hung))))
                   3))
          (should (funcall cancelled-p))
          (should (= 1 (length errors)))
          (should-not (process-live-p hung))
          (should (eq 'signal (process-status hung))))
      (when (process-live-p hung)
        (delete-process hung)))))

(ert-deftest magit-dash-pr-status/fetch-one-real-timeout-kills-gh ()
  (let* ((before (process-list))
         (magit-dash-pr-status-fetch-one-timeout 0.3)
         (out (magit-dash-pr-status-test/fetch-one-with-fake-gh 0 :sleep 30 :wait 60)))
    (should-not (plist-get out :result))
    (should (= 1 (length (plist-get out :errors))))
    (should (string-match-p "timed out" (plist-get out :error)))
    (should (magit-dash-pr-status-test/wait-until
             (lambda () (null (magit-dash-pr-status-test/new-live-gh-procs before)))
             2))))

(ert-deftest magit-dash-pr-status/fetch-one-timeout-kills-retried-process ()
  "A retry already running when the timeout fires is killed too."
  (let* ((before (process-list))
         (magit-dash-pr-status-fetch-one-timeout 0.8)
         (out (magit-dash-pr-status-test/fetch-one-with-fake-gh
               1 :sleep 30 :wait 60)))
    (should (= 1 (length (plist-get out :errors))))
    (should (string-match-p "timed out" (plist-get out :error)))
    (should (= 2 (plist-get out :calls)))
    (should (magit-dash-pr-status-test/wait-until
             (lambda () (null (magit-dash-pr-status-test/new-live-gh-procs before)))
             2))))

(ert-deftest magit-dash-pr-status/null-thread-nodes-not-counted ()
  (let* ((node (copy-tree (car (map-nested-elt (magit-dash-pr-status-test/response)
                                               '(data search nodes)))))
         (base (plist-get (magit-dash-pr-status--node-to-pr node) :threads-open)))
    (setf (alist-get 'nodes (alist-get 'reviewThreads node))
          (append (list nil :null) (alist-get 'nodes (alist-get 'reviewThreads node))))
    (should (= base (plist-get (magit-dash-pr-status--node-to-pr node) :threads-open)))))

(ert-deftest magit-dash-pr-status/fetch-one-truncated-flag ()
  (let* ((node (copy-tree (car (map-nested-elt (magit-dash-pr-status-test/response)
                                               '(data search nodes)))))
         (page-info (copy-tree (alist-get 'pageInfo (alist-get 'reviewThreads node)))))
    (setf (alist-get 'hasNextPage page-info) t)
    (setf (alist-get 'pageInfo (alist-get 'reviewThreads node)) page-info)
    (let ((out (magit-dash-pr-status-test/fetch-one-with-fake-gh
                0 :body (json-encode
                         `((data . ((repository . ((pullRequest . ,node))))))))))
      (should (plist-get out :result))
      (should (eq t (plist-get (plist-get out :result) :truncated))))))

(ert-deftest magit-dash-pr-status/fetch-one-errors-alongside-data ()
  ;; Matches `--validate-response': a GraphQL errors key is a hard failure.
  (let* ((node (car (map-nested-elt (magit-dash-pr-status-test/response)
                                    '(data search nodes))))
         (out (magit-dash-pr-status-test/fetch-one-with-fake-gh
               0 :body (json-encode
                        `((data . ((repository . ((pullRequest . ,node)))))
                          (errors . [((message . "partial boom"))]))))))
    (should-not (plist-get out :result))
    (should (= 1 (length (plist-get out :errors))))
    (should (string-match-p "partial boom" (plist-get out :error)))))

(ert-deftest magit-dash-pr-status/fetch-one-non-positive-number ()
  (seq-do (lambda (n)
            (let ((out (magit-dash-pr-status-test/fetch-one-with-fake-gh 0 :number n)))
              (should-not (plist-get out :result))
              (should (= 0 (plist-get out :calls)))
              (should (= 1 (length (plist-get out :errors))))
              (should (string-match-p "positive integer" (plist-get out :error)))))
          '(0 -1)))

(ert-deftest magit-dash-pr-status/fetch-one-invalid-slug-and-number ()
  (let ((out (magit-dash-pr-status-test/fetch-one-with-fake-gh 0 :slug "nonsense")))
    (should-not (plist-get out :result))
    (should (= 0 (plist-get out :calls)))
    (should (= 1 (length (plist-get out :errors))))
    (should (string-match-p "slug" (plist-get out :error))))
  (let ((out (magit-dash-pr-status-test/fetch-one-with-fake-gh 0 :number "12")))
    (should-not (plist-get out :result))
    (should (= 0 (plist-get out :calls)))
    (should (= 1 (length (plist-get out :errors))))
    (should (string-match-p "integer" (plist-get out :error)))))

(ert-deftest magit-dash-pr-status/fetch-one-callbacks-fire-once ()
  ;; A signalling success callback must not trigger on-error.
  (let* ((node (car (map-nested-elt (magit-dash-pr-status-test/response)
                                    '(data search nodes))))
         (body (json-encode `((data . ((repository . ((pullRequest . ,node))))))))
         (errors nil)
         (successes 0))
    (cl-letf (((symbol-function 'magit-dash-gh--run-process)
               (lambda (_args _dir on-success &rest _) (funcall on-success body))))
      (should-error
       (magit-dash-pr-status-fetch-one
        magit-dash-pr-status-test/slug 9921 "/tmp"
        (lambda (_) (setq successes (1+ successes)) (error "callback boom"))
        (lambda (e) (push e errors)))))
    (should (= 1 successes))
    (should-not errors))
  ;; A signalling on-error after a bad payload runs once.
  (let ((count 0))
    (cl-letf (((symbol-function 'magit-dash-gh--run-process)
               (lambda (_args _dir on-success &rest _) (funcall on-success "not json {"))))
      (should-error
       (magit-dash-pr-status-fetch-one
        magit-dash-pr-status-test/slug 9921 "/tmp" #'ignore
        (lambda (_) (setq count (1+ count)) (error "callback boom")))))
    (should (= 1 count)))
  ;; A signalling error callback must not run on-error a second time.
  (let ((count 0))
    (let ((err (should-error
                (magit-dash-pr-status-test/fetch-one-with-fake-gh
                 0 :slug "nonsense"
                 :on-error (lambda (_) (setq count (1+ count)) (error "callback boom")))
                :type 'error)))
      (should (string-match-p "callback boom" (cadr err))))
    (should (= 1 count)))
  ;; Synchronous on-error from a missing directory, callback signals.
  (let ((count 0))
    (cl-letf (((symbol-function 'magit-dash-gh--repo-account) #'ignore))
      (should-error
       (magit-dash-pr-status-fetch-one
        magit-dash-pr-status-test/slug 1 "/nonexistent-dir-xyz"
        #'ignore
        (lambda (_) (setq count (1+ count)) (error "callback boom")))))
    (should (= 1 count))))

;;;; fetch-page-async: retries, cancellation, callbacks

(defun magit-dash-pr-status-test/with-counting-gh (script fn)
  "Call FN with a temp dir whose `gh' runs SCRIPT after bumping a call counter."
  (let ((dir (make-temp-file "fp-async" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "gh" dir)
            (insert (format "#!/bin/sh
n=$(cat %s 2>/dev/null || echo 0); n=$((n+1)); echo $n > %s
%s
" (expand-file-name "count" dir) (expand-file-name "count" dir) script)))
          (set-file-modes (expand-file-name "gh" dir) #o755)
          (let ((exec-path (cons dir exec-path))
                (process-environment (cons (concat "PATH=" dir ":" (getenv "PATH"))
                                           process-environment)))
            (cl-letf (((symbol-function 'magit-dash-gh--repo-account) #'ignore))
              (funcall fn dir))))
      (delete-directory dir t))))

(defun magit-dash-pr-status-test/gh-calls (dir)
  "Return how many times the counting fake gh in DIR has run."
  (let ((file (expand-file-name "count" dir)))
    (if (file-exists-p file)
        (string-to-number (with-temp-buffer (insert-file-contents file) (buffer-string)))
      0)))

(ert-deftest magit-dash-pr-status/fetch-page-async-always-504-makes-three-calls ()
  "Only one retry layer runs: 3 attempts, not 3x3."
  (magit-dash-pr-status-test/with-counting-gh
   "echo 'gh: HTTP 504' >&2; exit 1"
   (lambda (dir)
     (let ((magit-dash-gh-retry-delay 0)
           (magit-dash-gh-retry-attempts 3)
           (magit-dash-pr-status-retry-delay 0)
           (magit-dash-pr-status-retry-attempts 3)
           (errors nil))
       (magit-dash-pr-status--fetch-page-async
        "q" dir 10 nil #'ignore (lambda (e) (push e errors)))
       (should (magit-dash-pr-status-test/wait-until (lambda () errors)))
       (accept-process-output nil 0.2)
       (should (= 1 (length errors)))
       (should (string-match-p "HTTP 504" (car errors)))
       (should (= 3 (magit-dash-pr-status-test/gh-calls dir)))))))

(ert-deftest magit-dash-pr-status/fetch-page-async-cancel-stops-pending-retry ()
  "Cancelling the job while the retry timer is pending stops further calls."
  (magit-dash-pr-status-test/with-counting-gh
   "echo 'gh: HTTP 504' >&2; exit 1"
   (lambda (dir)
     (let ((magit-dash-gh-retry-delay 0.5)
           (magit-dash-gh-retry-attempts 3)
           (job (list nil))
           (called nil)
           (timer nil))
       (magit-dash-pr-status--fetch-page-async
        "q" dir 10 nil (lambda (_) (setq called t)) (lambda (_) (setq called t))
        nil nil job)
       (setq timer
             (cdr (should (magit-dash-pr-status-test/wait-until
                           (lambda ()
                             (seq-find (lambda (h) (and (consp h) (timerp (cdr h))))
                                       (cdr job)))))))
       (should (memq timer timer-list))
       (should (= 1 (magit-dash-pr-status-test/gh-calls dir)))
       (magit-dash-pr-status--job-cancel job)
       (should (eq t (car job)))
       (should (null (cdr job)))
       (should-not (memq timer timer-list))
       (accept-process-output nil 0.8)
       (should (= 1 (magit-dash-pr-status-test/gh-calls dir)))
       (should-not called)))))

(ert-deftest magit-dash-pr-status/fetch-page-async-cancel-kills-live-process ()
  (magit-dash-pr-status-test/with-counting-gh
   "exec sleep 30"
   (lambda (dir)
     (let ((before (process-list))
           (job (list nil))
           (called nil)
           (live nil))
       (unwind-protect
           (progn
             (magit-dash-pr-status--fetch-page-async
              "q" dir 10 nil (lambda (_) (setq called t)) (lambda (_) (setq called t))
              nil nil job)
             (setq live (magit-dash-pr-status-test/new-live-gh-procs before))
             (should (= 1 (length live)))
             (should (consp (cadr job)))
             (magit-dash-pr-status--job-cancel job)
             (should (eq t (car job)))
             (should (null (cdr job)))
             (should (magit-dash-pr-status-test/wait-until
                      (lambda () (not (process-live-p (car live))))
                      2))
             (should (eq 'signal (process-status (car live))))
             (accept-process-output nil 0.05)
             (should-not called))
         (seq-do (lambda (p) (when (process-live-p p) (delete-process p)))
                 live))))))

(ert-deftest magit-dash-pr-status/fetch-page-async-success-callback-error-is-messaged ()
  "An error in ON-SUCCESS is messaged and never reclassified as ON-ERROR."
  (magit-dash-pr-status-test/with-counting-gh
   (format "cat <<'EOF'\n%s\nEOF" (json-encode (magit-dash-pr-status-test/response)))
   (lambda (dir)
     (let ((msgs nil)
           (errors nil)
           (seen nil))
       (cl-letf (((symbol-function 'message)
                  (lambda (fmt &rest args) (push (apply #'format fmt args) msgs))))
         (magit-dash-pr-status--fetch-page-async
          "q" dir 10 nil
          (lambda (_) (setq seen t) (error "downstream boom"))
          (lambda (e) (push e errors)))
         (should (magit-dash-pr-status-test/wait-until (lambda () seen)))
         (accept-process-output nil 0.1))
       (should-not errors)
       (should (seq-find (lambda (m) (string-match-p "downstream boom" m)) msgs))))))

(ert-deftest magit-dash-pr-status/fetch-page-async-bad-response-calls-on-error ()
  (magit-dash-pr-status-test/with-counting-gh
   "echo 'not json {'"
   (lambda (dir)
     (let ((errors nil))
       (magit-dash-pr-status--fetch-page-async
        "q" dir 10 nil #'ignore (lambda (e) (push e errors)))
       (should (magit-dash-pr-status-test/wait-until (lambda () errors)))
       (should (= 1 (length errors)))))))

(ert-deftest magit-dash-pr-status/fetch-page-async-callbacks-fire-once ()
  "Neither callback runs more than once, even if a callback signals."
  ;; Synchronous on-error from a missing directory when on-error signals.
  (let ((error-count 0)
        (success-count 0))
    (cl-letf (((symbol-function 'magit-dash-gh--repo-account) #'ignore))
      (should-error
       (magit-dash-pr-status--fetch-page-async
        "q" "/nonexistent-dir-xyz" 10 nil
        (lambda (_) (setq success-count (1+ success-count)))
        (lambda (_) (setq error-count (1+ error-count)) (error "callback boom")))))
    (should (= 0 success-count))
    (should (= 1 error-count)))
  ;; Synchronous make-process failure when on-error signals.
  (let ((error-count 0)
        (success-count 0))
    (cl-letf (((symbol-function 'magit-dash-gh--repo-account) #'ignore)
              ((symbol-function 'make-process)
               (lambda (&rest _) (error "no fork"))))
      (should-error
       (magit-dash-pr-status--fetch-page-async
        "q" temporary-file-directory 10 nil
        (lambda (_) (setq success-count (1+ success-count)))
        (lambda (_) (setq error-count (1+ error-count)) (error "callback boom")))))
    (should (= 0 success-count))
    (should (= 1 error-count)))
  ;; Validation failure when run-process delivers bad payload synchronously and on-error signals.
  (let ((error-count 0)
        (success-count 0))
    (cl-letf (((symbol-function 'magit-dash-gh--run-process)
               (lambda (_args _dir on-success &rest _) (funcall on-success "not json {"))))
      (should-error
       (magit-dash-pr-status--fetch-page-async
        "q" "/tmp" 10 nil
        (lambda (_) (setq success-count (1+ success-count)))
        (lambda (_) (setq error-count (1+ error-count)) (error "callback boom")))))
    (should (= 0 success-count))
    (should (= 1 error-count)))
  ;; Signalling on-success runs once and never triggers on-error.
  (let ((error-count 0)
        (success-count 0)
        (body (json-encode (magit-dash-pr-status-test/response))))
    (cl-letf (((symbol-function 'magit-dash-gh--run-process)
               (lambda (_args _dir on-success &rest _) (funcall on-success body))))
      (magit-dash-pr-status--fetch-page-async
       "q" "/tmp" 10 nil
       (lambda (_) (setq success-count (1+ success-count)) (error "callback boom"))
       (lambda (_) (setq error-count (1+ error-count)))))
    (should (= 1 success-count))
    (should (= 0 error-count))))

(ert-deftest magit-dash-pr-status/fetch-one-retry-start-failure-calls-on-error-without-timeout ()
  "A failure to start the retry reports through ON-ERROR even with no timeout."
  (let ((real-make-process (symbol-function 'make-process))
        (starts 0)
        (errors nil)
        (magit-dash-pr-status-fetch-one-timeout nil)
        (magit-dash-gh-retry-delay 0)
        (magit-dash-gh-retry-attempts 3))
    (magit-dash-pr-status-test/with-counting-gh
     "echo 'gh: HTTP 503' >&2; exit 1"
     (lambda (dir)
       (cl-letf (((symbol-function 'make-process)
                  (lambda (&rest args)
                    (setq starts (1+ starts))
                    (if (> starts 1)
                        (error "no fork")
                      (apply real-make-process args)))))
         (magit-dash-pr-status-fetch-one
          magit-dash-pr-status-test/slug 7 dir #'ignore
          (lambda (e) (push e errors)))
         (should (magit-dash-pr-status-test/wait-until (lambda () errors)))
         (accept-process-output nil 0.1)
         (should (= 1 (length errors)))
         (should (string-match-p "no fork" (car errors))))))))

(provide 'test-magit-dash-pr-status)
;;; test-magit-dash-pr-status.el ends here
