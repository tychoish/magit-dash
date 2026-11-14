;;; test-magit-dash-mcp.el --- ERT tests for magit-dash-mcp -*- lexical-binding: t -*-

;;; Commentary:

;; Unit tests for `magit-dash-mcp'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'magit-dash)
(require 'magit-dash-mcp)

(ert-deftest magit-dash-mcp/service-and-tools-registered ()
  "Verify `magit-dash' mcpkit service and its 4 high-level tools are registered."
  (let* ((svc (mcpkit-get-service 'magit-dash))
         (tools (and svc (mcpkit-service-tools svc))))
    (should svc)
    (should (hash-table-p tools))
    (should (gethash "manage_worktrees" tools))
    (should (gethash "create_worktree_for_pr" tools))
    (should (gethash "clean_merged_worktrees" tools))
    (should (gethash "prune_branches" tools))
    (should (gethash "magit_dash_pr_status" tools))
    (should (gethash "magit_dash_pr_threads" tools))
    (should (gethash "magit_dash_pr_ci_failures" tools))
    (should (gethash "stack_show" tools))
    (should (gethash "stack_sync" tools))))

(ert-deftest magit-dash-mcp/manage-worktrees-actions ()
  "Test `manage_worktrees' tool for list, create, delete, and review actions."
  (let ((tmp-repo (make-temp-file "md-mcp-repo-" t))
        (tmp-wt (make-temp-file "md-mcp-wt-" t))
        (deleted-wt nil))
    (unwind-protect
        (cl-letf (((symbol-function 'magit-dash-gh--git-worktrees)
                   (lambda (_dir)
                     (list `((path . ,tmp-repo) (branch . "main") (head . "111"))
                           `((path . ,tmp-wt) (branch . "feat-x") (head . "222")))))
                  ((symbol-function 'magit-dash-gh-pr--fetch-repo-prs)
                   (lambda (_slug _state &optional _dir)
                     (list '((number . 42)
                             (state . "OPEN")
                             (mergedAt . :null)
                             (headRefName . "feat-x")))))
                  ((symbol-function 'magit-dash-gh--default-branch)
                   (lambda () "main"))
                  ((symbol-function 'magit-dash-gh--git-merged-branches)
                   (lambda (_def _dir) nil))
                  ((symbol-function 'magit-dash-worktree-uncommitted-changes-p)
                   (lambda (_dir) nil))
                  ((symbol-function 'magit-dash-worktree-create)
                   (lambda (_main wt-path branch _base)
                     (format "%s/%s" wt-path branch)))
                  ((symbol-function 'magit-dash-gh-pr-delete-worktree)
                   (lambda (wt-path _main)
                     (setq deleted-wt (expand-file-name wt-path))))
                  ((symbol-function 'magit-dash-gh-pr-review-worktrees)
                   (lambda (_dir _prompt-p)
                     '(:renamed 1 :deleted 2 :matching 3 :skipped 0))))
          ;; 1. list
          (let* ((res (magit-dash-mcp-call-tool
                       "manage_worktrees"
                       (list :repo tmp-repo :action "list")))
                 (wts (plist-get res :worktrees)))
            (should (equal (plist-get res :action) "list"))
            (should (= (length wts) 2))
            (should (equal (plist-get (aref wts 1) :branch) "feat-x"))
            (should (equal (plist-get (aref wts 1) :pr_number) 42)))
          ;; 2. create
          (let ((res (magit-dash-mcp-call-tool
                      "manage_worktrees"
                      (list :repo tmp-repo
                            :action "create"
                            :branch "new-feature"
                            :worktree_path "/tmp/custom-wt"))))
            (should (equal (plist-get res :action) "create"))
            (should (equal (plist-get res :branch) "new-feature"))
            (should (equal (plist-get res :worktree_path) "/tmp/custom-wt/new-feature")))
          ;; 3. delete
          (let ((res (magit-dash-mcp-call-tool
                      "manage_worktrees"
                      (list :repo tmp-repo
                            :action "delete"
                            :worktree_path tmp-wt))))
            (should (equal (plist-get res :action) "delete"))
            (should (eq (plist-get res :deleted) t))
            (should (equal deleted-wt (expand-file-name tmp-wt))))
          ;; 4. review
          (let ((res (magit-dash-mcp-call-tool
                      "manage_worktrees"
                      (list :repo tmp-repo :action "review"))))
            (should (equal (plist-get res :action) "review"))
            (should (= (plist-get res :renamed) 1))
            (should (= (plist-get res :deleted) 2))
            (should (= (plist-get res :matching) 3))))
      (delete-directory tmp-repo t)
      (delete-directory tmp-wt t))))

(ert-deftest magit-dash-mcp/create-worktree-for-pr ()
  "Test `create_worktree_for_pr' for single PR, multiple PRs, and all_open."
  (let ((tmp-repo (make-temp-file "md-mcp-pr-" t))
        (created-nums nil))
    (unwind-protect
        (cl-letf (((symbol-function 'magit-dash-mcp--resolve-repo-slug)
                   (lambda (_repo _dir) "org/repo"))
                  ((symbol-function 'magit-dash-gh-pr--create-worktree-for-entry)
                   (lambda (entry &optional _switch-p)
                     (let ((num (plist-get entry :number)))
                       (push num created-nums)
                       (format "/tmp/wt-pr-%d" num))))
                  ((symbol-function 'magit-dash-gh-pr-review-worktrees)
                   (lambda (_dir _prompt) '(:renamed 0 :deleted 0 :matching 0 :skipped 0)))
                  ((symbol-function 'magit-dash-gh-pr--fetch-repo-prs)
                   (lambda (_slug _state &optional _dir)
                     (list '((number . 10) (headRefName . "b10"))
                           '((number . 20) (headRefName . "b20")))))
                  ((symbol-function 'magit-dash-gh--git-worktrees)
                   (lambda (_dir) nil)))
          ;; Single PR
          (let ((res (magit-dash-mcp-call-tool
                      "create_worktree_for_pr"
                      (list :repo tmp-repo :pr_number 7))))
            (should (= (plist-get res :created) 1))
            (should (equal (plist-get res :worktree_path) "/tmp/wt-pr-7")))
          ;; All open PRs
          (setq created-nums nil)
          (let ((res (magit-dash-mcp-call-tool
                      "create_worktree_for_pr"
                      (list :repo tmp-repo :all_open t))))
            (should (= (plist-get res :created) 2))
            (should (equal (nreverse created-nums) '(10 20)))))
      (delete-directory tmp-repo t))))

(ert-deftest magit-dash-mcp/clean-merged-worktrees-and-prune-branches ()
  "Test `clean_merged_worktrees' and `prune_branches' MCP tools."
  (let ((tmp-repo (make-temp-file "md-mcp-prune-" t))
        (cleaned-entries nil))
    (unwind-protect
        (cl-letf (((symbol-function 'magit-dash-mcp--resolve-repo-slug)
                   (lambda (_repo _dir) "org/repo"))
                  ((symbol-function 'magit-dash-gh-pr--clean-worktree-for-entry)
                   (lambda (entry _prompt-p)
                     (push (plist-get entry :number) cleaned-entries)
                     :deleted))
                  ((symbol-function 'magit-dash-gh-pr-clean-merged-worktrees)
                   (lambda (_dir _prompt-p)
                     '(:deleted 2 :skipped 1)))
                  ((symbol-function 'magit-dash-gh--default-branch)
                   (lambda () "main"))
                  ((symbol-function 'magit-dash-gh--prune-scan)
                   (lambda ()
                     (list (cons "merged-a" '((number . 1) (state . "MERGED")))
                           (cons "merged-b" '((number . 2) (state . "MERGED"))))))
                  ((symbol-function 'magit-dash-gh--prune-delete-branches)
                   (lambda (branches _path _prompt-p)
                     (list :deleted (length branches) :skipped 0 :quit nil))))
          ;; Specific PRs clean
          (let ((res (magit-dash-mcp-call-tool
                      "clean_merged_worktrees"
                      (list :repo tmp-repo :pr_numbers [11 12]))))
            (should (= (plist-get res :deleted) 2))
            (should (equal (nreverse cleaned-entries) '(11 12))))
          ;; Dry-run prune_branches
          (let ((res (magit-dash-mcp-call-tool
                      "prune_branches"
                      (list :repo tmp-repo :dry_run t))))
            (should (eq (plist-get res :dry_run) t))
            (should (= (plist-get res :deleted_count) 0))
            (should (equal (plist-get res :candidate_branches) ["merged-a" "merged-b"])))
          ;; Actual prune_branches
          (let ((res (magit-dash-mcp-call-tool
                      "prune_branches"
                      (list :repo tmp-repo))))
            (should (eq (plist-get res :dry_run) :false))
            (should (= (plist-get res :deleted_count) 2))
            (should (= (plist-get res :worktrees_cleaned) 2))))
      (delete-directory tmp-repo t))))


(defun magit-dash-mcp-test--status-response ()
  "Return a GraphQL response with one PR (#7) holding one approval."
  `((data . ((search . ((nodes . (((number . 7) (title . "t") (headRefName . "b")
                                   (baseRefName . "main")
                                   (reviews . ((nodes . (((state . "APPROVED")
                                                          (author . ((login . "a")
                                                                     (__typename . "User"))))))))
                                   (reviewThreads . ((nodes . nil))))))))))))

(defun magit-dash-mcp-test--status-bucket (repo-list &optional all extra cache)
  "Run `magit_dash_pr_status' with REPO-LIST registered; return its result.
When ALL is non-nil omit `repo' so the author-wide collection is used."
  (let ((tmp-repo (make-temp-file "md-mcp-status-" t))
        (magit-dash-gh-pr--tracking-cache (or cache (make-hash-table :test #'equal))))
    (unwind-protect
        (let ((magit-dash-gh-pr-status-required-approvals 2)
              (magit-dash-repo-list repo-list))
          (cl-letf (((symbol-function 'magit-dash-mcp--resolve-repo-slug)
                     (lambda (_repo _dir) "org/repo"))
                    ((symbol-function 'magit-dash-gh-pr-status--fetch)
                     (lambda (_slug _dir) (magit-dash-mcp-test--status-response)))
                    ((symbol-function 'magit-dash-gh-pr-status--fetch-search)
                     (lambda (_q _dir) (magit-dash-mcp-test--status-response))))
            (magit-dash-mcp-call-tool
             "magit_dash_pr_status" (append (if all nil (list :repo tmp-repo)) extra))))
      (delete-directory tmp-repo t))))

(ert-deftest magit-dash-mcp/pr-status-tool ()
  "Test `magit_dash_pr_status' returns structured data without files."
  (let* ((res (magit-dash-mcp-test--status-bucket nil))
         (pr (aref (plist-get res :prs) 0)))
    (should (= 1 (plist-get res :count)))
    (should-not (plist-member res :file))
    (should (stringp (plist-get res :fingerprint)))
    ;; One approval of the default two required.
    (should (equal "reviews" (plist-get pr :bucket)))
    (should (= 7 (plist-get pr :number)))
    (should (= 1 (plist-get pr :approvals)))
    (should (vectorp (plist-get pr :failing)))
    (should (equal "new" (plist-get (plist-get pr :tracking) :delta_type)))
    (should (equal "needs 1 review" (plist-get (plist-get pr :tracking) :action_summary)))
    (should (stringp (json-serialize res)))))

(ert-deftest magit-dash-mcp/pr-status-tool-all-repos ()
  "Omitting `repo' uses the author-wide collection."
  (let ((res (magit-dash-mcp-test--status-bucket nil t)))
    (should (= 1 (plist-get res :count)))
    (should (eq :null (plist-get res :repo)))))

(ert-deftest magit-dash-mcp/pr-status-tool-per-repo-override ()
  "A per-repo `:pr' override changes the reported bucket."
  (let ((res (magit-dash-mcp-test--status-bucket
              (list (magit-dash-repo--make :name "m" :path "/tmp/m" :repo "org/repo"
                                           :pr '(:required-approvals 1))))))
    (should (= 1 (plist-get res :count)))
    (should (equal "ready" (plist-get (aref (plist-get res :prs) 0) :bucket)))))

(ert-deftest magit-dash-mcp/pr-status-tool-commit-flag ()
  "Default call leaves the tracking cache unchanged; `commit' t updates it."
  (let ((cache (make-hash-table :test #'equal)))
    (let ((res (magit-dash-mcp-test--status-bucket nil t nil cache)))
      (should (= 0 (hash-table-count cache)))
      (should (equal "new" (plist-get (plist-get (aref (plist-get res :prs) 0) :tracking)
                                      :delta_type))))
    ;; Repeat inspection still reports "new" since nothing was consumed.
    (let ((res (magit-dash-mcp-test--status-bucket nil t (list :commit :false) cache)))
      (should (= 0 (hash-table-count cache)))
      (should (equal "new" (plist-get (plist-get (aref (plist-get res :prs) 0) :tracking)
                                      :delta_type))))
    (magit-dash-mcp-test--status-bucket nil t (list :commit t) cache)
    (should (= 1 (hash-table-count cache)))
    (let ((res (magit-dash-mcp-test--status-bucket nil t nil cache)))
      (should-not (equal "new" (plist-get (plist-get (aref (plist-get res :prs) 0) :tracking)
                                          :delta_type))))))

(ert-deftest magit-dash-mcp/fingerprint-nil-fields-and-json ()
  "Fingerprint tolerates nil :repo/:state; omitted-repo result serializes."
  (should (stringp (magit-dash-gh-pr-tracking-fingerprint
                    (list (list :number 1 :repo nil :state nil :bucket nil)))))
  (let* ((res (magit-dash-mcp-test--status-bucket nil t))
         (json (json-serialize res)))
    (should (stringp json))
    (should (string-match-p "\"repo\":null" json))
    (let ((parsed (json-parse-string json :object-type 'plist)))
      (should (eq :null (plist-get parsed :repo))))
    (let ((tool-schema '(:type "object" :properties (:repo (:type "string")) :required [])))
      (should (string-match-p "\"required\":\\[\\]" (json-serialize tool-schema))))))

(ert-deftest magit-dash-mcp/pr-threads-tool ()
  "Test `magit_dash_pr_threads' bounds, untrusted flag, and injection safety."
  (let ((mock-graphql
         '((data
            (repository
             (pullRequest
              (reviewThreads
               (nodes
                . [((id . "TH_UNRESOLVED")
                    (isResolved . :json-false)
                    (isOutdated . :json-false)
                    (path . "lisp/foo.el")
                    (line . 15)
                    (comments
                     (nodes
                      . [((id . "C1")
                          (author (login . "reviewer1"))
                          (body . "* Injected Org Heading\n:PROPERTIES:\n:PR: evil\n:END:\nPlease fix this.")
                          (createdAt . "2026-11-10T10:00:00Z"))
                         ((id . "C2")
                          (author (login . "author1"))
                          (body . "This is a very long comment that exceeds the specified bound for testing truncation.")
                          (createdAt . "2026-11-10T11:00:00Z"))])))
                   ((id . "TH_RESOLVED")
                    (isResolved . t)
                    (isOutdated . :json-false)
                    (path . "lisp/bar.el")
                    (line . 20)
                    (comments
                     (nodes
                      . [((id . "C3")
                          (author (login . "reviewer2"))
                          (body . "Already addressed.")
                          (createdAt . "2026-11-09T08:00:00Z"))])))]))))))))
    (cl-letf (((symbol-function 'magit-dash-gh--call-process)
               (lambda (_args _dir)
                 (cons 0 (json-serialize mock-graphql :false-object :json-false :null-object nil)))))
      ;; Test basic fetch and structure
      (let* ((res (magit-dash-mcp-call-tool
                   "magit_dash_pr_threads"
                   (list :repo "owner/repo" :number 42 :limit 10 :max_chars 20)))
             (threads (plist-get res :threads))
             (json (json-serialize res :false-object :false :null-object nil)))
        (should (equal "owner/repo" (plist-get res :repo)))
        (should (= 42 (plist-get res :number)))
        (should (eq t (plist-get res :untrusted)))
        (should (= 1 (plist-get res :count)))
        (should (= 1 (length threads)))
        ;; Only unresolved thread is included
        (let ((th (aref threads 0)))
          (should (equal "TH_UNRESOLVED" (plist-get th :id)))
          (should (equal "lisp/foo.el" (plist-get th :path)))
          (should (equal "reviewer1" (plist-get th :author)))
          (should (eq :false (plist-get th :outdated)))
          ;; Truncation check
          (should (string-suffix-p "..." (plist-get th :body)))
          (should (<= (length (plist-get th :body)) 20))
          ;; Comments
          (should (= 2 (length (plist-get th :comments))))
          (should (string-suffix-p "..." (plist-get (aref (plist-get th :comments) 1) :body))))
        ;; Injection check: stays completely valid JSON
        (should (stringp json))
        (should (string-match-p "\\* Injected Org" json))))))

(ert-deftest magit-dash-mcp/pr-threads-tool-errors ()
  "Test `magit_dash_pr_threads' handles network and parameter errors safely."
  ;; Missing repo
  (let ((res (magit-dash-mcp-call-tool "magit_dash_pr_threads" (list :number 42))))
    (should (plist-get res :error)))
  ;; Invalid number
  (let ((res (magit-dash-mcp-call-tool "magit_dash_pr_threads" (list :repo "owner/repo" :number 0))))
    (should (plist-get res :error)))
  ;; Network error returns (:error ...)
  (cl-letf (((symbol-function 'magit-dash-gh--call-process)
             (lambda (_args _dir) (cons 1 "GraphQL request failed"))))
    (let ((res (magit-dash-mcp-call-tool "magit_dash_pr_threads"
                                         (list :repo "owner/repo" :number 42))))
      (should (plist-get res :error))
      (should (string-match-p "Failed to fetch review threads" (plist-get res :error))))))

(ert-deftest magit-dash-mcp/pr-ci-failures-tool ()
  "Test `magit_dash_pr_ci_failures' failing checks, log tail bounding, and untrusted flag."
  (let ((mock-pr-view
         '((statusCheckRollup
            . [((name . "test-suite")
                (conclusion . "FAILURE")
                (detailsUrl . "https://github.com/owner/repo/actions/runs/98765/job/111"))
               ((name . "lint")
                (conclusion . "SUCCESS")
                (detailsUrl . "https://github.com/owner/repo/actions/runs/98765/job/222"))
               ((name . "typecheck")
                (conclusion . "TIMED_OUT")
                (detailsUrl . "https://ci.external.com/build/123"))])
           (headRefName . "feature-branch")))
        (mock-log (mapconcat (lambda (i) (format "Log line %d: error occurred" i))
                             (number-sequence 1 300)
                             "\n")))
    (cl-letf (((symbol-function 'magit-dash-gh--call-process)
               (lambda (args _dir)
                 (cond
                  ((member "statusCheckRollup,headRefName" args)
                   (cons 0 (json-serialize mock-pr-view :false-object :json-false :null-object nil)))
                  ((member "--log-failed" args)
                   (cons 0 mock-log))
                  (t (cons 1 "unexpected call"))))))
      (let* ((res (magit-dash-mcp-call-tool
                   "magit_dash_pr_ci_failures"
                   (list :repo "owner/repo" :number 50 :tail_lines 50)))
             (failing (plist-get res :failing_checks))
             (json (json-serialize res :false-object :false :null-object nil)))
        (should (equal "owner/repo" (plist-get res :repo)))
        (should (= 50 (plist-get res :number)))
        (should (eq t (plist-get res :untrusted)))
        (should (= 98765 (plist-get res :run_id)))
        (should (= 2 (length failing)))
        (should (equal "test-suite" (plist-get (aref failing 0) :name)))
        (should (equal "failure" (plist-get (aref failing 0) :conclusion)))
        (should (equal "typecheck" (plist-get (aref failing 1) :name)))
        (should (equal "timed_out" (plist-get (aref failing 1) :conclusion)))
        ;; Log tail bounding
        (should (= 50 (plist-get res :log_lines)))
        (should (string-match-p "Log line 300: error occurred" (plist-get res :log_tail)))
        (should-not (string-match-p "Log line 200: error occurred" (plist-get res :log_tail)))
        ;; Serializes to valid JSON
        (should (stringp json))))))

(ert-deftest magit-dash-mcp/pr-ci-failures-tool-errors ()
  "Test `magit_dash_pr_ci_failures' handles errors gracefully without signaling."
  ;; Missing repo
  (let ((res (magit-dash-mcp-call-tool "magit_dash_pr_ci_failures" (list :number 10))))
    (should (plist-get res :error)))
  ;; Network failure
  (cl-letf (((symbol-function 'magit-dash-gh--call-process)
             (lambda (_args _dir) (cons 1 "gh pr view not found"))))
    (let ((res (magit-dash-mcp-call-tool "magit_dash_pr_ci_failures"
                                         (list :repo "owner/repo" :number 10))))
      (should (plist-get res :error)))))

(provide 'test-magit-dash-mcp)

;;; test-magit-dash-mcp.el ends here
