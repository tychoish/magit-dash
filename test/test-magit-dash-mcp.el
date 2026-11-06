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
    (should (gethash "magit_dash_pr_status" tools))))

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

(defun magit-dash-mcp-test--status-bucket (repo-list)
  "Run `magit_dash_pr_status' with REPO-LIST registered; return its result."
  (let ((tmp-repo (make-temp-file "md-mcp-status-" t))
        (file (make-temp-file "md-mcp-status" nil ".org")))
    (unwind-protect
        (let ((magit-dash-gh-pr-status-file file)
              (magit-dash-gh-pr-status-required-approvals 2)
              (magit-dash-repo-list repo-list))
          (cl-letf (((symbol-function 'magit-dash-mcp--resolve-repo-slug)
                     (lambda (_repo _dir) "org/repo"))
                    ((symbol-function 'magit-dash-gh-pr-status--fetch)
                     (lambda (_slug _dir) (magit-dash-mcp-test--status-response))))
            (let ((res (magit-dash-mcp-call-tool
                        "magit_dash_pr_status" (list :repo tmp-repo))))
              (should (equal file (plist-get res :file)))
              (should (string-match-p "^:PR: 7$"
                                      (with-temp-buffer (insert-file-contents file)
                                                        (buffer-string))))
              res)))
      (delete-directory tmp-repo t)
      (delete-file file))))

(ert-deftest magit-dash-mcp/pr-status-tool ()
  "Test `magit_dash_pr_status' regenerates the file and reports buckets."
  (let ((res (magit-dash-mcp-test--status-bucket nil)))
    (should (= 1 (plist-get res :count)))
    ;; One approval of the default two required.
    (should (equal "reviews" (plist-get (aref (plist-get res :prs) 0) :bucket)))))

(ert-deftest magit-dash-mcp/pr-status-tool-per-repo-override ()
  "A per-repo `:pr' override changes the reported bucket."
  (let ((res (magit-dash-mcp-test--status-bucket
              (list (magit-dash-repo--make :name "m" :path "/tmp/m" :repo "org/repo"
                                           :pr '(:required-approvals 1))))))
    (should (= 1 (plist-get res :count)))
    (should (equal "ready" (plist-get (aref (plist-get res :prs) 0) :bucket)))))

(provide 'test-magit-dash-mcp)

;;; test-magit-dash-mcp.el ends here
