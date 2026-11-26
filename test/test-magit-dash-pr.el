;;; test-magit-dash-pr.el --- ERT tests for magit-dash-pr -*- lexical-binding: t; no-byte-compile: t; -*-

;; Run inside a live Emacs session:
;;   (ert "^magit-dash-pr/")
;;
;; Batch run:
;;   emacs --batch -l test/test-helper.el \
;;     -l test/test-magit-dash-pr.el \
;;     --eval '(ert-run-tests-batch-and-exit "magit-dash-pr/")'

(require 'ert)
(require 'cl-lib)
(require 'map)
(require 'magit-dash)
(require 'magit-dash-pr)
(require 'magit-dash-worktree-clean)
(require 'magit-dash-gh-actions)

;;; Test helpers

(defmacro magit-dash-pr-test/with-temp-dir (&rest body)
  "Execute BODY with `default-directory' set to a fresh temp directory."
  (declare (indent 0))
  `(let ((dir (make-temp-file "magit-dash-pr-test-" t)))
     (unwind-protect
         (let ((default-directory dir))
           ,@body)
       (delete-directory dir t))))

(defun magit-dash-pr-test/make-thread (id resolved path creator last-commentor)
  "Return a fake GraphQL review thread node alist."
  `((id . ,id)
    (isResolved . ,(if resolved t :false))
    (path . ,path)
    (comments . ((nodes . (((author . ((login . ,creator)))
                             (body . "first comment")
                             (createdAt . "2026-01-01T00:00:00Z"))
                            ((author . ((login . ,last-commentor)))
                             (body . "last comment")
                             (createdAt . "2026-01-02T00:00:00Z"))))))))

(defun magit-dash-pr-test/graphql-response (threads)
  "Wrap THREADS in a fake GraphQL response alist."
  `((data . ((repository . ((pullRequest . ((reviewThreads . ((nodes . ,threads)))))))))))


;;; magit-dash-pr--parse-threads


(ert-deftest magit-dash-pr/parse-threads-empty ()
  (let* ((response (magit-dash-pr-test/graphql-response nil))
         (threads (magit-dash-pr--parse-threads response)))
    (should (null threads))))

(ert-deftest magit-dash-pr/parse-threads-resolved ()
  (let* ((node (magit-dash-pr-test/make-thread "t1" t "src/foo.el" "alice" "bob"))
         (response (magit-dash-pr-test/graphql-response (list node)))
         (threads (magit-dash-pr--parse-threads response)))
    (should (= 1 (length threads)))
    (let ((th (car threads)))
      (should (equal "t1"         (plist-get th :id)))
      (should (eq    t            (plist-get th :resolved)))
      (should (equal "src/foo.el" (plist-get th :path)))
      (should (equal "alice"      (plist-get th :creator)))
      (should (equal "bob"        (plist-get th :last-commentor))))))

(ert-deftest magit-dash-pr/parse-threads-unresolved ()
  (let* ((node (magit-dash-pr-test/make-thread "t2" nil "lisp/bar.el" "charlie" "dave"))
         (response (magit-dash-pr-test/graphql-response (list node)))
         (threads (magit-dash-pr--parse-threads response)))
    (let ((th (car threads)))
      (should (eq nil (plist-get th :resolved))))))

(ert-deftest magit-dash-pr/parse-threads-multiple ()
  (let* ((nodes (list
                 (magit-dash-pr-test/make-thread "t1" t   "a.el" "u1" "u2")
                 (magit-dash-pr-test/make-thread "t2" nil "b.el" "u3" "u4")
                 (magit-dash-pr-test/make-thread "t3" t   "c.el" "u5" "u5")))
         (response (magit-dash-pr-test/graphql-response nodes))
         (threads (magit-dash-pr--parse-threads response)))
    (should (= 3 (length threads)))
    (should (eq t   (plist-get (nth 0 threads) :resolved)))
    (should (eq nil (plist-get (nth 1 threads) :resolved)))
    (should (eq t   (plist-get (nth 2 threads) :resolved)))))

(ert-deftest magit-dash-pr/parse-threads-creator-from-first-comment ()
  "Creator comes from the first comment's author."
  (let* ((node (magit-dash-pr-test/make-thread "t1" nil "x.el" "firstauthor" "lastauthor"))
         (response (magit-dash-pr-test/graphql-response (list node)))
         (threads (magit-dash-pr--parse-threads response)))
    (should (equal "firstauthor" (plist-get (car threads) :creator)))
    (should (equal "lastauthor"  (plist-get (car threads) :last-commentor)))))

;;; magit-dash-pr--thread-table

(ert-deftest magit-dash-pr/thread-table-resolved ()
  (let* ((th  '(:id "t1" :resolved t :path "x.el" :creator "a" :last-commentor "b"))
         (ht  (magit-dash-pr--thread-table th)))
    (should (equal "t1"  (map-elt ht "id")))
    (should (eq    t     (map-elt ht "resolved")))
    (should (equal "x.el" (map-elt ht "path")))
    (should (equal "a"   (map-elt ht "creator")))
    (should (equal "b"   (map-elt ht "last_commentor")))))

(ert-deftest magit-dash-pr/thread-table-unresolved ()
  (let* ((th '(:id "t2" :resolved nil :path "" :creator "c" :last-commentor "d"))
         (ht (magit-dash-pr--thread-table th)))
    (should (eq :false (map-elt ht "resolved")))))


;;; has_unresolved_threads derivation

(ert-deftest magit-dash-pr/has-unresolved-all-resolved ()
  (let ((threads '((:resolved t) (:resolved t))))
    (should-not (seq-some (lambda (th) (not (plist-get th :resolved))) threads))))

(ert-deftest magit-dash-pr/has-unresolved-one-unresolved ()
  (let ((threads '((:resolved t) (:resolved nil) (:resolved t))))
    (should (seq-some (lambda (th) (not (plist-get th :resolved))) threads))))

(ert-deftest magit-dash-pr/has-unresolved-empty ()
  (should-not (seq-some (lambda (th) (not (plist-get th :resolved))) nil)))

;;; Pipeline: step-finalize index structure

(ert-deftest magit-dash-pr/step-finalize-writes-index ()
  (magit-dash-pr-test/with-temp-dir
    (let* ((pr-info `((number . 42)
                      (title . "Fix the thing")
                      (state . "OPEN")
                      (author . ((login . "tychoish")))
                      (headRefName . "fix/the-thing")
                      (reviewDecision . "CHANGES_REQUESTED")))
           (threads (list '(:id "t1" :resolved nil :path "x.el"
                               :creator "alice" :last-commentor "bob")
                          '(:id "t2" :resolved t :path "y.el"
                               :creator "carol" :last-commentor "dave")))
           (ctx (list :dir dir
                      :pr-info pr-info
                      :threads threads
                      :files (list '(:path "pr-info.json" :type "metadata")
                                   '(:path "pr-review-threads.json" :type "review-threads")
                                   '(:path "pr-issue-comments.json" :type "issue-comments")))))
      (magit-dash-pr--step-finalize ctx)
      (should (file-exists-p (expand-file-name "index.json" dir)))
      (let* ((raw (with-temp-buffer
                    (insert-file-contents (expand-file-name "index.json" dir))
                    (buffer-string)))
             (index (json-parse-string raw :object-type 'alist)))
        (should (equal "pr"                 (map-elt index 'type)))
        (should (= 42                       (map-elt index 'pr_number)))
        (should (equal "Fix the thing"      (map-elt index 'title)))
        (should (equal "OPEN"               (map-elt index 'state)))
        (should (equal "tychoish"           (map-elt index 'author)))
        (should (eq    t                    (map-elt index 'has_unresolved_threads)))
        (should (= 3                        (map-elt index 'artifact_count)))
        (should (= 2                        (length (map-elt index 'threads))))))))

(ert-deftest magit-dash-pr/step-finalize-all-resolved ()
  "When all threads are resolved, has_unresolved_threads is :false."
  (magit-dash-pr-test/with-temp-dir
    (let* ((pr-info `((number . 1) (title . "Done") (state . "MERGED")
                      (author . ((login . "alice"))) (headRefName . "main")
                      (reviewDecision . "APPROVED")))
           (threads '((:resolved t) (:resolved t)))
           (ctx (list :dir dir :pr-info pr-info :threads threads :files nil)))
      (magit-dash-pr--step-finalize ctx)
      (let* ((raw (with-temp-buffer
                    (insert-file-contents (expand-file-name "index.json" dir))
                    (buffer-string)))
             (index (json-parse-string raw :object-type 'alist)))
        (should (eq :false (map-elt index 'has_unresolved_threads)))))))

(ert-deftest magit-dash-pr/step-finalize-no-threads ()
  "When there are no threads, has_unresolved_threads is :false."
  (magit-dash-pr-test/with-temp-dir
    (let* ((pr-info `((number . 2) (title . "Simple") (state . "OPEN")
                      (author . ((login . "bob"))) (headRefName . "feat")
                      (reviewDecision . nil)))
           (ctx (list :dir dir :pr-info pr-info :threads nil :files nil)))
      (magit-dash-pr--step-finalize ctx)
      (let* ((raw (with-temp-buffer
                    (insert-file-contents (expand-file-name "index.json" dir))
                    (buffer-string)))
             (index (json-parse-string raw :object-type 'alist)))
        (should (eq :false (map-elt index 'has_unresolved_threads)))))))

;;; Pipeline: step-threads mocking

(ert-deftest magit-dash-pr/step-threads-calls-gh ()
  "step-threads invokes gh api graphql with correct args."
  (magit-dash-pr-test/with-temp-dir
    (let* ((gh-args nil)
           (fake-graphql-output
            (json-serialize
             (let ((ht (make-hash-table :test #'equal)))
               (puthash "data"
                        (let ((h2 (make-hash-table :test #'equal)))
                          (puthash "repository"
                                   (let ((h3 (make-hash-table :test #'equal)))
                                     (puthash "pullRequest"
                                              (let ((h4 (make-hash-table :test #'equal)))
                                                (puthash "reviewThreads"
                                                         (let ((h5 (make-hash-table :test #'equal)))
                                                           (puthash "nodes" [] h5)
                                                           h5)
                                                         h4)
                                                h4)
                                              h3)
                                     h3)
                                   h2)
                          h2)
                        ht)
               ht)))
           (comments-called nil)
           (ctx (list :dir dir :owner "owner" :repo "repo" :pr-number 5
                      :repo-dir dir :threads nil :files nil :pr-info nil)))
      (cl-letf (((symbol-function 'magit-dash-gh--run-process)
                 (lambda (args _dir on-success &optional _on-error)
                   (setq gh-args args)
                   (funcall on-success fake-graphql-output)))
                ((symbol-function 'magit-dash-pr--step-comments)
                 (lambda (_ctx) (setq comments-called t))))
        (magit-dash-pr--step-threads ctx)
        (should comments-called)
        (should (member "graphql" gh-args))
        (should (file-exists-p (expand-file-name "pr-review-threads.json" dir)))))))

;;; magit-dash-pr-dashboard--find-local-path

(defvar magit-dash-repo-list)

(defun magit-dash-pr-test/make-repos (&rest path-pairs)
  "Return a list of magit-dash-repo structs from PATH-PAIRS (name path ...)."
  (let (repos)
    (while path-pairs
      (push (magit-dash-repo--make :name (pop path-pairs) :path (pop path-pairs)) repos))
    (nreverse repos)))

(ert-deftest magit-dash-pr/find-local-path-match ()
  "Returns path when repo name matches a registered entry's directory."
  (let ((magit-dash-repo-list
         (magit-dash-pr-test/make-repos "test-1" "/home/user/projects/myrepo"
                                      "test-2" "/home/user/projects/other-repo")))
    (should (equal "/home/user/projects/myrepo"
                   (magit-dash-pr-dashboard--find-local-path "owner/myrepo")))))

(ert-deftest magit-dash-pr/find-local-path-no-match ()
  "Returns nil when no registered entry matches."
  (let ((magit-dash-repo-list
         (magit-dash-pr-test/make-repos "test" "/home/user/projects/myrepo")))
    (should-not (magit-dash-pr-dashboard--find-local-path "owner/unknown"))))

(ert-deftest magit-dash-pr/find-local-path-empty-list ()
  "Returns nil when magit-dash-repo-list is empty."
  (let ((magit-dash-repo-list nil))
    (should-not (magit-dash-pr-dashboard--find-local-path "owner/repo"))))

;;; magit-dash-pr-dashboard-fetch-ci

(ert-deftest magit-dash-pr/dashboard-fetch-ci-calls-ci-fetch ()
  "fetch-ci extracts pr-number and local path, then calls magit-dash-gh-actions-fetch-for-pr."
  (let ((fetch-args nil))
    (cl-letf (((symbol-function 'magit-dash-pr-dashboard--entry-at-point)
               (lambda () '(:number 42 :repo "owner/myrepo" :pr nil)))
              ((symbol-function 'magit-dash-pr-dashboard--find-local-path)
               (lambda (_) "/repos/myrepo"))
              ((symbol-function 'magit-dash-gh-actions-fetch-for-pr)
               (lambda (num path) (setq fetch-args (list num path)))))
      (magit-dash-pr-dashboard-fetch-ci)
      (should (equal '(42 "/repos/myrepo") fetch-args)))))

(ert-deftest magit-dash-pr/dashboard-fetch-ci-no-local-path ()
  "fetch-ci signals user-error when no local checkout is registered."
  (cl-letf (((symbol-function 'magit-dash-pr-dashboard--entry-at-point)
             (lambda () '(:number 5 :repo "owner/unknown-repo" :pr nil)))
            ((symbol-function 'magit-dash-pr-dashboard--find-local-path)
             (lambda (_) nil)))
    (should-error (magit-dash-pr-dashboard-fetch-ci) :type 'user-error)))

;;; magit-dash-pr and worktree operations

(ert-deftest magit-dash-pr/dash-creates-buffer ()
  "magit-dash-pr creates *magit-dash-pr* buffer."
  (cl-letf (((symbol-function 'magit-dash-gh--check-gh) #'ignore)
            ((symbol-function 'magit-dash-pr-dashboard-refresh) #'ignore))
    (unwind-protect
        (let ((buf (get-buffer-create magit-dash-pr-buffer-name)))
          (magit-dash-pr)
          (should (equal (buffer-name (get-buffer magit-dash-pr-buffer-name)) "*magit-dash-pr*"))
          (with-current-buffer buf
            (should (eq major-mode 'magit-dash-pr-dashboard-mode))))
      (when-let* ((buf (get-buffer magit-dash-pr-buffer-name)))
        (kill-buffer buf)))))

(ert-deftest magit-dash-pr/default-filters-aggregate-all-open ()
  "magit-dash-pr-dashboard-mode sets default filters with :author nil to aggregate all open PRs."
  (with-temp-buffer
    (magit-dash-pr-dashboard-mode)
    (let ((filters magit-dash-pr-dashboard--filters))
      (should (equal "open" (plist-get filters :state)))
      (should-not (plist-get filters :author)))))

(ert-deftest magit-dash-pr/open-worktree-opens-existing ()
  "open-worktree switches directly to existing worktree."
  (let ((setup-path nil))
    (cl-letf (((symbol-function 'magit-dash-pr-own-p) (lambda (&rest _) t))
              ((symbol-function 'magit-dash-pr-dashboard--entry-at-point)
               (lambda () '(:number 42 :repo "owner/myrepo" :pr nil)))
              ((symbol-function 'magit-dash-pr-dashboard--find-local-path)
               (lambda (_) "/tmp/myrepo"))
              ((symbol-function 'file-directory-p)
               (lambda (dir) (string-suffix-p ".wt/pr-42" dir)))
              ((symbol-function 'magit-status-setup-buffer)
               (lambda (path) (setq setup-path path))))
      (magit-dash-pr-dashboard-open-worktree)
      (should (equal (expand-file-name "/tmp/myrepo/.wt/pr-42") setup-path)))))

(ert-deftest magit-dash-pr/open-worktree-fetches-and-creates ()
  "open-worktree runs git fetch and git worktree add when worktree does not yet exist."
  (let ((git-calls nil)
        (setup-path nil))
    (cl-letf (((symbol-function 'magit-dash-pr-own-p) (lambda (&rest _) t))
              ((symbol-function 'magit-dash-pr-dashboard--entry-at-point)
               (lambda () '(:number 42 :repo "owner/myrepo" :pr nil)))
              ((symbol-function 'magit-dash-pr-dashboard--find-local-path)
               (lambda (_) "/tmp/myrepo"))
              ((symbol-function 'file-directory-p) (lambda (_) nil))
              ((symbol-function 'magit-call-git)
               (lambda (&rest args) (push args git-calls) 0))
              ((symbol-function 'magit-status-setup-buffer)
               (lambda (path) (setq setup-path path))))
      (magit-dash-pr-dashboard-open-worktree)
      (should (equal (expand-file-name "/tmp/myrepo/.wt/pr-42") setup-path))
      ;; Verify git fetch and git worktree add calls
      (should (seq-some (lambda (call)
                          (and (equal (nth 0 call) "fetch")
                               (equal (nth 1 call) "origin")
                               (equal (nth 2 call) "pull/42/head:pr-42")))
                        git-calls))
      (should (seq-some (lambda (call)
                          (and (equal (nth 0 call) "worktree")
                               (equal (nth 1 call) "add")
                               (equal (nth 2 call) (expand-file-name "/tmp/myrepo/.wt/pr-42"))
                               (equal (nth 3 call) "pr-42")))
                        git-calls)))))
;;; magit-dash-pr-dashboard--build-format

(ert-deftest magit-dash-pr/build-format-caps-repo-width-with-entries ()
  "Repo column width is capped at the longest repo name + 3."
  (let* ((entries '(((:repo "owner/repo-short") ["repo-short" "1" "pass" "Title" "·" "1d" "0" "approved"])
                    ((:repo "owner/a-much-longer-repository-name") ["a-much-longer-repository-name" "2" "pass" "Title" "·" "1d" "0" "approved"])))
         (fmt (magit-dash-pr-dashboard--build-format 150 entries)))
    (should (equal "Repo" (car (aref fmt 0))))
    (should (= (+ (length "a-much-longer-repository-name") 3)
               (cadr (aref fmt 0))))))

(ert-deftest magit-dash-pr/build-format-show-org ()
  "When show-org is non-nil, build-format includes Org and Repo columns, followed by PR#, CI, Title, WT, Updated."
  (let* ((magit-dash-pr-show-org t)
         (magit-dash-pr-columns (append '((author . nil)) magit-dash-pr-columns))
         (entries '(((:repo "casaphq/my-repo") ["casaphq" "my-repo" "1" "pass" "Title" "·" "1d" "0" "approved"])))
         (fmt (magit-dash-pr-dashboard--build-format 150 entries)))
    (should (equal "Org" (car (aref fmt 0))))
    (should (equal "Repo" (car (aref fmt 1))))
    (should (equal "PR#" (car (aref fmt 2))))
    (should (equal "CI" (car (aref fmt 3))))
    (should (equal "Title" (car (aref fmt 4))))
    (should (equal "WT" (car (aref fmt 5))))
    (should (equal "Updated" (car (aref fmt 6))))))

(ert-deftest magit-dash-pr/build-format-fallback-no-entries ()
  "When no entries exist, Repo column falls back to available window space (min 12)."
  (let* ((magit-dash-pr-columns (append '((author . nil)) magit-dash-pr-columns))
         (fmt (magit-dash-pr-dashboard--build-format 100 nil)))
    (should (= (- 100 77 9) (cadr (aref fmt 0))))))

(ert-deftest magit-dash-pr/split-slug ()
  "Split slug correctly extracts owner and repo."
  (should (equal '("owner" . "repo") (magit-dash-pr--split-slug "owner/repo")))
  (should (equal '("" . "standalone") (magit-dash-pr--split-slug "standalone"))))

(ert-deftest magit-dash-pr/worktree-detection ()
  "Worktree detection checks local checkout and worktree directory."
  (let ((magit-dash-repo-list (magit-dash-pr-test/make-repos "test-repo" "/tmp/test-repo")))
    (cl-letf (((symbol-function 'magit-dash-pr-own-p) (lambda (&rest _) t)))
      (should (equal (expand-file-name "/tmp/test-repo/.wt/pr-10")
                     (magit-dash-pr-dashboard--worktree-path '(:number 10 :repo "owner/test-repo")))))
    (cl-letf (((symbol-function 'magit-dash-pr-own-p) (lambda (&rest _) t))
              ((symbol-function 'file-directory-p)
               (lambda (path) (string-suffix-p ".wt/pr-10" path))))
      (should (magit-dash-pr-dashboard--worktree-exists-p '(:number 10 :repo "owner/test-repo")))
      (should-not (magit-dash-pr-dashboard--worktree-exists-p '(:number 11 :repo "owner/test-repo"))))))

(ert-deftest magit-dash-pr/open-dired ()
  "open-dired opens Dired in worktree directory."
  (let ((opened-dir nil))
    (cl-letf (((symbol-function 'magit-dash-pr-dashboard--entry-at-point)
               (lambda () '(:number 42 :repo "owner/myrepo" :pr nil)))
              ((symbol-function 'magit-dash-pr-dashboard--worktree-path)
               (lambda (_) "/tmp/myrepo-pr-42"))
              ((symbol-function 'file-directory-p)
               (lambda (_) t))
              ((symbol-function 'dired)
               (lambda (d) (setq opened-dir d))))
      (magit-dash-pr-dashboard-open-dired)
      (should (equal "/tmp/myrepo-pr-42" opened-dir)))))


;;; magit-dash-pr--parse-status-rollup

(ert-deftest magit-dash-pr/parse-status-rollup-nil ()
  "Empty or nil rollup returns nil."
  (should-not (magit-dash-pr--parse-status-rollup nil))
  (should-not (magit-dash-pr--parse-status-rollup []))
  (should-not (magit-dash-pr--parse-status-rollup '())))

(ert-deftest magit-dash-pr/parse-status-rollup-legacy-state ()
  "Single alist with state field returns that state."
  (should (equal "SUCCESS" (magit-dash-pr--parse-status-rollup '((state . "SUCCESS")))))
  (should (equal "FAILURE" (magit-dash-pr--parse-status-rollup '((state . "failure"))))))

(ert-deftest magit-dash-pr/parse-status-rollup-success ()
  "All successful check runs produce SUCCESS."
  (let ((rollup (list '((status . "COMPLETED") (conclusion . "SUCCESS"))
                      '((status . "COMPLETED") (conclusion . "SKIPPED")))))
    (should (equal "SUCCESS" (magit-dash-pr--parse-status-rollup rollup)))))

(ert-deftest magit-dash-pr/parse-status-rollup-failure-priority ()
  "Failure takes priority over pending or success."
  (let ((rollup (list '((status . "COMPLETED") (conclusion . "SUCCESS"))
                      '((status . "IN_PROGRESS") (conclusion . ""))
                      '((status . "COMPLETED") (conclusion . "FAILURE")))))
    (should (equal "FAILURE" (magit-dash-pr--parse-status-rollup rollup)))))

(ert-deftest magit-dash-pr/parse-status-rollup-pending-priority ()
  "Pending takes priority over success when there are no failures."
  (let ((rollup (list '((status . "COMPLETED") (conclusion . "SUCCESS"))
                      '((status . "IN_PROGRESS") (conclusion . "")))))
    (should (equal "PENDING" (magit-dash-pr--parse-status-rollup rollup)))))

(ert-deftest magit-dash-pr/refresh-does-not-clear-entries-while-fetching ()
  "Calling `magit-dash-pr-dashboard-refresh' does not clear `tabulated-list-entries' before fetch completes."
  (with-temp-buffer
    (magit-dash-pr-dashboard-mode)
    (setq tabulated-list-entries
          (list (magit-dash-pr-dashboard--build-entry '((number . 1)) "o/r")))
    (cl-letf (((symbol-function 'magit-dash-gh--run-process)
               (lambda (_args _dir _on-success &optional _on-error)
                 nil)))
      (magit-dash-pr-dashboard-refresh)
      (should (= 1 (length tabulated-list-entries))))))


;;; Round 3: View switcher, window split, and quit tests

(ert-deftest magit-dash-pr/build-args-review-requested ()
  "build-args includes --review-requested when specified."
  (let ((args (magit-dash-pr-dashboard--build-args
               (list :state "open" :review-requested "@me"))))
    (should (member "--review-requested" args))
    (should (member "@me" args))))

(ert-deftest magit-dash-pr/switch-view-presets ()
  "switch-view updates filters according to preset view keys."
  (let ((refreshed nil)
        (magit-dash-pr-dashboard--filters nil))
    (cl-letf (((symbol-function 'magit-dash-pr-dashboard-refresh)
               (lambda () (setq refreshed t))))
      ;; 1. my-prs
      (magit-dash-pr-dashboard-switch-view "my-prs")
      (should (equal "@me" (plist-get magit-dash-pr-dashboard--filters :author)))
      (should (equal "open" (plist-get magit-dash-pr-dashboard--filters :state)))
      (should refreshed)
      ;; 2. needs-review
      (setq refreshed nil)
      (magit-dash-pr-dashboard-switch-view "needs-review")
      (should (equal "@me" (plist-get magit-dash-pr-dashboard--filters :review-requested)))
      ;; 3. closed
      (magit-dash-pr-dashboard-switch-view "closed")
      (should (equal "closed" (plist-get magit-dash-pr-dashboard--filters :state))))))

(ert-deftest magit-dash-pr/quit-window-deletes-split ()
  "quit buries buffer and deletes window when multiple windows exist."
  (let ((buried nil)
        (deleted nil))
    (cl-letf (((symbol-function 'window-list)
               (lambda (&rest _) '(win1 win2)))
              ((symbol-function 'bury-buffer)
               (lambda (&rest _) (setq buried t)))
              ((symbol-function 'delete-window)
               (lambda (&rest _) (setq deleted t))))
      (magit-dash-pr-dashboard-quit)
      (should buried)
      (should deleted))))


;;; Column toggling tests

(ert-deftest magit-dash-pr/columns-defaults ()
  "PR dashboard has expected default columns enabled, disabled, and ordered."
  (should (eq nil (alist-get 'org magit-dash-pr-columns)))
  (should (eq t (alist-get 'repo magit-dash-pr-columns)))
  (should (eq t (alist-get 'pr magit-dash-pr-columns)))
  (should (eq t (alist-get 'ci magit-dash-pr-columns)))
  (should (eq t (alist-get 'title magit-dash-pr-columns)))
  (should (eq t (alist-get 'worktree magit-dash-pr-columns)))
  (should (eq t (alist-get 'updated magit-dash-pr-columns)))
  (should (equal '(repo pr author ci title worktree updated comments review approvals threads)
                 (magit-dash-pr--active-columns)))
  (let ((was-on (bound-and-true-p magit-dash-global-mode)))
    (unwind-protect
        (progn
          (unless was-on (magit-dash-global-mode 1))
          (should (memq 'magit-dash-pr-columns savehist-additional-variables)))
      (unless was-on
        (magit-dash-global-mode -1)))))

(ert-deftest magit-dash-pr/toggle-column ()
  "Toggling a column flips its enabled state and syncs show-org."
  (let ((magit-dash-pr-columns
         '((org . nil) (repo . t) (pr . t) (ci . t) (title . t) (worktree . t) (updated . t) (age . nil) (comments . t) (review . t)))
        (magit-dash-pr-show-org nil))
    (cl-letf (((symbol-function 'magit-dash-pr-dashboard-refresh) #'ignore))
      (magit-dash-pr-toggle-column 'org)
      (should (eq t (magit-dash-pr--column-enabled-p 'org)))
      (should (eq t magit-dash-pr-show-org))
      (magit-dash-pr-toggle-column 'ci)
      (should-not (magit-dash-pr--column-enabled-p 'ci)))))

(ert-deftest magit-dash-pr/build-entry-respects-arbitrary-columns ()
  "build-entry outputs only the active columns."
  (let ((magit-dash-pr-columns
         '((org . nil) (repo . t) (pr . t) (author . nil) (base . nil) (ci . nil) (title . nil) (worktree . nil) (updated . nil) (age . nil) (comments . nil) (review . nil) (approvals . nil) (threads . nil)))
        (pr '((number . 10) (title . "T") (updatedAt . "") (comments . 0))))
    (let* ((entry (magit-dash-pr-dashboard--build-entry pr "owner/repo"))
           (vec (cadr entry)))
      (should (= 2 (length vec)))
      (should (equal "repo" (aref vec 0)))
      (should (equal "10" (aref vec 1))))))


(ert-deftest magit-dash-pr/worktree-path-format-options ()
  "Test custom worktree path formatting options: sibling, dot-wt, and custom string."
  (let ((entry '(:number 42 :repo "owner/myrepo" :pr nil)))
    (cl-letf (((symbol-function 'magit-dash-pr-own-p) (lambda (&rest _) t))
              ((symbol-function 'magit-dash-pr-dashboard--find-local-path)
               (lambda (_) "/tmp/myrepo"))
              ((symbol-function 'file-directory-p) (lambda (_) nil)))
      ;; Default: sibling
      (let ((magit-dash-pr-worktree-path-format 'sibling))
        (should (equal (expand-file-name "/tmp/myrepo-pr-42")
                       (magit-dash-pr-dashboard--worktree-path entry))))
      ;; dot-wt
      (let ((magit-dash-pr-worktree-path-format 'dot-wt))
        (should (equal (expand-file-name "/tmp/myrepo/.wt/pr-42")
                       (magit-dash-pr-dashboard--worktree-path entry))))
      ;; Custom string template
      (let ((magit-dash-pr-worktree-path-format "%r/.worktrees/%n"))
        (should (equal (expand-file-name "/tmp/myrepo/.worktrees/pr-42")
                       (magit-dash-pr-dashboard--worktree-path entry)))))))

(ert-deftest magit-dash-pr/open-worktree-mirrors-remote-branch ()
  "open-worktree uses headRefName as branch name while keeping worktree name as pr-NUM."
  (let ((git-calls nil)
        (setup-path nil))
    (cl-letf (((symbol-function 'magit-dash-pr-own-p) (lambda (&rest _) t))
              ((symbol-function 'magit-dash-pr-dashboard--entry-at-point)
               (lambda () '(:number 42 :repo "owner/myrepo"
                            :pr ((headRefName . "feature/cool-idea")))))
              ((symbol-function 'magit-dash-pr-dashboard--find-local-path)
               (lambda (_) "/tmp/myrepo"))
              ((symbol-function 'file-directory-p) (lambda (_) nil))
              ((symbol-function 'magit-call-git)
               (lambda (&rest args) (push args git-calls) 0))
              ((symbol-function 'magit-status-setup-buffer)
               (lambda (path) (setq setup-path path))))
      (let ((magit-dash-pr-worktree-path-format 'dot-wt))
        (magit-dash-pr-dashboard-open-worktree)
        (should (equal (expand-file-name "/tmp/myrepo/.wt/pr-42") setup-path))
        ;; Verify fetch used remote branch name
        (should (seq-some (lambda (call)
                            (and (equal (nth 0 call) "fetch")
                                 (equal (nth 1 call) "origin")
                                 (equal (nth 2 call) "pull/42/head:feature/cool-idea")))
                          git-calls))
        ;; Verify worktree add checked out that branch into wt directory
        (should (seq-some (lambda (call)
                            (and (equal (nth 0 call) "worktree")
                                 (equal (nth 1 call) "add")
                                 (equal (nth 2 call) (expand-file-name "/tmp/myrepo/.wt/pr-42"))
                                 (equal (nth 3 call) "feature/cool-idea")))
                          git-calls))))))

(ert-deftest magit-dash-pr/worktree-detection-finds-dot-wt ()
  "worktree-exists-p detects existing worktrees in .wt directory."
  (let ((entry '(:number 10 :repo "owner/test-repo" :pr nil)))
    (cl-letf (((symbol-function 'magit-dash-pr-dashboard--find-local-path)
               (lambda (_) "/tmp/test-repo"))
              ((symbol-function 'file-directory-p)
               (lambda (path)
                 (member (directory-file-name path)
                         (list (expand-file-name "/tmp/test-repo")
                               (expand-file-name "/tmp/test-repo/.wt/pr-10"))))))
      (should (magit-dash-pr-dashboard--worktree-exists-p entry)))))

(ert-deftest magit-dash-pr/build-entry-updated-column ()
  "build-entry populates columns in order: repo, pr, ci, title, worktree, updated, comments, review."
  (let* ((pr '((number . 15)
               (title . "Fix stuff")
               (updatedAt . "2024-06-01T12:00:00Z")
               (headRefName . "bugfix/issue-15")
               (comments . 1)
               (reviewDecision . nil)
               (statusCheckRollup . ((state . "FAILURE")))
               (url . "https://example.com/15")))
         (magit-dash-pr-columns (append '((author . nil)) magit-dash-pr-columns))
         (entry (magit-dash-pr-dashboard--build-entry pr "owner/myrepo"))
         (id (car entry))
         (vec (cadr entry)))
    ;; id should store :updated and :head-ref
    (should (equal "2024-06-01T12:00:00Z" (plist-get id :updated)))
    (should (equal "bugfix/issue-15" (plist-get id :head-ref)))
    ;; PR# column is aref 1 -> "15"
    (should (equal "15" (substring-no-properties (aref vec 1))))
    ;; CI column is aref 2 -> "x"
    (should (equal "x" (substring-no-properties (aref vec 2))))
    ;; Title column is aref 3 -> "Fix stuff"
    (should (equal "Fix stuff" (substring-no-properties (aref vec 3))))
    ;; WT column is aref 4 -> "·"
    (should (equal "·" (substring-no-properties (aref vec 4))))
    ;; Updated column is aref 5
    (should-not (string-empty-p (aref vec 5)))))


;;; Worktree commands & review tests

(ert-deftest magit-dash-pr/dashboard-create-worktree-no-switch ()
  "dashboard-create-worktree creates worktree without opening magit status when switch is nil."
  (let ((git-calls nil)
        (setup-called nil))
    (cl-letf (((symbol-function 'magit-dash-pr-own-p) (lambda (&rest _) t))
              ((symbol-function 'magit-dash-pr-dashboard--entry-at-point)
               (lambda () '(:number 42 :repo "owner/myrepo" :pr nil)))
              ((symbol-function 'magit-dash-pr-dashboard--find-local-path)
               (lambda (_) "/tmp/myrepo"))
              ((symbol-function 'file-directory-p) (lambda (_) nil))
              ((symbol-function 'magit-call-git)
               (lambda (&rest args) (push args git-calls) 0))
              ((symbol-function 'magit-status-setup-buffer)
               (lambda (_) (setq setup-called t))))
      (let ((res (magit-dash-pr-dashboard-create-worktree nil nil)))
        (should (equal (expand-file-name "/tmp/myrepo/.wt/pr-42") res))
        (should-not setup-called)
        (should (seq-some (lambda (call)
                            (and (equal (nth 0 call) "worktree")
                                 (equal (nth 1 call) "add")))
                          git-calls))))))

(ert-deftest magit-dash-pr/dashboard-prefix-keys-are-documented-submaps ()
  "The c, m, v and f prefixes are named sub-maps with docs and plain keys."
  (seq-do (lambda (spec)
            (progn
              (should (eq (keymap-lookup magit-dash-pr-dashboard-mode-map (car spec))
                          (symbol-value (cdr spec))))
              (should (documentation-property (cdr spec) 'variable-documentation))
              (map-keymap (lambda (event _cmd)
                            (should (characterp event))
                            (should-not (/= 0 (logand event (lognot #x3fffff)))))
                          (symbol-value (cdr spec)))))
          '(("c" . magit-dash-pr-dashboard-review-map)
            ("m" . magit-dash-pr-dashboard-mark-map)
            ("v" . magit-dash-pr-dashboard-view-map)
            ("f" . magit-dash-pr-dashboard-filter-map))))

(ert-deftest magit-dash-pr/dashboard-mode-map-keys ()
  "magit-dash-pr-dashboard-mode-map uses two-letter combinations for cw, ca, ci and mark keys."
  (let ((m magit-dash-pr-dashboard-mode-map))
    (should (eq (lookup-key m (kbd "c w")) #'magit-dash-pr-dashboard-create-worktree))
    (should (eq (lookup-key m (kbd "c a")) #'magit-dash-pr-dashboard-create-all-worktrees))
    (should (eq (lookup-key m (kbd "c i")) #'magit-dash-pr-dashboard-fetch-ci))
    (should (eq (lookup-key m (kbd "c r")) #'magit-dash-pr-dashboard-diff-review))
    (seq-do (lambda (available)
              (unwind-protect
                  (cl-letf (((symbol-function 'magit-dash-pr-code-review-available-p)
                             (lambda () available)))
                    (magit-dash-pr-dashboard--setup-code-review-keys)
                    (should (eq (and available #'magit-dash-pr-dashboard-code-review)
                                (lookup-key m (kbd "c b")))))
                (magit-dash-pr-dashboard--setup-code-review-keys)))
            '(t nil))
    (should (eq (lookup-key m (kbd "b"))   #'magit-dash-pr-dashboard-open-browser))
    (should (eq (lookup-key m (kbd "m m")) #'magit-dash-pr-dashboard-menu))
    (should (eq (lookup-key m (kbd "m t")) #'magit-dash-pr-dashboard-toggle-mark))
    (should (eq (lookup-key m (kbd "m u")) #'magit-dash-pr-dashboard-unmark))
    (should (eq (lookup-key m (kbd "m c")) #'magit-dash-pr-dashboard-unmark-all))
    (should (eq (lookup-key m (kbd "m a")) #'magit-dash-pr-dashboard-mark-all))
    (should (eq (lookup-key m (kbd "SPC")) #'magit-dash-pr-dashboard-toggle-mark))
    (should (eq (lookup-key m (kbd "*")) #'magit-dash-pr-dashboard-mark-all))
    (should (eq (lookup-key m (kbd "a")) #'magit-dash-pr-dashboard-create-all-worktrees))))

(ert-deftest magit-dash-pr/dashboard-menu-includes-worktree-options ()
  "magit-dash-pr-dashboard-menu contains create, create-all, review, clean, and mark commands."
  (let ((layout (get 'magit-dash-pr-dashboard-menu 'transient--layout)))
    (should layout)
    (let ((flat (format "%S" layout))
          (case-fold-search nil))
      (should (string-match-p "magit-dash-pr-dashboard-code-review" flat))
      (should (string-match-p "magit-dash-pr-dashboard-create-worktree" flat))
      (should (string-match-p "magit-dash-pr-dashboard-create-all-worktrees" flat))
      (should (string-match-p "magit-dash-pr-dashboard-review-worktrees" flat))
      (should (string-match-p "magit-dash-pr-dashboard-clean-merged-worktrees" flat))
      (should (string-match-p "magit-dash-pr-dashboard-toggle-mark" flat))
      (should (string-match-p "magit-dash-pr-dashboard-unmark-all" flat))
      (should (string-match-p ":key \"wm\" :description \"Resume chain\" :command magit-dash-chain-resume" flat))
      (should-not (string-match-p ":key \"wC\"" flat)))))

(ert-deftest magit-dash-pr/dashboard-menu-keys-bound-and-unique ()
  "The transient binds cr and cb to the review commands, and every key is unique."
  (let* ((flat (format "%S" (get 'magit-dash-pr-dashboard-menu 'transient--layout)))
         (case-fold-search nil)
         (keys nil)
         (start 0))
    (should (string-match-p ":key \"cr\" :description \"[^\"]*\" :command magit-dash-pr-dashboard-diff-review" flat))
    (should (string-match-p ":key \"cb\" :description \"[^\"]*\" :command magit-dash-pr-dashboard-code-review" flat))
    (should (string-match-p "magit-dash-pr-dashboard-code-review[^]]*:if magit-dash-pr-code-review-available-p" flat))
    (while (string-match ":key \"\\([^\"]+\\)\"" flat start)
      (push (match-string 1 flat) keys)
      (setq start (match-end 0)))
    (should keys)
    (should (equal (length keys) (length (delete-dups (copy-sequence keys)))))))

(ert-deftest magit-dash-pr/dashboard-pr-url-extraction ()
  "magit-dash-pr-dashboard--pr-url resolves URLs from strings, plists, and alists."
  (should (equal "https://github.com/foo/bar/pull/10"
                 (magit-dash-pr-dashboard--pr-url "https://github.com/foo/bar/pull/10")))
  ;; Entry with :pr alist having 'url
  (let ((entry '(:number 12 :repo "org/repo" :pr ((url . "https://github.com/org/repo/pull/12")))))
    (should (equal "https://github.com/org/repo/pull/12"
                   (magit-dash-pr-dashboard--pr-url entry))))
  ;; Entry with :pr plist having :url
  (let ((entry '(:number 15 :repo "org/repo" :pr (:url "https://github.com/org/repo/pull/15"))))
    (should (equal "https://github.com/org/repo/pull/15"
                   (magit-dash-pr-dashboard--pr-url entry))))
  ;; Entry without explicit url synthesizes from repo and number
  (let ((entry '(:number 20 :repo "myorg/cool-proj" :pr nil)))
    (should (equal "https://github.com/myorg/cool-proj/pull/20"
                   (magit-dash-pr-dashboard--pr-url entry)))))

(ert-deftest magit-dash-pr/dashboard-code-review-opens ()
  "magit-dash-pr-dashboard-code-review passes URL to code-review-start."
  (let ((called-url nil))
    (cl-letf (((symbol-function 'code-review-start)
               (lambda (url) (setq called-url url))))
      (magit-dash-pr-dashboard-code-review
       '(:number 42 :repo "tychoish/magit-dash" :pr ((url . "https://github.com/tychoish/magit-dash/pull/42"))))
      (should (equal "https://github.com/tychoish/magit-dash/pull/42" called-url)))))

(ert-deftest magit-dash-pr/dashboard-diff-review-opens ()
  "magit-dash-pr-dashboard-diff-review dispatches entry to magit-dash-review-pr."
  (let ((called-entry nil))
    (should-not (eq (lookup-key magit-dash-pr-dashboard-mode-map (kbd "V"))
                    #'magit-dash-pr-dashboard-diff-review))
    (should (eq (lookup-key magit-dash-pr-dashboard-mode-map (kbd "c v"))
                #'magit-dash-pr-dashboard-diff-review))
    (should (eq (lookup-key magit-dash-pr-dashboard-mode-map (kbd "v v"))
                #'magit-dash-pr-dashboard-diff-review))
    (cl-letf (((symbol-function 'magit-dash-review-pr)
               (lambda (ent &optional _dir) (setq called-entry ent))))
      (magit-dash-pr-dashboard-diff-review
       '(:number 42 :repo "tychoish/magit-dash"))
      (should (equal (plist-get called-entry :number) 42)))))

(ert-deftest magit-dash-pr/code-review-requires-url-and-package ()
  "magit-dash-pr-code-review validates URL and package availability."
  (should-error (magit-dash-pr-code-review "") :type 'user-error)
  (should-error (magit-dash-pr-code-review nil) :type 'user-error)
  (cl-letf (((symbol-function 'fboundp) (lambda (_) nil))
            ((symbol-function 'require) (lambda (&rest _) nil)))
    (should-error (magit-dash-pr-code-review "https://github.com/o/r/pull/1")
                  :type 'user-error)))

(ert-deftest magit-dash-pr/code-review-conditional-activation ()
  "Keybindings and menu for code-review are only active when package is available."
  (unwind-protect
      (progn
        ;; When unavailable
        (cl-letf (((symbol-function 'magit-dash-pr-code-review-available-p) (lambda () nil)))
          (magit-dash-pr-dashboard--setup-code-review-keys)
          (should-not (lookup-key magit-dash-pr-dashboard-mode-map (kbd "c b")))
          (should (eq (lookup-key magit-dash-pr-dashboard-mode-map (kbd "c r"))
                      #'magit-dash-pr-dashboard-diff-review)))
        ;; When available
        (cl-letf (((symbol-function 'magit-dash-pr-code-review-available-p) (lambda () t)))
          (magit-dash-pr-dashboard--setup-code-review-keys)
          (should (eq (lookup-key magit-dash-pr-dashboard-mode-map (kbd "c b"))
                      #'magit-dash-pr-dashboard-code-review))))
    (magit-dash-pr-dashboard--setup-code-review-keys)))

(ert-deftest magit-dash-pr/dashboard-menu-opens ()
  "The dashboard transient can set up its complete keymap."
  (with-temp-buffer
    (unwind-protect
        (progn
          (magit-dash-pr-dashboard-menu)
          (should (eq (lookup-key transient--transient-map (kbd "m c"))
                      #'magit-dash-pr-dashboard-unmark-all)))
      (when transient--prefix (transient-quit-all)))))

(ert-deftest magit-dash-pr/delete-worktree-refuses-on-uncommitted ()
  "delete-worktree refuses to remove worktree if uncommitted changes exist."
  (cl-letf (((symbol-function 'magit-dash-worktree-uncommitted-changes-p)
             (lambda (_) t)))
    (should-error (magit-dash-pr-delete-worktree "/tmp/myrepo/.wt/pr-42" "/tmp/myrepo")
                  :type 'user-error)))

(ert-deftest magit-dash-pr/delete-worktree-removes-clean ()
  "delete-worktree removes clean worktree via git worktree remove and prunes."
  (let ((git-calls nil))
    (cl-letf (((symbol-function 'magit-dash-worktree-uncommitted-changes-p)
               (lambda (_) nil))
              ((symbol-function 'call-process)
               (lambda (prog &rest args)
                 (when (equal prog "git")
                   (push args git-calls))
                 0)))
      (should (magit-dash-pr-delete-worktree "/tmp/myrepo/.wt/pr-42" "/tmp/myrepo"))
      ;; Check worktree remove call
      (should (seq-some (lambda (args)
                          (and (member "worktree" args)
                               (member "remove" args)
                               (member (expand-file-name "/tmp/myrepo/.wt/pr-42") args)))
                        git-calls))
      ;; Check worktree prune call
      (should (seq-some (lambda (args)
                          (and (member "worktree" args)
                               (member "prune" args)))
                        git-calls)))))

(ert-deftest magit-dash-pr/review-worktrees-prompts-and-renames-mismatched ()
  "review-worktrees prompts to rename worktree when path does not match PR format."
  (let ((git-calls nil)
        (prompted nil)
        (prs '(((number . 42) (headRefName . "feature-42") (state . "OPEN"))))
        (wts '(((path . "/tmp/myrepo-pr-42") (branch . "feature-42")))))
    (cl-letf (((symbol-function 'magit-dash-worktree-main-dir) (lambda (&optional d) (or d "/tmp/myrepo")))
              ((symbol-function 'magit-dash-worktree-risks) #'ignore)
              ((symbol-function 'magit-dash-worktree-risks) #'ignore)
              ((symbol-function 'magit-dash-pr-own-p) (lambda (&rest _) t))
              ((symbol-function 'magit-dash-pr--git-worktrees) (lambda (_) wts))
              ((symbol-function 'file-directory-p) (lambda (_) t))
              ((symbol-function 'magit-dash-worktree-uncommitted-changes-p) (lambda (_) nil))
              ((symbol-function 'y-or-n-p)
               (lambda (prompt)
                 (setq prompted prompt)
                 t))
              ((symbol-function 'call-process)
               (lambda (prog &rest args)
                 (when (equal prog "git")
                   (push args git-calls))
                 0)))
      (let ((res (magit-dash-pr-review-worktrees "/tmp/myrepo" t prs)))
        (should prompted)
        (should (= 1 (plist-get res :renamed)))
        (should (seq-some (lambda (args)
                            (and (member "worktree" args)
                                 (member "move" args)))
                          git-calls))))))

(ert-deftest magit-dash-pr/review-worktrees-skips-matching ()
  "review-worktrees counts matching worktree without prompt."
  (let ((prompted nil)
        (prs '(((number . 42) (headRefName . "feature-42") (state . "OPEN"))))
        (wts `(((path . ,(expand-file-name "/tmp/myrepo/.wt/pr-42")) (branch . "feature-42")))))
    (cl-letf (((symbol-function 'magit-dash-pr-own-p) (lambda (&rest _) t))
              ((symbol-function 'magit-dash-pr--git-worktrees) (lambda (_) wts))
              ((symbol-function 'file-directory-p) (lambda (_) t))
              ((symbol-function 'y-or-n-p) (lambda (_) (setq prompted t) t)))
      (let ((res (magit-dash-pr-review-worktrees "/tmp/myrepo" t prs)))
        (should-not prompted)
        (should (= 1 (plist-get res :matching)))
        (should (= 0 (plist-get res :renamed)))))))

(ert-deftest magit-dash-pr/review-worktrees-skips-merged-with-uncommitted ()
  "review-worktrees skips merged PR worktree if it has uncommitted changes."
  (let ((deleted nil)
        (prs '(((number . 42) (headRefName . "pr-42") (state . "MERGED"))))
        (wts `(((path . ,(expand-file-name "/tmp/myrepo/.wt/pr-42")) (branch . "pr-42")))))
    (cl-letf (((symbol-function 'magit-dash-pr--git-worktrees) (lambda (_) wts))
              ((symbol-function 'file-directory-p) (lambda (_) t))
              ((symbol-function 'magit-dash-worktree-uncommitted-changes-p) (lambda (_) t))
              ((symbol-function 'magit-dash-pr-delete-worktree) (lambda (&rest _) (setq deleted t))))
      (let ((res (magit-dash-pr-review-worktrees "/tmp/myrepo" t prs)))
        (should-not deleted)
        (should (= 1 (plist-get res :skipped)))
        (should (= 0 (plist-get res :deleted)))))))

(ert-deftest magit-dash-pr/review-worktrees-deletes-merged-clean ()
  "review-worktrees deletes clean worktree for merged PR."
  (let ((deleted nil)
        (prs '(((number . 42) (headRefName . "pr-42") (state . "MERGED"))))
        (wts `(((path . ,(expand-file-name "/tmp/myrepo/.wt/pr-42")) (branch . "pr-42")))))
    (cl-letf (((symbol-function 'magit-dash-worktree-risks) #'ignore)
              ((symbol-function 'magit-dash-pr--git-worktrees) (lambda (_) wts))
              ((symbol-function 'file-directory-p) (lambda (_) t))
              ((symbol-function 'magit-dash-worktree-uncommitted-changes-p) (lambda (_) nil))
              ((symbol-function 'y-or-n-p) (lambda (_) t))
              ((symbol-function 'magit-dash-pr-delete-worktree) (lambda (&rest _) (setq deleted t))))
      (let ((res (magit-dash-pr-review-worktrees "/tmp/myrepo" t prs)))
        (should deleted)
        (should (= 1 (plist-get res :deleted)))))))

(ert-deftest magit-dash-pr/pr-merged-p-handles-null-and-cases ()
  "magit-dash-pr--pr-merged-p handles :null mergedAt, lowercase state, and open PRs."
  ;; Open PR with :null mergedAt must NOT be considered merged
  (should-not (magit-dash-pr--pr-merged-p '((number . 1) (state . "OPEN") (mergedAt . :null))))
  ;; Closed PR with :null mergedAt and no gh fallback is not merged
  (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
    (should-not (magit-dash-pr--pr-merged-p '((number . 2) (state . "CLOSED") (mergedAt . :null)) "o/r" 2)))
  ;; Merged PR by state (uppercase or lowercase)
  (should (magit-dash-pr--pr-merged-p '((number . 3) (state . "MERGED") (mergedAt . :null))))
  (should (magit-dash-pr--pr-merged-p '((number . 4) (state . "merged"))))
  ;; Merged PR by mergedAt timestamp even when state is "closed" (from gh search prs)
  (should (magit-dash-pr--pr-merged-p '((number . 5) (state . "closed") (mergedAt . "2026-01-01T00:00:00Z")))))

(ert-deftest magit-dash-pr/clean-merged-worktrees ()
  "clean-merged-worktrees deletes clean merged worktrees and skips dirty ones."
  (let ((deleted-paths nil)
        (prs '(((number . 1) (headRefName . "pr-1") (state . "MERGED"))
               ((number . 2) (headRefName . "pr-2") (state . "MERGED"))
               ((number . 3) (headRefName . "pr-3") (state . "OPEN") (mergedAt . :null))))
        (wts `(((path . ,(expand-file-name "/tmp/myrepo/.wt/pr-1")) (branch . "pr-1"))
               ((path . ,(expand-file-name "/tmp/myrepo/.wt/pr-2")) (branch . "pr-2"))
               ((path . ,(expand-file-name "/tmp/myrepo/.wt/pr-3")) (branch . "pr-3")))))
    (cl-letf (((symbol-function 'magit-dash-worktree-risks) #'ignore)
              ((symbol-function 'magit-dash-pr--fetch-repo-prs) (lambda (&rest _) prs))
              ((symbol-function 'magit-dash-pr--git-worktrees) (lambda (_) wts))
              ((symbol-function 'file-directory-p) (lambda (_) t))
              ((symbol-function 'magit-dash-worktree-uncommitted-changes-p)
               (lambda (path) (string-suffix-p "pr-2" path)))
              ((symbol-function 'y-or-n-p) (lambda (_) t))
              ((symbol-function 'magit-dash-pr-delete-worktree)
               (lambda (path &rest _) (push path deleted-paths))))
      (let ((res (magit-dash-pr-clean-merged-worktrees "/tmp/myrepo" t)))
        (should (equal (list (expand-file-name "/tmp/myrepo/.wt/pr-1")) deleted-paths))
        (should (= 1 (plist-get res :deleted)))
        (should (= 1 (plist-get res :skipped)))))))

(ert-deftest magit-dash-pr/clean-merged-protects-open-pr-on-reused-branch ()
  "An open PR protects its worktree even when an older PR was merged."
  (let ((prs '(((number . 1) (headRefName . "shared") (state . "MERGED"))
               ((number . 2) (headRefName . "shared") (state . "OPEN"))))
        (wts '(((path . "/tmp/wt") (branch . "shared"))))
        deleted)
    (cl-letf (((symbol-function 'magit-dash-pr--fetch-repo-prs) (lambda (&rest _) prs))
              ((symbol-function 'magit-dash-pr--git-worktrees) (lambda (_) wts))
              ((symbol-function 'file-directory-p) (lambda (_) t))
              ((symbol-function 'magit-dash-pr-delete-worktree) (lambda (&rest _) (setq deleted t))))
      (magit-dash-pr-clean-merged-worktrees "/tmp/repo" nil)
      (should-not deleted))))

(ert-deftest magit-dash-pr/clean-merged-looks-up-older-numbered-worktrees ()
  "Look up numbered worktrees absent from the recent PR list."
  (let ((wts '(((path . "/tmp/repo/.wt/pr-42") (branch . "old-branch"))
               ((path . "/tmp/repo/.wt/pr-43") (branch . "unknown"))))
        deleted)
    (cl-letf (((symbol-function 'magit-dash-worktree-risks) #'ignore)
              ((symbol-function 'magit-dash-pr--fetch-repo-prs) (lambda (&rest _) nil))
              ((symbol-function 'magit-dash-pr--fetch-pr)
               (lambda (_repo number _dir)
                 (when (= number 42) '((number . 42) (headRefName . "old-branch") (state . "MERGED")))))
              ((symbol-function 'magit-dash-pr--git-worktrees) (lambda (_) wts))
              ((symbol-function 'file-directory-p) (lambda (_) t))
              ((symbol-function 'magit-dash-worktree-uncommitted-changes-p) (lambda (_) nil))
              ((symbol-function 'magit-dash-pr-delete-worktree)
               (lambda (path &rest _) (push path deleted))))
      (let ((counts (magit-dash-pr-clean-merged-worktrees "/tmp/repo" nil)))
        (should (equal deleted '("/tmp/repo/.wt/pr-42")))
        (should (= 1 (plist-get counts :deleted)))
        (should (= 1 (plist-get counts :skipped)))))))

(ert-deftest magit-dash-pr/clean-merged-looks-up-pr-deps-named-worktrees ()
  "A worktree named pr-deps-N is resolved by its trailing number."
  (let ((wts '(((path . "/tmp/repo/.wt/pr-deps-11137") (branch . "dependabot/x"))))
        deleted)
    (cl-letf (((symbol-function 'magit-dash-worktree-risks) #'ignore)
              ((symbol-function 'magit-dash-pr--fetch-repo-prs) (lambda (&rest _) nil))
              ((symbol-function 'magit-dash-pr--fetch-pr)
               (lambda (_repo number _dir)
                 (when (= number 11137) '((number . 11137) (headRefName . "dependabot/x") (state . "MERGED")))))
              ((symbol-function 'magit-dash-pr--git-worktrees) (lambda (_) wts))
              ((symbol-function 'file-directory-p) (lambda (_) t))
              ((symbol-function 'magit-dash-worktree-uncommitted-changes-p) (lambda (_) nil))
              ((symbol-function 'magit-dash-pr-delete-worktree)
               (lambda (path &rest _) (push path deleted))))
      (magit-dash-pr-clean-merged-worktrees "/tmp/repo" nil)
      (should (equal deleted '("/tmp/repo/.wt/pr-deps-11137"))))))

(ert-deftest magit-dash-pr/clean-merged-matches-by-branch-not-dir-name ()
  "The worktree branch, not its directory name, selects the PR."
  (let ((wts '(((path . "/tmp/repo/.wt/whatever") (branch . "dependabot/x"))))
        deleted)
    (cl-letf (((symbol-function 'magit-dash-worktree-risks) #'ignore)
              ((symbol-function 'magit-dash-pr--fetch-repo-prs) (lambda (&rest _) nil))
              ((symbol-function 'magit-dash-pr--fetch-prs-for-branch)
               (lambda (_repo branch _dir)
                 (when (equal branch "dependabot/x")
                   '(((number . 11137) (headRefName . "dependabot/x") (state . "MERGED"))))))
              ((symbol-function 'magit-dash-pr--worktree-pr-branch)
               (lambda (wt) (alist-get 'branch wt)))
              ((symbol-function 'magit-dash-pr--git-worktrees) (lambda (_) wts))
              ((symbol-function 'file-directory-p) (lambda (_) t))
              ((symbol-function 'magit-dash-worktree-uncommitted-changes-p) (lambda (_) nil))
              ((symbol-function 'magit-dash-pr-delete-worktree)
               (lambda (path &rest _) (push path deleted))))
      (magit-dash-pr-clean-merged-worktrees "/tmp/repo" nil)
      (should (equal deleted '("/tmp/repo/.wt/whatever"))))))

(ert-deftest magit-dash-pr/worktree-pr-branch-prefers-upstream ()
  "The tracking branch name (remote stripped) wins over the local name."
  (let* ((dir (make-temp-file "wt" t))
         (default-directory dir))
    (unwind-protect
        (progn
          (call-process "git" nil nil nil "init" "-q" "-b" "local-name")
          (call-process "git" nil nil nil "-c" "user.name=t" "-c" "user.email=t@t"
                        "commit" "-q" "--allow-empty" "-m" "x")
          (call-process "git" nil nil nil "update-ref" "refs/remotes/origin/feature/up" "HEAD")
          (call-process "git" nil nil nil "config" "remote.origin.url" "/nonexistent")
          (call-process "git" nil nil nil "config" "remote.origin.fetch"
                        "+refs/heads/*:refs/remotes/origin/*")
          (call-process "git" nil nil nil "config" "branch.local-name.remote" "origin")
          (call-process "git" nil nil nil "config" "branch.local-name.merge" "refs/heads/feature/up")
          (should (equal "feature/up"
                         (magit-dash-pr--worktree-pr-branch
                          `((path . ,dir) (branch . "local-name")))))
          (should (equal "x" (magit-dash-pr--worktree-pr-branch
                              `((path . "/nonexistent") (branch . "wt/x"))))))
      (delete-directory dir t))))

(ert-deftest magit-dash-pr/clean-entry-skips-open-pr ()
  "Marked open PRs are never deleted by merged cleanup."
  (let ((entry '(:number 2 :repo "o/r" :pr ((number . 2) (state . "OPEN"))))
        deleted)
    (cl-letf (((symbol-function 'magit-dash-worktree-risks) #'ignore)
              ((symbol-function 'magit-dash-pr-dashboard--find-local-path) (lambda (_) "/tmp/repo"))
              ((symbol-function 'magit-dash-pr--find-corroborated-worktree)
               (lambda (&rest _) '((path . "/tmp/wt"))))
              ((symbol-function 'file-directory-p) (lambda (_) t))
              ((symbol-function 'magit-dash-pr-delete-worktree) (lambda (&rest _) (setq deleted t))))
      (should (eq :skipped (magit-dash-pr--clean-worktree-for-entry entry nil)))
      (should-not deleted))))

(ert-deftest magit-dash-pr/create-all-worktrees-respects-view-and-confirmation ()
  "Only view entries without worktrees are proposed and created after confirmation."
  (with-temp-buffer
    (magit-dash-pr-dashboard-mode)
    (setq magit-dash-pr-dashboard--filters '(:repo "o/r" :author "@me"))
    (setq tabulated-list-entries
          '(((:repo "o/r" :number 10 :pr ((headRefName . "existing"))) [])
            ((:repo "o/r" :number 20 :pr ((headRefName . "feature"))) [])))
    (let (created proposed (confirm nil)
          (magit-dash-pr--pending-worktrees (make-hash-table :test #'equal)))
      (cl-letf (((symbol-function 'magit-dash-pr-dashboard--find-local-path) (lambda (_) "/tmp"))
                ((symbol-function 'magit-dash-pr--find-existing-worktree)
                 (lambda (_dir number &rest _) (when (= number 10) "/tmp/existing")))
                ((symbol-function 'magit-dash-pr--fetch-repo-prs)
                 (lambda (&rest _) (ert-fail "Must not fetch repository PRs")))
                ((symbol-function 'magit-dash-pr-dashboard-refresh)
                 (lambda () (ert-fail "Must not refresh the view")))
                ((symbol-function 'magit-dash-pr-dashboard--rebuild-entries) #'ignore)
                ((symbol-function 'yes-or-no-p)
                 (lambda (_prompt)
                   (setq proposed (with-current-buffer "*PR worktrees to create*" (buffer-string)))
                   confirm))
                ((symbol-function 'magit-dash-pr--create-batch-async)
                 (lambda (targets &rest _)
                   (setq created (mapcar (lambda (target) (plist-get (car target) :number)) targets)))))
        (magit-dash-pr-create-all-worktrees)
        (should-not created)
        (should (string-match-p "o/r #20  feature" proposed))
        (should-not (string-match-p "o/r #10" proposed))
        (setq confirm t)
        (let ((result (magit-dash-pr-dashboard-create-all-worktrees)))
          (should (equal created '(20)))
          (should (= 1 (plist-get result :queued)))
          (should (= 1 (plist-get result :existing))))))))

(ert-deftest magit-dash-pr/batch-creates-after-fetch-and-clears-pending ()
  (let* ((entry '(:repo "o/r" :number 20 :pr ((headRefName . "feature"))))
         (magit-dash-pr--pending-worktrees (make-hash-table :test #'equal))
         calls callback)
    (puthash '("o/r" . 20) t magit-dash-pr--pending-worktrees)
    (cl-letf (((symbol-function 'magit-dash-pr-dashboard--find-local-path) (lambda (_) "/tmp"))
              ((symbol-function 'magit-dash-pr--find-existing-worktree) (lambda (&rest _) nil))
              ((symbol-function 'magit-dash-pr-dashboard--rebuild-entries) #'ignore)
              ((symbol-function 'magit-dash-pr--compute-worktree-path) (lambda (&rest _) "/tmp/pr-20"))
              ((symbol-function 'magit-dash-worktree-symlink-worktree) #'ignore)
              ((symbol-function 'magit-dash-pr--run-git-async)
               (lambda (_dir args cb) (push args calls) (setq callback cb))))
      (magit-dash-pr--create-batch-async (list (cons entry "feature")) (current-buffer) 0 0)
      (should (equal (caar calls) "fetch"))
      (should (gethash '("o/r" . 20) magit-dash-pr--pending-worktrees))
      (funcall callback 0 "")
      (should (equal (car calls) '("worktree" "add" "/tmp/pr-20" "feature")))
      (funcall callback 0 "")
      (should-not (gethash '("o/r" . 20) magit-dash-pr--pending-worktrees)))))

(ert-deftest magit-dash-pr/batch-fetch-failure-does-not-add-worktree ()
  (let ((magit-dash-pr--pending-worktrees (make-hash-table :test #'equal))
        calls callback)
    (puthash '("o/r" . 20) t magit-dash-pr--pending-worktrees)
    (cl-letf (((symbol-function 'magit-dash-pr-dashboard--find-local-path) (lambda (_) "/tmp"))
              ((symbol-function 'magit-dash-pr--find-existing-worktree) (lambda (&rest _) nil))
              ((symbol-function 'magit-dash-pr-dashboard--rebuild-entries) #'ignore)
              ((symbol-function 'magit-dash-pr--run-git-async)
               (lambda (_dir args cb) (push args calls) (setq callback cb))))
      (magit-dash-pr--create-batch-async
       (list (cons '(:repo "o/r" :number 20) "feature")) (current-buffer) 0 0)
      (funcall callback 1 "fetch failed")
      (should (= 1 (length calls)))
      (should-not (gethash '("o/r" . 20) magit-dash-pr--pending-worktrees)))))

(ert-deftest magit-dash-pr/fetch-pr-uses-account-aware-runner ()
  (let (called)
    (cl-letf (((symbol-function 'executable-find) (lambda (_) t))
              ((symbol-function 'magit-dash-gh--call-process)
               (lambda (args dir)
                 (setq called (list args dir))
                 '(0 . "{\"number\":42,\"state\":\"MERGED\"}"))))
      (should (equal (alist-get 'number (magit-dash-pr--fetch-pr "o/r" 42 "/tmp/")) 42))
      (should (equal (cadr called) "/tmp/"))
      (should (equal (seq-take (car called) 3) '("pr" "view" "42"))))))

(ert-deftest magit-dash-pr/create-all-worktrees-requires-dashboard ()
  (with-temp-buffer
    (should-error (magit-dash-pr-create-all-worktrees) :type 'user-error)))

(ert-deftest magit-dash-pr/match-worktree-to-pr ()
  "Test matching worktrees to pull requests by branch and directory."
  (let ((prs '(((number . 10) (headRefName . "feat/login"))
               ((number . 20) (headRefName . "fix-bug"))
               ((number . 30) (headRefName . "other")))))
    (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t)))
      ;; Match by branch
      (let ((matched (magit-dash-pr--match-worktree-to-pr
                      '((path . "/tmp/repo/.wt/custom") (branch . "feat/login"))
                      prs)))
        (should (= 10 (alist-get 'number matched))))

      ;; Match by directory name pr-NUM
      (let ((matched (magit-dash-pr--match-worktree-to-pr
                      '((path . "/tmp/repo/.wt/pr-20") (branch . "detached"))
                      prs)))
        (should (= 20 (alist-get 'number matched))))

      ;; No match
      (let ((matched (magit-dash-pr--match-worktree-to-pr
                      '((path . "/tmp/repo/.wt/random") (branch . "unknown"))
                      prs)))
        (should-not matched)))))

(ert-deftest magit-dash-pr/path-equal-p ()
  "Test path equality comparisons."
  (should (magit-dash-pr--path-equal-p "/tmp/foo/bar" "/tmp/foo/bar/"))
  (should (magit-dash-pr--path-equal-p "/tmp/foo/bar/" "/tmp/foo/bar"))
  (should-not (magit-dash-pr--path-equal-p "/tmp/foo/bar" "/tmp/foo/baz"))
  (should-not (magit-dash-pr--path-equal-p nil "/tmp/foo/bar"))
  (should-not (magit-dash-pr--path-equal-p "/tmp/foo/bar" nil)))

(ert-deftest magit-dash-pr/move-worktree ()
  "Test git worktree move invocation and symlink updating."
  (let ((git-calls nil)
        (symlinked nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (_program _in _dest _display &rest args)
                 (push args git-calls)
                 0))
              ((symbol-function 'file-exists-p) (lambda (_) nil))
              ((symbol-function 'magit-dash-worktree-symlink-worktree)
               (lambda (wt-dir repo-dir) (setq symlinked (list wt-dir repo-dir)))))
      (let ((res (magit-dash-pr--move-worktree "/tmp/repo" "/tmp/repo/old" "/tmp/repo/new")))
        (should res)
        (should (equal (list (expand-file-name "/tmp/repo/new") (expand-file-name "/tmp/repo"))
                       symlinked))
        (should (seq-some (lambda (call) (and (member "worktree" call) (member "move" call)))
                          git-calls))))))

(ert-deftest magit-dash-pr/dashboard-create-worktree-renames-mismatched ()
  "dashboard-create-worktree prompts and renames existing worktree at non-matching path."
  (let* ((entry '(:number 42 :repo "org/myrepo" :pr ((number . 42) (headRefName . "feature"))))
         (moved nil)
         (prompted nil))
    (cl-letf (((symbol-function 'magit-dash-pr-own-p) (lambda (&rest _) t))
              ((symbol-function 'magit-dash-pr-dashboard--find-local-path)
               (lambda (_) "/tmp/myrepo"))
              ((symbol-function 'file-directory-p) (lambda (_) t))
              ((symbol-function 'magit-dash-pr--find-existing-worktree)
               (lambda (&rest _) "/tmp/myrepo/other-dir"))
              ((symbol-function 'y-or-n-p)
               (lambda (_) (setq prompted t) t))
              ((symbol-function 'magit-dash-pr--move-worktree)
               (lambda (_repo old new) (setq moved (list old new)) t)))
      (let ((res (magit-dash-pr-dashboard-create-worktree entry nil)))
        (should prompted)
        (should (equal (list (expand-file-name "/tmp/myrepo/other-dir")
                             (expand-file-name "/tmp/myrepo/.wt/pr-42"))
                       moved))
        (should (equal (expand-file-name "/tmp/myrepo/.wt/pr-42") res))))))

(ert-deftest magit-dash-pr/dashboard-review-and-clean-commands ()
  "Test dashboard interactive wrappers for review and clean worktrees."
  (let ((reviewed nil)
        (cleaned nil)
        (entry '(:number 1 :repo "org/myrepo")))
    (cl-letf (((symbol-function 'magit-dash-pr-dashboard--entry-at-point)
               (lambda () entry))
              ((symbol-function 'magit-dash-pr-dashboard--find-local-path)
               (lambda (_) "/tmp/myrepo"))
              ((symbol-function 'magit-dash-pr-review-worktrees)
               (lambda (path prompt) (setq reviewed (list path prompt))))
              ((symbol-function 'magit-dash-worktree-clean-plan)
               (lambda (path) (setq cleaned path)))
              ((symbol-function 'magit-dash-pr-dashboard-refresh) #'ignore))
      (magit-dash-pr-dashboard-review-worktrees)
      (should (equal (list "/tmp/myrepo" t) reviewed))

      (magit-dash-pr-dashboard-clean-merged-worktrees)
      (should (equal "/tmp/myrepo" cleaned)))))

(ert-deftest magit-dash-pr/dashboard-clean-without-marks-plans-repo-scan ()
  "With nothing marked, cleaning plans the whole repository instead of prompting."
  (let ((planned nil))
    (cl-letf (((symbol-function 'magit-dash-pr-dashboard--entry-at-point)
               (lambda () '(:number 42 :repo "org/myrepo")))
              ((symbol-function 'magit-dash-pr-dashboard--find-local-path)
               (lambda (_) "/tmp/myrepo"))
              ((symbol-function 'magit-dash-pr-dashboard--marked-or-at-point-entries)
               (lambda () nil))
              ((symbol-function 'magit-dash-worktree-clean-plan)
               (lambda (path) (setq planned path))))
      (magit-dash-pr-dashboard-clean-merged-worktrees)
      (should (equal "/tmp/myrepo" planned)))))

(ert-deftest magit-dash-pr/mark-and-bulk-operations ()
  "Test marking, unmarking, mark-all, unmark-all, and bulk create/clean worktree operations."
  (with-temp-buffer
    (magit-dash-pr-dashboard-mode)
    (let* ((pr1 '((number . 10) (title . "First PR") (updatedAt . "") (comments . 0)))
           (pr2 '((number . 20) (title . "Second PR") (updatedAt . "") (comments . 0)))
           (e1 (magit-dash-pr-dashboard--build-entry pr1 "org/repo1"))
           (e2 (magit-dash-pr-dashboard--build-entry pr2 "org/repo2")))
      (setq tabulated-list-entries (list e1 e2))
      (tabulated-list-print t)
      (goto-char (point-min))
      ;; Toggle mark on first entry
      (magit-dash-pr-dashboard-toggle-mark)
      (should (equal '(("org/repo1" . 10)) magit-dash-pr-dashboard--marked-entries))
      ;; Verify visual indicator "*" on PR# column (index 1)
      (should (string-prefix-p "*" (substring-no-properties (aref (cadr (car tabulated-list-entries)) 1))))
      ;; Mark all
      (magit-dash-pr-dashboard-mark-all)
      (should (= 2 (length magit-dash-pr-dashboard--marked-entries)))
      ;; Bulk create worktrees for marked PRs
      (let ((created-nums nil))
        (cl-letf (((symbol-function 'magit-dash-pr--create-worktree-for-entry)
                   (lambda (entry &optional _)
                     (push (plist-get entry :number) created-nums)
                     "/tmp/wt")))
          (magit-dash-pr-dashboard-create-worktree)
          (should (equal '(20 10) created-nums))
          ;; Marks are cleared after bulk operation
          (should (null magit-dash-pr-dashboard--marked-entries))))
      ;; Mark all again and test bulk clean
      (magit-dash-pr-dashboard-mark-all)
      (let ((planned nil))
        (cl-letf (((symbol-function 'magit-dash-worktree-clean-entry-target)
                   (lambda (entry) (list :number (plist-get entry :number))))
                  ((symbol-function 'magit-dash-worktree-clean-plan-targets)
                   (lambda (targets &optional _) (setq planned targets))))
          (magit-dash-pr-dashboard-clean-merged-worktrees)
          (should (equal '(10 20) (sort (seq-map (lambda (tg) (plist-get tg :number)) planned) #'<)))
          (should (null magit-dash-pr-dashboard--marked-entries)))))))

(defun magit-dash-pr-test/rows-fixture ()
  "Fill the current dashboard buffer with rows for org/r #1 and #2."
  (magit-dash-pr-dashboard-mode)
  (setq tabulated-list-entries
        (list (magit-dash-pr-dashboard--build-entry
               '((number . 1) (title . "one") (state . "OPEN") (updatedAt . "")) "org/r")
              (magit-dash-pr-dashboard--build-entry
               '((number . 2) (title . "two") (state . "OPEN") (updatedAt . "")) "org/r")))
  (tabulated-list-print t))

(ert-deftest magit-dash-pr/refresh-entries-queries-only-the-given-pr ()
  "Refreshing one row runs one `gh pr view' and no discovery query."
  (with-temp-buffer
    (magit-dash-pr-test/rows-fixture)
    (let ((calls nil))
      (cl-letf (((symbol-function 'magit-dash-gh--run-process)
                 (lambda (args _dir on-success &optional _on-error _attempt)
                   (push args calls)
                   (funcall on-success "{\"number\":2,\"title\":\"two v2\",\"state\":\"OPEN\"}")))
                ((symbol-function 'magit-dash-pr-status-fetch-one)
                 (lambda (&rest _) nil)))
        (magit-dash-pr-dashboard-refresh-entries
         (list (car (cadr tabulated-list-entries))))
        (should (= 1 (length calls)))
        (should (equal '("pr" "view" "2" "-R" "org/r") (seq-take (car calls) 5)))
        (should (equal '("one" "two v2")
                       (seq-map (lambda (e) (map-elt (plist-get (car e) :pr) 'title))
                                tabulated-list-entries)))))))

(ert-deftest magit-dash-pr/refresh-entries-drops-pr-no-longer-open ()
  "Under the open filter a PR that merged leaves the table, other rows stay."
  (with-temp-buffer
    (magit-dash-pr-test/rows-fixture)
    (let ((key (magit-dash-pr-dashboard--entry-key (car (car tabulated-list-entries)))))
      (magit-dash-pr-dashboard--replace-row
       (current-buffer) key "{\"number\":1,\"state\":\"MERGED\"}")
      (should (equal '(2) (seq-map (lambda (e) (plist-get (car e) :number))
                                   tabulated-list-entries))))))

(ert-deftest magit-dash-pr/tracking-cache-roundtrip ()
  (let ((magit-dash-pr--tracking-cache (make-hash-table :test #'equal))
        (pr (list :number 3 :repo "o/r" :bucket 'ready :threads-open 0
                  :approvals 1 :failing nil :stack-parent 2 :ticket "FND-1")))
    (let ((tr (plist-get (magit-dash-pr-attach-tracking pr) :tracking)))
      (should (eq 'new (plist-get tr :delta-type)))
      (should (equal "ready to merge" (plist-get tr :action-summary)))
      (should (= 2 (plist-get tr :stack-parent)))
      (should (equal "FND-1" (plist-get tr :ticket))))
    (should (zerop (hash-table-count magit-dash-pr--tracking-cache)))
    (magit-dash-pr-commit-tracking (list pr))
    (should (equal '(:bucket ready) (list :bucket (plist-get (gethash "o/r#3" magit-dash-pr--tracking-cache) :bucket))))
    (let ((tr (plist-get (magit-dash-pr-attach-tracking
                          (plist-put (copy-sequence pr) :bucket 'blocked))
                         :tracking)))
      (should (eq 'bucket-changed (plist-get tr :delta-type)))
      (should (eq 'ready (plist-get tr :previous-bucket))))))

;;;; author/base columns, CI filter, limit, bindings

(ert-deftest magit-dash-pr/author-and-base-columns ()
  "Author shows the login; Base is off by default and shows the target branch."
  (let ((magit-dash-pr-columns (copy-tree magit-dash-pr--default-columns)))
    (should (magit-dash-pr--column-enabled-p 'author))
    (should-not (magit-dash-pr--column-enabled-p 'base))
    (setf (alist-get 'base magit-dash-pr-columns) t)
    (let* ((pr '((number . 7) (title . "t") (author . ((login . "alice")))
                 (baseRefName . "main")))
           (cells (cadr (magit-dash-pr-dashboard--build-entry pr "o/r")))
           (cols (magit-dash-pr--active-columns)))
      (should (equal "alice" (aref cells (seq-position cols 'author))))
      (should (equal "main" (aref cells (seq-position cols 'base)))))))

(ert-deftest magit-dash-pr/author-column-defaults-when-missing-from-persisted ()
  "A persisted alist lacking `author' still falls back to the default."
  (let ((magit-dash-pr-columns '((repo . t))))
    (should (magit-dash-pr--column-enabled-p 'author))
    (should-not (magit-dash-pr--column-enabled-p 'base))))

(ert-deftest magit-dash-pr/build-args-includes-limit ()
  "Both listing forms pass --limit, defaulting to the custom variable."
  (let ((magit-dash-pr-fetch-limit 100))
    (dolist (filters '((:state "open" :repo "o/r") (:state "open")))
      (let ((args (magit-dash-pr-dashboard--build-args filters)))
        (should (equal "100" (cadr (member "--limit" args)))))
      (should (equal "250" (cadr (member "--limit"
                                         (magit-dash-pr-dashboard--build-args filters 250))))))))

(ert-deftest magit-dash-pr/hide-failing-drops-failing-rows ()
  "With --hide-failing set, only non-failing rows remain."
  (let* ((mk (lambda (n state)
               (magit-dash-pr-dashboard--build-entry
                `((number . ,n) (statusCheckRollup . (((state . ,state)))))
                "o/r")))
         (entries (list (funcall mk 1 "SUCCESS") (funcall mk 2 "FAILURE"))))
    (let ((magit-dash-pr-dashboard--hide-failing nil))
      (should (= 2 (length (magit-dash-pr-dashboard--apply-ci-filter entries)))))
    (let ((magit-dash-pr-dashboard--hide-failing t))
      (should (equal '(1) (seq-map (lambda (e) (plist-get (car e) :number))
                                   (magit-dash-pr-dashboard--apply-ci-filter entries)))))))

(ert-deftest magit-dash-pr/keymap-mirrors-dash ()
  "\"m m\" opens the menu, SPC toggles marks, \"f x\" hides failing CI."
  (should (eq #'magit-dash-pr-dashboard-menu
              (keymap-lookup magit-dash-pr-dashboard-mode-map "m m")))
  (should (eq #'magit-dash-pr-dashboard-toggle-mark
              (keymap-lookup magit-dash-pr-dashboard-mode-map "SPC")))
  (should (eq #'magit-dash-pr-dashboard-toggle-hide-failing
              (keymap-lookup magit-dash-pr-dashboard-mode-map "f x"))))

(ert-deftest magit-dash-pr/print-preserves-point-by-key ()
  "Redraw with rebuilt ids keeps point on the same PR."
  (with-temp-buffer
    (magit-dash-pr-dashboard-mode)
    (let ((mk (lambda ()
                (seq-map (lambda (n)
                           (magit-dash-pr-dashboard--build-entry
                            `((number . ,n) (title . "t")) "o/r"))
                         '(1 2 3)))))
      (setq tabulated-list-entries (funcall mk))
      (tabulated-list-print t)
      (goto-char (point-min))
      (forward-line 2)
      (setq tabulated-list-entries (funcall mk))
      (magit-dash-pr-dashboard--print)
      (should (= 3 (plist-get (tabulated-list-get-id) :number))))))

(ert-deftest magit-dash-pr/create-worktrees-filters-existing ()
  "Creating worktrees for open PRs filters out those that already exist."
  (let ((e1 (list :number 10 :repo "org/repo" :head-ref "feat-10"))
        (e2 (list :number 20 :repo "org/repo" :head-ref "feat-20"))
        (created nil))
    (cl-letf (((symbol-function 'magit-dash-pr-dashboard--find-local-path)
               (lambda (_) "/tmp/repo"))
              ((symbol-function 'magit-dash-pr--find-existing-worktree)
               (lambda (_repo num &rest _)
                 (when (= num 10) "/tmp/repo/.wt/pr-10")))
              ((symbol-function 'magit-dash-pr--create-worktree-for-entry)
               (lambda (entry &optional _)
                 (push (plist-get entry :number) created))))
      ;; Marked creation filters out e1 (num 10) and only creates e2 (num 20)
      (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
                ((symbol-function 'magit-dash-pr-dashboard--marked-or-at-point-entries)
                 (lambda () (list e1 e2)))
                ((symbol-function 'magit-dash-pr-dashboard--rebuild-entries) #'ignore))
        (magit-dash-pr-dashboard-create-worktree)
        (should (equal created '(20)))))))

(ert-deftest magit-dash-pr/clean-worktree-closed-prs ()
  "Worktree cleanup applies to PRs that are closed as well as merged."
  (let ((closed-entry (list :number 30 :repo "org/repo" :state "CLOSED" :pr '((state . "CLOSED"))))
        (deleted nil))
    (cl-letf (((symbol-function 'magit-dash-worktree-risks) #'ignore)
              ((symbol-function 'magit-dash-pr-dashboard--find-local-path)
               (lambda (_) "/tmp/repo"))
              ((symbol-function 'magit-dash-pr--find-corroborated-worktree)
               (lambda (&rest _) '((path . "/tmp/repo/.wt/pr-30"))))
              ((symbol-function 'file-directory-p) (lambda (_) t))
              ((symbol-function 'magit-dash-pr--pr-merged-p) (lambda (&rest _) nil))
              ((symbol-function 'magit-dash-worktree-uncommitted-changes-p) (lambda (_) nil))
              ((symbol-function 'magit-dash-pr-delete-worktree)
               (lambda (path &rest _) (setq deleted path))))
      (let ((res (magit-dash-pr--clean-worktree-for-entry closed-entry nil)))
        (should (eq res :deleted))
        (should (equal deleted "/tmp/repo/.wt/pr-30"))))))

(ert-deftest magit-dash-pr/default-directory-resolution ()
  "PR dashboard default-directory is worktree if context is in it, repo root otherwise."
  (let ((repo-root "/home/user/project/")
        (wt-path "/home/user/project/.wt/pr-42/"))
    (cl-letf (((symbol-function 'magit-dash-pr-dashboard--find-local-path)
               (lambda (_) "/home/user/project"))
              ((symbol-function 'magit-dash-pr--git-worktrees)
               (lambda (_)
                 (list `((path . ,(directory-file-name wt-path))))))
              ((symbol-function 'file-directory-p) #'identity))
      ;; From within the worktree -> resolves to worktree
      (should (string-prefix-p wt-path
                               (magit-dash-pr--resolve-dashboard-default-directory
                                "org/project" (concat wt-path "src/"))))
      ;; From elsewhere -> resolves to main repo root
      (should (equal (magit-dash-pr--resolve-dashboard-default-directory
                      "org/project" "/home/user/other/")
                     repo-root)))))

(ert-deftest magit-dash-pr/conflicts-view-test ()
  "Conflicts view can be switched to and correctly configures filters."
  (with-temp-buffer
    (magit-dash-pr-dashboard-mode)
    (cl-letf (((symbol-function 'magit-dash-pr-dashboard-refresh) #'ignore))
      (magit-dash-pr-dashboard-switch-view "conflicts")
      (should (eq (plist-get magit-dash-pr-dashboard--filters :conflicts) t))
      ;; Check args with :conflicts
      (let ((args (magit-dash-pr-dashboard--build-args
                   '(:repo "org/repo" :conflicts t))))
        (should (member "number,title,state,mergedAt,author,createdAt,updatedAt,comments,reviewDecision,statusCheckRollup,isDraft,url,headRefName,headRefOid,baseRefName,mergeable,mergeStateStatus" args))
        (should (seq-some (lambda (a) (string-match-p "CONFLICTING.*DIRTY" a)) args)))
      ;; Check views menu binding
      (let ((flat (format "%S" (get 'magit-dash-pr-views-menu 'transient--layout))))
        (should (string-match-p ":key \"c\" :description \"Merge conflicts\" :command magit-dash-pr-dash-conflicts" flat))))))

;;;; Worktree prune prompts name the branch

(ert-deftest magit-dash-pr/clean-prompt-names-branch ()
  "The delete prompt shows the worktree's branch, or \"detached\" when unknown."
  (let ((prompts nil))
    (cl-letf (((symbol-function 'magit-dash-worktree-risks) #'ignore)
              ((symbol-function 'magit-dash-pr-dashboard--find-local-path) (lambda (_) "/tmp/repo"))
              ((symbol-function 'magit-dash-pr--find-corroborated-worktree)
               (lambda (&rest _) '((path . "/tmp/repo/.wt/pr-7"))))
              ((symbol-function 'file-directory-p) (lambda (_) t))
              ((symbol-function 'magit-dash-pr--pr-merged-p) (lambda (&rest _) t))
              ((symbol-function 'magit-dash-worktree-uncommitted-changes-p) (lambda (_) nil))
              ((symbol-function 'magit-dash-pr-delete-worktree) #'ignore)
              ((symbol-function 'y-or-n-p) (lambda (p) (push p prompts) nil)))
      (magit-dash-pr--clean-worktree-for-entry
       '(:number 7 :repo "o/r" :state "MERGED" :pr ((state . "MERGED") (headRefName . "feat/x")))
       t)
      (magit-dash-pr--clean-worktree-for-entry
       '(:number 7 :repo "o/r" :state "MERGED" :pr ((state . "MERGED")))
       t)
      (should (string-match-p "(branch 'feat/x')" (cadr prompts)))
      (should (string-match-p "(branch 'detached')" (car prompts))))))

;;;; Status columns

(require 'magit-dash-pr-cache)
(require 'magit-dash-pr-status)

(defmacro magit-dash-pr-test/with-cache (&rest body)
  "Run BODY with a temp cache root, only approvals/threads/ci columns and no gh."
  (declare (indent 0))
  `(let* ((root (make-temp-file "magit-dash-pr-status-cols-" t))
          (magit-dash-pr-columns '((approvals . t) (threads . t) (ci . t)
                                   (comments . t) (pr . t))))
     (unwind-protect
         (cl-letf (((symbol-function 'sprite-state-path)
                    (lambda (name) (expand-file-name name root)))
                   ((symbol-function 'magit-dash-gh--repo-dir)
                    (lambda () root)))
           ,@body)
       (delete-directory root t))))

(defun magit-dash-pr-test/status (&rest overrides)
  "Return a status plist with OVERRIDES applied."
  (let ((status (list :ci "SUCCESS" :approvals 1 :required 2 :requested '("alice" "bob")
                      :changes-requested 0 :threads-open 3 :threads-outdated 1
                      :failing nil :bucket 'threads)))
    (while overrides
      (setq status (plist-put status (pop overrides) (pop overrides))))
    status))

(defun magit-dash-pr-test/cells (entry)
  "Return ENTRY's cells keyed by active column."
  (cl-mapcar #'cons (magit-dash-pr--active-columns) (append (cadr entry) nil)))

(ert-deftest magit-dash-pr/status-columns-registered ()
  (should (memq 'approvals magit-dash-pr--all-columns))
  (should (memq 'threads magit-dash-pr--all-columns))
  (should (assq 'approvals magit-dash-pr--column-defs))
  (should (assq 'threads magit-dash-pr--column-defs)))

(ert-deftest magit-dash-pr/status-row-from-cached-status ()
  (magit-dash-pr-test/with-cache
    (with-temp-buffer
      (magit-dash-pr-dashboard-mode)
      (magit-dash-pr-dashboard--set-status "o/r" 7 (magit-dash-pr-test/status))
      (let* ((cells (magit-dash-pr-test/cells
                     (magit-dash-pr-dashboard--build-entry
                      '((number . 7) (title . "t") (commentsCount . 4)) "o/r")))
             (appr (alist-get 'approvals cells)))
        (should (equal "1/2" appr))
        (should (string-match-p "alice, bob" (get-text-property 0 'help-echo appr)))
        (should (equal "3" (alist-get 'threads cells)))
        (should (equal "✓" (alist-get 'ci cells)))
        (should (equal "4" (alist-get 'comments cells)))))))

(ert-deftest magit-dash-pr/status-row-markers-and-blanks ()
  (magit-dash-pr-test/with-cache
    (with-temp-buffer
      (magit-dash-pr-dashboard-mode)
      (let ((pr '((number . 7) (title . "t"))))
        (let ((cells (magit-dash-pr-test/cells
                      (magit-dash-pr-dashboard--build-entry pr "o/r"))))
          (should (equal "" (alist-get 'approvals cells)))
          (should (equal "" (alist-get 'threads cells))))
        (magit-dash-pr-dashboard--set-status
         "o/r" 7 (magit-dash-pr-test/status :changes-requested 1 :threads-open 0))
        (let ((cells (magit-dash-pr-test/cells
                      (magit-dash-pr-dashboard--build-entry pr "o/r"))))
          (should (equal "1/2!" (alist-get 'approvals cells)))
          (should (equal "" (alist-get 'threads cells))))))))

(ert-deftest magit-dash-pr/status-refresh-updates-threads-one-file ()
  (magit-dash-pr-test/with-cache
    (magit-dash-pr-cache-put "o/r" 1 '((number . 1) (title . "one")) (magit-dash-pr-test/status))
    (magit-dash-pr-cache-put "o/r" 2 '((number . 2) (title . "two")) (magit-dash-pr-test/status))
    (let ((other (magit-dash-pr-cache-get "o/r" 2)))
      (with-temp-buffer
        (magit-dash-pr-dashboard-mode)
        (setq magit-dash-pr-dashboard--filters (list :state "open" :repo "o/r"))
        (setq tabulated-list-entries
              (magit-dash-pr-dashboard--cached-entries magit-dash-pr-dashboard--filters))
        (cl-letf (((symbol-function 'magit-dash-gh--run-process) #'ignore)
                  ((symbol-function 'magit-dash-pr-status-fetch-one)
                   (lambda (_slug _number _dir on-success _on-error)
                     (funcall on-success (magit-dash-pr-test/status :threads-open 0
                                                                      :approvals 2)))))
          (magit-dash-pr-dashboard-refresh-entries
           (list (car (seq-find (lambda (e) (eql 1 (plist-get (car e) :number)))
                                tabulated-list-entries)))))
        (should (= 0 (plist-get (plist-get (magit-dash-pr-cache-get "o/r" 1) :status)
                                :threads-open)))
        (should (= 2 (plist-get (plist-get (magit-dash-pr-cache-get "o/r" 1) :status)
                                :approvals)))
        (should (equal "one" (map-elt (plist-get (magit-dash-pr-cache-get "o/r" 1) :payload)
                                      'title)))
        (should (equal other (magit-dash-pr-cache-get "o/r" 2)))
        (should (equal "" (alist-get 'threads
                                     (magit-dash-pr-test/cells
                                      (seq-find (lambda (e) (eql 1 (plist-get (car e) :number)))
                                                tabulated-list-entries)))))))))

(ert-deftest magit-dash-pr/status-fetch-error-keeps-old-status ()
  (magit-dash-pr-test/with-cache
    (magit-dash-pr-cache-put "o/r" 1 '((number . 1) (title . "one")) (magit-dash-pr-test/status))
    (with-temp-buffer
      (magit-dash-pr-dashboard-mode)
      (setq magit-dash-pr-dashboard--filters (list :state "open" :repo "o/r"))
      (setq tabulated-list-entries
            (magit-dash-pr-dashboard--cached-entries magit-dash-pr-dashboard--filters))
      (let ((before (magit-dash-pr-cache-get "o/r" 1))
            (entry (car (car tabulated-list-entries))))
        (cl-letf (((symbol-function 'magit-dash-gh--run-process)
                   (lambda (_args _dir on-success &rest _)
                     (funcall on-success "{\"number\":1,\"title\":\"one v2\",\"state\":\"OPEN\"}")))
                  ((symbol-function 'magit-dash-pr-status-fetch-one)
                   (lambda (_slug _number _dir _on-success on-error)
                     (funcall on-error "boom"))))
          (magit-dash-pr-dashboard-refresh-entries (list entry)))
        (let ((after (magit-dash-pr-cache-get "o/r" 1)))
          (should (equal (plist-get before :status) (plist-get after :status)))
          (should (equal "one v2" (map-elt (plist-get after :payload) 'title))))
        (should (equal "3" (alist-get 'threads
                                      (magit-dash-pr-test/cells
                                       (car tabulated-list-entries)))))))))

(ert-deftest magit-dash-pr/status-store-rejects-partial-over-good ()
  (magit-dash-pr-test/with-cache
    (magit-dash-pr-cache-put "o/r" 1 '((number . 1)) (magit-dash-pr-test/status))
    (with-temp-buffer
      (magit-dash-pr-dashboard-mode)
      (should (null (magit-dash-pr-dashboard--store-status
                     "o/r" 1 (magit-dash-pr-test/status :truncated t :threads-open 0) nil)))
      (should (null (magit-dash-pr-dashboard--store-status "o/r" 1 "garbage" nil)))
      (should (= 3 (plist-get (plist-get (magit-dash-pr-cache-get "o/r" 1) :status)
                              :threads-open))))))

(ert-deftest magit-dash-pr/status-collection-fills-matching-rows ()
  (magit-dash-pr-test/with-cache
    (with-temp-buffer
      (magit-dash-pr-dashboard-mode)
      (setq tabulated-list-entries
            (list (magit-dash-pr-dashboard--build-entry '((number . 1) (title . "a")) "o/r")
                  (magit-dash-pr-dashboard--build-entry '((number . 1) (title . "b")) "o/other")
                  (magit-dash-pr-dashboard--build-entry '((number . 2) (title . "c")) "o/r")))
      (magit-dash-pr-dashboard--fill-statuses
       (current-buffer)
       (list (append (list :repo "o/r" :number 1) (magit-dash-pr-test/status :threads-open 5))
             (append (list :repo "o/r" :number 99) (magit-dash-pr-test/status))))
      (should (equal '("5" "" "")
                     (seq-map (lambda (e) (alist-get 'threads (magit-dash-pr-test/cells e)))
                              tabulated-list-entries)))
      (should (equal '("o/r/1.eld")
                     (seq-map (lambda (f) (file-relative-name f (magit-dash-pr-cache--root)))
                              (directory-files-recursively (magit-dash-pr-cache--root)
                                                           "\\.eld\\'"))))
      (should (= 5 (plist-get (plist-get (magit-dash-pr-cache-get "o/r" 1) :status)
                              :threads-open))))))

;;;; Status hardening

(defun magit-dash-pr-test/stale-record (repo number)
  "Return the fetched-at of the cache record for REPO and NUMBER."
  (plist-get (magit-dash-pr-cache-get repo number) :fetched-at))

(defun magit-dash-pr-test/age-to-one (repo number)
  "Rewrite the record for REPO and NUMBER with :fetched-at 1.0."
  (magit-dash-pr-cache--write
   (magit-dash-pr-cache--file repo number)
   (plist-put (magit-dash-pr-cache-get repo number) :fetched-at 1.0)))

(ert-deftest magit-dash-pr/status-write-keeps-fetched-at-fill ()
  (magit-dash-pr-test/with-cache
    (magit-dash-pr-cache-put "o/r" 1 '((number . 1) (title . "one")) (magit-dash-pr-test/status))
    (magit-dash-pr-test/age-to-one "o/r" 1)
    (with-temp-buffer
      (magit-dash-pr-dashboard-mode)
      (setq tabulated-list-entries
            (list (magit-dash-pr-dashboard--build-entry '((number . 1) (title . "a")) "o/r")))
      (magit-dash-pr-dashboard--fill-statuses
       (current-buffer)
       (list (append (list :repo "o/r" :number 1)
                     (magit-dash-pr-test/status :threads-open 9))))
      (let ((record (magit-dash-pr-cache-get "o/r" 1)))
        (should (= 9 (plist-get (plist-get record :status) :threads-open)))
        (should (= 1.0 (plist-get record :fetched-at)))
        (should (magit-dash-pr-cache-stale-p record))
        (should (equal "one" (map-elt (plist-get record :payload) 'title)))))))

(ert-deftest magit-dash-pr/status-write-keeps-fetched-at-refresh ()
  (magit-dash-pr-test/with-cache
    (magit-dash-pr-cache-put "o/r" 1 '((number . 1) (title . "one")) (magit-dash-pr-test/status))
    (magit-dash-pr-test/age-to-one "o/r" 1)
    (with-temp-buffer
      (magit-dash-pr-dashboard-mode)
      (cl-letf (((symbol-function 'magit-dash-pr-status-fetch-one)
                 (lambda (_slug _number _dir on-success _on-error)
                   (funcall on-success (magit-dash-pr-test/status :threads-open 7)))))
        (magit-dash-pr-dashboard--refresh-status
         (current-buffer) '("o/r" . 1) '((number . 1)) "/"))
      (should (= 1.0 (magit-dash-pr-test/stale-record "o/r" 1)))
      (should (= 7 (plist-get (plist-get (magit-dash-pr-cache-get "o/r" 1) :status)
                              :threads-open))))))

(ert-deftest magit-dash-pr/status-write-without-prior-record ()
  (magit-dash-pr-test/with-cache
    (with-temp-buffer
      (magit-dash-pr-dashboard-mode)
      (should (magit-dash-pr-dashboard--store-status
               "o/r" 5 (magit-dash-pr-test/status) '((number . 5) (title . "five"))))
      (let ((record (magit-dash-pr-cache-get "o/r" 5)))
        (should (numberp (plist-get record :fetched-at)))
        (should (equal "five" (map-elt (plist-get record :payload) 'title)))
        (should (= 3 (plist-get (plist-get record :status) :threads-open)))))))

(ert-deftest magit-dash-pr/status-malformed-cached-status-never-signals ()
  (magit-dash-pr-test/with-cache
    (let ((bad (list (magit-dash-pr-test/status :requested "alice")
                     (magit-dash-pr-test/status :threads-outdated "z")
                     "str"
                     '(:approvals 1 :required)
                     (magit-dash-pr-test/status :ci 5)))
          (n 0))
      (seq-do (lambda (status)
                (setq n (1+ n))
                (magit-dash-pr-cache-put "o/r" n `((number . ,n) (state . "OPEN")) status))
              bad)
      (with-temp-buffer
        (magit-dash-pr-dashboard-mode)
        (let ((entries (magit-dash-pr-dashboard--cached-entries (list :state "open"))))
          (should (= 5 (length entries)))
          (seq-do (lambda (entry)
                    (should (stringp (alist-get 'approvals (magit-dash-pr-test/cells entry)))))
                  entries))
        (should (equal "" (magit-dash-pr-dashboard--format-approvals
                           (magit-dash-pr-test/status :requested "alice"))))
        (should (equal "" (magit-dash-pr-dashboard--format-threads "str")))
        (should (stringp (magit-dash-pr-dashboard--format-ci 5)))))))

(ert-deftest magit-dash-pr/status-survives-mode-reentry ()
  (with-temp-buffer
    (magit-dash-pr-dashboard-mode)
    (magit-dash-pr-dashboard--set-status "o/r" 1 (magit-dash-pr-test/status))
    (setq magit-dash-pr-dashboard--status-cancel #'ignore)
    (magit-dash-pr-dashboard-mode)
    (should (magit-dash-pr-dashboard--status "o/r" 1))
    (should (eq #'ignore magit-dash-pr-dashboard--status-cancel))
    (should (memq #'magit-dash-pr-dashboard--cancel-status-collection
                  kill-buffer-hook))))

(ert-deftest magit-dash-pr/status-kill-buffer-cancels-collection ()
  (magit-dash-pr-test/with-cache
    (let ((cancelled 0)
          (buf (generate-new-buffer " *dash-kill*")))
      (cl-letf (((symbol-function 'magit-dash-pr-status-collect-all-async)
                 (lambda (&rest _) (lambda () (setq cancelled (1+ cancelled))))))
        (with-current-buffer buf
          (magit-dash-pr-dashboard-mode)
          (magit-dash-pr-dashboard--collect-statuses buf "/")
          (should (functionp magit-dash-pr-dashboard--status-cancel)))
        (kill-buffer buf)
        (should (= 1 cancelled))))))

(ert-deftest magit-dash-pr/status-collect-wiring ()
  (magit-dash-pr-test/with-cache
    (let (calls (cancelled 0) (messages nil))
      (cl-letf (((symbol-function 'magit-dash-pr-status-collect-all-async)
                 (lambda (on-done on-error dir on-start on-progress)
                   (push (list on-done on-error dir on-start on-progress) calls)
                   (lambda () (setq cancelled (1+ cancelled)))))
                ((symbol-function 'message)
                 (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
        (with-temp-buffer
          (magit-dash-pr-dashboard-mode)
          (setq tabulated-list-entries
                (list (magit-dash-pr-dashboard--build-entry '((number . 1) (title . "a")) "o/r")))
          (magit-dash-pr-dashboard--collect-statuses (current-buffer) "/d")
          (should (= 0 cancelled))
          (magit-dash-pr-dashboard--collect-statuses (current-buffer) "/d")
          (should (= 1 cancelled))
          (let ((call (car calls))
                (prs (list (append (list :repo "o/r" :number 1)
                                   (magit-dash-pr-test/status :threads-open 4)))))
            (should (equal "/d" (nth 2 call)))
            (funcall (nth 4 call) prs nil nil)
            (should (equal "4" (alist-get 'threads
                                          (magit-dash-pr-test/cells
                                           (car tabulated-list-entries)))))
            (funcall (nth 1 call) "boom")
            (should (seq-some (lambda (m) (string-match-p "unavailable: boom" m)) messages))))))))

(ert-deftest magit-dash-pr/status-unchanged-costs-no-disk ()
  (magit-dash-pr-test/with-cache
    (let ((gets 0) (puts 0)
          (real-get (symbol-function 'magit-dash-pr-cache-get))
          (real-put (symbol-function 'magit-dash-pr-cache-put-status))
          (prs (list (append (list :repo "o/r" :number 1) (magit-dash-pr-test/status)))))
      (cl-letf (((symbol-function 'magit-dash-pr-cache-get)
                 (lambda (&rest args) (setq gets (1+ gets)) (apply real-get args)))
                ((symbol-function 'magit-dash-pr-cache-put-status)
                 (lambda (&rest args) (setq puts (1+ puts)) (apply real-put args))))
        (with-temp-buffer
          (magit-dash-pr-dashboard-mode)
          (setq tabulated-list-entries
                (list (magit-dash-pr-dashboard--build-entry '((number . 1)) "o/r")))
          (magit-dash-pr-dashboard--fill-statuses (current-buffer) prs)
          (should (= 1 puts))
          (let ((gets-after-first gets))
            (should (> gets-after-first 0))
            (magit-dash-pr-dashboard--fill-statuses (current-buffer) prs)
            (should (= gets-after-first gets))
            (should (= 1 puts))))))))

(ert-deftest magit-dash-pr/status-refresh-entries-survives-setup-failure ()
  (magit-dash-pr-test/with-cache
    (let (views fetched)
      (with-temp-buffer
        (magit-dash-pr-dashboard-mode)
        (setq tabulated-list-entries
              (list (magit-dash-pr-dashboard--build-entry '((number . 1)) "bad")
                    (magit-dash-pr-dashboard--build-entry '((number . 2)) "o/r")))
        (cl-letf (((symbol-function 'magit-dash-gh--run-process)
                   (lambda (args &rest _) (push args views)))
                  ((symbol-function 'magit-dash-pr-status-fetch-one)
                   (lambda (slug number &rest _)
                     (if (equal slug "bad")
                         (error "bad slug")
                       (push (cons slug number) fetched)))))
          (magit-dash-pr-dashboard-refresh-entries
           (seq-map #'car tabulated-list-entries))
          (should (= 2 (length views)))
          (should (equal '(("o/r" . 2)) fetched)))))))

(ert-deftest magit-dash-pr/status-store-guards ()
  (magit-dash-pr-test/with-cache
    (with-temp-buffer
      (magit-dash-pr-dashboard-mode)
      (should (null (magit-dash-pr-dashboard--store-status
                     "o/r" 1 (list :ci nil :approvals nil :threads-open nil) nil)))
      (should (null (magit-dash-pr-dashboard--store-status
                     "o/r" 1 (list :error "boom" :approvals 1) nil)))
      (should (null (magit-dash-pr-cache-get "o/r" 1)))
      ;; truncated with no prior is accepted
      (should (magit-dash-pr-dashboard--store-status
               "o/r" 1 (magit-dash-pr-test/status :truncated t :threads-open 5) '((number . 1))))
      ;; a worse truncated result is rejected, a not-worse one accepted
      (should (null (magit-dash-pr-dashboard--store-status
                     "o/r" 1 (magit-dash-pr-test/status :truncated t :threads-open 2) nil)))
      (should (magit-dash-pr-dashboard--store-status
               "o/r" 1 (magit-dash-pr-test/status :truncated t :threads-open 6) nil))
      (should (= 6 (plist-get (plist-get (magit-dash-pr-cache-get "o/r" 1) :status)
                              :threads-open))))))

(ert-deftest magit-dash-pr/build-format-default-columns-fit-width-80 ()
  (let* ((fmt (magit-dash-pr-dashboard--build-format 80 nil))
         (total (seq-reduce (lambda (acc col) (+ acc (cadr col))) (append fmt nil) 0))
         (repo (cadr (aref fmt 0))))
    (should (or (<= total 80) (>= repo 12)))))

(defmacro magit-dash-pr-test--with-owner (login &rest body)
  "Run BODY with gh stubbed so the account in use has LOGIN (nil: unknown)."
  (declare (indent 1))
  `(let ((magit-dash-pr--account-logins (make-hash-table :test #'equal)))
     (cl-letf (((symbol-function 'magit-dash-gh--repo-account)
                (lambda (&rest _) (and ,login (list :user ,login :host "github.com"))))
               ((symbol-function 'magit-dash-gh--run-process) #'ignore))
       ,@body)))

(ert-deftest magit-dash-pr/worktree-name-prefixes ()
  (should (equal "pr-7" (magit-dash-pr-worktree-name 7 t)))
  (should (equal "cr-7" (magit-dash-pr-worktree-name 7 nil))))

(ert-deftest magit-dash-pr/own-p-compares-account-login ()
  (magit-dash-pr-test--with-owner "me"
    (should (magit-dash-pr-own-p "Me" "/tmp/r"))
    (should-not (magit-dash-pr-own-p "other" "/tmp/r"))
    (should-not (magit-dash-pr-own-p nil "/tmp/r"))))

(ert-deftest magit-dash-pr/own-p-unknown-login-is-not-own ()
  (magit-dash-pr-test--with-owner nil
    (should-not (magit-dash-pr-own-p "me" "/tmp/r"))))

(ert-deftest magit-dash-pr/worktree-regexp-accepts-both-prefixes ()
  (seq-do (lambda (name)
            (should (string-match magit-dash-pr--worktree-number-regexp name))
            (should (equal "12" (match-string 1 name))))
          '("pr-12" "cr-12" "repo-pr-12" "repo-cr-12" "cr-feat-12"))
  (should (equal "\\(?:\\`\\|-\\)\\(?:pr\\|cr\\)-\\(?:[[:alnum:]]+-\\)*\\([0-9]+\\)\\'"
                 magit-dash-pr--worktree-number-regexp))
  (should-not (string-match magit-dash-pr--worktree-number-regexp "xr-12")))

(ert-deftest magit-dash-pr/match-worktree-accepts-both-prefixes ()
  (let* ((dir (make-temp-file "wt" t))
         (pr '((number . 5) (headRefName . "feat"))))
    (unwind-protect
        (progn
          (seq-do (lambda (name)
                    (let ((path (expand-file-name name dir)))
                      (make-directory path)
                      (should (eq pr (magit-dash-pr--match-worktree-to-pr
                                      `((path . ,path)) (list pr))))))
                  '("pr-5" "cr-5" "repo-cr-5"))
          (seq-do (lambda (name)
                    (let ((path (expand-file-name name dir)))
                      (make-directory path)
                      (should-not (magit-dash-pr--match-worktree-to-pr
                                   `((path . ,path)) (list pr)))))
                '("xcr-5" "ocr-5" "cr-ticket-5")))
      (delete-directory dir t))))

(defmacro magit-dash-pr-test--with-fake-git (repo adds &rest body)
  "Run BODY with git/branch plumbing stubbed for REPO, counting adds in ADDS."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'magit-dash-pr-dashboard--find-local-path) (lambda (_) ,repo))
             ((symbol-function 'magit-dash-pr--resolve-remote-branch) (lambda (&rest _) "feat"))
             ((symbol-function 'magit-dash-pr--git-worktrees) (lambda (_) nil))
             ((symbol-function 'magit-dash-worktree-symlink-worktree) #'ignore)
             ((symbol-function 'y-or-n-p) (lambda (&rest _) (ert-fail "unexpected y-or-n-p")))
             ((symbol-function 'magit-call-git)
              (lambda (&rest args)
                (when (equal (car args) "worktree")
                  (cl-incf ,adds)
                  (make-directory (nth 2 args) t))
                0)))
     ,@body))

(ert-deftest magit-dash-pr/create-worktree-names-by-owner-and-is-idempotent ()
  (seq-do
   (lambda (case)
     (let* ((repo (make-temp-file "repo" t))
            (entry (list :number 9 :repo "o/r"
                         :pr `((author . ((login . ,(car case)))) (headRefName . "feat"))))
            (adds 0))
       (unwind-protect
           (magit-dash-pr-test--with-owner "me"
             (magit-dash-pr-test--with-fake-git repo adds
               (let ((first (magit-dash-pr--create-worktree-for-entry entry))
                     (second (magit-dash-pr--create-worktree-for-entry entry)))
                 (should (equal first second))
                 (should (equal (cadr case) (file-name-nondirectory first)))
                 (should (= 1 adds)))))
         (delete-directory repo t))))
   '(("me" "pr-9") ("someone" "cr-9"))))

(ert-deftest magit-dash-pr/create-worktree-cold-then-warm-login-no-prompt ()
  "An own PR created with a cold login cache keeps its cr-N worktree once warm."
  (let* ((repo (make-temp-file "repo" t))
         (entry (list :number 9 :repo "o/r"
                      :pr '((author . ((login . "me"))) (headRefName . "feat"))))
         (adds 0))
    (unwind-protect
        (magit-dash-pr-test--with-owner nil
          (magit-dash-pr-test--with-fake-git repo adds
            (let ((first (magit-dash-pr--create-worktree-for-entry entry)))
              (should (equal "cr-9" (file-name-nondirectory first)))
              (setf (map-elt magit-dash-pr--account-logins '(:default . "github.com")) "me")
              (should (equal first (magit-dash-pr--create-worktree-for-entry entry)))
              (should (= 1 adds)))))
      (delete-directory repo t))))

(ert-deftest magit-dash-pr/create-worktree-legacy-pr-for-others-does-not-prompt ()
  (let* ((repo (make-temp-file "repo" t))
         (legacy (expand-file-name ".wt/pr-9" repo))
         (entry (list :number 9 :repo "o/r"
                      :pr '((author . ((login . "someone"))) (headRefName . "feat"))))
         (adds 0))
    (make-directory legacy t)
    (unwind-protect
        (magit-dash-pr-test--with-owner "me"
          (magit-dash-pr-test--with-fake-git repo adds
            (should (equal legacy (magit-dash-pr--create-worktree-for-entry entry)))
            (should (= 0 adds))))
      (delete-directory repo t))))

(ert-deftest magit-dash-pr/find-existing-prefers-computed-name-deterministically ()
  (let* ((repo (make-temp-file "repo" t))
         (pr (expand-file-name ".wt/pr-3" repo))
         (cr (expand-file-name ".wt/cr-3" repo)))
    (make-directory pr t)
    (make-directory cr t)
    (unwind-protect
        (cl-letf (((symbol-function 'magit-dash-pr--git-worktrees) (lambda (_) nil)))
          (should (equal cr (magit-dash-pr--find-existing-worktree repo 3 nil "cr-3")))
          (should (equal pr (magit-dash-pr--find-existing-worktree repo 3 nil "pr-3")))
          (should (equal pr (magit-dash-pr--find-existing-worktree repo 3))))
      (delete-directory repo t))))

(ert-deftest magit-dash-pr/unstubbed-own-p-nil-login-names-cr ()
  "With no known login every naming path yields cr-N."
  (let* ((repo (make-temp-file "repo" t))
         (entry (list :number 4 :repo "o/r"
                      :pr '((author . ((login . "me"))) (headRefName . "feat"))))
         (adds 0))
    (unwind-protect
        (magit-dash-pr-test--with-owner nil
          (magit-dash-pr-test--with-fake-git repo adds
            (let ((magit-dash-repo-list nil))
              (should (equal "cr-4" (magit-dash-pr--entry-worktree-name entry repo)))
              (should (equal "cr-4" (file-name-nondirectory
                                     (magit-dash-pr-dashboard--worktree-path entry))))
              (should (equal "cr-4" (file-name-nondirectory
                                     (magit-dash-pr--create-worktree-for-entry entry)))))))
      (delete-directory repo t))))

(ert-deftest magit-dash-pr/account-login-spawns-one-process-per-burst ()
  (let ((repo (make-temp-file "repo" t))
        (spawned nil))
    (unwind-protect
        (let ((magit-dash-pr--account-logins (make-hash-table :test #'equal))
              (magit-dash-pr--repo-hosts (make-hash-table :test #'equal)))
          (cl-letf (((symbol-function 'magit-dash-gh--repo-account) #'ignore)
                    ((symbol-function 'magit-dash-gh--run-process)
                     (lambda (args _dir _ok &optional _err &rest _)
                       (push args spawned))))
            (seq-do (lambda (_) (magit-dash-pr-own-p "me" repo)) '(1 2 3 4 5))
            (should (= 1 (length spawned)))
            (should (member "--hostname" (car spawned)))
            ;; a failed lookup is not retried inside the window
            (setf (map-elt magit-dash-pr--account-logins '(:default . "github.com"))
                  (cons :failed (current-time)))
            (magit-dash-pr-own-p "me" repo)
            (should (= 1 (length spawned)))
            ;; ...but is once it expires
            (setf (map-elt magit-dash-pr--account-logins '(:default . "github.com"))
                  (cons :failed (time-subtract (current-time) 600)))
            (magit-dash-pr-own-p "me" repo)
            (should (= 2 (length spawned)))))
      (delete-directory repo t))))

(ert-deftest magit-dash-pr/repo-host-from-remote ()
  (let ((repo (make-temp-file "repo" t))
        (magit-dash-pr--repo-hosts (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (call-process "git" nil nil nil "-C" repo "init" "-q")
          (call-process "git" nil nil nil "-C" repo "remote" "add" "origin"
                        "git@ghe.example.com:o/r.git")
          (should (equal "ghe.example.com" (magit-dash-pr--repo-host repo)))
          (should (equal "github.com" (magit-dash-pr--repo-host "/nonexistent"))))
      (delete-directory repo t))))

(ert-deftest magit-dash-pr/corroboration-requires-branch-or-exact-name ()
  (let* ((dir (make-temp-file "wt" t))
         (pr '((number . 5) (headRefName . "feat"))))
    (unwind-protect
        (let ((mk (lambda (name branch)
                    (let ((path (expand-file-name name dir)))
                      (make-directory path t)
                      `((path . ,path) (branch . ,branch))))))
          (should-not (magit-dash-pr--worktree-corroborates-pr-p
                       (funcall mk "cr-ticket-5" "my-work") pr 5))
          (should (magit-dash-pr--worktree-corroborates-pr-p
                   (funcall mk "cr-ticket-5" "wt/feat") pr 5))
          (should-not (magit-dash-pr--worktree-corroborates-pr-p
                       (funcall mk "cr-5" "my-work") pr 5))
          (should (magit-dash-pr--worktree-corroborates-pr-p
                   (funcall mk "cr-5" "cr-5") pr 5)))
      (delete-directory dir t))))

(ert-deftest magit-dash-pr/review-worktrees-refuses-exact-name-with-unrelated-branch ()
  (let* ((dir (make-temp-file "repo" t))
         (wt (expand-file-name "cr-77" dir))
         (deleted nil))
    (make-directory wt t)
    (unwind-protect
        (cl-letf (((symbol-function 'magit-dash-worktree-risks) #'ignore)
              ((symbol-function 'magit-dash-pr--git-worktrees)
                   (lambda (_) `(((path . ,wt) (branch . "custom")))))
                  ((symbol-function 'magit-dash-pr--fetch-prs-for-branch) #'ignore)
                  ((symbol-function 'magit-dash-pr--fetch-pr)
                   (lambda (&rest _) '((number . 77) (state . "MERGED") (mergedAt . "2026-01-01"))))
                  ((symbol-function 'magit-dash-worktree-uncommitted-changes-p) #'ignore)
                  ((symbol-function 'magit-dash-pr-dashboard--current-repo-slug) (lambda () "o/r"))
                  ((symbol-function 'magit-dash-pr-delete-worktree)
                   (lambda (path &rest _) (push path deleted) t)))
          (let ((counts (magit-dash-pr-review-worktrees dir nil '((dummy)))))
            (should-not deleted)
            (should (= 0 (plist-get counts :deleted)))
            (should (= 1 (plist-get counts :skipped)))))
      (delete-directory dir t))))

;;;; login priming, lookup hardening, refresh concurrency cap

(defun magit-dash-pr-test--wait-until (pred &optional timeout)
  "Run the event loop until PRED is non-nil or TIMEOUT (default 5) seconds pass."
  (let ((deadline (+ (float-time) (or timeout 5))))
    (while (and (not (funcall pred)) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (funcall pred)))

(ert-deftest magit-dash-pr/account-login-start-failure-marks-failed ()
  (let ((repo (make-temp-file "repo" t)))
    (unwind-protect
        (let ((magit-dash-pr--account-logins (make-hash-table :test #'equal))
              (magit-dash-pr--repo-hosts (make-hash-table :test #'equal)))
          (cl-letf (((symbol-function 'magit-dash-gh--repo-account) #'ignore)
                    ((symbol-function 'magit-dash-gh--run-process)
                     (lambda (&rest _) (error "no fork"))))
            (should-not (magit-dash-pr-own-p "me" repo))
            (should (eq :failed (car (map-elt magit-dash-pr--account-logins
                                              '(:default . "github.com")))))))
      (delete-directory repo t))))

(ert-deftest magit-dash-pr/account-login-lookup-times-out ()
  "A silent lookup is killed, cancelled and marked failed."
  (let ((repo (make-temp-file "repo" t))
        (magit-dash-pr--account-logins (make-hash-table :test #'equal))
        (magit-dash-pr--repo-hosts (make-hash-table :test #'equal))
        (magit-dash-pr--login-timeout 0.1)
        (hung nil)
        (cancelled-p nil)
        (called nil))
    (unwind-protect
        (cl-letf (((symbol-function 'magit-dash-gh--repo-account) #'ignore)
                  ((symbol-function 'magit-dash-gh--run-process)
                   (lambda (_args _dir on-success &optional on-error _attempt cancelled handle)
                     (setq cancelled-p cancelled)
                     (setq hung (make-process :name "login-hung"
                                              :command '("sleep" "5") :noquery t))
                     (setcar handle hung)
                     (setq called (list on-success on-error))
                     hung)))
          (should-not (magit-dash-pr-own-p "me" repo))
          (should (eq :pending (car (map-elt magit-dash-pr--account-logins
                                             '(:default . "github.com")))))
          (should (magit-dash-pr-test--wait-until
                   (lambda ()
                     (eq :failed (car (map-elt magit-dash-pr--account-logins
                                               '(:default . "github.com")))))))
          (should (magit-dash-pr-test--wait-until (lambda () (not (process-live-p hung)))))
          (should (funcall cancelled-p))
          ;; a late answer does not resurrect the lookup
          (funcall (car called) "me\n")
          (should (eq :failed (car (map-elt magit-dash-pr--account-logins
                                            '(:default . "github.com"))))))
      (when (process-live-p hung)
        (delete-process hung))
      (delete-directory repo t))))

(ert-deftest magit-dash-pr/repo-host-fallback-is-not-cached ()
  (let ((repo (make-temp-file "repo" t))
        (magit-dash-pr--repo-hosts (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (call-process "git" nil nil nil "-C" repo "init" "-q")
          (should (equal "github.com" (magit-dash-pr--repo-host repo)))
          (should-not (map-elt magit-dash-pr--repo-hosts repo))
          (call-process "git" nil nil nil "-C" repo "remote" "add" "origin"
                        "git@ghe.example.com:o/r.git")
          (should (equal "ghe.example.com" (magit-dash-pr--repo-host repo)))
          (should (equal "ghe.example.com" (map-elt magit-dash-pr--repo-hosts repo))))
      (delete-directory repo t))))

(ert-deftest magit-dash-pr/prime-account-logins-one-lookup-per-repo ()
  (let ((a (make-temp-file "repo" t))
        (b (make-temp-file "repo" t))
        (spawned nil))
    (unwind-protect
        (let ((magit-dash-pr--account-logins (make-hash-table :test #'equal))
              (magit-dash-pr--repo-hosts (make-hash-table :test #'equal)))
          (cl-letf (((symbol-function 'magit-dash-gh--repo-account) #'ignore)
                    ((symbol-function 'magit-dash-gh--run-process)
                     (lambda (_args dir &rest _) (push dir spawned) nil)))
            (magit-dash-pr--prime-account-logins (list a b a))
            (magit-dash-pr--prime-account-logins (list a b))
            ;; both share the default github.com login: one lookup in flight
            (should (= 1 (length spawned)))))
      (delete-directory a t)
      (delete-directory b t))))

(ert-deftest magit-dash-pr/dashboard-refresh-primes-logins ()
  (let ((primed 0))
    (with-temp-buffer
      (magit-dash-pr-dashboard-mode)
      (cl-letf (((symbol-function 'magit-dash-pr--prime-account-logins)
                 (lambda (&rest _) (setq primed (1+ primed))))
                ((symbol-function 'magit-dash-gh--run-process)
                 (lambda (&rest _) nil)))
        (magit-dash-pr-dashboard-refresh)
        (should (= 1 primed))))))

(ert-deftest magit-dash-pr/primed-login-names-own-pr-pr-n ()
  "With the login primed before first use, an own PR is pr-N."
  (let ((repo (make-temp-file "repo" t))
        (magit-dash-pr--account-logins (make-hash-table :test #'equal))
        (magit-dash-pr--repo-hosts (make-hash-table :test #'equal))
        (answer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'magit-dash-gh--repo-account) #'ignore)
                  ((symbol-function 'magit-dash-gh--run-process)
                   (lambda (_args _dir on-success &rest _)
                     (setq answer on-success)
                     nil)))
          (magit-dash-pr--prime-account-logins (list repo))
          (funcall answer "me\n")
          (should (equal "pr-3" (magit-dash-pr--entry-worktree-name
                                 '(:number 3 :author "me") repo)))
          (should (equal "cr-3" (magit-dash-pr--entry-worktree-name
                                 '(:number 3 :author "other") repo))))
      (delete-directory repo t))))

(ert-deftest magit-dash-pr/reset-account-logins-clears-token-cache ()
  (let ((magit-dash-gh--token-cache (make-hash-table :test #'equal))
        (magit-dash-pr--account-logins (make-hash-table :test #'equal))
        (magit-dash-pr--repo-hosts (make-hash-table :test #'equal)))
    (setf (map-elt magit-dash-gh--token-cache '("u" . "github.com"))
          (cons "tok" (current-time)))
    (magit-dash-pr-reset-account-logins)
    (should (zerop (hash-table-count magit-dash-gh--token-cache)))))

(ert-deftest magit-dash-pr/run-limited-caps-concurrency ()
  (let ((started nil)
        (dones nil)
        (tasks nil))
    (dotimes (i 6)
      (push (lambda (done) (push i started) (push done dones)) tasks))
    (magit-dash-pr--run-limited (nreverse tasks) 2)
    (should (= 2 (length started)))
    (funcall (car (last dones)))
    (should (= 3 (length started)))
    (seq-do #'funcall (butlast dones))
    (should (>= (length started) 4))
    (while dones
      (let ((d (pop dones)))
        (funcall d)))
    (should (= 6 (length started)))))

(ert-deftest magit-dash-pr/run-limited-failing-task-frees-its-slot ()
  (let ((ran nil))
    (magit-dash-pr--run-limited
     (list (lambda (_done) (error "boom"))
           (lambda (_done) (push 'b ran))
           (lambda (_done) (push 'c ran)))
     1)
    (should (equal '(b) ran))))

(ert-deftest magit-dash-pr/refresh-entries-caps-concurrent-gh-processes ()
  (with-temp-buffer
    (magit-dash-pr-test/rows-fixture)
    (let ((magit-dash-pr-refresh-max-processes 1)
          (views nil)
          (view-done nil)
          (fetches nil))
      (cl-letf (((symbol-function 'magit-dash-gh--run-process)
                 (lambda (args _dir on-success &rest _)
                   (push args views)
                   (setq view-done on-success)
                   nil))
                ((symbol-function 'magit-dash-pr-status-fetch-one)
                 (lambda (slug number _dir on-success &rest _)
                   (push (cons slug number) fetches)
                   (funcall on-success (list :ci nil)))))
        (magit-dash-pr-dashboard-refresh-entries
         (seq-map #'car tabulated-list-entries))
        ;; only the first view runs until it finishes
        (should (= 1 (length views)))
        (should (null fetches))
        (funcall view-done "{\"number\":1,\"state\":\"OPEN\"}")
        (should (= 1 (length fetches)))
        (should (= 2 (length views)))))))

;;;; New since last review

(defun magit-dash-pr-test/write-review (file &optional verdict md)
  "Write review JSON FILE with the alists VERDICT and MD (magit_dash)."
  (with-temp-file file
    (insert (json-serialize
             (append (list :format "diff-review/1")
                     (and verdict (list :verdict verdict))
                     (and md (list :magit_dash md)))))))

(defvar magit-dash-pr-test/review-file-calls 0)
(defvar magit-dash-pr-test/local-path-calls 0)
(defvar magit-dash-pr-test/process-calls 0)

(defmacro magit-dash-pr-test/with-review (head file-var &rest body)
  "Run BODY with a temp review file FILE-VAR and cached head HEAD for o/r#7.
Counts calls to `find-local-path', `magit-dash-review-file' and process
spawns (which also error, so no network)."
  (declare (indent 2))
  `(magit-dash-pr-test/with-cache
     (let ((,file-var (make-temp-file "nsr-review-" nil ".json")))
       (setq magit-dash-pr-test/review-file-calls 0
             magit-dash-pr-test/local-path-calls 0
             magit-dash-pr-test/process-calls 0)
       (unwind-protect
           (cl-letf (((symbol-function 'magit-dash-pr-dashboard--find-local-path)
                      (lambda (_repo)
                        (cl-incf magit-dash-pr-test/local-path-calls)
                        "/tmp/nsr-root"))
                     ((symbol-function 'magit-dash-review-file)
                      (lambda (&rest _)
                        (cl-incf magit-dash-pr-test/review-file-calls)
                        ,file-var))
                     ((symbol-function 'call-process)
                      (lambda (&rest _)
                        (cl-incf magit-dash-pr-test/process-calls)
                        (error "no process allowed")))
                     ((symbol-function 'make-process)
                      (lambda (&rest _)
                        (cl-incf magit-dash-pr-test/process-calls)
                        (error "no process allowed"))))
             (clrhash magit-dash-pr-dashboard--reviewed-heads)
             (when ,head
               (magit-dash-pr-cache-put "o/r" 7 `((number . 7) (state . "OPEN")
                                                  (headRefOid . ,,head))))
             ,@body)
         (ignore-errors (delete-file ,file-var))))))

(defun magit-dash-pr-test/new-cell (head)
  "Return the new-column cell for o/r#7 at HEAD."
  (magit-dash-pr-dashboard--format-new "o/r" 7 head))

(ert-deftest magit-dash-pr/new-column-registered-and-off-by-default ()
  (should (memq 'new magit-dash-pr--all-columns))
  (should (assq 'new magit-dash-pr--column-defs))
  (should-not (cdr (assq 'new magit-dash-pr--default-columns))))

(ert-deftest magit-dash-pr/new-column-new-when-heads-differ ()
  (magit-dash-pr-test/with-review "bbbb2222" file
    (magit-dash-pr-test/write-review file '((state . "changes") (head . "aaaa1111")))
    (let ((cell (magit-dash-pr-test/new-cell "bbbb2222")))
      (should (equal "new" (substring-no-properties cell)))
      (should (string-match-p "New commits since last review"
                              (get-text-property 0 'help-echo cell))))))

(ert-deftest magit-dash-pr/new-column-blank-when-unchanged ()
  (magit-dash-pr-test/with-review "aaaa1111" file
    (magit-dash-pr-test/write-review file '((state . "approved") (head . "aaaa1111")))
    (let ((cell (magit-dash-pr-test/new-cell "aaaa1111")))
      (should (equal " " (substring-no-properties cell)))
      (should (string-match-p "Unchanged since last review"
                              (get-text-property 0 'help-echo cell))))))

(ert-deftest magit-dash-pr/new-column-abbreviated-head-is-not-new ()
  (magit-dash-pr-test/with-review "abcdef1234567890" file
    (magit-dash-pr-test/write-review file '((head . "abcdef1")))
    (should (equal " " (substring-no-properties
                        (magit-dash-pr-test/new-cell "abcdef1234567890")))))
  (magit-dash-pr-test/with-review "abcdef1234567890" file
    (magit-dash-pr-test/write-review file '((head . "abcd")))
    (should (equal "new" (substring-no-properties
                          (magit-dash-pr-test/new-cell "abcdef1234567890"))))))

(ert-deftest magit-dash-pr/new-column-blank-when-never-reviewed ()
  (magit-dash-pr-test/with-review "aaaa1111" file
    (magit-dash-pr-test/write-review file)
    (let ((cell (magit-dash-pr-test/new-cell "aaaa1111")))
      (should (equal " " (substring-no-properties cell)))
      (should (equal "Never reviewed" (get-text-property 0 'help-echo cell)))))
  (magit-dash-pr-test/with-review "aaaa1111" file
    (delete-file file)
    (should (equal "Never reviewed"
                   (get-text-property 0 'help-echo (magit-dash-pr-test/new-cell "aaaa1111"))))))

(ert-deftest magit-dash-pr/new-column-empty-without-head ()
  (magit-dash-pr-test/with-review nil file
    (magit-dash-pr-test/write-review file '((head . "aaaa1111")))
    (should (equal "" (magit-dash-pr-test/new-cell nil)))
    (should (equal "" (magit-dash-pr-test/new-cell "")))))

(ert-deftest magit-dash-pr/new-column-head-fallbacks ()
  (magit-dash-pr-test/with-review "bbbb2222" file
    (magit-dash-pr-test/write-review file '((submitted_head . "aaaa1111")))
    (should (equal "new" (substring-no-properties (magit-dash-pr-test/new-cell "bbbb2222")))))
  ;; head_oid alone is a sync, not a review: neutral whether or not heads match.
  (seq-do (lambda (current)
            (magit-dash-pr-test/with-review current file
              (magit-dash-pr-test/write-review file nil '((head_oid . "bbbb2222")))
              (let ((cell (magit-dash-pr-test/new-cell current)))
                (should (equal " " (substring-no-properties cell)))
                (should (equal "synced review file, no verdict recorded"
                               (get-text-property 0 'help-echo cell))))))
          '("bbbb2222" "cccc3333"))
  (magit-dash-pr-test/with-review "bbbb2222" file
    (magit-dash-pr-test/write-review file '((head . "aaaa1111")) '((head_oid . "bbbb2222")))
    (should (equal "new" (substring-no-properties (magit-dash-pr-test/new-cell "bbbb2222")))))
  (magit-dash-pr-test/with-review "bbbb2222" file
    (magit-dash-pr-test/write-review file '((head . "bbbb2222")) '((head_oid . "aaaa1111")))
    (should (string-match-p "Unchanged"
                            (get-text-property 0 'help-echo (magit-dash-pr-test/new-cell "bbbb2222"))))))

(ert-deftest magit-dash-pr/new-column-malformed-json-is-unreadable-once ()
  (magit-dash-pr-test/with-review "aaaa1111" file
    (with-temp-file file (insert "{not json"))
    (let ((messages nil)
          (native-comp-enable-subr-trampolines nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
        (should (equal "review file unreadable"
                       (get-text-property 0 'help-echo (magit-dash-pr-test/new-cell "aaaa1111"))))
        (magit-dash-pr-test/new-cell "aaaa1111")
        (should (= 1 (seq-count (lambda (m) (string-match-p "unreadable" m)) messages)))))))

(ert-deftest magit-dash-pr/new-column-reads-cached-until-mtime-changes ()
  (magit-dash-pr-test/with-review "bbbb2222" file
    (magit-dash-pr-test/write-review file '((head . "aaaa1111")))
    (set-file-times file (seconds-to-time 1000000))
    (should (equal "aaaa1111" (plist-get (magit-dash-pr-dashboard--reviewed "o/r" 7) :head)))
    (magit-dash-pr-test/write-review file '((head . "cccc3333")))
    (set-file-times file (seconds-to-time 1000000))
    (should (equal "aaaa1111" (plist-get (magit-dash-pr-dashboard--reviewed "o/r" 7) :head)))
    (set-file-times file (seconds-to-time 2000000))
    (should (equal "cccc3333" (plist-get (magit-dash-pr-dashboard--reviewed "o/r" 7) :head)))
    ;; Disappearing file: evicted and re-resolved to none.
    (delete-file file)
    (should-not (plist-get (magit-dash-pr-dashboard--reviewed "o/r" 7) :head))
    (should (eq 'none (plist-get (magit-dash-pr-dashboard--reviewed "o/r" 7) :state)))))

(ert-deftest magit-dash-pr/new-column-negative-result-is-cached ()
  (magit-dash-pr-test/with-review "aaaa1111" file
    (delete-file file)
    (magit-dash-pr-test/new-cell "aaaa1111")
    (magit-dash-pr-test/new-cell "aaaa1111")
    (magit-dash-pr-test/new-cell "aaaa1111")
    (should (= 1 magit-dash-pr-test/local-path-calls))
    (should (= 1 magit-dash-pr-test/review-file-calls))
    (should (= 0 magit-dash-pr-test/process-calls))
    ;; Past the TTL it is resolved again.
    (let ((magit-dash-pr-dashboard--reviewed-negative-ttl -1))
      (magit-dash-pr-test/new-cell "aaaa1111"))
    (should (= 2 magit-dash-pr-test/review-file-calls))))

(ert-deftest magit-dash-pr/new-column-row-cell-exact-and-stale-face ()
  (magit-dash-pr-test/with-review "bbbb2222" file
    (magit-dash-pr-test/write-review file '((head . "aaaa1111")))
    (let ((magit-dash-pr-columns '((new . t) (pr . t))))
      (with-temp-buffer
        (magit-dash-pr-dashboard-mode)
        (let* ((i (seq-position (magit-dash-pr--active-columns) 'new))
               (fresh (cadr (magit-dash-pr-dashboard--build-entry '((number . 7)) "o/r")))
               (stale (cadr (magit-dash-pr-dashboard--build-entry '((number . 7)) "o/r" 1.0)))
               (live (cadr (magit-dash-pr-dashboard--build-entry
                            '((number . 7) (headRefOid . "aaaa1111")) "o/r"))))
          ;; Columns: pr, new.
          (should (equal "new" (substring-no-properties (aref fresh i))))
          (should (equal "new" (substring-no-properties (aref stale i))))
          (should-not (memq 'magit-dash-stale (ensure-list (get-text-property 0 'face (aref fresh i)))))
          (should (memq 'magit-dash-stale (ensure-list (get-text-property 0 'face (aref stale i)))))
          ;; The row's own headRefOid wins over the cache record.
          (should (equal " " (substring-no-properties (aref live i)))))))))


(ert-deftest magit-dash-pr/self-view-hides-author-column ()
  "The current user's PR view omits the redundant author column."
  (with-temp-buffer
    (let ((magit-dash-pr-columns '((author . t))))
      (setq-local magit-dash-pr-dashboard--filters '(:author "@me"))
      (should-not (memq 'author (magit-dash-pr--active-columns)))
      (setq-local magit-dash-pr-dashboard--filters '(:author nil))
      (should (memq 'author (magit-dash-pr--active-columns))))))

(ert-deftest magit-dash-pr/refresh-rebuilds-columns-before-discovery ()
  "Changing the author filter immediately updates existing rows and header."
  (with-temp-buffer
    (magit-dash-pr-dashboard-mode)
    (setq tabulated-list-entries
          (list (magit-dash-pr-dashboard--build-entry
                 '((number . 7) (author . ((login . "alice")))) "o/r")))
    (setq-local magit-dash-pr-dashboard--filters '(:state "open" :author "@me"))
    (cl-letf (((symbol-function 'magit-dash-gh--run-process) #'ignore)
              ((symbol-function 'magit-dash-pr--prime-account-logins) #'ignore))
      (magit-dash-pr-dashboard-refresh))
    (should-not (seq-find (lambda (column) (equal (car column) "Author"))
                          (append tabulated-list-format nil)))
    (should (= (length tabulated-list-format)
               (length (cadr (car tabulated-list-entries)))))))

(provide 'test-magit-dash-pr)
;;; test-magit-dash-pr.el ends here
