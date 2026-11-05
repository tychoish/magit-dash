;;; test-magit-dash-gh-pr.el --- ERT tests for magit-dash-gh-pr -*- lexical-binding: t; no-byte-compile: t; -*-

;; Run inside a live Emacs session:
;;   (ert "^magit-dash-gh-pr/")
;;
;; Batch run:
;;   emacs --batch -l test/test-helper.el \
;;     -l test/test-magit-dash-gh-pr.el \
;;     --eval '(ert-run-tests-batch-and-exit "magit-dash-gh-pr/")'

(require 'ert)
(require 'cl-lib)
(require 'map)
(require 'magit-dash)
(require 'magit-dash-gh-pr)
(require 'magit-dash-gh-actions)

;;; Test helpers

(defmacro magit-dash-gh-pr-test/with-temp-dir (&rest body)
  "Execute BODY with `default-directory' set to a fresh temp directory."
  (declare (indent 0))
  `(let ((dir (make-temp-file "magit-dash-gh-pr-test-" t)))
     (unwind-protect
         (let ((default-directory dir))
           ,@body)
       (delete-directory dir t))))

(defun magit-dash-gh-pr-test/make-thread (id resolved path creator last-commentor)
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

(defun magit-dash-gh-pr-test/graphql-response (threads)
  "Wrap THREADS in a fake GraphQL response alist."
  `((data . ((repository . ((pullRequest . ((reviewThreads . ((nodes . ,threads)))))))))))


;;; magit-dash-gh-pr--parse-threads


(ert-deftest magit-dash-gh-pr/parse-threads-empty ()
  (let* ((response (magit-dash-gh-pr-test/graphql-response nil))
         (threads (magit-dash-gh-pr--parse-threads response)))
    (should (null threads))))

(ert-deftest magit-dash-gh-pr/parse-threads-resolved ()
  (let* ((node (magit-dash-gh-pr-test/make-thread "t1" t "src/foo.el" "alice" "bob"))
         (response (magit-dash-gh-pr-test/graphql-response (list node)))
         (threads (magit-dash-gh-pr--parse-threads response)))
    (should (= 1 (length threads)))
    (let ((th (car threads)))
      (should (equal "t1"         (plist-get th :id)))
      (should (eq    t            (plist-get th :resolved)))
      (should (equal "src/foo.el" (plist-get th :path)))
      (should (equal "alice"      (plist-get th :creator)))
      (should (equal "bob"        (plist-get th :last-commentor))))))

(ert-deftest magit-dash-gh-pr/parse-threads-unresolved ()
  (let* ((node (magit-dash-gh-pr-test/make-thread "t2" nil "lisp/bar.el" "charlie" "dave"))
         (response (magit-dash-gh-pr-test/graphql-response (list node)))
         (threads (magit-dash-gh-pr--parse-threads response)))
    (let ((th (car threads)))
      (should (eq nil (plist-get th :resolved))))))

(ert-deftest magit-dash-gh-pr/parse-threads-multiple ()
  (let* ((nodes (list
                 (magit-dash-gh-pr-test/make-thread "t1" t   "a.el" "u1" "u2")
                 (magit-dash-gh-pr-test/make-thread "t2" nil "b.el" "u3" "u4")
                 (magit-dash-gh-pr-test/make-thread "t3" t   "c.el" "u5" "u5")))
         (response (magit-dash-gh-pr-test/graphql-response nodes))
         (threads (magit-dash-gh-pr--parse-threads response)))
    (should (= 3 (length threads)))
    (should (eq t   (plist-get (nth 0 threads) :resolved)))
    (should (eq nil (plist-get (nth 1 threads) :resolved)))
    (should (eq t   (plist-get (nth 2 threads) :resolved)))))

(ert-deftest magit-dash-gh-pr/parse-threads-creator-from-first-comment ()
  "Creator comes from the first comment's author."
  (let* ((node (magit-dash-gh-pr-test/make-thread "t1" nil "x.el" "firstauthor" "lastauthor"))
         (response (magit-dash-gh-pr-test/graphql-response (list node)))
         (threads (magit-dash-gh-pr--parse-threads response)))
    (should (equal "firstauthor" (plist-get (car threads) :creator)))
    (should (equal "lastauthor"  (plist-get (car threads) :last-commentor)))))

;;; magit-dash-gh-pr--thread-table

(ert-deftest magit-dash-gh-pr/thread-table-resolved ()
  (let* ((th  '(:id "t1" :resolved t :path "x.el" :creator "a" :last-commentor "b"))
         (ht  (magit-dash-gh-pr--thread-table th)))
    (should (equal "t1"  (map-elt ht "id")))
    (should (eq    t     (map-elt ht "resolved")))
    (should (equal "x.el" (map-elt ht "path")))
    (should (equal "a"   (map-elt ht "creator")))
    (should (equal "b"   (map-elt ht "last_commentor")))))

(ert-deftest magit-dash-gh-pr/thread-table-unresolved ()
  (let* ((th '(:id "t2" :resolved nil :path "" :creator "c" :last-commentor "d"))
         (ht (magit-dash-gh-pr--thread-table th)))
    (should (eq :false (map-elt ht "resolved")))))


;;; has_unresolved_threads derivation

(ert-deftest magit-dash-gh-pr/has-unresolved-all-resolved ()
  (let ((threads '((:resolved t) (:resolved t))))
    (should-not (seq-some (lambda (th) (not (plist-get th :resolved))) threads))))

(ert-deftest magit-dash-gh-pr/has-unresolved-one-unresolved ()
  (let ((threads '((:resolved t) (:resolved nil) (:resolved t))))
    (should (seq-some (lambda (th) (not (plist-get th :resolved))) threads))))

(ert-deftest magit-dash-gh-pr/has-unresolved-empty ()
  (should-not (seq-some (lambda (th) (not (plist-get th :resolved))) nil)))

;;; Pipeline: step-finalize index structure

(ert-deftest magit-dash-gh-pr/step-finalize-writes-index ()
  (magit-dash-gh-pr-test/with-temp-dir
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
      (magit-dash-gh-pr--step-finalize ctx)
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

(ert-deftest magit-dash-gh-pr/step-finalize-all-resolved ()
  "When all threads are resolved, has_unresolved_threads is :false."
  (magit-dash-gh-pr-test/with-temp-dir
    (let* ((pr-info `((number . 1) (title . "Done") (state . "MERGED")
                      (author . ((login . "alice"))) (headRefName . "main")
                      (reviewDecision . "APPROVED")))
           (threads '((:resolved t) (:resolved t)))
           (ctx (list :dir dir :pr-info pr-info :threads threads :files nil)))
      (magit-dash-gh-pr--step-finalize ctx)
      (let* ((raw (with-temp-buffer
                    (insert-file-contents (expand-file-name "index.json" dir))
                    (buffer-string)))
             (index (json-parse-string raw :object-type 'alist)))
        (should (eq :false (map-elt index 'has_unresolved_threads)))))))

(ert-deftest magit-dash-gh-pr/step-finalize-no-threads ()
  "When there are no threads, has_unresolved_threads is :false."
  (magit-dash-gh-pr-test/with-temp-dir
    (let* ((pr-info `((number . 2) (title . "Simple") (state . "OPEN")
                      (author . ((login . "bob"))) (headRefName . "feat")
                      (reviewDecision . nil)))
           (ctx (list :dir dir :pr-info pr-info :threads nil :files nil)))
      (magit-dash-gh-pr--step-finalize ctx)
      (let* ((raw (with-temp-buffer
                    (insert-file-contents (expand-file-name "index.json" dir))
                    (buffer-string)))
             (index (json-parse-string raw :object-type 'alist)))
        (should (eq :false (map-elt index 'has_unresolved_threads)))))))

;;; Pipeline: step-threads mocking

(ert-deftest magit-dash-gh-pr/step-threads-calls-gh ()
  "step-threads invokes gh api graphql with correct args."
  (magit-dash-gh-pr-test/with-temp-dir
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
                ((symbol-function 'magit-dash-gh-pr--step-comments)
                 (lambda (_ctx) (setq comments-called t))))
        (magit-dash-gh-pr--step-threads ctx)
        (should comments-called)
        (should (member "graphql" gh-args))
        (should (file-exists-p (expand-file-name "pr-review-threads.json" dir)))))))

;;; magit-dash-gh-pr-dashboard--find-local-path

(defvar magit-dash-repo-list)

(defun magit-dash-gh-pr-test/make-repos (&rest path-pairs)
  "Return a list of magit-dash-repo structs from PATH-PAIRS (name path ...)."
  (let (repos)
    (while path-pairs
      (push (magit-dash-repo--make :name (pop path-pairs) :path (pop path-pairs)) repos))
    (nreverse repos)))

(ert-deftest magit-dash-gh-pr/find-local-path-match ()
  "Returns path when repo name matches a registered entry's directory."
  (let ((magit-dash-repo-list
         (magit-dash-gh-pr-test/make-repos "test-1" "/home/user/projects/myrepo"
                                      "test-2" "/home/user/projects/other-repo")))
    (should (equal "/home/user/projects/myrepo"
                   (magit-dash-gh-pr-dashboard--find-local-path "owner/myrepo")))))

(ert-deftest magit-dash-gh-pr/find-local-path-no-match ()
  "Returns nil when no registered entry matches."
  (let ((magit-dash-repo-list
         (magit-dash-gh-pr-test/make-repos "test" "/home/user/projects/myrepo")))
    (should-not (magit-dash-gh-pr-dashboard--find-local-path "owner/unknown"))))

(ert-deftest magit-dash-gh-pr/find-local-path-empty-list ()
  "Returns nil when magit-dash-repo-list is empty."
  (let ((magit-dash-repo-list nil))
    (should-not (magit-dash-gh-pr-dashboard--find-local-path "owner/repo"))))

;;; magit-dash-gh-pr-dashboard-fetch-ci

(ert-deftest magit-dash-gh-pr/dashboard-fetch-ci-calls-ci-fetch ()
  "fetch-ci extracts pr-number and local path, then calls magit-dash-gh-actions-fetch-for-pr."
  (let ((fetch-args nil))
    (cl-letf (((symbol-function 'magit-dash-gh-pr-dashboard--entry-at-point)
               (lambda () '(:number 42 :repo "owner/myrepo" :pr nil)))
              ((symbol-function 'magit-dash-gh-pr-dashboard--find-local-path)
               (lambda (_) "/repos/myrepo"))
              ((symbol-function 'magit-dash-gh-actions-fetch-for-pr)
               (lambda (num path) (setq fetch-args (list num path)))))
      (magit-dash-gh-pr-dashboard-fetch-ci)
      (should (equal '(42 "/repos/myrepo") fetch-args)))))

(ert-deftest magit-dash-gh-pr/dashboard-fetch-ci-no-local-path ()
  "fetch-ci signals user-error when no local checkout is registered."
  (cl-letf (((symbol-function 'magit-dash-gh-pr-dashboard--entry-at-point)
             (lambda () '(:number 5 :repo "owner/unknown-repo" :pr nil)))
            ((symbol-function 'magit-dash-gh-pr-dashboard--find-local-path)
             (lambda (_) nil)))
    (should-error (magit-dash-gh-pr-dashboard-fetch-ci) :type 'user-error)))

;;; magit-gh-pr-dash and worktree operations

(ert-deftest magit-dash-gh-pr/dash-creates-buffer ()
  "magit-gh-pr-dash creates *magit-gh-pr-dash* buffer."
  (cl-letf (((symbol-function 'magit-dash-gh--check-gh) #'ignore)
            ((symbol-function 'magit-dash-gh-pr-dashboard-refresh) #'ignore))
    (unwind-protect
        (let ((buf (get-buffer-create magit-dash-gh-pr-buffer-name)))
          (magit-gh-pr-dash)
          (should (equal (buffer-name (get-buffer magit-dash-gh-pr-buffer-name)) "*magit-gh-pr-dash*"))
          (with-current-buffer buf
            (should (eq major-mode 'magit-dash-gh-pr-dashboard-mode))))
      (when-let* ((buf (get-buffer magit-dash-gh-pr-buffer-name)))
        (kill-buffer buf)))))

(ert-deftest magit-dash-gh-pr/default-filters-aggregate-all-open ()
  "magit-dash-gh-pr-dashboard-mode sets default filters with :author nil to aggregate all open PRs."
  (with-temp-buffer
    (magit-dash-gh-pr-dashboard-mode)
    (let ((filters magit-dash-gh-pr-dashboard--filters))
      (should (equal "open" (plist-get filters :state)))
      (should-not (plist-get filters :author)))))

(ert-deftest magit-dash-gh-pr/open-worktree-opens-existing ()
  "open-worktree switches directly to existing worktree."
  (let ((setup-path nil))
    (cl-letf (((symbol-function 'magit-dash-gh-pr-dashboard--entry-at-point)
               (lambda () '(:number 42 :repo "owner/myrepo" :pr nil)))
              ((symbol-function 'magit-dash-gh-pr-dashboard--find-local-path)
               (lambda (_) "/tmp/myrepo"))
              ((symbol-function 'file-directory-p)
               (lambda (dir) (string-suffix-p ".wt/pr-42" dir)))
              ((symbol-function 'magit-status-setup-buffer)
               (lambda (path) (setq setup-path path))))
      (magit-dash-gh-pr-dashboard-open-worktree)
      (should (equal (expand-file-name "/tmp/myrepo/.wt/pr-42") setup-path)))))

(ert-deftest magit-dash-gh-pr/open-worktree-fetches-and-creates ()
  "open-worktree runs git fetch and git worktree add when worktree does not yet exist."
  (let ((git-calls nil)
        (setup-path nil))
    (cl-letf (((symbol-function 'magit-dash-gh-pr-dashboard--entry-at-point)
               (lambda () '(:number 42 :repo "owner/myrepo" :pr nil)))
              ((symbol-function 'magit-dash-gh-pr-dashboard--find-local-path)
               (lambda (_) "/tmp/myrepo"))
              ((symbol-function 'file-directory-p) (lambda (_) nil))
              ((symbol-function 'magit-call-git)
               (lambda (&rest args) (push args git-calls) 0))
              ((symbol-function 'magit-status-setup-buffer)
               (lambda (path) (setq setup-path path))))
      (magit-dash-gh-pr-dashboard-open-worktree)
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
;;; magit-dash-gh-pr-dashboard--build-format

(ert-deftest magit-dash-gh-pr/build-format-caps-repo-width-with-entries ()
  "Repo column width is capped at the longest repo name + 3."
  (let* ((entries '(((:repo "owner/repo-short") ["repo-short" "1" "pass" "Title" "·" "1d" "0" "approved"])
                    ((:repo "owner/a-much-longer-repository-name") ["a-much-longer-repository-name" "2" "pass" "Title" "·" "1d" "0" "approved"])))
         (fmt (magit-dash-gh-pr-dashboard--build-format 150 entries)))
    (should (equal "Repo" (car (aref fmt 0))))
    (should (= (+ (length "a-much-longer-repository-name") 3)
               (cadr (aref fmt 0))))))

(ert-deftest magit-dash-gh-pr/build-format-show-org ()
  "When show-org is non-nil, build-format includes Org and Repo columns, followed by PR#, CI, Title, WT, Updated."
  (let* ((magit-dash-gh-pr-show-org t)
         (entries '(((:repo "casaphq/my-repo") ["casaphq" "my-repo" "1" "pass" "Title" "·" "1d" "0" "approved"])))
         (fmt (magit-dash-gh-pr-dashboard--build-format 150 entries)))
    (should (equal "Org" (car (aref fmt 0))))
    (should (equal "Repo" (car (aref fmt 1))))
    (should (equal "PR#" (car (aref fmt 2))))
    (should (equal "CI" (car (aref fmt 3))))
    (should (equal "Title" (car (aref fmt 4))))
    (should (equal "WT" (car (aref fmt 5))))
    (should (equal "Updated" (car (aref fmt 6))))))

(ert-deftest magit-dash-gh-pr/build-format-fallback-no-entries ()
  "When no entries exist, Repo column falls back to available window space (min 12)."
  (let ((fmt (magit-dash-gh-pr-dashboard--build-format 100 nil)))
    (should (= (- 100 77) (cadr (aref fmt 0))))))

(ert-deftest magit-dash-gh-pr/split-slug ()
  "Split slug correctly extracts owner and repo."
  (should (equal '("owner" . "repo") (magit-dash-gh-pr--split-slug "owner/repo")))
  (should (equal '("" . "standalone") (magit-dash-gh-pr--split-slug "standalone"))))

(ert-deftest magit-dash-gh-pr/worktree-detection ()
  "Worktree detection checks local checkout and worktree directory."
  (let ((magit-dash-repo-list (magit-dash-gh-pr-test/make-repos "test-repo" "/tmp/test-repo")))
    (should (equal (expand-file-name "/tmp/test-repo/.wt/pr-10")
                   (magit-dash-gh-pr-dashboard--worktree-path '(:number 10 :repo "owner/test-repo"))))
    (cl-letf (((symbol-function 'file-directory-p)
               (lambda (path) (string-suffix-p ".wt/pr-10" path))))
      (should (magit-dash-gh-pr-dashboard--worktree-exists-p '(:number 10 :repo "owner/test-repo")))
      (should-not (magit-dash-gh-pr-dashboard--worktree-exists-p '(:number 11 :repo "owner/test-repo"))))))

(ert-deftest magit-dash-gh-pr/open-dired ()
  "open-dired opens Dired in worktree directory."
  (let ((opened-dir nil))
    (cl-letf (((symbol-function 'magit-dash-gh-pr-dashboard--entry-at-point)
               (lambda () '(:number 42 :repo "owner/myrepo" :pr nil)))
              ((symbol-function 'magit-dash-gh-pr-dashboard--worktree-path)
               (lambda (_) "/tmp/myrepo-pr-42"))
              ((symbol-function 'file-directory-p)
               (lambda (_) t))
              ((symbol-function 'dired)
               (lambda (d) (setq opened-dir d))))
      (magit-dash-gh-pr-dashboard-open-dired)
      (should (equal "/tmp/myrepo-pr-42" opened-dir)))))


;;; magit-dash-gh-pr--parse-status-rollup

(ert-deftest magit-dash-gh-pr/parse-status-rollup-nil ()
  "Empty or nil rollup returns nil."
  (should-not (magit-dash-gh-pr--parse-status-rollup nil))
  (should-not (magit-dash-gh-pr--parse-status-rollup []))
  (should-not (magit-dash-gh-pr--parse-status-rollup '())))

(ert-deftest magit-dash-gh-pr/parse-status-rollup-legacy-state ()
  "Single alist with state field returns that state."
  (should (equal "SUCCESS" (magit-dash-gh-pr--parse-status-rollup '((state . "SUCCESS")))))
  (should (equal "FAILURE" (magit-dash-gh-pr--parse-status-rollup '((state . "failure"))))))

(ert-deftest magit-dash-gh-pr/parse-status-rollup-success ()
  "All successful check runs produce SUCCESS."
  (let ((rollup (list '((status . "COMPLETED") (conclusion . "SUCCESS"))
                      '((status . "COMPLETED") (conclusion . "SKIPPED")))))
    (should (equal "SUCCESS" (magit-dash-gh-pr--parse-status-rollup rollup)))))

(ert-deftest magit-dash-gh-pr/parse-status-rollup-failure-priority ()
  "Failure takes priority over pending or success."
  (let ((rollup (list '((status . "COMPLETED") (conclusion . "SUCCESS"))
                      '((status . "IN_PROGRESS") (conclusion . ""))
                      '((status . "COMPLETED") (conclusion . "FAILURE")))))
    (should (equal "FAILURE" (magit-dash-gh-pr--parse-status-rollup rollup)))))

(ert-deftest magit-dash-gh-pr/parse-status-rollup-pending-priority ()
  "Pending takes priority over success when there are no failures."
  (let ((rollup (list '((status . "COMPLETED") (conclusion . "SUCCESS"))
                      '((status . "IN_PROGRESS") (conclusion . "")))))
    (should (equal "PENDING" (magit-dash-gh-pr--parse-status-rollup rollup)))))

(ert-deftest magit-dash-gh-pr/refresh-does-not-clear-entries-while-fetching ()
  "Calling `magit-dash-gh-pr-dashboard-refresh' does not clear `tabulated-list-entries' before fetch completes."
  (with-temp-buffer
    (magit-dash-gh-pr-dashboard-mode)
    (setq tabulated-list-entries '(((:repo "o/r" :number 1) ["o/r" "1" "pass" "T" "·" "1d" "0" "approved"])))
    (cl-letf (((symbol-function 'magit-dash-gh--run-process)
               (lambda (_args _dir _on-success &optional _on-error)
                 nil)))
      (magit-dash-gh-pr-dashboard-refresh)
      (should (= 1 (length tabulated-list-entries))))))


;;; Round 3: View switcher, window split, and quit tests

(ert-deftest magit-dash-gh-pr/build-args-review-requested ()
  "build-args includes --review-requested when specified."
  (let ((args (magit-dash-gh-pr-dashboard--build-args
               (list :state "open" :review-requested "@me"))))
    (should (member "--review-requested" args))
    (should (member "@me" args))))

(ert-deftest magit-dash-gh-pr/switch-view-presets ()
  "switch-view updates filters according to preset view keys."
  (let ((refreshed nil)
        (magit-dash-gh-pr-dashboard--filters nil))
    (cl-letf (((symbol-function 'magit-dash-gh-pr-dashboard-refresh)
               (lambda () (setq refreshed t))))
      ;; 1. my-prs
      (magit-dash-gh-pr-dashboard-switch-view "my-prs")
      (should (equal "@me" (plist-get magit-dash-gh-pr-dashboard--filters :author)))
      (should (equal "open" (plist-get magit-dash-gh-pr-dashboard--filters :state)))
      (should refreshed)
      ;; 2. needs-review
      (setq refreshed nil)
      (magit-dash-gh-pr-dashboard-switch-view "needs-review")
      (should (equal "@me" (plist-get magit-dash-gh-pr-dashboard--filters :review-requested)))
      ;; 3. closed
      (magit-dash-gh-pr-dashboard-switch-view "closed")
      (should (equal "closed" (plist-get magit-dash-gh-pr-dashboard--filters :state))))))

(ert-deftest magit-dash-gh-pr/quit-window-deletes-split ()
  "quit buries buffer and deletes window when multiple windows exist."
  (let ((buried nil)
        (deleted nil))
    (cl-letf (((symbol-function 'window-list)
               (lambda (&rest _) '(win1 win2)))
              ((symbol-function 'bury-buffer)
               (lambda (&rest _) (setq buried t)))
              ((symbol-function 'delete-window)
               (lambda (&rest _) (setq deleted t))))
      (magit-dash-gh-pr-dashboard-quit)
      (should buried)
      (should deleted))))


;;; Column toggling tests

(ert-deftest magit-dash-gh-pr/columns-defaults ()
  "PR dashboard has expected default columns enabled, disabled, and ordered."
  (should (eq nil (alist-get 'org magit-dash-gh-pr-columns)))
  (should (eq t (alist-get 'repo magit-dash-gh-pr-columns)))
  (should (eq t (alist-get 'pr magit-dash-gh-pr-columns)))
  (should (eq t (alist-get 'ci magit-dash-gh-pr-columns)))
  (should (eq t (alist-get 'title magit-dash-gh-pr-columns)))
  (should (eq t (alist-get 'worktree magit-dash-gh-pr-columns)))
  (should (eq t (alist-get 'updated magit-dash-gh-pr-columns)))
  (should (equal '(repo pr ci title worktree updated comments review)
                 (magit-dash-gh-pr--active-columns)))
  (magit-dash-global-mode 1)
  (should (memq 'magit-dash-gh-pr-columns savehist-additional-variables)))

(ert-deftest magit-dash-gh-pr/toggle-column ()
  "Toggling a column flips its enabled state and syncs show-org."
  (let ((magit-dash-gh-pr-columns
         '((org . nil) (repo . t) (pr . t) (ci . t) (title . t) (worktree . t) (updated . t) (age . nil) (comments . t) (review . t)))
        (magit-dash-gh-pr-show-org nil))
    (cl-letf (((symbol-function 'magit-dash-gh-pr-dashboard-refresh) #'ignore))
      (magit-dash-gh-pr-toggle-column 'org)
      (should (eq t (magit-dash-gh-pr--column-enabled-p 'org)))
      (should (eq t magit-dash-gh-pr-show-org))
      (magit-dash-gh-pr-toggle-column 'ci)
      (should-not (magit-dash-gh-pr--column-enabled-p 'ci)))))

(ert-deftest magit-dash-gh-pr/build-entry-respects-arbitrary-columns ()
  "build-entry outputs only the active columns."
  (let ((magit-dash-gh-pr-columns
         '((org . nil) (repo . t) (pr . t) (ci . nil) (title . nil) (worktree . nil) (updated . nil) (age . nil) (comments . nil) (review . nil)))
        (pr '((number . 10) (title . "T") (updatedAt . "") (comments . 0))))
    (let* ((entry (magit-dash-gh-pr-dashboard--build-entry pr "owner/repo"))
           (vec (cadr entry)))
      (should (= 2 (length vec)))
      (should (equal "repo" (aref vec 0)))
      (should (equal "10" (aref vec 1))))))


(ert-deftest magit-dash-gh-pr/worktree-path-format-options ()
  "Test custom worktree path formatting options: sibling, dot-wt, and custom string."
  (let ((entry '(:number 42 :repo "owner/myrepo" :pr nil)))
    (cl-letf (((symbol-function 'magit-dash-gh-pr-dashboard--find-local-path)
               (lambda (_) "/tmp/myrepo"))
              ((symbol-function 'file-directory-p) (lambda (_) nil)))
      ;; Default: sibling
      (let ((magit-dash-gh-pr-worktree-path-format 'sibling))
        (should (equal (expand-file-name "/tmp/myrepo-pr-42")
                       (magit-dash-gh-pr-dashboard--worktree-path entry))))
      ;; dot-wt
      (let ((magit-dash-gh-pr-worktree-path-format 'dot-wt))
        (should (equal (expand-file-name "/tmp/myrepo/.wt/pr-42")
                       (magit-dash-gh-pr-dashboard--worktree-path entry))))
      ;; Custom string template
      (let ((magit-dash-gh-pr-worktree-path-format "%r/.worktrees/%n"))
        (should (equal (expand-file-name "/tmp/myrepo/.worktrees/pr-42")
                       (magit-dash-gh-pr-dashboard--worktree-path entry)))))))

(ert-deftest magit-dash-gh-pr/open-worktree-mirrors-remote-branch ()
  "open-worktree uses headRefName as branch name while keeping worktree name as pr-NUM."
  (let ((git-calls nil)
        (setup-path nil))
    (cl-letf (((symbol-function 'magit-dash-gh-pr-dashboard--entry-at-point)
               (lambda () '(:number 42 :repo "owner/myrepo"
                            :pr ((headRefName . "feature/cool-idea")))))
              ((symbol-function 'magit-dash-gh-pr-dashboard--find-local-path)
               (lambda (_) "/tmp/myrepo"))
              ((symbol-function 'file-directory-p) (lambda (_) nil))
              ((symbol-function 'magit-call-git)
               (lambda (&rest args) (push args git-calls) 0))
              ((symbol-function 'magit-status-setup-buffer)
               (lambda (path) (setq setup-path path))))
      (let ((magit-dash-gh-pr-worktree-path-format 'dot-wt))
        (magit-dash-gh-pr-dashboard-open-worktree)
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

(ert-deftest magit-dash-gh-pr/worktree-detection-finds-dot-wt ()
  "worktree-exists-p detects existing worktrees in .wt directory."
  (let ((entry '(:number 10 :repo "owner/test-repo" :pr nil)))
    (cl-letf (((symbol-function 'magit-dash-gh-pr-dashboard--find-local-path)
               (lambda (_) "/tmp/test-repo"))
              ((symbol-function 'file-directory-p)
               (lambda (path)
                 (member (directory-file-name path)
                         (list (expand-file-name "/tmp/test-repo")
                               (expand-file-name "/tmp/test-repo/.wt/pr-10"))))))
      (should (magit-dash-gh-pr-dashboard--worktree-exists-p entry)))))

(ert-deftest magit-dash-gh-pr/build-entry-updated-column ()
  "build-entry populates columns in order: repo, pr, ci, title, worktree, updated, comments, review."
  (let* ((pr '((number . 15)
               (title . "Fix stuff")
               (updatedAt . "2024-06-01T12:00:00Z")
               (headRefName . "bugfix/issue-15")
               (comments . 1)
               (reviewDecision . nil)
               (statusCheckRollup . ((state . "FAILURE")))
               (url . "https://example.com/15")))
         (entry (magit-dash-gh-pr-dashboard--build-entry pr "owner/myrepo"))
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

(ert-deftest magit-dash-gh-pr/dashboard-create-worktree-no-switch ()
  "dashboard-create-worktree creates worktree without opening magit status when switch is nil."
  (let ((git-calls nil)
        (setup-called nil))
    (cl-letf (((symbol-function 'magit-dash-gh-pr-dashboard--entry-at-point)
               (lambda () '(:number 42 :repo "owner/myrepo" :pr nil)))
              ((symbol-function 'magit-dash-gh-pr-dashboard--find-local-path)
               (lambda (_) "/tmp/myrepo"))
              ((symbol-function 'file-directory-p) (lambda (_) nil))
              ((symbol-function 'magit-call-git)
               (lambda (&rest args) (push args git-calls) 0))
              ((symbol-function 'magit-status-setup-buffer)
               (lambda (_) (setq setup-called t))))
      (let ((res (magit-dash-gh-pr-dashboard-create-worktree nil nil)))
        (should (equal (expand-file-name "/tmp/myrepo/.wt/pr-42") res))
        (should-not setup-called)
        (should (seq-some (lambda (call)
                            (and (equal (nth 0 call) "worktree")
                                 (equal (nth 1 call) "add")))
                          git-calls))))))

(ert-deftest magit-dash-gh-pr/dashboard-mode-map-keys ()
  "magit-dash-gh-pr-dashboard-mode-map uses two-letter combinations for cw, ca, ci and mark keys."
  (let ((m magit-dash-gh-pr-dashboard-mode-map))
    (should (eq (lookup-key m (kbd "c w")) #'magit-dash-gh-pr-dashboard-create-worktree))
    (should (eq (lookup-key m (kbd "c a")) #'magit-dash-gh-pr-dashboard-create-all-worktrees))
    (should (eq (lookup-key m (kbd "c i")) #'magit-dash-gh-pr-dashboard-fetch-ci))
    (should (eq (lookup-key m (kbd "m m")) #'magit-dash-gh-pr-dashboard-toggle-mark))
    (should (eq (lookup-key m (kbd "m u")) #'magit-dash-gh-pr-dashboard-unmark))
    (should (eq (lookup-key m (kbd "m c")) #'magit-dash-gh-pr-dashboard-unmark-all))
    (should (eq (lookup-key m (kbd "m a")) #'magit-dash-gh-pr-dashboard-mark-all))
    (should (eq (lookup-key m (kbd "SPC")) #'magit-dash-gh-pr-dashboard-toggle-mark))
    (should (eq (lookup-key m (kbd "*")) #'magit-dash-gh-pr-dashboard-mark-all))
    (should (eq (lookup-key m (kbd "a")) #'magit-dash-gh-pr-dashboard-create-all-worktrees))))

(ert-deftest magit-dash-gh-pr/dashboard-menu-includes-worktree-options ()
  "magit-dash-gh-pr-dashboard-menu contains create, create-all, review, clean, and mark commands."
  (let ((layout (get 'magit-dash-gh-pr-dashboard-menu 'transient--layout)))
    (should layout)
    (let ((flat (format "%S" layout)))
      (should (string-match-p "magit-dash-gh-pr-dashboard-create-worktree" flat))
      (should (string-match-p "magit-dash-gh-pr-dashboard-create-all-worktrees" flat))
      (should (string-match-p "magit-dash-gh-pr-dashboard-review-worktrees" flat))
      (should (string-match-p "magit-dash-gh-pr-dashboard-clean-merged-worktrees" flat))
      (should (string-match-p "magit-dash-gh-pr-dashboard-toggle-mark" flat))
      (should (string-match-p "magit-dash-gh-pr-dashboard-unmark-all" flat)))))

(ert-deftest magit-dash-gh-pr/delete-worktree-refuses-on-uncommitted ()
  "delete-worktree refuses to remove worktree if uncommitted changes exist."
  (cl-letf (((symbol-function 'magit-dash-worktree-uncommitted-changes-p)
             (lambda (_) t)))
    (should-error (magit-dash-gh-pr-delete-worktree "/tmp/myrepo/.wt/pr-42" "/tmp/myrepo")
                  :type 'user-error)))

(ert-deftest magit-dash-gh-pr/delete-worktree-removes-clean ()
  "delete-worktree removes clean worktree via git worktree remove and prunes."
  (let ((git-calls nil))
    (cl-letf (((symbol-function 'magit-dash-worktree-uncommitted-changes-p)
               (lambda (_) nil))
              ((symbol-function 'call-process)
               (lambda (prog &rest args)
                 (when (equal prog "git")
                   (push args git-calls))
                 0)))
      (should (magit-dash-gh-pr-delete-worktree "/tmp/myrepo/.wt/pr-42" "/tmp/myrepo"))
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

(ert-deftest magit-dash-gh-pr/review-worktrees-prompts-and-renames-mismatched ()
  "review-worktrees prompts to rename worktree when path does not match PR format."
  (let ((git-calls nil)
        (prompted nil)
        (prs '(((number . 42) (headRefName . "feature-42") (state . "OPEN"))))
        (wts '(((path . "/tmp/myrepo-pr-42") (branch . "feature-42")))))
    (cl-letf (((symbol-function 'magit-dash-gh-pr--git-worktrees) (lambda (_) wts))
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
      (let ((res (magit-dash-gh-pr-review-worktrees "/tmp/myrepo" t prs)))
        (should prompted)
        (should (= 1 (plist-get res :renamed)))
        (should (seq-some (lambda (args)
                            (and (member "worktree" args)
                                 (member "move" args)))
                          git-calls))))))

(ert-deftest magit-dash-gh-pr/review-worktrees-skips-matching ()
  "review-worktrees counts matching worktree without prompt."
  (let ((prompted nil)
        (prs '(((number . 42) (headRefName . "feature-42") (state . "OPEN"))))
        (wts `(((path . ,(expand-file-name "/tmp/myrepo/.wt/pr-42")) (branch . "feature-42")))))
    (cl-letf (((symbol-function 'magit-dash-gh-pr--git-worktrees) (lambda (_) wts))
              ((symbol-function 'file-directory-p) (lambda (_) t))
              ((symbol-function 'y-or-n-p) (lambda (_) (setq prompted t) t)))
      (let ((res (magit-dash-gh-pr-review-worktrees "/tmp/myrepo" t prs)))
        (should-not prompted)
        (should (= 1 (plist-get res :matching)))
        (should (= 0 (plist-get res :renamed)))))))

(ert-deftest magit-dash-gh-pr/review-worktrees-skips-merged-with-uncommitted ()
  "review-worktrees skips merged PR worktree if it has uncommitted changes."
  (let ((deleted nil)
        (prs '(((number . 42) (headRefName . "pr-42") (state . "MERGED"))))
        (wts `(((path . ,(expand-file-name "/tmp/myrepo/.wt/pr-42")) (branch . "pr-42")))))
    (cl-letf (((symbol-function 'magit-dash-gh-pr--git-worktrees) (lambda (_) wts))
              ((symbol-function 'file-directory-p) (lambda (_) t))
              ((symbol-function 'magit-dash-worktree-uncommitted-changes-p) (lambda (_) t))
              ((symbol-function 'magit-dash-gh-pr-delete-worktree) (lambda (&rest _) (setq deleted t))))
      (let ((res (magit-dash-gh-pr-review-worktrees "/tmp/myrepo" t prs)))
        (should-not deleted)
        (should (= 1 (plist-get res :skipped)))
        (should (= 0 (plist-get res :deleted)))))))

(ert-deftest magit-dash-gh-pr/review-worktrees-deletes-merged-clean ()
  "review-worktrees deletes clean worktree for merged PR."
  (let ((deleted nil)
        (prs '(((number . 42) (headRefName . "pr-42") (state . "MERGED"))))
        (wts `(((path . ,(expand-file-name "/tmp/myrepo/.wt/pr-42")) (branch . "pr-42")))))
    (cl-letf (((symbol-function 'magit-dash-gh-pr--git-worktrees) (lambda (_) wts))
              ((symbol-function 'file-directory-p) (lambda (_) t))
              ((symbol-function 'magit-dash-worktree-uncommitted-changes-p) (lambda (_) nil))
              ((symbol-function 'y-or-n-p) (lambda (_) t))
              ((symbol-function 'magit-dash-gh-pr-delete-worktree) (lambda (&rest _) (setq deleted t))))
      (let ((res (magit-dash-gh-pr-review-worktrees "/tmp/myrepo" t prs)))
        (should deleted)
        (should (= 1 (plist-get res :deleted)))))))

(ert-deftest magit-dash-gh-pr/pr-merged-p-handles-null-and-cases ()
  "magit-dash-gh-pr--pr-merged-p handles :null mergedAt, lowercase state, and open PRs."
  ;; Open PR with :null mergedAt must NOT be considered merged
  (should-not (magit-dash-gh-pr--pr-merged-p '((number . 1) (state . "OPEN") (mergedAt . :null))))
  ;; Closed PR with :null mergedAt and no gh fallback is not merged
  (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
    (should-not (magit-dash-gh-pr--pr-merged-p '((number . 2) (state . "CLOSED") (mergedAt . :null)) "o/r" 2)))
  ;; Merged PR by state (uppercase or lowercase)
  (should (magit-dash-gh-pr--pr-merged-p '((number . 3) (state . "MERGED") (mergedAt . :null))))
  (should (magit-dash-gh-pr--pr-merged-p '((number . 4) (state . "merged"))))
  ;; Merged PR by mergedAt timestamp even when state is "closed" (from gh search prs)
  (should (magit-dash-gh-pr--pr-merged-p '((number . 5) (state . "closed") (mergedAt . "2026-01-01T00:00:00Z")))))

(ert-deftest magit-dash-gh-pr/clean-merged-worktrees ()
  "clean-merged-worktrees deletes clean merged worktrees and skips dirty ones."
  (let ((deleted-paths nil)
        (prs '(((number . 1) (headRefName . "pr-1") (state . "MERGED"))
               ((number . 2) (headRefName . "pr-2") (state . "MERGED"))
               ((number . 3) (headRefName . "pr-3") (state . "OPEN") (mergedAt . :null))))
        (wts `(((path . ,(expand-file-name "/tmp/myrepo/.wt/pr-1")) (branch . "pr-1"))
               ((path . ,(expand-file-name "/tmp/myrepo/.wt/pr-2")) (branch . "pr-2"))
               ((path . ,(expand-file-name "/tmp/myrepo/.wt/pr-3")) (branch . "pr-3")))))
    (cl-letf (((symbol-function 'magit-dash-gh-pr--fetch-repo-prs) (lambda (&rest _) prs))
              ((symbol-function 'magit-dash-gh-pr--git-worktrees) (lambda (_) wts))
              ((symbol-function 'file-directory-p) (lambda (_) t))
              ((symbol-function 'magit-dash-worktree-uncommitted-changes-p)
               (lambda (path) (string-suffix-p "pr-2" path)))
              ((symbol-function 'y-or-n-p) (lambda (_) t))
              ((symbol-function 'magit-dash-gh-pr-delete-worktree)
               (lambda (path &optional _) (push path deleted-paths))))
      (let ((res (magit-dash-gh-pr-clean-merged-worktrees "/tmp/myrepo" t)))
        (should (equal (list (expand-file-name "/tmp/myrepo/.wt/pr-1")) deleted-paths))
        (should (= 1 (plist-get res :deleted)))
        (should (= 1 (plist-get res :skipped)))))))

(ert-deftest magit-dash-gh-pr/clean-merged-protects-open-pr-on-reused-branch ()
  "An open PR protects its worktree even when an older PR was merged."
  (let ((prs '(((number . 1) (headRefName . "shared") (state . "MERGED"))
               ((number . 2) (headRefName . "shared") (state . "OPEN"))))
        (wts '(((path . "/tmp/wt") (branch . "shared"))))
        deleted)
    (cl-letf (((symbol-function 'magit-dash-gh-pr--fetch-repo-prs) (lambda (&rest _) prs))
              ((symbol-function 'magit-dash-gh-pr--git-worktrees) (lambda (_) wts))
              ((symbol-function 'file-directory-p) (lambda (_) t))
              ((symbol-function 'magit-dash-gh-pr-delete-worktree) (lambda (&rest _) (setq deleted t))))
      (magit-dash-gh-pr-clean-merged-worktrees "/tmp/repo" nil)
      (should-not deleted))))

(ert-deftest magit-dash-gh-pr/clean-entry-skips-open-pr ()
  "Marked open PRs are never deleted by merged cleanup."
  (let ((entry '(:number 2 :repo "o/r" :pr ((number . 2) (state . "OPEN"))))
        deleted)
    (cl-letf (((symbol-function 'magit-dash-gh-pr-dashboard--find-local-path) (lambda (_) "/tmp/repo"))
              ((symbol-function 'magit-dash-gh-pr--find-existing-worktree) (lambda (&rest _) "/tmp/wt"))
              ((symbol-function 'file-directory-p) (lambda (_) t))
              ((symbol-function 'magit-dash-gh-pr-delete-worktree) (lambda (&rest _) (setq deleted t))))
      (should (eq :skipped (magit-dash-gh-pr--clean-worktree-for-entry entry nil)))
      (should-not deleted))))

(ert-deftest magit-dash-gh-pr/create-all-worktrees ()
  "create-all-worktrees reviews worktrees and creates worktree for each open PR."
  (let ((reviewed nil)
        (created nil)
        (prs '(((number . 10) (headRefName . "feat-10") (state . "OPEN"))
               ((number . 20) (headRefName . "feat-20") (state . "OPEN")))))
    (cl-letf (((symbol-function 'magit-dash-gh-pr--fetch-repo-prs) (lambda (&rest _) prs))
              ((symbol-function 'magit-dash-gh-pr-review-worktrees)
               (lambda (&rest _) (setq reviewed t)))
              ((symbol-function 'magit-dash-gh-pr--find-existing-worktree)
               (lambda (_dir num &optional _) (when (= num 10) "/tmp/myrepo/.wt/pr-10")))
              ((symbol-function 'magit-dash-gh-pr-dashboard-create-worktree)
               (lambda (entry &optional _) (push (plist-get entry :number) created))))
      (let ((res (magit-dash-gh-pr-create-all-worktrees "/tmp/myrepo")))
        (should reviewed)
        (should (equal '(20) created))
        (should (= 1 (plist-get res :created)))
        (should (= 1 (plist-get res :existing)))))))

(ert-deftest magit-dash-gh-pr/match-worktree-to-pr ()
  "Test matching worktrees to pull requests by branch and directory."
  (let ((prs '(((number . 10) (headRefName . "feat/login"))
               ((number . 20) (headRefName . "fix-bug"))
               ((number . 30) (headRefName . "other")))))
    (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t)))
      ;; Match by branch
      (let ((matched (magit-dash-gh-pr--match-worktree-to-pr
                      '((path . "/tmp/repo/.wt/custom") (branch . "feat/login"))
                      prs)))
        (should (= 10 (alist-get 'number matched))))

      ;; Match by directory name pr-NUM
      (let ((matched (magit-dash-gh-pr--match-worktree-to-pr
                      '((path . "/tmp/repo/.wt/pr-20") (branch . "detached"))
                      prs)))
        (should (= 20 (alist-get 'number matched))))

      ;; No match
      (let ((matched (magit-dash-gh-pr--match-worktree-to-pr
                      '((path . "/tmp/repo/.wt/random") (branch . "unknown"))
                      prs)))
        (should-not matched)))))

(ert-deftest magit-dash-gh-pr/path-equal-p ()
  "Test path equality comparisons."
  (should (magit-dash-gh-pr--path-equal-p "/tmp/foo/bar" "/tmp/foo/bar/"))
  (should (magit-dash-gh-pr--path-equal-p "/tmp/foo/bar/" "/tmp/foo/bar"))
  (should-not (magit-dash-gh-pr--path-equal-p "/tmp/foo/bar" "/tmp/foo/baz"))
  (should-not (magit-dash-gh-pr--path-equal-p nil "/tmp/foo/bar"))
  (should-not (magit-dash-gh-pr--path-equal-p "/tmp/foo/bar" nil)))

(ert-deftest magit-dash-gh-pr/move-worktree ()
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
      (let ((res (magit-dash-gh-pr--move-worktree "/tmp/repo" "/tmp/repo/old" "/tmp/repo/new")))
        (should res)
        (should (equal (list (expand-file-name "/tmp/repo/new") (expand-file-name "/tmp/repo"))
                       symlinked))
        (should (seq-some (lambda (call) (and (member "worktree" call) (member "move" call)))
                          git-calls))))))

(ert-deftest magit-dash-gh-pr/dashboard-create-worktree-renames-mismatched ()
  "dashboard-create-worktree prompts and renames existing worktree at non-matching path."
  (let* ((entry '(:number 42 :repo "org/myrepo" :pr ((number . 42) (headRefName . "feature"))))
         (moved nil)
         (prompted nil))
    (cl-letf (((symbol-function 'magit-dash-gh-pr-dashboard--find-local-path)
               (lambda (_) "/tmp/myrepo"))
              ((symbol-function 'file-directory-p) (lambda (_) t))
              ((symbol-function 'magit-dash-gh-pr--find-existing-worktree)
               (lambda (&rest _) "/tmp/myrepo/other-dir"))
              ((symbol-function 'y-or-n-p)
               (lambda (_) (setq prompted t) t))
              ((symbol-function 'magit-dash-gh-pr--move-worktree)
               (lambda (_repo old new) (setq moved (list old new)) t)))
      (let ((res (magit-dash-gh-pr-dashboard-create-worktree entry nil)))
        (should prompted)
        (should (equal (list (expand-file-name "/tmp/myrepo/other-dir")
                             (expand-file-name "/tmp/myrepo/.wt/pr-42"))
                       moved))
        (should (equal (expand-file-name "/tmp/myrepo/.wt/pr-42") res))))))

(ert-deftest magit-dash-gh-pr/dashboard-review-and-clean-commands ()
  "Test dashboard interactive wrappers for review and clean worktrees."
  (let ((reviewed nil)
        (cleaned nil)
        (entry '(:number 1 :repo "org/myrepo")))
    (cl-letf (((symbol-function 'magit-dash-gh-pr-dashboard--entry-at-point)
               (lambda () entry))
              ((symbol-function 'magit-dash-gh-pr-dashboard--find-local-path)
               (lambda (_) "/tmp/myrepo"))
              ((symbol-function 'magit-dash-gh-pr-review-worktrees)
               (lambda (path prompt) (setq reviewed (list path prompt))))
              ((symbol-function 'magit-dash-gh-pr-clean-merged-worktrees)
               (lambda (path prompt) (setq cleaned (list path prompt))))
              ((symbol-function 'magit-dash-gh-pr-dashboard-refresh) #'ignore))
      (magit-dash-gh-pr-dashboard-review-worktrees)
      (should (equal (list "/tmp/myrepo" t) reviewed))

      (magit-dash-gh-pr-dashboard-clean-merged-worktrees)
      (should (equal (list "/tmp/myrepo" t) cleaned)))))

(ert-deftest magit-dash-gh-pr/dashboard-clean-selected-merged-pr ()
  "When the selected PR at point is merged and has an existing worktree, clean-merged-worktrees cleans it directly."
  (let ((cleaned-entry nil)
        (entry '(:number 42 :repo "org/myrepo" :pr ((number . 42) (state . "MERGED")))))
    (cl-letf (((symbol-function 'magit-dash-gh-pr-dashboard--entry-at-point)
               (lambda () entry))
              ((symbol-function 'magit-dash-gh-pr-dashboard--find-local-path)
               (lambda (_) "/tmp/myrepo"))
              ((symbol-function 'magit-dash-gh-pr-dashboard--worktree-exists-p)
               (lambda (_) t))
              ((symbol-function 'magit-dash-gh-pr--clean-worktree-for-entry)
               (lambda (e prompt) (setq cleaned-entry (list e prompt)) :deleted))
              ((symbol-function 'magit-dash-gh-pr-dashboard-refresh) #'ignore))
      (magit-dash-gh-pr-dashboard-clean-merged-worktrees)
      (should (equal (list entry t) cleaned-entry)))))

(ert-deftest magit-dash-gh-pr/mark-and-bulk-operations ()
  "Test marking, unmarking, mark-all, unmark-all, and bulk create/clean worktree operations."
  (with-temp-buffer
    (magit-dash-gh-pr-dashboard-mode)
    (let* ((pr1 '((number . 10) (title . "First PR") (updatedAt . "") (comments . 0)))
           (pr2 '((number . 20) (title . "Second PR") (updatedAt . "") (comments . 0)))
           (e1 (magit-dash-gh-pr-dashboard--build-entry pr1 "org/repo1"))
           (e2 (magit-dash-gh-pr-dashboard--build-entry pr2 "org/repo2")))
      (setq tabulated-list-entries (list e1 e2))
      (tabulated-list-print t)
      (goto-char (point-min))
      ;; Toggle mark on first entry
      (magit-dash-gh-pr-dashboard-toggle-mark)
      (should (equal '(("org/repo1" . 10)) magit-dash-gh-pr-dashboard--marked-entries))
      ;; Verify visual indicator "*" on PR# column (index 1)
      (should (string-prefix-p "*" (substring-no-properties (aref (cadr (car tabulated-list-entries)) 1))))
      ;; Mark all
      (magit-dash-gh-pr-dashboard-mark-all)
      (should (= 2 (length magit-dash-gh-pr-dashboard--marked-entries)))
      ;; Bulk create worktrees for marked PRs
      (let ((created-nums nil))
        (cl-letf (((symbol-function 'magit-dash-gh-pr--create-worktree-for-entry)
                   (lambda (entry &optional _)
                     (push (plist-get entry :number) created-nums)
                     "/tmp/wt")))
          (magit-dash-gh-pr-dashboard-create-worktree)
          (should (equal '(20 10) created-nums))
          ;; Marks are cleared after bulk operation
          (should (null magit-dash-gh-pr-dashboard--marked-entries))))
      ;; Mark all again and test bulk clean
      (magit-dash-gh-pr-dashboard-mark-all)
      (let ((cleaned-nums nil))
        (cl-letf (((symbol-function 'magit-dash-gh-pr--clean-worktree-for-entry)
                   (lambda (entry &optional _)
                     (push (plist-get entry :number) cleaned-nums)
                     :deleted)))
          (magit-dash-gh-pr-dashboard-clean-merged-worktrees)
          (should (equal '(20 10) cleaned-nums))
          (should (null magit-dash-gh-pr-dashboard--marked-entries)))))))

(provide 'test-magit-dash-gh-pr)
;;; test-magit-dash-gh-pr.el ends here
