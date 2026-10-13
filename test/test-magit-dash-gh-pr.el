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
               (lambda (dir) (string-suffix-p "myrepo-pr-42" dir)))
              ((symbol-function 'magit-status-setup-buffer)
               (lambda (path) (setq setup-path path))))
      (magit-dash-gh-pr-dashboard-open-worktree)
      (should (equal (expand-file-name "/tmp/myrepo-pr-42") setup-path)))))

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
      (should (equal (expand-file-name "/tmp/myrepo-pr-42") setup-path))
      ;; Verify git fetch and git worktree add calls
      (should (seq-some (lambda (call)
                          (and (equal (nth 0 call) "fetch")
                               (equal (nth 1 call) "origin")
                               (equal (nth 2 call) "pull/42/head:pr-42")))
                        git-calls))
      (should (seq-some (lambda (call)
                          (and (equal (nth 0 call) "worktree")
                               (equal (nth 1 call) "add")
                               (equal (nth 2 call) (expand-file-name "/tmp/myrepo-pr-42"))
                               (equal (nth 3 call) "pr-42")))
                        git-calls)))))
;;; magit-dash-gh-pr-dashboard--build-format

(ert-deftest magit-dash-gh-pr/build-format-caps-repo-width-with-entries ()
  "Repo column width is capped at the longest repo name + 3."
  (let* ((entries '(((:repo "owner/repo-short") ["repo-short" "·" "1" "Title" "pass" "1d" "0" "approved"])
                    ((:repo "owner/a-much-longer-repository-name") ["a-much-longer-repository-name" "·" "2" "Title" "pass" "1d" "0" "approved"])))
         (fmt (magit-dash-gh-pr-dashboard--build-format 150 entries)))
    (should (equal "Repo" (car (aref fmt 0))))
    (should (= (+ (length "a-much-longer-repository-name") 3)
               (cadr (aref fmt 0))))))

(ert-deftest magit-dash-gh-pr/build-format-show-org ()
  "When show-org is non-nil, build-format includes Org and Repo columns."
  (let* ((magit-dash-gh-pr-show-org t)
         (entries '(((:repo "casaphq/my-repo") ["casaphq" "my-repo" "·" "1" "Title" "pass" "1d" "0" "approved"])))
         (fmt (magit-dash-gh-pr-dashboard--build-format 150 entries)))
    (should (equal "Org" (car (aref fmt 0))))
    (should (equal "Repo" (car (aref fmt 1))))
    (should (equal "WT" (car (aref fmt 2))))))

(ert-deftest magit-dash-gh-pr/build-format-fallback-no-entries ()
  "When no entries exist, Repo column falls back to available window space (min 12)."
  (let ((fmt (magit-dash-gh-pr-dashboard--build-format 100 nil)))
    (should (= (- 100 80) (cadr (aref fmt 0))))))

(ert-deftest magit-dash-gh-pr/split-slug ()
  "Split slug correctly extracts owner and repo."
  (should (equal '("owner" . "repo") (magit-dash-gh-pr--split-slug "owner/repo")))
  (should (equal '("" . "standalone") (magit-dash-gh-pr--split-slug "standalone"))))

(ert-deftest magit-dash-gh-pr/worktree-detection ()
  "Worktree detection checks local checkout and worktree directory."
  (let ((magit-dash-repo-list (magit-dash-gh-pr-test/make-repos "test-repo" "/tmp/test-repo")))
    (should (equal (expand-file-name "/tmp/test-repo-pr-10")
                   (magit-dash-gh-pr-dashboard--worktree-path '(:number 10 :repo "owner/test-repo"))))
    (cl-letf (((symbol-function 'file-directory-p)
               (lambda (path) (string-suffix-p "test-repo-pr-10" path))))
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
    (setq tabulated-list-entries '(((:repo "o/r" :number 1) ["o/r" "1" "T" "pass" "1d" "0" "approved"])))
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
  "PR dashboard has expected default columns enabled and disabled."
  (should (eq nil (alist-get 'org magit-dash-gh-pr-columns)))
  (should (eq t (alist-get 'repo magit-dash-gh-pr-columns)))
  (should (eq t (alist-get 'worktree magit-dash-gh-pr-columns)))
  (should (eq t (alist-get 'title magit-dash-gh-pr-columns)))
  (should (eq t (alist-get 'ci magit-dash-gh-pr-columns)))
  (should (memq 'magit-dash-gh-pr-columns savehist-additional-variables)))

(ert-deftest magit-dash-gh-pr/toggle-column ()
  "Toggling a column flips its enabled state and syncs show-org."
  (let ((magit-dash-gh-pr-columns
         '((org . nil) (repo . t) (worktree . t) (pr . t) (title . t) (ci . t) (age . t) (comments . t) (review . t)))
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
         '((org . nil) (repo . t) (worktree . nil) (pr . t) (title . nil) (ci . nil) (age . nil) (comments . nil) (review . nil)))
        (pr '((number . 10) (title . "T") (updatedAt . "") (comments . 0))))
    (let* ((entry (magit-dash-gh-pr-dashboard--build-entry pr "owner/repo"))
           (vec (cadr entry)))
      (should (= 2 (length vec)))
      (should (equal "repo" (aref vec 0)))
      (should (equal "10" (aref vec 1))))))

(provide 'test-magit-dash-gh-pr)
;;; test-magit-dash-gh-pr.el ends here
