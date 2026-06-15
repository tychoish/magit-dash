;;; test-magit-dash-gh-pr.el --- ERT tests for magit-dash-gh-pr -*- lexical-binding: t -*-

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

(provide 'test-magit-dash-gh-pr)
;;; test-magit-dash-gh-pr.el ends here
