;;; test-magit-dash-review.el --- ERT tests for magit-dash-review -*- lexical-binding: t; no-byte-compile: t; -*-

;; Run inside a live Emacs session:
;;   (ert "^magit-dash-review/")

(require 'ert)
(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'json)

(require 'test-helper)
(require 'magit-dash-review)

;;; Test Helpers

(defmacro magit-dash-review-test/with-temp-dir (&rest body)
  "Execute BODY with `default-directory' set to a fresh temp directory."
  (declare (indent 0))
  `(let ((dir (make-temp-file "magit-dash-review-test-" t)))
     (unwind-protect
         (let ((default-directory dir))
           ,@body)
       (delete-directory dir t))))

(defun magit-dash-review-test/make-thread-node (id path line &optional start-line resolved outdated author body replies commit-oid)
  "Create a fixture GraphQL reviewThread alist node."
  `((id . ,id)
    (path . ,path)
    (line . ,line)
    (startLine . ,start-line)
    (diffSide . "RIGHT")
    (isResolved . ,resolved)
    (isOutdated . ,outdated)
    (comments . ((nodes . ,(cons
                            `((id . ,(format "%s-c0" id))
                              (author . ((login . ,(or author "reviewer"))))
                              (body . ,(or body "Test comment body"))
                              (createdAt . "2026-11-15T19:00:00Z")
                              (commit . ((oid . ,(or commit-oid "deadbeef")))))
                            (mapcar
                             (lambda (r)
                               `((id . ,(map-elt r :id))
                                 (author . ((login . ,(map-elt r :author))))
                                 (body . ,(map-elt r :body))
                                 (createdAt . ,(or (map-elt r :at) "2026-11-15T20:00:00Z"))))
                             replies)))))))

;;; Tests

(ert-deftest magit-dash-review/review-file-location ()
  "magit-dash-review-file returns <git-common-dir>/review/pr-<num>.json."
  (magit-dash-review-test/with-temp-dir
    (call-process "git" nil nil nil "init" ".")
    (let* ((root default-directory)
           (review-path (magit-dash-review-file root 42)))
      (should (string-suffix-p "review/pr-42.json" review-path))
      (should (file-name-absolute-p review-path)))))

(ert-deftest magit-dash-review/transcode-single-line ()
  "Transcoding a single-line reviewThread produces lines [L, L] and origin github."
  (let* ((node (magit-dash-review-test/make-thread-node "t1" "lisp/foo.el" 42 nil nil nil "alice" "Check this line"))
         (ht (magit-dash-review--transcode-thread node "/nonexistent" "head123" "base123")))
    (should (equal (map-elt ht "gh_thread_id") "t1"))
    (should (equal (map-elt ht "gh_comment_id") "t1-c0"))
    (should (equal (map-elt ht "path") "lisp/foo.el"))
    (should (equal (map-elt ht "lines") [42 42]))
    (should (equal (map-elt ht "author") "alice"))
    (should (equal (map-elt ht "body") "Check this line"))
    (should (equal (map-elt ht "origin") "github"))
    (should (eq (map-elt ht "resolved") :false))
    (should (equal (map-elt ht "replies") []))))

(ert-deftest magit-dash-review/transcode-multi-line ()
  "Transcoding a multi-line reviewThread computes sorted [min max] line vector."
  (let* ((node (magit-dash-review-test/make-thread-node "t2" "lisp/bar.el" 50 45 nil nil "bob" "Multi line range"))
         (ht (magit-dash-review--transcode-thread node "/nonexistent" "head123" "base123")))
    (should (equal (map-elt ht "lines") [45 50]))
    (should (equal (map-elt ht "author") "bob"))))

(ert-deftest magit-dash-review/transcode-resolved-and-outdated ()
  "Transcoding preserves resolved and outdated flags."
  (let* ((node (magit-dash-review-test/make-thread-node "t3" "lisp/baz.el" 10 nil t t "carol" "Already fixed"))
         (ht (magit-dash-review--transcode-thread node "/nonexistent" "head123" "base123")))
    (should (eq (map-elt ht "resolved") t))
    (should (eq (map-elt ht "gh_resolved") t))
    (should (eq (map-elt ht "outdated") t))))

(ert-deftest magit-dash-review/transcode-with-replies ()
  "Transcoding converts subsequent comments into reply hash tables."
  (let* ((replies (list '(:id "r1" :author "tychoish" :body "Fixed in commit 123" :at "2026-11-15T20:10:00Z")))
         (node (magit-dash-review-test/make-thread-node "t4" "lisp/qux.el" 20 nil nil nil "dan" "Question" replies))
         (ht (magit-dash-review--transcode-thread node "/nonexistent" "head123" "base123"))
         (rep-vec (map-elt ht "replies")))
    (should (= (length rep-vec) 1))
    (let ((rep0 (aref rep-vec 0)))
      (should (equal (map-elt rep0 "gh_comment_id") "r1"))
      (should (equal (map-elt rep0 "author") "tychoish"))
      (should (equal (map-elt rep0 "body") "Fixed in commit 123")))))

(ert-deftest magit-dash-review/three-way-merge-preserves-local-unsynced ()
  "Three-way merge preserves local unsynced comments and unsynced replies."
  (let* ((existing-ht (make-hash-table :test #'equal))
         (local-c1 (make-hash-table :test #'equal))
         (local-c2 (make-hash-table :test #'equal))
         (local-reply (make-hash-table :test #'equal))
         (viewed-table (make-hash-table :test #'equal))
         (verdict-table (make-hash-table :test #'equal)))
    ;; Local viewed mark
    (setf (map-elt viewed-table "lisp/foo.el") "hash123")
    ;; Local verdict
    (setf (map-elt verdict-table "state") "approved")
    (setf (map-elt verdict-table "by") "tychoish")

    ;; Local comment 1 corresponds to remote thread "t1", with a local unpublished reply
    (setf (map-elt local-c1 "id") "loc001")
    (setf (map-elt local-c1 "gh_thread_id") "t1")
    (setf (map-elt local-c1 "author") "alice")
    (setf (map-elt local-c1 "body") "Old body")
    (setf (map-elt local-c1 "resolved") :false)
    (setf (map-elt local-reply "id") "rep001")
    (setf (map-elt local-reply "author") "me")
    (setf (map-elt local-reply "body") "Local draft reply not on GH")
    (setf (map-elt local-c1 "replies") (vector local-reply))

    ;; Local comment 2 is completely local (no gh_thread_id)
    (setf (map-elt local-c2 "id") "loc002")
    (setf (map-elt local-c2 "origin") "local")
    (setf (map-elt local-c2 "author") "tychoish")
    (setf (map-elt local-c2 "body") "Draft note locally")
    (setf (map-elt local-c2 "resolved") :false)
    (setf (map-elt local-c2 "replies") [])

    (setf (map-elt existing-ht "comments") (vector local-c1 local-c2))
    (setf (map-elt existing-ht "viewed") viewed-table)
    (setf (map-elt existing-ht "verdict") verdict-table)

    ;; Remote has updated thread "t1" with a published remote reply "r-gh"
    (let* ((remote-reply (make-hash-table :test #'equal))
           (remote-c1 (make-hash-table :test #'equal))
           (remote-meta '(:repo "owner/repo" :number 42 :worktree "/tmp/wt"
                          :head-ref "feature" :base-ref "main"
                          :head_oid "head123" :base_oid "base123"
                          :url "https://github.com/owner/repo/pull/42")))
      (setf (map-elt remote-reply "id") "r001")
      (setf (map-elt remote-reply "gh_comment_id") "r-gh")
      (setf (map-elt remote-reply "author") "alice")
      (setf (map-elt remote-reply "body") "Remote reply")

      (setf (map-elt remote-c1 "id") "rem001")
      (setf (map-elt remote-c1 "gh_thread_id") "t1")
      (setf (map-elt remote-c1 "author") "alice")
      (setf (map-elt remote-c1 "body") "Updated body from remote")
      (setf (map-elt remote-c1 "resolved") :false)
      (setf (map-elt remote-c1 "gh_resolved") :false)
      (setf (map-elt remote-c1 "replies") (vector remote-reply))

      (let* ((merged (magit-dash-review--merge-json existing-ht remote-meta (list remote-c1)))
             (comments (append (map-elt merged "comments") nil)))
        ;; Should have 2 comments: merged t1 and local-c2
        (should (= (length comments) 2))

        (let ((merged-t1 (seq-find (lambda (c) (equal (map-elt c "gh_thread_id") "t1")) comments))
              (merged-local (seq-find (lambda (c) (equal (map-elt c "id") "loc002")) comments)))
          (should merged-t1)
          (should merged-local)
          ;; Preserves local id "loc001" for t1
          (should (equal (map-elt merged-t1 "id") "loc001"))
          ;; Merges replies: has remote reply AND local draft reply!
          (let ((reps (append (map-elt merged-t1 "replies") nil)))
            (should (= (length reps) 2))
            (should (seq-find (lambda (r) (equal (map-elt r "gh_comment_id") "r-gh")) reps))
            (should (seq-find (lambda (r) (equal (map-elt r "body") "Local draft reply not on GH")) reps)))
          ;; Preserves viewed marks and verdict
          (should (equal (map-elt (map-elt merged "viewed") "lisp/foo.el") "hash123"))
          (should (equal (map-elt (map-elt merged "verdict") "state") "approved"))
          ;; Metadata key magit_dash populated
          (should (equal (map-elt (map-elt merged "magit_dash") "repo") "owner/repo"))
          (should (= (map-elt (map-elt merged "magit_dash") "number") 42)))))))

(ert-deftest magit-dash-review/extract-findings-and-hunks ()
  "magit-dash-review-extract-findings extracts unresolved threads with git diff hunks."
  (magit-dash-review-test/with-temp-dir
    (call-process "git" nil nil nil "init" ".")
    (call-process "git" nil nil nil "config" "user.email" "test@example.com")
    (call-process "git" nil nil nil "config" "user.name" "Test User")
    (with-temp-file (expand-file-name "sample.el" default-directory)
      (insert "(defun foo ()\n  1)\n"))
    (call-process "git" nil nil nil "add" "sample.el")
    (call-process "git" nil nil nil "commit" "-m" "Initial")
    (call-process "git" nil nil nil "branch" "-M" "main")
    (call-process "git" nil nil nil "checkout" "-b" "feature")
    (with-temp-file (expand-file-name "sample.el" default-directory)
      (insert "(defun foo ()\n  (+ 1 2))\n"))
    (call-process "git" nil nil nil "commit" "-am" "Update foo")

    (let* ((review-file (magit-dash-review-file default-directory 7))
           (c-open (make-hash-table :test #'equal))
           (c-resolved (make-hash-table :test #'equal))
           (c-draft (make-hash-table :test #'equal))
           (data (make-hash-table :test #'equal)))
      (setf (map-elt c-open "id") "c01")
      (setf (map-elt c-open "path") "sample.el")
      (setf (map-elt c-open "lines") [2 2])
      (setf (map-elt c-open "author") "reviewer")
      (setf (map-elt c-open "body") "Use a constant here")
      (setf (map-elt c-open "resolved") :false)
      (setf (map-elt c-open "replies") [])

      (setf (map-elt c-resolved "id") "c02")
      (setf (map-elt c-resolved "path") "sample.el")
      (setf (map-elt c-resolved "lines") [1 1])
      (setf (map-elt c-resolved "author") "reviewer")
      (setf (map-elt c-resolved "body") "Resolved comment")
      (setf (map-elt c-resolved "resolved") t)
      (setf (map-elt c-resolved "replies") [])

      (setf (map-elt c-draft "id") "c03")
      (setf (map-elt c-draft "path") "sample.el")
      (setf (map-elt c-draft "lines") [2 2])
      (setf (map-elt c-draft "author") "gptel")
      (setf (map-elt c-draft "body") "Unpromoted draft")
      (setf (map-elt c-draft "status") "draft")
      (setf (map-elt c-draft "resolved") :false)
      (setf (map-elt c-draft "replies") [])

      (setf (map-elt data "format") "diff-review/1")
      (setf (map-elt data "root") default-directory)
      (setf (map-elt data "base") "main")
      (setf (map-elt data "head") "feature")
      (setf (map-elt data "viewed") (make-hash-table :test #'equal))
      (setf (map-elt data "comments") (vector c-open c-resolved c-draft))
      (magit-dash-review--write-json-file review-file data)

      ;; Unresolved only excludes resolved and draft comments
      (let ((findings (magit-dash-review-extract-findings review-file t)))
        (should (= (length findings) 1))
        (let ((f (car findings)))
          (should (equal (plist-get f :id) "c01"))
          (should (equal (plist-get f :path) "sample.el"))
          (should (equal (plist-get f :lines) [2 2]))
          (should (= (plist-get f :line) 2))
          (should (equal (plist-get f :body) "Use a constant here"))
          (should (string-match-p "(\\+ 1 2)" (plist-get f :hunk)))))

      ;; All findings includes all 3
      (let ((all-findings (magit-dash-review-extract-findings review-file nil)))
        (should (= (length all-findings) 3))))))

(ert-deftest magit-dash-review/append-reply-and-resolve-policy ()
  "magit-dash-review-append-reply appends reply and respects auto-resolve policy."
  (magit-dash-review-test/with-temp-dir
    (let* ((review-file (expand-file-name "review/pr-10.json" default-directory))
           (c1 (make-hash-table :test #'equal))
           (data (make-hash-table :test #'equal)))
      (setf (map-elt c1 "id") "c100")
      (setf (map-elt c1 "path") "foo.el")
      (setf (map-elt c1 "lines") [10 12])
      (setf (map-elt c1 "author") "alice")
      (setf (map-elt c1 "body") "Fix this edge case")
      (setf (map-elt c1 "resolved") :false)
      (setf (map-elt c1 "replies") [])

      (setf (map-elt data "format") "diff-review/1")
      (setf (map-elt data "root") default-directory)
      (setf (map-elt data "base") "main")
      (setf (map-elt data "head") "feat")
      (setf (map-elt data "viewed") (make-hash-table :test #'equal))
      (setf (map-elt data "comments") (vector c1))
      (magit-dash-review--write-json-file review-file data)

      ;; Default: magit-dash-review-agent-auto-resolve is nil -> leaves resolved :false
      (let ((magit-dash-review-agent-auto-resolve nil))
        (magit-dash-review-append-reply review-file "c100" "agent-shell" "Fixed in working tree")
        (let* ((reloaded (magit-dash-review--read-json-file review-file))
               (c (aref (map-elt reloaded "comments") 0))
               (replies (map-elt c "replies")))
          (should (= (length replies) 1))
          (should (equal (map-elt (aref replies 0) "author") "agent-shell"))
          (should (equal (map-elt (aref replies 0) "body") "Fixed in working tree"))
          (should (eq (map-elt c "resolved") :false))))

      ;; With magit-dash-review-agent-auto-resolve = t -> marks resolved t
      (let ((magit-dash-review-agent-auto-resolve t))
        (magit-dash-review-append-reply review-file "c100" "agent-shell" "Auto-resolved fix")
        (let* ((reloaded (magit-dash-review--read-json-file review-file))
               (c (aref (map-elt reloaded "comments") 0))
               (replies (map-elt c "replies")))
          (should (= (length replies) 2))
          (should (eq (map-elt c "resolved") t)))))))

(ert-deftest magit-dash-review/current-hunk-context ()
  "magit-dash-review-current-hunk extracts file path, line span, and local hunk."
  (with-temp-buffer
    (insert "line 1\nline 2\nline 3\nline 4\n")
    (goto-char (point-min))
    (forward-line 1)
    (setq-local diff-review--file-path "lisp/example.el")
    (setq-local diff-review--hunks (list '(:line 2 :count 2 :removed ("old line 2"))))
    (let ((ctx (magit-dash-review-current-hunk)))
      (should (equal (plist-get ctx :path) "lisp/example.el"))
      (should (= (plist-get ctx :line) 2))
      (should (= (plist-get ctx :end-line) 2))
      (should (equal (plist-get ctx :removed) '("old line 2"))))))

(ert-deftest magit-dash-review/worktree-entrypoint-and-sync ()
  "magit-dash-review-pr, magit-dash-review-worktree-diff, and magit-dash-review-sync hydrate JSON."
  (magit-dash-review-test/with-temp-dir
    (call-process "git" nil nil nil "init" ".")
    (call-process "git" nil nil nil "config" "user.email" "test@example.com")
    (call-process "git" nil nil nil "config" "user.name" "Test User")
    (with-temp-file (expand-file-name "core.el" default-directory)
      (insert "(provide 'core)\n"))
    (call-process "git" nil nil nil "add" "core.el")
    (call-process "git" nil nil nil "commit" "-m" "Init")

    (let ((opened-files nil)
          (wt-dir (expand-file-name ".wt/pr-12/" default-directory)))
      (make-directory wt-dir t)
      (cl-letf (((symbol-function 'diff-review-find-file)
                 (lambda (f &rest _) (push f opened-files) (current-buffer)))
                ((symbol-function 'magit-dash-pr--find-existing-worktree)
                 (lambda (_dir _num _branch) wt-dir))
                ((symbol-function 'magit-dash-review--fetch-pr-metadata)
                 (lambda (_slug _num _dir)
                   '((number . 12)
                     (title . "Add review bridge")
                     (url . "https://github.com/owner/repo/pull/12")
                     (headRefName . "feat/bridge")
                     (baseRefName . "main")
                     (headRefOid . "1111111111111111111111111111111111111111")
                     (baseRefOid . "2222222222222222222222222222222222222222"))))
                ((symbol-function 'magit-dash-pr-fetch-threads-graphql)
                 (lambda (_slug _num &rest _)
                   (list (list :id "PRRT_12"
                               :path "core.el"
                               :line 1
                               :resolved :false
                               :outdated :false
                               :author "reviewer"
                               :body "Looks clean"
                               :comments (vector (list :id "PRRC_12"
                                                       :author "reviewer"
                                                       :body "Looks clean"
                                                       :created_at "2026-11-15T19:00:00Z")))))))
        (let* ((rf (magit-dash-review-pr "https://github.com/owner/repo/pull/12" default-directory))
               (data (magit-dash-review--read-json-file rf))
               (md (map-elt data "magit_dash"))
               (comments (map-elt data "comments")))
          (should (member rf opened-files))
          (should (equal (map-elt data "root") (file-name-as-directory wt-dir)))
          (should (equal (map-elt data "head") "feat/bridge"))
          (should (equal (map-elt md "repo") "owner/repo"))
          (should (= (map-elt md "number") 12))
          (should (= (length comments) 1))
          (should (equal (map-elt (aref comments 0) "gh_thread_id") "PRRT_12")))

        ;; Also test magit-dash-review-worktree-diff (omits "head")
        (let* ((wt-rf (magit-dash-review-worktree-diff wt-dir "HEAD"))
               (wt-data (magit-dash-review--read-json-file wt-rf)))
          (should (equal (map-elt wt-data "root") (file-name-as-directory wt-dir)))
          (should (equal (map-elt wt-data "base") "HEAD"))
          (should-not (map-elt wt-data "head")))))))

(ert-deftest magit-dash-review/graphql-delta-submit ()
  "magit-dash-review-submit publishes non-draft threads, replies, resolutions, and stamps IDs."
  (magit-dash-review-test/with-temp-dir
    (let* ((review-file (expand-file-name "review/pr-99.json" default-directory))
           (c-remote (make-hash-table :test #'equal))
           (c-local (make-hash-table :test #'equal))
           (c-draft (make-hash-table :test #'equal))
           (local-reply (make-hash-table :test #'equal))
           (verdict (make-hash-table :test #'equal))
           (md (make-hash-table :test #'equal))
           (data (make-hash-table :test #'equal))
           (gh-calls nil))
      ;; Existing GitHub thread: resolved locally, has 1 pending local reply
      (setf (map-elt local-reply "id") "rep1")
      (setf (map-elt local-reply "author") "tychoish")
      (setf (map-elt local-reply "body") "Addressed in latest commit")
      (setf (map-elt c-remote "id") "c-rem")
      (setf (map-elt c-remote "gh_thread_id") "PRRT_EXISTING")
      (setf (map-elt c-remote "gh_comment_id") "PRRC_EXISTING")
      (setf (map-elt c-remote "gh_resolved") :false)
      (setf (map-elt c-remote "resolved") t)
      (setf (map-elt c-remote "path") "foo.el")
      (setf (map-elt c-remote "lines") [10 10])
      (setf (map-elt c-remote "author") "alice")
      (setf (map-elt c-remote "body") "Please fix")
      (setf (map-elt c-remote "replies") (vector local-reply))

      ;; Unsynced non-draft local thread -> should be submitted
      (setf (map-elt c-local "id") "c-loc")
      (setf (map-elt c-local "origin") "local")
      (setf (map-elt c-local "path") "bar.el")
      (setf (map-elt c-local "lines") [20 25])
      (setf (map-elt c-local "author") "tychoish")
      (setf (map-elt c-local "body") "New inline comment")
      (setf (map-elt c-local "resolved") :false)
      (setf (map-elt c-local "replies") [])

      ;; Unpromoted draft thread -> MUST be excluded from GraphQL submission
      (setf (map-elt c-draft "id") "c-drf")
      (setf (map-elt c-draft "status") "draft")
      (setf (map-elt c-draft "path") "bar.el")
      (setf (map-elt c-draft "lines") [30 30])
      (setf (map-elt c-draft "author") "gptel")
      (setf (map-elt c-draft "body") "Unpromoted AI draft")
      (setf (map-elt c-draft "resolved") :false)
      (setf (map-elt c-draft "replies") [])

      (setf (map-elt verdict "state") "approved")
      (setf (map-elt md "repo") "owner/repo")
      (setf (map-elt md "number") 99)
      (setf (map-elt md "pr_node_id") "PR_kwDO99")
      (setf (map-elt data "format") "diff-review/1")
      (setf (map-elt data "root") default-directory)
      (setf (map-elt data "base") "main")
      (setf (map-elt data "head") "feat")
      (setf (map-elt data "magit_dash") md)
      (setf (map-elt data "verdict") verdict)
      (setf (map-elt data "viewed") (make-hash-table :test #'equal))
      (setf (map-elt data "comments") (vector c-remote c-local c-draft))
      (magit-dash-review--write-json-file review-file data)

      (cl-letf (((symbol-function 'magit-dash-gh--call-process)
                 (lambda (args _dir)
                   (push args gh-calls)
                   (let ((joined (string-join args " ")))
                     (cond
                      ((string-match-p "addPullRequestReview(" joined)
                       ;; Ensure draft thread was excluded from payload
                       (should-not (string-match-p "Unpromoted AI draft" joined))
                       (should (string-match-p "New inline comment" joined))
                       (should (string-match-p "APPROVE" joined))
                       (cons 0 (json-serialize
                                '((data . ((addPullRequestReview .
                                            ((pullRequestReview .
                                              ((id . "PRR_1")
                                               (state . "APPROVED")
                                               (comments .
                                                ((nodes . [((id . "PRRC_NEW_1")
                                                            (path . "bar.el")
                                                            (body . "New inline comment")
                                                            (pullRequestReviewThread . ((id . "PRRT_NEW_1"))))])))))))))))))
                      ((string-match-p "addPullRequestReviewThreadReply" joined)
                       (cons 0 (json-serialize
                                '((data . ((addPullRequestReviewThreadReply .
                                            ((comment . ((id . "PRRC_REPLY_1")))))))))))
                      ((string-match-p "resolveReviewThread" joined)
                       (cons 0 (json-serialize
                                '((data . ((resolveReviewThread .
                                            ((thread . ((id . "PRRT_EXISTING")
                                                        (isResolved . t)))))))))))
                      (t (error "Unexpected gh call: %S" args)))))))
        (magit-dash-review-submit review-file "LGTM overall")
        (should (= (length gh-calls) 3))
        (let* ((reloaded (magit-dash-review--read-json-file review-file))
               (comments (append (map-elt reloaded "comments") nil))
               (rem (seq-find (lambda (c) (equal (map-elt c "id") "c-rem")) comments))
               (loc (seq-find (lambda (c) (equal (map-elt c "id") "c-loc")) comments))
               (drf (seq-find (lambda (c) (equal (map-elt c "id") "c-drf")) comments)))
          ;; Post-submit IDs stamped onto c-local
          (should (equal (map-elt loc "gh_thread_id") "PRRT_NEW_1"))
          (should (equal (map-elt loc "gh_comment_id") "PRRC_NEW_1"))
          (should (equal (map-elt loc "origin") "github"))
          ;; Draft thread remains untouched with no gh_thread_id
          (should-not (map-elt drf "gh_thread_id"))
          ;; Reply ID and resolution stamped onto c-remote
          (should (eq (map-elt rem "gh_resolved") t))
          (should (equal (map-elt (aref (map-elt rem "replies") 0) "gh_comment_id")
                         "PRRC_REPLY_1")))))))

(provide 'test-magit-dash-review)
;;; test-magit-dash-review.el ends here
