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

(defun magit-dash-review-test/sync-fixture (file)
  "Write a minimal synced review FILE for owner/repo#12."
  (let ((data (make-hash-table :test #'equal))
        (md (make-hash-table :test #'equal)))
    (setf (map-elt md "repo") "owner/repo")
    (setf (map-elt md "number") 12)
    (setf (map-elt md "worktree") (file-name-directory file))
    (setf (map-elt data "magit_dash") md)
    (setf (map-elt data "comments") [])
    (magit-dash-review--write-json-file file data)))

(defmacro magit-dash-review-test/with-sync-stubs (on-threads &rest body)
  "Run BODY with network stubbed; ON-THREADS is called during the threads fetch."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'magit-dash-review--fetch-pr-metadata)
              (lambda (&rest _) '((headRefName . "h") (baseRefName . "main"))))
             ((symbol-function 'magit-dash-pr-fetch-threads-graphql)
              (lambda (&rest _) ,on-threads nil)))
     ,@body))

(ert-deftest magit-dash-review/sync-merges-against-fresh-read ()
  "Edits made to the file during the network calls survive the sync."
  (magit-dash-review-test/with-temp-dir
    (let ((file (expand-file-name "review/pr-12.json" default-directory)))
      (magit-dash-review-test/sync-fixture file)
      (magit-dash-review-test/with-sync-stubs
          (let ((data (magit-dash-review--read-json-file file))
                (c (make-hash-table :test #'equal)))
            (setf (map-elt c "id") "late")
            (setf (map-elt c "body") "added meanwhile")
            (setf (map-elt data "comments") (vector c))
            (magit-dash-review--write-json-file file data))
        (magit-dash-review-sync file))
      (should (equal '("added meanwhile")
                     (seq-map (lambda (c) (map-elt c "body"))
                              (map-elt (magit-dash-review--read-json-file file) "comments")))))))

(ert-deftest magit-dash-review/sync-skips-when-buffer-has-unsaved-edits ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (expand-file-name "review/pr-12.json" default-directory)))
      (magit-dash-review-test/sync-fixture file)
      (let ((buf (find-file-noselect file))
            (before (with-temp-buffer (insert-file-contents file) (buffer-string))))
        (unwind-protect
            (progn
              (with-current-buffer buf (goto-char (point-max)) (insert " "))
              (magit-dash-review-test/with-sync-stubs nil
                (should-not (magit-dash-review-sync file)))
              (should (buffer-modified-p buf))
              (should (equal before (with-temp-buffer (insert-file-contents file) (buffer-string)))))
          (with-current-buffer buf (set-buffer-modified-p nil))
          (kill-buffer buf))))))

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
                   (let* ((req (magit-dash-review-test/gh-request args))
                          (joined (format "%S" req)))
                     (cond
                      ((member "view" args)
                       (cons 0 "{\"headRefOid\":\"H1\"}"))
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
        (should (= (length gh-calls) 4))
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

;;; Submit test helpers (all gh traffic is stubbed)

(defun magit-dash-review-test/gh-request (args)
  "Return the JSON request body gh ARGS would send via --input, or nil."
  (when-let* ((pos (member "--input" args)))
    (with-temp-buffer
      (insert-file-contents (cadr pos))
      (json-parse-string (buffer-string) :object-type 'alist :array-type 'list))))

(defun magit-dash-review-test/c (&rest kv)
  "Return a comment table from the key/value list KV."
  (let ((h (make-hash-table :test #'equal)))
    (seq-do (lambda (pair) (setf (map-elt h (car pair)) (cadr pair)))
            (seq-partition kv 2))
    h))

(defvar magit-dash-review-test/gh-log nil "Requests seen by the fake gh, newest first.")

(defmacro magit-dash-review-test/with-gh (handler &rest body)
  "Run BODY with gh replaced by HANDLER called as (KIND ARGS REQUEST).
KIND is one of view, review, reply, resolve, unresolve, node-id.  HANDLER
returns an alist response, or (CODE . OUTPUT) for a failure."
  (declare (indent 1))
  `(let ((magit-dash-review-test/gh-log nil))
     (cl-letf (((symbol-function 'magit-dash-gh--call-process)
                (lambda (args _dir)
                  (let* ((req (magit-dash-review-test/gh-request args))
                         (q (or (map-elt req 'query) ""))
                         (kind (cond
                                ((member "view" args) 'view)
                                ((string-match-p "addPullRequestReview(" q) 'review)
                                ((string-match-p "addPullRequestReviewThreadReply" q) 'reply)
                                ((string-match-p "unresolveReviewThread" q) 'unresolve)
                                ((string-match-p "resolveReviewThread" q) 'resolve)
                                (t 'node-id)))
                         (res (funcall ,handler kind args req)))
                    (push (list kind args req) magit-dash-review-test/gh-log)
                    (if (and (consp res) (integerp (car res)))
                        res
                      (cons 0 (json-serialize res)))))))
       ,@body)))

(defun magit-dash-review-test/review-response (threads)
  "Return the addPullRequestReview response echoing request THREADS."
  (let ((nodes (vconcat
                (seq-map (lambda (th)
                           `((id . ,(concat "C-" (map-elt th 'body)))
                             (path . ,(map-elt th 'path))
                             (body . ,(map-elt th 'body))
                             (line . ,(map-elt th 'line))
                             (pullRequestReviewThread . ((id . ,(concat "T-" (map-elt th 'body)))))))
                         threads))))
    `((data . ((addPullRequestReview
                . ((pullRequestReview
                    . ((id . "PRR")
                       (state . "COMMENTED")
                       (comments . ((nodes . ,nodes))))))))))))

(defun magit-dash-review-test/gh-default (head)
  "Return a handler that succeeds, echoing posted threads; PR head is HEAD."
  (lambda (kind _args req)
    (pcase kind
      ('view `((headRefOid . ,head)))
      ('review (magit-dash-review-test/review-response
                (map-nested-elt req '(variables input threads))))
      ('reply `((data . ((addPullRequestReviewThreadReply
                          . ((comment . ((id . ,(concat "R-" (map-nested-elt req '(variables body)))))))))))) 
      (_ '((data . ((resolveReviewThread . ((thread . ((id . "x"))))))))))))

(defun magit-dash-review-test/submit-file (comments &optional verdict head)
  "Write a review file holding COMMENTS (and VERDICT) for owner/repo#99; return it."
  (let ((file (expand-file-name "review/pr-99.json" default-directory))
        (data (make-hash-table :test #'equal)))
    (setf (map-elt data "format") "diff-review/1")
    (setf (map-elt data "root") default-directory)
    (setf (map-elt data "magit_dash")
          (magit-dash-review-test/c "repo" "owner/repo" "number" 99 "pr_node_id" "PR_1"
                                    "head_oid" (or head "H1")))
    (when verdict (setf (map-elt data "verdict") verdict))
    (setf (map-elt data "viewed") (make-hash-table :test #'equal))
    (setf (map-elt data "comments") (vconcat comments))
    (magit-dash-review--write-json-file file data)
    file))

(defun magit-dash-review-test/gh-thread (id &rest replies)
  "An existing GitHub thread ID with unposted REPLIES (body strings)."
  (magit-dash-review-test/c
   "id" id "gh_thread_id" (concat "T-" id) "gh_comment_id" (concat "C-" id)
   "gh_resolved" :false "resolved" :false "path" "f.el" "lines" [1 1] "body" id
   "replies" (vconcat (seq-map (lambda (b) (magit-dash-review-test/c "id" (concat "r-" b) "body" b))
                               replies))))

(defun magit-dash-review-test/kinds ()
  "Kinds of the gh calls made so far, oldest first."
  (nreverse (seq-map #'car magit-dash-review-test/gh-log)))

(defun magit-dash-review-test/read-comments (file)
  "Comments of FILE as a list."
  (append (map-elt (magit-dash-review--read-json-file file) "comments") nil))

;;; Submit tests

(ert-deftest magit-dash-review/submit-never-sends-text-through-F ()
  "HIGH-3: bodies like @file, 123 and null reach gh as literal strings, never via -F."
  (magit-dash-review-test/with-temp-dir
    (let* ((file (magit-dash-review-test/submit-file
                  (list (magit-dash-review-test/gh-thread "a" "@/etc/hostname" "123" "null")
                        (magit-dash-review-test/c "id" "n" "path" "n.el" "lines" [2 2]
                                                  "body" "@/etc/hostname"))))
           (magit-dash-review--submit-chunk-size 40))
      (magit-dash-review-test/with-gh (magit-dash-review-test/gh-default "H1")
        (magit-dash-review-submit file "true")
        (should magit-dash-review-test/gh-log)
        (seq-do (lambda (call)
                  (should-not (member "-F" (nth 1 call)))
                  (should-not (seq-some (lambda (a) (string-match-p "@/etc/hostname" a)) (nth 1 call))))
                magit-dash-review-test/gh-log)
        (let ((bodies (seq-map (lambda (call) (map-nested-elt (nth 2 call) '(variables body)))
                               (seq-filter (lambda (call) (eq (car call) 'reply))
                                           magit-dash-review-test/gh-log))))
          (should (equal (sort (copy-sequence bodies) #'string<) '("123" "@/etc/hostname" "null"))))
        (let ((review (seq-find (lambda (call) (eq (car call) 'review)) magit-dash-review-test/gh-log)))
          (should (equal "@/etc/hostname"
                         (map-elt (car (map-nested-elt (nth 2 review) '(variables input threads))) 'body)))
          (should (equal "true" (map-nested-elt (nth 2 review) '(variables input body)))))))))

(ert-deftest magit-dash-review/submit-partial-failure-persists-and-resubmit-posts-remainder ()
  "HIGH-1: phase 1 and reply 1 are stamped when reply 2 fails; resubmit posts only reply 2."
  (magit-dash-review-test/with-temp-dir
    (let* ((file (magit-dash-review-test/submit-file
                  (list (magit-dash-review-test/gh-thread "a" "one" "two")
                        (magit-dash-review-test/c "id" "n" "path" "n.el" "lines" [2 2] "body" "new"))))
           (ok (magit-dash-review-test/gh-default "H1")))
      (magit-dash-review-test/with-gh
          (lambda (kind args req)
            (if (and (eq kind 'reply) (equal "two" (map-nested-elt req '(variables body))))
                (cons 1 "boom")
              (funcall ok kind args req)))
        (should-error (magit-dash-review-submit file) :type 'error))
      (let* ((comments (magit-dash-review-test/read-comments file))
             (new (seq-find (lambda (c) (equal (map-elt c "id") "n")) comments))
             (replies (map-elt (seq-find (lambda (c) (equal (map-elt c "id") "a")) comments) "replies")))
        (should (equal "T-new" (map-elt new "gh_thread_id")))
        (should (equal "R-one" (map-elt (aref replies 0) "gh_comment_id")))
        (should-not (map-elt (aref replies 1) "gh_comment_id")))
      (magit-dash-review-test/with-gh ok
        (magit-dash-review-submit file)
        (should (equal '(reply) (magit-dash-review-test/kinds)))
        (should (equal "two" (map-nested-elt (nth 2 (car magit-dash-review-test/gh-log))
                                             '(variables body)))))
      (magit-dash-review-test/with-gh ok
        (magit-dash-review-submit file)
        (should-not magit-dash-review-test/gh-log)))))

(ert-deftest magit-dash-review/submit-matches-returned-nodes-by-content-not-index ()
  (magit-dash-review-test/with-temp-dir
    (let* ((file (magit-dash-review-test/submit-file
                  (list (magit-dash-review-test/c "id" "A" "path" "a.el" "lines" [5 5] "body" "first")
                        (magit-dash-review-test/c "id" "B" "path" "b.el" "lines" [7 7] "body" "second"))))
           (ok (magit-dash-review-test/gh-default "H1")))
      (magit-dash-review-test/with-gh
          (lambda (kind args req)
            (let ((res (funcall ok kind args req)))
              (if (eq kind 'review)
                  ;; Return the nodes in reverse order.
                  (let ((nodes (map-nested-elt res '(data addPullRequestReview pullRequestReview comments nodes))))
                    `((data . ((addPullRequestReview
                                . ((pullRequestReview
                                    . ((comments . ((nodes . ,(vconcat (reverse (append nodes nil))))))))))))))
                  res)))
        (magit-dash-review-submit file))
      (let ((comments (magit-dash-review-test/read-comments file)))
        (should (equal "T-first" (map-elt (car comments) "gh_thread_id")))
        (should (equal "T-second" (map-elt (cadr comments) "gh_thread_id")))))))

(ert-deftest magit-dash-review/submit-chunks-many-threads-and-resumes ()
  "HIGH-1: more than 50 threads go in chunks, stamped after each; resubmit sends the rest."
  (magit-dash-review-test/with-temp-dir
    (let* ((threads (seq-map (lambda (i)
                               (magit-dash-review-test/c "id" (format "n%d" i) "path" "p.el"
                                                         "lines" (vector i i) "body" (format "b%d" i)))
                             (number-sequence 1 95)))
           (file (magit-dash-review-test/submit-file threads))
           (ok (magit-dash-review-test/gh-default "H1"))
           (reviews 0))
      (magit-dash-review-test/with-gh
          (lambda (kind args req)
            (when (eq kind 'review)
              (setq reviews (1+ reviews))
              (when (= reviews 3)
                (error "third chunk fails")))
            (funcall ok kind args req))
        (should-error (magit-dash-review-submit file)))
      (should (= 80 (seq-count (lambda (c) (map-elt c "gh_thread_id"))
                               (magit-dash-review-test/read-comments file))))
      (magit-dash-review-test/with-gh ok
        (magit-dash-review-submit file)
        (should (equal '(view review) (magit-dash-review-test/kinds)))
        (should (= 15 (length (map-nested-elt (nth 2 (car magit-dash-review-test/gh-log))
                                              '(variables input threads))))))
      (should (= 95 (seq-count (lambda (c) (map-elt c "gh_thread_id"))
                               (magit-dash-review-test/read-comments file)))))))

(ert-deftest magit-dash-review/submit-error-on-one-thread-does-not-abort-others ()
  (magit-dash-review-test/with-temp-dir
    (let* ((file (magit-dash-review-test/submit-file
                  (list (magit-dash-review-test/gh-thread "a" "bad")
                        (magit-dash-review-test/gh-thread "b" "good"))))
           (ok (magit-dash-review-test/gh-default "H1")))
      (magit-dash-review-test/with-gh
          (lambda (kind args req)
            (if (and (eq kind 'reply) (equal "bad" (map-nested-elt req '(variables body))))
                (cons 1 "boom")
              (funcall ok kind args req)))
        (let ((err (should-error (magit-dash-review-submit file))))
          (should (string-match-p "T-a" (error-message-string err)))))
      (let ((b (cadr (magit-dash-review-test/read-comments file))))
        (should (equal "R-good" (map-elt (aref (map-elt b "replies") 0) "gh_comment_id")))))))

(ert-deftest magit-dash-review/submit-skips-gh-missing-threads ()
  "MED-4: replies and resolution on a thread gone from GitHub are skipped, others go on."
  (magit-dash-review-test/with-temp-dir
    (let* ((gone (magit-dash-review-test/gh-thread "a" "lost"))
           (file (progn
                   (setf (map-elt gone "gh_missing") t)
                   (setf (map-elt gone "resolved") t)
                   (magit-dash-review-test/submit-file
                    (list gone (magit-dash-review-test/gh-thread "b" "kept"))))))
      (magit-dash-review-test/with-gh (magit-dash-review-test/gh-default "H1")
        (magit-dash-review-submit file)
        (should (equal '(reply) (magit-dash-review-test/kinds)))))))

(ert-deftest magit-dash-review/merge-flags-gh-missing-only-for-complete-fetch ()
  (let* ((local (magit-dash-review-test/gh-thread "a"))
         (existing (magit-dash-review-test/c "comments" (vector local)
                                             "magit_dash" (magit-dash-review-test/c "repo" "o/r"))))
    (let ((merged (magit-dash-review--merge-json (magit-dash-review-json-deep-copy existing)
                                                 '(:repo "o/r") nil)))
      (should-not (map-elt (aref (map-elt merged "comments") 0) "gh_missing")))
    (let ((merged (magit-dash-review--merge-json (magit-dash-review-json-deep-copy existing)
                                                 '(:repo "o/r" :threads-complete t) nil)))
      (should (eq t (map-elt (aref (map-elt merged "comments") 0) "gh_missing"))))
    (let* ((node (magit-dash-review-test/make-thread-node "T-a" "f.el" 1))
           (merged (magit-dash-review--merge-json
                    (magit-dash-review--merge-json (magit-dash-review-json-deep-copy existing)
                                                   '(:repo "o/r" :threads-complete t) nil)
                    '(:repo "o/r" :threads-complete t)
                    (list (magit-dash-review--transcode-thread node nil nil nil)))))
      (should-not (map-elt (aref (map-elt merged "comments") 0) "gh_missing")))))

(ert-deftest magit-dash-review/submit-verdict-stamped-and-not-resent ()
  "HIGH-2: verdict is sent once with commitOID, stamped, and re-sent only when changed."
  (magit-dash-review-test/with-temp-dir
    (let* ((verdict (magit-dash-review-test/c "state" "approved" "by" "me"))
           (file (magit-dash-review-test/submit-file nil verdict))
           (ok (magit-dash-review-test/gh-default "H1")))
      (magit-dash-review-test/with-gh ok
        (magit-dash-review-submit file)
        (should (equal '(view review) (magit-dash-review-test/kinds)))
        (let ((input (map-nested-elt (nth 2 (car magit-dash-review-test/gh-log)) '(variables input))))
          (should (equal "APPROVE" (map-elt input 'event)))
          (should (equal "H1" (map-elt input 'commitOID)))))
      (let ((v (map-elt (magit-dash-review--read-json-file file) "verdict")))
        (should (equal "H1" (map-elt v "submitted_head")))
        (should (equal "approved" (map-elt v "submitted_state")))
        (should (map-elt v "submitted_at")))
      (magit-dash-review-test/with-gh ok
        (magit-dash-review-submit file)
        (should-not magit-dash-review-test/gh-log))
      (let ((data (magit-dash-review--read-json-file file)))
        (setf (map-elt (map-elt data "verdict") "state") "changes")
        (magit-dash-review--write-json-file file data))
      (magit-dash-review-test/with-gh ok
        (magit-dash-review-submit file)
        (should (equal "REQUEST_CHANGES"
                       (map-nested-elt (nth 2 (car magit-dash-review-test/gh-log))
                                       '(variables input event))))))))

(ert-deftest magit-dash-review/submit-refuses-approve-when-head-moved ()
  (magit-dash-review-test/with-temp-dir
    (let* ((file (magit-dash-review-test/submit-file
                  (list (magit-dash-review-test/c "id" "n" "path" "n.el" "lines" [2 2] "body" "new"))
                  (magit-dash-review-test/c "state" "approved"))))
      (magit-dash-review-test/with-gh (magit-dash-review-test/gh-default "H2")
        (should-error (magit-dash-review-submit file) :type 'user-error)
        (should-not (memq 'review (magit-dash-review-test/kinds))))
      (should-not (map-elt (car (magit-dash-review-test/read-comments file)) "gh_thread_id")))))

(ert-deftest magit-dash-review/submit-unknown-verdict-state-posts-nothing ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file nil (magit-dash-review-test/c "state" "bogus"))))
      (magit-dash-review-test/with-gh (magit-dash-review-test/gh-default "H1")
        (magit-dash-review-submit file)
        (magit-dash-review-submit file)
        (should-not magit-dash-review-test/gh-log)))))

(ert-deftest magit-dash-review/submit-does-not-re-resolve-thread-reopened-on-github ()
  "MED-1: a thread GitHub reopened after the local resolve is not resolved again."
  (magit-dash-review-test/with-temp-dir
    (let* ((local (magit-dash-review-test/gh-thread "a"))
           (existing (magit-dash-review-test/c "comments" (vector local)
                                               "magit_dash" (magit-dash-review-test/c "repo" "o/r"))))
      (setf (map-elt local "resolved") t)
      (setf (map-elt local "gh_resolved") t)
      (let* ((node (magit-dash-review-test/make-thread-node "T-a" "f.el" 1))
             (merged (magit-dash-review--merge-json
                      existing '(:repo "o/r" :number 99) (list (magit-dash-review--transcode-thread node nil nil nil))))
             (c (aref (map-elt merged "comments") 0)))
        (should (eq t (map-elt c "gh_reopened")))
        (should (eq :false (map-elt c "gh_resolved")))
        (should (eq t (map-elt c "resolved")))
        (setf (map-elt (map-elt merged "magit_dash") "pr_node_id") "PR_1")
        (let ((file (expand-file-name "review/pr-99.json" default-directory)))
          (magit-dash-review--write-json-file file merged)
          (magit-dash-review-test/with-gh (magit-dash-review-test/gh-default "H1")
            (magit-dash-review-submit file)
            (should-not magit-dash-review-test/gh-log)))))))

(ert-deftest magit-dash-review/submit-resolves-only-for-local-intent ()
  (magit-dash-review-test/with-temp-dir
    (let* ((file (magit-dash-review-test/submit-file
                  (list (magit-dash-review-test/gh-thread "a")))))
      (magit-dash-review-append-reply file "T-a" "me" "done" t)
      (let ((c (car (magit-dash-review-test/read-comments file))))
        (should (eq t (map-elt c "local_resolved"))))
      (magit-dash-review-test/with-gh (magit-dash-review-test/gh-default "H1")
        (magit-dash-review-submit file)
        (should (equal '(reply resolve) (magit-dash-review-test/kinds))))
      (let ((c (car (magit-dash-review-test/read-comments file))))
        (should (eq t (map-elt c "gh_resolved")))
        (should-not (map-elt c "local_resolved"))))))

(ert-deftest magit-dash-review/submit-sends-side-and-single-line ()
  (magit-dash-review-test/with-temp-dir
    (let* ((file (magit-dash-review-test/submit-file
                  (list (magit-dash-review-test/c "id" "one" "path" "a.el" "lines" [4] "body" "one"
                                                  "diff_side" "LEFT")
                        (magit-dash-review-test/c "id" "rng" "path" "b.el" "lines" [3 6] "body" "rng"
                                                  "diff_side" "RIGHT")))))
      (magit-dash-review-test/with-gh (magit-dash-review-test/gh-default "H1")
        (magit-dash-review-submit file)
        (let* ((review (seq-find (lambda (call) (eq (car call) 'review)) magit-dash-review-test/gh-log))
               (threads (map-nested-elt (nth 2 review) '(variables input threads)))
               (one (car threads))
               (rng (cadr threads)))
          (should (= 4 (map-elt one 'line)))
          (should (equal "LEFT" (map-elt one 'side)))
          (should-not (map-elt one 'startLine))
          (should (= 6 (map-elt rng 'line)))
          (should (= 3 (map-elt rng 'startLine)))
          (should (equal "RIGHT" (map-elt rng 'startSide))))))))

(ert-deftest magit-dash-review/submit-refuses-corrupt-file-fallback-without-confirmation ()
  "MED-3: a submit built on the .bak fallback needs the user's confirmation."
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/gh-thread "a" "one")))))
      (magit-dash-review--write-json-file file (magit-dash-review--read-json-file file))
      (with-temp-file file (insert "{\"trunc"))
      (magit-dash-review-test/with-gh (magit-dash-review-test/gh-default "H1")
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
          (should-error (magit-dash-review-submit file) :type 'user-error))
        (should-not magit-dash-review-test/gh-log)
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
          (magit-dash-review-submit file)
          (should (equal '(reply) (magit-dash-review-test/kinds))))))))

;;; Shared buffer refresh helper

(ert-deftest magit-dash-review/append-reply-skips-modified-visiting-buffer ()
  "MED-2: a modified buffer visiting the file is neither reverted nor refreshed."
  (magit-dash-review-test/with-temp-dir
    (let* ((file (magit-dash-review-test/submit-file (list (magit-dash-review-test/gh-thread "a"))))
           (buf (find-file-noselect file)))
      (unwind-protect
          (progn
            (with-current-buffer buf (goto-char (point-max)) (insert "unsaved edit"))
            (magit-dash-review-append-reply file "a" "me" "hello")
            (with-current-buffer buf
              (should (buffer-modified-p))
              (should (string-suffix-p "unsaved edit" (buffer-string))))
            (should (= 1 (length (map-elt (car (magit-dash-review-test/read-comments file)) "replies")))))
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf)))))

(ert-deftest magit-dash-review/append-reply-refreshes-clean-visiting-buffer ()
  (magit-dash-review-test/with-temp-dir
    (let* ((file (magit-dash-review-test/submit-file (list (magit-dash-review-test/gh-thread "a"))))
           (buf (find-file-noselect file)))
      (unwind-protect
          (progn
            (magit-dash-review-append-reply file "a" "me" "hello-world")
            (with-current-buffer buf
              (should-not (buffer-modified-p))
              (should (string-match-p "hello-world" (buffer-string)))))
        (kill-buffer buf)))))

(ert-deftest magit-dash-review/promote-skips-modified-visiting-buffer ()
  (magit-dash-review-test/with-temp-dir
    (let* ((file (magit-dash-review-test/submit-file
                  (list (magit-dash-review-test/c "id" "d" "status" "draft" "body" "x"))))
           (buf (find-file-noselect file)))
      (unwind-protect
          (progn
            (with-current-buffer buf (goto-char (point-max)) (insert "unsaved edit"))
            (cl-letf (((symbol-function 'magit-dash-review-current-hunk) (lambda () nil)))
              (magit-dash-review-promote-draft file "d"))
            (with-current-buffer buf (should (buffer-modified-p)))
            (should (equal "accepted" (map-elt (car (magit-dash-review-test/read-comments file)) "status"))))
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf)))))

(ert-deftest magit-dash-review/save-action-saves-modified-buffer-before-writing ()
  (magit-dash-review-test/with-temp-dir
    (let* ((file (magit-dash-review-test/submit-file (list (magit-dash-review-test/gh-thread "a"))))
           (buf (find-file-noselect file))
           (magit-dash-review-json-modified-buffer-action 'save))
      (unwind-protect
          (progn
            (with-current-buffer buf (goto-char (point-max)) (insert "\n"))
            (magit-dash-review-append-reply file "a" "me" "hello")
            (with-current-buffer buf (should-not (buffer-modified-p))))
        (kill-buffer buf)))))

(ert-deftest magit-dash-review/reply-ids-differ-within-one-second ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file (list (magit-dash-review-test/gh-thread "a")))))
      (cl-letf (((symbol-function 'format-time-string) (lambda (&rest _) "2026-01-01T00:00:00+00:00")))
        (magit-dash-review-append-reply file "a" "me" "same")
        (magit-dash-review-append-reply file "a" "me" "same"))
      (let ((replies (map-elt (car (magit-dash-review-test/read-comments file)) "replies")))
        (should-not (equal (map-elt (aref replies 0) "id") (map-elt (aref replies 1) "id")))))))

(ert-deftest magit-dash-review/legacy-file-without-repo-is-not-adopted ()
  (magit-dash-review-test/with-temp-dir
    (call-process "git" nil nil nil "init" "-q" ".")
    (let ((legacy (magit-dash-review-file default-directory 7)))
      (magit-dash-review--write-json-file legacy (magit-dash-review-test/c "comments" []))
      (should-not (equal legacy (magit-dash-review-file default-directory 7 "o/r")))
      (magit-dash-review--write-json-file
       legacy (magit-dash-review-test/c "magit_dash" (magit-dash-review-test/c "repo" "o/r")))
      (should (equal legacy (magit-dash-review-file default-directory 7 "o/r"))))))

(ert-deftest magit-dash-review/transcode-stores-diff-side ()
  (let* ((node (magit-dash-review-test/make-thread-node "T" "f.el" 3))
         (left (cons '(diffSide . "LEFT") (assq-delete-all 'diffSide (copy-sequence node)))))
    (should (equal "RIGHT" (map-elt (magit-dash-review--transcode-thread node nil nil nil) "diff_side")))
    (should (equal "LEFT" (map-elt (magit-dash-review--transcode-thread left nil nil nil) "diff_side")))))

(ert-deftest magit-dash-review/send-to-agent-and-enqueue-asq ()
  "magit-dash-review-send-to-agent and magit-dash-review-enqueue-asq group and dispatch findings."
  (magit-dash-review-test/with-temp-dir
    (let* ((review-file (expand-file-name "review/pr-33.json" default-directory))
           (c1 (make-hash-table :test #'equal))
           (c2 (make-hash-table :test #'equal))
           (c3 (make-hash-table :test #'equal))
           (data (make-hash-table :test #'equal))
           (enqueued nil))
      (setf (map-elt c1 "id") "f1")
      (setf (map-elt c1 "path") "a.el")
      (setf (map-elt c1 "lines") [1 2])
      (setf (map-elt c1 "author") "tychoish")
      (setf (map-elt c1 "body") "Fix a.el part 1")
      (setf (map-elt c1 "resolved") :false)
      (setf (map-elt c1 "replies") [])

      (setf (map-elt c2 "id") "f2")
      (setf (map-elt c2 "path") "a.el")
      (setf (map-elt c2 "lines") [10 12])
      (setf (map-elt c2 "author") "tychoish")
      (setf (map-elt c2 "body") "Fix a.el part 2")
      (setf (map-elt c2 "resolved") :false)
      (setf (map-elt c2 "replies") [])

      (setf (map-elt c3 "id") "f3")
      (setf (map-elt c3 "path") "b.el")
      (setf (map-elt c3 "lines") [5 5])
      (setf (map-elt c3 "author") "tychoish")
      (setf (map-elt c3 "body") "Fix b.el")
      (setf (map-elt c3 "resolved") :false)
      (setf (map-elt c3 "replies") [])

      (setf (map-elt data "format") "diff-review/1")
      (setf (map-elt data "root") default-directory)
      (setf (map-elt data "base") "main")
      (setf (map-elt data "head") "feat")
      (setf (map-elt data "viewed") (make-hash-table :test #'equal))
      (setf (map-elt data "comments") (vector c1 c2 c3))
      (magit-dash-review--write-json-file review-file data)

      (let ((magit-dash-review-asq-enqueue-function
             (lambda (prompt wt)
               (push (cons prompt wt) enqueued))))
        ;; Per-file grouping produces 2 items (a.el and b.el)
        (magit-dash-review-enqueue-asq review-file 'per-file)
        (should (= (length enqueued) 2))
        ;; Per-thread grouping produces 3 items
        (setq enqueued nil)
        (magit-dash-review-enqueue-asq review-file 'per-thread)
        (should (= (length enqueued) 3)))

      ;; When all comments are resolved, send-to-agent signals user-error
      (setf (map-elt c1 "resolved") t)
      (setf (map-elt c2 "resolved") t)
      (setf (map-elt c3 "resolved") t)
      (magit-dash-review--write-json-file review-file data)
      (should-error (magit-dash-review-send-to-agent 'workflow review-file)
                    :type 'user-error))))

(ert-deftest magit-dash-review/hunk-critique-draft-test-and-promote ()
  "magit-dash-review-critique-hunk inserts draft comment; promote-draft promotes it; draft-test queues prompt."
  (magit-dash-review-test/with-temp-dir
    (let* ((review-file (expand-file-name "review/pr-44.json" default-directory))
           (data (make-hash-table :test #'equal))
           (enqueued nil))
      (setf (map-elt data "format") "diff-review/1")
      (setf (map-elt data "root") default-directory)
      (setf (map-elt data "base") "main")
      (setf (map-elt data "head") "feat")
      (setf (map-elt data "viewed") (make-hash-table :test #'equal))
      (setf (map-elt data "comments") [])
      (magit-dash-review--write-json-file review-file data)

      ;; 1. Critique hunk via mocked gptel-request
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (_prompt &rest args)
                   (let ((cb (plist-get args :callback)))
                     (funcall cb "Potential race condition when buffer is killed." nil))))
                ((symbol-function 'magit-dash-review-current-hunk)
                 (lambda ()
                   (list :review-file review-file
                         :root default-directory
                         :path "lisp/worker.el"
                         :line 40
                         :end-line 48
                         :hunk "@@ -40,3 +40,5 @@"))))
        (magit-dash-review-critique-hunk "check race conditions" review-file)
        (let* ((reloaded (magit-dash-review--read-json-file review-file))
               (comments (map-elt reloaded "comments")))
          (should (= (length comments) 1))
          (let* ((c (aref comments 0))
                 (cid (map-elt c "id")))
            (should (equal (map-elt c "status") "draft"))
            (should (equal (map-elt c "path") "lisp/worker.el"))
            (should (equal (map-elt c "lines") [40 48]))
            (should (string-match-p "race condition" (map-elt c "body")))

            ;; 2. Promote draft comment to accepted
            (magit-dash-review-promote-draft review-file cid)
            (let* ((after-promote (magit-dash-review--read-json-file review-file))
                   (cp (aref (map-elt after-promote "comments") 0)))
              (should (equal (map-elt cp "status") "accepted")))

            ;; 3. Draft regression test in worktree
            (let ((magit-dash-review-asq-enqueue-function
                   (lambda (prompt wt)
                     (push (cons prompt wt) enqueued))))
              (magit-dash-review-draft-test cid review-file)
              (should (= (length enqueued) 1))
              (should (string-match-p "lisp/worker.el" (caar enqueued))))))))))

(ert-deftest magit-dash-review/project-ears-criteria-and-local-comments ()
  "magit-dash-review-project-ears-criteria bridges EARS requirements and local code-review comments."
  (magit-dash-review-test/with-temp-dir
    (let* ((review-file (expand-file-name "review/pr-60.json" default-directory))
           (data (make-hash-table :test #'equal)))
      (setf (map-elt data "format") "diff-review/1")
      (setf (map-elt data "root") default-directory)
      (setf (map-elt data "base") "main")
      (setf (map-elt data "head") "feat")
      (setf (map-elt data "viewed") (make-hash-table :test #'equal))
      (setf (map-elt data "comments") [])
      (magit-dash-review--write-json-file review-file data)

      (let ((criteria (list '(:id "req-001"
                              :statement "WHEN a thread is draft THE SYSTEM SHALL exclude it from GitHub submit."
                              :paths ("magit-dash-review.el"))))
            (local-comments (list '(:id "cr-local-1"
                                    :path "magit-dash-review.el"
                                    :line 12
                                    :author "tychoish"
                                    :body "Bridged from code-review"))))
        (magit-dash-review-project-ears-criteria review-file criteria local-comments)
        (let* ((reloaded (magit-dash-review--read-json-file review-file))
               (comments (append (map-elt reloaded "comments") nil))
               (ears-vec (map-elt reloaded "ears_criteria"))
               (ears-c (seq-find (lambda (c) (equal (map-elt c "id") "ears-criteria")) comments))
               (bridged-c (seq-find (lambda (c) (equal (map-elt c "id") "cr-local-1")) comments)))
          (should (= (length ears-vec) 1))
          (should ears-c)
          (should (string-match-p "req-001" (map-elt ears-c "body")))
          (should bridged-c)
          (should (equal (map-elt bridged-c "path") "magit-dash-review.el"))
          (should (equal (map-elt bridged-c "body") "Bridged from code-review")))))))

(ert-deftest magit-dash-review/acr-unified-review-selector ()
  "magit-dash-review-select collects dirty worktree, review JSON files, and dispatches selection."
  (magit-dash-review-test/with-temp-dir
    (call-process "git" nil nil nil "init" ".")
    (call-process "git" nil nil nil "config" "user.email" "test@example.com")
    (call-process "git" nil nil nil "config" "user.name" "Test User")
    (with-temp-file (expand-file-name "main.el" default-directory)
      (insert ";; initial\n"))
    (call-process "git" nil nil nil "add" "main.el")
    (call-process "git" nil nil nil "commit" "-m" "Init")
    ;; Make working tree dirty
    (with-temp-file (expand-file-name "main.el" default-directory)
      (insert ";; modified\n"))
    ;; Create an active review JSON file
    (let* ((review-file (magit-dash-review-file default-directory 77))
           (data (make-hash-table :test #'equal)))
      (setf (map-elt data "format") "diff-review/1")
      (setf (map-elt data "root") default-directory)
      (setf (map-elt data "base") "main")
      (setf (map-elt data "head") "feat-77")
      (setf (map-elt data "viewed") (make-hash-table :test #'equal))
      (setf (map-elt data "comments") [])
      (magit-dash-review--write-json-file review-file data)

      (let ((cands (magit-dash-review--collect-candidates default-directory nil))
            (opened nil))
        (should (seq-find (lambda (c) (equal (plist-get c :key) "worktree (dirty)")) cands))
        (should (seq-find (lambda (c) (equal (plist-get c :key) "review:pr-77")) cands))
        (cl-letf (((symbol-function 'annotated-completing-read)
                   (lambda (_table &rest _) "review:pr-77"))
                  ((symbol-function 'magit-dash-review--open-front-end)
                   (lambda (f) (setq opened f))))
          (magit-dash-review-select nil)
          (should (equal opened review-file)))))))

(ert-deftest magit-dash-review/review-pr-uses-existing-worktree-without-prompts ()
  "An existing worktree is used as is: no creation, rename, or directory prompt."
  (magit-dash-review-test/with-temp-dir
    (call-process "git" nil nil nil "init" ".")
    (let ((wt-dir (expand-file-name ".wt/cr-12/" default-directory)))
      (make-directory wt-dir t)
      (cl-letf (((symbol-function 'diff-review-find-file) (lambda (&rest _) (current-buffer)))
                ((symbol-function 'magit-dash-pr--find-existing-worktree) (lambda (&rest _) wt-dir))
                ((symbol-function 'magit-dash-pr--create-worktree-for-entry)
                 (lambda (&rest _) (ert-fail "create-worktree-for-entry called")))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) (ert-fail "y-or-n-p called")))
                ((symbol-function 'read-directory-name)
                 (lambda (&rest _) (ert-fail "read-directory-name called")))
                ((symbol-function 'magit-dash-review--fetch-pr-metadata)
                 (lambda (&rest _) '((number . 12) (headRefName . "feat"))))
                ((symbol-function 'magit-dash-pr-fetch-threads-graphql) (lambda (&rest _) nil)))
        (let* ((rf (magit-dash-review-pr "https://github.com/owner/repo/pull/12" default-directory))
               (data (magit-dash-review--read-json-file rf)))
          (should (equal (map-elt data "root") (file-name-as-directory wt-dir))))))))

(provide 'test-magit-dash-review)
;;; test-magit-dash-review.el ends here
