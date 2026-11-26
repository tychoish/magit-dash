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
  "Execute BODY with `default-directory' set to a fresh temp directory.
`magic-mode-alist' is cleared so that a loaded diff-review does not open the
review files these tests visit in its read-only mode."
  (declare (indent 0))
  `(let ((dir (make-temp-file "magit-dash-review-test-" t))
         (magic-mode-alist nil))
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

(ert-deftest magit-dash-review/fetch-threads-never-sends-slug-through-F ()
  "Owner and repo are raw `-f' fields so `-F' cannot read local files."
  (let ((seen nil))
    (cl-letf (((symbol-function 'magit-dash-gh--call-process)
               (lambda (args &rest _)
                 (setq seen args)
                 (cons 0 "{\"data\":{\"repository\":{\"pullRequest\":{\"reviewThreads\":{\"nodes\":[]}}}}}"))))
      (magit-dash-pr-fetch-threads-graphql "@secret/@repo" 5))
    (should seen)
    (seq-do (lambda (pair)
              (when (string-match-p "\\`\\(owner\\|repo\\)=" (cadr pair))
                (should (equal "-f" (car pair)))))
            (seq-partition seen 2))
    (should (member "owner=@secret" seen))))


;;; Submit through code-review

(defvar code-review-github-host)
(defvar code-review-auth-backend)
(defvar magit-dash-review-submit-time-limit)

(defvar magit-dash-review-test/cr-calls nil
  "Calls the fake code-review API saw, newest first, as (NAME . ARGS).")

(defmacro magit-dash-review-test/with-cr (spec &rest body)
  "Run BODY with the code-review API faked and the code-review path on.
SPEC is a plist.  :submit is called as (KEYS ON-ITEM) and returns the submit
result; :reconcile returns the reconcile plist; :cr-verdict is the verdict
table the seam reports for code-review's pending verdict; :baseline is what
the pre-send snapshot returns.  The calls stay
in `magit-dash-review-test/cr-calls' after BODY."
  (declare (indent 1))
  `(let ((magit-dash-review-submit-via-code-review t)
         (magit-dash-review-sync-viewed nil)
         (spec ,spec))
     (setq magit-dash-review-test/cr-calls nil)
     (cl-letf (((symbol-function 'magit-dash-gh--repo-account)
                (lambda (&rest _) '(:user "me" :host "github.com")))
               ((symbol-function 'magit-dash-gh--call-with-account)
                (lambda (_account fn) (funcall fn)))
               ((symbol-function 'magit-dash-review--fetch-pr-metadata)
                (lambda (&rest _) (list (cons 'headRefOid (or (plist-get spec :live-head) "H1")))))
               ((symbol-function 'code-review-auth-resolve)
                (lambda (&rest _)
                  (push (cons 'resolve inhibit-quit) magit-dash-review-test/cr-calls)
                  "token"))
               ((symbol-function 'magit-dash-review--trusted-worktree)
                (lambda (_file wt) wt))
               ((symbol-function 'code-review-api-submit)
                (lambda (repo number &rest keys)
                  (push (list 'submit repo number keys) magit-dash-review-test/cr-calls)
                  (funcall (plist-get spec :submit) keys (plist-get keys :on-item))))
               ((symbol-function 'code-review-api-reconcile)
                (lambda (repo number &rest keys)
                  (push (list 'reconcile repo number keys) magit-dash-review-test/cr-calls)
                  (funcall (or (plist-get spec :reconcile)
                               (lambda () '(:reviews nil :comments nil))))))
               ((symbol-function 'magit-dash-review--cr-snapshot)
                (lambda (&rest _)
                  (or (plist-get spec :baseline) '(:reviews nil :comments nil))))
               ((symbol-function 'code-review-api-set-verdict)
                (lambda (repo number state &rest _)
                  (push (list 'set-verdict repo number state) magit-dash-review-test/cr-calls)))
               ((symbol-function 'redisplay) #'ignore))
       (let ((magit-dash-review-json-code-review-function
              (lambda (op _key &optional _arg)
                (when (eq op 'verdict)
                  (plist-get spec :cr-verdict)))))
         ,@body))))

(defun magit-dash-review-test/cr-call (name)
  "The newest fake code-review call NAME, as its argument list."
  (cdr (assq name magit-dash-review-test/cr-calls)))

(defun magit-dash-review-test/cr-count (name)
  "How many fake code-review calls NAME were made."
  (length (seq-filter (lambda (call) (eq (car call) name)) magit-dash-review-test/cr-calls)))

(defun magit-dash-review-test/cr-ok-submit (keys on-item)
  "Fake submit acknowledging every item of KEYS through ON-ITEM."
  (let ((n 100))
    (funcall on-item 'review 'sent 500 nil)
    (seq-do (lambda (c)
              (funcall on-item (plist-get c :id) 'sent (setq n (1+ n)) nil))
            (plist-get keys :comments))
    (seq-do (lambda (r)
              (funcall on-item (plist-get r :id) 'sent (setq n (1+ n)) nil))
            (plist-get keys :replies))
    (seq-do (lambda (r)
              (funcall on-item (plist-get r :id) 'sent nil nil))
            (plist-get keys :resolve))
    (list :sent '(review) :failed nil :event (plist-get keys :event) :head (plist-get keys :head-oid))))

(defun magit-dash-review-test/cr-comment (id body &rest kv)
  "A new local comment ID at n.el:2 with BODY and extra KV."
  (apply #'magit-dash-review-test/c "id" id "path" "n.el" "lines" [2 2] "body" body
         "resolved" :false "replies" [] kv))

(ert-deftest magit-dash-review/cr-submit-keys-and-stamps-for-each-verdict ()
  "Each verdict state maps to its event; keys carry head, account, body and items."
  (seq-do
   (lambda (case)
     (magit-dash-review-test/with-temp-dir
       (let ((file (magit-dash-review-test/submit-file
                    (list (magit-dash-review-test/cr-comment "n" "new thread"))
                    (magit-dash-review-test/c "state" (car case) "head" "H1"))))
         (magit-dash-review-test/with-cr (list :submit #'magit-dash-review-test/cr-ok-submit)
           (magit-dash-review-submit file "summary"))
         (let* ((call (magit-dash-review-test/cr-call 'submit))
                (keys (nth 2 call))
                (item (car (plist-get keys :comments)))
                (data (magit-dash-review--read-json-file file))
                (c (aref (map-elt data "comments") 0)))
           (should (equal '("owner/repo" 99) (seq-take call 2)))
           (should (equal (cdr case) (plist-get keys :event)))
           (should (equal "H1" (plist-get keys :head-oid)))
           (should (equal "me" (plist-get keys :account)))
           (should (equal "summary" (plist-get keys :body)))
           (should (equal '(:id "c:n" :path "n.el" :body "new thread" :line 2 :side "RIGHT") item))
           (should (equal "sent" (map-elt c "status")))
           (should (equal "101" (map-elt c "remote_id")))
           (should (= 101 (map-elt c "gh_database_id")))
           (should (equal (car case) (map-elt (map-elt data "verdict") "submitted_state")))
           (should (equal "H1" (map-elt (map-elt data "verdict") "submitted_head")))
           ;; The verdict came from the file, so code-review's own is not touched.
           (should (= 0 (magit-dash-review-test/cr-count 'set-verdict)))))))
   '(("approved" . "APPROVE") ("changes" . "REQUEST_CHANGES") ("comment" . "COMMENT"))))

(ert-deftest magit-dash-review/cr-submit-multi-line-and-left-side ()
  (let ((item (magit-dash-review--cr-comment-item
               (magit-dash-review-test/c "id" "m" "path" "a.el" "lines" [3 6] "body" "b"
                                         "diff_side" "LEFT"))))
    (should (equal '(:id "c:m" :path "a.el" :body "b" :line 6 :side "LEFT"
                         :start-line 3 :start-side "LEFT")
                   item))))

(ert-deftest magit-dash-review/cr-submit-stamps-each-item-as-acknowledged ()
  "A crash after the first acknowledgement keeps its stamp."
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "a" "one")
                       (magit-dash-review-test/cr-comment "b" "two")))))
      (magit-dash-review-test/with-cr
          (list :submit (lambda (_keys on-item)
                          (funcall on-item "c:a" 'sent 777 nil)
                          (error "crash")))
        (should-error (magit-dash-review-submit file)))
      (let ((comments (magit-dash-review-test/read-comments file)))
        (should (equal "sent" (map-elt (car comments) "status")))
        (should (equal "777" (map-elt (car comments) "remote_id")))
        (should-not (map-elt (cadr comments) "remote_id"))
        (should-not (equal "sent" (map-elt (cadr comments) "status")))))))

(ert-deftest magit-dash-review/cr-submit-failure-keeps-drafts-and-verdict ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "n" "new")
                       (magit-dash-review-test/cr-comment "d" "draft" "status" "draft"))
                 (magit-dash-review-test/c "state" "approved" "head" "H1"))))
      (magit-dash-review-test/with-cr
          (list :submit (lambda (_keys _on-item)
                          (list :sent nil
                                :failed (list '(:id review :error "422" :status 422 :retryable nil :maybe-accepted nil)
                                              '(:id "c:n" :error "422" :status 422 :retryable nil :maybe-accepted nil)))))
        (should-error (magit-dash-review-submit file)))
      (let ((comments (magit-dash-review-test/read-comments file)))
        (should (equal "draft" (map-elt (cadr comments) "status")))
        (should-not (map-elt (car comments) "remote_id"))
        (should-not (equal "sent" (map-elt (car comments) "status")))
        (should-not (map-elt (map-elt (magit-dash-review--read-json-file file) "verdict")
                             "submitted_at")))
      (should (= 0 (magit-dash-review-test/cr-count 'set-verdict)))
      (should (= 0 (magit-dash-review-test/cr-count 'reconcile))))))

(ert-deftest magit-dash-review/cr-submit-maybe-accepted-reconciles-never-resends ()
  "A found review is stamped sent without a second submit; a missing one is reported."
  (let ((maybe '((:id review :error "502" :status 502 :retryable nil :maybe-accepted t)
                 (:id "c:n" :error "502" :status 502 :retryable nil :maybe-accepted t))))
    ;; Found on GitHub.
    (magit-dash-review-test/with-temp-dir
      (let ((file (magit-dash-review-test/submit-file
                   (list (magit-dash-review-test/cr-comment "n" "new"))
                   (magit-dash-review-test/c "state" "approved" "head" "H1"))))
        (magit-dash-review-test/with-cr
            (list :submit (lambda (_keys _on-item)
                            (list :sent nil :failed maybe :head "H1"))
                  :reconcile (lambda ()
                               (list :reviews '((:id 9 :node-id "PRR_9" :user "me" :commit-id "H1"
                                                      :body "sum" :state "APPROVED"))
                                     :comments '((:id 41 :node-id "PRRC_41" :user "me" :commit-id "H1"
                                                       :body "new" :path "n.el" :line 2)))))
          (magit-dash-review-submit file "sum"))
        (should (= 1 (magit-dash-review-test/cr-count 'submit)))
        (should (= 1 (magit-dash-review-test/cr-count 'reconcile)))
        (let ((c (car (magit-dash-review-test/read-comments file))))
          (should (equal "sent" (map-elt c "status")))
          (should (equal "PRRC_41" (map-elt c "remote_id")))
          (should (equal "PRRC_41" (map-elt c "gh_comment_id")))
          (should (= 41 (map-elt c "gh_database_id"))))
        (should-not (map-elt (magit-dash-review--read-json-file file) "submit_pending"))
        (should (equal "approved" (map-elt (map-elt (magit-dash-review--read-json-file file) "verdict")
                                           "submitted_state")))))
    ;; Not found, and GitHub could not be checked.
    (seq-do
     (lambda (recon)
       (magit-dash-review-test/with-temp-dir
         (let ((file (magit-dash-review-test/submit-file
                      (list (magit-dash-review-test/cr-comment "n" "new"))
                      (magit-dash-review-test/c "state" "approved" "head" "H1"))))
           (magit-dash-review-test/with-cr
               (list :submit (lambda (_keys _on-item)
                               (list :sent nil :failed maybe :head "H1"))
                     :reconcile (lambda () (cdr recon)))
             (let ((err (should-error (magit-dash-review-submit file "sum"))))
               (should (string-match-p (car recon) (cadr err)))))
           (should (= 1 (magit-dash-review-test/cr-count 'submit)))
           (should (= 1 (magit-dash-review-test/cr-count 'reconcile)))
           (should (= 0 (magit-dash-review-test/cr-count 'set-verdict)))
           (should-not (map-elt (car (magit-dash-review-test/read-comments file)) "remote_id"))
           ;; Not found, or unverifiable, so soon after a failure proves nothing: it
           ;; may still land, and the record stays for the next submit.
           (should (map-elt (magit-dash-review--read-json-file file) "submit_pending")))))
     '(("may or may not have reached GitHub" :reviews nil :comments nil)
       ("may or may not have reached GitHub" :errors ("timeout"))))))

(ert-deftest magit-dash-review/cr-submit-refuses-stale-approval ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "n" "new"))
                 (magit-dash-review-test/c "state" "approved" "head" "H0"))))
      (magit-dash-review-test/with-cr (list :submit #'magit-dash-review-test/cr-ok-submit)
        (should-error (magit-dash-review-submit file) :type 'user-error))
      (should (= 0 (magit-dash-review-test/cr-count 'submit))))))

(ert-deftest magit-dash-review/cr-submit-defaults-event-from-code-review-verdict ()
  "No verdict in the file: code-review's pending verdict sets the event and is cleared."
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "n" "new")))))
      (magit-dash-review-test/with-cr
          (list :submit #'magit-dash-review-test/cr-ok-submit
                :cr-verdict (magit-dash-review-test/c "state" "changes" "head" "H1"))
        (magit-dash-review-submit file "needs work"))
      (let ((keys (nth 2 (magit-dash-review-test/cr-call 'submit))))
        (should (equal "REQUEST_CHANGES" (plist-get keys :event)))
        (should (equal "H1" (plist-get keys :head-oid))))
      (should (= 1 (magit-dash-review-test/cr-count 'set-verdict)))
      (should (equal "changes"
                     (map-elt (map-elt (magit-dash-review--read-json-file file) "verdict")
                              "submitted_state"))))))

(ert-deftest magit-dash-review/cr-submit-flag-off-keeps-legacy-path ()
  (let ((magit-dash-review-submit-via-code-review nil)
        (called nil))
    (cl-letf (((symbol-function 'magit-dash-review--legacy-submit)
               (lambda (&rest _) (setq called 'legacy)))
              ((symbol-function 'magit-dash-review--cr-submit)
               (lambda (&rest _) (setq called 'cr))))
      (magit-dash-review-submit "/tmp/x.json" "f")
      (should (eq 'legacy called))
      (let ((magit-dash-review-submit-via-code-review t))
        (magit-dash-review-submit "/tmp/x.json" "f")
        (should (eq 'cr called))))))

(ert-deftest magit-dash-review/cr-submit-replies-and-resolutions ()
  "Replies use the REST id of their thread; resolutions are stamped."
  (magit-dash-review-test/with-temp-dir
    (let* ((rest-thread (magit-dash-review-test/gh-thread "a" "via api"))
           (res-thread (magit-dash-review-test/gh-thread "c")))
      (setf (map-elt rest-thread "gh_database_id") 900)
      (setf (map-elt res-thread "resolved") t)
      (let ((file (magit-dash-review-test/submit-file (list rest-thread res-thread))))
        (magit-dash-review-test/with-cr (list :submit #'magit-dash-review-test/cr-ok-submit)
          (magit-dash-review-submit file))
        (let* ((keys (nth 2 (magit-dash-review-test/cr-call 'submit)))
               (reply (car (plist-get keys :replies)))
               (resolve (car (plist-get keys :resolve)))
               (comments (magit-dash-review-test/read-comments file)))
          (should-not (plist-get keys :event))
          (should (= 1 (length (plist-get keys :replies))))
          (should (equal 900 (plist-get reply :reply-to-id)))
          (should (equal "via api" (plist-get reply :body)))
          (should (equal '(:id "t:T-c" :thread-id "T-c" :resolved t :remote-resolved nil) resolve))
          (should (equal "sent" (map-elt (aref (map-elt (nth 0 comments) "replies") 0) "status")))
          (should (eq t (map-elt (nth 1 comments) "gh_resolved"))))))))

(ert-deftest magit-dash-review/h5-a-reply-without-a-rest-id-refuses-before-sending ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/gh-thread "b" "via graphql")
                       (magit-dash-review-test/cr-comment "n" "new"))))
          (posted nil))
      (cl-letf (((symbol-function 'magit-dash-review--post-reply)
                 (lambda (&rest _) (setq posted t))))
        (magit-dash-review-test/with-cr (list :submit #'magit-dash-review-test/cr-ok-submit)
          (let ((err (should-error (magit-dash-review-submit file) :type 'user-error)))
            (should (string-match-p "sync" (cadr err))))
          (should (= 0 (magit-dash-review-test/cr-count 'submit)))))
      (should-not posted)
      (should-not (map-elt (magit-dash-review--read-json-file file) "submit_pending")))))

(ert-deftest magit-dash-review/cr-submit-needs-an-account ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "n" "new")))))
      (magit-dash-review-test/with-cr (list :submit #'magit-dash-review-test/cr-ok-submit)
        (cl-letf (((symbol-function 'magit-dash-gh--repo-account) (lambda (&rest _) nil)))
          (should-error (magit-dash-review-submit file) :type 'user-error)))
      (should (= 0 (magit-dash-review-test/cr-count 'submit))))))

;; Reconcile entries as code-review-api-reconcile returns them.
(defconst magit-dash-review-test/recon
  '(:reviews ((:id 1 :node-id "R1" :user "me" :commit-id "H1" :body " sum " :state "APPROVED")
              (:id 2 :user "me" :commit-id "H2" :body "sum" :state "APPROVED")
              (:id 3 :user "other" :commit-id "H1" :body "blank" :state "APPROVED"))
    :comments ((:id 5 :node-id "C5" :user "me" :commit-id "H1" :body "x" :path "a.el" :line 4
                    :in-reply-to-id 7)))
  "A reconcile result.")

(ert-deftest magit-dash-review/cr-match-posted-by-account-body-commit-and-line ()
  (let ((recon magit-dash-review-test/recon))
    (should (= 1 (plist-get (magit-dash-review--cr-match-posted
                             'review '(:body "sum" :event "APPROVE") recon "H1" "me")
                            :id)))
    (should-not (magit-dash-review--cr-match-posted
                 'review '(:body "sum" :event "COMMENT") recon "H1" "me"))
    (should-not (magit-dash-review--cr-match-posted
                 'review '(:body "blank" :event "APPROVE") recon "H1" "me"))
    (should-not (magit-dash-review--cr-match-posted
                 'review '(:body "sum" :event "APPROVE") recon nil "me"))
    (should-not (magit-dash-review--cr-match-posted
                 'review '(:body "sum" :event "APPROVE") recon "H1" nil))
    (should (= 5 (plist-get (magit-dash-review--cr-match-posted
                             'comment '(:body "x" :path "a.el" :line 4) recon "H1" "me")
                            :id)))
    (should-not (magit-dash-review--cr-match-posted
                 'comment '(:body "x" :path "a.el" :line 5) recon "H1" "me"))
    (should-not (magit-dash-review--cr-match-posted
                 'comment '(:body "x" :path "b.el" :line 4) recon "H1" "me"))
    (should (= 5 (plist-get (magit-dash-review--cr-match-posted
                             'reply '(:body "x" :reply-to-id 7) recon nil "me")
                            :id)))))

(defconst magit-dash-review-test/none '(:reviews nil :comments nil) "An empty baseline.")

(ert-deftest magit-dash-review/cr-reconcile-matches-only-per-item-and-consumes ()
  (let* ((item (lambda (id)
                 (cons id (cons 'comment (list :id id :body "x" :path "a.el" :line 4)))))
         (items (list (funcall item "c:a") (funcall item "c:b"))))
    (cl-letf (((symbol-function 'code-review-api-reconcile)
               (lambda (&rest _) magit-dash-review-test/recon)))
      ;; One remote entry satisfies only one of two identical items.
      (let ((found (magit-dash-review--cr-reconcile
                    '("c:a" "c:b") items "H1" "me" "o/r" 1 magit-dash-review-test/none)))
        (should (equal '(("c:a" 5 "C5")) (plist-get found :matched)))
        (should (equal '("c:b") (plist-get found :missing)))
        (should-not (plist-get found :unchecked)))
      ;; Another account's entries never match.
      (should-not (plist-get (magit-dash-review--cr-reconcile
                              '("c:a") items "H1" "you" "o/r" 1 magit-dash-review-test/none)
                             :matched))
      ;; The baseline (already there before the send) and ids stamped elsewhere
      ;; in the file are not candidates.
      (should-not (plist-get (magit-dash-review--cr-reconcile
                              '("c:a") items "H1" "me" "o/r" 1 '(:reviews nil :comments (5)))
                             :matched))
      (should-not (plist-get (magit-dash-review--cr-reconcile
                              '("c:a") items "H1" "me" "o/r" 1 magit-dash-review-test/none '(:sent ("C5")))
                             :matched))
      (should-not (plist-get (magit-dash-review--cr-reconcile
                              '("c:a") items "H1" "me" "o/r" 1 magit-dash-review-test/none '(:sent (5)))
                             :matched))
      ;; No head, or no baseline: nothing can be told.
      (should (equal '("c:a") (plist-get (magit-dash-review--cr-reconcile
                                          '("c:a") items nil "me" "o/r" 1 magit-dash-review-test/none)
                                         :unchecked)))
      (should (equal '("c:a") (plist-get (magit-dash-review--cr-reconcile
                                          '("c:a") items "H1" "me" "o/r" 1 nil)
                                         :unchecked))))
    ;; A review that matches does NOT make unsent comments sent.
    (cl-letf (((symbol-function 'code-review-api-reconcile)
               (lambda (&rest _)
                 '(:reviews ((:id 1 :user "me" :commit-id "H1" :body "sum" :state "APPROVED"))))))
      (let ((found (magit-dash-review--cr-reconcile
                    '(review "c:a")
                    (cons (cons 'review (cons 'review '(:body "sum" :event "APPROVE"))) items)
                    "H1" "me" "o/r" 1 magit-dash-review-test/none)))
        (should (equal '((review 1 nil)) (plist-get found :matched)))
        (should (equal '("c:a") (plist-get found :missing)))))
    ;; A comment of another review does not match.
    (cl-letf (((symbol-function 'code-review-api-reconcile)
               (lambda (&rest _)
                 '(:reviews ((:id 1 :user "me" :commit-id "H1" :body "sum" :state "APPROVED"))
                   :comments ((:id 8 :review-id 2 :user "me" :commit-id "H1" :body "x"
                                   :path "a.el" :line 4))))))
      (should (equal '(review 1 nil)
                     (car (plist-get (magit-dash-review--cr-reconcile
                                      '(review "c:a")
                                      (cons (cons 'review (cons 'review '(:body "sum" :event "APPROVE")))
                                            items)
                                      "H1" "me" "o/r" 1 magit-dash-review-test/none)
                                     :matched)))))
    ;; A failed lookup leaves everything unchecked.
    (cl-letf (((symbol-function 'code-review-api-reconcile)
               (lambda (&rest _) (error "boom"))))
      (let ((found (magit-dash-review--cr-reconcile
                    '("c:a") items "H1" "me" "o/r" 1 magit-dash-review-test/none)))
        (should (equal '("c:a") (plist-get found :unchecked)))
        (should-not (plist-get found :missing))))))

(ert-deftest magit-dash-review/cr-reply-items-split-api-and-graphql-per-thread ()
  (let* ((a (magit-dash-review-test/gh-thread "a" "one" "two"))
         (b (magit-dash-review-test/gh-thread "b" "three"))
         (gone (magit-dash-review-test/gh-thread "g" "four")))
    (setf (map-elt a "gh_database_id") 900)
    (setf (map-elt gone "gh_missing") t)
    (let ((found (magit-dash-review--cr-reply-items (list a b gone))))
      (should (equal '("r:r-one" "r:r-two") (seq-map (lambda (i) (plist-get i :id)) (car found))))
      (should (equal '(900 900) (seq-map (lambda (i) (plist-get i :reply-to-id)) (car found))))
      (should (equal '(("r-three")) (cdr found))))))


(ert-deftest magit-dash-review/viewed-mutation-names ()
  (should (string-match-p "markFileAsViewed" (magit-dash-review--viewed-mutation t)))
  (should-not (string-match-p "unmark" (magit-dash-review--viewed-mutation t)))
  (should (string-match-p "unmarkFileAsViewed" (magit-dash-review--viewed-mutation nil))))

(ert-deftest magit-dash-review/common-dir-falls-back-when-diff-review-signals ()
  "A loaded diff-review that errors on a non-git directory must not break the lookup."
  (magit-dash-review-test/with-temp-dir
    (cl-letf (((symbol-function 'diff-review--common-dir)
               (lambda (_root) (error "Git rev-parse: not a git repository"))))
      (should (stringp (magit-dash-review--common-dir dir))))))

(ert-deftest magit-dash-review/legacy-submit-direct ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/c "id" "n" "path" "n.el" "lines" [2 2] "body" "new")))))
      (magit-dash-review-test/with-gh (magit-dash-review-test/gh-default "H1")
        (magit-dash-review--legacy-submit file nil)
        (should (memq 'review (magit-dash-review-test/kinds)))
        (should (equal "sent" (map-elt (car (magit-dash-review-test/read-comments file)) "status")))))))

(ert-deftest magit-dash-review/cr-submit-writes-pending-and-resubmit-reconciles-first ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "n" "new")))))
      ;; The send is accepted by GitHub but the process dies before any stamp.
      (magit-dash-review-test/with-cr
          (list :submit (lambda (_keys _on-item) (error "killed")))
        (should-error (magit-dash-review-submit file)))
      (should (map-elt (magit-dash-review--read-json-file file) "submit_pending"))
      ;; GitHub cannot be checked: refuse, send nothing.
      (magit-dash-review-test/with-cr
          (list :submit #'magit-dash-review-test/cr-ok-submit
                :reconcile (lambda () '(:errors ("down"))))
        (should-error (magit-dash-review-submit file) :type 'user-error)
        (should (= 0 (magit-dash-review-test/cr-count 'submit))))
      ;; GitHub has it: stamped, and not sent again.
      (magit-dash-review-test/with-cr
          (list :submit #'magit-dash-review-test/cr-ok-submit
                :reconcile (lambda ()
                             '(:reviews ((:id 9 :user "me" :commit-id "H1" :body "" :state "COMMENTED"))
                               :comments ((:id 41 :node-id "N41" :user "me" :commit-id "H1"
                                               :body "new" :path "n.el" :line 2)))))
        (magit-dash-review-submit file))
      (should-not (plist-get (nth 2 (magit-dash-review-test/cr-call 'submit)) :comments))
      (should (equal "N41" (map-elt (car (magit-dash-review-test/read-comments file)) "gh_comment_id")))
      (should-not (map-elt (magit-dash-review--read-json-file file) "submit_pending")))))

(ert-deftest magit-dash-review/cr-submit-replays-failed-stamps-from-the-result ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "n" "new"))))
          (real (symbol-function 'magit-dash-review--cr-stamp))
          (calls 0))
      (cl-letf (((symbol-function 'magit-dash-review--cr-stamp)
                 (lambda (&rest args)
                   (setq calls (1+ calls))
                   (when (= calls 1)
                     (error "disk"))
                   (apply real args))))
        (magit-dash-review-test/with-cr
            (list :submit (lambda (_keys on-item)
                            (funcall on-item "c:n" 'sent 12 '(:node-id "N12"))
                            (list :sent '("c:n") :failed nil :remote-ids '(("c:n" . 12))
                                  :node-ids '(("c:n" . "N12")))))
          (magit-dash-review-submit file "x")))
      (let ((c (car (magit-dash-review-test/read-comments file))))
        (should (equal "sent" (map-elt c "status")))
        (should (equal "N12" (map-elt c "gh_comment_id")))))))

(ert-deftest magit-dash-review/cr-submit-settles-an-empty-comment-verdict ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file nil)))
      (magit-dash-review-test/with-cr
          (list :submit #'magit-dash-review-test/cr-ok-submit
                :cr-verdict (magit-dash-review-test/c "state" "comment" "head" "H1"))
        (magit-dash-review-submit file))
      (should-not (plist-get (nth 2 (magit-dash-review-test/cr-call 'submit)) :event))
      (should (= 1 (magit-dash-review-test/cr-count 'set-verdict)))
      (should (equal "comment" (map-elt (map-elt (magit-dash-review--read-json-file file) "verdict")
                                        "submitted_state"))))))

(ert-deftest magit-dash-review/cr-submit-only-clears-a-verdict-it-sent ()
  "A COMMENT review made of new threads, with no pending verdict, clears nothing."
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "n" "new")))))
      (magit-dash-review-test/with-cr (list :submit #'magit-dash-review-test/cr-ok-submit)
        (magit-dash-review-submit file))
      (should (= 0 (magit-dash-review-test/cr-count 'set-verdict))))))

(ert-deftest magit-dash-review/cr-submit-interactive-confirms-the-blocking-send ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "n" "new"))))
          (prompt nil))
      (magit-dash-review-test/with-cr (list :submit #'magit-dash-review-test/cr-ok-submit)
        (let ((noninteractive nil))
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (p) (setq prompt p) nil)))
            (should-error (magit-dash-review-submit file) :type 'user-error))))
      (should (string-match-p "blocks" prompt))
      (should (string-match-p "worst case" prompt))
      (should (= 0 (magit-dash-review-test/cr-count 'submit))))))

(ert-deftest magit-dash-review/cr-submit-groups-identical-errors ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "a" "one")
                       (magit-dash-review-test/cr-comment "b" "two")))))
      (magit-dash-review-test/with-cr
          (list :submit (lambda (_keys _on-item)
                          (list :failed (list '(:id review :error "422 bad")
                                              '(:id "c:a" :error "422 bad")
                                              '(:id "c:b" :error "422 bad")))))
        (let ((err (should-error (magit-dash-review-submit file))))
          (should (= 2 (length (split-string (cadr err) "422 bad")))))))))

;;; Scenarios: submit, uncertain failure, reconcile, next submit

(defun magit-dash-review-test/uncertain (&rest ids)
  "A fake submit result in which every one of IDS failed with :maybe-accepted."
  (list :sent nil :head "H1"
        :failed (seq-map (lambda (id)
                           (list :id id :error "502" :status 502 :retryable nil :maybe-accepted t))
                         ids)))

(defun magit-dash-review-test/pending-ids (file)
  "The item ids in FILE's submit_pending record."
  (seq-map (lambda (e) (map-elt e "id"))
           (append (map-elt (map-elt (magit-dash-review--read-json-file file) "submit_pending")
                            "items")
                   nil)))

(ert-deftest magit-dash-review/scenario-an-older-empty-review-never-settles-new-comments ()
  "An empty COMMENTED review at the same head, there before the send, matches nothing."
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "n" "new")))))
      (magit-dash-review-test/with-cr
          (list :submit (lambda (_keys _on-item) (magit-dash-review-test/uncertain 'review "c:n"))
                :baseline '(:reviews (3) :comments nil)
                :reconcile (lambda ()
                             '(:reviews ((:id 3 :user "me" :commit-id "H1" :body "" :state "COMMENTED"))
                               :comments nil)))
        (should-error (magit-dash-review-submit file)))
      (let ((c (car (magit-dash-review-test/read-comments file))))
        (should-not (equal "sent" (map-elt c "status")))
        (should-not (map-elt c "remote_id")))
      (should (equal '("review" "c:n") (magit-dash-review-test/pending-ids file))))))

(ert-deftest magit-dash-review/scenario-a-landed-review-without-the-comment-leaves-it-unsent ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "n" "new")))))
      (magit-dash-review-test/with-cr
          (list :submit (lambda (_keys _on-item) (magit-dash-review-test/uncertain 'review "c:n"))
                :reconcile (lambda ()
                             '(:reviews ((:id 9 :user "me" :commit-id "H1" :body "" :state "COMMENTED"))
                               :comments nil)))
        (should-error (magit-dash-review-submit file)))
      (should-not (equal "sent" (map-elt (car (magit-dash-review-test/read-comments file)) "status")))
      ;; The review itself landed: it is settled, the comment stays pending.
      (should (equal '("c:n") (magit-dash-review-test/pending-ids file))))))

(ert-deftest magit-dash-review/scenario-an-old-identical-reply-is-not-the-new-one ()
  "A new \"Done\" reply must not take the id of an older \"Done\" stamped on another reply."
  (magit-dash-review-test/with-temp-dir
    (let* ((thread (magit-dash-review-test/gh-thread "a" "Done"))
           (old (magit-dash-review-test/c "id" "r-old" "body" "Done" "status" "sent"
                                          "gh_database_id" 901 "gh_comment_id" "N901")))
      (setf (map-elt thread "gh_database_id") 900)
      (setf (map-elt thread "replies") (vector old (aref (map-elt thread "replies") 0)))
      (let ((file (magit-dash-review-test/submit-file (list thread))))
        (magit-dash-review-test/with-cr
            (list :submit (lambda (_keys _on-item) (magit-dash-review-test/uncertain "r:r-Done"))
                  :reconcile (lambda ()
                               '(:reviews nil
                                 :comments ((:id 901 :node-id "N901" :user "me" :body "Done"
                                                 :in-reply-to-id 900)))))
          (should-error (magit-dash-review-submit file)))
        (let ((replies (map-elt (car (magit-dash-review-test/read-comments file)) "replies")))
          (should (= 901 (map-elt (aref replies 0) "gh_database_id")))
          (should-not (map-elt (aref replies 1) "gh_database_id"))
          (should-not (equal "sent" (map-elt (aref replies 1) "status"))))
        (should (equal '("r:r-Done") (magit-dash-review-test/pending-ids file)))))))

(ert-deftest magit-dash-review/scenario-missing-after-a-timeout-keeps-the-record-until-it-lands ()
  "Not found right after a failure is premature; the next submit reconciles first."
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "n" "new")))))
      (magit-dash-review-test/with-cr
          (list :submit (lambda (_keys _on-item) (magit-dash-review-test/uncertain 'review "c:n")))
        (let ((err (should-error (magit-dash-review-submit file))))
          (should (string-match-p "may or may not have reached GitHub" (cadr err)))))
      (should (equal '("review" "c:n") (magit-dash-review-test/pending-ids file)))
      ;; The slow POST has landed by the next submit.
      (magit-dash-review-test/with-cr
          (list :submit #'magit-dash-review-test/cr-ok-submit
                :baseline '(:reviews nil :comments nil)
                :reconcile (lambda ()
                             '(:reviews ((:id 9 :user "me" :commit-id "H1" :body "" :state "COMMENTED"))
                               :comments ((:id 41 :node-id "N41" :user "me" :commit-id "H1"
                                               :body "new" :path "n.el" :line 2 :review-id 9)))))
        (magit-dash-review-submit file))
      (should-not (plist-get (nth 2 (magit-dash-review-test/cr-call 'submit)) :comments))
      (should (equal "N41" (map-elt (car (magit-dash-review-test/read-comments file)) "gh_comment_id")))
      (should-not (map-elt (magit-dash-review--read-json-file file) "submit_pending")))))

(ert-deftest magit-dash-review/scenario-quit-stays-inhibited-until-the-file-is-settled ()
  "`inhibit-quit' covers the send, the stamps, the pending record and the verdict clear.
\(Emacs 31 in batch signals a set `quit-flag' even then, so the flag itself is not
exercised; the binding is.)"
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "n" "new"))))
          (seen nil))
      (cl-letf* ((stamp (symbol-function 'magit-dash-review--cr-stamp))
                 (drop (symbol-function 'magit-dash-review--cr-pending-drop))
                 ((symbol-function 'magit-dash-review--cr-stamp)
                  (lambda (&rest args)
                    (push (cons 'stamp inhibit-quit) seen)
                    (apply stamp args)))
                 ((symbol-function 'magit-dash-review--cr-pending-drop)
                  (lambda (&rest args)
                    (push (cons 'drop inhibit-quit) seen)
                    (apply drop args))))
        (magit-dash-review-test/with-cr
            (list :submit (lambda (keys on-item)
                            (push (cons 'submit inhibit-quit) seen)
                            (magit-dash-review-test/cr-ok-submit keys on-item))
                  :cr-verdict (magit-dash-review-test/c "state" "approved" "head" "H1"))
          (cl-letf (((symbol-function 'code-review-api-set-verdict)
                     (lambda (&rest _) (push (cons 'verdict inhibit-quit) seen))))
            (magit-dash-review-submit file "sum"))))
      (should (seq-every-p #'cdr seen))
      (should (seq-every-p (lambda (name) (assq name seen)) '(submit stamp drop verdict))))))

(ert-deftest magit-dash-review/scenario-a-failed-stamp-keeps-the-record-and-the-verdict ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "n" "new")))))
      (cl-letf* ((real (symbol-function 'magit-dash-review--cr-stamp))
                 ((symbol-function 'magit-dash-review--cr-stamp)
                  (lambda (f id &rest args)
                    (when (and (stringp id) (string-prefix-p "c:" id))
                      (error "disk full"))
                    (apply real f id args))))
        (magit-dash-review-test/with-cr
            (list :submit #'magit-dash-review-test/cr-ok-submit
                  :cr-verdict (magit-dash-review-test/c "state" "approved" "head" "H1"))
          (should-error (magit-dash-review-submit file "sum"))))
      (should (member "c:n" (magit-dash-review-test/pending-ids file)))
      (should (= 0 (magit-dash-review-test/cr-count 'set-verdict))))))

(ert-deftest magit-dash-review/scenario-an-edited-body-after-an-uncertain-send-is-not-a-new-item ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "n" "new")))))
      (magit-dash-review-test/with-cr
          (list :submit (lambda (_keys _on-item) (error "killed")))
        (should-error (magit-dash-review-submit file)))
      (let ((data (magit-dash-review--read-json-file file)))
        (setf (map-elt (aref (map-elt data "comments") 0) "body") "edited")
        (magit-dash-review--write-json-file file data))
      (magit-dash-review-test/with-cr
          (list :submit #'magit-dash-review-test/cr-ok-submit
                ;; Were the edited text reconciled, this would settle it.
                :reconcile (lambda ()
                             '(:reviews nil
                               :comments ((:id 41 :user "me" :commit-id "H1" :body "edited"
                                               :path "n.el" :line 2)))))
        (should-error (magit-dash-review-submit file) :type 'user-error)
        (should (= 0 (magit-dash-review-test/cr-count 'submit)))))))

(ert-deftest magit-dash-review/cr-submit-refusals-write-nothing ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 nil (magit-dash-review-test/c "state" "changes" "head" "H1"))))
      ;; REQUEST_CHANGES needs a body.
      (magit-dash-review-test/with-cr (list :submit #'magit-dash-review-test/cr-ok-submit)
        (should-error (magit-dash-review-submit file) :type 'user-error))
      (should-not (map-elt (magit-dash-review--read-json-file file) "submit_pending"))
      ;; The API itself refuses (live head moved): the record it would leave is dropped.
      (magit-dash-review-test/with-cr
          (list :submit (lambda (_keys _on-item) (user-error "head moved")))
        (should-error (magit-dash-review-submit file "why") :type 'user-error))
      (should-not (map-elt (magit-dash-review--read-json-file file) "submit_pending"))
      ;; A token that is not the gh account's is refused before anything is written.
      (magit-dash-review-test/with-cr (list :submit #'magit-dash-review-test/cr-ok-submit)
        (let ((code-review-auth-backend 'auth-source))
          (should-error (magit-dash-review-submit file "why") :type 'user-error)))
      (should (= 0 (magit-dash-review-test/cr-count 'submit))))))


(ert-deftest magit-dash-review/clear-submit-pending-resolves-each-item ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "a" "one")
                       (magit-dash-review-test/cr-comment "b" "two"))))
          (prompts nil))
      (magit-dash-review-test/pending-submit file "c:a" "c:b")
      (should (equal '("review" "c:a" "c:b") (magit-dash-review-test/pending-ids file)))
      (magit-dash-review-test/with-cr (list :submit #'magit-dash-review-test/cr-ok-submit)
        ;; review kept, c:a marked sent, c:b discarded.
        (let ((answers (list "keep" "mark sent" "discard (the next submit sends it)")))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_p) t))
                    ((symbol-function 'completing-read)
                     (lambda (prompt &rest _)
                       (push prompt prompts)
                       (pop answers))))
            (magit-dash-review-clear-submit-pending file))))
      (should (seq-some (lambda (p) (string-match-p "not found on GitHub" p)) prompts))
      (should (equal '("review") (magit-dash-review-test/pending-ids file)))
      (let ((comments (magit-dash-review-test/read-comments file)))
        (should (equal "sent" (map-elt (car comments) "status")))
        (should-not (equal "sent" (map-elt (cadr comments) "status"))))
      (should-error (magit-dash-review-clear-submit-pending
                     (magit-dash-review-test/submit-file nil))
                    :type 'user-error))))

(ert-deftest magit-dash-review/viewed-synced-set-records-and-removes ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file nil)))
      (magit-dash-review--viewed-synced-set file "a.el" "abcdef1")
      (should (equal "abcdef1" (map-elt (map-elt (map-elt (magit-dash-review--read-json-file file)
                                                          "magit_dash")
                                                 "viewed_synced")
                                        "a.el")))
      (magit-dash-review--viewed-synced-set file "a.el" nil)
      (should-not (map-elt (map-elt (map-elt (magit-dash-review--read-json-file file) "magit_dash")
                                    "viewed_synced")
                           "a.el")))))

(ert-deftest magit-dash-review/viewed-record-changes-only-after-github-confirms ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file nil)))
      (let ((data (magit-dash-review--read-json-file file)))
        (setf (map-elt data "viewed") (magit-dash-review-test/viewed-table "a.el" "abcdef1234"))
        (setf (map-elt (map-elt data "magit_dash") "viewed_synced")
              (magit-dash-review-test/viewed-table "d.el" "ddddddd111"))
        (magit-dash-review--write-json-file file data))
      (magit-dash-review-test/with-viewed
          (list :head "H1" :fail-paths '("a.el" "d.el")
                :states '(("a.el" . "UNVIEWED") ("d.el" . "VIEWED"))
                :api (magit-dash-review-test/c
                      "a.el" (magit-dash-review-test/c "blob" "abcdef1" "viewed" :false)))
        (magit-dash-review--sync-viewed file '("owner/repo" . 99) default-directory
                                        '(:user "me" :host "github.com"))
        ;; Both requests were tried, with the review's account, and failed: the
        ;; record still says d.el is on GitHub, and a.el is not claimed.
        (should (equal '((mark . "a.el") (unmark . "d.el")) (magit-dash-review-test/gh-mutations)))
        (should (seq-some (lambda (call) (eq (car call) 'account)) magit-dash-review-test/cr-calls))
        (let ((synced (map-elt (map-elt (magit-dash-review--read-json-file file) "magit_dash")
                               "viewed_synced")))
          (should (map-elt synced "d.el"))
          (should-not (map-elt synced "a.el")))))))

(ert-deftest magit-dash-review/viewed-sync-with-no-viewed-table-unmarks-nothing ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file nil)))
      (let ((data (magit-dash-review--read-json-file file)))
        (setf (map-elt data "viewed") [])
        (setf (map-elt (map-elt data "magit_dash") "viewed_synced")
              (magit-dash-review-test/viewed-table "d.el" "ddddddd111"))
        (magit-dash-review--write-json-file file data))
      (magit-dash-review-test/with-viewed
          (list :head "H1" :states '(("d.el" . "VIEWED")) :api (magit-dash-review-test/c))
        (magit-dash-review--sync-viewed file '("owner/repo" . 99) default-directory nil)
        (should-not (magit-dash-review-test/gh-mutations))))))

;;; Third-review scenarios (each written to fail before its fix)

(defun magit-dash-review-test/pending-submit (file &rest ids)
  "Run a submit of FILE whose send times out for IDS, leaving `submit_pending'."
  (magit-dash-review-test/with-cr
      (list :submit (lambda (_keys _on-item) (apply #'magit-dash-review-test/uncertain ids)))
    (should-error (magit-dash-review-submit file "sum"))))

(defconst magit-dash-review-test/landed
  '(:reviews ((:id 9 :node-id "R9" :user "me" :commit-id "H1" :body "sum" :state "COMMENTED"))
    :comments ((:id 41 :node-id "N41" :user "me" :commit-id "H1" :body "new"
                    :path "n.el" :line 2 :review-id 9)))
  "A summary review and its comment, landed.")

(ert-deftest magit-dash-review/p1-a-landed-summary-is-not-posted-again ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "n" "new")))))
      (magit-dash-review-test/pending-submit file 'review "c:n")
      (magit-dash-review-test/with-cr
          (list :submit #'magit-dash-review-test/cr-ok-submit
                :reconcile (lambda () magit-dash-review-test/landed))
        (magit-dash-review-submit file "sum"))
      (should (= 0 (magit-dash-review-test/cr-count 'submit))))))

(ert-deftest magit-dash-review/p1-a-landed-review-with-a-missing-comment-blocks-everything ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "n" "new")))))
      (magit-dash-review-test/pending-submit file 'review "c:n")
      (magit-dash-review-test/with-cr
          (list :submit #'magit-dash-review-test/cr-ok-submit
                :reconcile (lambda () (list :reviews (plist-get magit-dash-review-test/landed :reviews)
                                            :comments nil)))
        (should-error (magit-dash-review-submit file "sum") :type 'user-error)
        (should (= 0 (magit-dash-review-test/cr-count 'submit))))
      ;; The review was matched and stamped; the comment is what stays unresolved.
      (should (equal '("c:n") (magit-dash-review-test/pending-ids file))))))

(ert-deftest magit-dash-review/p2-a-missing-item-is-never-resent-whatever-the-head ()
  "Not found on GitHub after an uncertain send blocks the submit; nothing is posted."
  (seq-do
   (lambda (live-head)
     (magit-dash-review-test/with-temp-dir
       (let ((file (magit-dash-review-test/submit-file
                    (list (magit-dash-review-test/cr-comment "n" "new")))))
         (magit-dash-review-test/pending-submit file 'review "c:n")
         (magit-dash-review-test/with-cr
             (list :submit #'magit-dash-review-test/cr-ok-submit :live-head live-head)
           (let ((err (should-error (magit-dash-review-submit file "sum") :type 'user-error)))
             (should (string-match-p "c:n" (cadr err)))
             (should (string-match-p "magit-dash-review-clear-submit-pending" (cadr err))))
           (should (= 0 (magit-dash-review-test/cr-count 'submit)))))))
   '("H1" "H2")))

(ert-deftest magit-dash-review/p3-a-sync-imported-twin-is-not-resent ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "n" "new" "origin" "agent")))))
      (magit-dash-review-test/pending-submit file 'review "c:n")
      ;; A sync imports the landed comment as a thread of its own.
      (let ((data (magit-dash-review--read-json-file file)))
        (setf (map-elt data "comments")
              (vconcat (map-elt data "comments")
                       (vector (magit-dash-review-test/c
                                "id" "imp" "gh_thread_id" "T41" "gh_comment_id" "N41"
                                "gh_database_id" 41 "origin" "github" "body" "new"
                                "path" "n.el" "lines" [2 2] "replies" []))))
        (magit-dash-review--write-json-file file data))
      (magit-dash-review-test/with-cr
          (list :submit #'magit-dash-review-test/cr-ok-submit
                :reconcile (lambda () magit-dash-review-test/landed))
        (should-error (magit-dash-review-submit file "sum") :type 'user-error))
      (should (= 0 (magit-dash-review-test/cr-count 'submit))))))

(ert-deftest magit-dash-review/p4-account-host-must-be-code-reviews-host ()
  (seq-do
   (lambda (case)
     (magit-dash-review-test/with-temp-dir
       (let ((file (magit-dash-review-test/submit-file
                    (list (magit-dash-review-test/cr-comment "n" "new"))))
             (code-review-github-host (nth 1 case)))
         (magit-dash-review-test/with-cr (list :submit #'magit-dash-review-test/cr-ok-submit)
           (cl-letf (((symbol-function 'magit-dash-gh--repo-account)
                      (lambda (&rest _) (list :user "me" :host (nth 0 case)))))
             (cond
              ((nth 2 case)
               (should-error (magit-dash-review-submit file) :type 'user-error)
               (should (= 0 (magit-dash-review-test/cr-count 'submit))))
              (t
               (magit-dash-review-submit file)
               (should (= 1 (magit-dash-review-test/cr-count 'submit))))))))))
   '(("ghe.corp" "api.github.com" t)
     ("github.com" "ghe.corp/api/v3" t)
     ("github.com" "api.github.com" nil)
     ("GitHub.COM" "API.GitHub.com" nil)
     ("ghe.corp" "https://ghe.corp/api/v3" nil))))

(ert-deftest magit-dash-review/m1-a-held-lock-refuses-and-a-stale-one-is-taken ()
  (magit-dash-review-test/with-temp-dir
    (let* ((file (magit-dash-review-test/submit-file
                  (list (magit-dash-review-test/cr-comment "n" "new"))))
           (lock (concat file ".submit-lock")))
      (with-temp-file lock
        (insert (format "%d %f %s tok" (emacs-pid) (float-time) (system-name))))
      (magit-dash-review-test/with-cr (list :submit #'magit-dash-review-test/cr-ok-submit)
        (should-error (magit-dash-review-submit file) :type 'user-error)
        (should (= 0 (magit-dash-review-test/cr-count 'submit))))
      (should (file-exists-p lock))
      (with-temp-file lock
        (insert (format "%d %f %s tok" 999999 (float-time) (system-name))))
      (magit-dash-review-test/with-cr (list :submit #'magit-dash-review-test/cr-ok-submit)
        (magit-dash-review-submit file))
      (should-not (file-exists-p lock)))))

(ert-deftest magit-dash-review/m1-the-lock-is-released-when-the-submit-fails ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "n" "new")))))
      (magit-dash-review-test/with-cr (list :submit (lambda (_k _o) (error "killed")))
        (should-error (magit-dash-review-submit file)))
      (should-not (file-exists-p (concat file ".submit-lock"))))))

(ert-deftest magit-dash-review/m2-the-legacy-path-refuses-over-a-pending-record ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "n" "new")))))
      (magit-dash-review-test/pending-submit file 'review "c:n")
      (magit-dash-review-test/with-gh (magit-dash-review-test/gh-default "H1")
        (should-error (magit-dash-review--legacy-submit file nil) :type 'user-error)
        (should-not magit-dash-review-test/gh-log)))))

(ert-deftest magit-dash-review/m3-an-edited-or-deleted-pending-item-blocks-the-submit ()
  (seq-do
   (lambda (change)
     (magit-dash-review-test/with-temp-dir
       (let ((file (magit-dash-review-test/submit-file
                    (list (magit-dash-review-test/cr-comment "n" "new")))))
         (magit-dash-review-test/pending-submit file 'review "c:n")
         (let ((data (magit-dash-review--read-json-file file)))
           (funcall change data)
           (magit-dash-review--write-json-file file data))
         (magit-dash-review-test/with-cr (list :submit #'magit-dash-review-test/cr-ok-submit)
           (should-error (magit-dash-review-submit file "sum") :type 'user-error)
           (should (= 0 (magit-dash-review-test/cr-count 'submit))))
         (should (member "c:n" (magit-dash-review-test/pending-ids file))))))
   (list (lambda (data)
           (setf (map-elt (aref (map-elt data "comments") 0) "body") "edited"))
         (lambda (data)
           (setf (map-elt data "comments") [])))))

(ert-deftest magit-dash-review/m4-the-token-is-resolved-before-quit-is-inhibited ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "n" "new")))))
      (magit-dash-review-test/with-cr (list :submit #'magit-dash-review-test/cr-ok-submit)
        (magit-dash-review-submit file))
      (let ((resolved (cdr (assq 'resolve magit-dash-review-test/cr-calls))))
        (should (assq 'resolve magit-dash-review-test/cr-calls))
        (should-not resolved)))))

(ert-deftest magit-dash-review/m4-the-no-quit-region-has-a-time-limit ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "n" "new"))))
          (magit-dash-review-submit-time-limit 0.3))
      (magit-dash-review-test/with-cr
          (list :submit (lambda (_keys _on-item) (sleep-for 3) (list :sent nil)))
        (let ((err (should-error (magit-dash-review-submit file))))
          (should (string-match-p "timed out" (cadr err)))))
      (should (member "c:n" (magit-dash-review-test/pending-ids file))))))



(ert-deftest magit-dash-review/b1-a-retry-with-another-summary-never-posts-a-second-review ()
  (seq-do
   (lambda (case)
     (magit-dash-review-test/with-temp-dir
       (let ((file (magit-dash-review-test/submit-file
                    nil (magit-dash-review-test/c "state" "approved" "head" "H1"))))
         (magit-dash-review-test/with-cr
             (list :submit (lambda (_keys _on-item) (magit-dash-review-test/uncertain 'review)))
           (should-error (magit-dash-review-submit file "sum")))
         (should (equal '("review") (magit-dash-review-test/pending-ids file)))
         (magit-dash-review-test/with-cr
             (list :submit #'magit-dash-review-test/cr-ok-submit
                   :reconcile (lambda () (car case)))
           (condition-case nil
               (magit-dash-review-submit file "")
             (user-error nil))
           (should (= 0 (magit-dash-review-test/cr-count 'submit)))))))
   ;; The approval landed (matched by the summary it was sent with); or it did not.
   (list (list '(:reviews ((:id 9 :user "me" :commit-id "H1" :body "sum" :state "APPROVED"))
                 :comments nil))
         (list '(:reviews nil :comments nil)))))

(ert-deftest magit-dash-review/b2-the-lock-is-taken-before-anything-is-read ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "n" "new"))))
          (reads 0))
      (with-temp-file (concat file ".submit-lock")
        (insert (format "%d %f %s token" (emacs-pid) (float-time) (system-name))))
      (magit-dash-review-test/with-cr (list :submit #'magit-dash-review-test/cr-ok-submit)
        (let ((read (symbol-function 'magit-dash-review--read-for-write)))
          (cl-letf (((symbol-function 'magit-dash-review--read-for-write)
                     (lambda (&rest args)
                       (setq reads (1+ reads))
                       (apply read args))))
            (should-error (magit-dash-review-submit file) :type 'user-error))))
      (should (= 0 reads))
      (should-not (assq 'resolve magit-dash-review-test/cr-calls)))))

(ert-deftest magit-dash-review/b2-another-submit-finishing-while-we-wait-is-seen ()
  "The file is read after the lock is held: a comment the other instance sent is not sent twice."
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "n" "new")))))
      (magit-dash-review-test/with-cr (list :submit #'magit-dash-review-test/cr-ok-submit)
        (let ((lock (symbol-function 'magit-dash-review--cr-lock)))
          (cl-letf (((symbol-function 'magit-dash-review--cr-lock)
                     (lambda (f)
                       ;; The other instance stamps c:n just before we get the lock.
                       (magit-dash-review--cr-stamp f "c:n" 7 "N7")
                       (funcall lock f))))
            (magit-dash-review-submit file))))
      (should-not (plist-get (nth 2 (magit-dash-review-test/cr-call 'submit)) :comments)))))


(ert-deftest magit-dash-review/the-whole-no-quit-region-has-the-time-limit ()
  "A lookup that hangs before the send is abandoned too."
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "n" "new"))))
          (magit-dash-review-submit-time-limit 0.3))
      (magit-dash-review-test/pending-submit file 'review "c:n")
      (magit-dash-review-test/with-cr
          (list :submit #'magit-dash-review-test/cr-ok-submit
                :reconcile (lambda () (sleep-for 3) '(:reviews nil :comments nil)))
        (let ((err (should-error (magit-dash-review-submit file "sum"))))
          (should (string-match-p "timed out" (cadr err))))
        (should (= 0 (magit-dash-review-test/cr-count 'submit)))))))

(ert-deftest magit-dash-review/cr-lock-and-unlock-directly ()
  (magit-dash-review-test/with-temp-dir
    (let* ((file (expand-file-name "review.json" default-directory))
           (path (concat file ".submit-lock"))
           (old (- (float-time) 7200)))
      (let ((lock (magit-dash-review--cr-lock file)))
        (should (equal path (car lock)))
        (should-error (magit-dash-review--cr-lock file) :type 'user-error)
        ;; Another owner's token is never removed.
        (magit-dash-review--cr-unlock (cons path "someone else"))
        (should (file-exists-p path))
        (magit-dash-review--cr-unlock lock)
        (should-not (file-exists-p path)))
      ;; Stale: a dead process on this host, or an old lock of an unknown host.
      (seq-do (lambda (content)
                (with-temp-file path (insert content))
                (let ((lock (magit-dash-review--cr-lock file)))
                  (should (file-exists-p path))
                  (magit-dash-review--cr-unlock lock))
                (should-not (file-exists-p path)))
              (list (format "999999 %f %s tok" (float-time) (system-name))
                    (format "%d %f otherhost tok" 4242 old)))
      ;; Not stale: a live process on this host however old; a young foreign lock.
      (seq-do (lambda (content)
                (with-temp-file path (insert content))
                (should-error (magit-dash-review--cr-lock file) :type 'user-error)
                (should (file-exists-p path))
                (delete-file path))
              (list (format "%d %f %s tok" (emacs-pid) old (system-name))
                    (format "%d %f otherhost tok" 4242 (float-time)))))))

;;; Fifth-review scenarios

(ert-deftest magit-dash-review/b1-a-landed-comment-review-never-stamps-a-newer-verdict ()
  "A COMMENT review landed; the verdict was set to approved since: nothing is stamped or posted."
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "n" "new")))))
      (magit-dash-review-test/pending-submit file 'review "c:n")
      (let ((data (magit-dash-review--read-json-file file)))
        (setf (map-elt data "verdict") (magit-dash-review-test/c "state" "approved" "head" "H1"))
        (magit-dash-review--write-json-file file data))
      (magit-dash-review-test/with-cr
          (list :submit #'magit-dash-review-test/cr-ok-submit
                :reconcile (lambda () magit-dash-review-test/landed))
        (should-error (magit-dash-review-submit file "sum") :type 'user-error)
        (should (= 0 (magit-dash-review-test/cr-count 'submit)))
        (should (= 0 (magit-dash-review-test/cr-count 'set-verdict))))
      (should-not (map-elt (map-elt (magit-dash-review--read-json-file file) "verdict")
                           "submitted_state")))))

(ert-deftest magit-dash-review/b2-a-different-summary-after-a-landed-review-posts-nothing ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "n" "new")))))
      (magit-dash-review-test/pending-submit file 'review "c:n")
      (let ((data (magit-dash-review--read-json-file file)))
        (setf (map-elt data "comments")
              (vconcat (map-elt data "comments")
                       (vector (magit-dash-review-test/cr-comment "m" "another"))))
        (magit-dash-review--write-json-file file data))
      (magit-dash-review-test/with-cr
          (list :submit #'magit-dash-review-test/cr-ok-submit
                :reconcile (lambda () magit-dash-review-test/landed))
        (let ((err (should-error (magit-dash-review-submit file "a different summary")
                                 :type 'user-error)))
          (should (string-match-p "landed" (cadr err))))
        (should (= 0 (magit-dash-review-test/cr-count 'submit))))
      ;; The landed comment was stamped; the new one was not sent.
      (let ((comments (magit-dash-review-test/read-comments file)))
        (should (equal "sent" (map-elt (car comments) "status")))
        (should-not (equal "sent" (map-elt (cadr comments) "status")))))))

(ert-deftest magit-dash-review/b3-the-confirmation-comes-before-the-file-is-read ()
  "What the user edits while the prompt is up is what gets sent."
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "n" "new")))))
      (magit-dash-review-test/with-cr (list :submit #'magit-dash-review-test/cr-ok-submit)
        (let ((noninteractive nil))
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (_prompt)
                       (let ((data (magit-dash-review--read-json-file file)))
                         (setf (map-elt data "comments")
                               (vconcat (map-elt data "comments")
                                        (vector (magit-dash-review-test/cr-comment "z" "added"))))
                         (magit-dash-review--write-json-file file data))
                       t)))
            (magit-dash-review-submit file))))
      (should (equal '("c:n" "c:z")
                     (seq-map (lambda (i) (plist-get i :id))
                              (plist-get (nth 2 (magit-dash-review-test/cr-call 'submit)) :comments)))))))

(ert-deftest magit-dash-review/b4-the-legacy-path-takes-the-lock ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/c "id" "n" "path" "n.el" "lines" [2 2] "body" "new")))))
      (with-temp-file (concat file ".submit-lock")
        (insert (format "%d %f %s tok" (emacs-pid) (float-time) (system-name))))
      (magit-dash-review-test/with-gh (magit-dash-review-test/gh-default "H1")
        (should-error (magit-dash-review--legacy-submit file nil) :type 'user-error)
        (should-not magit-dash-review-test/gh-log))
      (should (file-exists-p (concat file ".submit-lock"))))))

(ert-deftest magit-dash-review/clear-command-marks-sent-with-the-ids-it-found-and-defaults-to-keep ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "n" "new")))))
      (magit-dash-review-test/pending-submit file 'review "c:n")
      (magit-dash-review-test/with-cr (list :submit #'magit-dash-review-test/cr-ok-submit
                                            :reconcile (lambda () magit-dash-review-test/landed))
        ;; The default answer changes nothing.
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_p _c _pred _req _init _hist def) def)))
          (magit-dash-review-clear-submit-pending file))
        (should (equal '("review" "c:n") (magit-dash-review-test/pending-ids file)))
        ;; Mark sent stamps what was found.
        (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "mark sent")))
          (magit-dash-review-clear-submit-pending file)))
      (let ((c (car (magit-dash-review-test/read-comments file))))
        (should (equal "N41" (map-elt c "gh_comment_id")))
        (should (= 41 (map-elt c "gh_database_id"))))
      (should-not (magit-dash-review-test/pending-ids file)))))

(ert-deftest magit-dash-review/clear-command-confirms-risky-choices ()
  (seq-do
   (lambda (case)
     (magit-dash-review-test/with-temp-dir
       (let ((file (magit-dash-review-test/submit-file
                    (list (magit-dash-review-test/cr-comment "n" "new"))))
             (asked nil))
         (magit-dash-review-test/pending-submit file "c:n")
         (magit-dash-review-test/with-cr (list :submit #'magit-dash-review-test/cr-ok-submit
                                               :reconcile (lambda () (car case)))
           (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (cadr case)))
                     ((symbol-function 'yes-or-no-p) (lambda (p) (setq asked p) nil)))
             (magit-dash-review-clear-submit-pending file)))
         (should asked)
         ;; Declined: nothing changed.
         (should (equal '("review" "c:n") (magit-dash-review-test/pending-ids file))))))
   (list (list magit-dash-review-test/landed "discard (the next submit sends it)")
         (list '(:reviews nil :comments nil) "mark sent"))))

(ert-deftest magit-dash-review/clear-command-holds-the-lock-only-while-working ()
  (magit-dash-review-test/with-temp-dir
    (let* ((file (magit-dash-review-test/submit-file
                  (list (magit-dash-review-test/cr-comment "n" "new"))))
           (lock (concat file ".submit-lock"))
           (at-prompt nil))
      (magit-dash-review-test/pending-submit file "c:n")
      (magit-dash-review-test/with-cr (list :submit #'magit-dash-review-test/cr-ok-submit)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _) (push (file-exists-p lock) at-prompt) "keep")))
          (magit-dash-review-clear-submit-pending file))
        (should (equal '(nil nil) at-prompt))
        ;; And it refuses while another submit holds the lock.
        (with-temp-file lock
          (insert (format "%d %f %s tok" (emacs-pid) (float-time) (system-name))))
        (should-error (magit-dash-review-clear-submit-pending file) :type 'user-error)))))

(ert-deftest magit-dash-review/an-item-that-landed-but-was-edited-blocks-and-is-not-stamped ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "n" "new")))))
      (magit-dash-review-test/pending-submit file 'review "c:n")
      (let ((data (magit-dash-review--read-json-file file)))
        (setf (map-elt (aref (map-elt data "comments") 0) "body") "new, edited")
        (magit-dash-review--write-json-file file data))
      (magit-dash-review-test/with-cr
          (list :submit #'magit-dash-review-test/cr-ok-submit
                :reconcile (lambda () magit-dash-review-test/landed))
        (should-error (magit-dash-review-submit file "sum") :type 'user-error)
        (should (= 0 (magit-dash-review-test/cr-count 'submit))))
      (should-not (equal "sent" (map-elt (car (magit-dash-review-test/read-comments file)) "status")))
      (should (member "c:n" (magit-dash-review-test/pending-ids file))))))

(ert-deftest magit-dash-review/match-posted-needs-the-same-body-and-commit ()
  (let ((recon '(:comments ((:id 1 :user "me" :commit-id "H1" :body "x" :path "a.el" :line 4)))))
    (should (magit-dash-review--cr-match-posted
             'comment '(:body "x" :path "a.el" :line 4) recon "H1" "me"))
    (should-not (magit-dash-review--cr-match-posted
                 'comment '(:body "y" :path "a.el" :line 4) recon "H1" "me"))
    (should-not (magit-dash-review--cr-match-posted
                 'comment '(:body "x" :path "a.el" :line 4) recon "H2" "me"))
    (should-not (magit-dash-review--cr-match-posted
                 'comment '(:body "x" :path "a.el" :line 4) recon nil "me"))))

(ert-deftest magit-dash-review/plan-check-and-resolve-record-directly ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "n" "new"))
                 (magit-dash-review-test/c "state" "approved" "head" "H1"))))
      (magit-dash-review-test/with-cr (list :submit #'magit-dash-review-test/cr-ok-submit)
        (let ((plan (magit-dash-review--cr-plan file "sum")))
          (should (equal "APPROVE" (plist-get plan :api-event)))
          (should (equal '("c:n") (seq-map (lambda (i) (plist-get i :id)) (plist-get plan :comment-items))))
          (should (equal "me" (plist-get plan :account)))
          (should-not (magit-dash-review--cr-check plan))
          ;; No feedback with REQUEST_CHANGES is refused by the check.
          (should-error (magit-dash-review--cr-check
                         (plist-put (plist-put (copy-sequence plan) :api-event "REQUEST_CHANGES")
                                     :feedback-p nil))
                        :type 'user-error))))))


;;; Sixth-review scenarios

(defconst magit-dash-review-test/approved-landed
  '(:reviews ((:id 9 :user "me" :commit-id "H1" :body "sum" :state "APPROVED")) :comments nil)
  "An APPROVE review with the summary \"sum\", landed at H1.")

(defun magit-dash-review-test/set-verdict (file state &optional head)
  "Set the verdict of FILE to STATE (at HEAD, default H1)."
  (let ((data (magit-dash-review--read-json-file file)))
    (setf (map-elt data "verdict")
          (magit-dash-review-test/c "state" state "head" (or head "H1")))
    (magit-dash-review--write-json-file file data)))

(ert-deftest magit-dash-review/q1-a-landed-approve-never-makes-a-later-changes-verdict-look-sent ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 nil (magit-dash-review-test/c "state" "approved" "head" "H1"))))
      (magit-dash-review-test/pending-submit file 'review)
      (magit-dash-review-test/set-verdict file "changes")
      (magit-dash-review-test/with-cr
          (list :submit #'magit-dash-review-test/cr-ok-submit
                :reconcile (lambda () magit-dash-review-test/approved-landed))
        (should-error (magit-dash-review-submit file "sum") :type 'user-error)
        (should (= 0 (magit-dash-review-test/cr-count 'submit))))
      ;; REQUEST_CHANGES was never sent, so it is still pending ...
      (should (magit-dash-review--verdict-pending-p
               (map-elt (magit-dash-review--read-json-file file) "verdict")))
      ;; ... and the next submit sends it.
      (magit-dash-review-test/with-cr (list :submit #'magit-dash-review-test/cr-ok-submit)
        (magit-dash-review-submit file "why"))
      (should (equal "REQUEST_CHANGES"
                     (plist-get (nth 2 (magit-dash-review-test/cr-call 'submit)) :event))))))

(ert-deftest magit-dash-review/q1-mark-sent-stamps-the-recorded-verdict-not-the-current-one ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 nil (magit-dash-review-test/c "state" "approved" "head" "H1"))))
      (magit-dash-review-test/pending-submit file 'review)
      (magit-dash-review-test/set-verdict file "changes")
      (magit-dash-review-test/with-cr
          (list :submit #'magit-dash-review-test/cr-ok-submit
                :reconcile (lambda () magit-dash-review-test/approved-landed))
        (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "mark sent")))
          (magit-dash-review-clear-submit-pending file)))
      (let ((verdict (map-elt (magit-dash-review--read-json-file file) "verdict")))
        (should (equal "approved" (map-elt verdict "submitted_state")))
        (should (magit-dash-review--verdict-pending-p verdict))))))

(ert-deftest magit-dash-review/sf2-a-reapproval-at-a-new-head-after-a-landed-one-is-not-success ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 nil (magit-dash-review-test/c "state" "approved" "head" "H1"))))
      (magit-dash-review-test/pending-submit file 'review)
      (let ((data (magit-dash-review--read-json-file file)))
        (setf (map-elt (map-elt data "magit_dash") "head_oid") "H2")
        (setf (map-elt data "verdict") (magit-dash-review-test/c "state" "approved" "head" "H2"))
        (magit-dash-review--write-json-file file data))
      (magit-dash-review-test/with-cr
          (list :submit #'magit-dash-review-test/cr-ok-submit
                :reconcile (lambda () magit-dash-review-test/approved-landed))
        (should-error (magit-dash-review-submit file "sum") :type 'user-error)
        (should (= 0 (magit-dash-review-test/cr-count 'submit))))
      (should (magit-dash-review--verdict-pending-p
               (map-elt (magit-dash-review--read-json-file file) "verdict"))))))

(defun magit-dash-review-test/published-comment (&rest kv)
  "A comment sent through code-review (no thread id), with KV added."
  (apply #'magit-dash-review-test/c
         "id" "n" "status" "sent" "origin" "github" "gh_database_id" 41 "gh_comment_id" "N41"
         "remote_id" "N41" "path" "n.el" "lines" [2 2] "body" "new" "gh_resolved" :false
         "resolved" :false kv))

(ert-deftest magit-dash-review/sf3-a-reply-to-a-comment-sent-through-code-review-is-sent ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/published-comment
                        "replies" (vector (magit-dash-review-test/c "id" "r1" "body" "Done")))))))
      (magit-dash-review-test/with-cr (list :submit #'magit-dash-review-test/cr-ok-submit)
        (magit-dash-review-submit file))
      (let ((reply (car (plist-get (nth 2 (magit-dash-review-test/cr-call 'submit)) :replies))))
        (should (equal 41 (plist-get reply :reply-to-id)))
        (should (equal "Done" (plist-get reply :body)))))))

(ert-deftest magit-dash-review/sf3-resolving-a-thread-without-an-id-refuses-instead-of-skipping ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/published-comment "resolved" t)))))
      (magit-dash-review-test/with-cr (list :submit #'magit-dash-review-test/cr-ok-submit)
        (let ((err (should-error (magit-dash-review-submit file) :type 'user-error)))
          (should (string-match-p "sync" (cadr err))))
        (should (= 0 (magit-dash-review-test/cr-count 'submit)))))))

(ert-deftest magit-dash-review/sf4-an-item-stamped-before-the-crash-does-not-block-forever ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "n" "new")))))
      (magit-dash-review-test/pending-submit file 'review "c:n")
      ;; The send landed and was stamped; only the record was not trimmed.
      (magit-dash-review--cr-stamp file "c:n" 41 "N41")
      (magit-dash-review--cr-pending-drop file '(review))
      (should (equal '("c:n") (magit-dash-review-test/pending-ids file)))
      (magit-dash-review-test/with-cr (list :submit #'magit-dash-review-test/cr-ok-submit)
        (magit-dash-review-submit file))
      (should-not (magit-dash-review-test/pending-ids file))
      (should (= 0 (magit-dash-review-test/cr-count 'submit))))))

(ert-deftest magit-dash-review/sf4-the-clear-command-calls-a-stamped-item-stamped ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "n" "new")))))
      (magit-dash-review-test/pending-submit file "c:n")
      (magit-dash-review--cr-stamp file "c:n" 41 "N41")
      (let ((prompts nil))
        (magit-dash-review-test/with-cr (list :submit #'magit-dash-review-test/cr-ok-submit)
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (p &rest _) (push p prompts) "discard (the next submit sends it)"))
                    ((symbol-function 'yes-or-no-p)
                     (lambda (&rest _) (ert-fail "no question for a stamped item"))))
            (magit-dash-review-clear-submit-pending file)))
        (should (seq-some (lambda (p) (string-match-p "already stamped" p)) prompts))))))

(ert-deftest magit-dash-review/sf5-discarding-an-unchecked-item-asks ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "n" "new"))))
          (asked nil))
      (magit-dash-review-test/pending-submit file "c:n")
      (magit-dash-review-test/with-cr
          (list :submit #'magit-dash-review-test/cr-ok-submit
                :reconcile (lambda () (error "GitHub is down")))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _) "discard (the next submit sends it)"))
                  ((symbol-function 'yes-or-no-p) (lambda (p) (setq asked p) nil)))
          (magit-dash-review-clear-submit-pending file)))
      (should asked)
      (should (member "c:n" (magit-dash-review-test/pending-ids file))))))

(ert-deftest magit-dash-review/a33-the-review-stays-pending-until-its-verdict-is-stamped ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 nil (magit-dash-review-test/c "state" "approved" "head" "H1"))))
      (cl-letf* ((real (symbol-function 'magit-dash-review--cr-stamp))
                 ((symbol-function 'magit-dash-review--cr-stamp)
                  (lambda (f id &rest args)
                    (when (eq id 'review)
                      (error "disk full"))
                    (apply real f id args))))
        (magit-dash-review-test/with-cr (list :submit #'magit-dash-review-test/cr-ok-submit)
          (should-error (magit-dash-review-submit file "sum"))))
      (should (member "review" (magit-dash-review-test/pending-ids file))))))

(ert-deftest magit-dash-review/a04-a-lock-replaced-while-being-judged-is-not-taken ()
  (magit-dash-review-test/with-temp-dir
    (let* ((file (expand-file-name "review.json" default-directory))
           (path (concat file ".submit-lock"))
           (live (format "%d %f %s tok" (emacs-pid) (float-time) (system-name))))
      (with-temp-file path (insert (format "999999 %f %s tok" (float-time) (system-name))))
      (let ((rename (symbol-function 'rename-file)))
        (cl-letf (((symbol-function 'rename-file)
                   (lambda (from to &rest args)
                     ;; Another instance replaces the stale lock just before we move it.
                     (when (equal from path)
                       (with-temp-file path (insert live)))
                     (apply rename from to args))))
          (should-error (magit-dash-review--cr-lock file) :type 'user-error)))
      ;; The live lock was put back.
      (should (equal live (with-temp-buffer (insert-file-contents path) (buffer-string)))))))

(ert-deftest magit-dash-review/a05-losing-the-second-create-refuses ()
  (magit-dash-review-test/with-temp-dir
    (let* ((file (expand-file-name "review.json" default-directory))
           (path (concat file ".submit-lock")))
      (with-temp-file path (insert (format "999999 %f %s tok" (float-time) (system-name))))
      (let ((delete (symbol-function 'delete-file)))
        (cl-letf (((symbol-function 'delete-file)
                   (lambda (f &rest args)
                     (apply delete f args)
                     ;; Another instance creates the lock right after the stale one is gone.
                     (when (string-match-p "stale" f)
                       (with-temp-file path
                         (insert (format "%d %f %s tok" (emacs-pid) (float-time) (system-name))))))))
          (should-error (magit-dash-review--cr-lock file) :type 'user-error))))))

(ert-deftest magit-dash-review/a14-the-plan-is-checked-again-after-the-prompt ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 nil (magit-dash-review-test/c "state" "approved" "head" "H1"))))
      (magit-dash-review-test/with-cr (list :submit #'magit-dash-review-test/cr-ok-submit)
        (let ((noninteractive nil))
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (_p)
                       ;; While the prompt is up the verdict becomes REQUEST_CHANGES, no summary.
                       (magit-dash-review-test/set-verdict file "changes")
                       t)))
            (should-error (magit-dash-review-submit file) :type 'user-error))))
      (should (= 0 (magit-dash-review-test/cr-count 'submit))))))

(ert-deftest magit-dash-review/a20-a-record-changed-while-deciding-is-not-applied ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "n" "new")))))
      (magit-dash-review-test/pending-submit file 'review "c:n")
      (magit-dash-review-test/with-cr (list :submit #'magit-dash-review-test/cr-ok-submit)
        (let ((first t))
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _)
                       ;; Someone resolves "review" while we are being asked about it.
                       (when first
                         (setq first nil)
                         (magit-dash-review--cr-pending-drop file '(review)))
                       "discard (the next submit sends it)"))
                    ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (should-error (magit-dash-review-clear-submit-pending file) :type 'user-error))))
      (should (equal '("c:n") (magit-dash-review-test/pending-ids file))))))

(ert-deftest magit-dash-review/a21-the-answers-are-applied-under-the-lock ()
  (magit-dash-review-test/with-temp-dir
    (let* ((file (magit-dash-review-test/submit-file
                  (list (magit-dash-review-test/cr-comment "n" "new"))))
           (lock (concat file ".submit-lock"))
           (held nil))
      (magit-dash-review-test/pending-submit file "c:n")
      (let ((drop (symbol-function 'magit-dash-review--cr-pending-drop)))
        (magit-dash-review-test/with-cr (list :submit #'magit-dash-review-test/cr-ok-submit)
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _) "discard (the next submit sends it)"))
                    ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                    ((symbol-function 'magit-dash-review--cr-pending-drop)
                     (lambda (&rest args)
                       (push (file-exists-p lock) held)
                       (apply drop args))))
            (magit-dash-review-clear-submit-pending file))))
      (should held)
      (should (seq-every-p #'identity held)))))

(ert-deftest magit-dash-review/clear-command-refuses-when-a-sync-imported-the-landed-comment ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "n" "new")))))
      (magit-dash-review-test/pending-submit file 'review "c:n")
      (magit-dash-review-test/with-cr
          (list :submit #'magit-dash-review-test/cr-ok-submit
                :reconcile (lambda () magit-dash-review-test/landed))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _)
                     ;; A sync imports the landed comment while we are deciding.
                     (let ((data (magit-dash-review--read-json-file file)))
                       (setf (map-elt data "comments")
                             (vconcat (map-elt data "comments")
                                      (vector (magit-dash-review-test/c
                                               "id" "imp" "gh_thread_id" "T41" "gh_comment_id" "N41"
                                               "gh_database_id" 41 "origin" "github" "body" "new"
                                               "path" "n.el" "lines" [2 2] "replies" []))))
                       (magit-dash-review--write-json-file file data))
                     "mark sent")))
          (should-error (magit-dash-review-clear-submit-pending file) :type 'user-error)))
      (should-not (equal "sent" (map-elt (car (magit-dash-review-test/read-comments file)) "status"))))))

;;; Direct tests of the pieces of the submit

(ert-deftest magit-dash-review/cr-hosts-direct ()
  (let ((code-review-github-host "api.github.com"))
    (should (equal '("github.com" . "github.com")
                   (magit-dash-review--cr-hosts '(:user "me" :host "GitHub.com"))))
    (should (equal '("ghe.corp" . "github.com")
                   (magit-dash-review--cr-hosts '(:user "me" :host "https://ghe.corp/x"))))))

(ert-deftest magit-dash-review/cr-confirm-direct ()
  (let ((plan (list :slug "o/r" :pr-num 3 :api-replies nil :resolve-items nil :record nil))
        (prompt nil))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (p) (setq prompt p) t)))
      (magit-dash-review--cr-confirm plan))
    (should (string-match-p "o/r#3" prompt))
    (should (string-match-p "blocks" prompt))
    (should (string-match-p "worst case" prompt))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_p) nil)))
      (should-error (magit-dash-review--cr-confirm plan) :type 'user-error))))

(ert-deftest magit-dash-review/cr-survey-direct ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "n" "new")))))
      (should-error (magit-dash-review--cr-survey file) :type 'user-error)
      (magit-dash-review-test/pending-submit file "c:n")
      (magit-dash-review-test/with-cr (list :submit #'magit-dash-review-test/cr-ok-submit
                                            :reconcile (lambda () magit-dash-review-test/landed))
        (let ((survey (magit-dash-review--cr-survey file)))
          (should (equal '("review" "c:n") (seq-map (lambda (e) (map-elt e "id"))
                                                    (plist-get survey :entries))))
          (should (assoc "c:n" (plist-get (plist-get survey :found) :matched)))
          (should (equal '("review" "c:n") (seq-map #'car (nth 4 (plist-get survey :signature)))))))
      (should-not (file-exists-p (concat file ".submit-lock"))))))

(ert-deftest magit-dash-review/cr-resolve-record-and-send-direct ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "n" "new")))))
      (magit-dash-review-test/pending-submit file 'review "c:n")
      (magit-dash-review-test/with-cr
          (list :submit #'magit-dash-review-test/cr-ok-submit
                :reconcile (lambda () magit-dash-review-test/landed))
        (let* ((plan (magit-dash-review--cr-plan file "sum"))
               (run (make-hash-table :test #'eq)))
          (setf (map-elt run 'comment-items) (plist-get plan :comment-items))
          (setf (map-elt run 'api-replies) nil)
          (setf (map-elt run 'api-event) (plist-get plan :api-event))
          (magit-dash-review--cr-resolve-record file plan run)
          (should (member 'review (map-elt run 'settled)))
          (should (member "c:n" (map-elt run 'settled)))
          (should-not (map-elt run 'comment-items))
          (should-not (map-elt run 'api-event))
          (should (equal "sent" (map-elt (car (magit-dash-review-test/read-comments file)) "status")))))
      ;; Sending a fresh plan stamps and settles through `--cr-send'.
      (let ((file2 (magit-dash-review-test/submit-file
                    (list (magit-dash-review-test/cr-comment "m" "other")))))
        (magit-dash-review-test/with-cr (list :submit #'magit-dash-review-test/cr-ok-submit)
          (let* ((plan (magit-dash-review--cr-plan file2 nil))
                 (run (make-hash-table :test #'eq)))
            (setf (map-elt run 'comment-items) (plist-get plan :comment-items))
            (setf (map-elt run 'api-replies) nil)
            (setf (map-elt run 'api-event) (plist-get plan :api-event))
            (magit-dash-review--cr-send file2 plan run)
            (should (member "c:m" (map-elt run 'settled)))
            (should-not (map-elt run 'errors))
            (should (equal "sent" (map-elt (car (magit-dash-review-test/read-comments file2)) "status")))))))))


;;; Seventh-gate scenarios

(ert-deftest magit-dash-review/s1-a-reply-with-the-same-id-does-not-settle-a-comment ()
  (magit-dash-review-test/with-temp-dir
    (let* ((thread (magit-dash-review-test/gh-thread "a"))
           (file (magit-dash-review-test/submit-file
                  (list thread (magit-dash-review-test/cr-comment "n" "new")))))
      (let ((data (magit-dash-review--read-json-file file)))
        (setf (map-elt (aref (map-elt data "comments") 0) "replies")
              (vector (magit-dash-review-test/c "id" "n" "body" "x" "status" "sent"
                                                "gh_database_id" 5)))
        (magit-dash-review--write-json-file file data))
      (magit-dash-review-test/pending-submit file 'review "c:n")
      (magit-dash-review--cr-pending-drop file '(review))
      (magit-dash-review-test/with-cr (list :submit #'magit-dash-review-test/cr-ok-submit)
        (should-error (magit-dash-review-submit file "sum") :type 'user-error)
        (should (= 0 (magit-dash-review-test/cr-count 'submit)))))))

(ert-deftest magit-dash-review/s1-a-comment-with-the-same-id-does-not-settle-a-reply ()
  (magit-dash-review-test/with-temp-dir
    (let* ((file (magit-dash-review-test/submit-file
                  (list (magit-dash-review-test/published-comment "id" "n")))))
      (should-not (magit-dash-review--cr-stamped-p
                   (magit-dash-review--read-json-file file) nil
                   (let ((e (make-hash-table :test #'equal)))
                     (setf (map-elt e "id") "r:n")
                     e)))
      (should (magit-dash-review--cr-stamped-p
               (magit-dash-review--read-json-file file) nil
               (let ((e (make-hash-table :test #'equal)))
                 (setf (map-elt e "id") "c:n")
                 e))))))

(ert-deftest magit-dash-review/s2-a-replaced-record-is-not-the-one-decided-on ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 nil (magit-dash-review-test/c "state" "approved" "head" "H1"))))
      (magit-dash-review-test/pending-submit file 'review)
      (magit-dash-review-test/with-cr
          (list :submit #'magit-dash-review-test/cr-ok-submit
                :reconcile (lambda () magit-dash-review-test/approved-landed))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _)
                     ;; Another instance settles the record and writes a new one
                     ;; with the same ids and hashes but another send behind it.
                     (magit-dash-review--cr-pending-drop file '(review))
                     (magit-dash-review--cr-pending-write
                      file (list (cons 'review (cons 'review '(:body "sum" :event "REQUEST_CHANGES"))))
                      '(:reviews nil :comments nil) "H2" '(:state "changes" :head "H2"))
                     "mark sent")))
          (should-error (magit-dash-review-clear-submit-pending file) :type 'user-error)))
      (should-not (map-elt (map-elt (magit-dash-review--read-json-file file) "verdict")
                           "submitted_state"))
      (should (equal '("review") (magit-dash-review-test/pending-ids file))))))

(ert-deftest magit-dash-review/m4-nothing-checkable-means-no-lookup ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "n" "new")))))
      (magit-dash-review-test/pending-submit file 'review "c:n")
      (magit-dash-review--cr-pending-drop file '(review))
      (let ((data (magit-dash-review--read-json-file file)))
        (setf (map-elt (aref (map-elt data "comments") 0) "body") "edited")
        (magit-dash-review--write-json-file file data))
      (magit-dash-review-test/with-cr (list :submit #'magit-dash-review-test/cr-ok-submit)
        (should-error (magit-dash-review-submit file) :type 'user-error)
        (should (= 0 (magit-dash-review-test/cr-count 'reconcile)))))))

(ert-deftest magit-dash-review/b08-a-stamped-review-settles-without-a-lookup ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 nil (magit-dash-review-test/c "state" "approved" "head" "H1"))))
      (magit-dash-review-test/pending-submit file 'review)
      (magit-dash-review--cr-stamp file 'review nil nil '(:state "approved" :head "H1"))
      (magit-dash-review-test/with-cr (list :submit #'magit-dash-review-test/cr-ok-submit)
        (magit-dash-review-submit file)
        (should (= 0 (magit-dash-review-test/cr-count 'reconcile)))
        (should (= 0 (magit-dash-review-test/cr-count 'submit))))
      (should-not (map-elt (magit-dash-review--read-json-file file) "submit_pending")))))

(ert-deftest magit-dash-review/b03-a-review-stamped-for-another-state-is-not-the-recorded-one ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 nil (magit-dash-review-test/c "state" "approved" "head" "H1"))))
      (magit-dash-review-test/pending-submit file 'review)
      (let ((data (magit-dash-review--read-json-file file)))
        (setf (map-elt data "verdict")
              (magit-dash-review-test/c "state" "changes" "head" "H1" "submitted_at" "2026-01-01T00:00:00Z"
                                        "submitted_state" "changes" "submitted_head" "H1"))
        (magit-dash-review--write-json-file file data))
      (magit-dash-review-test/with-cr (list :submit #'magit-dash-review-test/cr-ok-submit)
        ;; No summary: only the recorded review being unresolved can refuse this.
        (should-error (magit-dash-review-submit file) :type 'user-error)
        (should (= 0 (magit-dash-review-test/cr-count 'submit)))))))

(ert-deftest magit-dash-review/b22-a-stamped-review-still-refuses-a-different-summary ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 nil (magit-dash-review-test/c "state" "approved" "head" "H1"))))
      (magit-dash-review-test/pending-submit file 'review)
      (magit-dash-review--cr-stamp file 'review nil nil '(:state "approved" :head "H1"))
      (magit-dash-review-test/with-cr (list :submit #'magit-dash-review-test/cr-ok-submit)
        (should-error (magit-dash-review-submit file "another summary") :type 'user-error)
        (should (= 0 (magit-dash-review-test/cr-count 'submit)))))))

;;; Gaps found by mutation testing

(ert-deftest magit-dash-review/match-posted-review-id-parent-and-start-line ()
  (let ((recon '(:comments ((:id 1 :user "me" :commit-id "H1" :body "x" :path "a.el" :line 4
                                 :review-id 2 :in-reply-to-id 7)
                            (:id 3 :user "me" :commit-id "H1" :body "x" :path "a.el" :line 4
                                 :start-line 2)))))
    (should-not (magit-dash-review--cr-match-posted
                 'comment '(:body "x" :path "a.el" :line 4) (list :comments (list (car (plist-get recon :comments))))
                 "H1" "me" 9))
    (should (magit-dash-review--cr-match-posted
             'comment '(:body "x" :path "a.el" :line 4)
             (list :comments (list (car (plist-get recon :comments)))) "H1" "me" 2))
    (should-not (magit-dash-review--cr-match-posted
                 'reply '(:body "x" :reply-to-id 8) recon nil "me"))
    (should (magit-dash-review--cr-match-posted
             'reply '(:body "x" :reply-to-id 7) recon nil "me"))
    (should-not (magit-dash-review--cr-match-posted
                 'comment '(:body "x" :path "a.el" :line 4 :start-line 3)
                 (list :comments (list (cadr (plist-get recon :comments)))) "H1" "me"))
    (should (magit-dash-review--cr-match-posted
             'comment '(:body "x" :path "a.el" :line 4 :start-line 2)
             (list :comments (list (cadr (plist-get recon :comments)))) "H1" "me"))))

(ert-deftest magit-dash-review/cr-submit-refuses-when-the-baseline-cannot-be-read ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "n" "new")))))
      (magit-dash-review-test/with-cr (list :submit #'magit-dash-review-test/cr-ok-submit)
        (cl-letf (((symbol-function 'magit-dash-review--cr-snapshot) (lambda (&rest _) nil)))
          (let ((err (should-error (magit-dash-review-submit file) :type 'user-error)))
            (should (string-match-p "nothing was sent" (cadr err)))))
        (should (= 0 (magit-dash-review-test/cr-count 'submit))))
      (should-not (map-elt (magit-dash-review--read-json-file file) "submit_pending")))))

(ert-deftest magit-dash-review/definite-failures-leave-the-pending-record ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "n" "new")))))
      (magit-dash-review-test/with-cr
          (list :submit (lambda (_keys _on-item)
                          (list :sent nil :failed (list '(:id review :error "422")
                                                        '(:id "c:n" :error "422")))))
        (should-error (magit-dash-review-submit file)))
      (should-not (map-elt (magit-dash-review--read-json-file file) "submit_pending")))))

(ert-deftest magit-dash-review/cr-reconcile-reports-a-twin-by-rest-or-node-id ()
  (seq-do
   (lambda (imported)
     (cl-letf (((symbol-function 'code-review-api-reconcile)
                (lambda (&rest _)
                  '(:reviews nil
                    :comments ((:id 41 :node-id "N41" :user "me" :commit-id "H1" :body "x"
                                    :path "a.el" :line 4))))))
       (let ((found (magit-dash-review--cr-reconcile
                     '("c:a") (list (cons "c:a" (cons 'comment '(:id "c:a" :body "x" :path "a.el" :line 4))))
                     "H1" "me" "o/r" 1 magit-dash-review-test/none (list :imported imported))))
         (should (equal '("c:a") (plist-get found :twin)))
         (should-not (plist-get found :matched)))))
   '((41) ("N41"))))

(ert-deftest magit-dash-review/pending-write-and-snapshot-direct ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file nil)))
      (magit-dash-review--cr-pending-write
       file (list (cons "c:n" (cons 'comment '(:id "c:n" :body "b" :path "n.el" :line 2))))
       '(:reviews (1) :comments (2)) "H1")
      (let ((record (map-elt (magit-dash-review--read-json-file file) "submit_pending")))
        (should (equal "H1" (map-elt record "head")))
        (should (equal [1] (map-elt (map-elt record "baseline") "reviews")))
        (let ((entry (aref (map-elt record "items") 0)))
          (should (equal "c:n" (map-elt entry "id")))
          (should (equal "b" (map-elt entry "body")))
          (should (equal (secure-hash 'sha1 "b") (map-elt entry "hash")))
          (should (= 2 (map-elt entry "line")))))
      (cl-letf (((symbol-function 'code-review-api-reconcile)
                 (lambda (&rest _) '(:reviews ((:id 1)) :comments ((:id 2) (:id 3))))))
        (should (equal '(:reviews (1) :comments (2 3))
                       (magit-dash-review--cr-snapshot "o/r" 1 "me"))))
      (cl-letf (((symbol-function 'code-review-api-reconcile)
                 (lambda (&rest _) '(:errors ("x")))))
        (should-not (magit-dash-review--cr-snapshot "o/r" 1 "me"))))))

;;; Published sent items survive a sync

(defun magit-dash-review-test/remote-thread (thread-id comments)
  "A transcoded remote thread; COMMENTS is a list of (NODE DB BODY)."
  (magit-dash-review--transcode-thread
   (list :id thread-id :path "n.el" :line 2
         :comments (seq-map (lambda (c)
                              (list :id (nth 0 c) :database_id (nth 1 c) :body (nth 2 c)
                                    :author "me"))
                            comments))
   nil nil nil))

(ert-deftest magit-dash-review/cr-stamped-items-merge-once-and-never-reupsert ()
  "After --cr-stamp a sync matches the thread and reply: one of each, no drafts."
  (seq-do
   (lambda (with-node)
     (magit-dash-review-test/with-temp-dir
       (let* ((existing (magit-dash-review-test/gh-thread "a" "reply text"))
              (file (magit-dash-review-test/submit-file
                     (list existing (magit-dash-review-test/cr-comment "n" "new thread"))))
              (writes nil))
         (magit-dash-review--cr-stamp file "c:n" 101 (and with-node "N101"))
         (magit-dash-review--cr-stamp file "r:r-reply text" 901 (and with-node "N901"))
         (let* ((remote (list (magit-dash-review-test/remote-thread
                               "T-a" '(("C-a" 900 "a") ("N901" 901 "reply text")))
                              (magit-dash-review-test/remote-thread
                               "T-n" '(("N101" 101 "new thread")))))
                (merged (magit-dash-review--merge-json
                         (magit-dash-review--read-json-file file)
                         '(:repo "owner/repo" :number 99) remote))
                (comments (append (map-elt merged "comments") nil))
                (thread-a (seq-find (lambda (c) (equal (map-elt c "gh_thread_id") "T-a")) comments))
                (thread-n (seq-find (lambda (c) (equal (map-elt c "gh_thread_id") "T-n")) comments)))
           (should (= 2 (length comments)))
           (should thread-n)
           (should (equal "n" (map-elt thread-n "id")))
           (should (= 1 (length (map-elt thread-a "replies"))))
           (should (= 901 (map-elt (aref (map-elt thread-a "replies") 0) "gh_database_id")))
           (should (equal '() (magit-dash-review-json--reply-content thread-a)))
           (cl-letf (((symbol-function 'code-review-api-comments) (lambda (&rest _) nil))
                     ((symbol-function 'code-review-api-upsert-comment)
                      (lambda (_repo _number plist &rest _)
                        (push plist writes)
                        nil)))
             (let* ((magit-dash-review-json-source-function (lambda (_) merged))
                    (magit-dash-review-json-code-review-function
                     #'magit-dash-review-json-code-review-default)
                    (exported (magit-dash-review-json-export '("owner/repo" . 99))))
               (should (equal "sent" (map-elt (seq-find (lambda (c) (equal (map-elt c "id") "n"))
                                                        (append (map-elt exported "comments") nil))
                                              "status")))
               (should-not (seq-some (lambda (c) (map-elt c "cr_id"))
                                     (append (map-elt exported "comments") nil)))
               (magit-dash-review-json-import exported)
               (should-not writes)))))))
   '(t nil)))

;;; Viewed sync

(defun magit-dash-review-test/viewed-response (spec)
  "JSON the fake gh answers the viewed-state query with, from SPEC."
  (let ((nodes (vconcat
                (seq-map (lambda (s)
                           `((path . ,(car s)) (viewerViewedState . ,(cdr s))))
                         (plist-get spec :states)))))
    (json-serialize
     `((data . ((repository . ((pullRequest
                                . ((id . "PR_1")
                                   (headRefOid . ,(plist-get spec :head))
                                   (files . ((pageInfo . ((hasNextPage . ,(cond ((plist-get spec :truncated) t) (t :false)))))
                                              (nodes . ,nodes)))))))))))))

(defmacro magit-dash-review-test/with-viewed (spec &rest body)
  "Run BODY with the viewed seam and the async gh runner faked.
SPEC is a plist: :api is the seam `viewed' table, :states the GitHub
alist of path to state, :head GitHub's head.  Seam and gh calls are logged."
  (declare (indent 1))
  `(let ((magit-dash-review-test/cr-calls nil)
         (spec ,spec))
     (cl-letf (((symbol-function 'magit-dash-gh--call-with-account)
                (lambda (account fn)
                  (when account
                    (push (list 'account account) magit-dash-review-test/cr-calls))
                  (funcall fn)))
               ((symbol-function 'magit-dash-gh--run-process)
                (lambda (args _dir on-success &optional on-error &rest _)
                  (push (list 'gh args) magit-dash-review-test/cr-calls)
                  (cond
                   ((seq-some (lambda (a) (string-match-p "files(first" a)) args)
                    (funcall on-success (magit-dash-review-test/viewed-response spec)))
                   ((seq-some (lambda (a) (member a (seq-map (lambda (p) (concat "path=" p))
                                                             (plist-get spec :fail-paths))))
                              args)
                    (funcall on-error "boom" 1))
                   (t
                    (funcall on-success "{}"))))))
       (let ((magit-dash-review-json-code-review-function
              (lambda (op key &optional arg)
                (push (list op key arg) magit-dash-review-test/cr-calls)
                (pcase op
                  ('viewed (plist-get spec :api))
                  (_ nil)))))
         ,@body))))

(defun magit-dash-review-test/viewed-table (&rest kv)
  "A table of the path/blob pairs KV."
  (apply #'magit-dash-review-test/c kv))

(defun magit-dash-review-test/gh-mutations ()
  "The (MUTATION . PATH) pairs of the fake gh mutations so far, oldest first."
  (nreverse
   (seq-keep (lambda (call)
               (when (eq (car call) 'gh)
                 (let* ((args (cadr call))
                        (q (seq-find (lambda (a) (string-prefix-p "query=mutation" a)) args))
                        (path (seq-find (lambda (a) (string-prefix-p "path=" a)) args)))
                   (when q
                     (cons (cond
                            ((string-match-p "unmarkFileAsViewed" q) 'unmark)
                            (t 'mark))
                           (substring path 5))))))
             magit-dash-review-test/cr-calls)))

(ert-deftest magit-dash-review/viewed-sync-mirrors-agreeing-blobs-only ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file nil)))
      (let* ((data (magit-dash-review--read-json-file file))
             (synced (magit-dash-review-test/viewed-table "d.el" "ddddddd111")))
        (setf (map-elt data "viewed")
              (magit-dash-review-test/viewed-table
               "a.el" "abcdef1234" "b.el" "1111111aaa" "c.el" "ccccccc999"))
        (setf (map-elt (map-elt data "magit_dash") "viewed_synced") synced)
        (magit-dash-review--write-json-file file data))
      (magit-dash-review-test/with-viewed
          (list :head "H1"
                :states '(("a.el" . "UNVIEWED") ("b.el" . "VIEWED") ("c.el" . "UNVIEWED")
                          ("d.el" . "VIEWED") ("e.el" . "VIEWED"))
                :api (magit-dash-review-test/c
                      "a.el" (magit-dash-review-test/c "blob" "abcdef1" "viewed" :false)
                      "b.el" (magit-dash-review-test/c "blob" "9999999" "viewed" t)))
        (magit-dash-review--sync-viewed file '("owner/repo" . 99) default-directory nil)
        (let ((set (seq-find (lambda (call) (eq (car call) 'set-viewed))
                             magit-dash-review-test/cr-calls)))
          (should set)
          (should (equal '("a.el") (map-keys (nth 2 set))))
          (should (equal "abcdef1234" (map-elt (nth 2 set) "a.el"))))
        ;; a.el marks.  d.el was mirrored before and is no longer viewed locally, so
        ;; it is unmarked.  b.el (stale blob), e.el (viewed on GitHub only) and c.el
        ;; are left alone.
        (should (equal '((mark . "a.el") (unmark . "d.el"))
                       (magit-dash-review-test/gh-mutations)))
        (should (equal '("a.el")
                       (map-keys (map-elt (map-elt (magit-dash-review--read-json-file file) "magit_dash")
                                          "viewed_synced"))))
        (seq-do (lambda (call)
                  (when (and (eq (car call) 'gh)
                             (seq-some (lambda (a) (string-prefix-p "path=" a)) (cadr call)))
                    (should-not (member "-F" (cadr call)))))
                magit-dash-review-test/cr-calls)))))

(ert-deftest magit-dash-review/viewed-sync-needs-a-known-head-and-a-small-pr ()
  (seq-do
   (lambda (case)
     (magit-dash-review-test/with-temp-dir
       (let ((file (magit-dash-review-test/submit-file nil nil (car case))))
         (let ((data (magit-dash-review--read-json-file file)))
           (setf (map-elt data "viewed") (magit-dash-review-test/viewed-table "a.el" "abcdef1234"))
           (when (null (car case))
             (map-delete (map-elt data "magit_dash") "head_oid"))
           (magit-dash-review--write-json-file file data))
         (magit-dash-review-test/with-viewed
             (list :head "H1" :truncated (cdr case) :states '(("a.el" . "UNVIEWED"))
                   :api (magit-dash-review-test/c
                         "a.el" (magit-dash-review-test/c "blob" "abcdef1" "viewed" :false)))
           (magit-dash-review--sync-viewed file '("owner/repo" . 99) default-directory nil)
           (should-not (magit-dash-review-test/gh-mutations))))))
   '((nil . nil) ("H1" . t))))

(ert-deftest magit-dash-review/viewed-sync-skips-github-when-head-moved ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file nil)))
      (let ((data (magit-dash-review--read-json-file file)))
        (setf (map-elt data "viewed") (magit-dash-review-test/viewed-table "a.el" "abcdef1234"))
        (magit-dash-review--write-json-file file data))
      (magit-dash-review-test/with-viewed
          (list :head "H2" :states '(("a.el" . "UNVIEWED"))
                :api (magit-dash-review-test/c
                      "a.el" (magit-dash-review-test/c "blob" "abcdef1" "viewed" :false)))
        (magit-dash-review--sync-viewed file '("owner/repo" . 99) default-directory nil)
        (should-not (magit-dash-review-test/gh-mutations))))))

(ert-deftest magit-dash-review/viewed-sync-failure-never-blocks-submit ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file
                 (list (magit-dash-review-test/cr-comment "n" "new")))))
      (magit-dash-review-test/with-cr (list :submit #'magit-dash-review-test/cr-ok-submit)
        (let ((magit-dash-review-sync-viewed t))
          (cl-letf (((symbol-function 'magit-dash-review--sync-viewed)
                     (lambda (&rest _) (error "gh exploded"))))
            (should (equal file (magit-dash-review-submit file)))))))))

(ert-deftest magit-dash-review/viewed-import-only-where-blobs-agree ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file nil)))
      (magit-dash-review-test/with-viewed
          (list :head "H1"
                :states '(("a.el" . "VIEWED") ("b.el" . "VIEWED") ("c.el" . "UNVIEWED"))
                :api (magit-dash-review-test/c
                      "a.el" (magit-dash-review-test/c "blob" "abcdef1" "viewed" :false)
                      "b.el" (magit-dash-review-test/c "blob" "9999999" "viewed" :false)
                      "c.el" (magit-dash-review-test/c "blob" "ccccccc" "viewed" :false)))
        (cl-letf (((symbol-function 'magit-dash-review-json-resolve-blob)
                   (lambda (_dir _rev path)
                     (pcase path
                       ("a.el" "abcdef1234567")
                       (_ "0000000000")))))
          (magit-dash-review--import-github-viewed file "owner/repo" 99 default-directory nil))
        (let ((viewed (map-elt (magit-dash-review--read-json-file file) "viewed")))
          (should (equal "abcdef1234567" (map-elt viewed "a.el")))
          (should-not (map-elt viewed "b.el"))
          (should-not (map-elt viewed "c.el")))
        (should-not (magit-dash-review-test/gh-mutations))))))

(ert-deftest magit-dash-review/viewed-import-keeps-a-local-unview ()
  (magit-dash-review-test/with-temp-dir
    (let ((file (magit-dash-review-test/submit-file nil)))
      (let ((data (magit-dash-review--read-json-file file)))
        (setf (map-elt (map-elt data "magit_dash") "viewed_synced")
              (magit-dash-review-test/viewed-table "a.el" "abcdef1234567"))
        (magit-dash-review--write-json-file file data))
      (magit-dash-review-test/with-viewed
          (list :head "H1" :states '(("a.el" . "VIEWED") ("b.el" . "VIEWED"))
                :api (magit-dash-review-test/c
                      "a.el" (magit-dash-review-test/c "blob" "abcdef1" "viewed" :false)
                      "b.el" (magit-dash-review-test/c "blob" "bbbbbbb" "viewed" :false)))
        (cl-letf (((symbol-function 'magit-dash-review-json-resolve-blob)
                   (lambda (_dir _rev path)
                     (pcase path
                       ("a.el" "abcdef1234567")
                       (_ "bbbbbbb999")))))
          (magit-dash-review--import-github-viewed file "owner/repo" 99 default-directory nil))
        (let ((viewed (map-elt (magit-dash-review--read-json-file file) "viewed")))
          (should-not (map-elt viewed "a.el"))
          (should (equal "bbbbbbb999" (map-elt viewed "b.el"))))))))

(ert-deftest magit-dash-review/viewed-import-needs-the-code-review-path ()
  (let ((magit-dash-review-submit-via-code-review nil)
        (called nil))
    (cl-letf (((symbol-function 'magit-dash-review--import-github-viewed)
               (lambda (&rest _) (setq called t)))
              ((symbol-function 'magit-dash-gh--repo-account)
               (lambda (&rest _) '(:user "me" :host "github.com"))))
      (magit-dash-review--maybe-import-viewed "/tmp/x" "o/r" 1 "/tmp")
      (should-not called)
      (let ((magit-dash-review-submit-via-code-review t))
        (magit-dash-review--maybe-import-viewed "/tmp/x" "o/r" 1 "/tmp")
        (should called)))))

(provide 'test-magit-dash-review)
;;; test-magit-dash-review.el ends here
