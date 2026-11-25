;;; test-magit-dash-review-json.el --- Tests for the code-review JSON adapter -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Export, import and merge through `magit-dash-review-json', with
;; code-review replaced by an in-memory stub behind
;; `magit-dash-review-json-code-review-function'.  No network, no closql.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'test-helper)
(require 'magit-dash-review-json)
(require 'magit-dash-review)

(defconst magit-dash-review-json-test/fixture-dir
  (expand-file-name "fixtures" (file-name-directory (or load-file-name buffer-file-name))))

(defvar magit-dash-review-json-test/store nil "Stub code-review comments.")
(defvar magit-dash-review-json-test/source nil "Stub review file table.")
(defvar magit-dash-review-json-test/fail nil "When non-nil, writes signal.")
(defvar magit-dash-review-json-test/next-id 1000)
(defvar magit-dash-review-json-test/verdict nil "Stub stored verdict.")
(defvar magit-dash-review-json-test/verdict-writes 0 "Count of verdict writes.")

(defun magit-dash-review-json-test/counts (result)
  "Return the count keys of import RESULT."
  (list :added (plist-get result :added)
        :updated (plist-get result :updated)
        :skipped (plist-get result :skipped)))

(defun magit-dash-review-json-test/comment (cr-id body)
  "A minimal comment with CR-ID (may be nil) and BODY."
  (let ((c (make-hash-table :test #'equal)))
    (setf (map-elt c "id") "x1")
    (setf (map-elt c "path") "a.el")
    (setf (map-elt c "lines") [3 3])
    (setf (map-elt c "body") body)
    (when cr-id
      (setf (map-elt c "cr_id") cr-id))
    c))

(defun magit-dash-review-json-test/table (&rest comments)
  "A table for owner/repo#42 holding COMMENTS."
  (let ((table (make-hash-table :test #'equal))
        (md (make-hash-table :test #'equal)))
    (setf (map-elt md "repo") "owner/repo")
    (setf (map-elt md "number") 42)
    (setf (map-elt table "magit_dash") md)
    (setf (map-elt table "comments") (vconcat comments))
    table))

(defun magit-dash-review-json-test/stub (op _pr-key &optional arg)
  "In-memory code-review for OP and ARG."
  (pcase op
    ('comments magit-dash-review-json-test/store)
    ('pullreq nil)
    ('verdict
     (when magit-dash-review-json-test/fail
       (error "network down"))
     (cond
      (arg
       (setq magit-dash-review-json-test/verdict-writes (1+ magit-dash-review-json-test/verdict-writes))
       (setq magit-dash-review-json-test/verdict (magit-dash-review-json-deep-copy arg)))
      (t magit-dash-review-json-test/verdict)))
    ('write
     (when magit-dash-review-json-test/fail
       (error "network down"))
     (let ((copy (magit-dash-review-json-deep-copy arg)))
       (unless (map-elt copy "cr_id")
         (setf (map-elt copy "cr_id") (setq magit-dash-review-json-test/next-id (1+ magit-dash-review-json-test/next-id))))
       (setf magit-dash-review-json-test/store
             (append (seq-remove (lambda (c) (equal (map-elt c "cr_id") (map-elt copy "cr_id")))
                                 magit-dash-review-json-test/store)
                     (list copy)))
       copy))))

(defmacro magit-dash-review-json-test/with-stub (&rest body)
  "Run BODY with the stubbed code-review and an empty store."
  (declare (indent 0))
  `(let ((magit-dash-review-json-test/store nil)
         (magit-dash-review-json-test/source nil)
         (magit-dash-review-json-test/fail nil)
         (magit-dash-review-json-test/verdict nil)
         (magit-dash-review-json-test/verdict-writes 0)
         (magit-dash-review-json-code-review-function #'magit-dash-review-json-test/stub)
         (magit-dash-review-json-source-function (lambda (_key) magit-dash-review-json-test/source)))
     ,@body))

(defun magit-dash-review-json-test/read (name)
  "Read fixture NAME."
  (magit-dash-review-json-read-file (expand-file-name name magit-dash-review-json-test/fixture-dir)))

(defun magit-dash-review-json-test/plain (data)
  "Return DATA as sorted alists and lists for `equal'."
  (cond
   ((hash-table-p data)
    (thread-last (map-pairs data)
                 (seq-map (lambda (pair)
                            (cons (car pair) (magit-dash-review-json-test/plain (cdr pair)))))
                 (seq-sort-by #'car #'string<)))
   ((vectorp data) (seq-map #'magit-dash-review-json-test/plain data))
   (t data)))

(defun magit-dash-review-json-test/full-table ()
  "A table with threads, a reply, resolved state and drafts, all with cr_id."
  (let ((table (magit-dash-review-json-test/read "review-remote-threads.json"))
        (draft (magit-dash-review-json-test/read "review-draft.json")))
    (setf (map-elt table "comments")
          (vconcat (map-elt table "comments") (map-elt draft "comments")))
    (setf (map-elt table "verdict") (map-elt draft "verdict"))
    table))

(ert-deftest magit-dash-review-json/round-trip-export-import-export ()
  (magit-dash-review-json-test/with-stub
    (setq magit-dash-review-json-test/source (magit-dash-review-json-test/full-table))
    (let* ((key '("owner/repo" . 42))
           (first (magit-dash-review-json-export key))
           (counts (magit-dash-review-json-import first))
           (second (magit-dash-review-json-export key)))
      (should (equal (magit-dash-review-json-test/counts counts) '(:added 4 :updated 0 :skipped 0)))
      (should (equal (magit-dash-review-json-test/plain first)
                     (magit-dash-review-json-test/plain second)))
      (should (equal (magit-dash-review-json-test/plain first)
                     (magit-dash-review-json-test/plain magit-dash-review-json-test/source)))
      (should (equal (seq-map (lambda (c) (map-elt c "status")) (map-elt second "comments"))
                     '("sent" "sent" "draft" "pending"))))))

(ert-deftest magit-dash-review-json/import-is-idempotent ()
  (magit-dash-review-json-test/with-stub
    (let ((table (magit-dash-review-json-test/full-table)))
      (should (equal (magit-dash-review-json-test/counts (magit-dash-review-json-import table)) '(:added 4 :updated 0 :skipped 0)))
      (should (equal (magit-dash-review-json-test/counts (magit-dash-review-json-import table)) '(:added 0 :updated 0 :skipped 4)))
      (should (= 4 (length magit-dash-review-json-test/store))))))

(ert-deftest magit-dash-review-json/import-updates-edited-comment ()
  (magit-dash-review-json-test/with-stub
    (let ((table (magit-dash-review-json-test/full-table)))
      (magit-dash-review-json-import table)
      (setf (map-elt (aref (map-elt table "comments") 2) "body") "Edited draft.")
      (should (equal (magit-dash-review-json-test/counts (magit-dash-review-json-import table)) '(:added 0 :updated 1 :skipped 3)))
      (should (= 4 (length magit-dash-review-json-test/store)))
      (should (equal "Edited draft."
                     (map-elt (seq-find (lambda (c) (equal (map-elt c "cr_id") 7))
                                        magit-dash-review-json-test/store)
                              "body"))))))

(ert-deftest magit-dash-review-json/transport-failure-leaves-drafts ()
  (magit-dash-review-json-test/with-stub
    (let* ((table (magit-dash-review-json-test/full-table))
           (before (magit-dash-review-json-test/plain table))
           (magit-dash-review-json-test/fail t))
      (let ((result (magit-dash-review-json-import table)))
        (should (equal (magit-dash-review-json-test/counts result) '(:added 0 :updated 0 :skipped 4)))
        (should (= 1 (plist-get result :verdict-failed))))
      (should (equal (magit-dash-review-json-test/plain table) before))
      (should-not magit-dash-review-json-test/store)
      (setq magit-dash-review-json-test/source table)
      (should (equal (seq-map (lambda (c) (map-elt c "status"))
                              (map-elt (magit-dash-review-json-export '("owner/repo" . 42)) "comments"))
                     '("sent" "sent" "draft" "pending"))))))

(ert-deftest magit-dash-review-json/remote-thread-added-between-imports-appears-once ()
  (magit-dash-review-json-test/with-stub
    (setq magit-dash-review-json-test/source (magit-dash-review-json-test/full-table))
    (let ((key '("owner/repo" . 42))
          (late (make-hash-table :test #'equal)))
      (magit-dash-review-json-import (magit-dash-review-json-export key))
      (setf (map-elt late "cr_id") 500)
      (setf (map-elt late "remote_id") "PRRC_late")
      (setf (map-elt late "status") "sent")
      (setf (map-elt late "path") "src/z.ts")
      (setf (map-elt late "lines") [4 4])
      (setf (map-elt late "author") "octocat")
      (setf (map-elt late "at") "2026-11-22T10:00:00+13:00")
      (setf (map-elt late "body") "Late remote thread.")
      (setq magit-dash-review-json-test/store
            (append magit-dash-review-json-test/store (list late)))
      (let ((second (magit-dash-review-json-export key)))
        (should (= 5 (length (map-elt second "comments"))))
        (should (equal (map-elt (aref (map-elt second "comments") 4) "origin") "code-review"))
        ;; Persist the second export as the review file; a third export adds nothing.
        (setq magit-dash-review-json-test/source second)
        (magit-dash-review-json-import second)
        (let ((third (magit-dash-review-json-export key)))
          (should (= 5 (length (map-elt third "comments"))))
          (should (= 1 (seq-count (lambda (c) (equal (map-elt c "remote_id") "PRRC_late"))
                                  (map-elt third "comments")))))))))

(ert-deftest magit-dash-review-json/merge-remote-thread-added-between-syncs-appears-once ()
  (let* ((meta '(:repo "owner/repo" :number 42 :worktree "/tmp/wt"))
         (t1 (let ((c (make-hash-table :test #'equal)))
               (setf (map-elt c "id") "a1")
               (setf (map-elt c "gh_thread_id") "T1")
               (setf (map-elt c "body") "one")
               c))
         (t2 (let ((c (make-hash-table :test #'equal)))
               (setf (map-elt c "id") "a2")
               (setf (map-elt c "gh_thread_id") "T2")
               (setf (map-elt c "body") "two")
               c))
         (first (magit-dash-review-json-merge nil meta (list t1)))
         (second (magit-dash-review-json-merge first meta (list t1 t2)))
         (third (magit-dash-review-json-merge second meta (list t1 t2))))
    (should (= 1 (length (map-elt first "comments"))))
    (should (equal '("T1" "T2") (seq-map (lambda (c) (map-elt c "gh_thread_id"))
                                         (map-elt third "comments"))))))

(ert-deftest magit-dash-review-json/idless-draft-edit-updates-after-apply-assigned ()
  (magit-dash-review-json-test/with-stub
    (let* ((table (magit-dash-review-json-test/table (magit-dash-review-json-test/comment nil "old")))
           (first (magit-dash-review-json-import table)))
      (should (equal (magit-dash-review-json-test/counts first) '(:added 1 :updated 0 :skipped 0)))
      (should (equal (plist-get first :assigned) '((0 . 1001))))
      (should-not (map-elt (aref (map-elt table "comments") 0) "cr_id"))
      (magit-dash-review-json-apply-assigned table first)
      (setf (map-elt (aref (map-elt table "comments") 0) "body") "new")
      (should (equal (magit-dash-review-json-test/counts (magit-dash-review-json-import table))
                     '(:added 0 :updated 1 :skipped 0)))
      (should (equal (magit-dash-review-json-test/counts (magit-dash-review-json-import table))
                     '(:added 0 :updated 0 :skipped 1)))
      (should (= 1 (length magit-dash-review-json-test/store))))))

(ert-deftest magit-dash-review-json/import-compares-replies-by-content ()
  (magit-dash-review-json-test/with-stub
    (let* ((table (magit-dash-review-json-test/table (magit-dash-review-json-test/comment 5 "b")))
           (reply (make-hash-table :test #'equal)))
      (setf (map-elt reply "author") "me")
      (setf (map-elt reply "body") "one")
      (setf (map-elt (aref (map-elt table "comments") 0) "replies") (vector reply))
      (magit-dash-review-json-import table)
      (setf (map-elt reply "body") "two")
      (should (equal (magit-dash-review-json-test/counts (magit-dash-review-json-import table))
                     '(:added 0 :updated 1 :skipped 0))))))

(ert-deftest magit-dash-review-json/import-ignores-status-difference ()
  (magit-dash-review-json-test/with-stub
    (let ((table (magit-dash-review-json-test/table (magit-dash-review-json-test/comment 5 "b"))))
      (magit-dash-review-json-import table)
      (setf (map-elt (car magit-dash-review-json-test/store) "status") "pending")
      (setf (map-elt (aref (map-elt table "comments") 0) "status") "draft")
      (should (equal (magit-dash-review-json-test/counts (magit-dash-review-json-import table))
                     '(:added 0 :updated 0 :skipped 1))))))

(ert-deftest magit-dash-review-json/verdict-written-only-when-changed ()
  (magit-dash-review-json-test/with-stub
    (let ((table (magit-dash-review-json-test/full-table)))
      (magit-dash-review-json-import table)
      (magit-dash-review-json-import table)
      (should (= 1 magit-dash-review-json-test/verdict-writes))
      (setf (map-elt (map-elt table "verdict") "state") "changes")
      (magit-dash-review-json-import table)
      (should (= 2 magit-dash-review-json-test/verdict-writes)))))

(ert-deftest magit-dash-review-json/match-thread-id-outranks-content-hash ()
  (let* ((a (magit-dash-review-json-test/comment nil "old"))
         (b (magit-dash-review-json-test/comment nil "new"))
         (remote (magit-dash-review-json-test/comment nil "new")))
    (setf (map-elt a "gh_thread_id") "T1")
    (setf (map-elt a "id") "a")
    (setf (map-elt b "id") "b")
    (setf (map-elt remote "gh_thread_id") "T1")
    (setf (map-elt remote "origin") "github")
    (should (eq a (magit-dash-review-json-find-match remote (list b a))))
    (let ((merged (magit-dash-review-json-merge
                   (magit-dash-review-json-test/table b a) nil (list remote))))
      (should (= 1 (seq-count (lambda (c) (equal (map-elt c "gh_thread_id") "T1"))
                              (map-elt merged "comments"))))
      (should (equal '("a")
                     (seq-map (lambda (c) (map-elt c "id"))
                              (seq-filter (lambda (c) (equal (map-elt c "gh_thread_id") "T1"))
                                          (map-elt merged "comments")))))
      (should (= 2 (length (map-elt merged "comments")))))))

(ert-deftest magit-dash-review-json/merge-keeps-agent-comment-whole ()
  (let* ((agent (magit-dash-review-json-test/comment nil "same"))
         (remote (magit-dash-review-json-test/comment nil "same")))
    (setf (map-elt agent "origin") "agent")
    (setf (map-elt remote "origin") "github")
    (setf (map-elt remote "gh_thread_id") "T9")
    (let* ((merged (magit-dash-review-json-merge
                    (magit-dash-review-json-test/table agent) nil (list remote)))
           (comments (map-elt merged "comments")))
      (should (= 2 (length comments)))
      (should (equal '("agent" "github") (seq-map (lambda (c) (map-elt c "origin")) comments)))
      (should-not (map-elt (aref comments 0) "gh_thread_id")))))

(ert-deftest magit-dash-review-json/export-nil-source-fills-pr-key ()
  (magit-dash-review-json-test/with-stub
    (setq magit-dash-review-json-test/store
          (list (magit-dash-review-json-test/comment 3 "stored")))
    (let ((table (magit-dash-review-json-export '("owner/repo" . 42))))
      (should (equal '("owner/repo" . 42) (magit-dash-review-json-pr-key table)))
      (should (equal (magit-dash-review-json-test/counts (magit-dash-review-json-import table))
                     '(:added 0 :updated 0 :skipped 1))))))

(ert-deftest magit-dash-review-json/default-source-rejects-other-repo ()
  (require 'magit-dash-review)
  (require 'magit-dash-worktree)
  (let ((file (make-temp-file "mdrj" nil ".json")))
    (unwind-protect
        (cl-letf (((symbol-function 'magit-dash-worktree-main-dir) (lambda (&rest _) "/x"))
                  ((symbol-function 'magit-dash-review-file) (lambda (&rest _) file)))
          (magit-dash-review-json-write-file file (magit-dash-review-json-test/table))
          (should (magit-dash-review-json-default-source '("owner/repo" . 42)))
          (should-not (magit-dash-review-json-default-source '("other/repo" . 42))))
      (delete-file file))))

(ert-deftest magit-dash-review-json/merge-thread-clears-remote-owned-keys ()
  (let ((local (magit-dash-review-json-test/comment 4 "b"))
        (remote (magit-dash-review-json-test/comment nil "b")))
    (setf (map-elt local "outdated") t)
    (setf (map-elt local "blob") "old")
    (map-delete remote "lines")
    (let ((merged (magit-dash-review-json-merge-thread local remote)))
      (should-not (map-contains-key merged "outdated"))
      (should-not (map-contains-key merged "blob"))
      (should-not (map-contains-key merged "lines"))
      (should (equal 4 (map-elt merged "cr_id"))))))

(ert-deftest magit-dash-review-json/merge-does-not-alias-inputs ()
  (let* ((existing (magit-dash-review-json-test/table (magit-dash-review-json-test/comment 4 "b")))
         (remote (magit-dash-review-json-test/comment nil "other"))
         (reply (make-hash-table :test #'equal)))
    (setf (map-elt remote "path") "z.el")
    (setf (map-elt reply "body") "r")
    (setf (map-elt remote "replies") (vector reply))
    (setf (map-elt existing "viewed") (make-hash-table :test #'equal))
    (setf (map-elt existing "verdict") (make-hash-table :test #'equal))
    (setf (map-elt existing "code_review") (make-hash-table :test #'equal))
    (let ((before-existing (magit-dash-review-json-test/plain existing))
          (before-remote (magit-dash-review-json-test/plain remote))
          (merged (magit-dash-review-json-merge existing nil (list remote))))
      (setf (map-elt (map-elt merged "viewed") "f") "h")
      (setf (map-elt (map-elt merged "verdict") "state") "approved")
      (setf (map-elt (map-elt merged "code_review") "pullreq_id") "P")
      (setf (map-elt (aref (map-elt merged "comments") 0) "body") "mutated")
      (setf (map-elt (aref (map-elt (aref (map-elt merged "comments") 1) "replies") 0) "body") "mutated")
      (should (equal before-existing (magit-dash-review-json-test/plain existing)))
      (should (equal before-remote (magit-dash-review-json-test/plain remote))))))

(ert-deftest magit-dash-review-json/deep-copy-handles-lists-and-atoms ()
  (let* ((table (make-hash-table :test #'equal))
         (inner (list "a" (vector 1 :false) (cons "k" 2))))
    (setf (map-elt table "list") inner)
    (setf (map-elt table "flag") :false)
    (setf (map-elt table "on") t)
    (setf (map-elt table "none") nil)
    (setf (map-elt table "n") 1.5)
    (let ((copy (magit-dash-review-json-deep-copy table)))
      (should (equal inner (map-elt copy "list")))
      (should-not (eq inner (map-elt copy "list")))
      (should-not (eq (car inner) (car (map-elt copy "list"))))
      (should (eq :false (map-elt copy "flag")))
      (should (eq t (map-elt copy "on")))
      (should (= 1.5 (map-elt copy "n"))))))

(ert-deftest magit-dash-review-json/import-requires-pr-key ()
  (should-error (magit-dash-review-json-import (make-hash-table :test #'equal))
                :type 'user-error))

(defmacro magit-dash-review-json-test/with-temp-dir (&rest body)
  "Run BODY with DIR bound to a fresh temporary directory."
  (declare (indent 0))
  `(let ((dir (make-temp-file "mdrj-test-" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory dir t))))

(defun magit-dash-review-json-test/unicode-table ()
  "A table with non-ASCII body, author and path."
  (let ((c (magit-dash-review-json-test/comment nil "é日😀"))
        (table (magit-dash-review-json-test/table)))
    (setf (map-elt c "author") "Zoë")
    (setf (map-elt c "path") "src/日本.el")
    (setf (map-elt table "comments") (vector c))
    table))

(ert-deftest magit-dash-review-json/write-file-utf-8-round-trip ()
  (magit-dash-review-json-test/with-temp-dir
    (let ((file (expand-file-name "pr-1.json" dir)))
      (magit-dash-review-json-write-file file (magit-dash-review-json-test/unicode-table))
      (let ((c (aref (map-elt (magit-dash-review-json-read-file file) "comments") 0))
            (raw (with-temp-buffer
                   (set-buffer-multibyte nil)
                   (insert-file-contents-literally file)
                   (buffer-string))))
        (should (equal "é日😀" (map-elt c "body")))
        (should (equal "Zoë" (map-elt c "author")))
        (should (equal "src/日本.el" (map-elt c "path")))
        (should-not (string-match-p "\\\\[0-9][0-9][0-9]" raw))
        (should (string-match-p (regexp-quote (encode-coding-string "é日😀" 'utf-8)) raw))))))

(defun magit-dash-review-json-test/slurp (file)
  "Return the contents of FILE."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(ert-deftest magit-dash-review-json/write-file-failure-leaves-original-intact ()
  "A real failure (unwritable directory) leaves the original and no temp file."
  (skip-unless (not (zerop (user-uid))))
  (magit-dash-review-json-test/with-temp-dir
    (let ((file (expand-file-name "pr-1.json" dir)))
      (magit-dash-review-json-write-file file (magit-dash-review-json-test/table))
      (let ((before (magit-dash-review-json-test/slurp file)))
        (unwind-protect
            (progn
              (set-file-modes dir #o500)
              (should-error (magit-dash-review-json-write-file
                             file (magit-dash-review-json-test/unicode-table))))
          (set-file-modes dir #o700))
        (should (equal before (magit-dash-review-json-test/slurp file)))
        (should (equal '("pr-1.json") (directory-files dir nil "\\`[^.]")))))))

(ert-deftest magit-dash-review-json/write-file-rename-failure-cleans-temp ()
  (magit-dash-review-json-test/with-temp-dir
    (let ((file (expand-file-name "pr-1.json" dir)))
      (magit-dash-review-json-write-file file (magit-dash-review-json-test/table))
      (let ((before (magit-dash-review-json-test/slurp file)))
        (cl-letf (((symbol-function 'rename-file)
                   (lambda (&rest _) (error "disk full"))))
          (should-error (magit-dash-review-json-write-file
                         file (magit-dash-review-json-test/unicode-table))))
        (should (equal before (magit-dash-review-json-test/slurp file)))
        (should (equal '("pr-1.json" "pr-1.json.bak") (directory-files dir nil "\\`[^.]")))))))

(ert-deftest magit-dash-review-json/read-file-falls-back-to-backup ()
  (magit-dash-review-json-test/with-temp-dir
    (let ((file (expand-file-name "pr-1.json" dir)))
      (magit-dash-review-json-write-file file (magit-dash-review-json-test/table))
      (magit-dash-review-json-write-file file (magit-dash-review-json-test/unicode-table))
      (let ((good (magit-dash-review-json-test/plain (magit-dash-review-json-read-file file))))
        (should-not magit-dash-review-json-last-read-fallback)
        (magit-dash-review-json-write-file file (magit-dash-review-json-test/unicode-table))
        (with-temp-file file (insert "{\"truncat"))
        (let ((table (magit-dash-review-json-read-file file)))
          (should (equal good (magit-dash-review-json-test/plain table)))
          (should (equal (concat file ".bak") magit-dash-review-json-last-read-fallback))))
      (delete-file (concat file ".bak"))
      (should-error (magit-dash-review-json-read-file file)))))

(ert-deftest magit-dash-review-json/read-file-errors-when-backup-is-corrupt-too ()
  (magit-dash-review-json-test/with-temp-dir
    (let ((file (expand-file-name "pr-1.json" dir)))
      (with-temp-file file (insert "{\"truncat"))
      (with-temp-file (concat file ".bak") (insert "{\"also bad"))
      (should-error (magit-dash-review-json-read-file file)))))

(ert-deftest magit-dash-review-json/backup-keeps-last-good-file ()
  "MED-3: a corrupt file is never copied over a good backup."
  (magit-dash-review-json-test/with-temp-dir
    (let ((file (expand-file-name "pr-1.json" dir)))
      (magit-dash-review-json-write-file file (magit-dash-review-json-test/table))
      (magit-dash-review-json-write-file file (magit-dash-review-json-test/unicode-table))
      (let ((good-bak (magit-dash-review-json-test/slurp (concat file ".bak"))))
        (with-temp-file file (insert "{\"truncat"))
        (magit-dash-review-json-write-file file (magit-dash-review-json-test/table))
        (should (equal good-bak (magit-dash-review-json-test/slurp (concat file ".bak"))))
        (should (hash-table-p (magit-dash-review-json-read-file file)))))))

(ert-deftest magit-dash-review-json/read-for-write-needs-confirmation-on-fallback ()
  (magit-dash-review-json-test/with-temp-dir
    (let ((file (expand-file-name "pr-1.json" dir)))
      (magit-dash-review-json-write-file file (magit-dash-review-json-test/table))
      (magit-dash-review-json-write-file file (magit-dash-review-json-test/table))
      (with-temp-file file (insert "{\"truncat"))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
        (should-error (magit-dash-review-json-read-for-write file) :type 'user-error))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (should (hash-table-p (magit-dash-review-json-read-for-write file)))))))

(ert-deftest magit-dash-review-json/write-file-keeps-symlink-and-modes ()
  (magit-dash-review-json-test/with-temp-dir
    (let* ((target (expand-file-name "real.json" dir))
           (link (expand-file-name "link.json" dir)))
      (magit-dash-review-json-write-file target (magit-dash-review-json-test/table))
      (set-file-modes target #o640)
      (make-symbolic-link target link)
      (magit-dash-review-json-write-file link (magit-dash-review-json-test/unicode-table))
      (should (file-symlink-p link))
      (should (= #o640 (file-modes target)))
      (should (equal "é日😀" (map-elt (aref (map-elt (magit-dash-review-json-read-file target) "comments") 0)
                                      "body")))
      (should-not (file-exists-p (concat link ".bak")))
      (should (file-exists-p (concat target ".bak")))
      (let ((fresh (expand-file-name "new.json" dir)))
        (magit-dash-review-json-write-file fresh (magit-dash-review-json-test/table))
        (should (= #o600 (file-modes fresh)))))))

(ert-deftest magit-dash-review-json/write-file-removes-only-stale-own-temp-files ()
  (magit-dash-review-json-test/with-temp-dir
    (let* ((file (expand-file-name "pr-1.json" dir))
           (old (concat file ".tmp-old"))
           (fresh (concat file ".tmp-fresh"))
           (other (expand-file-name "pr-2.json.tmp-old" dir)))
      (seq-do (lambda (f) (with-temp-file f (insert "x"))) (list old fresh other))
      (set-file-times old (time-subtract nil (* 3 24 3600)))
      (set-file-times other (time-subtract nil (* 3 24 3600)))
      (magit-dash-review-json-write-file file (magit-dash-review-json-test/table))
      (should-not (file-exists-p old))
      (should (file-exists-p fresh))
      (should (file-exists-p other)))))

(ert-deftest magit-dash-review-json/refresh-helper-skips-modified-buffer ()
  (magit-dash-review-json-test/with-temp-dir
    (let* ((file (expand-file-name "pr-1.json" dir))
           (buf (progn (magit-dash-review-json-write-file file (magit-dash-review-json-test/table))
                       (find-file-noselect file))))
      (unwind-protect
          (progn
            (should (magit-dash-review-json-refresh-visiting-buffer file))
            (with-current-buffer buf (insert " "))
            (should-not (magit-dash-review-json-refresh-visiting-buffer file))
            (with-current-buffer buf (should (buffer-modified-p))))
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (should (magit-dash-review-json-refresh-visiting-buffer (expand-file-name "none.json" dir))))))

(ert-deftest magit-dash-review-json/relocate-skips-refresh-of-modified-buffer ()
  (magit-dash-review-json-test/with-temp-dir
    (let ((default-directory (file-name-as-directory dir)))
      (call-process "git" nil nil nil "init" "-q" ".")
      (let* ((review-dir (expand-file-name ".git/review" dir))
             (file (expand-file-name "pr-1.json" review-dir))
             (table (magit-dash-review-json-test/table)))
        (setf (map-elt table "root") "/old/wt/")
        (magit-dash-review-json-write-file file table)
        (let ((buf (find-file-noselect file)))
          (unwind-protect
              (progn
                (with-current-buffer buf (goto-char (point-max)) (insert " "))
                (should (= 1 (magit-dash-review-json-relocate-worktree dir "/old/wt/" "/new/wt/")))
                (with-current-buffer buf (should (buffer-modified-p)))
                (should (equal "/new/wt/" (map-elt (magit-dash-review-json-read-file file) "root"))))
            (with-current-buffer buf (set-buffer-modified-p nil))
            (kill-buffer buf)))))))

(ert-deftest magit-dash-review-json/merge-ignores-other-repo-with-same-number ()
  (let* ((existing (magit-dash-review-json-test/table
                    (magit-dash-review-json-test/comment nil "repo A comment")))
         (remote (magit-dash-review-json-test/comment nil "repo B remote")))
    (setf (map-elt (map-elt existing "magit_dash") "repo") "a/proj")
    (setf (map-elt remote "path") "z.el")
    (let ((merged (magit-dash-review-json-merge
                   existing (list :repo "b/proj" :number 42) (list remote))))
      (should (equal "b/proj" (map-elt (map-elt merged "magit_dash") "repo")))
      (should (equal '("repo B remote")
                     (seq-map (lambda (c) (map-elt c "body")) (map-elt merged "comments")))))))

(ert-deftest magit-dash-review-json/merge-preserves-local-order-and-appends-new ()
  (let* ((a (magit-dash-review-json-test/comment nil "a"))
         (b (magit-dash-review-json-test/comment nil "b"))
         (new (magit-dash-review-json-test/comment nil "new"))
         (rb (magit-dash-review-json-test/comment nil "b")))
    (setf (map-elt a "origin") "local")
    (setf (map-elt a "lines") [1 1])
    (setf (map-elt b "gh_thread_id") "T")
    (setf (map-elt rb "gh_thread_id") "T")
    (setf (map-elt new "path") "n.el")
    (let ((merged (magit-dash-review-json-merge
                   (magit-dash-review-json-test/table a b) nil (list new rb))))
      (should (equal '("a" "b" "new")
                     (seq-map (lambda (c) (map-elt c "body")) (map-elt merged "comments")))))))

(ert-deftest magit-dash-review-json/content-hash-covers-all-lines-and-make-id-is-stable ()
  (let ((a (magit-dash-review-json-test/comment nil "x"))
        (b (magit-dash-review-json-test/comment nil "x")))
    (setf (map-elt a "lines") [3 4])
    (setf (map-elt b "lines") [3 9])
    (should-not (equal (magit-dash-review-json-content-hash a)
                       (magit-dash-review-json-content-hash b))))
  (should (equal (magit-dash-review-json-make-id "PRRT_abc")
                 (substring (secure-hash 'sha1 "PRRT_abc") 0 12))))

(ert-deftest magit-dash-review-json/merge-keeps-published-reply-missing-from-remote ()
  (let* ((local (magit-dash-review-json-test/comment nil "t"))
         (remote (magit-dash-review-json-test/comment nil "t"))
         (reply (make-hash-table :test #'equal)))
    (setf (map-elt local "gh_thread_id") "T")
    (setf (map-elt remote "gh_thread_id") "T")
    (setf (map-elt reply "gh_comment_id") "R1")
    (setf (map-elt reply "body") "published")
    (setf (map-elt local "replies") (vector reply))
    (setf (map-elt remote "replies") [])
    (let* ((merged (magit-dash-review-json-merge
                    (magit-dash-review-json-test/table local) nil (list remote)))
           (replies (map-elt (aref (map-elt merged "comments") 0) "replies")))
      (should (= 1 (length replies)))
      (should (eq t (map-elt (aref replies 0) "gh_missing"))))))

(ert-deftest magit-dash-review-json/transcode-without-created-at-omits-at ()
  (let ((c (magit-dash-review-json-transcode-thread
            '((id . "T") (path . "") (comments . ((nodes . (((id . "C") (body . "b")))))))
            nil nil nil)))
    (should-not (map-contains-key c "at"))))

(ert-deftest magit-dash-review-json/export-status-follows-seam-and-vectorizes ()
  (magit-dash-review-json-test/with-stub
    (let ((local (magit-dash-review-json-test/comment 7 "same"))
          (stored (magit-dash-review-json-test/comment 7 "same")))
      (setf (map-elt local "status") "draft")
      (setf (map-elt stored "status") "sent")
      (setf (map-elt stored "lines") (list 3 3))
      (setq magit-dash-review-json-test/store (list stored))
      (setq magit-dash-review-json-test/source (magit-dash-review-json-test/table local))
      (let ((table (magit-dash-review-json-export '("owner/repo" . 42))))
        (should (equal "sent" (map-elt (aref (map-elt table "comments") 0) "status"))))
      (setq magit-dash-review-json-test/source nil)
      (let ((c (aref (map-elt (magit-dash-review-json-export '("owner/repo" . 42)) "comments") 0)))
        (should (equal [3 3] (map-elt c "lines")))))))

(ert-deftest magit-dash-review-json/import-survives-seam-read-error ()
  (let ((magit-dash-review-json-code-review-function
         (lambda (op &rest _) (when (eq op 'comments) (error "boom")))))
    (let ((result (magit-dash-review-json-import
                   (magit-dash-review-json-test/table
                    (magit-dash-review-json-test/comment nil "x")))))
      (should (= 1 (plist-get result :skipped)))
      (should (= 0 (plist-get result :added))))))

(ert-deftest magit-dash-review-file/keyed-by-repo-with-legacy-fallback ()
  (magit-dash-review-json-test/with-temp-dir
    (let ((default-directory (file-name-as-directory dir)))
      (call-process "git" nil nil nil "init" "-q" ".")
      (let ((legacy (magit-dash-review-file dir 5))
            (a (magit-dash-review-file dir 5 "a/proj"))
            (b (magit-dash-review-file dir 5 "b/proj")))
        (should-not (equal a b))
        (should (equal a (magit-dash-review-file dir 5 "a/proj")))
        (let ((table (magit-dash-review-json-test/table)))
          (setf (map-elt table "magit_dash") (make-hash-table :test #'equal))
          (magit-dash-review-json-write-file legacy table))
        (should-not (equal legacy (magit-dash-review-file dir 5 "a/proj")))
        (let ((table (magit-dash-review-json-test/table)))
          (setf (map-elt (map-elt table "magit_dash") "repo") "a/proj")
          (magit-dash-review-json-write-file legacy table))
        (should (equal legacy (magit-dash-review-file dir 5 "a/proj")))
        (should (equal b (magit-dash-review-file dir 5 "b/proj")))))))

(ert-deftest magit-dash-review-json/merge-clears-resolve-intent-when-github-reopens ()
  (let* ((local (magit-dash-review-json-test/comment nil "t"))
         (remote (magit-dash-review-json-test/comment nil "t")))
    (setf (map-elt local "gh_thread_id") "T1")
    (setf (map-elt remote "gh_thread_id") "T1")
    (setf (map-elt local "gh_resolved") t)
    (setf (map-elt local "resolved") t)
    (setf (map-elt local "local_resolved") t)
    (setf (map-elt remote "gh_resolved") :false)
    (setf (map-elt remote "resolved") :false)
    (let ((merged (magit-dash-review-json-merge-thread local remote)))
      (should (eq t (map-elt merged "gh_reopened")))
      (should-not (map-elt merged "local_resolved"))
      (should (eq t (map-elt merged "resolved"))))
    (setf (map-elt remote "gh_resolved") t)
    (let ((merged (magit-dash-review-json-merge-thread
                   (let ((l (magit-dash-review-json-deep-copy local)))
                     (setf (map-elt l "gh_reopened") t)
                     l)
                   remote)))
      (should-not (map-elt merged "gh_reopened")))))

;;; In-memory fake of the code-review API

(defvar magit-dash-review-json-test/fake nil
  "Fake code-review state: plist :comments (list of plists) :verdict :viewed :analysis :worktree.")

(defun magit-dash-review-json-test/fake-id-p (id)
  "Return non-nil when ID is a valid draft id."
  (and (integerp id) (<= (ash 1 44) id) (< id (ash 1 53))))

(defmacro magit-dash-review-json-test/with-fake-api (&rest body)
  "Run BODY with `code-review-api-*' faked in memory and the default seam."
  (declare (indent 0))
  `(let ((magit-dash-review-json-test/fake (list :comments nil :verdict nil :viewed nil :analysis nil :worktree t))
         (magit-dash-review-json-code-review-function #'magit-dash-review-json-code-review-default)
         (magit-dash-review-json-source-function (lambda (_key) magit-dash-review-json-test/source))
         (magit-dash-review-json-test/source nil)
         (magit-dash-review-json-dry-run nil)
         (next 1))
     (cl-letf (((symbol-function 'code-review-api-comments)
                (lambda (_repo _number)
                  (copy-tree (plist-get magit-dash-review-json-test/fake :comments))))
               ((symbol-function 'code-review-api-upsert-comment)
                (lambda (_repo _number plist &rest keys)
                  (let* ((id (or (plist-get plist :id)
                                 (+ (ash 1 44) (setq next (1+ next)))))
                         (old (seq-find (lambda (c) (equal (plist-get c :id) id))
                                        (plist-get magit-dash-review-json-test/fake :comments)))
                         (parent (and (plist-get plist :reply-to)
                                      (seq-find (lambda (c) (equal (plist-get c :id) (plist-get plist :reply-to)))
                                                (plist-get magit-dash-review-json-test/fake :comments)))))
                    (unless (magit-dash-review-json-test/fake-id-p id)
                      (user-error "bad id"))
                    (when (and old (not (eq (plist-get old :status) 'draft)))
                      (user-error "sent comments are read-only"))
                    (when (and (plist-get plist :reply-to) (not parent))
                      (user-error "bad reply-to"))
                    (let ((new (list :id id :remote-id nil
                                     :path (or (plist-get plist :path) (plist-get parent :path))
                                     :line (plist-get plist :line) :lines (plist-get plist :lines)
                                     :side (plist-get plist :side) :body (plist-get plist :body)
                                     :author (or (plist-get plist :author) "local")
                                     :at "2026-01-01T00:00:00Z" :status 'draft
                                     :reply-to (plist-get plist :reply-to) :resolved nil)))
                      (unless (plist-get new :path)
                        (user-error "A comment needs a :path"))
                      (unless (plist-get keys :dry-run)
                        (plist-put magit-dash-review-json-test/fake :comments
                                   (append (seq-remove (lambda (c) (equal (plist-get c :id) id))
                                                       (plist-get magit-dash-review-json-test/fake :comments))
                                           (list new))))
                      new))))
               ((symbol-function 'code-review-api-verdict)
                (lambda (_repo _number) (plist-get magit-dash-review-json-test/fake :verdict)))
               ((symbol-function 'code-review-api-set-verdict)
                (lambda (_repo _number state &optional head &rest keys)
                  (unless (memq (if (stringp state) (intern state) state) '(approved changes comment nil))
                    (user-error "bad state"))
                  (let ((new (and state (list :state (if (stringp state) (intern state) state)
                                              :head-oid head :at "2026-01-01T00:00:00Z"))))
                    (unless (plist-get keys :dry-run)
                      (plist-put magit-dash-review-json-test/fake :verdict new))
                    new)))
               ((symbol-function 'code-review-api-viewed)
                (lambda (_repo _number) (copy-tree (plist-get magit-dash-review-json-test/fake :viewed))))
               ((symbol-function 'code-review-api-set-viewed)
                (lambda (_repo _number path blob viewed-p)
                  (let ((entry (assoc path (plist-get magit-dash-review-json-test/fake :viewed))))
                    (cond
                     ((null entry) nil)
                     ((and blob (not (string-prefix-p blob (plist-get (cdr entry) :blob)))) nil)
                     ((eq (plist-get (cdr entry) :viewed-p) (and viewed-p t)) :unchanged)
                     (t (plist-put (cdr entry) :viewed-p (and viewed-p t)) t)))))
               ((symbol-function 'code-review-api-analysis-available-p)
                (lambda (_repo _number) (plist-get magit-dash-review-json-test/fake :worktree)))
               ((symbol-function 'code-review-api-analysis)
                (lambda (_repo _number) (copy-tree (plist-get magit-dash-review-json-test/fake :analysis)))))
       ,@body)))

(defun magit-dash-review-json-test/fake-snapshot ()
  "Return a deep copy of the fake state for comparison."
  (copy-tree magit-dash-review-json-test/fake t))

(defun magit-dash-review-json-test/draft (id body &optional cr-id)
  "A local draft comment table with ID and BODY, and CR-ID when given."
  (let ((c (magit-dash-review-json-test/comment cr-id body)))
    (setf (map-elt c "id") id)
    (setf (map-elt c "origin") "agent")
    c))

(ert-deftest magit-dash-review-json/api-import-export-idempotent-with-cr-id ()
  "Export, import, export is identical including the derived cr_id."
  (magit-dash-review-json-test/with-fake-api
    (setq magit-dash-review-json-test/source
          (magit-dash-review-json-test/table
           (magit-dash-review-json-test/draft "d1" "one")
           (magit-dash-review-json-test/draft "d2" "two")))
    (let* ((key '("owner/repo" . 42))
           (first (magit-dash-review-json-export key))
           (counts (magit-dash-review-json-import first))
           (second (magit-dash-review-json-export key)))
      (should (equal (magit-dash-review-json-test/counts counts) '(:added 2 :updated 0 :skipped 0)))
      (should (seq-every-p (lambda (c) (magit-dash-review-json-test/fake-id-p (map-elt c "cr_id")))
                           (map-elt first "comments")))
      (should (= 2 (length (map-elt second "comments"))))
      (should (equal (magit-dash-review-json-test/plain first)
                     (magit-dash-review-json-test/plain second)))
      (should (equal (magit-dash-review-json-test/counts (magit-dash-review-json-import second))
                     '(:added 0 :updated 0 :skipped 2)))
      (should (= 2 (length (plist-get magit-dash-review-json-test/fake :comments)))))))

(ert-deftest magit-dash-review-json/api-import-updates-and-maps-plist-shape ()
  (magit-dash-review-json-test/with-fake-api
    (let ((table (magit-dash-review-json-test/table (magit-dash-review-json-test/draft "d1" "one"))))
      (setf (map-elt (aref (map-elt table "comments") 0) "lines") [3 5])
      (setf (map-elt (aref (map-elt table "comments") 0) "diff_side") "LEFT")
      (magit-dash-review-json-import table)
      (let ((stored (car (plist-get magit-dash-review-json-test/fake :comments))))
        (should (equal "a.el" (plist-get stored :path)))
        (should (= 5 (plist-get stored :line)))
        (should (equal '(3 5) (plist-get stored :lines)))
        (should (equal "LEFT" (plist-get stored :side)))
        (should (equal "one" (plist-get stored :body))))
      (setf (map-elt (aref (map-elt table "comments") 0) "body") "edited")
      (should (equal (magit-dash-review-json-test/counts (magit-dash-review-json-import table))
                     '(:added 0 :updated 1 :skipped 0)))
      (should (= 1 (length (plist-get magit-dash-review-json-test/fake :comments))))
      (should (equal "edited" (plist-get (car (plist-get magit-dash-review-json-test/fake :comments)) :body))))))

(ert-deftest magit-dash-review-json/api-export-maps-comments-replies-and-status ()
  (magit-dash-review-json-test/with-fake-api
    (let ((parent (+ (ash 1 40) 5)))
      (plist-put magit-dash-review-json-test/fake :comments
                 (list (list :id parent :remote-id "PRRC_x" :path "a.el" :line 7 :lines nil :side "RIGHT"
                             :body "remote" :author "bob" :at "t0" :status 'sent :reply-to nil :resolved t)
                       (list :id (+ (ash 1 44) 9) :remote-id nil :path "a.el" :line 7 :lines nil :side nil
                             :body "my reply" :author "me" :at "t1" :status 'draft :reply-to parent :resolved nil)))
      (let* ((table (magit-dash-review-json-export '("owner/repo" . 42)))
             (c (aref (map-elt table "comments") 0)))
        (should (= 1 (length (map-elt table "comments"))))
        (should (equal parent (map-elt c "cr_id")))
        (should (equal "PRRC_x" (map-elt c "remote_id")))
        (should (equal "sent" (map-elt c "status")))
        (should (eq t (map-elt c "resolved")))
        (should (equal [7 7] (map-elt c "lines")))
        (should (equal "my reply" (map-elt (aref (map-elt c "replies") 0) "body")))))))

(ert-deftest magit-dash-review-json/api-unpublished-reply-on-sent-comment-is-upserted-as-reply ()
  (magit-dash-review-json-test/with-fake-api
    (let* ((parent (+ (ash 1 40) 5))
           (reply (make-hash-table :test #'equal))
           (comment (magit-dash-review-json-test/comment parent "remote")))
      (plist-put magit-dash-review-json-test/fake :comments
                 (list (list :id parent :remote-id "PRRC_x" :path "a.el" :line 3 :body "remote"
                             :author "bob" :status 'sent)))
      (setf (map-elt comment "gh_comment_id") "PRRC_x")
      (setf (map-elt reply "id") "r1")
      (setf (map-elt reply "author") "me")
      (setf (map-elt reply "body") "thanks")
      (setf (map-elt comment "replies") (vector reply))
      (let ((table (magit-dash-review-json-test/table comment)))
        (should (equal (magit-dash-review-json-test/counts (magit-dash-review-json-import table))
                       '(:added 0 :updated 1 :skipped 0)))
        (let ((stored (plist-get magit-dash-review-json-test/fake :comments)))
          (should (= 2 (length stored)))
          (should (equal parent (plist-get (cadr stored) :reply-to))))
        (should (equal (magit-dash-review-json-test/counts (magit-dash-review-json-import table))
                       '(:added 0 :updated 0 :skipped 1)))))))

(ert-deftest magit-dash-review-json/api-sent-comment-without-stored-copy-is-skipped ()
  (magit-dash-review-json-test/with-fake-api
    (let ((comment (magit-dash-review-json-test/comment nil "from github")))
      (setf (map-elt comment "gh_thread_id") "T1")
      (setf (map-elt comment "status") "sent")
      (should (equal (magit-dash-review-json-test/counts
                      (magit-dash-review-json-import (magit-dash-review-json-test/table comment)))
                     '(:added 0 :updated 0 :skipped 1)))
      (should-not (plist-get magit-dash-review-json-test/fake :comments)))))

(ert-deftest magit-dash-review-json/api-dry-run-leaves-fake-unchanged ()
  (magit-dash-review-json-test/with-fake-api
    (setq magit-dash-review-json-test/source
          (magit-dash-review-json-test/table (magit-dash-review-json-test/draft "d1" "one")))
    (setf (map-elt magit-dash-review-json-test/source "verdict")
          (let ((v (make-hash-table :test #'equal)))
            (setf (map-elt v "state") "approved")
            (setf (map-elt v "head") "abc")
            v))
    (let ((before (magit-dash-review-json-test/fake-snapshot))
          (result (magit-dash-review-json-import-dry-run '("owner/repo" . 42))))
      (should (equal (magit-dash-review-json-test/counts result) '(:added 1 :updated 0 :skipped 0)))
      (should (plist-get result :dry-run))
      (should (equal before (magit-dash-review-json-test/fake-snapshot)))
      (should-not magit-dash-review-json-dry-run)
      (should (equal (magit-dash-review-json-test/counts
                      (magit-dash-review-json-import (magit-dash-review-json-export '("owner/repo" . 42))))
                     '(:added 1 :updated 0 :skipped 0)))
      (should (= 1 (length (plist-get magit-dash-review-json-test/fake :comments))))
      (should (eq 'approved (plist-get (plist-get magit-dash-review-json-test/fake :verdict) :state))))))

(ert-deftest magit-dash-review-json/api-dry-run-parses-key-string-and-rejects-garbage ()
  (magit-dash-review-json-test/with-fake-api
    (setq magit-dash-review-json-test/source (magit-dash-review-json-test/table))
    (should (= 0 (plist-get (magit-dash-review-json-import-dry-run "owner/repo#42") :added)))
    (should-error (magit-dash-review-json-import-dry-run "nonsense") :type 'user-error)))

(ert-deftest magit-dash-review-json/api-dry-run-errors-without-review-source ()
  (magit-dash-review-json-test/with-fake-api
    (setq magit-dash-review-json-test/source nil)
    (let ((err (should-error (magit-dash-review-json-import-dry-run '("owner/repo" . 42))
                             :type 'user-error)))
      (should (string-match-p "No review file" (cadr err))))))

(ert-deftest magit-dash-review-json/api-dry-run-refuses-seam-without-declared-support ()
  (let ((writes 0)
        (magit-dash-review-json-source-function
         (lambda (_key) (magit-dash-review-json-test/table (magit-dash-review-json-test/draft "d1" "x"))))
        (magit-dash-review-json-code-review-function
         (lambda (op &rest _) (when (eq op 'write) (setq writes (1+ writes))) nil)))
    (should-error (magit-dash-review-json-import-dry-run '("owner/repo" . 42)) :type 'user-error)
    (should (= 0 writes))
    (put 'magit-dash-review-json-test/declared-seam 'magit-dash-review-json-dry-run-supported t)
    (fset 'magit-dash-review-json-test/declared-seam (lambda (&rest _) nil))
    (unwind-protect
        (let ((magit-dash-review-json-code-review-function 'magit-dash-review-json-test/declared-seam))
          (should (plist-get (magit-dash-review-json-import-dry-run '("owner/repo" . 42)) :dry-run)))
      (fmakunbound 'magit-dash-review-json-test/declared-seam))))

(ert-deftest magit-dash-review-json/api-import-refuses-idless-drafts ()
  (magit-dash-review-json-test/with-fake-api
    (let ((c (magit-dash-review-json-test/comment nil "hand written")))
      (map-delete c "id")
      (should (equal (magit-dash-review-json-test/counts
                      (magit-dash-review-json-import (magit-dash-review-json-test/table c)))
                     '(:added 0 :updated 0 :skipped 1)))
      (should-not (plist-get magit-dash-review-json-test/fake :comments))
      (setq magit-dash-review-json-test/source (magit-dash-review-json-test/table c))
      (let* ((exported (magit-dash-review-json-export '("owner/repo" . 42)))
             (stamped (aref (map-elt exported "comments") 0)))
        (should (stringp (map-elt stamped "id")))
        (should (magit-dash-review-json-test/fake-id-p (map-elt stamped "cr_id")))
        (should (equal '(:added 1 :updated 0 :skipped 0)
                       (magit-dash-review-json-test/counts (magit-dash-review-json-import exported))))))))

(ert-deftest magit-dash-review-json/api-resolved-only-difference-is-unchanged ()
  (magit-dash-review-json-test/with-fake-api
    (let ((table (magit-dash-review-json-test/table (magit-dash-review-json-test/draft "d1" "x"))))
      (magit-dash-review-json-import table)
      (setf (map-elt (aref (map-elt table "comments") 0) "resolved") t)
      (should (equal '(:added 0 :updated 0 :skipped 1)
                     (magit-dash-review-json-test/counts (magit-dash-review-json-import table)))))))

(ert-deftest magit-dash-review-json/api-published-reply-compares-consistently ()
  (magit-dash-review-json-test/with-fake-api
    (let* ((parent (+ (ash 1 40) 5))
           (comment (magit-dash-review-json-test/comment parent "remote"))
           (published (make-hash-table :test #'equal))
           (draft (make-hash-table :test #'equal)))
      (plist-put magit-dash-review-json-test/fake :comments
                 (list (list :id parent :remote-id "PRRC_x" :path "a.el" :line 3 :body "remote"
                             :author "bob" :status 'sent)))
      (setf (map-elt comment "gh_comment_id") "PRRC_x")
      (setf (map-elt published "gh_comment_id") "PRRC_r")
      (setf (map-elt published "author") "bob")
      (setf (map-elt published "body") "already on github")
      (setf (map-elt comment "replies") (vector published))
      (let ((table (magit-dash-review-json-test/table comment)))
        (should (equal '(:added 0 :updated 0 :skipped 1)
                       (magit-dash-review-json-test/counts (magit-dash-review-json-import table))))
        (setf (map-elt draft "id") "r1")
        (setf (map-elt draft "body") "mine")
        (setf (map-elt comment "replies") (vector published draft))
        (should (equal '(:added 0 :updated 1 :skipped 0)
                       (magit-dash-review-json-test/counts (magit-dash-review-json-import table))))
        (should (equal '(:added 0 :updated 0 :skipped 1)
                       (magit-dash-review-json-test/counts (magit-dash-review-json-import table))))
        (should (= 2 (length (plist-get magit-dash-review-json-test/fake :comments))))))))

(ert-deftest magit-dash-review-json/api-origin-github-is-never-stored-as-draft ()
  (magit-dash-review-json-test/with-fake-api
    (let ((c (magit-dash-review-json-test/comment nil "from github")))
      (setf (map-elt c "origin") "github")
      (should (equal '(:added 0 :updated 0 :skipped 1)
                     (magit-dash-review-json-test/counts
                      (magit-dash-review-json-import (magit-dash-review-json-test/table c)))))
      (should-not (plist-get magit-dash-review-json-test/fake :comments)))))

(ert-deftest magit-dash-review-json/api-import-never-clears-verdict ()
  (magit-dash-review-json-test/with-fake-api
    (plist-put magit-dash-review-json-test/fake :verdict
               (list :state 'approved :head-oid "abc" :at "t"))
    (let ((table (magit-dash-review-json-test/table))
          (empty (make-hash-table :test #'equal)))
      (setf (map-elt table "verdict") empty)
      (should (= 0 (plist-get (magit-dash-review-json-import table) :verdict-failed)))
      (should (eq 'approved (plist-get (plist-get magit-dash-review-json-test/fake :verdict) :state)))
      (setf (map-elt empty "state") "")
      (magit-dash-review-json-import table)
      (should (eq 'approved (plist-get (plist-get magit-dash-review-json-test/fake :verdict) :state)))
      (should-error (funcall magit-dash-review-json-code-review-function 'verdict
                             '("owner/repo" . 42) empty)
                    :type 'user-error))))

(ert-deftest magit-dash-review-json/api-op-checks-the-exact-functions-it-calls ()
  (magit-dash-review-json-test/with-fake-api
    (fmakunbound 'code-review-api-analysis-available-p)
    (let ((start (with-current-buffer (get-buffer-create "*Messages*") (point-max)))
          (message-log-max t))
      (should-not (funcall magit-dash-review-json-code-review-function 'analysis '("owner/repo" . 42)))
      (should (string-match-p "code-review-api-analysis-available-p is not available"
                              (with-current-buffer "*Messages*"
                                (buffer-substring-no-properties start (point-max))))))))

(ert-deftest magit-dash-review-json/api-autoloaded-entry-point-counts-as-available ()
  (let ((file (make-temp-file "mdrj-autoload-" nil ".el"
                              ";;; -*- lexical-binding: t -*-\n(defun code-review-api-comments (_r _n) (list (list :id 1 :path \"a\" :body \"b\" :status 'sent)))\n"))
        (old (and (fboundp 'code-review-api-comments) (symbol-function 'code-review-api-comments))))
    (unwind-protect
        (progn
          (fset 'code-review-api-comments (list 'autoload file nil nil nil))
          (should (autoloadp (symbol-function 'code-review-api-comments)))
          (let ((comments (magit-dash-review-json-code-review-default 'comments '("o/r" . 1))))
            (should (= 1 (length comments)))
            (should (equal "b" (map-elt (car comments) "body")))))
      (if old
          (fset 'code-review-api-comments old)
        (fmakunbound 'code-review-api-comments))
      (delete-file file))))

(ert-deftest magit-dash-review-json/api-verdict-round-trip-and-bad-state ()
  (magit-dash-review-json-test/with-fake-api
    (let ((table (magit-dash-review-json-test/table))
          (verdict (make-hash-table :test #'equal)))
      (setf (map-elt verdict "state") "changes")
      (setf (map-elt verdict "head") "abc")
      (setf (map-elt verdict "by") "me")
      (setf (map-elt table "verdict") verdict)
      (should (= 0 (plist-get (magit-dash-review-json-import table) :verdict-failed)))
      (should (equal "changes" (map-elt (funcall magit-dash-review-json-code-review-function
                                                 'verdict '("owner/repo" . 42))
                                        "state")))
      (should (= 0 (plist-get (magit-dash-review-json-import table) :verdict-failed)))
      (setf (map-elt verdict "state") "bogus")
      (should (= 1 (plist-get (magit-dash-review-json-import table) :verdict-failed))))))

(ert-deftest magit-dash-review-json/api-viewed-ops-and-blob-agreement ()
  (magit-dash-review-json-test/with-fake-api
    (plist-put magit-dash-review-json-test/fake :viewed
               (list (cons "a.el" (list :blob "1234567abcdef" :viewed-p t))
                     (cons "b.el" (list :blob "9999999" :viewed-p nil))))
    (let* ((key '("owner/repo" . 42))
           (api (funcall magit-dash-review-json-code-review-function 'viewed key))
           (local (make-hash-table :test #'equal)))
      (should (eq t (map-elt (map-elt api "a.el") "viewed")))
      (should (eq :false (map-elt (map-elt api "b.el") "viewed")))
      (setf (map-elt local "a.el") "1234567abcdef")
      (setf (map-elt local "b.el") "9999999")
      (setf (map-elt local "c.el") "0000000")
      (should (equal '(("a.el" . "1234567abcdef"))
                     (map-pairs (magit-dash-review-json-viewed-agreement local api))))
      (setf (map-elt local "a.el") "7654321zzz")
      (should-not (map-pairs (magit-dash-review-json-viewed-agreement local api)))
      (setf (map-elt local "a.el") "1234")
      (should-not (map-pairs (magit-dash-review-json-viewed-agreement local api)))
      (let ((marks (make-hash-table :test #'equal)))
        (setf (map-elt marks "a.el") "1234567")
        (setf (map-elt marks "b.el") "9999999")
        (setf (map-elt marks "b.el") "0000000")
        (setf (map-elt marks "z.el") "9999999")
        (let ((result (funcall magit-dash-review-json-code-review-function 'set-viewed key marks)))
          (should (equal "unchanged" (map-elt result "a.el")))
          (should (equal "stale" (map-elt result "b.el")))
          (should (equal "stale" (map-elt result "z.el"))))
        (setf (map-elt marks "b.el") "9999999")
        (should (equal "set" (map-elt (funcall magit-dash-review-json-code-review-function
                                               'set-viewed key marks)
                                      "b.el")))))))

(ert-deftest magit-dash-review-json/api-analysis-export-keyed-by-ranges ()
  (magit-dash-review-json-test/with-fake-api
    (plist-put magit-dash-review-json-test/fake :analysis
               '(("a.el" . (("-1,3 +1,4" . (:rank 2 :tags ("NO-TEST") :criteria ("c1")))
                            ("-10,2 +11,2" . (:rank 1 :tags nil :criteria nil))))))
    (let* ((key '("owner/repo" . 42))
           (plain (magit-dash-review-json-export key))
           (table (magit-dash-review-json-export key t))
           (hunk (map-nested-elt table '("code_review" "analysis" "a.el" "-1,3 +1,4"))))
      (should-not (map-nested-elt plain '("code_review" "analysis")))
      (should (= 2 (map-elt hunk "rank")))
      (should (equal ["NO-TEST"] (map-elt hunk "tags")))
      (should (equal ["c1"] (map-elt hunk "criteria")))
      (should (equal [] (map-nested-elt table '("code_review" "analysis" "a.el" "-10,2 +11,2" "tags"))))
      (should (json-serialize table :null-object nil :false-object :false)))
    (plist-put magit-dash-review-json-test/fake :worktree nil)
    (should-not (map-nested-elt (magit-dash-review-json-export '("owner/repo" . 42) t)
                                '("code_review" "analysis")))))

(ert-deftest magit-dash-review-json/api-skips-with-message-when-not-available ()
  (let ((saved (seq-map (lambda (f) (cons f (symbol-function f)))
                        (seq-filter #'fboundp
                                    '(code-review-api-comments code-review-api-upsert-comment
                                      code-review-api-verdict code-review-api-set-verdict
                                      code-review-api-viewed code-review-api-set-viewed
                                      code-review-api-analysis code-review-api-analysis-available-p)))))
    (unwind-protect
        (let ((start (with-current-buffer (get-buffer-create "*Messages*") (point-max)))
              (message-log-max t)
              (magit-dash-review-json-code-review-function #'magit-dash-review-json-code-review-default))
          (seq-do (lambda (f) (fmakunbound (car f))) saved)
          (should-not (funcall magit-dash-review-json-code-review-function 'comments '("o/r" . 1)))
          (should-error (funcall magit-dash-review-json-code-review-function 'write '("o/r" . 1)
                                 (magit-dash-review-json-test/comment nil "x"))
                        :type 'user-error)
          (should (equal (magit-dash-review-json-test/counts
                          (magit-dash-review-json-import
                           (magit-dash-review-json-test/table (magit-dash-review-json-test/comment nil "x"))))
                         '(:added 0 :updated 0 :skipped 1)))
          (should (string-match-p "skipping comments, code-review-api-comments is not available"
                                  (with-current-buffer "*Messages*"
                                    (buffer-substring-no-properties start (point-max))))))
      (seq-do (lambda (f) (fset (car f) (cdr f))) saved))))

(ert-deftest magit-dash-review-json/api-failing-write-is-skipped-not-raised ()
  (magit-dash-review-json-test/with-fake-api
    (let ((c (magit-dash-review-json-test/draft "d1" "x")))
      (map-delete c "path")
      (should (equal (magit-dash-review-json-test/counts
                      (magit-dash-review-json-import (magit-dash-review-json-test/table c)))
                     '(:added 0 :updated 0 :skipped 1))))))

(ert-deftest magit-dash-review-json/draft-id-is-deterministic-and-in-range ()
  (let ((a (magit-dash-review-json-test/draft "d1" "one"))
        (b (magit-dash-review-json-test/draft "d2" "one")))
    (should (= (magit-dash-review-json-draft-id a) (magit-dash-review-json-draft-id a)))
    (should-not (= (magit-dash-review-json-draft-id a) (magit-dash-review-json-draft-id b)))
    (should (magit-dash-review-json-test/fake-id-p (magit-dash-review-json-draft-id a)))
    (should-not (= (magit-dash-review-json-draft-id a) (magit-dash-review-json-draft-id b a)))))

;;; Worktree resolver

(ert-deftest magit-dash-review-json/resolver-uses-existing-worktree-and-never-creates ()
  (let ((created nil)
        (seen nil))
    (cl-letf (((symbol-function 'magit-dash-pr--find-existing-worktree)
               (lambda (dir number &rest _) (setq seen (list dir number)) "/tmp/repo/.wt/pr-5/"))
              ((symbol-function 'magit-dash-pr--create-worktree-for-entry)
               (lambda (&rest _) (setq created t) nil))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) (error "prompted")))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) (error "prompted")))
              ((symbol-function 'read-string) (lambda (&rest _) (error "prompted"))))
      (should (equal "/tmp/repo/.wt/pr-5" (magit-dash-review-worktree-resolver "/tmp/repo/" 5)))
      (should (equal '("/tmp/repo" 5) seen))
      (should-not created))))

(ert-deftest magit-dash-review-json/resolver-returns-nil-on-failure-or-no-worktree ()
  (cl-letf (((symbol-function 'magit-dash-pr--find-existing-worktree)
             (lambda (&rest _) (error "git exploded"))))
    (should-not (magit-dash-review-worktree-resolver "/tmp/repo" 5)))
  (cl-letf (((symbol-function 'magit-dash-pr--find-existing-worktree) (lambda (&rest _) nil)))
    (should-not (magit-dash-review-worktree-resolver "/tmp/repo" 5))))

(ert-deftest magit-dash-review-json/mirror-flag-stays-off-by-default ()
  (should-not (default-value 'magit-dash-review-use-code-review-adapter)))

;;; Integration against the real code-review API (skipped without it)

(ert-deftest magit-dash-review-json/real-api-import-dry-run-and-idempotence ()
  "Run the default seam on a temp closql db; skip only when ~/src/code-review is absent."
  (let ((dir (expand-file-name "~/src/code-review")))
    (unless (file-directory-p dir)
      (ert-skip "~/src/code-review does not exist"))
    (add-to-list 'load-path dir)
    (add-to-list 'load-path (expand-file-name "test" dir))
    (unless (and (locate-library "code-review-api") (locate-library "code-review-test-helpers"))
      (ert-fail (format "%s exists but code-review-api or its test helpers are missing" dir)))
    (require 'code-review)
    (require 'code-review-api)
    (require 'code-review-test-helpers))
  (eval
   '(code-review-test--with-db
      (code-review-db--pullreq-create
       (code-review-github-repo :owner "owner" :repo "repo" :number "42"))
      (let ((pr (code-review-db-get-pullreq)))
        (oset pr saved t)
        (code-review-db-update pr))
      (let* ((magit-dash-review-json-code-review-function #'magit-dash-review-json-code-review-default)
             (magit-dash-review-json-dry-run nil)
             (source (magit-dash-review-json-test/table
                      (magit-dash-review-json-test/draft "d1" "one")))
             (magit-dash-review-json-source-function (lambda (_key) source))
             (key '("owner/repo" . 42)))
        (should (= 0 (plist-get (magit-dash-review-json-import-dry-run key) :skipped)))
        (should-not (code-review-api-comments "owner/repo" 42))
        (let* ((first (magit-dash-review-json-export key))
               (counts (magit-dash-review-json-import first))
               (second (magit-dash-review-json-export key)))
          (should (equal (magit-dash-review-json-test/counts counts) '(:added 1 :updated 0 :skipped 0)))
          (should (equal (magit-dash-review-json-test/plain first)
                         (magit-dash-review-json-test/plain second)))
          (should (= 1 (length (code-review-api-comments "owner/repo" 42))))
          (should (equal (magit-dash-review-json-test/counts (magit-dash-review-json-import second))
                         '(:added 0 :updated 0 :skipped 1))))))
   t))

(provide 'test-magit-dash-review-json)
;;; test-magit-dash-review-json.el ends here
