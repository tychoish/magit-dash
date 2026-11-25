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

(ert-deftest magit-dash-review-json/default-seam-skips-everything ()
  "With the default seam (no public code-review API) nothing is written."
  (let ((magit-dash-review-json-code-review-function #'magit-dash-review-json-code-review-default))
    (should (equal (magit-dash-review-json-test/counts
                    (magit-dash-review-json-import (magit-dash-review-json-test/full-table)))
                   '(:added 0 :updated 0 :skipped 4)))))

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

(provide 'test-magit-dash-review-json)
;;; test-magit-dash-review-json.el ends here
