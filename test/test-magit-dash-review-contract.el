;;; test-magit-dash-review-contract.el --- Tests for the diff-review/1 contract -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Reads the fixtures in test/fixtures/review-*.json through the real
;; `magit-dash-review--read-json-file', round-trips them through
;; `--write-json-file', and characterises `--merge-json'.  See
;; docs/review-json.org.

;;; Code:

(require 'ert)
(require 'map)
(require 'seq)
(require 'magit-dash-review)
(require 'test-helper)

(defconst magit-dash-review-contract/fixture-dir
  (expand-file-name "fixtures" (file-name-directory (or load-file-name buffer-file-name))))

(defconst magit-dash-review-contract/fixtures
  '("review-plain.json" "review-remote-threads.json" "review-draft.json" "review-mixed.json"))

(defun magit-dash-review-contract/read (name)
  "Read fixture NAME with `magit-dash-review--read-json-file'."
  (magit-dash-review--read-json-file
   (expand-file-name name magit-dash-review-contract/fixture-dir)))

(defun magit-dash-review-contract/plain (data)
  "Return DATA with hash tables as key-sorted alists and arrays as lists."
  (cond
   ((hash-table-p data)
    (thread-last (map-pairs data)
                 (seq-map (lambda (pair)
                            (cons (car pair) (magit-dash-review-contract/plain (cdr pair)))))
                 (seq-sort-by #'car #'string<)))
   ((vectorp data)
    (seq-map #'magit-dash-review-contract/plain data))
   (t data)))

(defun magit-dash-review-contract/comment (data id)
  "Return the comment in DATA whose \"id\" is ID."
  (seq-find (lambda (c) (equal (map-elt c "id") id)) (map-elt data "comments")))

(defun magit-dash-review-contract/remote-thread ()
  "Return a transcoded-style remote thread matching fixture thread gh-t1."
  (let ((remote (make-hash-table :test #'equal))
        (reply (make-hash-table :test #'equal)))
    (setf (map-elt reply "id") "gh-r1")
    (setf (map-elt reply "gh_comment_id") "PRRC_kwDOabc2")
    (setf (map-elt reply "author") "tychoish")
    (setf (map-elt reply "at") "2026-11-18T11:00:00+13:00")
    (setf (map-elt reply "body") "Order matters.")
    (setf (map-elt remote "id") "gh-new")
    (setf (map-elt remote "gh_thread_id") "PRRT_kwDOabc1")
    (setf (map-elt remote "gh_comment_id") "PRRC_kwDOabc1")
    (setf (map-elt remote "gh_resolved") :false)
    (setf (map-elt remote "origin") "github")
    (setf (map-elt remote "author") "octocat")
    (setf (map-elt remote "at") "2026-11-18T10:00:00+13:00")
    (setf (map-elt remote "body") "Why not use a map here?")
    (setf (map-elt remote "resolved") :false)
    (setf (map-elt remote "replies") (vector reply))
    remote))

(ert-deftest magit-dash-review/contract-core-fields ()
  (seq-do
   (lambda (name)
     (let ((data (magit-dash-review-contract/read name)))
       (should (equal (map-elt data "format") "diff-review/1"))
       (should (stringp (map-elt data "root")))
       (should (stringp (map-elt data "base")))
       (should (hash-table-p (map-elt data "viewed")))
       (should (vectorp (map-elt data "comments")))
       (seq-do (lambda (c)
                 (should (stringp (map-elt c "id")))
                 (should (stringp (map-elt c "author")))
                 (should (stringp (map-elt c "at")))
                 (should (stringp (map-elt c "body")))
                 (when (map-contains-key c "resolved")
                   (should (memq (map-elt c "resolved") '(t :false))))
                 (when (map-elt c "lines")
                   (should (= 2 (length (map-elt c "lines"))))
                   (should (stringp (map-elt c "path")))))
               (map-elt data "comments"))))
   magit-dash-review-contract/fixtures))

(ert-deftest magit-dash-review/contract-extension-keys ()
  (let ((remote (magit-dash-review-contract/read "review-remote-threads.json"))
        (draft (magit-dash-review-contract/read "review-draft.json")))
    (should (stringp (map-elt (map-elt remote "code_review") "pullreq_id")))
    (should (map-elt (map-elt remote "code_review") "analysis"))
    (should (map-elt (map-elt draft "code_review") "analysis"))
    (seq-do (lambda (c)
              (should (integerp (map-elt c "cr_id")))
              (should (stringp (map-elt c "remote_id")))
              (should (equal (map-elt c "status") "sent")))
            (map-elt remote "comments"))
    (let ((t1 (magit-dash-review-contract/comment remote "gh-t1")))
      (should-not (equal (map-elt t1 "remote_id") (map-elt t1 "gh_comment_id")))
      (should-not (eq (map-elt t1 "resolved") (map-elt t1 "gh_resolved"))))
    (should (equal (seq-map (lambda (c) (map-elt c "status")) (map-elt draft "comments"))
                   '("draft" "pending")))
    (should-not (seq-some (lambda (c) (map-elt c "remote_id")) (map-elt draft "comments")))))

(ert-deftest magit-dash-review/contract-round-trip ()
  (seq-do
   (lambda (name)
     (let ((file (make-temp-file "review-contract" nil ".json")))
       (unwind-protect
           (progn
             (magit-dash-review--write-json-file file (magit-dash-review-contract/read name))
             (should (equal (magit-dash-review-contract/plain (magit-dash-review--read-json-file file))
                            (magit-dash-review-contract/plain (magit-dash-review-contract/read name)))))
         (delete-file file))))
   magit-dash-review-contract/fixtures))

(ert-deftest magit-dash-review/contract-merge-unmatched-remote-passthrough ()
  "Remote threads without a local match are added untouched; no existing file."
  (let ((merged (magit-dash-review--merge-json
                 nil nil
                 (map-elt (magit-dash-review-contract/read "review-remote-threads.json")
                          "comments"))))
    (should (equal (seq-map (lambda (c) (map-elt c "id")) (map-elt merged "comments"))
                   '("gh-t1" "gh-t2")))
    (should (equal (map-elt (magit-dash-review-contract/comment merged "gh-t1") "cr_id") 101))
    (should (equal (map-elt (map-elt merged "magit_dash") "provider") "github"))))

;; Merge behaviour after the json-adapter task (docs/review-json.org, "Merge behaviour").

(ert-deftest magit-dash-review/contract-merge-preserves-code-review-and-viewed ()
  (let* ((draft (magit-dash-review-contract/read "review-draft.json"))
         (merged (magit-dash-review--merge-json draft nil nil)))
    (should (equal (magit-dash-review-contract/plain (map-elt merged "code_review"))
                   (magit-dash-review-contract/plain (map-elt draft "code_review"))))
    (should (equal (magit-dash-review-contract/plain (map-elt merged "viewed"))
                   (magit-dash-review-contract/plain (map-elt draft "viewed"))))
    (should (equal (map-elt (map-elt merged "verdict") "state") "approved"))
    ;; Threads without gh_thread_id are kept whole.
    (should (equal (seq-map (lambda (c) (map-elt c "cr_id")) (map-elt merged "comments"))
                   '(7 8)))))

(defun magit-dash-review-contract/merge-mixed ()
  "Merge the mixed fixture with the matching remote thread."
  (magit-dash-review--merge-json
   (magit-dash-review-contract/read "review-mixed.json")
   nil (list (magit-dash-review-contract/remote-thread))))

(ert-deftest magit-dash-review/contract-merge-matched-sticky-resolved-and-replies ()
  (let ((matched (magit-dash-review-contract/comment
                  (magit-dash-review-contract/merge-mixed) "gh-t1")))
    (should (eq (map-elt matched "resolved") t))
    (should (equal (seq-map (lambda (r) (map-elt r "body")) (map-elt matched "replies"))
                   '("Order matters." "Unpublished local reply.")))))

(ert-deftest magit-dash-review/contract-merge-local-only-thread-kept-whole ()
  (let ((local (magit-dash-review-contract/comment
                (magit-dash-review-contract/merge-mixed) "l1")))
    (should (equal (map-elt local "x_bar") "kept"))
    (should (equal (map-elt local "cr_id") 9))))

(ert-deftest magit-dash-review/contract-merge-preserves-top-level-unknown-key ()
  (should (equal (map-elt (magit-dash-review-contract/merge-mixed) "x_foo")
                 "top-level unknown")))

(ert-deftest magit-dash-review/contract-merge-preserves-code-review-key ()
  (should (equal (map-elt (map-elt (magit-dash-review-contract/merge-mixed) "code_review")
                          "pullreq_id")
                 "PR_kwDOabc123")))

(ert-deftest magit-dash-review/contract-merge-matched-keeps-unknown-comment-key ()
  (should (equal (map-elt (magit-dash-review-contract/comment
                           (magit-dash-review-contract/merge-mixed) "gh-t1")
                          "x_bar")
                 "local-only key")))

(ert-deftest magit-dash-review/contract-merge-matched-keeps-cr-id ()
  (should (equal (map-elt (magit-dash-review-contract/comment
                           (magit-dash-review-contract/merge-mixed) "gh-t1")
                          "cr_id")
                 101)))

(ert-deftest magit-dash-review/contract-merge-matched-keeps-remote-id-and-status ()
  (let ((matched (magit-dash-review-contract/comment
                  (magit-dash-review-contract/merge-mixed) "gh-t1")))
    (should (equal (map-elt matched "remote_id") "PRRC_kwDOnode1"))
    (should (equal (map-elt matched "status") "sent"))))

(ert-deftest magit-dash-review/contract-merge-matched-keeps-local-id ()
  (should-not (magit-dash-review-contract/comment
               (magit-dash-review-contract/merge-mixed) "gh-new")))

(ert-deftest magit-dash-review/contract-merge-preserves-viewed ()
  (should (equal (map-elt (map-elt (magit-dash-review-contract/merge-mixed) "viewed") "src/a.ts")
                 "bbbb2222")))

(ert-deftest magit-dash-review/contract-merge-matches-by-cr-id-then-hash ()
  "A thread with no gh ids matches by cr_id, else by content hash.
The hash tier only absorbs local-origin candidates for a local-origin comment."
  (let* ((existing (magit-dash-review-contract/read "review-mixed.json"))
         (by-cr (make-hash-table :test #'equal))
         (by-hash (make-hash-table :test #'equal)))
    (setf (map-elt by-cr "id") "r1")
    (setf (map-elt by-cr "cr_id") 9)
    (setf (map-elt by-cr "body") "Local-only thread, edited.")
    (setf (map-elt by-hash "id") "r2")
    (setf (map-elt by-hash "origin") "local")
    (setf (map-elt by-hash "path") "src/c.ts")
    (setf (map-elt by-hash "lines") [1 2])
    (setf (map-elt by-hash "body") "Local-only thread.")
    (should (equal "l1" (map-elt (magit-dash-review-json-find-match
                                  by-cr (magit-dash-review-json--comments existing))
                                 "id")))
    (should (equal "l1" (map-elt (magit-dash-review-json-find-match
                                  by-hash (magit-dash-review-json--comments existing))
                                 "id")))))

(provide 'test-magit-dash-review-contract)
;;; test-magit-dash-review-contract.el ends here
