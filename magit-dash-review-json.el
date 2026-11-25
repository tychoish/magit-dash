;;; magit-dash-review-json.el --- diff-review/1 JSON adapter for code-review -*- lexical-binding: t; -*-

;;; Commentary:
;; Interchange between the diff-review/1 review JSON file and code-review.
;; See docs/review-json.org.
;;
;; - `magit-dash-review-json-export' builds the diff-review/1 table for a
;;   PR from the review file plus code-review's comments.
;; - `magit-dash-review-json-import' mirrors the JSON comments and verdict
;;   into code-review and reports (:added N :updated N :skipped N
;;   :verdict-failed N :assigned ALIST); `-apply-assigned' writes the
;;   cr_ids code-review assigned back into a table.
;; - `magit-dash-review-json-merge' and `-transcode-thread' hold the
;;   GitHub thread merge that used to live in magit-dash-review.el.
;;
;; code-review has no supported data API for reading or writing comments
;; (the audit is in docs/review-json.org), so every call into it goes through
;; one overridable seam, `magit-dash-review-json-code-review-function'.
;; This file never touches closql objects.

;;; Code:

(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)

(declare-function magit-dash-review-file "magit-dash-review" (root pr-number &optional repo))
(declare-function magit-dash-worktree-main-dir "magit-dash-worktree" (&optional directory))

(defgroup magit-dash-review-json nil
  "diff-review/1 JSON adapter for code-review."
  :group 'magit-dash)

(defcustom magit-dash-review-json-code-review-function
  #'magit-dash-review-json-code-review-default
  "Function through which the adapter reads and writes code-review state.
Called as (FUNCTION OP PR-KEY &optional ARG) where PR-KEY is (REPO . NUMBER)
and OP is one of:
  `comments'  return code-review's comments as a list of tables shaped
              like diff-review/1 comments (id, path, lines, author, at,
              body, resolved, replies, cr_id, remote_id, status).
  `pullreq'   return a table with pullreq_id, account and analysis, or nil.
  `write'     store the comment table ARG; return the stored table (with
              cr_id and status set), or signal an error.
  `verdict'   with ARG, store the verdict table ARG and return non-nil or
              signal; without ARG, return the stored verdict table or nil.
Tables passed and returned are diff-review/1 JSON values: hash tables
\(equal test), vectors, strings, numbers, t, nil and :false.  Lists are
tolerated in what the seam returns: export turns them into vectors.
`magit-dash-review-json-deep-copy' accepts exactly these shapes.
The default reports that code-review exposes no supported data API."
  :type 'function
  :group 'magit-dash-review-json)

(defcustom magit-dash-review-json-source-function
  #'magit-dash-review-json-default-source
  "Function returning the current review JSON table for a PR key, or nil.
Called with (REPO . NUMBER)."
  :type 'function
  :group 'magit-dash-review-json)

(defconst magit-dash-review-json--remote-keys
  '("gh_thread_id" "gh_comment_id" "gh_resolved" "origin" "path" "lines"
    "blob" "author" "at" "body" "outdated")
  "Comment keys that GitHub owns: a merge overwrites them with remote values.
Every other key keeps its local value when the local thread has one.")

;;; Seam default and source

(defun magit-dash-review-json-code-review-default (op _pr-key &optional arg)
  "Default `magit-dash-review-json-code-review-function' for OP.
Reads return nil; writes signal, so comments stay drafts."
  (cond
   ((memq op '(comments pullreq)) nil)
   ((and (eq op 'verdict) (null arg)) nil)
   (t (error "code-review has no supported data API to %s; set `magit-dash-review-json-code-review-function'" op))))

(defun magit-dash-review-json-default-source (pr-key)
  "Read the review file for PR-KEY from the main worktree of `default-directory'.
Return nil, with a message, when the file cannot be read or its magit_dash
repo is not (car PR-KEY)."
  (require 'magit-dash-review)
  (condition-case err
      (when-let* ((dir (and (fboundp 'magit-dash-worktree-main-dir)
                            (magit-dash-worktree-main-dir default-directory)))
                  (file (magit-dash-review-file dir (cdr pr-key) (car pr-key)))
                  ((file-readable-p file))
                  (table (magit-dash-review-json-read-file file)))
        (cond
         ((equal (car (magit-dash-review-json-pr-key table)) (car pr-key))
          table)
         (t
          (message "magit-dash-review-json: %s is for %s, not %s; ignoring it"
                   file (car (magit-dash-review-json-pr-key table)) (car pr-key))
          nil)))
    (error (message "magit-dash-review-json: cannot read review file for %s: %s"
                    pr-key (error-message-string err))
           nil)))

;;; File I/O and small helpers

(defun magit-dash-review-json--parse-file (file)
  "Parse FILE as UTF-8 JSON into a hash table."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents file))
    (json-parse-string (buffer-string)
                       :object-type 'hash-table
                       :array-type 'array
                       :null-object nil
                       :false-object :false)))

(defun magit-dash-review-json-read-file (review-file)
  "Parse REVIEW-FILE as a diff-review/1 JSON hash table.
When REVIEW-FILE is unparsable and the REVIEW-FILE.bak left by
`magit-dash-review-json-write-file' parses, message and return the backup."
  (unless (and review-file (file-readable-p review-file))
    (error "Review file not readable: %s" review-file))
  (let ((backup (concat review-file ".bak")))
    (condition-case err
        (magit-dash-review-json--parse-file review-file)
      (error
       (cond
        ((file-readable-p backup)
         (message "magit-dash-review-json: %s is corrupt (%s); falling back to %s"
                  review-file (error-message-string err) backup)
         (magit-dash-review-json--parse-file backup))
        (t (signal (car err) (cdr err))))))))

(defun magit-dash-review-json-write-file (review-file data)
  "Serialize DATA hash table to REVIEW-FILE as pretty-printed UTF-8 JSON.
The write is atomic: the JSON goes to a temporary file in the same
directory which is renamed over REVIEW-FILE, so a failure leaves the old
file intact.  The previous file is first copied to REVIEW-FILE.bak."
  (let* ((file (expand-file-name review-file))
         (temp nil)
         (json (decode-coding-string
                (json-serialize data :null-object nil :false-object :false)
                'utf-8)))
    (make-directory (file-name-directory file) t)
    (unwind-protect
        (progn
          (setq temp (make-temp-file (concat file ".tmp-")))
          (with-temp-buffer
            (insert json)
            (json-pretty-print-buffer)
            (insert "\n")
            (let ((coding-system-for-write 'utf-8-unix))
              (write-region (point-min) (point-max) temp nil 'silent)))
          (when (file-exists-p file)
            (copy-file file (concat file ".bak") t))
          (rename-file temp file t)
          (setq temp nil))
      (when (and temp (file-exists-p temp))
        (delete-file temp))))
  review-file)

(defun magit-dash-review-json-relocate-worktree (repo-dir old-path new-path)
  "Point the review files of REPO-DIR that name OLD-PATH at NEW-PATH.
Rewrites the root and magit_dash.worktree of every <git-common-dir>/review
JSON file whose value is OLD-PATH, after a worktree was moved.  Unreadable
files are left alone.  Return the number of files rewritten."
  (let* ((common (with-temp-buffer
                   (when (zerop (call-process "git" nil (list (current-buffer) nil) nil
                                              "-C" (expand-file-name repo-dir)
                                              "rev-parse" "--path-format=absolute"
                                              "--git-common-dir"))
                     (string-trim (buffer-string)))))
         (review-dir (and common (not (string-empty-p common))
                          (expand-file-name "review" common)))
         (old (file-name-as-directory (expand-file-name old-path)))
         (new (file-name-as-directory (expand-file-name new-path)))
         (same (lambda (value)
                 (and (stringp value) (not (string-empty-p value))
                      (equal (file-name-as-directory (expand-file-name value)) old)))))
    (if (not (and review-dir (file-directory-p review-dir)))
        0
      (length
       (seq-filter
        (lambda (file)
          (condition-case err
              (let* ((table (magit-dash-review-json-read-file file))
                     (md (map-elt table "magit_dash"))
                     (hit nil))
                (when (funcall same (map-elt table "root"))
                  (setf (map-elt table "root") new)
                  (setq hit t))
                (when (and (hash-table-p md) (funcall same (map-elt md "worktree")))
                  (setf (map-elt md "worktree") new)
                  (setq hit t))
                (when hit
                  (magit-dash-review-json-write-file file table))
                hit)
            (error (message "magit-dash-review-json: cannot relocate %s: %s"
                            file (error-message-string err))
                   nil)))
        (directory-files review-dir t "\\.json\\'"))))))

(defun magit-dash-review-json-deep-copy (value)
  "Return a deep copy of VALUE.
Hash tables (same test), vectors, lists and strings are copied
recursively; numbers, symbols (t, nil, :false) are returned as is."
  (cond
   ((hash-table-p value)
    (let ((copy (make-hash-table :test (hash-table-test value) :size (hash-table-count value))))
      (map-do (lambda (key val)
                (setf (map-elt copy key) (magit-dash-review-json-deep-copy val)))
              value)
      copy))
   ((vectorp value) (vconcat (seq-map #'magit-dash-review-json-deep-copy value)))
   ((stringp value) (copy-sequence value))
   ((proper-list-p value) (seq-map #'magit-dash-review-json-deep-copy value))
   ((consp value)
    (cons (magit-dash-review-json-deep-copy (car value))
          (magit-dash-review-json-deep-copy (cdr value))))
   (t value)))

(defun magit-dash-review-json-make-id (thread-id)
  "Generate a comment id from THREAD-ID string.
For a non-empty THREAD-ID the id is the first 12 hex digits (48 bits) of
its SHA-1, stable across sessions and Emacs versions (`sxhash' is neither).
Otherwise it is 12 random hex digits."
  (if (and thread-id (not (string-empty-p thread-id)))
      (substring (secure-hash 'sha1 thread-id) 0 12)
    (format "%012x" (random (ash 1 48)))))

(defun magit-dash-review-json-resolve-blob (root rev path)
  "Resolve the git blob SHA for PATH at REV in repository at ROOT, or nil."
  (when (and root rev path (file-directory-p root))
    (let ((default-directory (file-name-as-directory (expand-file-name root))))
      (with-temp-buffer
        (when (= 0 (call-process "git" nil t nil "rev-parse" (format "%s:%s" rev path)))
          (string-trim (buffer-string)))))))

(defun magit-dash-review-json--empty-p (value)
  "Return non-nil when VALUE is nil or the empty string."
  (or (null value) (equal value "")))

(defun magit-dash-review-json--vectorize (value)
  "Return VALUE with every non-empty proper list below it turned into a vector.
The top level of a list VALUE (the comments list) is kept as a list; JSON
only serializes vectors, so export normalizes whatever the seam returned.
Hash tables are modified in place."
  (cond
   ((hash-table-p value)
    (map-do (lambda (key val)
              (setf (map-elt value key) (magit-dash-review-json--vectorize-nested val)))
            value)
    value)
   ((proper-list-p value) (seq-map #'magit-dash-review-json--vectorize value))
   (t value)))

(defun magit-dash-review-json--vectorize-nested (value)
  "Vectorize VALUE, turning a non-empty proper list into a vector."
  (cond
   ((and value (proper-list-p value))
    (vconcat (seq-map #'magit-dash-review-json--vectorize-nested value)))
   ((vectorp value) (vconcat (seq-map #'magit-dash-review-json--vectorize-nested value)))
   (t (magit-dash-review-json--vectorize value))))

(defun magit-dash-review-json--comments (table)
  "Return the comments of TABLE as a list."
  (append (or (map-elt table "comments") []) nil))

(defun magit-dash-review-json-pr-key (table)
  "Return (REPO . NUMBER) from the magit_dash key of TABLE, or nil."
  (let ((md (map-elt table "magit_dash")))
    (when (and md (map-elt md "repo") (map-elt md "number"))
      (cons (map-elt md "repo") (map-elt md "number")))))

;;; Matching

(defun magit-dash-review-json-content-hash (comment)
  "Return a hash of the path, all line numbers and body of COMMENT."
  (secure-hash 'sha1
               (mapconcat (lambda (part) (format "%s" part))
                          (list (or (map-elt comment "path") "")
                                (mapconcat (lambda (n) (format "%s" n))
                                           (append (map-elt comment "lines") nil)
                                           ",")
                                (or (map-elt comment "body") ""))
                          "\0")))

(defun magit-dash-review-json-find-match (comment candidates)
  "Return the member of CANDIDATES that is the same thread as COMMENT, or nil.
Tiers, first hit wins: GitHub node id (remote_id or gh_comment_id), cr_id,
gh_thread_id, then content hash of path, line and body.  A candidate whose
gh_thread_id differs from COMMENT's never matches.  The hash tier skips
candidates with origin local or agent unless COMMENT has that origin, so
locally authored comments are never absorbed by a GitHub thread."
  (let* ((thread-id (map-elt comment "gh_thread_id"))
         (node-ids (seq-remove #'magit-dash-review-json--empty-p
                               (list (map-elt comment "remote_id")
                                     (map-elt comment "gh_comment_id"))))
         (cr-id (map-elt comment "cr_id"))
         (hash (and (not (magit-dash-review-json--empty-p (map-elt comment "body")))
                    (magit-dash-review-json-content-hash comment)))
         (eligible (seq-remove
                    (lambda (cand)
                      (let ((other (map-elt cand "gh_thread_id")))
                        (and (not (magit-dash-review-json--empty-p thread-id))
                             (not (magit-dash-review-json--empty-p other))
                             (not (equal thread-id other)))))
                    candidates)))
    (seq-some
     (lambda (tier) (seq-find tier eligible))
     (list (lambda (cand)
             (seq-some (lambda (id) (member id (list (map-elt cand "remote_id")
                                                     (map-elt cand "gh_comment_id"))))
                       node-ids))
           (lambda (cand) (and cr-id (equal cr-id (map-elt cand "cr_id"))))
           (lambda (cand)
             (and (not (magit-dash-review-json--empty-p thread-id))
                  (equal thread-id (map-elt cand "gh_thread_id"))))
           (lambda (cand)
             (and hash
                  (or (not (member (map-elt cand "origin") '("local" "agent")))
                      (equal (map-elt cand "origin") (map-elt comment "origin")))
                  (equal hash (magit-dash-review-json-content-hash cand))))))))

;;; Transcoding and merge

(defun magit-dash-review-json-transcode-thread (node root head-sha base-sha)
  "Transcode a GraphQL reviewThread NODE into a diff-review/1 comment hash table.
ROOT is the repository root directory.
HEAD-SHA and BASE-SHA are the commit SHAs for resolving blob hashes."
  (let* ((thread-id (or (map-elt node 'id) (map-elt node :id) ""))
         (is-resolved (or (eq (map-elt node 'isResolved) t)
                          (eq (map-elt node :resolved) t)))
         (is-outdated (or (eq (map-elt node 'isOutdated) t)
                          (eq (map-elt node :outdated) t)))
         (path (or (map-elt node 'path) (map-elt node :path) ""))
         (line (or (map-elt node 'line) (map-elt node :line)))
         (start-line (or (map-elt node 'startLine) (map-elt node :start_line)))
         (orig-line (or (map-elt node 'originalLine) (map-elt node :original_line)))
         (orig-start-line (or (map-elt node 'originalStartLine) (map-elt node :original_start_line)))
         (diff-side (or (map-elt node 'diffSide) (map-elt node :diff_side) "RIGHT"))
         (eff-line (or line orig-line))
         (eff-start-line (or start-line orig-start-line eff-line))
         (lines (when eff-line
                  (vector (min eff-start-line eff-line) (max eff-start-line eff-line))))
         (comments-data (or (map-nested-elt node '(comments nodes))
                            (map-elt node :comments)))
         (comments-list (seq-into comments-data 'list))
         (first-c (car comments-list))
         (first-id (or (map-elt first-c 'id) (map-elt first-c :id) ""))
         (first-commit-oid (or (map-nested-elt first-c '(commit oid))
                               (map-elt first-c :commit_oid)))
         (rev (cond ((equal diff-side "LEFT") (or base-sha head-sha))
                    (first-commit-oid first-commit-oid)
                    (t head-sha)))
         (blob (and (not (string-empty-p path))
                    (magit-dash-review-json-resolve-blob root rev path)))
         (comment-ht (make-hash-table :test #'equal))
         (replies (seq-map
                   (lambda (c)
                     (let ((r-ht (make-hash-table :test #'equal))
                           (c-id (or (map-elt c 'id) (map-elt c :id) "")))
                       (setf (map-elt r-ht "id") (magit-dash-review-json-make-id c-id))
                       (unless (string-empty-p c-id)
                         (setf (map-elt r-ht "gh_comment_id") c-id))
                       (setf (map-elt r-ht "author") (or (map-nested-elt c '(author login))
                                                         (map-elt c :author)
                                                         "unknown"))
                       (when-let* ((at (or (map-elt c 'createdAt) (map-elt c :created_at))))
                         (setf (map-elt r-ht "at") at))
                       (setf (map-elt r-ht "body") (or (map-elt c 'body) (map-elt c :body) ""))
                       r-ht))
                   (cdr comments-list))))
    (setf (map-elt comment-ht "id") (magit-dash-review-json-make-id thread-id))
    (setf (map-elt comment-ht "gh_thread_id") thread-id)
    (setf (map-elt comment-ht "gh_comment_id") first-id)
    (setf (map-elt comment-ht "gh_resolved")
          (if is-resolved
              t
            :false))
    (setf (map-elt comment-ht "origin") "github")
    (unless (string-empty-p path)
      (setf (map-elt comment-ht "path") path))
    (when lines
      (setf (map-elt comment-ht "lines") lines))
    (when blob
      (setf (map-elt comment-ht "blob") blob))
    (setf (map-elt comment-ht "author") (or (map-nested-elt first-c '(author login))
                                            (map-elt first-c :author)
                                            (map-elt node :author)
                                            "unknown"))
    (when-let* ((at (or (map-elt first-c 'createdAt) (map-elt first-c :created_at))))
      (setf (map-elt comment-ht "at") at))
    (setf (map-elt comment-ht "body") (or (map-elt first-c 'body)
                                          (map-elt first-c :body)
                                          (map-elt node :body)
                                          ""))
    (setf (map-elt comment-ht "resolved")
          (if is-resolved
              t
            :false))
    (when is-outdated
      (setf (map-elt comment-ht "outdated") t))
    (setf (map-elt comment-ht "replies") (vconcat replies))
    comment-ht))

(defun magit-dash-review-json-merge-thread (local remote)
  "Merge the REMOTE thread into a copy of the LOCAL thread and return it.
Keys in `magit-dash-review-json--remote-keys' take the remote value, and are
removed when REMOTE omits them (a thread that stops being outdated clears).
Every other key keeps the local value (id, cr_id, remote_id, status and
unknown keys), falling back to the remote one when local has none.  Resolved
is sticky: once either side says resolved it stays resolved, so a thread
re-opened on GitHub stays resolved locally until the local file is edited.
Replies are all remote replies, then unpublished local ones (no
gh_comment_id), then published local replies the remote list lacks, kept
with gh_missing t because the remote fetch may be truncated.  The result
shares nothing with LOCAL or REMOTE."
  (let ((merged (magit-dash-review-json-deep-copy local)))
    (seq-do (lambda (key)
              (unless (map-contains-key remote key)
                (map-delete merged key)))
            magit-dash-review-json--remote-keys)
    (map-do (lambda (key value)
              (when (or (member key magit-dash-review-json--remote-keys)
                        (not (map-contains-key merged key)))
                (setf (map-elt merged key) (magit-dash-review-json-deep-copy value))))
            remote)
    (setf (map-elt merged "resolved")
          (if (or (eq (map-elt remote "resolved") t)
                  (eq (map-elt local "resolved") t))
              t
            :false))
    (let* ((remote-ids (seq-map (lambda (reply) (map-elt reply "gh_comment_id"))
                                (map-elt remote "replies")))
           (local-replies (append (map-elt local "replies") nil))
           (missing (seq-filter
                     (lambda (reply)
                       (and (not (magit-dash-review-json--empty-p (map-elt reply "gh_comment_id")))
                            (not (member (map-elt reply "gh_comment_id") remote-ids))))
                     local-replies))
           (flagged (seq-map (lambda (reply)
                               (let ((copy (magit-dash-review-json-deep-copy reply)))
                                 (setf (map-elt copy "gh_missing") t)
                                 copy))
                             missing)))
      (setf (map-elt merged "replies")
            (magit-dash-review-json-deep-copy
             (vconcat (map-elt remote "replies")
                      (seq-filter (lambda (reply)
                                    (magit-dash-review-json--empty-p (map-elt reply "gh_comment_id")))
                                  local-replies)
                      flagged))))
    merged))

(defun magit-dash-review-json-merge (existing-ht remote-metadata remote-threads)
  "Merge EXISTING-HT with REMOTE-METADATA and REMOTE-THREADS.
Return a new diff-review/1 table.  Unknown top-level keys (code_review and
others), unknown magit_dash keys, viewed and verdict are preserved.  Remote
threads are matched to local ones with `magit-dash-review-json-find-match',
each local thread at most once; unmatched remote threads are added and
unmatched local threads are kept whole.  Local order is preserved (a merged
thread keeps its position) and new remote threads are appended.  When
EXISTING-HT belongs to a different repo than REMOTE-METADATA, it is ignored
\(message) and a fresh table is built.  The result is fully independent
of EXISTING-HT and REMOTE-THREADS: mutating it changes neither."
  (let* ((incoming-repo (or (map-elt remote-metadata 'repo) (map-elt remote-metadata :repo)))
         (existing-repo (map-elt (map-elt existing-ht "magit_dash") "repo"))
         (foreign (and existing-ht
                       (not (magit-dash-review-json--empty-p incoming-repo))
                       (not (magit-dash-review-json--empty-p existing-repo))
                       (not (equal incoming-repo existing-repo))))
         (result (cond
                  (foreign
                   (message "magit-dash-review-json: existing review is for %s, not %s; starting fresh"
                            existing-repo incoming-repo)
                   (make-hash-table :test #'equal))
                  (existing-ht (magit-dash-review-json-deep-copy existing-ht))
                  (t (make-hash-table :test #'equal))))
         (md (if (hash-table-p (map-elt result "magit_dash"))
                 (map-elt result "magit_dash")
               (make-hash-table :test #'equal)))
         (state (seq-reduce
                 (lambda (acc remote-c)
                   (let ((local-c (magit-dash-review-json-find-match remote-c (cdr acc))))
                     (cons (cons (cons local-c
                                       (if local-c
                                           (magit-dash-review-json-merge-thread local-c remote-c)
                                         (magit-dash-review-json-deep-copy remote-c)))
                                 (car acc))
                           (seq-remove (lambda (c) (eq c local-c)) (cdr acc)))))
                 (seq-into remote-threads 'list)
                 (cons nil (magit-dash-review-json--comments result)))))
    (setf (map-elt md "provider") "github")
    (seq-do (lambda (spec)
              (when-let* ((value (or (map-elt remote-metadata (nth 1 spec))
                                     (map-elt remote-metadata (nth 2 spec)))))
                (setf (map-elt md (car spec)) value)))
            `(("repo" repo :repo) ("number" number :number) ("url" url :url)
              ("head_oid" headRefOid :head_oid) ("base_oid" baseRefOid :base_oid)))
    (setf (map-elt md "synced_at") (format-time-string "%FT%T%:z"))
    (setf (map-elt result "format") "diff-review/1")
    (when-let* ((worktree (or (map-elt remote-metadata 'worktree)
                              (map-elt remote-metadata :worktree)
                              (map-elt result "root"))))
      (setf (map-elt md "worktree") worktree)
      (setf (map-elt result "root") (file-name-as-directory (expand-file-name worktree))))
    (when-let* ((base-ref (or (map-elt remote-metadata 'baseRefName)
                              (map-elt remote-metadata :base-ref)
                              (map-elt result "base"))))
      (setf (map-elt result "base") base-ref))
    (when-let* ((head-ref (or (map-elt remote-metadata 'headRefName)
                              (map-elt remote-metadata :head-ref)
                              (map-elt result "head"))))
      (setf (map-elt result "head") head-ref))
    (setf (map-elt result "magit_dash") md)
    (unless (hash-table-p (map-elt result "viewed"))
      (setf (map-elt result "viewed") (make-hash-table :test #'equal)))
    (let ((pairs (nreverse (car state))))
      (setf (map-elt result "comments")
            (vconcat (seq-map (lambda (local-c)
                                (or (cdr (seq-find (lambda (pair) (eq (car pair) local-c)) pairs))
                                    local-c))
                              (magit-dash-review-json--comments result))
                     (seq-map #'cdr (seq-remove #'car pairs)))))
    result))

;;; Export and import

(defun magit-dash-review-json-export (pr-key)
  "Return the diff-review/1 hash table for PR-KEY, (REPO . NUMBER).
Starts from the review file (`magit-dash-review-json-source-function'),
then overlays code-review's cr_id, remote_id and status onto matching
threads (never overwriting local values), appends code-review comments
with no match, and fills missing code_review keys and the magit_dash repo
and number from PR-KEY, so the result can be imported.  Never mutates the
source table."
  (let* ((source (funcall magit-dash-review-json-source-function pr-key))
         (table (if source
                    (magit-dash-review-json-deep-copy source)
                  (make-hash-table :test #'equal)))
         (stored (magit-dash-review-json--vectorize
                  (funcall magit-dash-review-json-code-review-function 'comments pr-key)))
         (pullreq (magit-dash-review-json--vectorize
                   (funcall magit-dash-review-json-code-review-function 'pullreq pr-key)))
         (state (seq-reduce
                 (lambda (acc stored-c)
                   (let ((local-c (magit-dash-review-json-find-match stored-c (car acc))))
                     (cond
                      (local-c
                       (seq-do (lambda (key)
                                 (when (and (magit-dash-review-json--empty-p (map-elt local-c key))
                                            (map-elt stored-c key))
                                   (setf (map-elt local-c key) (map-elt stored-c key))))
                               '("cr_id" "remote_id"))
                       (when (and (not (magit-dash-review-json--empty-p (map-elt stored-c "status")))
                                  (not (equal (map-elt stored-c "status") (map-elt local-c "status"))))
                         (setf (map-elt local-c "status") (map-elt stored-c "status")))
                       (cons (seq-remove (lambda (c) (eq c local-c)) (car acc)) (cdr acc)))
                      (t
                       (let ((new-c (magit-dash-review-json-deep-copy stored-c)))
                         (when (magit-dash-review-json--empty-p (map-elt new-c "id"))
                           (setf (map-elt new-c "id")
                                 (magit-dash-review-json-make-id
                                  (format "%s" (or (map-elt new-c "remote_id")
                                                   (map-elt new-c "cr_id"))))))
                         (when (magit-dash-review-json--empty-p (map-elt new-c "origin"))
                           (setf (map-elt new-c "origin") "code-review"))
                         (cons (car acc) (cons new-c (cdr acc))))))))
                 stored
                 (cons (magit-dash-review-json--comments table) nil)))
         (code-review (or (map-elt table "code_review")
                          (make-hash-table :test #'equal))))
    (setf (map-elt table "comments")
          (vconcat (magit-dash-review-json--comments table) (nreverse (cdr state))))
    (map-do (lambda (key value)
              (when (magit-dash-review-json--empty-p (map-elt code-review key))
                (setf (map-elt code-review key) value)))
            pullreq)
    (when (> (map-length code-review) 0)
      (setf (map-elt table "code_review") code-review))
    (unless (map-elt table "format")
      (setf (map-elt table "format") "diff-review/1"))
    (let ((md (if (hash-table-p (map-elt table "magit_dash"))
                  (map-elt table "magit_dash")
                (make-hash-table :test #'equal))))
      (when (magit-dash-review-json--empty-p (map-elt md "repo"))
        (setf (map-elt md "repo") (car pr-key)))
      (when (magit-dash-review-json--empty-p (map-elt md "number"))
        (setf (map-elt md "number") (cdr pr-key)))
      (setf (map-elt table "magit_dash") md))
    (seq-do (lambda (key)
              (unless (map-elt table key)
                (setf (map-elt table key) "")))
            '("root" "base"))
    (unless (map-elt table "viewed")
      (setf (map-elt table "viewed") (make-hash-table :test #'equal)))
    table))

(defun magit-dash-review-json--reply-content (comment)
  "Return the (author at body) of each reply of COMMENT as a list."
  (seq-map (lambda (reply)
             (list (map-elt reply "author") (map-elt reply "at") (map-elt reply "body")))
           (map-elt comment "replies")))

(defun magit-dash-review-json-apply-assigned (table result)
  "Set cr_id on the comments of TABLE that RESULT says code-review numbered.
RESULT is the plist `magit-dash-review-json-import' returned for TABLE.
Mutates and returns TABLE.  Persisting TABLE afterwards makes the next
import match those comments by cr_id instead of adding them again."
  (seq-do (lambda (pair)
            (setf (map-elt (elt (map-elt table "comments") (car pair)) "cr_id") (cdr pair)))
          (plist-get result :assigned))
  table)

(defun magit-dash-review-json-import (table)
  "Mirror the comments and verdict of TABLE into code-review.
TABLE is a diff-review/1 table with magit_dash repo and number.  Returns
\(:added N :updated N :skipped N :verdict-failed N :assigned ALIST).
ALIST holds (INDEX . CR-ID) for each comment of TABLE that has no cr_id but
is stored in code-review under one; see `magit-dash-review-json-apply-assigned'.
Comments are matched with `magit-dash-review-json-find-match'.  A comment is
unchanged, and skipped, when body, resolved state and reply content (author,
time, body) equal the stored ones; status is ignored because code-review
assigns it.  A write that signals leaves the comment counted as skipped and
TABLE untouched, so drafts stay drafts.  The verdict is written only when it
differs from the stored one; a failed write counts in :verdict-failed.  TABLE
is never mutated."
  (let* ((pr-key (or (magit-dash-review-json-pr-key table)
                     (user-error "Review JSON has no magit_dash repo and number")))
         (fn magit-dash-review-json-code-review-function)
         (counts (list :added 0 :updated 0 :skipped 0 :verdict-failed 0))
         (fetch-failed nil)
         (remaining (condition-case err
                        (funcall fn 'comments pr-key)
                      (error (message "magit-dash-review-json: cannot read comments: %s"
                                      (error-message-string err))
                             (setq fetch-failed t)
                             nil)))
         (assigned nil))
    (seq-do-indexed
     (lambda (comment index)
       (let* ((match (magit-dash-review-json-find-match comment remaining))
              (kind (cond
                     ((null match) :added)
                     ((and (equal (map-elt comment "body") (map-elt match "body"))
                           (eq (eq (map-elt comment "resolved") t)
                               (eq (map-elt match "resolved") t))
                           (equal (magit-dash-review-json--reply-content comment)
                                  (magit-dash-review-json--reply-content match)))
                      nil)
                     (t :updated)))
              (written (when (and kind (not fetch-failed))
                         (let ((payload (magit-dash-review-json-deep-copy comment)))
                           (when (and match (map-elt match "cr_id"))
                             (setf (map-elt payload "cr_id") (map-elt match "cr_id")))
                           (condition-case err
                               (funcall fn 'write pr-key payload)
                             (error (message "magit-dash-review-json: %s" (error-message-string err))
                                    nil)))))
              (key (if (and kind written) kind :skipped))
              (cr-id (or (and (hash-table-p written) (map-elt written "cr_id"))
                         (map-elt match "cr_id"))))
         (setq counts (plist-put counts key (1+ (plist-get counts key))))
         (when (and cr-id (magit-dash-review-json--empty-p (map-elt comment "cr_id")))
           (push (cons index cr-id) assigned))
         (setq remaining (seq-remove (lambda (c) (eq c match)) remaining))))
     (magit-dash-review-json--comments table))
    (when-let* ((verdict (map-elt table "verdict")))
      (condition-case err
          (let ((current (funcall fn 'verdict pr-key)))
            (unless (and current
                         (= (map-length verdict) (map-length current))
                         (map-every-p (lambda (k v) (equal v (map-elt current k))) verdict))
              (unless (funcall fn 'verdict pr-key verdict)
                (error "verdict write returned nil"))))
        (error (message "magit-dash-review-json: %s" (error-message-string err))
               (setq counts (plist-put counts :verdict-failed 1)))))
    (plist-put counts :assigned (nreverse assigned))))

(provide 'magit-dash-review-json)
;;; magit-dash-review-json.el ends here
