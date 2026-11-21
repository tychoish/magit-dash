;;; magit-dash-review.el --- Native buffer diff review integration for magit-dash -*- lexical-binding: t; -*-

;; Author: sam kleinman
;; Maintainer: tychoish
;; Version: 0.1
;; Keywords: vc, git, review, github

;;; Commentary:

;; Orchestrates native-buffer code review via diff-review and code-review,
;; manages review JSON hydration, 3-way merge, GitHub submission,
;; worktrees, agent remediation, and ACR selection.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'json)

(require 'magit-dash-gh)
(require 'magit-dash-pr)
(require 'magit-dash-worktree)

;; External optional interfaces
(defvar diff-review--root)
(defvar diff-review--base)
(defvar diff-review--base-name)
(defvar diff-review--head)
(defvar diff-review--data)
(defvar diff-review--comments)
(defvar diff-review--file-path)
(defvar diff-review--hunks)
(defvar diff-review-file-mode)

(declare-function diff-review-find-file "diff-review" (file &optional base root head))
(declare-function diff-review-overview "diff-review" ())
(declare-function diff-review-refresh "diff-review" ())
(declare-function diff-review--common-dir "diff-review" (root))
(declare-function diff-review--toplevel "diff-review" (&optional directory))
(declare-function diff-review--modify-comments "diff-review" (fn))
(declare-function diff-review--review-buffer "diff-review" ())
(declare-function diff-review--region-lines "diff-review" ())
(declare-function diff-review--threads-at-point "diff-review" ())

(defgroup magit-dash-review nil
  "Diff-review and code-review integration for magit-dash."
  :group 'magit-dash)

(defcustom magit-dash-review-agent-auto-resolve nil
  "When non-nil, agent remediation marks review threads resolved automatically.
When nil (the default), agent replies are appended leaving the thread open
for human verification and sign-off."
  :type 'boolean
  :group 'magit-dash-review)

;;; Core Path & Git Helpers

(defun magit-dash-review--common-dir (root)
  "Return the common git directory for repository at ROOT."
  (or (and (fboundp 'diff-review--common-dir)
           (diff-review--common-dir root))
      (let ((default-directory (file-name-as-directory (expand-file-name root))))
        (file-name-as-directory
         (file-truename
          (expand-file-name
           (string-trim
            (with-temp-buffer
              (call-process "git" nil t nil "rev-parse" "--git-common-dir")
              (buffer-string)))
           default-directory))))))

(defun magit-dash-review-file (root pr-number)
  "Return path to the review JSON file for PR-NUMBER in repository at ROOT.
The file is located under <git-common-dir>/review/pr-<num>.json."
  (let* ((common-dir (magit-dash-review--common-dir root))
         (review-dir (expand-file-name "review" common-dir)))
    (expand-file-name (format "pr-%s.json" pr-number) review-dir)))

(defun magit-dash-review--resolve-blob (root rev path)
  "Resolve the git blob SHA for PATH at REV in repository at ROOT.
Returns the 40-character hex SHA string, or nil if resolution fails."
  (when (and root rev path (file-directory-p root))
    (let ((default-directory (file-name-as-directory (expand-file-name root))))
      (with-temp-buffer
        (let ((code (call-process "git" nil t nil "rev-parse"
                                  (format "%s:%s" rev path))))
          (when (= code 0)
            (string-trim (buffer-string))))))))

;;; Transcoding and Three-Way Merge

(defun magit-dash-review--make-comment-id (thread-id)
  "Generate a deterministic 6-character hex comment id from THREAD-ID string."
  (if (and thread-id (not (string-empty-p thread-id)))
      (format "%06x" (logand (sxhash thread-id) #xFFFFFF))
    (format "%06x" (random #x1000000))))

(defun magit-dash-review--transcode-thread (node root head-sha base-sha)
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
                  (let ((l1 (min eff-start-line eff-line))
                        (l2 (max eff-start-line eff-line)))
                    (vector l1 l2))))
         (comments-data (or (map-nested-elt node '(comments nodes))
                            (map-elt node :comments)
                            nil))
         (comments-list (if (vectorp comments-data)
                            (append comments-data nil)
                          comments-data))
         (first-c (car comments-list))
         (first-author (or (map-nested-elt first-c '(author login))
                           (map-elt first-c :author)
                           (map-elt node :author)
                           "unknown"))
         (first-body (or (map-elt first-c 'body)
                         (map-elt first-c :body)
                         (map-elt node :body)
                         ""))
         (first-at (or (map-elt first-c 'createdAt)
                       (map-elt first-c :created_at)
                       (format-time-string "%FT%T%:z")))
         (first-id (or (map-elt first-c 'id)
                       (map-elt first-c :id)
                       ""))
         (first-commit-oid (or (map-nested-elt first-c '(commit oid))
                               (map-elt first-c :commit_oid)))
         (rev (cond ((equal diff-side "LEFT") (or base-sha head-sha))
                    (first-commit-oid first-commit-oid)
                    (t head-sha)))
         (blob (and path (not (string-empty-p path))
                    (magit-dash-review--resolve-blob root rev path)))
         (comment-ht (make-hash-table :test #'equal))
         (replies (mapcar
                   (lambda (c)
                     (let ((r-ht (make-hash-table :test #'equal))
                           (c-id (or (map-elt c 'id) (map-elt c :id) ""))
                           (c-author (or (map-nested-elt c '(author login))
                                         (map-elt c :author)
                                         "unknown"))
                           (c-body (or (map-elt c 'body) (map-elt c :body) ""))
                           (c-at (or (map-elt c 'createdAt)
                                     (map-elt c :created_at)
                                     (format-time-string "%FT%T%:z"))))
                       (setf (map-elt r-ht "id") (magit-dash-review--make-comment-id c-id))
                       (when (not (string-empty-p c-id))
                         (setf (map-elt r-ht "gh_comment_id") c-id))
                       (setf (map-elt r-ht "author") c-author)
                       (setf (map-elt r-ht "at") c-at)
                       (setf (map-elt r-ht "body") c-body)
                       r-ht))
                   (cdr comments-list))))
    (setf (map-elt comment-ht "id") (magit-dash-review--make-comment-id thread-id))
    (setf (map-elt comment-ht "gh_thread_id") thread-id)
    (setf (map-elt comment-ht "gh_comment_id") first-id)
    (setf (map-elt comment-ht "gh_resolved") (if is-resolved t :false))
    (setf (map-elt comment-ht "origin") "github")
    (when (and path (not (string-empty-p path)))
      (setf (map-elt comment-ht "path") path))
    (when lines
      (setf (map-elt comment-ht "lines") lines))
    (when blob
      (setf (map-elt comment-ht "blob") blob))
    (setf (map-elt comment-ht "author") first-author)
    (setf (map-elt comment-ht "at") first-at)
    (setf (map-elt comment-ht "body") first-body)
    (setf (map-elt comment-ht "resolved") (if is-resolved t :false))
    (when is-outdated
      (setf (map-elt comment-ht "outdated") t))
    (setf (map-elt comment-ht "replies") (vconcat replies))
    comment-ht))

(defun magit-dash-review--merge-json (existing-ht remote-metadata remote-threads)
  "Merge EXISTING-HT with REMOTE-METADATA and REMOTE-THREADS.
Returns a newly constructed diff-review/1 hash table preserving local
unsynced threads, local replies, viewed marks, and verdict."
  (let* ((result (make-hash-table :test #'equal))
         (worktree (or (map-elt remote-metadata 'worktree)
                       (map-elt remote-metadata :worktree)
                       (and existing-ht (map-elt existing-ht "root"))))
         (head-ref (or (map-elt remote-metadata 'headRefName)
                       (map-elt remote-metadata :head-ref)
                       (and existing-ht (map-elt existing-ht "head"))))
         (base-ref (or (map-elt remote-metadata 'baseRefName)
                       (map-elt remote-metadata :base-ref)
                       (and existing-ht (map-elt existing-ht "base"))))
         (repo (or (map-elt remote-metadata 'repo)
                   (map-elt remote-metadata :repo)))
         (pr-number (or (map-elt remote-metadata 'number)
                        (map-elt remote-metadata :number)))
         (url (or (map-elt remote-metadata 'url)
                  (map-elt remote-metadata :url)))
         (head-oid (or (map-elt remote-metadata 'headRefOid)
                       (map-elt remote-metadata :head_oid)))
         (base-oid (or (map-elt remote-metadata 'baseRefOid)
                       (map-elt remote-metadata :base_oid)))
         (md (make-hash-table :test #'equal))
         (existing-comments (if existing-ht
                                (append (or (map-elt existing-ht "comments") []) nil)
                              nil))
         (existing-by-gh-id (make-hash-table :test #'equal))
         (existing-viewed (or (and existing-ht (map-elt existing-ht "viewed"))
                              (make-hash-table :test #'equal)))
         (existing-verdict (and existing-ht (map-elt existing-ht "verdict")))
         (merged-comments nil)
         (seen-local-ids (make-hash-table :test #'equal)))

    ;; Index existing comments by gh_thread_id
    (dolist (c existing-comments)
      (let ((gh-id (map-elt c "gh_thread_id")))
        (when (and gh-id (not (string-empty-p gh-id)))
          (setf (map-elt existing-by-gh-id gh-id) c))))

    ;; Build magit_dash metadata table
    (setf (map-elt md "provider") "github")
    (when repo (setf (map-elt md "repo") repo))
    (when pr-number (setf (map-elt md "number") pr-number))
    (when url (setf (map-elt md "url") url))
    (when worktree (setf (map-elt md "worktree") worktree))
    (when head-oid (setf (map-elt md "head_oid") head-oid))
    (when base-oid (setf (map-elt md "base_oid") base-oid))
    (setf (map-elt md "synced_at") (format-time-string "%FT%T%:z"))

    ;; Process remote threads
    (dolist (remote-c (if (vectorp remote-threads) (append remote-threads nil) remote-threads))
      (let* ((gh-id (map-elt remote-c "gh_thread_id"))
             (local-c (and gh-id (map-elt existing-by-gh-id gh-id))))
        (if (not local-c)
            ;; Brand new remote thread
            (progn
              (setf (map-elt seen-local-ids (map-elt remote-c "id")) t)
              (push remote-c merged-comments))
          ;; Merge existing local thread with remote thread
          (let* ((merged-c (make-hash-table :test #'equal))
                 (local-replies (append (or (map-elt local-c "replies") []) nil))
                 (remote-replies (append (or (map-elt remote-c "replies") []) nil))
                 ;; Local replies not yet published (have no gh_comment_id)
                 (pending-local-replies (seq-filter (lambda (r)
                                                      (let ((gid (map-elt r "gh_comment_id")))
                                                        (or (null gid) (string-empty-p gid))))
                                                    local-replies))
                 ;; All remote replies + any local pending replies
                 (all-replies (vconcat remote-replies pending-local-replies)))
            ;; Copy remote properties
            (map-do (lambda (k v) (setf (map-elt merged-c k) v)) remote-c)
            ;; Retain existing local ID to preserve external references
            (setf (map-elt merged-c "id") (map-elt local-c "id"))
            (setf (map-elt seen-local-ids (map-elt local-c "id")) t)
            ;; Update resolution: if remote is resolved, mark resolved; else respect local
            (let ((remote-res (map-elt remote-c "resolved"))
                  (local-res (map-elt local-c "resolved")))
              (setf (map-elt merged-c "resolved")
                    (cond ((eq remote-res t) t)
                          ((eq local-res t) t)
                          (t :false))))
            (setf (map-elt merged-c "replies") all-replies)
            (push merged-c merged-comments)))))

    ;; Retain local comments not originating from GitHub or without gh_thread_id
    (dolist (local-c existing-comments)
      (let* ((loc-id (map-elt local-c "id"))
             (gh-id (map-elt local-c "gh_thread_id"))
             (origin (map-elt local-c "origin")))
        (when (or (null gh-id)
                  (string-empty-p gh-id)
                  (equal origin "local")
                  (equal origin "agent")
                  (not (map-elt seen-local-ids loc-id)))
          (unless (map-elt seen-local-ids loc-id)
            (setf (map-elt seen-local-ids loc-id) t)
            (push local-c merged-comments)))))

    ;; Populate result root
    (setf (map-elt result "format") "diff-review/1")
    (when worktree (setf (map-elt result "root") (file-name-as-directory (expand-file-name worktree))))
    (when base-ref (setf (map-elt result "base") base-ref))
    (when head-ref (setf (map-elt result "head") head-ref))
    (setf (map-elt result "magit_dash") md)
    (setf (map-elt result "viewed") existing-viewed)
    (when existing-verdict
      (setf (map-elt result "verdict") existing-verdict))
    (setf (map-elt result "comments") (vconcat (nreverse merged-comments)))
    result))

;;; JSON File I/O Helpers

(defun magit-dash-review--read-json-file (review-file)
  "Parse REVIEW-FILE as a diff-review/1 JSON hash table."
  (unless (and review-file (file-readable-p review-file))
    (error "Review file not readable: %s" review-file))
  (with-temp-buffer
    (insert-file-contents review-file)
    (json-parse-string (buffer-string)
                       :object-type 'hash-table
                       :array-type 'array
                       :null-object nil
                       :false-object :false)))

(defun magit-dash-review--write-json-file (review-file data)
  "Serialize DATA hash table to REVIEW-FILE as pretty-printed JSON."
  (make-directory (file-name-directory (expand-file-name review-file)) t)
  (let ((json (json-serialize data :null-object nil :false-object :false)))
    (with-temp-file review-file
      (insert json)
      (json-pretty-print-buffer)
      (insert "\n")))
  review-file)

(defun magit-dash-review--current-review-file ()
  "Return the review JSON file path for the current diff-review buffer, or nil."
  (cond
   ((and (derived-mode-p 'diff-review-mode) buffer-file-name)
    buffer-file-name)
   ((and (fboundp 'diff-review--review-buffer)
         (diff-review--review-buffer))
    (buffer-file-name (diff-review--review-buffer)))
   (t nil)))

;;; Diff Hunk Extraction, Finding Extraction, and Safe Reply Writer

(defun magit-dash-review--filter-diff-hunks (diff-text lines)
  "Filter unified DIFF-TEXT to hunks overlapping LINES ([first last]).
If LINES is nil or no specific hunk matches, return DIFF-TEXT."
  (if (or (null lines) (string-empty-p diff-text) (< (length lines) 2))
      diff-text
    (let* ((first-line (elt lines 0))
           (last-line (elt lines 1))
           (raw-parts (split-string diff-text "^@@ -" nil))
           (header (if (and raw-parts (not (string-prefix-p "@@ -" diff-text)))
                       (pop raw-parts)
                     ""))
           (parts (mapcar (lambda (p) (concat "@@ -" p)) raw-parts))
           (matching nil))
      (dolist (hunk parts)
        (if (string-match
             "\\`@@ -[0-9]+\\(?:,[0-9]+\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@"
             hunk)
            (let* ((start (string-to-number (match-string 1 hunk)))
                   (count (if (match-string 2 hunk)
                              (string-to-number (match-string 2 hunk))
                            1))
                   (end (+ start (max count 1) 5))
                   (beg (max 1 (- start 5))))
              (when (and (<= beg last-line) (>= end first-line))
                (push hunk matching)))
          (push hunk matching)))
      (if matching
          (string-trim (concat header (string-join (nreverse matching) "")))
        (string-trim diff-text)))))

(defun magit-dash-review--extract-file-hunk (root base head path &optional lines)
  "Extract unified diff hunk for PATH between BASE and HEAD in ROOT.
When LINES is a 2-element sequence [first last], narrows to matching hunks."
  (if (not (and root (file-directory-p root) path (not (string-empty-p path))))
      ""
    (let ((default-directory (file-name-as-directory (expand-file-name root)))
          (diff-out ""))
      (with-temp-buffer
        (let* ((range-args (cond
                            ((and base head (not (string-empty-p head)))
                             (list (format "%s...%s" base head)))
                            ((and base (not (string-empty-p base)))
                             (list base))
                            (t nil)))
               (code (apply #'call-process "git" nil t nil
                            "diff" "-U5" (append range-args (list "--" path)))))
          (when (or (/= code 0) (zerop (buffer-size)))
            (erase-buffer)
            (when (and base head (not (string-empty-p head)))
              (call-process "git" nil t nil "diff" "-U5" base head "--" path)))
          (setq diff-out (buffer-string))))
      (magit-dash-review--filter-diff-hunks diff-out lines))))

(defun magit-dash-review-extract-findings (review-file &optional unresolved-only)
  "Extract review findings from REVIEW-FILE (diff-review/1 JSON).
When UNRESOLVED-ONLY is non-nil, only unresolved non-draft threads are
included.  Each finding is returned as a plist with keys:
  :id :gh-thread-id :path :lines :line :end-line :blob :author :body
  :resolved :status :replies :hunk :worktree :root :base :head :review-file."
  (let* ((file (expand-file-name review-file))
         (data (magit-dash-review--read-json-file file))
         (root (map-elt data "root"))
         (base (map-elt data "base"))
         (head (map-elt data "head"))
         (md (map-elt data "magit_dash"))
         (worktree (or (and md (map-elt md "worktree")) root))
         (comments (append (or (map-elt data "comments") []) nil))
         (filtered (if unresolved-only
                       (seq-filter
                        (lambda (c)
                          (and (not (eq (map-elt c "resolved") t))
                               (not (equal (map-elt c "status") "draft"))))
                        comments)
                     comments)))
    (mapcar
     (lambda (c)
       (let* ((path (map-elt c "path"))
              (raw-lines (map-elt c "lines"))
              (lines (cond
                      ((vectorp raw-lines) raw-lines)
                      ((and (listp raw-lines) (>= (length raw-lines) 2))
                       (vector (car raw-lines) (cadr raw-lines)))
                      (t nil)))
              (line (and lines (>= (length lines) 1) (aref lines 0)))
              (end-line (and lines (>= (length lines) 2) (aref lines 1)))
              (replies-vec (or (map-elt c "replies") []))
              (replies (mapcar
                        (lambda (r)
                          (list :id (map-elt r "id")
                                :author (map-elt r "author")
                                :at (map-elt r "at")
                                :body (map-elt r "body")))
                        (append replies-vec nil)))
              (hunk (and path
                         (magit-dash-review--extract-file-hunk
                          (or worktree root) base head path lines))))
         (list :id (map-elt c "id")
               :gh-thread-id (map-elt c "gh_thread_id")
               :path path
               :lines lines
               :line line
               :end-line end-line
               :blob (map-elt c "blob")
               :author (map-elt c "author")
               :body (map-elt c "body")
               :resolved (eq (map-elt c "resolved") t)
               :status (map-elt c "status")
               :replies replies
               :hunk (or hunk "")
               :worktree (or worktree root)
               :root root
               :base base
               :head head
               :review-file file)))
     filtered)))

(defun magit-dash-review-append-reply (review-file comment-id author body &optional resolve)
  "Append a reply by AUTHOR with BODY to COMMENT-ID in REVIEW-FILE.
When RESOLVE is non-nil, or when RESOLVE is nil and
`magit-dash-review-agent-auto-resolve' is non-nil, also marks the thread
resolved.  Returns the updated comment hash table."
  (let* ((file (expand-file-name review-file))
         (should-resolve (if (null resolve)
                             magit-dash-review-agent-auto-resolve
                           resolve))
         (timestamp (format-time-string "%FT%T%:z"))
         (reply-ht (make-hash-table :test #'equal))
         (live-buf (find-buffer-visiting file))
         (updated-comment nil))
    (setf (map-elt reply-ht "id")
          (magit-dash-review--make-comment-id
           (format "%s-%s-%s" comment-id (or author "agent-shell") timestamp)))
    (setf (map-elt reply-ht "author") (or author "agent-shell"))
    (setf (map-elt reply-ht "at") timestamp)
    (setf (map-elt reply-ht "body") body)

    (if (and live-buf
             (with-current-buffer live-buf (derived-mode-p 'diff-review-mode))
             (fboundp 'diff-review--modify-comments))
        (with-current-buffer live-buf
          (diff-review--modify-comments
           (lambda ()
             (let ((target (seq-find
                            (lambda (c)
                              (or (equal (map-elt c "id") comment-id)
                                  (equal (map-elt c "gh_thread_id") comment-id)))
                            diff-review--comments)))
               (unless target
                 (error "No comment with id %s in %s" comment-id file))
               (setf (map-elt target "replies")
                     (vconcat (or (map-elt target "replies") [])
                              (vector reply-ht)))
               (when should-resolve
                 (setf (map-elt target "resolved") t))
               (setq updated-comment target)))))
      (let* ((data (magit-dash-review--read-json-file file))
             (comments (or (map-elt data "comments") []))
             (target (seq-find
                      (lambda (c)
                        (or (equal (map-elt c "id") comment-id)
                            (equal (map-elt c "gh_thread_id") comment-id)))
                      comments)))
        (unless target
          (error "No comment with id %s in %s" comment-id file))
        (setf (map-elt target "replies")
              (vconcat (or (map-elt target "replies") [])
                       (vector reply-ht)))
        (when should-resolve
          (setf (map-elt target "resolved") t))
        (setf (map-elt data "comments") comments)
        (magit-dash-review--write-json-file file data)
        (setq updated-comment target)
        (when (and live-buf (buffer-live-p live-buf))
          (with-current-buffer live-buf
            (revert-buffer t t t)))))
    updated-comment))

(defun magit-dash-review-current-hunk ()
  "Extract review context and diff hunk around point in a diff-review buffer.
Returns a plist (:review-file :root :base :head :path :line :end-line :hunk
:comment-id :removed)."
  (let* ((review-buf (and (fboundp 'diff-review--review-buffer)
                          (diff-review--review-buffer)))
         (review-file (and review-buf (buffer-file-name review-buf)))
         (root (or (and review-buf (buffer-local-value 'diff-review--root review-buf))
                   default-directory))
         (base (and review-buf
                    (or (buffer-local-value 'diff-review--base-name review-buf)
                        (buffer-local-value 'diff-review--base review-buf))))
         (head (and review-buf (buffer-local-value 'diff-review--head review-buf)))
         (path (or (bound-and-true-p diff-review--file-path)
                   (and buffer-file-name root
                        (file-relative-name buffer-file-name root))))
         (line-pair (if (and (fboundp 'diff-review--region-lines)
                             (bound-and-true-p diff-review-file-mode))
                        (diff-review--region-lines)
                      (if (use-region-p)
                          (cons (line-number-at-pos (region-beginning) t)
                                (line-number-at-pos (max (region-beginning) (1- (region-end))) t))
                        (let ((l (line-number-at-pos nil t)))
                          (cons l l)))))
         (first-line (car line-pair))
         (last-line (cdr line-pair))
         (local-hunk (and (bound-and-true-p diff-review--hunks)
                          (seq-find
                           (lambda (h)
                             (let* ((h-start (plist-get h :line))
                                    (h-count (max 1 (or (plist-get h :count) 1)))
                                    (h-end (+ h-start h-count)))
                               (and (<= (max 1 (- h-start 3)) last-line)
                                    (>= (+ h-end 3) first-line))))
                           diff-review--hunks)))
         (removed (and local-hunk (plist-get local-hunk :removed)))
         (threads-here (and (fboundp 'diff-review--threads-at-point)
                            (bound-and-true-p diff-review-file-mode)
                            (diff-review--threads-at-point)))
         (comment-here (and threads-here
                            (overlay-get (car threads-here) 'diff-review-comment)))
         (comment-id (and comment-here (map-elt comment-here "id")))
         (hunk-text (if (and path root)
                        (magit-dash-review--extract-file-hunk
                         root base head path (vector first-line last-line))
                      "")))
    (list :review-file review-file
          :root root
          :base base
          :head head
          :path path
          :line first-line
          :end-line last-line
          :hunk hunk-text
          :comment-id comment-id
          :removed removed)))

(provide 'magit-dash-review)
;;; magit-dash-review.el ends here
