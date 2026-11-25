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
(require 'magit-dash-review-json)
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
(declare-function diff-review-branch "diff-review" (branch))
(declare-function annotated-completing-read "annotated-completing-read" (collection &rest args))
(declare-function agent-shell-workflow-dispatch "agent-shell-workflow" (id &rest args))
(declare-function agent-shell-prompt-queue "agent-shell-prompt-queue" (prompt &optional buffer))
(declare-function gptel-request "gptel" (&optional prompt &rest args))
(declare-function hitl-plan-list "hitl-plan" (&optional directory domain))
(declare-function hitl-plan-id "hitl-plan" (plan))
(declare-function hitl-plan-status "hitl-plan" (plan))
(declare-function hitl-plan-description "hitl-plan" (plan))
(declare-function hitl-plan-steps "hitl-plan" (plan))
(declare-function hitl-plan-step-status "hitl-plan" (step))
(declare-function hitl-plan-review "hitl-plan-ui" (plan &optional select))
(declare-function magit-dash-review-stage-hitl "magit-dash-review-hitl" (&optional review-file op-name))
(declare-function code-review-agents-shell-prompt "code-review-agents-shell" (criterion worktree))
(declare-function code-review-start "code-review" (url))

(defgroup magit-dash-review nil
  "Diff-review and code-review integration for magit-dash."
  :group 'magit-dash)

(defcustom magit-dash-review-agent-auto-resolve nil
  "When non-nil, agent remediation marks review threads resolved automatically.
When nil (the default), agent replies are appended leaving the thread open
for human verification and sign-off."
  :type 'boolean
  :group 'magit-dash-review)

(defcustom magit-dash-review-use-code-review-adapter nil
  "When non-nil, mirror review JSON into code-review after a pr open or sync.
Calls `magit-dash-review-json-import' on the merged file.  Off by default:
`magit-dash-review-submit' keeps its GraphQL mutations until submit is routed
through code-review, and code-review's own comment API is reached only
through `magit-dash-review-json-code-review-function'.  The merge itself
(unknown-key preserving) is always the adapter's."
  :type 'boolean
  :group 'magit-dash-review)

(defun magit-dash-review--maybe-mirror (merged file)
  "Import MERGED into code-review when `magit-dash-review-use-code-review-adapter'.
Comments code-review numbered on import get their cr_id written back into
FILE, so editing one later updates it instead of adding a second copy."
  (when magit-dash-review-use-code-review-adapter
    (let ((result (magit-dash-review-json-import merged)))
      (when (plist-get result :assigned)
        (magit-dash-review--write-json-file
         file (magit-dash-review-json-apply-assigned merged result))
        (magit-dash-review--refresh-visiting-buffer file)))))

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

(defun magit-dash-review-file (root pr-number &optional repo)
  "Return path to the review JSON file for PR-NUMBER in repository at ROOT.
The file is under <git-common-dir>/review/.  Without REPO it is pr-<num>.json.
With REPO (owner/name) it is pr-<owner>_<name>-<num>.json so the same PR
number in two repos sharing a git directory (forks) never collides; an
existing legacy pr-<num>.json is still returned only when its recorded repo
is REPO; one recording no repo cannot be verified and is ignored (message)."
  (let* ((common-dir (magit-dash-review--common-dir root))
         (review-dir (expand-file-name "review" common-dir))
         (legacy (expand-file-name (format "pr-%s.json" pr-number) review-dir)))
    (cond
     ((null repo) legacy)
     ((and (file-readable-p legacy)
           (let ((stored (ignore-errors
                           (map-elt (map-elt (magit-dash-review--read-json-file legacy)
                                             "magit_dash")
                                    "repo"))))
             (cond
              ((equal stored repo) t)
              ((member stored '(nil ""))
               (message "magit-dash-review: %s records no repo, so it cannot be verified for %s; ignoring it (add magit_dash.repo to adopt it)"
                        legacy repo)
               nil))))
      legacy)
     (t (expand-file-name (format "pr-%s-%s.json"
                                  (replace-regexp-in-string "[^A-Za-z0-9._-]" "_" repo)
                                  pr-number)
                          review-dir)))))

(defun magit-dash-review--trusted-worktree (file wt-dir)
  "Return WT-DIR when it is a directory of the repository that owns review FILE.
FILE lives in <git-common-dir>/review/.  The worktree recorded inside a
review file is untrusted: when it is missing or belongs to another
repository, message and return the main checkout of FILE's repository."
  (let* ((common (file-name-directory
                  (directory-file-name (file-name-directory (expand-file-name file)))))
         (main (if (equal (file-name-nondirectory (directory-file-name common)) ".git")
                   (file-name-directory (directory-file-name common))
                 common)))
    (cond
     ((and (stringp wt-dir) (file-directory-p wt-dir)
           (equal (file-truename (magit-dash-review--common-dir wt-dir))
                  (file-truename (file-name-as-directory common))))
      wt-dir)
     (t
      (message "magit-dash-review: worktree %s in %s is not a worktree of this repository; using %s"
               wt-dir file main)
      main))))

(defalias 'magit-dash-review--resolve-blob #'magit-dash-review-json-resolve-blob)

;;; Transcoding and Three-Way Merge
;; The implementations live in `magit-dash-review-json'; the old names stay
;; as aliases for existing callers.

(defalias 'magit-dash-review--make-comment-id #'magit-dash-review-json-make-id)
(defalias 'magit-dash-review--transcode-thread #'magit-dash-review-json-transcode-thread)
(defalias 'magit-dash-review--merge-json #'magit-dash-review-json-merge)

;;; JSON File I/O Helpers

(defalias 'magit-dash-review--read-json-file #'magit-dash-review-json-read-file)
(defalias 'magit-dash-review--write-json-file #'magit-dash-review-json-write-file)
(defalias 'magit-dash-review--read-for-write #'magit-dash-review-json-read-for-write)
(defalias 'magit-dash-review--refresh-visiting-buffer
  #'magit-dash-review-json-refresh-visiting-buffer)

(defun magit-dash-review--blank-p (value)
  "Return non-nil when VALUE is nil or the empty string."
  (or (null value) (equal value "")))

(defvar magit-dash-review--id-counter 0
  "Counter mixed into generated ids so ids made in the same second differ.")

(defun magit-dash-review--unique-id (seed)
  "Return a comment id derived from SEED, a counter and a random number."
  (magit-dash-review--make-comment-id
   (format "%s-%d-%d" seed (cl-incf magit-dash-review--id-counter) (random most-positive-fixnum))))

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
resolved and sets its `local_resolved' intent marker.  A buffer visiting
FILE with unsaved edits is not refreshed.  Returns the updated comment hash table."
  (let* ((file (expand-file-name review-file))
         (should-resolve (if (null resolve)
                             magit-dash-review-agent-auto-resolve
                           resolve))
         (timestamp (format-time-string "%FT%T%:z"))
         (reply-ht (make-hash-table :test #'equal))
         (live-buf (find-buffer-visiting file))
         (updated-comment nil))
    (setf (map-elt reply-ht "id")
          (magit-dash-review--unique-id
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
                 (setf (map-elt target "resolved") t)
                 (setf (map-elt target "local_resolved") t)
                 (map-delete target "gh_reopened"))
               (setq updated-comment target)))))
      (let* ((data (magit-dash-review--read-for-write file))
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
          (setf (map-elt target "resolved") t)
          (setf (map-elt target "local_resolved") t)
          (map-delete target "gh_reopened"))
        (setf (map-elt data "comments") comments)
        (magit-dash-review--write-json-file file data)
        (setq updated-comment target)
        (magit-dash-review--refresh-visiting-buffer file)))
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

;;; PR Worktree Orchestration, Hydration, and Front-End Entrypoints

(defun magit-dash-review--parse-pr-target (pr-number-or-url &optional repo-dir)
  "Normalize PR-NUMBER-OR-URL and REPO-DIR into `(:number :repo :repo-dir :pr)'."
  (let* ((dir (expand-file-name
               (magit-dash-worktree-main-dir
                (or repo-dir
                    (ignore-errors (magit-dash-worktree-main-dir default-directory))
                    default-directory))))
         (slug nil)
         (num nil)
         (pr-data nil))
    (cond
     ((and (listp pr-number-or-url) (plist-get pr-number-or-url :number))
      (setq num (plist-get pr-number-or-url :number)
            slug (plist-get pr-number-or-url :repo)
            pr-data (plist-get pr-number-or-url :pr)))
     ((integerp pr-number-or-url)
      (setq num pr-number-or-url))
     ((and (stringp pr-number-or-url)
           (string-match "github\\.com/\\([^/]+/[^/]+\\)/pull/\\([0-9]+\\)" pr-number-or-url))
      (setq slug (match-string 1 pr-number-or-url)
            num (string-to-number (match-string 2 pr-number-or-url))))
     ((and (stringp pr-number-or-url)
           (string-match "\\`#?\\([0-9]+\\)\\'" (string-trim pr-number-or-url)))
      (setq num (string-to-number (match-string 1 (string-trim pr-number-or-url))))))
    (unless slug
      (setq slug (or (ignore-errors
                       (let ((default-directory dir))
                         (magit-dash-pr-dashboard--current-repo-slug)))
                     "local/repo")))
    (unless (and (integerp num) (> num 0))
      (error "Invalid pull request number or URL: %S" pr-number-or-url))
    (list :number num :repo slug :repo-dir dir :pr pr-data)))

(defun magit-dash-review--fetch-pr-metadata (slug pr-number repo-dir)
  "Fetch pull request metadata for PR-NUMBER in SLUG via `gh pr view'."
  (pcase-let ((`(,code . ,output)
               (magit-dash-gh--call-process
                (list "pr" "view" (number-to-string pr-number)
                      "-R" slug
                      "--json" "number,title,url,headRefName,baseRefName,headRefOid,baseRefOid,state,reviewDecision")
                repo-dir)))
    (unless (zerop code)
      (error "Failed to fetch PR metadata for %s#%d: %s" slug pr-number output))
    (json-parse-string output :object-type 'alist)))

(defun magit-dash-review--open-front-end (review-file)
  "Open REVIEW-FILE in `diff-review-find-file' when available, else `find-file'."
  (if (fboundp 'diff-review-find-file)
      (diff-review-find-file review-file)
    (find-file-noselect review-file))
  review-file)

;;;###autoload
(defun magit-dash-review-pr (&optional pr-number-or-url repo-dir)
  "Hydrate and open a native-buffer `diff-review' session for PR-NUMBER-OR-URL.
Reuses or creates the `.wt/pr-<num>' worktree via `magit-dash-pr', fetches
PR metadata and GraphQL reviewThreads, performs a three-way merge into
`<git-common-dir>/review/pr-<num>.json', and opens `diff-review-find-file'."
  (interactive
   (list (read-string "PR number or URL: ") default-directory))
  (let* ((target (magit-dash-review--parse-pr-target pr-number-or-url repo-dir))
         (pr-num (plist-get target :number))
         (slug (plist-get target :repo))
         (dir (plist-get target :repo-dir))
         (pr-info (or (plist-get target :pr)
                      (magit-dash-review--fetch-pr-metadata slug pr-num dir)))
         (head-ref (or (map-elt pr-info 'headRefName) (format "pr-%d" pr-num)))
         (base-ref (or (map-elt pr-info 'baseRefName) "main"))
         (head-oid (map-elt pr-info 'headRefOid))
         (base-oid (map-elt pr-info 'baseRefOid))
         (url (map-elt pr-info 'url))
         (existing-wt (magit-dash-pr--find-existing-worktree dir pr-num head-ref))
         (wt-dir (or existing-wt
                     (condition-case err
                         (magit-dash-pr--create-worktree-for-entry
                          (list :number pr-num :repo slug :pr pr-info :repo-dir dir) nil)
                       (error
                        (user-error "Cannot create a worktree for PR #%d (%s); not reviewing in the main checkout"
                                    pr-num (error-message-string err))))))
         (raw-threads (or (ignore-errors
                            (magit-dash-pr-fetch-threads-graphql slug pr-num 50 4000 dir t))
                          nil))
         (transcoded (mapcar
                      (lambda (node)
                        (magit-dash-review--transcode-thread node wt-dir head-oid base-oid))
                      raw-threads))
         (review-file (magit-dash-review-file dir pr-num slug))
         (live-buf (find-buffer-visiting review-file))
         (remote-meta (list :repo slug
                            :number pr-num
                            :url url
                            :worktree (file-name-as-directory (expand-file-name wt-dir))
                            :head-ref head-ref
                            :base-ref base-ref
                            :head_oid head-oid
                            :base_oid base-oid
                            :threads-complete (and raw-threads (< (length raw-threads) 50)))))
    (cond
     ((and live-buf (buffer-modified-p live-buf))
      (message "magit-dash-review-pr: %s has unsaved edits; not merging remote threads into it"
               (buffer-name live-buf)))
     (t
      ;; Read only now, after every network call above, so edits made
      ;; meanwhile are merged rather than overwritten.
      (let ((merged (magit-dash-review--merge-json
                     (and (file-exists-p review-file)
                          (magit-dash-review--read-for-write review-file))
                     remote-meta transcoded)))
        (magit-dash-review--write-json-file review-file merged)
        (magit-dash-review--maybe-mirror merged review-file)
        (magit-dash-review--refresh-visiting-buffer review-file))))
    (magit-dash-review--open-front-end review-file)))

;;;###autoload
(defun magit-dash-review-worktree-diff (&optional worktree-dir base-ref)
  "Open a `diff-review' session for uncommitted changes in WORKTREE-DIR.
Writes `<git-common-dir>/review/worktree-<name>.json' with `head' omitted
so `diff-review' compares the live working tree against BASE-REF (default
\"HEAD\")."
  (interactive)
  (let* ((wt-dir (file-name-as-directory
                  (expand-file-name (or worktree-dir default-directory))))
         (wt-name (file-name-nondirectory (directory-file-name wt-dir)))
         (common-dir (magit-dash-review--common-dir wt-dir))
         (review-file (expand-file-name
                       (format "review/worktree-%s.json" wt-name)
                       common-dir))
         (existing-ht (and (file-exists-p review-file)
                           (magit-dash-review--read-for-write review-file)))
         (data (or existing-ht (make-hash-table :test #'equal))))
    (setf (map-elt data "format") "diff-review/1")
    (setf (map-elt data "root") wt-dir)
    (setf (map-elt data "base") (or base-ref (and existing-ht (map-elt existing-ht "base")) "HEAD"))
    (map-delete data "head")
    (unless (hash-table-p (map-elt data "viewed"))
      (setf (map-elt data "viewed") (make-hash-table :test #'equal)))
    (unless (vectorp (map-elt data "comments"))
      (setf (map-elt data "comments") []))
    (magit-dash-review--write-json-file review-file data)
    (magit-dash-review--refresh-visiting-buffer review-file)
    (magit-dash-review--open-front-end review-file)))

;;;###autoload
(defun magit-dash-review-sync (&optional review-file)
  "Refetch remote PR threads and metadata and three-way merge into REVIEW-FILE."
  (interactive)
  (let* ((file (expand-file-name
                (or review-file
                    (magit-dash-review--current-review-file)
                    (user-error "No active review JSON file to sync"))))
         (existing-ht (magit-dash-review--read-json-file file))
         (md (map-elt existing-ht "magit_dash"))
         (slug (and md (map-elt md "repo")))
         (pr-num (and md (map-elt md "number")))
         (wt-dir (magit-dash-review--trusted-worktree
                  file
                  (or (and md (map-elt md "worktree"))
                      (map-elt existing-ht "root")
                      default-directory))))
    (unless (and slug pr-num)
      (user-error "Review file %s has no `magit_dash' PR metadata to sync" file))
    (let* ((pr-info (magit-dash-review--fetch-pr-metadata slug pr-num wt-dir))
           (head-oid (or (map-elt pr-info 'headRefOid) (map-elt md "head_oid")))
           (base-oid (or (map-elt pr-info 'baseRefOid) (map-elt md "base_oid")))
           (raw-threads (magit-dash-pr-fetch-threads-graphql slug pr-num 50 4000 wt-dir t))
           (transcoded (mapcar
                        (lambda (node)
                          (magit-dash-review--transcode-thread node wt-dir head-oid base-oid))
                        raw-threads))
           (remote-meta (list :repo slug
                              :number pr-num
                              :url (or (map-elt pr-info 'url) (map-elt md "url"))
                              :worktree wt-dir
                              :head-ref (or (map-elt pr-info 'headRefName) (map-elt existing-ht "head"))
                              :base-ref (or (map-elt pr-info 'baseRefName) (map-elt existing-ht "base"))
                              :head_oid head-oid
                              :base_oid base-oid
                              :threads-complete (< (length raw-threads) 50)))
           (live-buf (find-buffer-visiting file)))
      (cond
       ((and live-buf (buffer-modified-p live-buf))
        (message "magit-dash-review-sync: %s has unsaved edits; save or revert it, then sync again"
                 (buffer-name live-buf))
        nil)
       (t
        ;; Merge against a fresh read: the file may have changed while the
        ;; network calls above ran.
        (let ((merged (magit-dash-review--merge-json
                       (magit-dash-review--read-for-write file) remote-meta transcoded)))
          (magit-dash-review--write-json-file file merged)
          (magit-dash-review--maybe-mirror merged file)
          (magit-dash-review--refresh-visiting-buffer file)
          file))))))

;;; GraphQL Delta Submission
;; Legacy path, kept until submit is routed through code-review.  Every
;; request goes through `magit-dash-review--gh-graphql': the query and its
;; variables travel as one JSON body in a temporary file (`gh api --input'),
;; never as `-F key=value' fields, because `-F' reads `@file' values from
;; disk and coerces numbers, true and null, which would let review text
;; choose what gets sent.

(defconst magit-dash-review--submit-chunk-size 40
  "Most new threads sent in one addPullRequestReview call.
Kept below the 50 comments the response lists so every posted thread can be
matched to its returned node.")

(defun magit-dash-review--gh-graphql (query variables dir)
  "Run GraphQL QUERY with VARIABLES, a hash table, with `gh' in DIR.
Return the parsed response alist.  Signal on a non-zero exit or when the
response has errors."
  (let ((input-file (make-temp-file "magit-dash-gh-" nil ".json"))
        (payload (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (setf (map-elt payload "query") query)
          (setf (map-elt payload "variables") variables)
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert (json-serialize payload :null-object nil :false-object :false))
            (let ((coding-system-for-write 'binary))
              (write-region (point-min) (point-max) input-file nil 'silent)))
          (pcase-let ((`(,code . ,output)
                       (magit-dash-gh--call-process
                        (list "api" "graphql"
                              "-H" "Content-Type: application/json"
                              "--input" input-file)
                        dir)))
            (unless (zerop code)
              (error "gh api graphql failed: %s" output))
            (let ((resp (json-parse-string output :object-type 'alist :array-type 'list)))
              (when (map-elt resp 'errors)
                (error "GraphQL errors: %S" (map-elt resp 'errors)))
              resp)))
      (when (file-exists-p input-file)
        (delete-file input-file)))))

(defun magit-dash-review--verdict-to-gh-event (verdict-ht)
  "Map VERDICT-HT state to a GitHub PullRequestReviewEvent string, or nil.
An unknown or missing state gives nil, so no review is posted for it."
  (let ((state (and (hash-table-p verdict-ht) (map-elt verdict-ht "state"))))
    (pcase state
      ("approved" "APPROVE")
      ("changes" "REQUEST_CHANGES")
      ("comment" "COMMENT")
      (_ nil))))

(defun magit-dash-review--verdict-pending-p (verdict-ht)
  "Return non-nil when VERDICT-HT names a known state not yet sent as it stands.
A verdict is sent when it has no `submitted_at', or its state or head differ
from `submitted_state' and `submitted_head'."
  (and (hash-table-p verdict-ht)
       (magit-dash-review--verdict-to-gh-event verdict-ht)
       (not (and (map-elt verdict-ht "submitted_at")
                 (equal (map-elt verdict-ht "submitted_state") (map-elt verdict-ht "state"))
                 (or (magit-dash-review--blank-p (map-elt verdict-ht "head"))
                     (equal (map-elt verdict-ht "head") (map-elt verdict-ht "submitted_head")))))))

(defun magit-dash-review--collect-unsynced-threads (comments)
  "Return non-draft local comments in COMMENTS never sent to GitHub.
Those have no `gh_thread_id' and a status other than draft or sent."
  (seq-filter
   (lambda (c)
     (let ((gh-id (map-elt c "gh_thread_id"))
           (status (map-elt c "status"))
           (path (map-elt c "path")))
       (and (or (null gh-id) (string-empty-p gh-id))
            (not (member status '("draft" "sent")))
            path (not (string-empty-p path)))))
   (append (or comments []) nil)))

(defun magit-dash-review--fetch-pr-node-id (slug pr-number dir)
  "Fetch the GraphQL node ID for PR-NUMBER in SLUG."
  (let* ((parts (split-string slug "/"))
         (vars (make-hash-table :test #'equal))
         (query "query($owner:String! $repo:String! $number:Int!){
  repository(owner:$owner name:$repo){
    pullRequest(number:$number){ id }
  }
}"))
    (setf (map-elt vars "owner") (car parts))
    (setf (map-elt vars "repo") (cadr parts))
    (setf (map-elt vars "number") pr-number)
    (or (map-nested-elt (magit-dash-review--gh-graphql query vars dir)
                        '(data repository pullRequest id))
        (error "Missing pullRequest.id in GraphQL response for %s#%d" slug pr-number))))

(defun magit-dash-review--thread-payload (c)
  "Return the DraftPullRequestReviewThread table for local comment C."
  (let* ((t-ht (make-hash-table :test #'equal))
         (lines (append (map-elt c "lines") nil))
         (l1 (nth 0 lines))
         (l2 (nth 1 lines))
         (side (map-elt c "diff_side")))
    (setf (map-elt t-ht "path") (map-elt c "path"))
    (setf (map-elt t-ht "body") (or (map-elt c "body") ""))
    (when (or l2 l1)
      (setf (map-elt t-ht "line") (or l2 l1)))
    (when (and l1 l2 (< l1 l2))
      (setf (map-elt t-ht "startLine") l1))
    (when (member side '("LEFT" "RIGHT"))
      (setf (map-elt t-ht "side") side)
      (when (map-elt t-ht "startLine")
        (setf (map-elt t-ht "startSide") side)))
    t-ht))

(defun magit-dash-review--node-matches-p (c node strict)
  "Return non-nil when returned review comment NODE is local comment C.
Path and trimmed body must be equal; when STRICT the line must be too,
as far as NODE reports one."
  (let* ((lines (append (map-elt c "lines") nil))
         (line (or (nth 1 lines) (nth 0 lines)))
         (node-line (or (map-elt node 'line) (map-elt node 'originalLine))))
    (and (equal (map-elt c "path") (map-elt node 'path))
         (equal (string-trim (or (map-elt c "body") ""))
                (string-trim (or (map-elt node 'body) "")))
         (or (not strict) (null node-line) (null line) (equal line node-line)))))

(defun magit-dash-review--pair-review-nodes (locals nodes)
  "Return an alist (LOCAL . NODE) pairing LOCALS with returned NODES.
Each node is used once.  Pairing is by path, line and body first, then by
path and body for the comments left over; never by position."
  (let ((remaining (copy-sequence nodes))
        (pairs nil))
    (seq-do (lambda (strict)
              (seq-do (lambda (c)
                        (unless (assq c pairs)
                          (when-let* ((node (seq-find (lambda (n)
                                                        (magit-dash-review--node-matches-p c n strict))
                                                      remaining)))
                            (setq remaining (delq node remaining))
                            (push (cons c node) pairs))))
                      locals))
            '(t nil))
    pairs))

(defun magit-dash-review--submit-threads (chunk event feedback pr-id head dir)
  "Post the new threads in CHUNK as one review with EVENT and FEEDBACK.
PR-ID is the pull request node id and HEAD the commit to attach it to.
Stamp the GitHub ids onto each comment of CHUNK, mark it sent, and return
the review state string."
  (let* ((input-ht (make-hash-table :test #'equal))
         (vars-ht (make-hash-table :test #'equal))
         (mutation "mutation($input:AddPullRequestReviewInput!){
  addPullRequestReview(input:$input){
    pullRequestReview{
      id
      state
      comments(first:50){
        nodes{ id path body line originalLine pullRequestReviewThread{ id } }
      }
    }
  }
}"))
    (setf (map-elt input-ht "pullRequestId") pr-id)
    (setf (map-elt input-ht "event") event)
    (when head
      (setf (map-elt input-ht "commitOID") head))
    (when (and feedback (not (string-empty-p feedback)))
      (setf (map-elt input-ht "body") feedback))
    (when chunk
      (setf (map-elt input-ht "threads")
            (vconcat (seq-map #'magit-dash-review--thread-payload chunk))))
    (setf (map-elt vars-ht "input") input-ht)
    (let* ((resp (magit-dash-review--gh-graphql mutation vars-ht dir))
           (review (map-nested-elt resp '(data addPullRequestReview pullRequestReview)))
           (pairs (magit-dash-review--pair-review-nodes
                   chunk (map-nested-elt review '(comments nodes)))))
      (seq-do (lambda (c)
                (let* ((node (alist-get c pairs nil nil #'eq))
                       (cid (map-elt node 'id))
                       (tid (map-nested-elt node '(pullRequestReviewThread id))))
                  (unless node
                    (message "magit-dash-review-submit: posted %s:%s but GitHub returned no matching comment; the next sync will link it"
                             (map-elt c "path") (map-elt c "lines")))
                  (when tid (setf (map-elt c "gh_thread_id") tid))
                  (when cid (setf (map-elt c "gh_comment_id") cid))
                  (setf (map-elt c "origin") "github")
                  (setf (map-elt c "status") "sent")
                  (setf (map-elt c "gh_resolved") :false)))
              chunk)
      (map-elt review 'state))))

(defun magit-dash-review--post-reply (c reply dir)
  "Post REPLY on the GitHub thread of comment C and stamp it."
  (let ((vars (make-hash-table :test #'equal))
        (mutation "mutation($threadId:ID! $body:String!){
  addPullRequestReviewThreadReply(input:{pullRequestReviewThreadId:$threadId body:$body}){
    comment{ id }
  }
}"))
    (setf (map-elt vars "threadId") (map-elt c "gh_thread_id"))
    (setf (map-elt vars "body") (or (map-elt reply "body") ""))
    (let ((new-cid (map-nested-elt (magit-dash-review--gh-graphql mutation vars dir)
                                   '(data addPullRequestReviewThreadReply comment id))))
      (when new-cid
        (setf (map-elt reply "gh_comment_id") new-cid))
      (setf (map-elt reply "status") "sent"))))

(defun magit-dash-review--set-thread-resolution (c resolve dir)
  "Resolve (RESOLVE non-nil) or unresolve the GitHub thread of comment C."
  (let ((vars (make-hash-table :test #'equal))
        (mutation (if resolve
                      "mutation($threadId:ID!){ resolveReviewThread(input:{threadId:$threadId}){ thread{ id isResolved } } }"
                    "mutation($threadId:ID!){ unresolveReviewThread(input:{threadId:$threadId}){ thread{ id isResolved } } }")))
    (setf (map-elt vars "threadId") (map-elt c "gh_thread_id"))
    (magit-dash-review--gh-graphql mutation vars dir)
    (setf (map-elt c "gh_resolved") (if resolve t :false))
    (map-delete c "local_resolved")))

(defun magit-dash-review--resolution-intent (c)
  "Return `resolve', `unresolve' or nil: what submit should do to C's thread.
Resolve only for a local intent: `local_resolved' set by a local resolve, or
a local resolved state GitHub never saw resolved (no `gh_reopened').  A
thread reopened on GitHub after the local resolve is left open."
  (let ((local-res (eq (map-elt c "resolved") t))
        (remote-res (eq (map-elt c "gh_resolved") t)))
    (cond
     ((and local-res (not remote-res)
           (or (eq (map-elt c "local_resolved") t)
               (not (eq (map-elt c "gh_reopened") t))))
      'resolve)
     ((and (not local-res) remote-res) 'unresolve))))

(defun magit-dash-review--check-verdict-head (md verdict-ht event current-head)
  "Refuse an APPROVE or REQUEST_CHANGES EVENT for a stale review.
The head the review was made at (MD head_oid, VERDICT-HT head) must be
CURRENT-HEAD, the pull request's head now."
  (when (member event '("APPROVE" "REQUEST_CHANGES"))
    (seq-do (lambda (reviewed)
              (when (and (not (magit-dash-review--blank-p reviewed))
                         current-head
                         (not (equal reviewed current-head)))
                (user-error "Refusing to send %s: reviewed %s but the PR head is now %s; sync and re-review the new commits first"
                            event (substring reviewed 0 (min 12 (length reviewed)))
                            (substring current-head 0 (min 12 (length current-head))))))
            (list (map-elt md "head_oid") (map-elt verdict-ht "head")))))

;;;###autoload
(defun magit-dash-review-submit (&optional review-file feedback)
  "Publish unsynced non-draft threads, replies, resolutions, and verdict in REVIEW-FILE.
Excludes comments with `status: \"draft\"'.  Stamps returned GitHub GraphQL
node IDs back into REVIEW-FILE, writing the file after every successful
mutation, so a failure part way leaves what was sent recorded and a repeated
submit sends only the remainder.  New threads go in chunks; a thread is
stamped by matching the returned comment on path, line and body.  The
verdict is sent once (stamped `submitted_state', `submitted_head',
`submitted_at') and again only when it changes; APPROVE and REQUEST_CHANGES
are refused when the PR head moved past the reviewed head.  Replies and
resolutions skip threads flagged `gh_missing'; an error on one thread is
collected and the others still run, then reported together."
  (interactive
   (list (or (magit-dash-review--current-review-file)
             (user-error "No active review JSON file to submit"))
         (read-string "Overall review summary (optional): ")))
  (let* ((file (expand-file-name
                (or review-file
                    (magit-dash-review--current-review-file)
                    (user-error "No active review JSON file to submit"))))
         (data (magit-dash-review--read-for-write file))
         (md (map-elt data "magit_dash"))
         (slug (and md (map-elt md "repo")))
         (pr-num (and md (map-elt md "number")))
         (dir (magit-dash-review--trusted-worktree
               file
               (or (and md (map-elt md "worktree"))
                   (map-elt data "root")
                   default-directory)))
         (comments (append (or (map-elt data "comments") []) nil))
         (verdict-ht (map-elt data "verdict"))
         (new-threads (magit-dash-review--collect-unsynced-threads comments))
         (event (magit-dash-review--verdict-to-gh-event verdict-ht))
         (verdict-pending (magit-dash-review--verdict-pending-p verdict-ht))
         (feedback-p (and feedback (not (string-empty-p feedback))))
         (errors nil)
         (save (lambda ()
                 (setf (map-elt data "comments") (vconcat comments))
                 (magit-dash-review--write-json-file file data))))
    (unless (and slug pr-num)
      (user-error "Review file %s lacks `magit_dash' PR metadata" file))
    (when (and verdict-ht (not event))
      (message "magit-dash-review-submit: unknown verdict state %S; no verdict sent"
               (map-elt verdict-ht "state")))

    ;; 1. New threads and/or verdict via addPullRequestReview, in chunks.
    (when (or new-threads verdict-pending feedback-p)
      (let* ((current-head (map-elt (magit-dash-review--fetch-pr-metadata slug pr-num dir)
                                    'headRefOid))
             (pr-id (or (map-elt md "pr_node_id")
                        (magit-dash-review--fetch-pr-node-id slug pr-num dir)))
             (review-head (or (map-elt md "head_oid") current-head))
             (chunks (or (seq-partition new-threads magit-dash-review--submit-chunk-size)
                         (list nil))))
        (when verdict-pending
          (magit-dash-review--check-verdict-head md verdict-ht event current-head))
        (seq-do-indexed
         (lambda (chunk index)
           (let ((last-p (= index (1- (length chunks)))))
             (magit-dash-review--submit-threads
              chunk
              (if (and last-p verdict-pending) event "COMMENT")
              (and last-p feedback)
              pr-id review-head dir)
             (when (and last-p verdict-pending)
               (setf (map-elt verdict-ht "submitted_state") (map-elt verdict-ht "state"))
               (setf (map-elt verdict-ht "submitted_head")
                     (or review-head ""))
               (setf (map-elt verdict-ht "submitted_at") (format-time-string "%FT%T%:z")))
             (funcall save)))
         chunks)))

    ;; 2. Pending replies on existing GitHub threads, one write per reply.
    (seq-do
     (lambda (c)
       (let ((tid (map-elt c "gh_thread_id"))
             (failed nil)
             (pending (seq-filter
                       (lambda (reply)
                         (and (magit-dash-review--blank-p (map-elt reply "gh_comment_id"))
                              (not (member (map-elt reply "status") '("draft" "sent")))))
                       (append (or (map-elt c "replies") []) nil))))
         (cond
          ((magit-dash-review--blank-p tid) nil)
          ((eq (map-elt c "gh_missing") t)
           (when pending
             (message "magit-dash-review-submit: thread %s is gone on GitHub; skipping %d repl%s"
                      tid (length pending) (if (cdr pending) "ies" "y"))))
          (t
           (seq-do (lambda (reply)
                     (unless failed
                       (condition-case err
                           (progn
                             (magit-dash-review--post-reply c reply dir)
                             (funcall save))
                         (error
                          (setq failed t)
                          (push (format "reply on %s: %s" tid (error-message-string err))
                                errors)))))
                   pending)))))
     comments)

    ;; 3. Resolution changes that are local intent.
    (seq-do
     (lambda (c)
       (let ((tid (map-elt c "gh_thread_id")))
         (when (and (not (magit-dash-review--blank-p tid))
                    (not (eq (map-elt c "gh_missing") t)))
           (when-let* ((intent (magit-dash-review--resolution-intent c)))
             (condition-case err
                 (progn
                   (magit-dash-review--set-thread-resolution c (eq intent 'resolve) dir)
                   (funcall save))
               (error
                (push (format "%s on %s: %s" intent tid (error-message-string err))
                      errors)))))))
     comments)

    (funcall save)
    (magit-dash-review--refresh-visiting-buffer file)
    (when errors
      (error "Submit incomplete, %d error(s); what was sent is saved, resubmit to retry: %s"
             (length errors) (string-join (nreverse errors) "; ")))
    file))

;;; Agent Remediation, ASW/ASQ Dispatch, Hunk Critique, and Draft Promotion

(defvar magit-dash-review-asq-enqueue-function nil
  "Optional override function `(lambda (prompt worktree))' for ASQ prompt queueing.")

(defun magit-dash-review--enqueue-prompt (prompt worktree)
  "Enqueue PROMPT for the agent session associated with WORKTREE."
  (let ((default-directory (file-name-as-directory
                            (expand-file-name (or worktree default-directory)))))
    (cond
     ((functionp magit-dash-review-asq-enqueue-function)
      (funcall magit-dash-review-asq-enqueue-function prompt default-directory))
     ((fboundp 'agent-shell-prompt-queue)
      (agent-shell-prompt-queue prompt))
     (t
      (error "No agent-shell queue dispatcher available for %s" default-directory)))
    prompt))

(defun magit-dash-review--format-remediation-prompt (findings review-file)
  "Format a remediation prompt for FINDINGS in REVIEW-FILE."
  (let* ((first-f (car findings))
         (worktree (or (plist-get first-f :worktree) default-directory))
         (idx 0))
    (concat
     (format "Remediate the following %d review finding(s) in `%s` (review file: `%s`):\n\n"
             (length findings) worktree review-file)
     (mapconcat
      (lambda (f)
        (setq idx (1+ idx))
        (let* ((id (or (plist-get f :id) "unknown"))
               (path (or (plist-get f :path) "(review-wide)"))
               (line (plist-get f :line))
               (end-line (plist-get f :end-line))
               (loc (cond
                     ((and line end-line (/= line end-line))
                      (format "%s:%d-%d" path line end-line))
                     (line (format "%s:%d" path line))
                     (t path)))
               (body (or (plist-get f :body) ""))
               (hunk (or (plist-get f :hunk) "")))
          (concat
           (format "%d. `%s` (id: `%s`):\n   %s\n" idx loc id body)
           (if (not (string-empty-p hunk))
               (format "```diff\n%s\n```\n" hunk)
             "")
           (format "   Reply command: `emacsclient --eval '(magit-dash-review-append-reply %S %S \"agent-shell\" \"<summary>\")'`\n"
                   review-file id))))
      findings
      "\n"))))

(defun magit-dash-review-send-findings-to-agent (findings &optional review-file)
  "Dispatch FINDINGS from REVIEW-FILE to `agent-shell' via ASW or ASQ."
  (unless findings
    (user-error "No unresolved review comments to send"))
  (let* ((first-f (car findings))
         (rf (expand-file-name (or review-file (plist-get first-f :review-file))))
         (worktree (or (plist-get first-f :worktree)
                       (plist-get first-f :root)
                       default-directory)))
    (if (and (fboundp 'agent-shell-workflow-dispatch)
             (not magit-dash-review-asq-enqueue-function))
        (agent-shell-workflow-dispatch
         'diff-review-remediate
         :args (list :review-file rf :findings findings :root worktree))
      (magit-dash-review--enqueue-prompt
       (magit-dash-review--format-remediation-prompt findings rf)
       worktree))))

;;;###autoload
(defun magit-dash-review-enqueue-asq (&optional review-file grouping)
  "Enqueue unresolved findings from REVIEW-FILE into ASQ.
GROUPING is `per-file' (default: one queue item per file) or `per-thread'."
  (interactive)
  (let* ((file (expand-file-name
                (or review-file
                    (magit-dash-review--current-review-file)
                    (user-error "No active review JSON file"))))
         (findings (magit-dash-review-extract-findings file t)))
    (unless findings
      (user-error "No unresolved review comments to send"))
    (let* ((worktree (or (plist-get (car findings) :worktree) default-directory))
           (groups (if (eq grouping 'per-thread)
                       (mapcar #'list findings)
                     (mapcar #'cdr
                             (seq-group-by
                              (lambda (f) (or (plist-get f :path) "(review)"))
                              findings)))))
      (mapcar
       (lambda (grp)
         (let ((prompt (magit-dash-review--format-remediation-prompt grp file)))
           (magit-dash-review--enqueue-prompt prompt worktree)))
       groups))))

;;;###autoload
(defun magit-dash-review-send-to-agent (&optional mode review-file)
  "Send unresolved findings from REVIEW-FILE to `agent-shell'.
MODE is one of:
  `workflow'     (default) — dispatch `diff-review-remediate' via ASW
  `hitl'         (C-u)     — stage findings in `hitl-plan' (`code-review' domain)
  `asq-per-file' (C-u C-u) — enqueue one ASQ work item per file."
  (interactive
   (list (cond
          ((equal current-prefix-arg '(16)) 'asq-per-file)
          (current-prefix-arg 'hitl)
          (t 'workflow))
         (magit-dash-review--current-review-file)))
  (let* ((file (expand-file-name
                (or review-file
                    (magit-dash-review--current-review-file)
                    (user-error "No active review JSON file"))))
         (mode (or mode 'workflow))
         (findings (magit-dash-review-extract-findings file t)))
    (unless findings
      (user-error "No unresolved review comments to send"))
    (pcase mode
      ('hitl
       (require 'magit-dash-review-hitl)
       (magit-dash-review-stage-hitl file 'send-to-agent))
      ('asq-per-file
       (magit-dash-review-enqueue-asq file 'per-file))
      (_
       (magit-dash-review-send-findings-to-agent findings file)))))

(defun magit-dash-review--insert-draft-comment (review-file path line end-line author body)
  "Insert a `status: \"draft\"' comment into REVIEW-FILE and return its table."
  (let* ((file (expand-file-name review-file))
         (data (magit-dash-review--read-for-write file))
         (comments (append (or (map-elt data "comments") []) nil))
         (c-ht (make-hash-table :test #'equal))
         (id (magit-dash-review--unique-id
              (format "%s:%s:%s:%s" (or path "") (or line 0) body (float-time)))))
    (setf (map-elt c-ht "id") id)
    (setf (map-elt c-ht "origin") "agent")
    (setf (map-elt c-ht "status") "draft")
    (when path (setf (map-elt c-ht "path") path))
    (when line (setf (map-elt c-ht "lines") (vector line (max line (or end-line line)))))
    (setf (map-elt c-ht "author") (or author "gptel"))
    (setf (map-elt c-ht "at") (format-time-string "%FT%T%:z"))
    (setf (map-elt c-ht "body") (string-trim body))
    (setf (map-elt c-ht "resolved") :false)
    (setf (map-elt c-ht "replies") [])
    (setf (map-elt data "comments") (vconcat comments (list c-ht)))
    (magit-dash-review--write-json-file file data)
    (magit-dash-review--refresh-visiting-buffer file)
    c-ht))

;;;###autoload
(defun magit-dash-review-critique-hunk (&optional extra-prompt review-file)
  "Run an AI critique on the diff hunk at point and record a draft comment.
Uses `gptel-request' when available, falling back to `agent-shell'."
  (interactive)
  (let* ((ctx (magit-dash-review-current-hunk))
         (rf (expand-file-name
              (or review-file
                  (plist-get ctx :review-file)
                  (magit-dash-review--current-review-file)
                  (user-error "Not in an active diff-review session"))))
         (path (or (plist-get ctx :path) "unknown"))
         (line (or (plist-get ctx :line) 1))
         (end-line (or (plist-get ctx :end-line) line))
         (hunk (or (plist-get ctx :hunk) ""))
         (prompt (format "Critique the following diff hunk in `%s` (lines %d-%d). Identify concrete defects, edge cases, or concurrency issues concisely.%s\n\n```diff\n%s\n```"
                         path line end-line
                         (if (and extra-prompt (not (string-empty-p extra-prompt)))
                             (concat "\nFocus: " extra-prompt)
                           "")
                         hunk)))
    (if (fboundp 'gptel-request)
        (gptel-request
         prompt
         :callback
         (lambda (response _info)
           (when (and (stringp response) (not (string-empty-p (string-trim response))))
             (magit-dash-review--insert-draft-comment
              rf path line end-line "gptel" response))))
      (magit-dash-review--enqueue-prompt prompt (plist-get ctx :root)))))

;;;###autoload
(defun magit-dash-review-draft-test (&optional comment-id review-file)
  "Dispatch a DRAFT-NOT-PUSH regression test authoring task for COMMENT-ID or hunk at point."
  (interactive)
  (let* ((ctx (magit-dash-review-current-hunk))
         (rf (expand-file-name
              (or review-file
                  (plist-get ctx :review-file)
                  (magit-dash-review--current-review-file)
                  (user-error "Not in an active diff-review session"))))
         (cid (or comment-id (plist-get ctx :comment-id)))
         (findings (magit-dash-review-extract-findings rf nil))
         (finding (and cid (seq-find (lambda (f) (equal (plist-get f :id) cid)) findings)))
         (worktree (or (and finding (plist-get finding :worktree))
                       (plist-get ctx :root)
                       default-directory))
         (path (or (and finding (plist-get finding :path))
                   (plist-get ctx :path)
                   "unknown"))
         (line (or (and finding (plist-get finding :line))
                   (plist-get ctx :line)
                   1))
         (body (or (and finding (plist-get finding :body))
                   "Write a regression test covering this change."))
         (hunk (or (and finding (plist-get finding :hunk))
                   (plist-get ctx :hunk)
                   ""))
         (prompt
          (if (fboundp 'code-review-agents-shell-prompt)
              (code-review-agents-shell-prompt
               (list :id (or cid "hunk")
                     :paths (list path)
                     :statement body)
               worktree)
            (format "DRAFT-NOT-PUSH Regression Test Task\n\nWorking tree: %s\nTarget file: %s (line %d)\nReview finding: %s\n\nDiff context:\n```diff\n%s\n```\n\nInstructions:\n1. Author a deterministic regression test in `%s` that fails without the fix and passes with it.\n2. Run the test suite to verify.\n3. Leave changes uncommitted in the working tree (DRAFT-NOT-PUSH) so the reviewer can inspect them."
                    worktree path line body hunk worktree))))
    (magit-dash-review--enqueue-prompt prompt worktree)))

;;;###autoload
(defun magit-dash-review-promote-draft (&optional review-file comment-id)
  "Promote a `status: \"draft\"' comment in REVIEW-FILE to `status: \"accepted\"'."
  (interactive)
  (let* ((ctx (magit-dash-review-current-hunk))
         (file (expand-file-name
                (or review-file
                    (plist-get ctx :review-file)
                    (magit-dash-review--current-review-file)
                    (user-error "No active review JSON file"))))
         (cid (or comment-id (plist-get ctx :comment-id)))
         (data (magit-dash-review--read-for-write file))
         (comments (append (or (map-elt data "comments") []) nil))
         (target (if cid
                     (seq-find (lambda (c) (equal (map-elt c "id") cid)) comments)
                   (seq-find (lambda (c) (equal (map-elt c "status") "draft")) comments))))
    (unless target
      (user-error "No draft comment found to promote in %s" file))
    (setf (map-elt target "status") "accepted")
    (setf (map-elt data "comments") (vconcat comments))
    (magit-dash-review--write-json-file file data)
    (magit-dash-review--refresh-visiting-buffer file)
    target))

;;; EARS Criteria & code-review State Bridge

(defun magit-dash-review--format-ears-criterion (crit)
  "Format a single EARS criterion plist or alist CRIT into a markdown bullet."
  (let ((id (or (plist-get crit :id) (map-elt crit 'id) "req"))
        (stmt (or (plist-get crit :statement) (map-elt crit 'statement)
                  (plist-get crit :text) (map-elt crit 'text) ""))
        (paths (or (plist-get crit :paths) (map-elt crit 'paths))))
    (format "- **[%s]** %s%s"
            id stmt
            (if paths (format " (`%s`)" (string-join paths "`, `")) ""))))

;;;###autoload
(defun magit-dash-review-project-ears-criteria (review-file &optional criteria-list local-comments)
  "Project EARS CRITERIA-LIST and LOCAL-COMMENTS from `code-review' into REVIEW-FILE.
Stores criteria metadata under `\"ears_criteria\"' and updates or inserts a
review-wide summary comment thread (`id: \"ears-criteria\"')."
  (let* ((file (expand-file-name review-file))
         (data (magit-dash-review--read-for-write file))
         (comments (append (or (map-elt data "comments") []) nil)))
    (when criteria-list
      (let* ((bullets (mapconcat #'magit-dash-review--format-ears-criterion
                                 criteria-list "\n"))
             (body (concat "### EARS Review Criteria\n\n" bullets))
             (existing (seq-find (lambda (c) (equal (map-elt c "id") "ears-criteria"))
                                 comments))
             (c-ht (or existing (make-hash-table :test #'equal))))
        (setf (map-elt c-ht "id") "ears-criteria")
        (setf (map-elt c-ht "origin") "code-review")
        (setf (map-elt c-ht "author") "code-review-criteria")
        (setf (map-elt c-ht "at") (format-time-string "%FT%T%:z"))
        (setf (map-elt c-ht "body") body)
        (unless existing
          (setf (map-elt c-ht "resolved") :false)
          (setf (map-elt c-ht "replies") [])
          (setq comments (cons c-ht comments)))
        (setf (map-elt data "ears_criteria")
              (vconcat
               (mapcar
                (lambda (crit)
                  (let ((ht (make-hash-table :test #'equal)))
                    (setf (map-elt ht "id") (or (plist-get crit :id) (map-elt crit 'id) ""))
                    (setf (map-elt ht "statement")
                          (or (plist-get crit :statement) (map-elt crit 'statement) ""))
                    ht))
                criteria-list)))))
    (dolist (lc local-comments)
      (let* ((cid (or (plist-get lc :id) (map-elt lc 'internal-id)
                      (magit-dash-review--make-comment-id
                       (format "%s:%s" (or (plist-get lc :path) (map-elt lc 'path) "")
                               (or (plist-get lc :body) (map-elt lc 'bodyText) "")))))
             (already (seq-find (lambda (c) (equal (map-elt c "id") cid)) comments)))
        (unless already
          (let ((c-ht (make-hash-table :test #'equal))
                (path (or (plist-get lc :path) (map-elt lc 'path)))
                (line (or (plist-get lc :line) (map-elt lc 'line)))
                (body (or (plist-get lc :body) (map-elt lc 'bodyText) "")))
            (setf (map-elt c-ht "id") cid)
            (setf (map-elt c-ht "origin") "code-review")
            (when path (setf (map-elt c-ht "path") path))
            (when line (setf (map-elt c-ht "lines") (vector line line)))
            (setf (map-elt c-ht "author") (or (plist-get lc :author) user-login-name))
            (setf (map-elt c-ht "at") (format-time-string "%FT%T%:z"))
            (setf (map-elt c-ht "body") body)
            (setf (map-elt c-ht "resolved") :false)
            (setf (map-elt c-ht "replies") [])
            (setq comments (append comments (list c-ht)))))))
    (setf (map-elt data "comments") (vconcat comments))
    (magit-dash-review--write-json-file file data)
    (magit-dash-review--refresh-visiting-buffer file)
    data))

;;;###autoload
(defun magit-dash-review-open-backend (&optional review-file)
  "Open the companion `code-review' buffer for REVIEW-FILE."
  (interactive)
  (let* ((file (expand-file-name
                (or review-file
                    (magit-dash-review--current-review-file)
                    (user-error "No active review JSON file"))))
         (data (magit-dash-review--read-json-file file))
         (md (map-elt data "magit_dash"))
         (url (and (hash-table-p md) (map-elt md "url")))
         (repo (and (hash-table-p md) (map-elt md "repo")))
         (num (and (hash-table-p md) (map-elt md "number"))))
    (cond
     ((and url (fboundp 'code-review-start))
      (code-review-start url))
     ((and repo num (fboundp 'code-review-start))
      (code-review-start (format "https://github.com/%s/pull/%d" repo num)))
     ((fboundp 'magit-dash-pr-dashboard-code-review)
      (funcall 'magit-dash-pr-dashboard-code-review
               (list :repo repo :number num)))
     (t
      (user-error "Cannot open code-review backend for %s" file)))))

;;; ACR Unified Review Selector

(defun magit-dash-review--collect-candidates (&optional root fetch-remote)
  "Collect unified review candidates for repository at ROOT.
Returns a list of plists `(:key :annotation :type :action :target)'."
  (let* ((dir (and (or root default-directory)
                   (file-directory-p (or root default-directory))
                   (file-name-as-directory (expand-file-name (or root default-directory)))))
         (in-git-p (and dir
                        (zerop (call-process "git" nil nil nil "-C" dir
                                             "rev-parse" "--is-inside-work-tree"))))
         (candidates nil))

    ;; 1. Uncommitted working-tree diff
    (when in-git-p
      (let ((status-out (string-trim
                         (with-output-to-string
                           (with-current-buffer standard-output
                             (call-process "git" nil t nil "-C" dir "status" "--porcelain"))))))
        (unless (string-empty-p status-out)
          (let ((mod-count (length (split-string status-out "\n" t))))
            (push (list :key "worktree (dirty)"
                        :annotation (format "[worktree] %d modified file%s"
                                            mod-count (if (= mod-count 1) "" "s"))
                        :type 'worktree
                        :target dir
                        :action (lambda () (magit-dash-review-worktree-diff dir)))
                  candidates)))))

    ;; 2. Active <git-common-dir>/review/*.json files
    (when in-git-p
      (let* ((common-dir (ignore-errors (magit-dash-review--common-dir dir)))
             (review-dir (and common-dir (expand-file-name "review" common-dir)))
             (json-files (and review-dir (file-directory-p review-dir)
                              (directory-files review-dir t "\\.json\\'"))))
        (dolist (jf json-files)
          (ignore-errors
            (let* ((data (magit-dash-review--read-json-file jf))
                   (name (file-name-sans-extension (file-name-nondirectory jf)))
                   (viewed-ht (map-elt data "viewed"))
                   (viewed-n (if (hash-table-p viewed-ht) (hash-table-count viewed-ht) 0))
                   (comments (append (or (map-elt data "comments") []) nil))
                   (open-n (seq-count (lambda (c) (not (eq (map-elt c "resolved") t))) comments))
                   (draft-n (seq-count (lambda (c) (equal (map-elt c "status") "draft")) comments))
                   (verdict-ht (map-elt data "verdict"))
                   (v-state (and (hash-table-p verdict-ht) (map-elt verdict-ht "state"))))
              (push (list :key (format "review:%s" name)
                          :annotation (format "[review] %d viewed  %d open%s%s"
                                              viewed-n open-n
                                              (if (> draft-n 0) (format " (%d draft)" draft-n) "")
                                              (if v-state (format "  [%s]" v-state) ""))
                          :type 'review-file
                          :target jf
                          :action (lambda () (magit-dash-review--open-front-end jf)))
                    candidates))))))

    ;; 3. Local worktree branches
    (when in-git-p
      (dolist (wt (ignore-errors (magit-dash-pr--git-worktrees dir)))
        (let ((branch (alist-get 'branch wt))
              (wt-path (alist-get 'path wt)))
          (when (and branch (not (member branch '("main" "master"))))
            (push (list :key (format "branch:%s" branch)
                        :annotation (format "[branch] %s" (or wt-path ""))
                        :type 'branch
                        :target branch
                        :action (lambda ()
                                  (if (fboundp 'diff-review-branch)
                                      (let ((default-directory dir))
                                        (diff-review-branch branch))
                                    (magit-dash-review-worktree-diff (or wt-path dir)))))
                  candidates)))))

    ;; 4. Cached or remote GitHub PRs
    (when (and in-git-p fetch-remote)
      (let* ((slug (ignore-errors
                     (let ((default-directory dir))
                       (magit-dash-pr-dashboard--current-repo-slug))))
             (prs (and slug (magit-dash-pr--fetch-repo-prs slug "open" dir))))
        (dolist (pr prs)
          (let ((num (map-elt pr 'number))
                (title (or (map-elt pr 'title) ""))
                (head (or (map-elt pr 'headRefName) "")))
            (when num
              (push (list :key (format "pr-%d" num)
                          :annotation (format "[PR #%d] %s (%s)" num title head)
                          :type 'pr
                          :target num
                          :action (lambda () (magit-dash-review-pr num dir)))
                    candidates))))))

    ;; 5. Staged hitl-plan items in the code-review domain
    (when (fboundp 'hitl-plan-list)
      (dolist (plan (ignore-errors (hitl-plan-list nil 'code-review)))
        (let ((st (hitl-plan-status plan)))
          (unless (memq st '(complete discarded))
            (let* ((pid (hitl-plan-id plan))
                   (desc (or (hitl-plan-description plan) ""))
                   (steps (hitl-plan-steps plan))
                   (pending (seq-count (lambda (s) (eq (hitl-plan-step-status s) 'pending))
                                       steps)))
              (push (list :key (format "hitl:%s" pid)
                          :annotation (format "[HITL] %d pending steps  %s (%s)"
                                              pending desc st)
                          :type 'hitl-plan
                          :target plan
                          :action (lambda ()
                                    (when (fboundp 'hitl-plan-review)
                                      (hitl-plan-review plan t))
                                    plan))
                    candidates))))))

    (nreverse candidates)))

;;;###autoload
(defun magit-dash-review-select (&optional fetch-remote)
  "Interactively select and open a review target using `annotated-completing-read'.
Aggregates dirty working-tree diffs, active `.git/review/*.json' files,
local worktree branches, GitHub PRs (fetched live with prefix arg
FETCH-REMOTE), and pending `code-review' `hitl-plan' items."
  (interactive "P")
  (let ((candidates (magit-dash-review--collect-candidates default-directory fetch-remote)))
    (unless candidates
      (user-error "No active review targets found"))
    (let* ((table (mapcar (lambda (c)
                            (cons (plist-get c :key) (plist-get c :annotation)))
                          candidates))
           (choice (if (fboundp 'annotated-completing-read)
                       (annotated-completing-read
                        table
                        :prompt "Review target: "
                        :require-match t
                        :category 'magit-dash-review)
                     (completing-read "Review target: " table nil t)))
           (matched (seq-find (lambda (c) (equal (plist-get c :key) choice))
                              candidates)))
      (unless matched
        (user-error "Unknown review target: %s" choice))
      (funcall (plist-get matched :action)))))

(provide 'magit-dash-review)
;;; magit-dash-review.el ends here
