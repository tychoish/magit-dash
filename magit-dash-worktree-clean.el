;;; magit-dash-worktree-clean.el --- Plan and execute merged-worktree cleanup via hitl-plan -*- lexical-binding: t -*-

;;; Commentary:

;; Deleting the worktrees of merged (or closed) pull requests used to ask a
;; `y-or-n-p' question per worktree while GitHub was being queried.  This
;; module splits that into two phases, like the ELPAish package install flow:
;;
;; 1. Plan: scan the repository's worktrees, match each to its PR, and turn
;;    every merged or closed one into a step of a `hitl-plan' in the
;;    `magit-dash-worktree-clean' domain.  Worktrees with uncommitted changes
;;    are listed too, but pre-marked `skipped'.
;; 2. Execute: review the plan in the `hitl-plan' buffer (skip steps with `m',
;;    run everything with `x', or one step with `o'), then delete.  Progress
;;    is saved after every step, so `C-g' pauses and `hitl-plan-resume'
;;    continues.
;;
;; Only the worktree is removed; the branch is kept until branch pruning.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'map)
(require 'hitl-plan)

(declare-function hitl-plan-review "hitl-plan-ui")
(declare-function magit-dash-worktree-main-dir "magit-dash-worktree")
(declare-function magit-dash-worktree-dirty-many "magit-dash-worktree")
(declare-function magit-dash-worktree-uncommitted-changes-p "magit-dash-worktree")
(declare-function magit-dash-pr--fetch-repo-prs "magit-dash-pr")
(declare-function magit-dash-pr--fetch-pr "magit-dash-pr")
(declare-function magit-dash-pr--git-worktrees "magit-dash-pr")
(declare-function magit-dash-pr--match-worktree-by-branch "magit-dash-pr")
(declare-function magit-dash-pr--match-worktree-to-pr "magit-dash-pr")
(declare-function magit-dash-pr--path-equal-p "magit-dash-pr")
(declare-function magit-dash-pr--pr-merged-p "magit-dash-pr")
(declare-function magit-dash-pr--find-existing-worktree "magit-dash-pr")
(declare-function magit-dash-pr-delete-worktree "magit-dash-pr")
(declare-function magit-dash-pr-dashboard--current-repo-slug "magit-dash-pr")
(declare-function magit-dash-pr-dashboard--find-local-path "magit-dash-pr")
(declare-function magit-dash-pr-dashboard-refresh "magit-dash-pr")
(declare-function magit-toplevel "magit-git")

(defgroup magit-dash-worktree-clean nil
  "Plan-then-execute cleanup of merged pull request worktrees."
  :group 'magit-dash)

(defcustom magit-dash-worktree-clean-plan-directory nil
  "Directory holding worktree cleanup plans.
When nil, a subdirectory of `hitl-plan-directory'."
  :type '(choice (const :tag "Under hitl-plan-directory" nil) directory)
  :group 'magit-dash-worktree-clean)

(defconst magit-dash-worktree-clean--number-regexp
  "\\(?:\\`\\|-\\)pr-\\(?:[[:alnum:]]+-\\)*\\([0-9]+\\)\\'"
  "Regexp extracting a PR number from a worktree directory name.")

;;;; Discovery

(defun magit-dash-worktree-clean--closed-state (pr slug number repo-dir)
  "Return \"merged\", \"closed\" or nil for PR (number NUMBER) in SLUG."
  (cond
   ((magit-dash-pr--pr-merged-p pr slug number repo-dir)
    "merged")
   ((equal (upcase (or (map-elt pr 'state) "")) "CLOSED")
    "closed")))

(defun magit-dash-worktree-clean--match (wt prs slug repo-dir)
  "Return the PR matching worktree WT among PRS, fetching when needed, or nil."
  (let* ((wt-name (file-name-nondirectory
                   (directory-file-name (alist-get 'path wt))))
         (number (when (string-match magit-dash-worktree-clean--number-regexp wt-name)
                   (string-to-number (match-string 1 wt-name))))
         (open-prs (seq-filter (lambda (pr)
                                 (equal (upcase (or (map-elt pr 'state) "")) "OPEN"))
                               prs)))
    (or (magit-dash-pr--match-worktree-by-branch wt prs slug repo-dir)
        (magit-dash-pr--match-worktree-to-pr wt open-prs)
        (when number
          (or (seq-find (lambda (pr) (equal (map-elt pr 'number) number)) prs)
              (magit-dash-pr--fetch-pr slug number repo-dir)))
        (magit-dash-pr--match-worktree-to-pr wt prs))))

(defun magit-dash-worktree-clean--target (wt pr state slug repo-dir)
  "Return the plan target plist for worktree WT, matched to PR in STATE."
  (let ((path (alist-get 'path wt)))
    (list :worktree path
          :repo-dir repo-dir
          :slug slug
          :number (map-elt pr 'number)
          :branch (alist-get 'branch wt)
          :state state)))

(defun magit-dash-worktree-clean--mark-dirty (targets)
  "Return TARGETS with `:dirty' set from one concurrent `git status' sweep."
  (let ((dirty (magit-dash-worktree-dirty-many
                (seq-map (lambda (target) (plist-get target :worktree)) targets))))
    (seq-map (lambda (target)
               (plist-put (copy-sequence target) :dirty
                          (and (cdr (assoc (plist-get target :worktree) dirty)) t)))
             targets)))

(defun magit-dash-worktree-clean-scan (repo-dir)
  "Return plan targets for the merged or closed PR worktrees of REPO-DIR."
  (let* ((repo-dir (expand-file-name repo-dir))
         (slug (or (ignore-errors
                     (let ((default-directory repo-dir))
                       (magit-dash-pr-dashboard--current-repo-slug)))
                   (file-name-nondirectory (directory-file-name repo-dir))))
         (prs (magit-dash-pr--fetch-repo-prs slug "all" repo-dir)))
    (thread-last
      (magit-dash-pr--git-worktrees repo-dir)
      (seq-filter (lambda (wt)
                    (when-let* ((path (alist-get 'path wt)))
                      (and (file-directory-p path)
                           (not (magit-dash-pr--path-equal-p path repo-dir))))))
      (seq-keep (lambda (wt)
                  (when-let* ((pr (magit-dash-worktree-clean--match wt prs slug repo-dir))
                              (state (magit-dash-worktree-clean--closed-state
                                      pr slug (map-elt pr 'number) repo-dir)))
                    (magit-dash-worktree-clean--target wt pr state slug repo-dir))))
      (magit-dash-worktree-clean--mark-dirty))))

(defun magit-dash-worktree-clean-entry-target (entry)
  "Return a plan target for dashboard PR ENTRY, or nil when it has no worktree.
Open PRs are not candidates, so nil is returned for them too."
  (when-let* ((repo (plist-get entry :repo))
              (local-path (magit-dash-pr-dashboard--find-local-path repo))
              (repo-dir (expand-file-name local-path))
              (number (plist-get entry :number))
              (pr-data (plist-get entry :pr))
              (path (magit-dash-pr--find-existing-worktree
                     repo-dir number (map-elt pr-data 'headRefName)))
              ((file-directory-p path))
              (state (magit-dash-worktree-clean--closed-state
                      pr-data repo number repo-dir)))
    (car (magit-dash-worktree-clean--mark-dirty
          (list (magit-dash-worktree-clean--target
                 `((path . ,path) (branch . ,(map-elt pr-data 'headRefName)))
                 pr-data state repo repo-dir))))))

;;;; Domain and operation

(defun magit-dash-worktree-clean--directory ()
  "Return the directory in which cleanup plans are stored."
  (file-name-as-directory
   (expand-file-name (or magit-dash-worktree-clean-plan-directory
                         (expand-file-name "magit-dash-worktree-clean/"
                                           hitl-plan-directory)))))

(defun magit-dash-worktree-clean--format-target (target)
  "Return (FILE . LABEL) columns for TARGET."
  (cons (abbreviate-file-name (plist-get target :worktree))
        (format "PR #%s %s%s"
                (or (plist-get target :number) "?")
                (plist-get target :state)
                (if (plist-get target :dirty) ", uncommitted changes" ""))))

(defun magit-dash-worktree-clean--visit-target (target &optional other-window)
  "Visit TARGET's worktree in Dired, in another window when OTHER-WINDOW."
  (when-let* ((dir (plist-get target :worktree))
              ((file-directory-p dir)))
    (if other-window
        (dired-other-window dir)
      (dired dir))))

(defun magit-dash-worktree-clean--sanitize-target (_target)
  "Refuse caller-supplied targets: cleanup plans only come from discovery."
  (user-error "Worktree cleanup plans are built by scanning, not from explicit targets"))

(defun magit-dash-worktree-clean--after-steps-added (_plan steps)
  "Pre-skip the STEPS whose worktree has uncommitted changes."
  (seq-do (lambda (step)
            (when (plist-get (hitl-plan-step-target step) :dirty)
              (setf (hitl-plan-step-status step) 'skipped
                    (hitl-plan-step-message step) "uncommitted changes")))
          steps))

(defun magit-dash-worktree-clean--apply ()
  "Delete the current step's worktree, refusing when it became dirty."
  (let* ((target hitl-plan-current-target)
         (path (plist-get target :worktree)))
    (when (magit-dash-worktree-uncommitted-changes-p path)
      (error "Worktree %s has uncommitted changes" path))
    (magit-dash-pr-delete-worktree path (plist-get target :repo-dir))))

(defun magit-dash-worktree-clean--after-apply (_file _steps plan)
  "Refresh a live PR dashboard once cleanup PLAN has run."
  (when (and (hitl-plan-in-domain-p plan 'magit-dash-worktree-clean)
             (get-buffer "*magit-dash-prs*"))
    (with-current-buffer "*magit-dash-prs*"
      (when (derived-mode-p 'magit-dash-pr-dashboard-mode)
        (magit-dash-pr-dashboard-refresh)))))

(hitl-plan-register-domain 'magit-dash-worktree-clean
  :label "Merged worktree cleanup"
  :resolve-target #'identity
  :format-target #'magit-dash-worktree-clean--format-target
  :visit-target #'magit-dash-worktree-clean--visit-target
  :directory #'magit-dash-worktree-clean--directory
  :after-steps-added #'magit-dash-worktree-clean--after-steps-added
  :sanitize-target #'magit-dash-worktree-clean--sanitize-target)

(hitl-plan-register-op 'magit-dash-worktree-clean 'delete-worktree
  :key ?d
  :label "delete worktree"
  :apply #'magit-dash-worktree-clean--apply
  :describe (lambda (&rest _) "remove the worktree (branch kept until pruning)")
  :schema '("none: each target names its own worktree"))

(add-hook 'hitl-plan-after-apply-functions #'magit-dash-worktree-clean--after-apply)

;;;; Entry points

;;;###autoload
(defun magit-dash-worktree-clean-plan-targets (targets &optional description)
  "Create a cleanup plan for TARGETS and open it for review.
Return the plan, or nil (after saying so) when TARGETS is empty."
  (if (null targets)
      (progn
        (message "magit-gh: no merged or closed PR worktrees to clean")
        nil)
    (let ((plan (hitl-plan-create
                 :domain 'magit-dash-worktree-clean
                 :targets targets
                 :ops '(delete-worktree)
                 :description (or description "Delete worktrees of merged or closed PRs"))))
      (require 'hitl-plan-ui)
      (hitl-plan-review plan)
      plan)))

;;;###autoload
(defun magit-dash-worktree-clean-plan (&optional repo-dir)
  "Plan deletion of merged or closed PR worktrees in REPO-DIR and review it.
Execute from the review buffer: `x' runs every pending step, `m' skips the
step at point."
  (interactive)
  (let ((repo-dir (expand-file-name
                   (or repo-dir
                       (magit-dash-worktree-main-dir)
                       (magit-toplevel)
                       default-directory))))
    (message "magit-gh: scanning worktrees in %s..." repo-dir)
    (magit-dash-worktree-clean-plan-targets
     (magit-dash-worktree-clean-scan repo-dir)
     (format "Delete worktrees of merged or closed PRs in %s"
             (abbreviate-file-name repo-dir)))))

(provide 'magit-dash-worktree-clean)
;;; magit-dash-worktree-clean.el ends here
