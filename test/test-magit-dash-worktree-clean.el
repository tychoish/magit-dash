;;; test-magit-dash-worktree-clean.el --- ERT tests for magit-dash-worktree-clean -*- lexical-binding: t; no-byte-compile: t; -*-

;; Batch run:
;;   emacs --batch -l test/test-helper.el -l test/test-magit-dash-worktree-clean.el \
;;     --eval '(ert-run-tests-batch-and-exit "magit-dash-worktree-clean/")'

(require 'ert)
(require 'cl-lib)
(require 'test-helper)
(require 'magit-dash)
(require 'magit-dash-review)
(require 'magit-dash-worktree-clean)
(require 'hitl-plan-ui)

(defmacro magit-dash-worktree-clean-test--with-store (&rest body)
  "Run BODY with plans stored in a throwaway directory."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "mdwc-test-" t))
          (magit-dash-worktree-clean-plan-directory dir))
     (unwind-protect (progn ,@body)
       (delete-directory dir t))))

(defun magit-dash-worktree-clean-test--target (name dirty)
  (list :worktree (concat "/tmp/" name) :repo-dir "/tmp/repo" :slug "o/r"
        :number 7 :branch name :state "merged" :dirty dirty))

(ert-deftest magit-dash-worktree-clean/dirty-steps-are-preskipped ()
  (magit-dash-worktree-clean-test--with-store
    (let* ((plan (hitl-plan-create
                  :domain 'magit-dash-worktree-clean
                  :targets (list (magit-dash-worktree-clean-test--target "clean" nil)
                                 (magit-dash-worktree-clean-test--target "dirty" t))
                  :ops '(delete-worktree)))
           (statuses (seq-map #'hitl-plan-step-status (hitl-plan-steps plan))))
      (should (equal '(pending skipped) statuses)))))

(ert-deftest magit-dash-worktree-clean/execute-deletes-only-pending ()
  (magit-dash-worktree-clean-test--with-store
    (let ((deleted nil))
      (cl-letf (((symbol-function 'magit-dash-worktree-clean--verify-target) #'ignore)
                ((symbol-function 'magit-dash-worktree-uncommitted-changes-p) #'ignore)
                ((symbol-function 'magit-dash-pr-delete-worktree)
                 (lambda (path &rest _) (push path deleted) t)))
        (let ((plan (hitl-plan-create
                     :domain 'magit-dash-worktree-clean
                     :targets (list (magit-dash-worktree-clean-test--target "clean" nil)
                                    (magit-dash-worktree-clean-test--target "dirty" t))
                     :ops '(delete-worktree))))
          (hitl-plan-execute plan)
          (should (equal '("/tmp/clean") deleted))
          (should (equal '(done skipped)
                         (seq-map #'hitl-plan-step-status (hitl-plan-steps plan)))))))))

(ert-deftest magit-dash-worktree-clean/apply-refuses-newly-dirty-worktree ()
  (magit-dash-worktree-clean-test--with-store
    (cl-letf (((symbol-function 'magit-dash-worktree-clean--verify-target) #'ignore)
              ((symbol-function 'magit-dash-worktree-uncommitted-changes-p) (lambda (_) t))
              ((symbol-function 'magit-dash-pr-delete-worktree)
               (lambda (&rest _) (error "must not delete"))))
      (let ((plan (hitl-plan-create
                   :domain 'magit-dash-worktree-clean
                   :targets (list (magit-dash-worktree-clean-test--target "late" nil))
                   :ops '(delete-worktree))))
        (hitl-plan-execute plan)
        (let ((step (car (hitl-plan-steps plan))))
          (if (fboundp 'hitl-plan-skip)
              (should (eq 'skipped (hitl-plan-step-status step)))
            (should (eq 'failed (hitl-plan-step-status step))))
          (should (equal "uncommitted changes" (hitl-plan-step-message step))))))))

(ert-deftest magit-dash-worktree-clean/empty-targets-make-no-plan ()
  (should-not (magit-dash-worktree-clean-plan-targets nil)))

(defun magit-dash-worktree-clean-test--match (name branch pr)
  "Match a worktree called NAME on BRANCH against merged PR, fetching PR."
  (let* ((dir (make-temp-file "mdwc-wt-" t))
         (path (expand-file-name name dir)))
    (make-directory path)
    (unwind-protect
        (cl-letf (((symbol-function 'magit-dash-pr--fetch-prs-for-branch) #'ignore)
                  ((symbol-function 'magit-dash-pr--fetch-pr) (lambda (&rest _) pr)))
          (magit-dash-worktree-clean--match
           `((path . ,path) (branch . ,branch)) nil "o/r" dir))
      (delete-directory dir t))))

(ert-deftest magit-dash-worktree-clean/number-only-match-needs-corroboration ()
  (let ((pr '((number . 5) (headRefName . "feat") (state . "MERGED"))))
    (should-not (magit-dash-worktree-clean-test--match "cr-ticket-5" "my-work" pr))
    (should-not (magit-dash-worktree-clean-test--match "xcr-5" "my-work" pr))
    (should-not (magit-dash-worktree-clean-test--match "cr-5" "my-work" pr))
    (should (magit-dash-worktree-clean-test--match "cr-5" "cr-5" pr))
    (should (magit-dash-worktree-clean-test--match "cr-ticket-5" "wt/feat" pr))))

;;; Safety: stale plans, risks and matching, with throwaway repositories

(defmacro magit-dash-worktree-clean-test--with-repo (vars &rest body)
  "Bind (REPO WT) of VARS to a repo and its linked worktree on branch feat."
  (declare (indent 1))
  `(let* ((,(car vars) (magit-dash-test--make-repo))
          (parent (make-temp-file "mdtest-wt-" t))
          (,(cadr vars) (expand-file-name "pr-7/" parent)))
     (unwind-protect
         (progn
           (magit-dash-test--git ,(car vars) "worktree" "add" "-q" "-b" "feat" ,(cadr vars) "HEAD")
           ,@body)
       (delete-directory ,(car vars) t)
       (delete-directory parent t))))

(defun magit-dash-worktree-clean-test--planned (repo wt)
  "Return a plan target for WT of REPO as discovery would build it."
  (list :worktree wt :repo-dir repo :slug "o/r" :number 7 :branch "feat"
        :head (magit-dash-test--git wt "rev-parse" "HEAD") :state "merged"))

(defmacro magit-dash-worktree-clean-test--with-merged-pr (&rest body)
  "Run BODY with GitHub reporting PR 7 merged."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'magit-dash-pr--fetch-pr)
              (lambda (&rest _) '((number . 7) (headRefName . "feat") (state . "MERGED")))))
     ,@body))

(ert-deftest magit-dash-worktree-clean/verify-accepts-unchanged-target ()
  (magit-dash-worktree-clean-test--with-repo (repo wt)
    (magit-dash-worktree-clean-test--with-merged-pr
      (should-not (magit-dash-worktree-clean--verify-target
                   (magit-dash-worktree-clean-test--planned repo wt))))))

(ert-deftest magit-dash-worktree-clean/verify-rejects-recreated-worktree-and-moved-head ()
  (magit-dash-worktree-clean-test--with-repo (repo wt)
    (magit-dash-worktree-clean-test--with-merged-pr
      (let ((target (magit-dash-worktree-clean-test--planned repo wt)))
        ;; HEAD moved after the plan.
        (magit-dash-test--commit wt "later.txt")
        (should (string-match-p "HEAD moved" (magit-dash-worktree-clean--verify-target target)))
        ;; Worktree removed and recreated on another branch at the same path.
        (magit-dash-test--git repo "worktree" "remove" "--force" wt)
        (magit-dash-test--git repo "worktree" "add" "-q" "-b" "other" wt "HEAD")
        (should (string-match-p "branch changed" (magit-dash-worktree-clean--verify-target target)))
        ;; Worktree gone altogether.
        (magit-dash-test--git repo "worktree" "remove" "--force" wt)
        (should (string-match-p "no longer a linked worktree"
                                (magit-dash-worktree-clean--verify-target target)))))))

(ert-deftest magit-dash-worktree-clean/verify-rejects-reopened-pr ()
  (magit-dash-worktree-clean-test--with-repo (repo wt)
    (cl-letf (((symbol-function 'magit-dash-pr--fetch-pr)
               (lambda (&rest _) '((number . 7) (state . "OPEN")))))
      (should (string-match-p "no longer known"
                              (magit-dash-worktree-clean--verify-target
                               (magit-dash-worktree-clean-test--planned repo wt)))))))

(ert-deftest magit-dash-worktree-clean/apply-skips-stale-target-and-keeps-worktree ()
  (magit-dash-worktree-clean-test--with-repo (repo wt)
    (magit-dash-worktree-clean-test--with-merged-pr
      (magit-dash-worktree-clean-test--with-store
        (let* ((stale (plist-put (magit-dash-worktree-clean-test--planned repo wt)
                                 :branch "gone"))
               (hitl-plan-current-target stale))
          (if (fboundp 'hitl-plan-skip)
              (let ((res (magit-dash-worktree-clean--apply)))
                (should (hitl-plan-skip-p res))
                (should (string-match-p "branch changed" (cdr res))))
            (should-error (magit-dash-worktree-clean--apply)))
          (should (file-directory-p wt))
          (let ((plan (hitl-plan-create
                       :domain 'magit-dash-worktree-clean
                       :targets (list stale)
                       :ops '(delete-worktree))))
            (hitl-plan-execute plan)
            (let ((step (car (hitl-plan-steps plan))))
              (if (fboundp 'hitl-plan-skip)
                  (should (eq 'skipped (hitl-plan-step-status step)))
                (should (eq 'failed (hitl-plan-step-status step))))
              (should (string-match-p "branch changed" (hitl-plan-step-message step))))
            (should (eq 'complete (hitl-plan-status plan)))
            (should (file-directory-p wt))))))))

(ert-deftest magit-dash-worktree-clean/execute-skips-declined-and-unattended-risks ()
  (magit-dash-worktree-clean-test--with-repo (repo wt)
    (magit-dash-worktree-clean-test--with-merged-pr
      (magit-dash-worktree-clean-test--with-store
        (with-temp-file (expand-file-name "keep.secret" wt) (insert "s"))
        (let ((target (magit-dash-worktree-clean-test--planned repo wt))
              (expected-refusal (if (fboundp 'hitl-plan-skip)
                                    'skipped
                                  'failed)))
          (let ((magit-dash-worktree-unattended t)
                (magit-dash-worktree-unattended-risk-policy 'refuse))
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (&rest _) (error "must not prompt"))))
              (let ((plan (hitl-plan-create
                           :domain 'magit-dash-worktree-clean
                           :targets (list target)
                           :ops '(delete-worktree))))
                (hitl-plan-execute plan)
                (let ((step (car (hitl-plan-steps plan))))
                  (should (eq expected-refusal (hitl-plan-step-status step)))
                  (should (string-match-p "no one to confirm"
                                          (hitl-plan-step-message step))))
                (should (file-exists-p (expand-file-name "keep.secret" wt))))))
          (let ((magit-dash-worktree-unattended-risk-policy 'ask))
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
              (let ((plan (hitl-plan-create
                           :domain 'magit-dash-worktree-clean
                           :targets (list target)
                           :ops '(delete-worktree))))
                (hitl-plan-execute plan)
                (let ((step (car (hitl-plan-steps plan))))
                  (should (eq expected-refusal (hitl-plan-step-status step)))
                  (should (string-match-p "confirmation declined"
                                          (hitl-plan-step-message step)))
                  (should (string-match-p "keep.secret"
                                          (hitl-plan-step-message step))))
                (should (file-exists-p (expand-file-name "keep.secret" wt)))))
            (let ((prompts 0))
              (cl-letf (((symbol-function 'yes-or-no-p)
                         (lambda (&rest _)
                           (setq prompts (1+ prompts))
                           t)))
                (let ((plan (hitl-plan-create
                             :domain 'magit-dash-worktree-clean
                             :targets (list target)
                             :ops '(delete-worktree))))
                  (hitl-plan-execute plan)
                  (should (= 1 prompts))
                  (should (eq 'done (hitl-plan-step-status (car (hitl-plan-steps plan)))))
                  (should-not (file-directory-p wt)))))))))))

(ert-deftest magit-dash-worktree-clean/execute-fails-on-git-error ()
  (magit-dash-worktree-clean-test--with-repo (repo wt)
    (magit-dash-worktree-clean-test--with-merged-pr
      (magit-dash-worktree-clean-test--with-store
        (magit-dash-test--git repo "worktree" "lock" wt)
        (let ((plan (hitl-plan-create
                     :domain 'magit-dash-worktree-clean
                     :targets (list (magit-dash-worktree-clean-test--planned repo wt))
                     :ops '(delete-worktree))))
          (hitl-plan-execute plan)
          (let ((step (car (hitl-plan-steps plan))))
            (should (eq 'failed (hitl-plan-step-status step)))
            (should (string-match-p "Failed to remove worktree"
                                    (hitl-plan-step-message step))))
          (should (file-directory-p wt)))))))

(ert-deftest magit-dash-worktree-clean/format-distinguishes-needs-confirmation-and-skipped ()
  (magit-dash-worktree-clean-test--with-store
    (let* ((clean (list :worktree "/tmp/wt-clean" :repo-dir "/tmp/repo"
                        :slug "o/r" :number 7 :state "merged"))
           (risky (list :worktree "/tmp/wt-risky" :repo-dir "/tmp/repo"
                        :slug "o/r" :number 8 :state "merged"
                        :risks '("1 ignored file(s) (keep.secret)")))
           (dirty (list :worktree "/tmp/wt-dirty" :repo-dir "/tmp/repo"
                        :slug "o/r" :number 9 :state "merged" :dirty t))
           (stale (list :worktree "/tmp/wt-stale" :repo-dir "/tmp/repo"
                        :slug "o/r" :number 10 :state "merged"))
           (plan (hitl-plan-create
                  :domain 'magit-dash-worktree-clean
                  :targets (list clean risky dirty stale)
                  :ops '(delete-worktree)))
           (can-skip (fboundp 'hitl-plan-skip)))
      (cl-letf (((symbol-function 'magit-dash-worktree-clean--verify-target)
                 (lambda (target)
                   (when (equal (plist-get target :worktree) "/tmp/wt-stale")
                     "HEAD moved since the plan"))))
        (hitl-plan-run-step plan (nth 3 (hitl-plan-steps plan))
                            (car (hitl-plan-ops plan))))
      (let ((buf (hitl-plan-review plan nil)))
        (unwind-protect
            (with-current-buffer buf
              (let ((text (buffer-string)))
                (if can-skip
                    (should (string-match-p "Progress: +0/4 done, 2 pending, 0 failed, 2 skipped" text))
                  (should (string-match-p "Progress: +0/4 done, 2 pending, 1 failed, 1 skipped" text)))
                (should (string-match-p
                         "\\[x\\] +2 \\[pending\\] /tmp/wt-risky +PR #8 merged \\[NEEDS CONFIRMATION: 1 ignored file(s) (keep\\.secret)\\]"
                         text))
                (should (string-match-p
                         "\\[ \\] +3 \\[skipped\\] /tmp/wt-dirty +PR #9 merged \\[dirty\\] — uncommitted changes"
                         text))
                (if can-skip
                    (should (string-match-p
                             "\\[ \\] +4 \\[skipped\\] /tmp/wt-stale +PR #10 merged — HEAD moved since the plan"
                             text))
                  (should (string-match-p
                           "\\[!] +4 \\[failed *\\] /tmp/wt-stale +PR #10 merged — HEAD moved since the plan"
                           text)))
                (goto-char (point-min))
                (re-search-forward "\\[skipped\\].*wt-dirty")
                (should (eq 'hitl-plan-skipped-face
                            (get-text-property (match-beginning 0) 'face)))
                (goto-char (point-min))
                (if can-skip
                    (progn
                      (re-search-forward "\\[skipped\\].*wt-stale")
                      (should (eq 'hitl-plan-skipped-face
                                  (get-text-property (match-beginning 0) 'face))))
                  (re-search-forward "\\[failed *\\].*wt-stale")
                  (should (eq 'hitl-plan-failed-face
                              (get-text-property (match-beginning 0) 'face))))))
          (when (buffer-live-p buf)
            (kill-buffer buf)))))))

(ert-deftest magit-dash-worktree-clean/delete-refuses-ignored-file-unless-confirmed ()
  (magit-dash-worktree-clean-test--with-repo (repo wt)
    (magit-dash-test--git repo "config" "remote.origin.url" "/nonexistent")
    (magit-dash-test--git repo "config" "remote.origin.fetch" "+refs/heads/*:refs/remotes/origin/*")
    (magit-dash-test--git repo "update-ref" "refs/remotes/origin/feat" "HEAD")
    (magit-dash-test--git repo "config" "branch.feat.remote" "origin")
    (magit-dash-test--git repo "config" "branch.feat.merge" "refs/heads/feat")
    (with-temp-file (expand-file-name "keep.secret" wt) (insert "s"))
    (let ((magit-dash-worktree-unattended-risk-policy 'ask))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
        (should-error (magit-dash-pr-delete-worktree wt repo) :type 'user-error)
        (should (file-exists-p (expand-file-name "keep.secret" wt))))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (should (magit-dash-pr-delete-worktree wt repo))
        (should-not (file-directory-p wt))))))

(ert-deftest magit-dash-worktree-clean/delete-refuses-detached-commit-and-unpushed-work ()
  (magit-dash-worktree-clean-test--with-repo (repo wt)
    (magit-dash-test--commit wt "unpushed.txt")
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
      (should-error (magit-dash-pr-delete-worktree wt repo) :type 'user-error)
      (magit-dash-test--git wt "checkout" "-q" "--detach" "HEAD")
      (magit-dash-test--git repo "branch" "-D" "feat")
      (should-error (magit-dash-pr-delete-worktree wt repo) :type 'user-error)
      (should (file-directory-p wt)))))

(ert-deftest magit-dash-worktree-clean/review-worktrees-never-deletes-risky-unattended ()
  (magit-dash-worktree-clean-test--with-repo (repo wt)
    (with-temp-file (expand-file-name "keep.secret" wt) (insert "s"))
    (cl-letf (((symbol-function 'magit-dash-pr-dashboard--current-repo-slug) (lambda () "o/r"))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) (error "must not prompt"))))
      (let ((counts (magit-dash-pr-review-worktrees
                     repo nil '(((number . 7) (headRefName . "feat") (state . "MERGED")))
                     t)))
        (should (= 0 (plist-get counts :deleted)))
        (should (= 1 (plist-get counts :skipped)))
        (should (file-directory-p wt))))))

(ert-deftest magit-dash-worktree-clean/scan-marks-risks-on-targets ()
  (magit-dash-worktree-clean-test--with-repo (repo wt)
    (with-temp-file (expand-file-name "keep.secret" wt) (insert "s"))
    (let ((target (car (magit-dash-worktree-clean--mark-dirty
                        (list (magit-dash-worktree-clean-test--planned repo wt))))))
      (should (plist-get target :risks))
      (should-not (plist-get target :dirty))
      (should (string-match-p "NEEDS CONFIRMATION"
                              (cdr (magit-dash-worktree-clean--format-target target)))))))

;;; Matching safety

(defun magit-dash-worktree-clean-test--remote (repo url)
  "Give REPO an origin remote URL."
  (magit-dash-test--git repo "remote" "add" "origin" url))

(ert-deftest magit-dash-worktree-clean/find-local-path-needs-full-owner-name ()
  (let* ((mine (magit-dash-test--make-repo))
         (magit-dash-repo-list (list (magit-dash-repo--make :name "proj" :path mine))))
    (unwind-protect
        (progn
          (magit-dash-worktree-clean-test--remote mine "git@github.com:me/proj.git")
          (should (equal mine (magit-dash-pr-dashboard--find-local-path "me/proj")))
          (should-not (magit-dash-pr-dashboard--find-local-path "upstream/proj"))
          (magit-dash-test--git mine "remote" "add" "upstream" "https://github.com/upstream/proj")
          (should (equal mine (magit-dash-pr-dashboard--find-local-path "upstream/proj"))))
      (delete-directory mine t))))

(ert-deftest magit-dash-worktree-clean/fork-pr-from-main-matches-nothing ()
  (magit-dash-worktree-clean-test--with-repo (repo wt)
    (let ((fork-pr '((number . 9) (headRefName . "main") (state . "MERGED")))
          (patch-pr '((number . 7) (headRefName . "patch-1") (state . "MERGED"))))
      (cl-letf (((symbol-function 'magit-dash-pr--fetch-prs-for-branch) #'ignore)
                ((symbol-function 'magit-dash-pr--fetch-pr) #'ignore))
        ;; Worktree on a branch literally named main: never matched by name.
        (magit-dash-test--git repo "worktree" "add" "-q" "-b" "scratch" (expand-file-name "scratch/" (file-name-directory (directory-file-name wt))) "HEAD")
        (let ((wts (magit-dash-pr--git-worktrees repo)))
          (seq-do (lambda (w)
                    (should-not (magit-dash-pr--match-worktree-corroborated
                                 w (list fork-pr patch-pr) "o/r" repo)))
                  wts))
        ;; The entry lookup refuses the main checkout and generic heads.
        (should-not (magit-dash-pr--find-corroborated-worktree repo 9 fork-pr))
        (should-not (magit-dash-pr--find-existing-worktree repo 99 "main"))))))

(ert-deftest magit-dash-worktree-clean/entry-target-requires-branch-corroboration ()
  (magit-dash-worktree-clean-test--with-repo (repo wt)
    (let ((pr '((number . 7) (headRefName . "feat") (state . "MERGED"))))
      (should (alist-get 'path (magit-dash-pr--find-corroborated-worktree repo 7 pr)))
      ;; Same directory name, but the branch was changed to something unrelated.
      (magit-dash-test--git wt "checkout" "-q" "-b" "my-own-work")
      (should-not (magit-dash-pr--find-corroborated-worktree repo 7 pr)))))

(ert-deftest magit-dash-worktree-clean/acceptable-p-passes-entry-to-path-format ()
  (let* ((seen nil)
         (magit-dash-pr-worktree-path-format
          (lambda (repo-dir name entry)
            (push entry seen)
            (expand-file-name name repo-dir))))
    (should (magit-dash-pr--worktree-acceptable-p "/tmp/r" "/tmp/r/pr-7" 7 '(:number 7 :tag x)))
    (should (equal '(:number 7 :tag x) (car seen)))))

(ert-deftest magit-dash-worktree-clean/move-worktree-relocates-review-file ()
  (magit-dash-worktree-clean-test--with-repo (repo wt)
    (let* ((common (file-name-as-directory (expand-file-name ".git" repo)))
           (file (expand-file-name "review/pr-7.json" common))
           (new (expand-file-name "pr-7-moved/" (file-name-directory (directory-file-name wt))))
           (table (make-hash-table :test #'equal))
           (md (make-hash-table :test #'equal)))
      (setf (map-elt md "repo") "o/r")
      (setf (map-elt md "number") 7)
      (setf (map-elt md "worktree") wt)
      (setf (map-elt table "magit_dash") md)
      (setf (map-elt table "root") wt)
      (magit-dash-review-json-write-file file table)
      (should (magit-dash-pr--move-worktree repo wt new))
      (let ((after (magit-dash-review-json-read-file file)))
        (should (equal new (map-elt after "root")))
        (should (equal new (map-elt (map-elt after "magit_dash") "worktree")))))))

(ert-deftest magit-dash-worktree-clean/create-worktree-does-not-touch-user-branch-of-same-name ()
  (let* ((origin (file-name-as-directory (make-temp-file "mdtest-origin-" t)))
         (repo (magit-dash-test--make-repo))
         (default-directory repo))
    (unwind-protect
        (progn
          (magit-dash-test--git origin "init" "-q" "--bare" "-b" "main")
          (magit-dash-test--git repo "remote" "add" "origin" origin)
          (magit-dash-test--git repo "push" "-q" "origin" "main")
          (magit-dash-test--git repo "checkout" "-q" "-b" "feat")
          (magit-dash-test--commit repo "mine.txt")
          (let ((mine (magit-dash-test--git repo "rev-parse" "feat")))
            (magit-dash-test--git repo "checkout" "-q" "-b" "pr-head" "main")
            (let ((theirs (magit-dash-test--commit repo "theirs.txt")))
              (magit-dash-test--git repo "push" "-q" "origin" "pr-head:refs/pull/5/head")
              (magit-dash-test--git repo "checkout" "-q" "main")
              (cl-letf (((symbol-function 'magit-dash-pr-own-p) (lambda (&rest _) t)))
                (let ((wt (magit-dash-pr--create-worktree-for-entry
                           (list :number 5 :repo "o/r" :repo-dir repo
                                 :pr '((headRefName . "feat")))
                           nil)))
                  (should (file-directory-p wt))
                  (should (equal mine (magit-dash-test--git repo "rev-parse" "feat")))
                  (should (equal theirs (magit-dash-test--git wt "rev-parse" "HEAD")))
                  (should (equal "pr-5" (magit-dash-test--git wt "symbolic-ref" "--short" "HEAD"))))))))
      (delete-directory repo t)
      (delete-directory origin t))))

(ert-deftest magit-dash-worktree-clean/review-pr-aborts-when-worktree-creation-fails ()
  (cl-letf (((symbol-function 'magit-dash-review--fetch-pr-metadata)
             (lambda (&rest _) '((headRefName . "feat") (baseRefName . "main"))))
            ((symbol-function 'magit-dash-pr--find-existing-worktree) #'ignore)
            ((symbol-function 'magit-dash-pr--create-worktree-for-entry)
             (lambda (&rest _) (error "no git today")))
            ((symbol-function 'magit-dash-review--open-front-end)
             (lambda (&rest _) (error "must not open"))))
    (should-error (magit-dash-review-pr 7 "/tmp") :type 'user-error)))

(ert-deftest magit-dash-worktree-clean/squash-merged-scan-and-entry-target-no-risk ()
  "Squash-merged worktrees with no upstream get :pr-head-oid and no commit risk."
  (magit-dash-worktree-clean-test--with-repo (repo wt)
    (magit-dash-worktree-clean-test--with-store
      (let* ((oid (magit-dash-test--commit wt "squash.txt"))
             (pr `((number . 7) (headRefName . "feat")
                   (headRefOid . ,oid) (state . "MERGED"))))
        (cl-letf (((symbol-function 'executable-find) (lambda (_) t))
                  ((symbol-function 'magit-dash-pr-dashboard--current-repo-slug)
                   (lambda () "o/r"))
                  ((symbol-function 'magit-dash-pr-dashboard--find-local-path)
                   (lambda (_) repo))
                  ((symbol-function 'magit-dash-worktree-confirm-risks)
                   (lambda (&rest _) (error "must not prompt")))
                  ((symbol-function 'magit-dash-gh--call-process)
                   (lambda (args _dir)
                     (let* ((fields (cadr (member "--json" args)))
                            (want-oid (and fields (member "headRefOid" (split-string fields ","))))
                            (payload (if want-oid
                                         pr
                                       (seq-remove (lambda (c) (eq (car c) 'headRefOid)) pr))))
                       (cons 0 (json-serialize
                                (if (member "view" args) payload (vector payload))))))))
          ;; 1. entry-target with partial :pr lacking headRefOid fetches it.
          (let ((entry-target (magit-dash-worktree-clean-entry-target
                               '(:number 7 :repo "o/r"
                                 :pr ((number . 7) (state . "closed"))))))
            (should entry-target)
            (should (equal oid (plist-get entry-target :pr-head-oid)))
            (should-not (plist-get entry-target :risks)))
          ;; 2. scan populates :pr-head-oid and executes deletion cleanly.
          (let* ((targets (magit-dash-worktree-clean-scan repo))
                 (target (car targets)))
            (should (= 1 (length targets)))
            (should (equal oid (plist-get target :pr-head-oid)))
            (should-not (plist-get target :risks))
            (let ((plan (hitl-plan-create
                         :domain 'magit-dash-worktree-clean
                         :targets targets
                         :ops '(delete-worktree))))
              (hitl-plan-execute plan)
              (should (eq 'done (hitl-plan-step-status (car (hitl-plan-steps plan)))))
              (should-not (file-directory-p wt)))))))))

(ert-deftest magit-dash-worktree-clean/squash-merged-diverged-tip-triggers-risk ()
  "Worktree ahead of PR headRefOid is flagged risky and refused unattended."
  (magit-dash-worktree-clean-test--with-repo (repo wt)
    (magit-dash-worktree-clean-test--with-store
      (let* ((pushed-oid (magit-dash-test--commit wt "c1.txt"))
             (_local-oid (magit-dash-test--commit wt "c2.txt"))
             (pr `((number . 7) (headRefName . "feat")
                   (headRefOid . ,pushed-oid) (state . "MERGED")))
             (magit-dash-worktree-unattended t)
             (magit-dash-worktree-unattended-risk-policy 'refuse))
        (cl-letf (((symbol-function 'executable-find) (lambda (_) t))
                  ((symbol-function 'magit-dash-pr-dashboard--current-repo-slug)
                   (lambda () "o/r"))
                  ((symbol-function 'magit-dash-gh--call-process)
                   (lambda (args _dir)
                     (let* ((fields (cadr (member "--json" args)))
                            (want-oid (and fields (member "headRefOid" (split-string fields ","))))
                            (payload (if want-oid
                                         pr
                                       (seq-remove (lambda (c) (eq (car c) 'headRefOid)) pr))))
                       (cons 0 (json-serialize
                                (if (member "view" args) payload (vector payload))))))))
          (let* ((targets (magit-dash-worktree-clean-scan repo))
                 (target (car targets))
                 (expected-refusal (if (fboundp 'hitl-plan-skip) 'skipped 'failed)))
            (should (seq-some (lambda (r) (string-match-p "no upstream" r))
                              (plist-get target :risks)))
            (let ((plan (hitl-plan-create
                         :domain 'magit-dash-worktree-clean
                         :targets targets
                         :ops '(delete-worktree))))
              (hitl-plan-execute plan)
              (should (eq expected-refusal
                          (hitl-plan-step-status (car (hitl-plan-steps plan)))))
              (should (file-directory-p wt)))))))))

(provide 'test-magit-dash-worktree-clean)
;;; test-magit-dash-worktree-clean.el ends here
