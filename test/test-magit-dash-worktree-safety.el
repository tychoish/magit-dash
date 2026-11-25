;;; test-magit-dash-worktree-safety.el --- Deletion safety tests -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Regression tests for the checks that guard worktree and branch deletion.
;; Every test works on throwaway git repositories under `make-temp-file'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'magit-dash)
(require 'magit-dash-worktree)
(require 'magit-dash-gh)
(require 'magit-dash-chain)
(require 'magit-dash-mcp)
(require 'test-helper)

(defmacro magit-dash-worktree-safety--with-wt (vars &rest body)
  "Bind (REPO WT) of VARS to a repo and a linked worktree on branch BRANCH.
VARS is (REPO WT &optional BRANCH), BRANCH defaulting to \"feat\".  The
worktree gets an up-to-date origin/BRANCH unless the body undoes it.  Run BODY."
  (declare (indent 1))
  (let ((branch (or (nth 2 vars) "feat")))
    `(let* ((,(car vars) (magit-dash-test--make-repo))
            (,(cadr vars) (expand-file-name "wt/" (make-temp-file "mdtest-wt-" t))))
       (unwind-protect
           (progn
             (magit-dash-test--git ,(car vars) "worktree" "add" "-q" "-b" ,branch ,(cadr vars) "HEAD")
             (magit-dash-worktree-safety--track ,(car vars) ,branch)
             ,@body)
         (delete-directory ,(car vars) t)
         (delete-directory (file-name-directory (directory-file-name ,(cadr vars))) t)))))

(defun magit-dash-worktree-safety--track (repo branch)
  "Make BRANCH of REPO track an up-to-date origin/BRANCH."
  (magit-dash-test--git repo "config" "remote.origin.url" "/nonexistent")
  (magit-dash-test--git repo "config" "remote.origin.fetch" "+refs/heads/*:refs/remotes/origin/*")
  (magit-dash-test--git repo "update-ref" (concat "refs/remotes/origin/" branch) branch)
  (magit-dash-test--git repo "config" (concat "branch." branch ".remote") "origin")
  (magit-dash-test--git repo "config" (concat "branch." branch ".merge")
                        (concat "refs/heads/" branch)))

(defun magit-dash-worktree-safety--exclude (repo &rest patterns)
  "Add PATTERNS to REPO's info/exclude."
  (with-temp-buffer
    (seq-do (lambda (p) (insert p "\n")) patterns)
    (append-to-file (point-min) (point-max) (expand-file-name ".git/info/exclude" repo))))

(defun magit-dash-worktree-safety--write (dir rel &optional text)
  "Write TEXT (default \"x\") to REL under DIR, creating directories."
  (let ((file (expand-file-name rel dir)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file (insert (or text "x")))))

;;; H2: ignorable defaults

(ert-deftest magit-dash-worktree-safety/ignorable-defaults-flag-data-dirs ()
  (dolist (rel '("out/results.db" "build/patch.diff" ".cache/token" "dist/app.tgz"))
    (magit-dash-worktree-safety--with-wt (repo wt)
      (magit-dash-worktree-safety--exclude repo "out/" "build/" ".cache/" "dist/")
      (magit-dash-worktree-safety--write wt rel)
      (let ((risks (magit-dash-worktree-risks wt)))
        (should (= 1 (length risks)))
        (should (string-match-p (regexp-quote (car (split-string rel "/"))) (car risks))))))
  (magit-dash-worktree-safety--with-wt (repo wt)
    (magit-dash-worktree-safety--write wt "node_modules/x/y.js")
    (magit-dash-worktree-safety--exclude repo "__pycache__/")
    (magit-dash-worktree-safety--write wt "__pycache__/a.pyc")
    (should-not (magit-dash-worktree-risks wt))))

;;; M2: status blind spots

(ert-deftest magit-dash-worktree-safety/untracked-visible-despite-config ()
  (magit-dash-worktree-safety--with-wt (repo wt)
    (magit-dash-test--git wt "config" "status.showUntrackedFiles" "no")
    (magit-dash-worktree-safety--write wt "notes.txt")
    (should (magit-dash-worktree-uncommitted-changes-p wt))
    (should (cdr (car (magit-dash-worktree-dirty-many (list wt)))))
    (should (magit-dash-chain--dirty-p wt))))

(ert-deftest magit-dash-worktree-safety/assume-unchanged-and-skip-worktree-are-risks ()
  (magit-dash-worktree-safety--with-wt (repo wt)
    (should-not (magit-dash-worktree-risks wt))
    (magit-dash-test--git wt "update-index" "--assume-unchanged" "a.txt")
    (magit-dash-worktree-safety--write wt "a.txt" "edited\n")
    (should-not (magit-dash-worktree-uncommitted-changes-p wt))
    (should (string-match-p "assume-unchanged" (string-join (magit-dash-worktree-risks wt) ";")))
    (magit-dash-test--git wt "update-index" "--no-assume-unchanged" "a.txt")
    (magit-dash-test--git wt "checkout" "-q" "--" "a.txt")
    (should-not (magit-dash-worktree-risks wt))
    (magit-dash-test--git wt "update-index" "--skip-worktree" "a.txt")
    (should (string-match-p "skip-worktree" (string-join (magit-dash-worktree-risks wt) ";")))))

;;; L1: in-progress operations and unattended refusal

(ert-deftest magit-dash-worktree-safety/in-progress-operation-is-a-risk ()
  (dolist (name '("MERGE_HEAD" "CHERRY_PICK_HEAD" "REVERT_HEAD" "rebase-merge" "rebase-apply" "sequencer"))
    (magit-dash-worktree-safety--with-wt (repo wt)
      (should-not (magit-dash-worktree-risks wt))
      (let ((gitdir (magit-dash-test--git wt "rev-parse" "--absolute-git-dir")))
        (if (member name '("rebase-merge" "rebase-apply" "sequencer"))
            (make-directory (expand-file-name name gitdir))
          (magit-dash-worktree-safety--write gitdir name "0\n")))
      (should (string-match-p (regexp-quote name)
                              (string-join (magit-dash-worktree-risks wt) ";"))))))

(ert-deftest magit-dash-worktree-safety/confirm-refuses-without-ui ()
  "In batch (or unattended) the confirm function refuses and never prompts."
  (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) (error "must not prompt"))))
    (let ((err (should-error (magit-dash-worktree-confirm-risks "/tmp/x" '("risk one"))
                             :type 'user-error)))
      (should (string-match-p "risk one" (cadr err)))))
  (let ((magit-dash-worktree-unattended-risk-policy 'ask))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (should (magit-dash-worktree-confirm-risks "/tmp/x" '("risk one"))))))

(ert-deftest magit-dash-worktree-safety/mcp-delete-refuses-and-reports-risks ()
  (magit-dash-worktree-safety--with-wt (repo wt)
    (magit-dash-test--commit wt "new.txt")
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) (error "must not prompt"))))
      (let ((res (magit-dash-mcp-call-tool
                  "manage_worktrees"
                  (list :repo repo :action "delete" :worktree_path wt))))
        (should (eq (plist-get res :deleted) :false))
        (should (plist-get res :refused))
        (should (string-match-p "ahead" (plist-get res :reason)))
        (should (file-directory-p wt))))))

;;; M1: interactive delete goes through the risk check

(ert-deftest magit-dash-worktree-safety/interactive-delete-checks-risks ()
  (magit-dash-worktree-safety--with-wt (repo wt)
    (magit-dash-test--commit wt "new.txt")
    (let* ((row (magit-dash-repo--make :name "wt" :path wt :worktree t))
           (asked nil)
           (magit-dash-worktree-unattended-risk-policy 'ask))
      (cl-letf (((symbol-function 'magit-dash--repo-at-point) (lambda () row))
                ((symbol-function 'magit-dash-refresh) #'ignore)
                ((symbol-function 'magit-worktree-delete)
                 (lambda (&rest _) (error "must not bypass the risk check")))
                ((symbol-function 'magit-dash-worktree-confirm-risks)
                 (lambda (_path risks) (setq asked risks) nil)))
        (should-error (magit-dash-worktree-delete) :type 'user-error)
        (should asked)
        (should (file-directory-p wt)))
      (cl-letf (((symbol-function 'magit-dash--repo-at-point) (lambda () row))
                ((symbol-function 'magit-dash-refresh) #'ignore)
                ((symbol-function 'magit-dash-worktree-confirm-risks) (lambda (&rest _) t)))
        (magit-dash-worktree-delete)
        (should-not (file-directory-p wt))))))

;;; H1: prune

(defun magit-dash-worktree-safety--prune (repo branches prompt-p)
  "Run the prune deletion for BRANCHES in REPO with PROMPT-P; return its result."
  (let ((magit-dash-gh--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'magit-dash-gh--prune-scan) #'ignore)
              ((symbol-function 'magit-dash-gh--default-branch) (lambda () "main"))
              ((symbol-function 'read-char-choice) (lambda (&rest _) ?!)))
      (magit-dash-gh--prune-delete-branches branches repo prompt-p))))

(ert-deftest magit-dash-worktree-safety/prune-skips-risky-worktree-unattended ()
  (magit-dash-worktree-safety--with-wt (repo wt "f5")
    (magit-dash-worktree-safety--exclude repo ".env")
    (magit-dash-test--commit wt "new.txt")
    (magit-dash-worktree-safety--write wt ".env" "SECRET=1\n")
    (let ((res (magit-dash-worktree-safety--prune repo '("f5") nil)))
      (should (= 0 (plist-get res :deleted)))
      (should (= 1 (plist-get res :skipped)))
      (should (string-match-p "f5" (car (plist-get res :details))))
      (should (file-directory-p wt))
      (should (file-exists-p (expand-file-name ".env" wt)))
      (should (magit-dash-gh--branch-tip "f5" repo)))))

(ert-deftest magit-dash-worktree-safety/prune-prompts-for-risky-worktree-even-yes-to-all ()
  (magit-dash-worktree-safety--with-wt (repo wt "f5")
    (magit-dash-worktree-safety--exclude repo ".env")
    (magit-dash-test--commit wt "new.txt")
    (magit-dash-worktree-safety--write wt ".env" "SECRET=1\n")
    (let ((asked 0))
      (cl-letf (((symbol-function 'magit-dash-worktree-confirm-risks)
                 (lambda (_path risks)
                   (setq asked (1+ asked))
                   (should risks)
                   nil)))
        (let ((res (magit-dash-worktree-safety--prune repo '("f5") t)))
          (should (= 1 asked))
          (should (= 1 (plist-get res :skipped)))
          (should (file-directory-p wt)))))))

(ert-deftest magit-dash-worktree-safety/prune-never-force-deletes-unmerged-branch ()
  "A confirmed risky worktree may go, but the unmerged branch survives (-d)."
  (magit-dash-worktree-safety--with-wt (repo wt "f5")
    (magit-dash-test--commit wt "new.txt")
    (cl-letf (((symbol-function 'magit-dash-worktree-confirm-risks) (lambda (&rest _) t)))
      (let ((res (magit-dash-worktree-safety--prune repo '("f5") t)))
        (should (= 0 (plist-get res :deleted)))
        (should (magit-dash-gh--branch-tip "f5" repo))))))

(ert-deftest magit-dash-worktree-safety/prune-deletes-verified-clean-worktree ()
  (magit-dash-worktree-safety--with-wt (repo wt "f5")
    ;; f5 has no commits of its own, so it is merged into main.
    (let ((res (magit-dash-worktree-safety--prune repo '("f5") nil)))
      (should (= 1 (plist-get res :deleted)))
      (should-not (file-directory-p wt))
      (should-not (magit-dash-gh--branch-tip "f5" repo)))))

(ert-deftest magit-dash-worktree-safety/prune-branch-without-worktree-needs-verification ()
  (let ((repo (magit-dash-test--make-repo)))
    (unwind-protect
        (progn
          (magit-dash-test--git repo "checkout" "-q" "-b" "lone")
          (magit-dash-test--commit repo "lone.txt")
          (magit-dash-test--git repo "checkout" "-q" "main")
          (let ((res (magit-dash-worktree-safety--prune repo '("lone") nil)))
            (should (= 0 (plist-get res :deleted)))
            (should (= 1 (plist-get res :skipped)))
            (should (magit-dash-gh--branch-tip "lone" repo))))
      (delete-directory repo t))))

(ert-deftest magit-dash-worktree-safety/scan-requires-more-than-a-name-match ()
  (let ((repo (magit-dash-test--make-repo)))
    (unwind-protect
        (let* ((default-directory repo)
               (magit-dash-gh--cache (make-hash-table :test #'equal))
               (tips nil))
          (dolist (b '("unsafe" "headoid" "pushed" "merged"))
            (magit-dash-test--git repo "checkout" "-q" "-b" b "main")
            (unless (equal b "merged")
              (push (cons b (magit-dash-test--commit repo (concat b ".txt"))) tips)))
          (magit-dash-test--git repo "update-ref" "refs/remotes/origin/pushed"
                                (alist-get "pushed" tips nil nil #'equal))
          (magit-dash-test--git repo "checkout" "-q" "main")
          (let ((prs (make-hash-table :test #'equal)))
            (dolist (b '("unsafe" "headoid" "pushed" "merged"))
              (setf (map-elt prs b)
                    `((number . 1) (state . "MERGED")
                      (headRefOid . ,(if (equal b "headoid")
                                         (alist-get b tips nil nil #'equal)
                                       "0000000000000000000000000000000000000000")))))
            (cl-letf (((symbol-function 'magit-dash-gh--repo-dir) (lambda () repo))
                      ((symbol-function 'magit-dash-gh--default-branch) (lambda () "main"))
                      ((symbol-function 'magit-dash-gh--fetch-closed-prs) (lambda (&optional _) prs)))
              (should (equal '("headoid" "merged" "pushed")
                             (seq-map #'car (magit-dash-gh--prune-scan)))))))
      (delete-directory repo t))))

(provide 'test-magit-dash-worktree-safety)
;;; test-magit-dash-worktree-safety.el ends here
