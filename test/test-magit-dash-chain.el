;;; test-magit-dash-chain.el --- ERT tests for magit-dash-chain -*- lexical-binding: t; no-byte-compile: t; -*-

;; Real git against temp repositories with a local bare "origin"; PR data
;; is stubbed through `magit-dash-chain--fetch-prs'.  No network, no gh.
;;
;; Batch run:
;;   emacs --batch -l test/test-helper.el -l test/test-magit-dash-chain.el \
;;     --eval '(ert-run-tests-batch-and-exit "magit-dash-chain/")'

(require 'ert)
(require 'cl-lib)
(require 'magit-dash)
(require 'magit-dash-mcp)
(require 'magit-dash-chain)

(defvar magit-dash-chain-test--prs nil "PR plists served by the stubbed fetch.")
(defvar magit-dash-chain-test--git-log nil "Every git argv issued by the module.")

(defun magit-dash-chain-test--git (dir &rest args)
  "Run git ARGS in DIR (test setup only); return trimmed output or fail."
  (with-temp-buffer
    (let ((code (apply #'process-file "git" nil t nil "-C" dir args)))
      (unless (zerop code)
        (error "test git %S failed: %s" args (buffer-string)))
      (string-trim-right (buffer-string)))))

(defun magit-dash-chain-test--write (dir file text)
  (with-temp-file (expand-file-name file dir) (insert text)))

(defun magit-dash-chain-test--commit (dir file text msg)
  (magit-dash-chain-test--write dir file text)
  (magit-dash-chain-test--git dir "add" file)
  (magit-dash-chain-test--git dir "commit" "-q" "-m" msg))

(defun magit-dash-chain-test--pr (number branch base &rest props)
  (append props
          (list :number number :branch branch :base base :state "OPEN"
                :url (format "https://example.invalid/pr/%d" number))))

(defun magit-dash-chain-test--make-repo (root)
  "Create ROOT/origin.git and ROOT/repo (a clone) and return the clone path.
The clone has main with base.txt, pushed to origin."
  (let ((origin (expand-file-name "origin.git" root))
        (repo (expand-file-name "repo" root)))
    (call-process "git" nil nil nil "init" "-q" "--bare" "-b" "main" origin)
    (call-process "git" nil nil nil "clone" "-q" origin repo)
    (magit-dash-chain-test--git repo "checkout" "-q" "-b" "main")
    (magit-dash-chain-test--git repo "config" "user.name" "Test")
    (magit-dash-chain-test--git repo "config" "user.email" "test@example.invalid")
    (magit-dash-chain-test--git repo "config" "commit.gpgsign" "false")
    (magit-dash-chain-test--commit repo "base.txt" "base\n" "base")
    (magit-dash-chain-test--git repo "push" "-q" "origin" "main")
    repo))

(defun magit-dash-chain-test--branch (repo name from file text)
  "Create branch NAME off FROM in REPO with a commit of TEXT to FILE; back to main."
  (magit-dash-chain-test--git repo "checkout" "-q" "-b" name from)
  (magit-dash-chain-test--commit repo file text (concat "work on " name))
  (magit-dash-chain-test--git repo "checkout" "-q" "main"))

(defun magit-dash-chain-test--advance-main (repo file text)
  (magit-dash-chain-test--commit repo file text "advance main")
  (magit-dash-chain-test--git repo "push" "-q" "origin" "main"))

(defmacro magit-dash-chain-test--with-env (vars &rest body)
  "Run BODY with a temp tree bound to ROOT (symbol in VARS) and hermetic state."
  (declare (indent 1))
  (let ((root (car vars)))
    `(let* ((,root (make-temp-file "md-chain-" t))
            (process-environment (append '("GIT_CONFIG_GLOBAL=/dev/null"
                                           "GIT_CONFIG_NOSYSTEM=1")
                                         process-environment))
            (hitl-plan-directory (expand-file-name "plans/" ,root))
            (magit-dash-chain-plan-directory nil)
            (magit-dash-chain-worktree-directory (expand-file-name "wts" ,root))
            (magit-dash-chain-conflict-strategy 'halt)
            (hitl-plan--store (make-hash-table :test #'equal))
            (hitl-plan-agent-may-apply nil)
            (magit-dash-chain-test--prs nil)
            (magit-dash-chain-test--git-log nil))
       (unwind-protect
           (cl-letf (((symbol-function 'magit-dash-chain--fetch-prs)
                      (lambda (_slug _dir) magit-dash-chain-test--prs))
                     ((symbol-function 'magit-dash-chain--git)
                      (let ((orig (symbol-function 'magit-dash-chain--git)))
                        (lambda (dir &rest args)
                          (push args magit-dash-chain-test--git-log)
                          (apply orig dir args)))))
             ,@body)
         (delete-directory ,root t)))))

(defun magit-dash-chain-test--chain-prs ()
  "PRs for main <- a <- b <- c."
  (list (magit-dash-chain-test--pr 1 "feat/a" "main")
        (magit-dash-chain-test--pr 2 "feat/b" "feat/a")
        (magit-dash-chain-test--pr 3 "feat/c" "feat/b")))

(defun magit-dash-chain-test--setup-chain (root)
  "Build repo with diverged main <- a <- b; return the repo path."
  (let ((repo (magit-dash-chain-test--make-repo root)))
    (magit-dash-chain-test--branch repo "feat/a" "main" "a.txt" "a\n")
    (magit-dash-chain-test--branch repo "feat/b" "feat/a" "b.txt" "b\n")
    (magit-dash-chain-test--advance-main repo "m2.txt" "main moved\n")
    (setq magit-dash-chain-test--prs
          (list (magit-dash-chain-test--pr 1 "feat/a" "main")
                (magit-dash-chain-test--pr 2 "feat/b" "feat/a")))
    repo))

(defun magit-dash-chain-test--plan (repo &rest opts)
  (let ((chain (magit-dash-chain-validate
                (magit-dash-chain-discover repo "1" nil magit-dash-chain-test--prs))))
    (should-not (plist-get chain :errors))
    (apply #'magit-dash-chain-create-plan chain opts)))

(defun magit-dash-chain-test--parents (dir rev)
  (cdr (split-string (magit-dash-chain-test--git dir "rev-list" "--parents" "-n1" rev))))

;;;; Discovery

(ert-deftest magit-dash-chain/discover-linear-chain ()
  (let* ((prs (magit-dash-chain-test--chain-prs))
         (chain (magit-dash-chain-discover "/nonexistent" 1 "main" prs)))
    (should (equal (mapcar (lambda (n) (plist-get n :branch)) (plist-get chain :nodes))
                   '("feat/a" "feat/b" "feat/c")))
    (should (equal (mapcar (lambda (n) (plist-get n :parent)) (plist-get chain :nodes))
                   '(nil "feat/a" "feat/b")))
    (should (equal (plist-get chain :base) "main"))))

(ert-deftest magit-dash-chain/discover-from-branch-and-subtree ()
  (let* ((prs (magit-dash-chain-test--chain-prs))
         (chain (magit-dash-chain-discover "/nonexistent" "feat/b" nil prs)))
    (should (equal (mapcar (lambda (n) (plist-get n :branch)) (plist-get chain :nodes))
                   '("feat/b" "feat/c")))
    (should (equal (plist-get chain :base) "feat/a"))))

(ert-deftest magit-dash-chain/discover-tree-two-children ()
  (let* ((prs (list (magit-dash-chain-test--pr 1 "feat/a" "main")
                    (magit-dash-chain-test--pr 3 "feat/c" "feat/a")
                    (magit-dash-chain-test--pr 2 "feat/b" "feat/a")
                    (magit-dash-chain-test--pr 4 "feat/d" "feat/b")))
         (chain (magit-dash-chain-discover "/nonexistent" 1 "main" prs))
         (ascii (magit-dash-chain-render-ascii chain)))
    (should (equal (mapcar (lambda (n) (plist-get n :branch)) (plist-get chain :nodes))
                   '("feat/a" "feat/b" "feat/d" "feat/c")))
    (should (string-match-p "main (base)" ascii))
    (should (string-match-p "|-- #2 feat/b" ascii))
    (should (string-match-p "`-- #3 feat/c" ascii))
    (should (string-match-p "`-- #4 feat/d" ascii))
    (should (string-match-p "^\\* PR chain" (magit-dash-chain-render-org chain)))))

(ert-deftest magit-dash-chain/discover-detects-cycle ()
  (let ((prs (list (magit-dash-chain-test--pr 1 "feat/a" "feat/b")
                   (magit-dash-chain-test--pr 2 "feat/b" "feat/a"))))
    (should-error (magit-dash-chain-discover "/nonexistent" 1 "main" prs) :type 'user-error)
    (should-error (magit-dash-chain-discover
                   "/nonexistent" 1 "main"
                   (list (magit-dash-chain-test--pr 1 "feat/a" "feat/a")))
                  :type 'user-error)))

;;;; Validation

(ert-deftest magit-dash-chain/validate-reports-problems ()
  (magit-dash-chain-test--with-env (root)
    (let ((repo (magit-dash-chain-test--setup-chain root)))
      (setq magit-dash-chain-test--prs
            (list (magit-dash-chain-test--pr 1 "feat/a" "main")
                  (magit-dash-chain-test--pr 2 "feat/b" "feat/a" :state "MERGED")
                  (magit-dash-chain-test--pr 3 "feat/ghost" "feat/b")))
      (let ((errs (plist-get (magit-dash-chain-validate
                              (magit-dash-chain-discover repo 1 nil magit-dash-chain-test--prs))
                             :errors)))
        (should (seq-some (lambda (e) (string-match-p "#2 is MERGED" e)) errs))
        (should (seq-some (lambda (e) (string-match-p "feat/ghost does not exist" e)) errs)))
      ;; dirty worktree + unresolvable base
      (magit-dash-chain-test--git repo "checkout" "-q" "feat/a")
      (magit-dash-chain-test--write repo "a.txt" "dirty\n")
      (let ((errs (plist-get (magit-dash-chain-validate
                              (magit-dash-chain-discover repo 1 "nobase" (list (car magit-dash-chain-test--prs))))
                             :errors)))
        (should (seq-some (lambda (e) (string-match-p "uncommitted changes" e)) errs))
        (should (seq-some (lambda (e) (string-match-p "Base branch nobase" e)) errs))))))

;;;; Execution

(ert-deftest magit-dash-chain/clean-sequential-merge-uses-no-ff-merge-commits ()
  (magit-dash-chain-test--with-env (root)
    (let* ((repo (magit-dash-chain-test--setup-chain root))
           (origin-before (magit-dash-chain-test--git repo "ls-remote" "origin"))
           (plan (magit-dash-chain-test--plan repo)))
      (should (= 2 (length (hitl-plan-steps plan))))
      (hitl-plan-execute plan)
      (should (eq 'complete (hitl-plan-status plan)))
      (should (equal '(done done) (mapcar #'hitl-plan-step-status (hitl-plan-steps plan))))
      (let ((a (magit-dash-chain-test--git repo "rev-parse" "feat/a"))
            (b (magit-dash-chain-test--git repo "rev-parse" "feat/b")))
        ;; a gained main's commit through a merge commit; b gained a likewise
        (should (= 2 (length (magit-dash-chain-test--parents repo "feat/a"))))
        (should (= 2 (length (magit-dash-chain-test--parents repo "feat/b"))))
        (should (member a (magit-dash-chain-test--parents repo "feat/b")))
        (should (magit-dash-chain--git-p repo "merge-base" "--is-ancestor" "main" "feat/b"))
        (should (string-match-p "\\`Merge "
                                (magit-dash-chain-test--git repo "log" "-1" "--format=%s" "feat/b")))
        (should (equal b (plist-get (magit-dash-chain--result plan 2) :sha))))
      ;; nothing was pushed
      (should (equal origin-before (magit-dash-chain-test--git repo "ls-remote" "origin")))
      ;; worktrees were created, clean
      (should (file-directory-p (expand-file-name "feat-b" magit-dash-chain-worktree-directory))))))

(ert-deftest magit-dash-chain/up-to-date-branch-makes-no-merge-commit ()
  (magit-dash-chain-test--with-env (root)
    (let* ((repo (magit-dash-chain-test--make-repo root)))
      (magit-dash-chain-test--branch repo "feat/a" "main" "a.txt" "a\n")
      (setq magit-dash-chain-test--prs (list (magit-dash-chain-test--pr 1 "feat/a" "main")))
      (let ((plan (magit-dash-chain-test--plan repo))
            (before (magit-dash-chain-test--git repo "rev-parse" "feat/a")))
        (hitl-plan-execute plan)
        (should (eq 'complete (hitl-plan-status plan)))
        (should (equal before (magit-dash-chain-test--git repo "rev-parse" "feat/a")))
        (should (equal "up-to-date" (plist-get (magit-dash-chain--result plan 1) :outcome)))))))

(defun magit-dash-chain-test--conflict-setup (root)
  "Chain whose root update conflicts on shared.txt; return the repo."
  (let ((repo (magit-dash-chain-test--make-repo root)))
    (magit-dash-chain-test--commit repo "shared.txt" "one\n" "shared")
    (magit-dash-chain-test--git repo "push" "-q" "origin" "main")
    (magit-dash-chain-test--branch repo "feat/a" "main" "shared.txt" "from a\n")
    (magit-dash-chain-test--branch repo "feat/b" "feat/a" "b.txt" "b\n")
    (magit-dash-chain-test--advance-main repo "shared.txt" "from main\n")
    (setq magit-dash-chain-test--prs
          (list (magit-dash-chain-test--pr 1 "feat/a" "main")
                (magit-dash-chain-test--pr 2 "feat/b" "feat/a")))
    repo))

(ert-deftest magit-dash-chain/conflict-halts-in-merge-state-and-records-files ()
  (magit-dash-chain-test--with-env (root)
    (let* ((repo (magit-dash-chain-test--conflict-setup root))
           (plan (magit-dash-chain-test--plan repo)))
      (hitl-plan-execute plan)
      (should (eq 'paused (hitl-plan-status plan)))
      (should (equal '(failed pending) (mapcar #'hitl-plan-step-status (hitl-plan-steps plan))))
      (let ((res (magit-dash-chain--result plan 1))
            (wt (expand-file-name "feat-a" magit-dash-chain-worktree-directory)))
        (should (eq 'blocked (plist-get res :status)))
        (should (equal '("shared.txt") (plist-get res :conflicts)))
        (should (magit-dash-chain--merge-in-progress-p wt))
        (should (string-match-p "shared.txt" (hitl-plan-step-message (car (hitl-plan-steps plan))))))
      ;; persisted: reload from the file
      (let* ((file (hitl-plan-file (hitl-plan-id plan) (hitl-plan-directory plan)))
             (data (with-temp-buffer (insert-file-contents file) (read (current-buffer)))))
        (should (equal '("shared.txt")
                       (plist-get (cdr (assq 1 (plist-get (plist-get (plist-get data :metadata) :chain)
                                                          :results)))
                                  :conflicts)))))))

(ert-deftest magit-dash-chain/resume-still-conflicted-stays-halted ()
  (magit-dash-chain-test--with-env (root)
    (let* ((repo (magit-dash-chain-test--conflict-setup root))
           (plan (magit-dash-chain-test--plan repo)))
      (hitl-plan-execute plan)
      (magit-dash-chain-resume plan)
      (should (eq 'paused (hitl-plan-status plan)))
      (should (eq 'failed (hitl-plan-step-status (car (hitl-plan-steps plan)))))
      (should (eq 'pending (hitl-plan-step-status (cadr (hitl-plan-steps plan))))))))

(ert-deftest magit-dash-chain/resume-after-resolved-but-uncommitted ()
  (magit-dash-chain-test--with-env (root)
    (let* ((repo (magit-dash-chain-test--conflict-setup root))
           (plan (magit-dash-chain-test--plan repo))
           (id (hitl-plan-id plan))
           (wt (expand-file-name "feat-a" magit-dash-chain-worktree-directory)))
      (hitl-plan-execute plan)
      (magit-dash-chain-test--write wt "shared.txt" "resolved\n")
      (magit-dash-chain-test--git wt "add" "shared.txt")
      ;; simulate an Emacs restart: drop the cache, resume from the file
      (clrhash hitl-plan--store)
      (magit-dash-chain-resume id)
      (let ((plan (hitl-plan-get id nil 'magit-dash-chain)))
        (should (eq 'complete (hitl-plan-status plan)))
        (should (equal "resumed" (plist-get (magit-dash-chain--result plan 1) :outcome)))
        (should-not (magit-dash-chain--merge-in-progress-p wt))
        (should (= 2 (length (magit-dash-chain-test--parents repo "feat/a"))))
        (should (= 2 (length (magit-dash-chain-test--parents repo "feat/b"))))
        (should (equal "resolved" (string-trim (magit-dash-chain-test--git
                                                repo "show" "feat/b:shared.txt"))))))))

(ert-deftest magit-dash-chain/resume-after-manual-commit ()
  (magit-dash-chain-test--with-env (root)
    (let* ((repo (magit-dash-chain-test--conflict-setup root))
           (plan (magit-dash-chain-test--plan repo))
           (wt (expand-file-name "feat-a" magit-dash-chain-worktree-directory)))
      (hitl-plan-execute plan)
      (magit-dash-chain-test--write wt "shared.txt" "resolved\n")
      (magit-dash-chain-test--git wt "add" "shared.txt")
      (magit-dash-chain-test--git wt "commit" "-q" "--no-edit")
      (magit-dash-chain-resume plan)
      (should (eq 'complete (hitl-plan-status plan)))
      (should (equal '(done done) (mapcar #'hitl-plan-step-status (hitl-plan-steps plan)))))))

(ert-deftest magit-dash-chain/resume-refuses-leftover-conflict-markers ()
  (magit-dash-chain-test--with-env (root)
    (let* ((repo (magit-dash-chain-test--conflict-setup root))
           (plan (magit-dash-chain-test--plan repo))
           (wt (expand-file-name "feat-a" magit-dash-chain-worktree-directory)))
      (hitl-plan-execute plan)
      (magit-dash-chain-test--write wt "shared.txt" "<<<<<<< ours\nx\n=======\ny\n>>>>>>> theirs\n")
      (magit-dash-chain-test--git wt "add" "shared.txt")
      (magit-dash-chain-resume plan)
      (should (eq 'paused (hitl-plan-status plan)))
      (should (magit-dash-chain--merge-in-progress-p wt)))))

;;;; Conflict strategies

(ert-deftest magit-dash-chain/agent-strategy-falls-back-to-halt-when-unavailable ()
  (magit-dash-chain-test--with-env (root)
    (let* ((magit-dash-chain-conflict-strategy 'agent-shell)
           (repo (magit-dash-chain-test--conflict-setup root))
           (plan (magit-dash-chain-test--plan repo)))
      (cl-letf (((symbol-function 'magit-dash-chain--dispatch-agent) (lambda (&rest _) nil)))
        (hitl-plan-execute plan))
      (should (eq 'paused (hitl-plan-status plan)))
      (should-not (plist-get (magit-dash-chain--result plan 1) :handoff))
      (should (eq 'blocked (plist-get (magit-dash-chain--result plan 1) :status))))))

(ert-deftest magit-dash-chain/agent-handoff-then-verified-completion ()
  (magit-dash-chain-test--with-env (root)
    (let* ((magit-dash-chain-conflict-strategy 'gptel)
           (repo (magit-dash-chain-test--conflict-setup root))
           (plan (magit-dash-chain-test--plan repo))
           (wt (expand-file-name "feat-a" magit-dash-chain-worktree-directory))
           prompt on-done)
      (cl-letf (((symbol-function 'magit-dash-chain--dispatch-agent)
                 (lambda (_strategy _dir p cb) (setq prompt p on-done cb) t)))
        (hitl-plan-execute plan))
      (should (equal "gptel" (plist-get (magit-dash-chain--result plan 1) :handoff)))
      (should (string-match-p "shared.txt" prompt))
      (should (string-match-p "Do NOT rebase" prompt))
      (should (string-match-p "do NOT push" prompt))
      ;; the agent finishing without resolving leaves the plan halted
      (funcall on-done)
      (should (eq 'paused (hitl-plan-status plan)))
      ;; the agent resolves and commits; its completion callback resumes
      (magit-dash-chain-test--write wt "shared.txt" "agent resolved\n")
      (magit-dash-chain-test--git wt "add" "shared.txt")
      (magit-dash-chain-test--git wt "commit" "-q" "--no-edit")
      (funcall on-done)
      (should (eq 'complete (hitl-plan-status plan))))))

;;;; Landing (explicit, gated; never really pushes in tests)

(ert-deftest magit-dash-chain/land-steps-are-separate-and-gated ()
  (magit-dash-chain-test--with-env (root)
    (let* ((repo (magit-dash-chain-test--setup-chain root))
           (landed nil)
           (magit-dash-chain-land-function
            (lambda (target) (push (plist-get target :branch) landed))))
      (let ((plain (magit-dash-chain-test--plan repo)))
        (should (equal '(update update)
                       (mapcar (lambda (s) (plist-get (hitl-plan-step-target s) :kind))
                               (hitl-plan-steps plain)))))
      ;; unapproved MCP-origin plan: land refuses, nothing lands
      (let ((plan (magit-dash-chain-test--plan repo :land t :origin 'mcp)))
        (should (equal '(update land update land)
                       (mapcar (lambda (s) (plist-get (hitl-plan-step-target s) :kind))
                               (hitl-plan-steps plan))))
        (hitl-plan-execute plan)
        (should (null landed))
        (should (eq 'failed (hitl-plan-step-status (nth 1 (hitl-plan-steps plan)))))
        (should (string-match-p "requires user approval"
                                (hitl-plan-step-message (nth 1 (hitl-plan-steps plan))))))
      ;; with approval, steps run in order and update follows land
      (setq landed nil)
      (let ((hitl-plan-agent-may-apply t)
            (plan (magit-dash-chain-test--plan repo :land t :origin 'mcp)))
        (hitl-plan-execute plan)
        (should (eq 'complete (hitl-plan-status plan)))
        (should (equal '("feat/b" "feat/a") landed))))))

;;;; No-rebase guard

(ert-deftest magit-dash-chain/module-never-invokes-rebase-or-pull ()
  (let ((file (locate-library "magit-dash-chain.el")))
    (should file)
    (with-temp-buffer
      (insert-file-contents (concat (file-name-sans-extension file) ".el"))
      ;; Drop the guard's own deny-list, then look for rebase/pull/force
      ;; as whole string literals (i.e. as git arguments).
      (goto-char (point-min))
      (when (re-search-forward "(defconst magit-dash-chain--forbidden-git-args" nil t)
        (let ((beg (match-beginning 0)))
          (search-forward "never passed.\")")
          (delete-region beg (point))))
      (goto-char (point-min))
      (should-not (re-search-forward
                   "\"\\(--?\\)?\\(rebase\\|pull\\|force\\|force-with-lease\\|f\\)\\(=[^\"]*\\)?\""
                   nil t))
      (goto-char (point-min))
      (should-not (re-search-forward "\"--rebase\\|\"--autostash\\|pull --rebase" nil t)))))

(ert-deftest magit-dash-chain/git-wrapper-refuses-history-rewriting ()
  (magit-dash-chain-test--with-env (root)
    (let ((repo (magit-dash-chain-test--make-repo root)))
      (should-error (magit-dash-chain--git repo "rebase" "main"))
      (should-error (magit-dash-chain--git repo "pull" "--rebase"))
      (should-error (magit-dash-chain--git repo "merge" "--rebase" "x"))
      (should-error (magit-dash-chain--git repo "push" "origin" "main"))
      (let ((magit-dash-chain--allow-push t))
        (should-error (magit-dash-chain--git repo "push" "--force" "origin" "main"))
        (should-error (magit-dash-chain--git repo "push" "-f" "origin" "main"))))))

(ert-deftest magit-dash-chain/full-run-issues-no-rebase-pull-or-push ()
  (magit-dash-chain-test--with-env (root)
    (let* ((repo (magit-dash-chain-test--conflict-setup root))
           (plan (magit-dash-chain-test--plan repo))
           (wt (expand-file-name "feat-a" magit-dash-chain-worktree-directory)))
      (hitl-plan-execute plan)
      (magit-dash-chain-test--write wt "shared.txt" "ok\n")
      (magit-dash-chain-test--git wt "add" "shared.txt")
      (magit-dash-chain-resume plan)
      (should (eq 'complete (hitl-plan-status plan)))
      (should magit-dash-chain-test--git-log)
      (dolist (argv magit-dash-chain-test--git-log)
        (dolist (a argv)
          (should-not (member a '("rebase" "pull" "push" "--rebase" "-f" "--force"))))))))

;;;; The plan as a live record

(ert-deftest magit-dash-chain/plan-file-and-record-are-updated-per-step ()
  (magit-dash-chain-test--with-env (root)
    (let* ((repo (magit-dash-chain-test--setup-chain root))
           (plan (magit-dash-chain-test--plan repo))
           (file (hitl-plan-file (hitl-plan-id plan) (hitl-plan-directory plan)))
           (record (magit-dash-chain-record-file plan))
           (seen nil))
      ;; the tree leads the saved plan and the record from the start
      (should (string-match-p "PR chain feat/a onto main" (hitl-plan-description plan)))
      (should (string-match-p "main (base)" (hitl-plan-description plan)))
      (should (file-exists-p record))
      (with-temp-buffer (insert-file-contents record)
                        (should (string-match-p "^#\\+begin_example" (buffer-string))))
      ;; observe the on-disk plan after each step via the step hook
      (let ((hitl-plan-step-functions
             (list (lambda (step _plan)
                     (with-temp-buffer
                       (insert-file-contents record)
                       (push (cons (hitl-plan-step-id step) (buffer-string)) seen))))))
        (hitl-plan-execute plan))
      (should (= 2 (length seen)))
      (let ((res (magit-dash-chain--result plan 2))
            (data (with-temp-buffer (insert-file-contents file) (read (current-buffer)))))
        (should (equal "done" (symbol-name (plist-get res :status))))
        (should (plist-get res :started))
        (should (plist-get res :finished))
        (should (stringp (plist-get res :sha)))
        (should (equal "complete" (symbol-name (plist-get data :status))))
        (should (string-match-p (regexp-quote (substring (plist-get res :sha) 0 10))
                                (with-temp-buffer (insert-file-contents record) (buffer-string))))))))

;;;; MCP surface

(ert-deftest magit-dash-chain/mcp-tools-registered-and-plan-tool-works ()
  (let ((tools (mcpkit-service-tools (mcpkit-get-service 'magit-dash))))
    (should (gethash "chain_plan" tools))
    (should (gethash "chain_resume" tools)))
  (magit-dash-chain-test--with-env (root)
    (let ((repo (magit-dash-chain-test--setup-chain root)))
      (cl-letf (((symbol-function 'magit-dash-mcp--resolve-repo-slug) (lambda (&rest _) "o/r")))
        (let* ((res (magit-dash-mcp-call-tool "chain_plan" (list :repo repo :root "1")))
               (plan (hitl-plan-get (plist-get res :plan_id) nil 'magit-dash-chain)))
          (should (eq t (plist-get res :valid)))
          (should (string-match-p "feat/b" (plist-get res :tree)))
          (should (hitl-plan-origin-mcp-p plan))
          (should (= 2 (length (plist-get res :steps))))
          ;; unapproved MCP plan: resume asks for the user
          (should (equal "awaiting_user_confirmation"
                         (plist-get (magit-dash-mcp-call-tool
                                     "chain_resume" (list :plan_id (hitl-plan-id plan)))
                                    :status)))
          (let ((hitl-plan-agent-may-apply t))
            (should (equal "complete"
                           (plist-get (magit-dash-mcp-call-tool
                                       "chain_resume" (list :plan_id (hitl-plan-id plan)))
                                      :status)))))
        ;; invalid chains create no plan
        (setq magit-dash-chain-test--prs (list (magit-dash-chain-test--pr 1 "feat/zzz" "main")))
        (let ((res (magit-dash-mcp-call-tool "chain_plan" (list :repo repo :root "1"))))
          (should (eq :false (plist-get res :valid)))
          (should (eq :null (plist-get res :plan_id))))))))

(ert-deftest magit-dash-chain-test-interactive-plan-opens-review ()
  "The interactive command creates the plan and opens its review buffer."
  (magit-dash-chain-test--with-env (root)
    (require (quote hitl-plan-ui))
    (let ((repo (magit-dash-chain-test--setup-chain root)) reviewed)
      (cl-letf (((symbol-function 'magit-dash-mcp--resolve-repo-slug) (lambda (&rest _) "o/r"))
                ((symbol-function 'hitl-plan-review) (lambda (p &rest _) (setq reviewed p))))
        (let ((plan (magit-dash-chain-plan repo "1")))
          (should (eq plan reviewed))
          (should-not (hitl-plan-origin-mcp-p plan))
          (should (= 2 (length (hitl-plan-steps plan)))))
        ;; invalid chain: error, no review
        (setq reviewed nil
              magit-dash-chain-test--prs (list (magit-dash-chain-test--pr 1 "feat/zzz" "main")))
        (should-error (magit-dash-chain-plan repo "1") :type 'user-error)
        (should-not reviewed)
        (kill-buffer "*magit-dash-chain*")))))

(provide 'test-magit-dash-chain)
;;; test-magit-dash-chain.el ends here
