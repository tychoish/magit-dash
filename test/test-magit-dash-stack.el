;;; test-magit-dash-stack.el --- ERT tests for magit-dash-stack -*- lexical-binding: t; no-byte-compile: t; -*-

;; Real git against temp repositories with a local bare "origin"; PR data is
;; stubbed through the chain fetch seam.  No network, no gh.  Helpers come
;; from the shared chain test fixtures.
;;
;; Batch run:
;;   emacs --batch -l test/test-helper.el -l test/test-magit-dash-stack.el \
;;     --eval '(ert-run-tests-batch-and-exit "magit-dash-stack/")'

(require 'ert)
(require 'cl-lib)
(require 'magit-dash)
(require 'magit-dash-chain)
(require 'hitl-plan-ui)
(require 'magit-dash-chain-test-fixtures
         (expand-file-name "fixtures/magit-dash-chain-test-fixtures.el"
                           (file-name-directory (or load-file-name buffer-file-name))))

(require 'magit-dash-stack)

(defconst magit-dash-stack-test--source
  (expand-file-name "../magit-dash-stack.el"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defalias 'stack-test--git #'magit-dash-chain-test--git)

(defun stack-test--worktree (repo branch)
  "Create a worktree for BRANCH next to REPO; return its path."
  (let ((path (expand-file-name (replace-regexp-in-string "/" "-" branch)
                                (expand-file-name "../wt" repo))))
    (make-directory (file-name-directory path) t)
    (stack-test--git repo "worktree" "add" "-q" path branch)
    path))

(defun stack-test--setup (root &rest branches)
  "Make a repo under ROOT with BRANCHES, a list of (NAME FROM FILE TEXT).
Each branch gets a worktree.  Return the repo path."
  (let ((repo (magit-dash-chain-test--make-repo root)))
    (dolist (b branches)
      (magit-dash-chain-test--branch repo (nth 0 b) (nth 1 b) (nth 2 b) (nth 3 b)))
    (dolist (b branches) (stack-test--worktree repo (nth 0 b)))
    repo))

(defun stack-test--linear (root)
  (stack-test--setup root
                     '("s/a" "main" "a.txt" "a\n")
                     '("s/b" "s/a" "b.txt" "b\n")
                     '("s/c" "s/b" "c.txt" "c\n")))

(defun stack-test--wt (repo branch)
  (magit-dash-worktree-for-branch repo branch))

(defun stack-test--commit-on (repo branch file text)
  "Commit TEXT to FILE on BRANCH in its worktree."
  (magit-dash-chain-test--commit (stack-test--wt repo branch) file text (concat "more " branch)))

(defun stack-test--branches (stack)
  (mapcar (lambda (n) (plist-get n :branch)) (plist-get stack :nodes)))

(defun stack-test--parents (stack)
  (mapcar (lambda (n) (plist-get n :parent)) (plist-get stack :nodes)))

(defun stack-test--node (stack branch)
  (magit-dash-stack--node stack branch))

(defmacro stack-test--with-env (vars &rest body)
  (declare (indent 1))
  `(magit-dash-chain-test--with-env ,vars ,@body))

;;;; Discovery

(ert-deftest magit-dash-stack/discover-linear ()
  (stack-test--with-env (root)
    (let* ((repo (stack-test--linear root))
           (stack (magit-dash-stack-discover repo "s/*" "main")))
      (should (equal '("s/a" "s/b" "s/c") (stack-test--branches stack)))
      (should (equal '(nil "s/a" "s/b") (stack-test--parents stack)))
      (should (equal '(1 1 1) (mapcar (lambda (n) (plist-get n :ahead)) (plist-get stack :nodes))))
      (should (equal "main" (plist-get stack :base)))
      ;; the parent was recorded (the only write)
      (should (equal "s/a" (stack-test--git repo "config" "branch.s/b.magit-dash-stack-parent")))
      (should (equal "main" (stack-test--git repo "config" "branch.s/a.magit-dash-stack-parent")))
      (should-not (plist-get stack :problems)))))

(ert-deftest magit-dash-stack/discover-tree ()
  (stack-test--with-env (root)
    (let* ((repo (stack-test--setup root
                                    '("s/a" "main" "a.txt" "a\n")
                                    '("s/b" "s/a" "b.txt" "b\n")
                                    '("s/c" "s/a" "c.txt" "c\n")))
           (stack (magit-dash-stack-discover repo "s/*")))
      (should (equal '("s/a" "s/b" "s/c") (stack-test--branches stack)))
      (should (equal '(nil "s/a" "s/a") (stack-test--parents stack))))))

(ert-deftest magit-dash-stack/discover-several-bottoms ()
  (stack-test--with-env (root)
    (let* ((repo (stack-test--setup root
                                    '("s/x" "main" "x.txt" "x\n")
                                    '("s/y" "main" "y.txt" "y\n")
                                    '("s/z" "s/y" "z.txt" "z\n")))
           (stack (magit-dash-stack-discover repo "s/*" "main")))
      (should (seq-some (lambda (p) (string-match-p "Several branches sit directly" p))
                        (plist-get stack :problems)))
      (should (equal '(nil nil "s/y") (stack-test--parents stack)))
      ;; without a root the chain data carries the problem, so validation fails
      (let ((chain (magit-dash-stack-to-chain stack)))
        (should (plist-get chain :problems))
        (should (plist-get (magit-dash-chain-validate chain) :errors)))
      ;; with a root only that subtree is synced, and it is valid
      (let ((chain (magit-dash-chain-validate (magit-dash-stack-to-chain stack "s/y"))))
        (should (equal '("s/y" "s/z")
                       (mapcar (lambda (n) (plist-get n :branch)) (plist-get chain :nodes))))
        (should-not (plist-get chain :errors)))
      ;; the buffer draws every bottom
      (with-current-buffer (magit-dash-stack-open repo "s/*" "main")
        (should (string-match-p "s/x" (buffer-string)))
        (should (string-match-p "s/y" (buffer-string)))
        (should (string-match-p "s/z" (buffer-string))))
      ;; sync needs a subtree choice
      (should-error (magit-dash-stack-sync stack nil nil) :type 'user-error))))

(ert-deftest magit-dash-stack/base-origin-main-is-normalized ()
  (stack-test--with-env (root)
    (let* ((repo (stack-test--linear root))
           (stack (magit-dash-stack-discover repo "s/*" "origin/main")))
      (should (equal "main" (plist-get stack :base)))
      (should (equal '(nil "s/a" "s/b") (stack-test--parents stack))))))

(ert-deftest magit-dash-stack/worktree-path-and-missing-worktree ()
  (stack-test--with-env (root)
    (let* ((repo (stack-test--linear root))
           (stack (magit-dash-stack-discover repo "s/*" "main")))
      (should (equal (file-truename (stack-test--wt repo "s/b"))
                     (file-truename (plist-get (stack-test--node stack "s/b") :worktree))))
      (should-not (plist-get stack :problems))
      (magit-dash-chain-test--branch repo "s/d" "s/c" "d.txt" "d\n")
      (let ((stack (magit-dash-stack-discover repo "s/*" "main")))
        (should-not (plist-get (stack-test--node stack "s/d") :worktree))
        (should (member "No worktree for s/d" (plist-get stack :problems)))))))

(ert-deftest magit-dash-stack/same-tip-is-a-problem ()
  (stack-test--with-env (root)
    (let ((repo (stack-test--linear root)))
      (stack-test--git repo "branch" "s/c2" "s/c")
      (let ((stack (magit-dash-stack-discover repo "s/*" "main")))
        (should (seq-some (lambda (p) (string-match-p "same commit" p))
                          (plist-get stack :problems)))
        (should (plist-get (magit-dash-stack-to-chain stack) :problems))))))

(ert-deftest magit-dash-stack/stale-recorded-parent-is-ignored ()
  (stack-test--with-env (root)
    (let ((repo (stack-test--linear root)))
      (stack-test--git repo "config" "branch.s/c.magit-dash-stack-parent" "gone")
      (let ((stack (magit-dash-stack-discover repo "s/*" "main")))
        (should (equal "s/b" (plist-get (stack-test--node stack "s/c") :parent)))
        (should (seq-some (lambda (p) (string-match-p "Recorded parent gone" p))
                          (plist-get stack :problems)))))))

(ert-deftest magit-dash-stack/recorded-parent-cycle-is-repaired ()
  (stack-test--with-env (root)
    (let ((repo (stack-test--linear root)))
      (stack-test--git repo "config" "branch.s/a.magit-dash-stack-parent" "s/b")
      (stack-test--git repo "config" "branch.s/b.magit-dash-stack-parent" "s/a")
      (let ((stack (magit-dash-stack-discover repo "s/*" "main" nil t)))
        (should (equal '("s/a" "s/b" "s/c") (sort (stack-test--branches stack) #'string<)))
        (should (seq-some (lambda (p) (string-match-p "Parent cycle among" p))
                          (plist-get stack :problems)))
        (should (null (plist-get (stack-test--node stack "s/a") :parent)))
        (should (null (plist-get (stack-test--node stack "s/b") :parent)))))))

(ert-deftest magit-dash-stack/pr-base-beats-ancestry ()
  (stack-test--with-env (root)
    (let* ((repo (stack-test--setup root
                                    '("s/a" "main" "a.txt" "a\n")
                                    '("s/b" "main" "b.txt" "b\n")))
           (stack (magit-dash-stack-discover
                   repo "s/*" "main"
                   (list (magit-dash-chain-test--pr 2 "s/b" "s/a")) t)))
      (should (equal "s/a" (plist-get (stack-test--node stack "s/b") :parent)))
      (should (eq 'pr (plist-get (stack-test--node stack "s/b") :parent-source)))
      ;; no-record: nothing written
      (should-error (stack-test--git repo "config" "branch.s/b.magit-dash-stack-parent")))))

;;;; Drift

(ert-deftest magit-dash-stack/drift-keeps-child-under-recorded-parent ()
  (stack-test--with-env (root)
    (let ((repo (stack-test--linear root)))
      (magit-dash-stack-discover repo "s/*" "main") ; records the parents
      (stack-test--commit-on repo "s/a" "a2.txt" "more\n")
      (let ((stack (magit-dash-stack-discover repo "s/*" "main")))
        (should (equal '(nil "s/a" "s/b") (stack-test--parents stack)))
        (should (equal '(nil 1 nil)
                       (mapcar (lambda (n) (plist-get n :drift)) (plist-get stack :nodes))))
        (should (eq 'recorded (plist-get (stack-test--node stack "s/b") :parent-source)))
        ;; the observed (ancestry) parent is base: the record and the tree disagree
        (should (null (plist-get (stack-test--node stack "s/b") :observed-parent)))))))

(ert-deftest magit-dash-stack/root-drifts-against-origin-base ()
  (stack-test--with-env (root)
    (let ((repo (stack-test--linear root)))
      (magit-dash-chain-test--advance-main repo "m2.txt" "moved\n")
      (let ((stack (magit-dash-stack-discover repo "s/*" "origin/main")))
        (should (equal 1 (plist-get (stack-test--node stack "s/a") :drift)))))))

(ert-deftest magit-dash-stack/ancestry-only-fallback-sees-drifted-branch-as-sibling ()
  "Documented limitation: with no record and no PR a drifted branch is a sibling."
  (stack-test--with-env (root)
    (let ((repo (stack-test--linear root)))
      (stack-test--commit-on repo "s/a" "a2.txt" "more\n")
      (let ((stack (magit-dash-stack-discover repo "s/*" "main")))
        (should (equal '(nil nil "s/b") (stack-test--parents stack)))
        (should (eq 'ancestry (plist-get (stack-test--node stack "s/b") :parent-source)))))))

;;;; Commits

(ert-deftest magit-dash-stack/numstat-counts-added-removed-and-binary ()
  (stack-test--with-env (root)
    (let* ((repo (stack-test--setup root '("s/a" "main" "a.txt" "a\n")))
           (wt (stack-test--wt repo "s/a")))
      (magit-dash-chain-test--write wt "t.txt" "1\n2\n3\n")
      (let ((coding-system-for-write 'binary))
        (write-region "ab\0cd" nil (expand-file-name "bin.dat" wt) nil 'silent))
      (stack-test--git wt "rm" "-q" "base.txt")
      (stack-test--git wt "add" "-A")
      (stack-test--git wt "commit" "-q" "-m" "mixed")
      (let* ((c (car (last (magit-dash-stack--commits repo "main" "s/a")))))
        (should (equal "mixed" (plist-get c :subject)))
        (should (= 3 (plist-get c :files)))
        (should (= 3 (plist-get c :added)))
        (should (= 1 (plist-get c :removed)))
        (should (= 1 (plist-get c :binary)))
        (should-not (plist-get c :merge))))))

(ert-deftest magit-dash-stack/merge-commit-is-a-marker-row ()
  (stack-test--with-env (root)
    (let ((repo (stack-test--linear root)))
      (stack-test--commit-on repo "s/a" "a2.txt" "more\n")
      (stack-test--git (stack-test--wt repo "s/b") "merge" "-q" "--no-ff" "-m" "sync" "s/a")
      (let* ((stack (magit-dash-stack-discover repo "s/*" "main"))
             (commits (plist-get (stack-test--node stack "s/b") :commits)))
        (should (equal '(nil t) (mapcar (lambda (c) (plist-get c :merge)) commits)))
        (let ((m (cadr commits)))
          (should (equal "sync" (plist-get m :subject)))
          (should (= 0 (plist-get m :added)))
          (should (= 0 (plist-get m :files))))
        (should-not (plist-get (stack-test--node stack "s/b") :drift))
        (should (= 1 (magit-dash-stack--own-count (stack-test--node stack "s/b"))))))))

;;;; Graph output

(defun stack-test--fixture ()
  "A stack plist (no git) shaped like the FND-1340 example."
  (let ((c (lambda (n) (cl-loop repeat n collect (list :sha "x" :subject "s" :merge nil)))))
    (list :repo-dir "/tmp/r" :base "main" :prefix "tycho/FND-1340-*"
          :problems nil
          :nodes (list (list :branch "tycho/FND-1340-remove-constants" :parent nil
                             :commits (funcall c 5))
                       (list :branch "tycho/FND-1340-reduce-surface-area"
                             :parent "tycho/FND-1340-remove-constants"
                             :commits (funcall c 7) :drift 2)
                       (list :branch "tycho/FND-1340-lambda-modules"
                             :parent "tycho/FND-1340-reduce-surface-area"
                             :commits (funcall c 1))))))

(ert-deftest magit-dash-stack/dot-golden ()
  (should (equal (magit-dash-stack-dot (stack-test--fixture))
                 "// tycho/FND-1340-* stack onto main. Edges point parent -> child; \"+N\" is the child's own
// commits over its parent. Red edges are drifted (the parent has moved on).
digraph tycho_FND_1340 {
  rankdir=LR;
  node [shape=box, fontname=\"monospace\", fontsize=10, style=filled, fillcolor=white];
  edge [fontname=\"monospace\", fontsize=9];

  base [label=\"main\", shape=ellipse];
  n0 [label=\"remove-constants\\n5 commits\"];
  n1 [label=\"reduce-surface-area\\n7 commits\"];
  n2 [label=\"lambda-modules\\n1 commit\"];

  base -> n0 [label=\"+5\"];
  n0 -> n1 [label=\"+7\", color=red, fontcolor=red];
  n1 -> n2 [label=\"+1\"];
}
")))

(ert-deftest magit-dash-stack/mermaid-golden ()
  (should (equal (magit-dash-stack-mermaid (stack-test--fixture))
                 "flowchart LR
  base([\"main\"])
  n0[\"remove-constants<br/>5 commits\"]
  n1[\"reduce-surface-area<br/>7 commits\"]
  n2[\"lambda-modules<br/>1 commit\"]
  base -->|\"+5\"| n0
  n0 -.->|\"+7, behind 2\"| n1
  n1 -->|\"+1\"| n2
")))

(ert-deftest magit-dash-stack/write-dot-and-mermaid-files ()
  (stack-test--with-env (root)
    (let* ((stack (stack-test--fixture))
           (dot (magit-dash-stack-write-dot stack (expand-file-name "out/s.dot" root)))
           (mmd (magit-dash-stack-write-mermaid stack (expand-file-name "s.mmd" root))))
      (should (equal (magit-dash-stack-dot stack)
                     (with-temp-buffer (insert-file-contents dot) (buffer-string))))
      (should (equal (magit-dash-stack-mermaid stack)
                     (with-temp-buffer (insert-file-contents mmd) (buffer-string)))))))

;;;; Buffer

(ert-deftest magit-dash-stack/buffer-shows-branches-commits-and-drift ()
  (stack-test--with-env (root)
    (let ((repo (stack-test--linear root)))
      (magit-dash-stack-discover repo "s/*" "main")
      (stack-test--commit-on repo "s/a" "a2.txt" "more\n")
      (let ((buf (magit-dash-stack-open repo "s/*" "main")))
        (unwind-protect
            (with-current-buffer buf
              (should (derived-mode-p 'magit-dash-stack-mode))
              (let ((text (buffer-substring-no-properties (point-min) (point-max))))
                (should (string-match-p "^Stack s/\\* onto main" text))
                (should (string-match-p "^s/a  " text))
                (should (string-match-p "^  s/b  .*DRIFT (behind s/a by 1)" text))
                (should-not (string-match-p "s/a .*DRIFT" text))
                (should (string-match-p "work on s/b" text))
                (should (string-match-p "+1 +-0 +1f" text)))
              ;; sections: one per branch, one per commit
              (goto-char (point-min))
              (re-search-forward "^  s/b")
              (let ((s (magit-current-section)))
                (should (eq 'magit-dash-stack-branch (oref s type)))
                (should (equal "s/b" (oref s value)))
                (should (= 1 (length (oref s children)))))
              (should (equal "s/b" (magit-dash-stack--branch-at-point)))
              ;; refresh keeps working
              (magit-dash-stack-refresh)
              (should (string-match-p "DRIFT" (buffer-string))))
          (kill-buffer buf))))))

;;;; Chain adapter and sync

(ert-deftest magit-dash-stack/to-chain-validates-and-creates-update-steps ()
  (stack-test--with-env (root)
    (let* ((repo (stack-test--linear root))
           (stack (magit-dash-stack-discover repo "s/*" "main"))
           (chain (magit-dash-chain-validate (magit-dash-stack-to-chain stack))))
      (should-not (plist-get chain :errors))
      (should (equal '("s/a" "s/b" "s/c")
                     (mapcar (lambda (n) (plist-get n :branch)) (plist-get chain :nodes))))
      (should (cl-every (lambda (n) (null (plist-get n :number))) (plist-get chain :nodes)))
      (let ((plan (magit-dash-chain-create-plan chain)))
        (should (equal '(update update update)
                       (mapcar (lambda (s) (plist-get (hitl-plan-step-target s) :kind))
                               (hitl-plan-steps plan))))
        (should (equal '("s/a" "s/b") (mapcar (lambda (s) (plist-get (hitl-plan-step-target s) :source))
                                              (cdr (hitl-plan-steps plan)))))
        ;; the single-branch stack has nothing to sync
        (let ((one (magit-dash-stack-discover repo "s/a" "main")))
          (should-error (magit-dash-stack-sync one) :type 'user-error))))))

(defmacro stack-test--capturing-review (var &rest body)
  "Run BODY with `hitl-plan-review' stubbed to record its plan in VAR."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'hitl-plan-review)
              (lambda (plan &rest _) (setq ,var plan))))
     ,@body))

(ert-deftest magit-dash-stack/sync-end-to-end ()
  (stack-test--with-env (root)
    (let* ((repo (stack-test--linear root))
           (reviewed nil))
      (magit-dash-stack-discover repo "s/*" "main")
      (stack-test--commit-on repo "s/a" "a2.txt" "more\n")
      (let* ((origin-before (stack-test--git repo "ls-remote" "origin"))
             (stack (magit-dash-stack-discover repo "s/*" "main"))
             (plan (stack-test--capturing-review reviewed
                     (magit-dash-stack-sync stack))))
        (should (eq plan reviewed))
        (should (eq 'stack (plist-get (hitl-plan-metadata plan) :origin)))
        (should (eq 'magit-dash-chain (hitl-plan-domain plan)))
        ;; nothing has run until the plan is executed
        (should (equal "" (stack-test--git repo "rev-list" "--merges" "s/b")))
        (hitl-plan-execute plan)
        (should (eq 'complete (hitl-plan-status plan)))
        (let ((a (stack-test--git repo "rev-parse" "s/a"))
              (b (stack-test--git repo "rev-parse" "s/b")))
          (should (= 2 (length (magit-dash-chain-test--parents repo "s/b"))))
          (should (member a (magit-dash-chain-test--parents repo "s/b")))
          (should (member b (magit-dash-chain-test--parents repo "s/c"))))
        (should (equal origin-before (stack-test--git repo "ls-remote" "origin")))
        (let ((after (magit-dash-stack-discover repo "s/*" "main")))
          (should (equal '(nil nil nil)
                         (mapcar (lambda (n) (plist-get n :drift)) (plist-get after :nodes))))
          (should (equal '(nil "s/a" "s/b") (stack-test--parents after))))))))

(ert-deftest magit-dash-stack/sync-fetch-updates-remote-tracking-base ()
  (stack-test--with-env (root)
    (let* ((repo (stack-test--linear root))
           (other (expand-file-name "other" root))
           (reviewed nil))
      (call-process "git" nil nil nil "clone" "-q" (expand-file-name "origin.git" root) other)
      (stack-test--git other "config" "user.name" "T")
      (stack-test--git other "config" "user.email" "t@example.invalid")
      (magit-dash-chain-test--commit other "up.txt" "up\n" "upstream")
      (stack-test--git other "push" "-q" "origin" "main")
      (let ((stack (magit-dash-stack-discover repo "s/*" "main"))
            (before (stack-test--git repo "rev-parse" "origin/main")))
        (stack-test--capturing-review reviewed
          (magit-dash-stack-sync stack nil t))
        (should-not (equal before (stack-test--git repo "rev-parse" "origin/main")))
        (should reviewed)))))

(ert-deftest magit-dash-stack/sync-conflict-halts-and-resume-completes ()
  (stack-test--with-env (root)
    (let* ((repo (magit-dash-chain-test--make-repo root))
           (reviewed nil))
      (magit-dash-chain-test--commit repo "shared.txt" "one\n" "shared")
      (stack-test--git repo "push" "-q" "origin" "main")
      (magit-dash-chain-test--branch repo "s/a" "main" "shared.txt" "from a\n")
      (magit-dash-chain-test--branch repo "s/b" "s/a" "b.txt" "b\n")
      (stack-test--worktree repo "s/a")
      (stack-test--worktree repo "s/b")
      (magit-dash-stack-discover repo "s/*" "main")
      (magit-dash-chain-test--advance-main repo "shared.txt" "from main\n")
      (let* ((stack (magit-dash-stack-discover repo "s/*" "main"))
             (plan (stack-test--capturing-review reviewed
                     (magit-dash-stack-sync stack)))
             (wt (stack-test--wt repo "s/a")))
        (should (equal 1 (plist-get (stack-test--node stack "s/a") :drift)))
        (hitl-plan-execute plan)
        (should (eq 'paused (hitl-plan-status plan)))
        (should (magit-dash-chain--merge-in-progress-p wt))
        (should (equal '("shared.txt")
                       (plist-get (magit-dash-chain--result plan 1) :conflicts)))
        ;; a blocked step can be rolled back by aborting the merge...
        (let ((out (magit-dash-chain-rollback plan)))
          (should (equal '((1 aborted nil)) out))
          (should-not (magit-dash-chain--merge-in-progress-p wt)))
        ;; ...then run again, resolve, and resume
        (hitl-plan-execute plan)
        (should (magit-dash-chain--merge-in-progress-p wt))
        (magit-dash-chain-test--write wt "shared.txt" "resolved\n")
        (stack-test--git wt "add" "shared.txt")
        (magit-dash-chain-resume plan)
        (should (eq 'complete (hitl-plan-status plan)))
        (should-not (magit-dash-chain--merge-in-progress-p wt))
        (should (member (stack-test--git repo "rev-parse" "s/a")
                        (magit-dash-chain-test--parents repo "s/b")))
        (should-not (plist-get (stack-test--node (magit-dash-stack-discover repo "s/*" "main")
                                                 "s/b")
                               :drift))))))

(ert-deftest magit-dash-stack/rollback-resets-then-refuses-after-new-commit ()
  (stack-test--with-env (root)
    (let* ((repo (stack-test--linear root))
           (reviewed nil))
      (magit-dash-stack-discover repo "s/*" "main")
      (stack-test--commit-on repo "s/a" "a2.txt" "more\n")
      (let* ((before-b (stack-test--git repo "rev-parse" "s/b"))
             (before-c (stack-test--git repo "rev-parse" "s/c"))
             (plan (stack-test--capturing-review reviewed
                     (magit-dash-stack-sync (magit-dash-stack-discover repo "s/*" "main")))))
        (hitl-plan-execute plan)
        (should (eq 'complete (hitl-plan-status plan)))
        ;; a commit lands on s/c after the sync: its step is refused, the others reset
        (stack-test--commit-on repo "s/c" "c2.txt" "extra\n")
        (let ((out (magit-dash-chain-rollback plan)))
          (should (eq 'refused (nth 1 (assq 3 out))))
          (should (eq 'reset (nth 1 (assq 2 out))))
          (should (eq 'refused (nth 1 (assq 1 out)))))
        (should (equal before-b (stack-test--git repo "rev-parse" "s/b")))
        (should-not (equal before-c (stack-test--git repo "rev-parse" "s/c")))))))

(ert-deftest magit-dash-stack/sync-tree-two-children-end-to-end ()
  (stack-test--with-env (root)
    (let* ((repo (stack-test--setup root
                                    '("s/a" "main" "a.txt" "a\n")
                                    '("s/b" "s/a" "b.txt" "b\n")
                                    '("s/c" "s/a" "c.txt" "c\n")))
           (reviewed nil))
      (magit-dash-stack-discover repo "s/*" "main")
      (stack-test--commit-on repo "s/a" "a2.txt" "more\n")
      (let ((plan (stack-test--capturing-review reviewed
                    (magit-dash-stack-sync (magit-dash-stack-discover repo "s/*" "main")))))
        (hitl-plan-execute plan)
        (should (eq 'complete (hitl-plan-status plan)))
        (let ((a (stack-test--git repo "rev-parse" "s/a")))
          (should (member a (magit-dash-chain-test--parents repo "s/b")))
          (should (member a (magit-dash-chain-test--parents repo "s/c"))))))))

(ert-deftest magit-dash-stack/sync-root-above-bottom ()
  (stack-test--with-env (root)
    (let* ((repo (stack-test--linear root))
           (reviewed nil)
           (a0 (stack-test--git repo "rev-parse" "s/a")))
      (magit-dash-stack-discover repo "s/*" "main")
      (stack-test--commit-on repo "s/b" "b2.txt" "more\n")
      (let ((plan (stack-test--capturing-review reviewed
                    (magit-dash-stack-sync (magit-dash-stack-discover repo "s/*" "main")
                                           "s/b"))))
        (should (equal '("s/b" "s/c")
                       (mapcar (lambda (s) (plist-get (hitl-plan-step-target s) :branch))
                               (hitl-plan-steps plan))))
        (hitl-plan-execute plan)
        (should (member (stack-test--git repo "rev-parse" "s/b")
                        (magit-dash-chain-test--parents repo "s/c")))
        (should (equal a0 (stack-test--git repo "rev-parse" "s/a")))))))

(ert-deftest magit-dash-stack/same-tip-elsewhere-does-not-block-subtree ()
  (stack-test--with-env (root)
    (let ((repo (stack-test--setup root
                                   '("s/a" "main" "a.txt" "a\n")
                                   '("s/b" "s/a" "b.txt" "b\n")
                                   '("s/x" "main" "x.txt" "x\n"))))
      (stack-test--git repo "branch" "s/y" "s/x")
      (let* ((stack (magit-dash-stack-discover repo "s/*" "main"))
             (sub (magit-dash-stack-to-chain stack "s/a")))
        (should (= 1 (length (delete-dups (mapcar (lambda (n) (plist-get n :tip))
                                                  (cl-remove-if-not
                                                   (lambda (n) (member (plist-get n :branch) '("s/x" "s/y")))
                                                   (plist-get stack :nodes)))))))
        (should-not (plist-get sub :problems))
        (should-not (plist-get (magit-dash-chain-validate sub) :errors))))))

(ert-deftest magit-dash-stack/mcp-show-requires-prefix ()
  (stack-test--with-env (root)
    (let ((repo (stack-test--linear root)))
      (should-error (magit-dash-mcp-call-tool "stack_show" (list :repo repo))))))

(ert-deftest magit-dash-stack/mcp-sync-validation-error-creates-no-plan ()
  (stack-test--with-env (root)
    (let ((repo (stack-test--setup root
                                   '("s/a" "main" "a.txt" "a\n")
                                   '("s/b" "s/a" "b.txt" "b\n")
                                   '("s/x" "main" "x.txt" "x\n"))))
      (cl-letf (((symbol-function 'magit-dash-mcp--resolve-repo-slug) (lambda (&rest _) "o/r")))
        (let ((res (magit-dash-mcp-call-tool "stack_sync" (list :repo repo :prefix "s/*"))))
          (should (eq :false (plist-get res :valid)))
          (should (> (length (plist-get res :errors)) 0))
          (should (eq :null (plist-get res :plan_id))))))))

(ert-deftest magit-dash-stack/rollback-single-step-by-id ()
  (stack-test--with-env (root)
    (let* ((repo (stack-test--linear root))
           (reviewed nil))
      (magit-dash-stack-discover repo "s/*" "main")
      (stack-test--commit-on repo "s/a" "a2.txt" "more\n")
      (let* ((before-b (stack-test--git repo "rev-parse" "s/b"))
             (plan (stack-test--capturing-review reviewed
                     (magit-dash-stack-sync (magit-dash-stack-discover repo "s/*" "main")))))
        (hitl-plan-execute plan)
        (let ((c (stack-test--git repo "rev-parse" "s/c")))
          (should (equal '((2 reset nil)) (magit-dash-chain-rollback plan 2)))
          (should (equal before-b (stack-test--git repo "rev-parse" "s/b")))
          (should (equal c (stack-test--git repo "rev-parse" "s/c"))))))))

;;;; Module properties

(defun stack-test--source ()
  (with-temp-buffer (insert-file-contents magit-dash-stack-test--source) (buffer-string)))

(ert-deftest magit-dash-stack/never-calls-chain-internals ()
  (should-not (string-match-p "magit-dash-chain--" (stack-test--source))))

(ert-deftest magit-dash-stack/never-mutates-through-git ()
  (should-not (string-match-p "\"\\(merge\\|rebase\\|push\\|reset\\|checkout\\)\"" (stack-test--source)))
  (should-not (string-match-p "--force" (stack-test--source))))

(ert-deftest magit-dash-stack/load-runs-no-process-and-changes-no-global-state ()
  (let* ((keymap-before (copy-tree global-map))
         (buffers-before (seq-remove (lambda (b) (string-match-p "Native-compile" (buffer-name b)))
                                     (buffer-list)))
         (dir default-directory)
         (hooks (list after-load-alist)))
    ;; Coverage's load handler visits the source and invokes VC, which would
    ;; otherwise be mistaken for effects of loading the module itself.
    (let ((file-name-handler-alist
           (rassq-delete-all 'undercover-file-handler
                             (copy-sequence file-name-handler-alist))))
      (cl-letf (((symbol-function 'call-process) (lambda (&rest _) (error "process at load")))
                ((symbol-function 'process-file) (lambda (&rest _) (error "process at load")))
                ((symbol-function 'start-process) (lambda (&rest _) (error "process at load")))
                ((symbol-function 'make-process) (lambda (&rest _) (error "process at load"))))
        (load magit-dash-stack-test--source nil t t)))
    (should (equal keymap-before global-map))
    (should (equal buffers-before
                   (seq-remove (lambda (b) (string-match-p "Native-compile" (buffer-name b)))
                               (buffer-list))))
    (should (equal dir default-directory))
    (should (equal hooks (list after-load-alist)))))

(provide 'test-magit-dash-stack)
;;; test-magit-dash-stack.el ends here

;;;; MCP surface

(ert-deftest magit-dash-stack/mcp-tools-registered-and-show-writes-nothing ()
  (let ((tools (mcpkit-service-tools (mcpkit-get-service 'magit-dash))))
    (should (gethash "stack_show" tools))
    (should (gethash "stack_sync" tools)))
  (stack-test--with-env (root)
    (let ((repo (stack-test--linear root)))
      (cl-letf (((symbol-function 'magit-dash-mcp--resolve-repo-slug) (lambda (&rest _) "o/r")))
        (let ((res (magit-dash-mcp-call-tool "stack_show" (list :repo repo :prefix "s/*"
                                                                :base "origin/main"))))
          (should (equal "main" (plist-get res :base)))
          (should (= 3 (length (plist-get res :nodes))))
          (let ((n (aref (plist-get res :nodes) 1)))
            (should (equal "s/b" (plist-get n :branch)))
            (should (equal "s/a" (plist-get n :parent)))
            (should (= 1 (plist-get n :commit_count)))
            (should (= 1 (plist-get n :lines_added))))
          (should (string-match-p "s/c" (plist-get res :tree)))
          (should (string-match-p "digraph" (plist-get res :dot)))
          (should (stringp (plist-get res :mermaid)))
          ;; read-only: no recorded parents, no plans
          (should-not (magit-get-all "branch.s/b.magit-dash-stack-parent"))
          (should-not (stack-test--lines-config repo)))))))

(defun stack-test--lines-config (repo)
  (let ((default-directory repo))
    (magit-git-lines "config" "--get-regexp" "magit-dash-stack-parent")))

(ert-deftest magit-dash-stack/mcp-sync-creates-gated-plan ()
  (stack-test--with-env (root)
    (let* ((repo (stack-test--linear root))
           (hitl-plan-agent-may-apply nil))
      (cl-letf (((symbol-function 'magit-dash-mcp--resolve-repo-slug) (lambda (&rest _) "o/r")))
        ;; record parents (as an interactive view does), then drift s/b
        (magit-dash-stack-discover repo "s/*" "main")
        (stack-test--commit-on repo "s/a" "a2.txt" "more\n")
        (let* ((res (magit-dash-mcp-call-tool "stack_sync" (list :repo repo :prefix "s/*")))
               (plan (hitl-plan-get (plist-get res :plan_id) nil 'magit-dash-chain)))
          (should (equal [] (plist-get res :errors)))
          (should (eq t (plist-get res :valid)))
          (should (hitl-plan-origin-mcp-p plan))
          (should (= 3 (length (plist-get res :steps))))
          (should (stringp (plist-get res :record_file)))
          (should (equal "awaiting_user_confirmation"
                         (plist-get (magit-dash-mcp-call-tool
                                     "chain_resume" (list :plan_id (hitl-plan-id plan)))
                                    :status)))
          (let ((hitl-plan-agent-may-apply t))
            (should (equal "complete"
                           (plist-get (magit-dash-mcp-call-tool
                                       "chain_resume" (list :plan_id (hitl-plan-id plan)))
                                      :status)))))
        ;; fewer than two branches: no plan
        (let ((res (magit-dash-mcp-call-tool "stack_sync" (list :repo repo :prefix "s/c"))))
          (should (eq :false (plist-get res :valid)))
          (should (eq :null (plist-get res :plan_id))))))))
