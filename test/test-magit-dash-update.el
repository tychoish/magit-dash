;;; test-magit-dash-update.el --- ERT tests for magit-dash-update -*- lexical-binding: t; no-byte-compile: t; -*-

;; Real git against temporary repositories with a local bare "origin".
;; No network, no gh.
;;
;; Batch run:
;;   emacs --batch -l test/test-helper.el -l test/test-magit-dash-update.el \
;;     --eval '(ert-run-tests-batch-and-exit "magit-dash-update/")'

(require 'ert)
(require 'cl-lib)
(require 'magit-dash)
(require 'magit-dash-mcp)
(require 'magit-dash-update)

(require 'magit-dash-chain-test-fixtures
         (expand-file-name "fixtures/magit-dash-chain-test-fixtures.el"
                           (file-name-directory (or load-file-name buffer-file-name))))

(defun magit-dash-update-test--worktree (repo name)
  "Create branch NAME off main in a worktree of REPO with one commit; return it."
  (let ((wt (expand-file-name (concat ".wt/" name) repo)))
    (magit-dash-chain-test--git repo "worktree" "add" "-q" "-b" name wt "main")
    (magit-dash-chain-test--git wt "config" "user.name" "Test")
    (magit-dash-chain-test--git wt "config" "user.email" "test@example.invalid")
    (magit-dash-chain-test--git wt "config" "commit.gpgsign" "false")
    (magit-dash-chain-test--commit wt (concat name ".txt") (concat name "\n") (concat "work " name))
    wt))

(defun magit-dash-update-test--conflict (root)
  "Repo whose topic worktree conflicts with main on shared.txt; return (REPO . WT)."
  (let ((repo (magit-dash-chain-test--make-repo root)))
    (magit-dash-chain-test--commit repo "shared.txt" "one\ntwo\n" "shared")
    (magit-dash-chain-test--git repo "push" "-q" "origin" "main")
    (let ((wt (magit-dash-update-test--worktree repo "topic")))
      (magit-dash-chain-test--commit wt "shared.txt" "one\nfrom topic\n" "topic edit")
      (magit-dash-chain-test--advance-main repo "shared.txt" "one\nfrom main\n")
      (cons repo wt))))

(defun magit-dash-update-test--fetches ()
  (seq-count (lambda (argv) (equal (car argv) "fetch")) magit-dash-chain-test--git-log))

;;;; State

(ert-deftest magit-dash-update/state-reports-branch-dirty-and-merge ()
  (magit-dash-chain-test--with-env (root)
    (let* ((repo (magit-dash-chain-test--make-repo root))
           (wt (magit-dash-update-test--worktree repo "topic"))
           (st (magit-dash-chain--state wt)))
      (should (equal "topic" (plist-get st :branch)))
      (should (equal (magit-dash-chain-test--git wt "rev-parse" "HEAD") (plist-get st :head)))
      (should-not (plist-get st :dirty))
      (should-not (plist-get st :merging))
      (with-temp-file (expand-file-name "topic.txt" wt) (insert "changed\n"))
      (should (plist-get (magit-dash-chain--state wt) :dirty))
      ;; untracked files are ignored
      (magit-dash-chain-test--git wt "checkout" "-q" "--" "topic.txt")
      (with-temp-file (expand-file-name "new.txt" wt) (insert "x\n"))
      (should-not (plist-get (magit-dash-chain--state wt) :dirty)))))

(ert-deftest magit-dash-update/unmerged-lists-paths-with-kinds ()
  (magit-dash-chain-test--with-env (root)
    (pcase-let* ((`(,_repo . ,wt) (magit-dash-update-test--conflict root)))
      (magit-dash-update-branch wt)
      (let ((st (magit-dash-chain--state wt)))
        (should (plist-get st :merging))
        (should (equal '("shared.txt") (plist-get st :unmerged))))
      (should (equal '(("shared.txt" . "both-modified")) (magit-dash-chain--unmerged wt))))))

(ert-deftest magit-dash-update/unmerged-kinds-for-delete-conflicts ()
  (magit-dash-chain-test--with-env (root)
    (let* ((repo (magit-dash-chain-test--make-repo root)))
      (magit-dash-chain-test--commit repo "gone.txt" "x\n" "add gone")
      (magit-dash-chain-test--git repo "push" "-q" "origin" "main")
      (let ((wt (magit-dash-update-test--worktree repo "topic")))
        (magit-dash-chain-test--commit wt "gone.txt" "edited\n" "topic edits gone")
        (magit-dash-chain-test--git repo "rm" "-q" "gone.txt")
        (magit-dash-chain-test--git repo "commit" "-q" "-m" "main deletes gone")
        (magit-dash-chain-test--git repo "push" "-q" "origin" "main")
        (let ((res (magit-dash-update-branch wt)))
          (should (eq 'conflict (plist-get res :outcome)))
          (let ((c (car (plist-get res :conflicts))))
            (should (equal "deleted-by-them" (plist-get c :kind)))
            (should (= 0 (plist-get c :hunks))))
          ;; no markers: verify flags it for review rather than failing
          (let ((v (magit-dash-update-verify wt)))
            (should (equal '("gone.txt") (plist-get v :needs_review)))))))))

;;;; Update

(ert-deftest magit-dash-update/clean-merge-fetches-only-the-base ()
  (magit-dash-chain-test--with-env (root)
    (let* ((repo (magit-dash-chain-test--make-repo root))
           (wt (magit-dash-update-test--worktree repo "topic")))
      (magit-dash-chain-test--advance-main repo "m.txt" "main moved\n")
      (setq magit-dash-chain-test--git-log nil)
      (let ((res (magit-dash-update-branch wt)))
        (should (eq 'merged (plist-get res :outcome)))
        (should (equal "main" (plist-get res :base)))
        (should (equal (magit-dash-chain-test--git wt "rev-parse" "HEAD") (plist-get res :sha)))
        (should (= 2 (length (magit-dash-chain-test--parents wt "HEAD"))))
        (should (file-exists-p (expand-file-name "m.txt" wt)))
        (should (member '("fetch" "origin" "main" "--quiet") magit-dash-chain-test--git-log))
        (should-not (cl-find-if (lambda (argv) (member (car argv) '("push" "rebase" "pull")))
                                magit-dash-chain-test--git-log))))))

(ert-deftest magit-dash-update/clean-update-stays-within-git-call-budget ()
  "Latency guard: status, fetch, merge and rev-parse, plus base discovery."
  (magit-dash-chain-test--with-env (root)
    (let* ((repo (magit-dash-chain-test--make-repo root))
           (wt (magit-dash-update-test--worktree repo "topic")))
      (magit-dash-chain-test--advance-main repo "m.txt" "main moved\n")
      (setq magit-dash-chain-test--git-log nil)
      (magit-dash-update-branch wt)
      ;; the fixture has no origin/HEAD, so base discovery takes two calls
      (should (<= (length magit-dash-chain-test--git-log) 6))
      (magit-dash-chain-test--advance-main repo "m2.txt" "again\n")
      (setq magit-dash-chain-test--git-log nil)
      (magit-dash-update-branch wt :base "main")
      (should (= 4 (length magit-dash-chain-test--git-log)))
      (magit-dash-chain-test--advance-main repo "m3.txt" "again\n")
      (magit-dash-chain-test--git wt "fetch" "-q" "origin" "main")
      (setq magit-dash-chain-test--git-log nil)
      (magit-dash-update-branch wt :base "main" :fetch nil)
      (should (= 3 (length magit-dash-chain-test--git-log))))))

(ert-deftest magit-dash-update/up-to-date-makes-no-commit ()
  (magit-dash-chain-test--with-env (root)
    (let* ((repo (magit-dash-chain-test--make-repo root))
           (wt (magit-dash-update-test--worktree repo "topic"))
           (head (magit-dash-chain-test--git wt "rev-parse" "HEAD")))
      (let ((res (magit-dash-update-branch wt)))
        (should (eq 'up-to-date (plist-get res :outcome)))
        (should (equal head (plist-get res :sha)))
        (should (equal head (magit-dash-chain-test--git wt "rev-parse" "HEAD")))))))

(ert-deftest magit-dash-update/status-only-reports-ahead-behind ()
  (magit-dash-chain-test--with-env (root)
    (let* ((repo (magit-dash-chain-test--make-repo root))
           (wt (magit-dash-update-test--worktree repo "topic")))
      (magit-dash-chain-test--advance-main repo "m.txt" "main moved\n")
      (let ((res (magit-dash-update-branch wt :merge nil)))
        (should (= 1 (plist-get res :ahead)))
        (should (= 1 (plist-get res :behind)))
        (should-not (plist-get res :outcome))
        (should-not (magit-dash-update-test--merge-p wt))))))

(defun magit-dash-update-test--merge-p (wt)
  (cdr (magit-dash-chain-test--parents wt "HEAD")))

(ert-deftest magit-dash-update/status-only-tolerates-a-dirty-tree ()
  (magit-dash-chain-test--with-env (root)
    (let* ((repo (magit-dash-chain-test--make-repo root))
           (wt (magit-dash-update-test--worktree repo "topic")))
      (with-temp-file (expand-file-name "topic.txt" wt) (insert "dirty\n"))
      (let ((res (magit-dash-update-branch wt :merge nil)))
        (should (plist-get res :dirty))))))

(ert-deftest magit-dash-update/refuses-unsafe-states ()
  (magit-dash-chain-test--with-env (root)
    (let* ((repo (magit-dash-chain-test--make-repo root))
           (wt (magit-dash-update-test--worktree repo "topic")))
      (should-error (magit-dash-update-branch repo) :type 'error)
      (with-temp-file (expand-file-name "topic.txt" wt) (insert "dirty\n"))
      (let ((err (should-error (magit-dash-update-branch wt))))
        (should (string-match-p "Dirty" (cadr err))))
      (magit-dash-chain-test--git wt "checkout" "-q" "--" "topic.txt")
      (magit-dash-chain-test--git wt "checkout" "-q" "--detach")
      (let ((err (should-error (magit-dash-update-branch wt))))
        (should (string-match-p "Detached" (cadr err))))
      (magit-dash-chain-test--git wt "checkout" "-q" "topic")
      (should-error (magit-dash-update-branch wt :base "--evil")))))

(ert-deftest magit-dash-update/conflict-reports-hunks-and-leaves-merge-state ()
  (magit-dash-chain-test--with-env (root)
    (pcase-let* ((`(,_repo . ,wt) (magit-dash-update-test--conflict root))
                 (res (magit-dash-update-branch wt :include-hunks t)))
      (should (eq 'conflict (plist-get res :outcome)))
      (should (magit-dash-chain--merge-in-progress-p wt))
      (let* ((c (car (plist-get res :conflicts)))
             (h (car (plist-get c :conflicts))))
        (should (equal "shared.txt" (plist-get c :file)))
        (should (equal "both-modified" (plist-get c :kind)))
        (should (= 1 (plist-get c :hunks)))
        (should (equal "from topic\n" (plist-get h :ours)))
        (should (equal "from main\n" (plist-get h :theirs)))))))

(ert-deftest magit-dash-update/hunk-text-respects-the-budget ()
  (magit-dash-chain-test--with-env (root)
    (pcase-let* ((`(,_repo . ,wt) (magit-dash-update-test--conflict root))
                 (res (magit-dash-update-branch wt :include-hunks t :max-chars 3))
                 (h (car (plist-get (car (plist-get res :conflicts)) :conflicts))))
      (should (plist-get h :truncated))
      (should-not (plist-get h :ours)))))

(ert-deftest magit-dash-update/resolve-then-verify-commit-finishes-merge ()
  (magit-dash-chain-test--with-env (root)
    (pcase-let* ((`(,_repo . ,wt) (magit-dash-update-test--conflict root)))
      (magit-dash-update-branch wt)
      (let ((rs (magit-dash-update-resolve wt '((:file "shared.txt" :take "both")))))
        (should (equal '(1 1 0 t)
                       (list (plist-get (car rs) :hunks) (plist-get (car rs) :resolved)
                             (plist-get (car rs) :remaining) (plist-get (car rs) :staged)))))
      (should (equal "one\nfrom topic\nfrom main\n"
                     (with-temp-buffer (insert-file-contents (expand-file-name "shared.txt" wt))
                                       (buffer-string))))
      (let ((v (magit-dash-update-verify wt :commit t)))
        (should (plist-get v :ok))
        (should (plist-get v :committed))
        (should (equal (magit-dash-chain-test--git wt "rev-parse" "HEAD") (plist-get v :sha))))
      (should-not (magit-dash-chain--merge-in-progress-p wt))
      (should (= 2 (length (magit-dash-chain-test--parents wt "HEAD")))))))

(ert-deftest magit-dash-update/verify-fails-on-markers-and-never-commits ()
  (magit-dash-chain-test--with-env (root)
    (pcase-let* ((`(,_repo . ,wt) (magit-dash-update-test--conflict root)))
      (magit-dash-update-branch wt)
      (let ((v (magit-dash-update-verify wt :commit t)))
        (should-not (plist-get v :ok))
        (should (equal '("shared.txt") (plist-get v :markers)))
        (should-not (plist-get v :committed))
        (should (magit-dash-chain--merge-in-progress-p wt))))))

(ert-deftest magit-dash-update/second-call-finishes-a-resolved-merge ()
  "Editing the file by hand and calling update again completes the merge."
  (magit-dash-chain-test--with-env (root)
    (pcase-let* ((`(,_repo . ,wt) (magit-dash-update-test--conflict root)))
      (magit-dash-update-branch wt)
      (magit-dash-chain-test--write wt "shared.txt" "one\nresolved\n")
      (magit-dash-chain-test--git wt "add" "shared.txt")
      (let ((res (magit-dash-update-branch wt)))
        (should (eq 'resumed (plist-get res :outcome)))
        (should-not (magit-dash-chain--merge-in-progress-p wt))))))

(ert-deftest magit-dash-update/abort-restores-the-branch ()
  (magit-dash-chain-test--with-env (root)
    (pcase-let* ((`(,_repo . ,wt) (magit-dash-update-test--conflict root))
                 (before (magit-dash-chain-test--git wt "rev-parse" "HEAD")))
      (magit-dash-update-branch wt)
      (should (eq 'aborted (plist-get (magit-dash-update-branch wt :abort t) :outcome)))
      (should-not (magit-dash-chain--merge-in-progress-p wt))
      (should (equal before (magit-dash-chain-test--git wt "rev-parse" "HEAD")))
      (should-error (magit-dash-update-branch wt :abort t)))))

(ert-deftest magit-dash-update/batch-shares-one-fetch-and-isolates-errors ()
  (magit-dash-chain-test--with-env (root)
    (let* ((repo (magit-dash-chain-test--make-repo root))
           (a (magit-dash-update-test--worktree repo "topic-a"))
           (b (magit-dash-update-test--worktree repo "topic-b")))
      (magit-dash-chain-test--advance-main repo "m.txt" "main moved\n")
      (setq magit-dash-chain-test--git-log nil)
      (let ((rs (magit-dash-update-branches (list a b repo))))
        (should (equal '(merged merged) (mapcar (lambda (r) (plist-get r :outcome))
                                                 (seq-take rs 2))))
        (should (plist-get (nth 2 rs) :error))
        (should (= 1 (magit-dash-update-test--fetches)))))))

;;;; Hunk parsing and resolution

(defun magit-dash-update-test--parse (text)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert text)
    (magit-dash-update--parse-hunks)))

(ert-deftest magit-dash-update/parse-hunks-handles-diff3-and-multiple-hunks ()
  (let ((hunks (magit-dash-update-test--parse
                (concat "a\n<<<<<<< HEAD\nours1\n||||||| base\nbase1\n=======\ntheirs1\n"
                        ">>>>>>> origin/main\nmid\n<<<<<<< HEAD\nours2\n=======\ntheirs2\n"
                        ">>>>>>> origin/main\nend\n"))))
    (should (= 2 (length hunks)))
    (should (equal '("ours1\n" "theirs1\n" "base1\n")
                   (list (plist-get (car hunks) :ours) (plist-get (car hunks) :theirs)
                         (plist-get (car hunks) :base))))
    (should (equal '("ours2\n" "theirs2\n" nil)
                   (list (plist-get (cadr hunks) :ours) (plist-get (cadr hunks) :theirs)
                         (plist-get (cadr hunks) :base))))
    (should (= 2 (plist-get (car hunks) :line)))
    (should (= 10 (plist-get (cadr hunks) :line)))))

(ert-deftest magit-dash-update/resolve-per-hunk-text-and-crlf ()
  (magit-dash-chain-test--with-env (root)
    (let* ((repo (magit-dash-chain-test--make-repo root))
           (wt (magit-dash-update-test--worktree repo "topic"))
           (st (list :unmerged '("f.txt")))
           (file (expand-file-name "f.txt" wt)))
      (ignore st)
      ;; Resolve a file directly (bypassing git's unmerged check) via the helper.
      (let ((coding-system-for-write 'binary))
        (with-temp-file file
          (set-buffer-multibyte nil)
          (insert "x\r\n<<<<<<< HEAD\r\nA\r\n=======\r\nB\r\n>>>>>>> origin/main\r\ny\r\n"
                  "<<<<<<< HEAD\r\nC\r\n=======\r\nD\r\n>>>>>>> origin/main\r\n")))
      (let ((r (magit-dash-update--resolve-file
                wt "f.txt" '(:hunks ((:index 0 :take "theirs")
                                     (:index 1 :take "text" :text "café"))))))
        (should (equal '(2 2 0) (list (plist-get r :hunks) (plist-get r :resolved)
                                      (plist-get r :remaining)))))
      (should (equal "x\r\nB\r\ny\r\ncaf\303\251\n"
                     (with-temp-buffer
                       (set-buffer-multibyte nil)
                       (let ((coding-system-for-read 'binary)) (insert-file-contents file))
                       (buffer-string)))))))

(ert-deftest magit-dash-update/resolve-leaves-unchosen-hunks-and-unstaged ()
  (magit-dash-chain-test--with-env (root)
    (pcase-let* ((`(,_repo . ,wt) (magit-dash-update-test--conflict root)))
      (magit-dash-update-branch wt)
      (let ((r (car (magit-dash-update-resolve wt '((:file "shared.txt"))))))
        (should (= 1 (plist-get r :remaining)))
        (should-not (plist-get r :staged))))))

(ert-deftest magit-dash-update/resolve-only-touches-unmerged-files ()
  (magit-dash-chain-test--with-env (root)
    (pcase-let* ((`(,_repo . ,wt) (magit-dash-update-test--conflict root)))
      (magit-dash-update-branch wt)
      (let ((rs (magit-dash-update-resolve
                 wt '((:file "topic.txt" :take "ours") (:file "../outside" :take "ours")))))
        (should (cl-every (lambda (r) (plist-get r :error)) rs))))))

;;;; Go verification

(ert-deftest magit-dash-update/touched-go-packages-follow-the-merge-range ()
  (magit-dash-chain-test--with-env (root)
    (let* ((repo (magit-dash-chain-test--make-repo root))
           (wt (magit-dash-update-test--worktree repo "topic")))
      (make-directory (expand-file-name "pkg/a" repo) t)
      (make-directory (expand-file-name "tests/e2e" repo) t)
      (magit-dash-chain-test--commit repo "pkg/a/a.go" "package a\n" "go a")
      (magit-dash-chain-test--commit repo "tests/e2e/e.go" "package e\n" "go e")
      (magit-dash-chain-test--git repo "push" "-q" "origin" "main")
      (magit-dash-update-branch wt :merge t)
      (should (equal '("./pkg/a")
                     (magit-dash-update-touched-go-packages
                      wt (magit-dash-chain--state wt)))))))

(ert-deftest magit-dash-update/verify-vets-touched-go-packages ()
  (skip-unless (and (executable-find "go") (executable-find "gofmt")))
  (magit-dash-chain-test--with-env (root)
    (let* ((repo (magit-dash-chain-test--make-repo root))
           (wt (magit-dash-update-test--worktree repo "topic"))
           (magit-dash-update-go-env (list "GOFLAGS=-mod=mod" "GOTOOLCHAIN=local"
                                           "GOSUMDB=off" "GO111MODULE=on")))
      (magit-dash-chain-test--commit repo "go.mod" "module example.invalid/m\n\ngo 1.20\n" "mod")
      (make-directory (expand-file-name "pkg" repo) t)
      (magit-dash-chain-test--commit repo "pkg/p.go" "package pkg\n\nfunc F() int { return 1 }\n" "good")
      (magit-dash-chain-test--git repo "push" "-q" "origin" "main")
      (magit-dash-update-branch wt)
      (let ((v (magit-dash-update-verify wt)))
        (should (plist-get v :ok))
        (should (equal '("./pkg") (plist-get v :packages))))
      ;; main later breaks the package: verify must report it
      (magit-dash-chain-test--commit repo "pkg/p.go" "package pkg\n\nfunc F() int { return \"x\" }\n" "bad")
      (magit-dash-chain-test--git repo "push" "-q" "origin" "main")
      (magit-dash-update-branch wt)
      (let ((v (magit-dash-update-verify wt)))
        (should-not (plist-get v :ok))
        (should (cl-some (lambda (l) (string-match-p "p.go" l)) (plist-get v :failures)))))))

(ert-deftest magit-dash-update/verify-commits-tracked-fixes-with-a-message ()
  (magit-dash-chain-test--with-env (root)
    (let* ((repo (magit-dash-chain-test--make-repo root))
           (wt (magit-dash-update-test--worktree repo "topic")))
      (magit-dash-chain-test--write wt "topic.txt" "fixed\n")
      (let ((v (magit-dash-update-verify wt :commit t :message "fix: adapt")))
        (should (plist-get v :committed))
        (should (equal "fix: adapt" (magit-dash-chain-test--git wt "log" "-1" "--format=%s"))))
      ;; nothing left to commit
      (let ((v (magit-dash-update-verify wt :commit t)))
        (should-not (plist-get v :committed))))))

;;;; MCP tools

(ert-deftest magit-dash-update/mcp-tools-registered ()
  (dolist (tool '("branch_update" "branch_resolve" "branch_verify"))
    (should (gethash tool (mcpkit-service-tools (mcpkit-get-service 'magit-dash))))))

(ert-deftest magit-dash-update/mcp-end-to-end-with-short-name ()
  (magit-dash-chain-test--with-env (root)
    (pcase-let* ((`(,repo . ,wt) (magit-dash-update-test--conflict root)))
      ;; a short name resolves under <repo>/.wt
      (let ((res (magit-dash-mcp-call-tool "branch_update" (list :repo repo :worktree "topic"))))
        (should (equal "conflict" (plist-get res :outcome)))
        (should (equal wt (directory-file-name (plist-get res :worktree))))
        (let ((c (aref (plist-get res :conflicts) 0)))
          (should (equal "shared.txt" (plist-get c :file)))
          (should (= 1 (length (plist-get c :conflicts))))))
      (let ((res (magit-dash-mcp-call-tool
                  "branch_resolve"
                  (list :worktree wt :files (vector (list :file "shared.txt" :take "theirs"))))))
        (should (eq t (plist-get (aref (plist-get res :files) 0) :staged))))
      (let ((res (magit-dash-mcp-call-tool "branch_verify" (list :worktree wt :commit t))))
        (should (eq t (plist-get res :ok)))
        (should (eq t (plist-get res :committed)))
        (should (stringp (plist-get res :sha)))))))

(ert-deftest magit-dash-update/mcp-batch-and-status ()
  (magit-dash-chain-test--with-env (root)
    (let* ((repo (magit-dash-chain-test--make-repo root))
           (a (magit-dash-update-test--worktree repo "topic-a"))
           (b (magit-dash-update-test--worktree repo "topic-b")))
      (magit-dash-chain-test--advance-main repo "m.txt" "main moved\n")
      (let ((res (magit-dash-mcp-call-tool "branch_update"
                                           (list :worktrees (vector a b) :merge :false))))
        (should (equal '(1 1) (mapcar (lambda (r) (plist-get r :behind))
                                      (append (plist-get res :worktrees) nil)))))
      (should-error (magit-dash-mcp-call-tool "branch_update" (list :worktree "topic-a"))))))

(provide 'test-magit-dash-update)
;;; test-magit-dash-update.el ends here
