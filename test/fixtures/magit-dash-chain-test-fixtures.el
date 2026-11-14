;;; magit-dash-chain-test-fixtures.el --- Shared chain test fixtures -*- lexical-binding: t; no-byte-compile: t; -*-

(require 'cl-lib)
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

(defun magit-dash-chain-test--parents (dir rev)
  (cdr (split-string (magit-dash-chain-test--git dir "rev-list" "--parents" "-n1" rev))))

(provide 'magit-dash-chain-test-fixtures)
;;; magit-dash-chain-test-fixtures.el ends here
