;;; test-magit-dash-remote-divergence.el --- Secondary remote divergence tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Local Git fixtures exercise same-branch divergence from other remotes.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'map)
(require 'magit-dash)
(require 'test-helper)

(defmacro magit-dash-remote-test--with-repo (&rest body)
  "Run BODY with a local repository and two missing remote commits."
  (declare (indent 0))
  `(let* ((dir (magit-dash-test--make-repo))
          (repo (magit-dash-repo--make :name "test" :path dir))
          (magit-dash-gh--cache (make-hash-table :test #'equal))
          (magit-dash-gh--cache-requests (make-hash-table :test #'equal)))
     (unwind-protect
         (progn
           (magit-dash-test--git dir "remote" "add" "origin" dir)
           (magit-dash-test--git dir "remote" "add" "other" dir)
           (magit-dash-test--git dir "update-ref" "refs/remotes/origin/main" "HEAD")
           (magit-dash-test--git dir "branch" "--set-upstream-to=origin/main" "main")
           (magit-dash-test--commit dir "second")
           (magit-dash-test--commit dir "third")
           (magit-dash-test--git dir "update-ref" "refs/remotes/other/main" "HEAD")
           (magit-dash-test--git dir "update-ref" "refs/remotes/unrelated/master" "HEAD")
           (magit-dash-test--git dir "reset" "--hard" "origin/main")
           ,@body)
       (delete-directory dir t))))

(ert-deftest magit-dash-remote-divergence-sync ()
  (magit-dash-remote-test--with-repo
    (magit-dash-test--git dir "update-ref" "refs/remotes/other/feature/main" "other/main")
    (let ((stats (magit-dash--collect-stats repo)))
      (should (equal '(("other/main" . 2)) (plist-get stats :other-remote-behind)))
      (should (zerop (plist-get stats :ahead)))
      (should (zerop (plist-get stats :behind)))
      (magit-dash-test--git dir "remote" "add" "third" dir)
      (magit-dash-test--git dir "update-ref" "refs/remotes/third/main" "other/main")
      (should (equal '(("other/main" . 2) ("third/main" . 2))
                     (plist-get (magit-dash--collect-stats repo) :other-remote-behind))))))

(ert-deftest magit-dash-remote-divergence-exact-branch-and-tracking-remote ()
  (magit-dash-remote-test--with-repo
    (magit-dash-test--git dir "branch" "--set-upstream-to=other/main" "main")
    (magit-dash-test--git dir "update-ref" "refs/remotes/origin/main" "other/main~1")
    (let ((stats (magit-dash--collect-stats repo)))
      (should (= 2 (plist-get stats :behind)))
      (should (equal '(("origin/main" . 1)) (plist-get stats :other-remote-behind))))
    (magit-dash-test--git dir "checkout" "-b" "feature/nested")
    (should-not (plist-get (magit-dash--collect-stats repo) :other-remote-behind))
    (magit-dash-test--git dir "update-ref" "refs/remotes/other/feature/nested" "other/main")
    (should (equal '(("other/feature/nested" . 2))
                   (plist-get (magit-dash--collect-stats repo) :other-remote-behind)))
    (magit-dash-test--git dir "checkout" "--detach" "HEAD")
    (should-not (plist-get (magit-dash--collect-stats repo) :other-remote-behind))))

(ert-deftest magit-dash-remote-divergence-async-matches-sync ()
  (magit-dash-remote-test--with-repo
    (let ((expected (plist-get (magit-dash--collect-stats repo) :other-remote-behind))
          result)
      (should (equal '(("other/main" . 2)) expected))
      ;; Run real Git commands immediately to make callback plumbing deterministic.
      (cl-letf (((symbol-function 'magit-dash--run-git)
                 (lambda (path args success &optional failure)
                   (with-temp-buffer
                     (let ((exit (apply #'call-process "git" nil t nil "-C" path args)))
                       (if (zerop exit)
                           (funcall success (buffer-string))
                         (when failure (funcall failure (buffer-string) exit))))))))
        (magit-dash--collect-stats-async repo (lambda (stats) (setq result stats))))
      (should (equal expected (plist-get result :other-remote-behind))))))

(ert-deftest magit-dash-remote-divergence-rendering-and-unknown ()
  (let ((text (magit-dash--format-status
               1 2 nil nil nil nil '(("other/main" . 3) ("third/main" . nil)))))
    (should (equal "↑1 ↓2 →3 →?" (substring-no-properties text)))
    (should (string-match-p "other/main" (get-text-property 6 'help-echo text)))
    (should (string-match-p "unavailable" (get-text-property 9 'help-echo text)))))

(ert-deftest magit-dash-remote-divergence-count-error-is-unknown ()
  (magit-dash-remote-test--with-repo
    (let ((original (symbol-function 'magit-git-string)))
      (cl-letf (((symbol-function 'magit-git-string)
                 (lambda (&rest args)
                   (unless (member "HEAD..refs/remotes/other/main" args)
                     (apply original args)))))
        (should (equal '(("other/main"))
                       (plist-get (magit-dash--collect-stats repo) :other-remote-behind)))))))

(ert-deftest magit-dash-remote-divergence-async-error-is-unknown ()
  (let (result)
    (cl-letf (((symbol-function 'magit-dash--run-git)
               (lambda (_path _args _success failure) (funcall failure "bad ref" 1))))
      (magit-dash--collect-other-remote-behind-async
       "/tmp/" '("refs/remotes/other/main") (lambda (counts) (setq result counts))))
    (should (equal '(("other/main")) result))))

(ert-deftest magit-dash-remote-divergence-status-width-after-update ()
  (with-temp-buffer
    (let* ((repo (magit-dash-repo--make :name "test" :path "/tmp/divergence-test/"))
           (magit-dash-columns '((status . t)))
           (stats '(:branch "main"))
           (tabulated-list-entries (list (list repo [""]))))
      (tabulated-list-mode)
      (cl-letf (((symbol-function 'magit-dash-gh--cache-get)
                 (lambda (_path key) (and (eq key :stats) stats)))
                ((symbol-function 'magit-dash--get-stats-fast) (lambda (_repo) stats))
                ((symbol-function 'magit-dash--repo-is-pr-p) #'ignore)
                ((symbol-function 'magit-dash--print) #'ignore))
        (setq tabulated-list-format (magit-dash--build-format (list repo)))
        (should (= 8 (cadr (aref tabulated-list-format 0))))
        (setq stats '(:branch "main" :other-remote-behind (("other/main" . 20))))
        (magit-dash--update-entry repo)
        (should (= 12 (cadr (aref tabulated-list-format 0))))
        (should (string-match-p "→20" (aref (cadar tabulated-list-entries) 0)))))))

(ert-deftest magit-dash-remote-divergence-slashes-in-remote-and-branch ()
  (should (equal '("refs/remotes/mirror/fork/feature/nested")
                 (magit-dash--other-remote-refs
                  "feature/nested" '("refs/heads/feature/nested\torigin")
                  '("refs/remotes/origin/feature/nested"
                    "refs/remotes/mirror/fork/feature/nested"
                    "refs/remotes/mirror/fork/other/feature/nested")
                  '("origin" "mirror/fork")))))

(provide 'test-magit-dash-remote-divergence)
;;; test-magit-dash-remote-divergence.el ends here
