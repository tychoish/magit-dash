;;; test-magit-dash-actionable.el --- Tests for actionable view -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'magit-dash-actionable)

(ert-deftest magit-dash-actionable-local-entries ()
  (let ((repo (magit-dash-repo--make :name "repo" :path "/tmp")))
    (cl-letf (((symbol-function 'magit-dash--get-stats)
               (lambda (_) '(:ahead 2 :behind 1 :dirty t))))
      (let ((rows (magit-dash-actionable--repo-entries repo)))
        (should (equal (mapcar (lambda (row) (plist-get (car row) :kind)) rows)
                       '(push pull changes)))))))

(ert-deftest magit-dash-actionable-pr-entries ()
  (let ((rows (magit-dash-actionable--pr-entries
               '((:repo "o/r" :number 1 :title "broken" :bucket fix-ci)
                 (:repo "o/r" :number 2 :title "feedback" :bucket threads)
                 (:repo "o/r" :number 3 :title "ready" :bucket ready)))))
    (should (= (length rows) 2))
    (should (equal (aref (cadar rows) 0) "o/r#1"))))

(ert-deftest magit-dash-actionable-keys ()
  (should (eq (lookup-key magit-dash-mode-map (kbd "a p")) 'magit-dash-actionable-open))
  (should (eq (lookup-key magit-dash-actionable-mode-map (kbd "C-c C-c")) 'magit-dash-actionable-menu)))

(ert-deftest magit-dash-actionable-refresh-retains-local-results-on-pr-failure ()
  (with-temp-buffer
    (magit-dash-actionable-mode)
    (let ((magit-dash-repo-list (list (magit-dash-repo--make :name "repo" :path "/tmp"))))
      (cl-letf (((symbol-function 'magit-dash--get-stats) (lambda (_) '(:ahead 1)))
                ((symbol-function 'magit-dash-gh-pr-status-collect-all)
                 (lambda (&rest _) (error "offline"))))
        (magit-dash-actionable-refresh)
        (should (= (length tabulated-list-entries) 1))
        (should (string-match-p "offline" header-line-format))))))

(ert-deftest magit-dash-actionable-mode-derives-from-magit-dash-mode ()
  "magit-dash-actionable-mode inherits full magit-dash-mode functionality."
  (with-temp-buffer
    (magit-dash-actionable-mode)
    (should (derived-mode-p 'magit-dash-mode))
    (should (derived-mode-p 'tabulated-list-mode))))

(ert-deftest magit-dash-actionable-columns-show-why-and-omit-type-sync ()
  "magit-dash-actionable-mode enables 'why' column and disables 'worktree' and 'sync'."
  (with-temp-buffer
    (magit-dash-actionable-mode)
    (should (eq t (cdr (assq 'why magit-dash-columns))))
    (should (null (cdr (assq 'worktree magit-dash-columns))))
    (should (null (cdr (assq 'sync magit-dash-columns))))))

(ert-deftest magit-dash-actionable-filters-clean-repos ()
  "Clean repositories with no PRs are filtered out of the actionable dashboard."
  (with-temp-buffer
    (magit-dash-actionable-mode)
    (let ((clean-repo (magit-dash-repo--make :name "clean" :path "/tmp/clean"))
          (dirty-repo (magit-dash-repo--make :name "dirty" :path "/tmp/dirty")))
      (let ((magit-dash-repo-list (list clean-repo dirty-repo)))
        (cl-letf (((symbol-function 'magit-dash--get-stats)
                   (lambda (r)
                     (if (equal (magit-dash-repo-name r) "dirty")
                         '(:dirty t)
                       '(:ahead 0 :behind 0 :dirty nil))))
                  ((symbol-function 'magit-dash-gh-pr-status-collect-all)
                   (lambda (&rest _) nil)))
          (magit-dash-actionable-refresh)
          (should (= (length tabulated-list-entries) 1))
          (should (equal (magit-dash-repo-name (caar tabulated-list-entries)) "dirty")))))))

(ert-deftest magit-dash-actionable/refresh-does-not-corrupt-other-buffers ()
  "magit-dash-actionable-refresh from an arbitrary buffer does not overwrite it."
  (with-temp-buffer
    (insert "original actionable text")
    (magit-dash-actionable-refresh)
    (should (equal (buffer-string) "original actionable text"))
    (should-not (bound-and-true-p tabulated-list-format))))

(provide 'test-magit-dash-actionable)
;;; test-magit-dash-actionable.el ends here
