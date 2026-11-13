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

(provide 'test-magit-dash-actionable)
;;; test-magit-dash-actionable.el ends here
