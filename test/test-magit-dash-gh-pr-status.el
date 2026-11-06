;;; test-magit-dash-gh-pr-status.el --- ERT tests for magit-dash-gh-pr-status -*- lexical-binding: t; no-byte-compile: t; -*-

;; The fixture is reconstructed from the 2026-11-04 snapshot of open PRs
;; in casaphq/go-monorepo (no PRs were open to capture live on 2026-11-06).
;;
;; Batch run:
;;   emacs --batch -l test/test-helper.el \
;;     -l test/test-magit-dash-gh-pr-status.el \
;;     --eval '(ert-run-tests-batch-and-exit "magit-dash-gh-pr-status/")'

(require 'ert)
(require 'cl-lib)
(require 'magit-dash)
(require 'magit-dash-gh-pr-status)

(defconst magit-dash-gh-pr-status-test/fixture
  (expand-file-name "fixtures/pr-status-2026-11-04.json"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defconst magit-dash-gh-pr-status-test/slug "casaphq/go-monorepo")

(defconst magit-dash-gh-pr-status-test/required 2
  "Approvals required by the shared render helpers.")

(defun magit-dash-gh-pr-status-test/response ()
  "Return the fixture parsed as a GraphQL response."
  (with-temp-buffer
    (insert-file-contents magit-dash-gh-pr-status-test/fixture)
    (json-parse-string (buffer-string) :object-type 'alist
                       :array-type 'list :null-object nil)))

(defun magit-dash-gh-pr-status-test/prs ()
  "Return the PR plists parsed from the fixture."
  (magit-dash-gh-pr-status--parse (magit-dash-gh-pr-status-test/response)
                                  magit-dash-gh-pr-status-test/required))

(defun magit-dash-gh-pr-status-test/pr (number &optional prs)
  (seq-find (lambda (pr) (= number (plist-get pr :number)))
            (or prs (magit-dash-gh-pr-status-test/prs))))

(defun magit-dash-gh-pr-status-test/pr-with (pr &rest props)
  "Return a copy of PR with PROPS set and its `:bucket' recomputed."
  (let ((new (copy-sequence pr)))
    (while props
      (setq new (plist-put new (pop props) (pop props))))
    (plist-put new :bucket
               (magit-dash-gh-pr-status--bucket
                new magit-dash-gh-pr-status-test/required))))

(defun magit-dash-gh-pr-status-test/replace-pr (prs number &rest props)
  "Return PRS with PR NUMBER updated by `--pr-with' using PROPS."
  (mapcar (lambda (pr)
            (if (= number (plist-get pr :number))
                (apply #'magit-dash-gh-pr-status-test/pr-with pr props)
              pr))
          prs))

(defun magit-dash-gh-pr-status-test/without (prs number)
  (seq-remove (lambda (pr) (= number (plist-get pr :number))) prs))

(defun magit-dash-gh-pr-status-test/rendered (existing prs &optional date)
  "Return the status text rendering PRS into EXISTING."
  (magit-dash-gh-pr-status--render
   existing prs magit-dash-gh-pr-status-test/slug
   magit-dash-gh-pr-status-test/required (or date "2026-11-04 Wed")))

(defun magit-dash-gh-pr-status-test/task-p (text keyword number)
  "Return non-nil when TEXT has a task heading with KEYWORD for PR NUMBER."
  (string-match-p (format "^\\*\\* %s %d: " keyword number) text))

(defun magit-dash-gh-pr-status-test/count (regexp text)
  (with-temp-buffer (insert text) (count-matches regexp (point-min) (point-max))))

(defmacro magit-dash-gh-pr-status-test/with-status-file (var &rest body)
  "Run BODY with VAR bound to a temp status file used as the status file."
  (declare (indent 1))
  `(let* ((,var (make-temp-file "pr-status" nil ".org"))
          (magit-dash-gh-pr-status-file ,var))
     (unwind-protect (progn ,@body)
       (when-let* ((buf (find-buffer-visiting ,var)))
         (with-current-buffer buf (set-buffer-modified-p nil))
         (kill-buffer buf))
       (delete-file ,var))))

(defun magit-dash-gh-pr-status-test/user (login state)
  `((state . ,state) (author . ((login . ,login) (__typename . "User")))))

;;; Classification

(ert-deftest magit-dash-gh-pr-status/bucket-fixture ()
  (let ((prs (magit-dash-gh-pr-status-test/prs)))
    (should (= 10 (length prs)))
    (dolist (expected '((9921 . ready) (11005 . threads)
                        (11051 . fix-ci) (11006 . fix-ci) (11192 . fix-ci) (11193 . fix-ci)
                        (11202 . reviews) (11055 . reviews) (11052 . reviews) (11053 . reviews)))
      (let ((pr (magit-dash-gh-pr-status-test/pr (car expected) prs)))
        (should (eq (cdr expected) (plist-get pr :bucket)))
        (should (eq (cdr expected) (magit-dash-gh-pr-status--bucket pr 2)))))))

(ert-deftest magit-dash-gh-pr-status/bucket-required-approvals ()
  (let ((pr (magit-dash-gh-pr-status-test/pr 11005)))
    (setq pr (plist-put (plist-put (plist-put (copy-sequence pr) :threads-open 0) :approvals 1) :merge-state "CLEAN"))
    (should (eq 'ready (magit-dash-gh-pr-status--bucket pr 1)))
    (should (eq 'reviews (magit-dash-gh-pr-status--bucket pr 2)))))

(ert-deftest magit-dash-gh-pr-status/bucket-changes-requested-is-not-ready ()
  (let ((pr (magit-dash-gh-pr-status-test/pr 9921)))
    (should (eq 'ready (magit-dash-gh-pr-status--bucket pr 0)))
    (should (eq 'threads (magit-dash-gh-pr-status--bucket
                          (plist-put (copy-sequence pr) :changes-requested 1) 0)))))

(ert-deftest magit-dash-gh-pr-status/changes-requested-counted-per-latest-review ()
  (let ((reviews (list (magit-dash-gh-pr-status-test/user "a" "CHANGES_REQUESTED")
                       (magit-dash-gh-pr-status-test/user "b" "CHANGES_REQUESTED")
                       (magit-dash-gh-pr-status-test/user "b" "APPROVED")
                       (magit-dash-gh-pr-status-test/user "c" "CHANGES_REQUESTED")
                       (magit-dash-gh-pr-status-test/user "c" "COMMENTED")
                       (magit-dash-gh-pr-status-test/user "claude" "CHANGES_REQUESTED"))))
    (should (= 2 (magit-dash-gh-pr-status--changes-requested reviews)))
    (should (= 1 (magit-dash-gh-pr-status--approvals reviews)))))

(ert-deftest magit-dash-gh-pr-status/changes-requested-task-heading ()
  (let* ((node `((number . 5) (title . "t") (headRefName . "b") (baseRefName . "main")
                 (reviews . ((nodes . (,(magit-dash-gh-pr-status-test/user "a" "CHANGES_REQUESTED")
                                       ,(magit-dash-gh-pr-status-test/user "b" "APPROVED")
                                       ,(magit-dash-gh-pr-status-test/user "c" "APPROVED")))))
                 (reviewThreads . ((nodes . nil)))))
         (prs (magit-dash-gh-pr-status--parse
               `((data . ((search . ((nodes . (,node))))))) 1))
         (pr (car prs))
         (text (magit-dash-gh-pr-status-test/rendered nil prs)))
    (should (= 1 (plist-get pr :changes-requested)))
    (should (eq 'threads (plist-get pr :bucket)))
    (should (string-match-p "^\\*\\* TODO 5: address changes requested by 1 reviewer$" text))
    (should (string-match-p "address threads" text))
    (should (string-match-p "^:STEP: threads" text))))

(ert-deftest magit-dash-gh-pr-status/required-approvals-per-repo ()
  (let ((magit-dash-repo-list
         (list (magit-dash-repo--make :name "m" :path "/tmp/m"
                                      :repo "casaphq/go-monorepo"
                                      :pr '(:required-approvals 3))))
        (magit-dash-gh-pr-status-required-approvals 1))
    (should (= 3 (magit-dash-gh-pr-status--required-approvals "casaphq/go-monorepo")))
    (should (= 1 (magit-dash-gh-pr-status--required-approvals "other/repo")))))

(ert-deftest magit-dash-gh-pr-status/required-approvals-validated ()
  (let ((magit-dash-gh-pr-status-required-approvals 1))
    (dolist (value '(nil "2" -1 1.5 :x))
      (let ((magit-dash-repo-list
             (list (magit-dash-repo--make :name "m" :path "/tmp/m" :repo "o/r"
                                          :pr (list :required-approvals value)))))
        (should (= 1 (magit-dash-gh-pr-status--required-approvals "o/r")))))
    (let ((magit-dash-repo-list
           (list (magit-dash-repo--make :name "m" :path "/tmp/m" :repo "o/r"
                                        :pr '(:required-approvals 0)))))
      (should (= 0 (magit-dash-gh-pr-status--required-approvals "o/r"))))))

(defmacro magit-dash-gh-pr-status-test/capturing-warnings (var &rest body)
  (declare (indent 1))
  `(let ((,var nil))
     (cl-letf (((symbol-function 'display-warning)
                (lambda (&rest args) (push args ,var))))
       ,@body)))

(ert-deftest magit-dash-gh-pr-status/check-options-warns-on-unknown-key ()
  (magit-dash-gh-pr-status-test/capturing-warnings warned
    (magit-dash-gh-pr-check-options "m" '(:bogus 1 :required-approvals 2))
    (should (= 1 (length warned)))))

(ert-deftest magit-dash-gh-pr-status/check-options-warns-on-bad-approvals ()
  (dolist (bad '(nil "2" -1))
    (magit-dash-gh-pr-status-test/capturing-warnings warned
      (magit-dash-gh-pr-check-options "m" (list :required-approvals bad))
      (should (= 1 (length warned)))))
  (magit-dash-gh-pr-status-test/capturing-warnings warned
    (magit-dash-gh-pr-check-options "m" '(:required-approvals 0))
    (should-not warned)))

(ert-deftest magit-dash-gh-pr-status/register-validates-pr-plist ()
  (let ((magit-dash-repo-list nil))
    (magit-dash-gh-pr-status-test/capturing-warnings warned
      (magit-dash-register :name "m" :path "/tmp/m" :repo "o/r" :pr '(:bogus 1))
      (should warned)
      (should (equal '(:bogus 1) (magit-dash-repo-pr (car magit-dash-repo-list)))))))

(ert-deftest magit-dash-gh-pr-status/repo-option-does-not-warn ()
  (let ((magit-dash-repo-list
         (list (magit-dash-repo--make :name "m" :path "/tmp/m" :repo "o/r"
                                      :pr '(:bogus 1)))))
    (magit-dash-gh-pr-status-test/capturing-warnings warned
      (should (eq 'dflt (magit-dash-gh-pr-repo-option "o/r" :required-approvals 'dflt)))
      (should (eq 1 (magit-dash-gh-pr-repo-option "o/r" :bogus 'dflt)))
      (should-not warned))))

(ert-deftest magit-dash-gh-pr-status/approvals-ignore-bots-and-comments ()
  (let ((pr (magit-dash-gh-pr-status-test/pr 11005)))
    ;; brian approved then commented; claude and a Bot approved.
    (should (= 1 (plist-get pr :approvals)))
    (should (equal '("james" "shilpam8") (plist-get pr :requested)))
    (should (= 5 (plist-get pr :threads-open)))
    (should (= 1 (plist-get pr :threads-outdated)))))

(ert-deftest magit-dash-gh-pr-status/approvals-changes-requested-retracts ()
  (should (= 0 (magit-dash-gh-pr-status--approvals
                (list (magit-dash-gh-pr-status-test/user "a" "APPROVED")
                      (magit-dash-gh-pr-status-test/user "a" "CHANGES_REQUESTED"))))))

(ert-deftest magit-dash-gh-pr-status/failing-checks-carry-urls ()
  (let ((pr (magit-dash-gh-pr-status-test/pr 11006)))
    (should (equal '("koalaty") (mapcar #'car (plist-get pr :failing))))
    (should (string-prefix-p "https://" (cdar (plist-get pr :failing))))
    (should (equal "FAILURE" (plist-get pr :ci)))))

(ert-deftest magit-dash-gh-pr-status/review-decision-empty ()
  (should (null (plist-get (magit-dash-gh-pr-status-test/pr 9921) :review-decision)))
  (should (plist-get (magit-dash-gh-pr-status-test/pr 11005) :review-decision)))

;;; Stacks

(ert-deftest magit-dash-gh-pr-status/stack-parents ()
  (let ((prs (magit-dash-gh-pr-status-test/prs)))
    (dolist (expected '((11006 . 11005) (11193 . 11192) (11051 . 11193) (11053 . 11052)
                        (9921) (11005) (11192) (11202) (11055) (11052)))
      (should (eq (cdr expected)
                  (plist-get (magit-dash-gh-pr-status-test/pr (car expected) prs)
                             :stack-parent))))))

(ert-deftest magit-dash-gh-pr-status/stack-parent-duplicate-head-branch ()
  (let* ((a (list :number 1 :branch "shared" :base "main"))
         (b (list :number 2 :branch "shared" :base "main"))
         (child (list :number 3 :branch "c" :base "shared"))
         (prs (list a b child)))
    (should (= 1 (magit-dash-gh-pr-status--stack-parent child prs)))
    (should (null (magit-dash-gh-pr-status--stack-parent a prs)))))

(ert-deftest magit-dash-gh-pr-status/ticket-ids ()
  (should (equal "FND-1269" (magit-dash-gh-pr-status--ticket "tycho/fnd-1269-top")))
  (should (null (magit-dash-gh-pr-status--ticket "tycho/cleanup"))))

;;; Rendering

(ert-deftest magit-dash-gh-pr-status/render-fresh-file ()
  (let ((text (magit-dash-gh-pr-status-test/rendered nil (magit-dash-gh-pr-status-test/prs))))
    (should (string-match-p "^\\* Summary$" text))
    (should (string-match-p "^| 11005 " text))
    (should (string-match-p "needs 2 reviews" text))
    (should (string-match-p "reviewDecision is empty for 9921, 11053, 11193" text))
    (should (= 10 (magit-dash-gh-pr-status-test/count "^:STEP: " text)))
    (should (string-match-p "^:PR: 11192\n:STEP: fix-ci" text))))

(ert-deftest magit-dash-gh-pr-status/render-idempotent ()
  (let* ((prs (magit-dash-gh-pr-status-test/prs))
         (once (magit-dash-gh-pr-status-test/rendered nil prs))
         (twice (magit-dash-gh-pr-status-test/rendered once prs)))
    (should (equal once twice))))

(ert-deftest magit-dash-gh-pr-status/render-preserves-edits ()
  (let* ((prs (magit-dash-gh-pr-status-test/prs))
         (once (magit-dash-gh-pr-status-test/rendered nil prs))
         (edited (replace-regexp-in-string
                  "\\*\\* TODO 9921: merge\\(\\(?:.\\|\n\\)*?:END:\\)"
                  "** DONE 9921: merged by hand\\1\nMy note." once))
         (again (magit-dash-gh-pr-status-test/rendered edited prs)))
    (should (string-match-p "\\*\\* DONE 9921: merged by hand" again))
    (should (string-match-p "My note\\." again))
    (should (equal edited again))))

(ert-deftest magit-dash-gh-pr-status/render-closes-vanished-pr ()
  (let* ((prs (magit-dash-gh-pr-status-test/prs))
         (once (magit-dash-gh-pr-status-test/rendered nil prs))
         (again (magit-dash-gh-pr-status-test/rendered
                 once (magit-dash-gh-pr-status-test/without prs 9921) "2026-11-05 Thu")))
    (should (string-match-p
             "^\\*\\* DONE 9921: merge\nCLOSED: \\[2026-11-05 Thu\\]\n:PROPERTIES:"
             again))
    (should-not (string-match-p "^| 9921 " again))
    (should (magit-dash-gh-pr-status-test/task-p again "TODO" 11005))))

(ert-deftest magit-dash-gh-pr-status/close-task-keywords ()
  (let* ((prs (magit-dash-gh-pr-status-test/prs))
         (once (magit-dash-gh-pr-status-test/rendered nil prs))
         (rest (magit-dash-gh-pr-status-test/without prs 9921)))
    (dolist (keyword magit-dash-gh-pr-status--open-keywords)
      (let* ((edited (replace-regexp-in-string
                      "^\\*\\* TODO 9921:" (format "** %s 9921:" keyword) once))
             (again (magit-dash-gh-pr-status-test/rendered edited rest)))
        (should (magit-dash-gh-pr-status-test/task-p again "DONE" 9921))
        (should (string-match-p "^CLOSED: " again))))))

(ert-deftest magit-dash-gh-pr-status/close-task-leaves-finished-tasks-alone ()
  (let* ((prs (magit-dash-gh-pr-status-test/prs))
         (once (magit-dash-gh-pr-status-test/rendered nil prs))
         (rest (magit-dash-gh-pr-status-test/without prs 9921)))
    (dolist (keyword '("DONE" "CANCELED"))
      (let* ((edited (replace-regexp-in-string
                      "^\\*\\* TODO 9921:" (format "** %s 9921:" keyword) once))
             (again (magit-dash-gh-pr-status-test/rendered edited rest)))
        (should (magit-dash-gh-pr-status-test/task-p again keyword 9921))
        (should-not (string-match-p "CLOSED:" again))))))

(ert-deftest magit-dash-gh-pr-status/close-task-appends-to-planning-line ()
  (let* ((prs (magit-dash-gh-pr-status-test/prs))
         (once (magit-dash-gh-pr-status-test/rendered nil prs))
         (scheduled (replace-regexp-in-string
                     "^\\(\\*\\* TODO 9921: merge\n\\)"
                     "\\1SCHEDULED: <2026-11-10 Tue>\n" once))
         (again (magit-dash-gh-pr-status-test/rendered
                 scheduled (magit-dash-gh-pr-status-test/without prs 9921)
                 "2026-11-05 Thu")))
    (should (string-match-p
             (concat "^\\*\\* DONE 9921: merge\n"
                     "SCHEDULED: <2026-11-10 Tue> CLOSED: \\[2026-11-05 Thu\\]\n"
                     ":PROPERTIES:")
             again))
    (should (= 1 (magit-dash-gh-pr-status-test/count "^CLOSED:\\|CLOSED: \\[" again)))
    ;; The drawer is still attached, so the task is still found by key.
    (should (equal again (magit-dash-gh-pr-status-test/rendered
                          again (magit-dash-gh-pr-status-test/without prs 9921)
                          "2026-11-05 Thu")))))

(ert-deftest magit-dash-gh-pr-status/render-adds-next-step-task ()
  (let* ((prs (magit-dash-gh-pr-status-test/prs))
         (once (magit-dash-gh-pr-status-test/rendered nil prs))
         (fixed (magit-dash-gh-pr-status-test/replace-pr prs 11006 :failing nil))
         (again (magit-dash-gh-pr-status-test/rendered once fixed)))
    ;; The earlier fix-CI task is left alone; the new next step is added.
    (should (= 2 (magit-dash-gh-pr-status-test/count "^:PR: 11006$" again)))
    (should (string-match-p "^\\*\\* TODO 11006: fix koalaty" again))
    (should (string-match-p "^\\*\\* TODO 11006: get 1 more approval" again))
    (should (string-match-p "^:PR: 11006\n:STEP: reviews" again))))

(ert-deftest magit-dash-gh-pr-status/render-archives-handwritten-file ()
  (let* ((legacy "#+title: Old\n#+filetags: :agent:\n\n* Current snapshot\n** Needs fixing\n*** TODO 1: x\n")
         (text (magit-dash-gh-pr-status-test/rendered legacy (magit-dash-gh-pr-status-test/prs))))
    (should (string-match-p "^\\* Archive$" text))
    (should (string-match-p "^\\*\\*\\* Current snapshot$" text))
    (should (string-match-p "^\\*\\*\\*\\*\\* TODO 1: x$" text))
    (should-not (string-match-p "^#\\+title: Old" text))
    ;; Archived TODOs are not generated tasks and are never touched.
    (should (equal text (magit-dash-gh-pr-status-test/rendered
                         text (magit-dash-gh-pr-status-test/prs))))))

(ert-deftest magit-dash-gh-pr-status/archive-with-only-keyword-lines ()
  (dolist (text '("#+title: Old\n#+filetags: :agent:\n" "#+title: Old\n#+filetags: :agent:"))
    (let ((archived (magit-dash-gh-pr-status--archive text "2026-11-04")))
      (should (equal (concat "\n* Archive\n"
                             "** Hand-written file before automation (archived 2026-11-04)\n")
                     archived)))
    (should (magit-dash-gh-pr-status--legacy-p text))
    (should (string-match-p "^\\* Archive$"
                            (magit-dash-gh-pr-status-test/rendered
                             text (magit-dash-gh-pr-status-test/prs))))))

(ert-deftest magit-dash-gh-pr-status/legacy-file-with-tasks-heading-is-kept ()
  (let* ((existing "#+title: Mine\n\n* Tasks\n** TODO my own thing\n")
         (text (magit-dash-gh-pr-status-test/rendered
                existing (magit-dash-gh-pr-status-test/prs))))
    (should-not (magit-dash-gh-pr-status--legacy-p existing))
    (should-not (string-match-p "^\\* Archive" text))
    (should (= 1 (magit-dash-gh-pr-status-test/count "^\\* Tasks" text)))
    (should (string-match-p "^\\*\\* TODO my own thing$" text))
    (should (= 10 (magit-dash-gh-pr-status-test/count "^:STEP: " text)))
    (should (equal text (magit-dash-gh-pr-status-test/rendered
                         text (magit-dash-gh-pr-status-test/prs))))))

(ert-deftest magit-dash-gh-pr-status/render-no-prs ()
  (let ((text (magit-dash-gh-pr-status-test/rendered nil nil)))
    (should (string-match-p "No open PRs\\." text))))

(ert-deftest magit-dash-gh-pr-status/decorated-tasks-heading-is-not-legacy ()
  (let* ((prs (magit-dash-gh-pr-status-test/prs))
         (once (magit-dash-gh-pr-status-test/rendered nil prs))
         (tagged (replace-regexp-in-string "^\\* Tasks$" "* Tasks :agenda:" once)))
    (should (equal tagged (magit-dash-gh-pr-status-test/rendered tagged prs)))))

(ert-deftest magit-dash-gh-pr-status/decorated-summary-heading-is-replaced ()
  (let* ((prs (magit-dash-gh-pr-status-test/prs))
         (once (magit-dash-gh-pr-status-test/rendered nil prs))
         (tagged (replace-regexp-in-string "^\\* Summary$" "* Summary :auto: (daily)" once))
         (again (magit-dash-gh-pr-status-test/rendered tagged prs)))
    (should (= 1 (magit-dash-gh-pr-status-test/count "^\\* Summary" again)))
    (should (string-match-p "^\\* Summary :auto: (daily)$" again))
    (should (equal tagged again))))

(ert-deftest magit-dash-gh-pr-status/full-page-does-not-close-tasks ()
  (let* ((prs (magit-dash-gh-pr-status-test/prs))
         (once (magit-dash-gh-pr-status-test/rendered nil prs))
         (magit-dash-gh-pr-status-max-prs 9)
         (again (magit-dash-gh-pr-status-test/rendered
                 once (magit-dash-gh-pr-status-test/without prs 9921))))
    (should (magit-dash-gh-pr-status-test/task-p again "TODO" 9921))
    (should-not (string-match-p "^CLOSED:" again))))

(ert-deftest magit-dash-gh-pr-status/empty-result-does-not-close-tasks ()
  (let* ((once (magit-dash-gh-pr-status-test/rendered
                nil (magit-dash-gh-pr-status-test/prs)))
         (again (magit-dash-gh-pr-status-test/rendered once nil "2026-11-05 Thu")))
    (should (string-match-p "No open PRs\\." again))
    (should (= 10 (magit-dash-gh-pr-status-test/count "^\\*\\* TODO " again)))
    (should-not (string-match-p "CLOSED:" again))))

(ert-deftest magit-dash-gh-pr-status/other-repo-tasks-are-not-closed ()
  (let* ((prs (magit-dash-gh-pr-status-test/prs))
         (once (magit-dash-gh-pr-status--render nil prs "other/repo" 2 "2026-11-04 Wed"))
         (again (magit-dash-gh-pr-status-test/rendered once (list (car prs)))))
    (should-not (string-match-p "^CLOSED:" again))))

(ert-deftest magit-dash-gh-pr-status/untrusted-text-is-escaped ()
  (let* ((prs (magit-dash-gh-pr-status-test/replace-pr
               (magit-dash-gh-pr-status-test/prs) 9921
               :title "* pwn\n* second" :failing '(("a|b" . "https://x/y"))))
         (text (magit-dash-gh-pr-status-test/rendered nil prs)))
    (should-not (string-match-p "^\\* pwn" text))
    (should-not (string-match-p "^\\* second" text))
    (should (string-match-p "^- \\* pwn \\* second$" text))
    (should (string-match-p "a\\\\vert{}b" text))
    (should-not (string-match-p "| *a|b" text))
    ;; The table keeps its seven columns on the 9921 row.
    (should (string-match-p "^| 9921 [^\n]*a\\\\vert{}b[^\n]* |$" text))
    (should (= 10 (magit-dash-gh-pr-status-test/count "^\\*\\* " text)))))

(ert-deftest magit-dash-gh-pr-status/pending-statuscontext-and-no-rollup ()
  (let* ((node '((number . 1) (title . "t") (headRefName . "b") (baseRefName . "main")
                 (commits . ((nodes . (((commit . ((statusCheckRollup . nil))))))))
                 (reviewThreads . ((nodes . nil)))))
         (pr (magit-dash-gh-pr-status--node-to-pr node)))
    (should (null (plist-get pr :failing)))
    (should (null (plist-get pr :ci))))
  (should (equal '(("lint" . "u"))
                 (magit-dash-gh-pr-status--failing
                  '(((__typename . "StatusContext") (context . "lint")
                     (state . "FAILURE") (targetUrl . "u"))
                    ((__typename . "StatusContext") (context . "slow")
                     (state . "PENDING")))))))

;;; Fetching

(defun magit-dash-gh-pr-status-test/fetch-with (code out &optional slug)
  (cl-letf (((symbol-function 'magit-dash-gh--call-process)
             (lambda (_args _dir) (cons code out))))
    (magit-dash-gh-pr-status--fetch (or slug "o/r") "/tmp")))

(ert-deftest magit-dash-gh-pr-status/fetch-gh-failure ()
  (should-error (magit-dash-gh-pr-status-test/fetch-with 1 "boom") :type 'user-error))

(ert-deftest magit-dash-gh-pr-status/fetch-non-json ()
  (should-error (magit-dash-gh-pr-status-test/fetch-with 0 "not json") :type 'user-error)
  (should-error (magit-dash-gh-pr-status-test/fetch-with 0 "") :type 'user-error))

(ert-deftest magit-dash-gh-pr-status/fetch-graphql-errors ()
  (should-error (magit-dash-gh-pr-status-test/fetch-with
                 0 "{\"errors\":[{\"message\":\"bad\"}]}")
                :type 'user-error))

(ert-deftest magit-dash-gh-pr-status/fetch-success ()
  (should (magit-dash-gh-pr-status-test/fetch-with
           0 "{\"data\":{\"search\":{\"nodes\":[]}}}")))

(ert-deftest magit-dash-gh-pr-status/fetch-empty-object-is-valid-json ()
  (let ((err (should-error (magit-dash-gh-pr-status--parse-json "oops")
                           :type 'user-error)))
    (should (string-match-p "non-JSON" (cadr err))))
  (should-error (magit-dash-gh-pr-status-test/fetch-with 0 "{}") :type 'user-error))

(ert-deftest magit-dash-gh-pr-status/fetch-rejects-bad-slug ()
  (dolist (slug '("" "o" "o/r --repo:evil" "o/r extra" "a/b/c" "/tmp/x"
                  "o/r\nx" "../etc" "o/.." "o/."))
    (should-error (magit-dash-gh-pr-status-test/fetch-with
                   0 "{\"data\":{}}" slug)
                  :type 'user-error)))

(ert-deftest magit-dash-gh-pr-status/slug-accepts-real-names ()
  (dolist (slug '("o/r" "casaphq/go-monorepo" "o/.github" "o/a.b_c-d"))
    (magit-dash-gh-pr-status--check-slug slug)))

(ert-deftest magit-dash-gh-pr-status/run-rejects-bad-slug-before-collecting ()
  (magit-dash-gh-pr-status-test/with-status-file file
    (cl-letf (((symbol-function 'magit-dash-gh-pr-status--collect)
               (lambda (&rest _) (ert-fail "collect called"))))
      (should-error (magit-dash-gh-pr-status-run "/tmp/path" "/tmp") :type 'user-error))))

;;; Files

(ert-deftest magit-dash-gh-pr-status/refuses-unsaved-buffer ()
  (magit-dash-gh-pr-status-test/with-status-file file
    (with-current-buffer (find-file-noselect file)
      (insert "dirty")
      (should-error (magit-dash-gh-pr-status--check-unsaved) :type 'user-error))))

(ert-deftest magit-dash-gh-pr-status/write-rechecks-unsaved-buffer ()
  (magit-dash-gh-pr-status-test/with-status-file file
    (with-current-buffer (find-file-noselect file)
      (insert "dirty")
      (should-error (magit-dash-gh-pr-status--write "new text") :type 'user-error)
      (should (equal "" (with-temp-buffer (insert-file-contents file) (buffer-string)))))))

(ert-deftest magit-dash-gh-pr-status/write-is-atomic-and-leaves-no-temp-files ()
  (magit-dash-gh-pr-status-test/with-status-file file
    (let (renamed)
      (cl-letf* ((orig (symbol-function 'rename-file))
                 ((symbol-function 'rename-file)
                  (lambda (from to ok)
                    (setq renamed (list from to ok))
                    (should (equal (file-name-directory from) (file-name-directory to)))
                    (should (equal "new text"
                                   (with-temp-buffer (insert-file-contents from)
                                                     (buffer-string))))
                    (funcall orig from to ok))))
        (magit-dash-gh-pr-status--write "new text"))
      (should (equal file (cadr renamed)))
      (should (eq t (nth 2 renamed)))
      (should-not (file-exists-p (car renamed)))
      (should (equal "new text" (with-temp-buffer (insert-file-contents file)
                                                  (buffer-string)))))))

(ert-deftest magit-dash-gh-pr-status/run-writes-file-and-reverts-buffer ()
  (magit-dash-gh-pr-status-test/with-status-file file
    (let ((prs (magit-dash-gh-pr-status-test/prs)))
      (cl-letf (((symbol-function 'magit-dash-gh-pr-status--collect)
                 (lambda (_slug _dir _required) prs)))
        (let ((buf (find-file-noselect file)))
          (let ((res (magit-dash-gh-pr-status-run "casaphq/go-monorepo" "/tmp")))
            (should (= 10 (length (plist-get res :prs))))
            (with-current-buffer buf
              (should (string-match-p "^| 11005 " (buffer-string)))
              (should-not (buffer-modified-p)))))))))

(ert-deftest magit-dash-gh-pr-status/registered-checkout-override ()
  (let ((magit-dash-repo-list nil)
        (magit-dash-gh-pr-status-required-approvals 1))
    (magit-dash-register :name "example" :path "/tmp/example" :repo "org/repo"
                         :pr '(:required-approvals 2))
    (should (= 2 (magit-dash-gh-pr-status--required-approvals "org/repo" "/tmp/example")))))

(ert-deftest magit-dash-gh-pr-status/blocked-prs-are-not-ready ()
  (let ((pr '(:approvals 1 :threads-open 0 :changes-requested 0)))
    (dolist (property '((:draft . t) (:ci . "PENDING") (:merge-state . "DIRTY")
                        (:merge-state . "BLOCKED") (:merge-state . "BEHIND")))
      (should (eq 'blocked (magit-dash-gh-pr-status--bucket
                            (plist-put (copy-sequence pr) (car property) (cdr property)) 1))))))

(ert-deftest magit-dash-gh-pr-status/fetch-rejects-incomplete-results ()
  (dolist (json '("{\"data\":{\"search\":{\"nodes\":[],\"pageInfo\":{\"hasNextPage\":true}}}}"
                  "{\"data\":{\"search\":{\"nodes\":[{\"number\":1,\"reviewThreads\":{\"pageInfo\":{\"hasNextPage\":true}}}]}}}"))
    (should-error (magit-dash-gh-pr-status-test/fetch-with 0 json) :type 'user-error)))

(ert-deftest magit-dash-gh-pr-status/write-preserves-mode-and-symlink ()
  (magit-dash-gh-pr-status-test/with-status-file file
    (let ((link (concat file "-link")))
      (unwind-protect
          (progn
            (set-file-modes file #o644)
            (make-symbolic-link file link)
            (let ((magit-dash-gh-pr-status-file link))
              (magit-dash-gh-pr-status--write "updated"))
            (should (file-symlink-p link))
            (should (= #o644 (file-modes file)))
            (should (equal "updated" (with-temp-buffer (insert-file-contents file) (buffer-string)))))
        (when (file-exists-p link) (delete-file link))))))

(provide 'test-magit-dash-gh-pr-status)
;;; test-magit-dash-gh-pr-status.el ends here
