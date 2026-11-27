;;; test-magit-dash-pr-cache.el --- ERT tests for magit-dash-pr-cache -*- lexical-binding: t; no-byte-compile: t; -*-

;; Batch run:
;;   emacs --batch -l test/test-helper.el \
;;     -l test/test-magit-dash-pr-cache.el \
;;     --eval '(ert-run-tests-batch-and-exit "magit-dash-pr-cache/")'

(require 'ert)
(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'magit-dash)
(require 'magit-dash-pr)
(require 'magit-dash-pr-cache)

(defmacro magit-dash-pr-cache-test/with-state (&rest body)
  "Run BODY with the cache rooted in a fresh temp directory and no network."
  (declare (indent 0))
  `(let* ((root (make-temp-file "magit-dash-pr-cache-test-" t))
          (magit-dash-pr-cache-stale-minutes 30))
     (unwind-protect
         (cl-letf (((symbol-function 'sprite-state-path)
                    (lambda (name) (expand-file-name name root)))
                   ((symbol-function 'magit-dash-gh--repo-dir)
                    (lambda () root))
                   ((symbol-function 'magit-dash-pr-dashboard--collect-statuses)
                    #'ignore))
           ,@body)
       (delete-directory root t))))

(defun magit-dash-pr-cache-test/pr (number &optional state)
  "Return a fake PR alist for NUMBER."
  `((number . ,number) (title . ,(format "pr %d" number))
    (state . ,(or state "OPEN")) (updatedAt . "2026-01-01T00:00:00Z")))

(defun magit-dash-pr-cache-test/titles ()
  "Return the PR titles shown in the current dashboard."
  (seq-map (lambda (e) (map-elt (plist-get (car e) :pr) 'title))
           tabulated-list-entries))

(defun magit-dash-pr-cache-test/files ()
  "Return the sorted cache files under the test root."
  (sort (directory-files-recursively (magit-dash-pr-cache--root) "\\.eld\\'") #'string<))

(defun magit-dash-pr-cache-test/age-file (repo number seconds)
  "Rewrite the record for REPO and NUMBER as fetched SECONDS ago."
  (let* ((file (magit-dash-pr-cache--file repo number))
         (record (magit-dash-pr-cache-get repo number)))
    (magit-dash-pr-cache--write
     file (plist-put record :fetched-at (- (float-time) seconds)))))

(ert-deftest magit-dash-pr-cache/clamps-stale-minutes ()
  (let ((magit-dash-pr-cache-stale-minutes 30))
    (customize-set-variable 'magit-dash-pr-cache-stale-minutes 3)
    (should (= 10 magit-dash-pr-cache-stale-minutes))
    (customize-set-variable 'magit-dash-pr-cache-stale-minutes 45)
    (should (= 45 magit-dash-pr-cache-stale-minutes))
    (should (= 2700 (magit-dash-pr-cache-stale-seconds)))))

(ert-deftest magit-dash-pr-cache/put-get-keeps-status ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-put "o/r" 1 (magit-dash-pr-cache-test/pr 1) '(:ci "SUCCESS"))
    (magit-dash-pr-cache-put "o/r" 1 (magit-dash-pr-cache-test/pr 1))
    (let ((rec (magit-dash-pr-cache-get "o/r" 1)))
      (should (equal '(:ci "SUCCESS") (plist-get rec :status)))
      (should (equal "2026-01-01T00:00:00Z" (plist-get rec :updated-at)))
      (should (numberp (plist-get rec :fetched-at))))
    (should (null (magit-dash-pr-cache-get "o/r" 2)))
    (should (= 1 (length (magit-dash-pr-cache-test/files))))))

(ert-deftest magit-dash-pr-cache/unreadable-file-skipped ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-put "o/r" 1 (magit-dash-pr-cache-test/pr 1))
    (with-temp-file (magit-dash-pr-cache--file "o/r" 2) (insert "(((broken"))
    (should (= 1 (length (magit-dash-pr-cache-list "o/r"))))
    (should (null (magit-dash-pr-cache-get "o/r" 2)))))

(ert-deftest magit-dash-pr-cache/stale-p ()
  (let ((now 10000.0))
    (should-not (magit-dash-pr-cache-stale-p (list :fetched-at (- now 60)) now))
    (should (magit-dash-pr-cache-stale-p (list :fetched-at (- now 3600)) now))))

(ert-deftest magit-dash-pr-cache/cold-start-populates-cache ()
  "No cache: the table is empty until discovery, which then writes each PR."
  (magit-dash-pr-cache-test/with-state
    (with-temp-buffer
      (magit-dash-pr-dashboard-mode)
      (setq magit-dash-pr-dashboard--filters (list :state "open" :repo "o/r"))
      (let ((during nil))
        (cl-letf (((symbol-function 'magit-dash-gh--run-process)
                   (lambda (_args _dir on-success &optional _on-error _attempt)
                     (setq during tabulated-list-entries)
                     (funcall on-success
                              (json-serialize
                               (vector (magit-dash-pr-cache-test/pr 1)
                                       (magit-dash-pr-cache-test/pr 2))
                               :null-object nil)))))
          (magit-dash-pr-dashboard-refresh))
        (should (null during))))
    (should (= 2 (length (magit-dash-pr-cache-test/files))))))

(ert-deftest magit-dash-pr-cache/warm-start-shows-cache-before-discovery ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-put "o/r" 1 (magit-dash-pr-cache-test/pr 1))
    (magit-dash-pr-cache-put "o/r" 2 (magit-dash-pr-cache-test/pr 2 "MERGED"))
    (with-temp-buffer
      (magit-dash-pr-dashboard-mode)
      (setq magit-dash-pr-dashboard--filters (list :state "open" :repo "o/r"))
      (cl-letf (((symbol-function 'magit-dash-gh--run-process)
                 (lambda (&rest _) nil)))
        (magit-dash-pr-dashboard-refresh))
      (should (equal '("pr 1") (magit-dash-pr-cache-test/titles))))))

(ert-deftest magit-dash-pr-cache/stale-rows-are-dimmed ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-put "o/r" 1 (magit-dash-pr-cache-test/pr 1))
    (magit-dash-pr-cache-put "o/r" 2 (magit-dash-pr-cache-test/pr 2))
    (magit-dash-pr-cache-test/age-file "o/r" 2 7200)
    (with-temp-buffer
      (magit-dash-pr-dashboard-mode)
      (setq magit-dash-pr-dashboard--filters (list :state "open" :repo "o/r"))
      (let* ((entries (magit-dash-pr-dashboard--cached-entries
                       magit-dash-pr-dashboard--filters))
             (faces (lambda (n)
                      (let* ((e (seq-find (lambda (x) (= n (plist-get (car x) :number)))
                                          entries))
                             (cell (seq-find (lambda (c) (string-match-p "pr " c)) (cadr e))))
                        (get-text-property 0 'face cell)))))
        (should (memq 'magit-dash-stale (ensure-list (funcall faces 2))))
        (should-not (memq 'magit-dash-stale (ensure-list (funcall faces 1))))))))

(ert-deftest magit-dash-pr-cache/failed-discovery-keeps-cached-rows ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-put "o/r" 1 (magit-dash-pr-cache-test/pr 1))
    (seq-do
     (lambda (mode)
      (with-temp-buffer
        (magit-dash-pr-dashboard-mode)
        (setq magit-dash-pr-dashboard--filters (list :state "open" :repo "o/r"))
        (cl-letf (((symbol-function 'magit-dash-gh--run-process)
                   (lambda (_args _dir on-success &optional on-error _attempt)
                     (if (eq mode 'error)
                         (when on-error (funcall on-error "boom" 1))
                       (funcall on-success "not json")))))
          (magit-dash-pr-dashboard-refresh))
        (should (equal '("pr 1") (magit-dash-pr-cache-test/titles)))))
      '(error garbage))
    (should (= 1 (length (magit-dash-pr-cache-test/files))))))

(ert-deftest magit-dash-pr-cache/row-refresh-rewrites-exactly-one-file ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-put "o/r" 1 (magit-dash-pr-cache-test/pr 1))
    (magit-dash-pr-cache-put "o/r" 2 (magit-dash-pr-cache-test/pr 2))
    (magit-dash-pr-cache-test/age-file "o/r" 1 100)
    (magit-dash-pr-cache-test/age-file "o/r" 2 100)
    (let ((before-1 (plist-get (magit-dash-pr-cache-get "o/r" 1) :fetched-at))
          (before-2 (plist-get (magit-dash-pr-cache-get "o/r" 2) :fetched-at)))
      (with-temp-buffer
        (magit-dash-pr-dashboard-mode)
        (setq magit-dash-pr-dashboard--filters (list :state "open" :repo "o/r"))
        (setq tabulated-list-entries (magit-dash-pr-dashboard--cached-entries
                                      magit-dash-pr-dashboard--filters))
        (magit-dash-pr-dashboard--replace-row
         (current-buffer) '("o/r" . 2)
         "{\"number\":2,\"title\":\"two v2\",\"state\":\"OPEN\"}"))
      (should (= before-1 (plist-get (magit-dash-pr-cache-get "o/r" 1) :fetched-at)))
      (should (> (plist-get (magit-dash-pr-cache-get "o/r" 2) :fetched-at) before-2))
      (should (equal "two v2" (map-elt (plist-get (magit-dash-pr-cache-get "o/r" 2) :payload)
                                       'title)))
      (should (= 2 (length (magit-dash-pr-cache-test/files)))))))

(defun magit-dash-pr-cache-test/stale-p-of (entry)
  "Return non-nil when every cell of ENTRY carries `magit-dash-stale'."
  (seq-every-p (lambda (cell)
                 (or (string-empty-p cell)
                     (memq 'magit-dash-stale
                           (ensure-list (get-text-property 0 'face cell)))))
               (cadr entry)))

(ert-deftest magit-dash-pr-cache/rejects-malformed-repo-and-number ()
  (magit-dash-pr-cache-test/with-state
    (seq-do (lambda (repo)
              (should (null (magit-dash-pr-cache--repo-dir repo)))
              (should (null (magit-dash-pr-cache-get repo 1)))
              (should (null (magit-dash-pr-cache-put repo 1 (magit-dash-pr-cache-test/pr 1))))
              (should (null (magit-dash-pr-cache-list repo))))
            '("../x" "x/.." "./x" "x/." ".." "a b/c" "o/r/s" "o" "" nil 5))
    (should (null (magit-dash-pr-cache--file "o/r" "1")))
    (should (null (magit-dash-pr-cache--file "o/r" nil)))
    (should (null (magit-dash-pr-cache-put "o/r" "../1" (magit-dash-pr-cache-test/pr 1))))
    (should-not (file-exists-p (magit-dash-pr-cache--root)))))

(ert-deftest magit-dash-pr-cache/set-stale-minutes-tolerates-non-number ()
  (let ((magit-dash-pr-cache-stale-minutes 30))
    (customize-set-variable 'magit-dash-pr-cache-stale-minutes "abc")
    (should (= 30 magit-dash-pr-cache-stale-minutes))))

(ert-deftest magit-dash-pr-cache/missing-fetched-at-is-stale-and-dimmed ()
  (magit-dash-pr-cache-test/with-state
    (should (magit-dash-pr-cache-stale-p (list :number 1)))
    (magit-dash-pr-cache-put "o/r" 1 (magit-dash-pr-cache-test/pr 1))
    (let ((file (magit-dash-pr-cache--file "o/r" 1)))
      (magit-dash-pr-cache--write
       file (map-delete (magit-dash-pr-cache-get "o/r" 1) :fetched-at)))
    (should (null (plist-get (magit-dash-pr-cache-get "o/r" 1) :fetched-at)))
    (let ((entries (magit-dash-pr-dashboard--cached-entries (list :state "open" :repo "o/r"))))
      (should (= 1 (length entries)))
      (should (magit-dash-pr-cache-test/stale-p-of (car entries))))))

(ert-deftest magit-dash-pr-cache/dimming-survives-mark-toggle ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-put "o/r" 1 (magit-dash-pr-cache-test/pr 1))
    (magit-dash-pr-cache-test/age-file "o/r" 1 7200)
    (with-temp-buffer
      (magit-dash-pr-dashboard-mode)
      (setq magit-dash-pr-dashboard--filters (list :state "open" :repo "o/r"))
      (setq tabulated-list-entries
            (magit-dash-pr-dashboard--cached-entries magit-dash-pr-dashboard--filters))
      (magit-dash-pr-dashboard--print)
      (goto-char (point-min))
      (should (magit-dash-pr-cache-test/stale-p-of (car tabulated-list-entries)))
      (magit-dash-pr-dashboard-toggle-mark)
      (should (magit-dash-pr-cache-test/stale-p-of (car tabulated-list-entries)))
      (magit-dash-pr-dashboard--rebuild-entries)
      (should (magit-dash-pr-cache-test/stale-p-of (car tabulated-list-entries))))))

(ert-deftest magit-dash-pr-cache/discovery-keeps-richer-cached-fields ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-put
     "o/r" 1 (append (magit-dash-pr-cache-test/pr 1) '((body . "rich"))))
    (with-temp-buffer
      (magit-dash-pr-dashboard-mode)
      (setq magit-dash-pr-dashboard--filters (list :state "open" :repo "o/r"))
      (cl-letf (((symbol-function 'magit-dash-gh--run-process)
                 (lambda (_args _dir on-success &rest _)
                   (funcall on-success
                            "[{\"number\":1,\"title\":\"new\",\"state\":\"OPEN\"}]"))))
        (magit-dash-pr-dashboard-refresh)))
    (let ((payload (plist-get (magit-dash-pr-cache-get "o/r" 1) :payload)))
      (should (equal "new" (map-elt payload 'title)))
      (should (equal "rich" (map-elt payload 'body))))))

(ert-deftest magit-dash-pr-cache/empty-array-clears-table ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-put "o/r" 1 (magit-dash-pr-cache-test/pr 1))
    (with-temp-buffer
      (magit-dash-pr-dashboard-mode)
      (setq magit-dash-pr-dashboard--filters (list :state "open" :repo "o/r"))
      (cl-letf (((symbol-function 'magit-dash-gh--run-process)
                 (lambda (_args _dir on-success &rest _)
                   (funcall on-success "[]"))))
        (magit-dash-pr-dashboard-refresh))
      (should (null tabulated-list-entries)))))

(ert-deftest magit-dash-pr-cache/repeated-refresh-does-not-grow-payload ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-put "o/r" 1 '((number . 1) (title . "t") (old . "keep")))
    (let (lengths)
      (dotimes (i 4)
        (magit-dash-pr-dashboard--cache-row
         `((number . 1) (title . ,(format "t%d" i))) "o/r")
        (push (length (plist-get (magit-dash-pr-cache-get "o/r" 1) :payload))
              lengths))
      (should (equal lengths '(3 3 3 3)))
      (let* ((payload (plist-get (magit-dash-pr-cache-get "o/r" 1) :payload))
             (keys (seq-map #'car payload)))
        (should (equal (seq-uniq keys) keys))
        (should (equal (map-elt payload 'title) "t3"))
        (should (equal (map-elt payload 'old) "keep"))))))

(ert-deftest magit-dash-pr-cache/merge-payload-fresh-wins-and-dedups ()
  (let ((merged (magit-dash-pr-cache-merge-payload
                 '((a . 1) (b . 2)) '((b . 9) (c . 3) (c . 4) (a . 8)))))
    (should (equal merged '((a . 1) (b . 2) (c . 3))))))

(ert-deftest magit-dash-pr-cache/stores-head-sha ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-put "o/r" 2 '((number . 2) (headRefOid . "abc123")))
    (should (equal (plist-get (magit-dash-pr-cache-get "o/r" 2) :head-sha)
                   "abc123"))))

;; put-status

(ert-deftest magit-dash-pr-cache/put-status-preserves-record-age ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-put "o/r" 1 '((number . 1) (updatedAt . "u") (headRefOid . "h")))
    (magit-dash-pr-cache-test/age-file "o/r" 1 100000)
    (let ((before (magit-dash-pr-cache-get "o/r" 1)))
      (magit-dash-pr-cache-put-status "o/r" 1 '(:approvals 1) '((number . 99)))
      (let ((after (magit-dash-pr-cache-get "o/r" 1)))
        (should (equal '(:approvals 1) (plist-get after :status)))
        (should (equal (plist-get before :fetched-at) (plist-get after :fetched-at)))
        (should (equal (plist-get before :payload) (plist-get after :payload)))
        (should (equal "h" (plist-get after :head-sha)))
        (should (magit-dash-pr-cache-stale-p after))))))

(ert-deftest magit-dash-pr-cache/put-status-without-prior-writes-payload ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-put-status "o/r" 2 '(:approvals 1) '((number . 2) (title . "t")))
    (let ((record (magit-dash-pr-cache-get "o/r" 2)))
      (should (equal '(:approvals 1) (plist-get record :status)))
      (should (equal "t" (map-elt (plist-get record :payload) 'title)))
      (should-not (magit-dash-pr-cache-stale-p record)))))

;; Hardening: validation

(defun magit-dash-pr-cache-test/write-raw (repo number form)
  "Write the printed FORM as the record file for REPO and NUMBER."
  (let ((file (magit-dash-pr-cache--file repo number)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file (prin1 form (current-buffer)))))

(ert-deftest magit-dash-pr-cache/validates-records ()
  (magit-dash-pr-cache-test/with-state
    (let ((good '(:repo "o/r" :number 1 :payload ((number . 1)) :status (:ci "x")
                  :fetched-at 5.0)))
      (seq-do-indexed
       (lambda (form i)
         (magit-dash-pr-cache-test/write-raw "o/r" (+ 10 i) form))
       (list (plist-put (copy-sequence good) :repo "bad repo")
             (plist-put (copy-sequence good) :number "1")
             (plist-put (copy-sequence good) :payload '(1 2))
             (plist-put (copy-sequence good) :payload 'sym)
             (plist-put (plist-put (copy-sequence good) :status '(:a)) :number 14)
             (plist-put (copy-sequence good) :fetched-at "x")
             (plist-put (copy-sequence good) :version 99)
             (plist-put (copy-sequence good) :url "https://github.com/x/y/pull/2")
             'not-a-plist))
      (magit-dash-pr-cache-test/write-raw "o/r" 1 good)
      (should (= 2 (length (magit-dash-pr-cache-list "o/r"))))
      (should (null (magit-dash-pr-cache-get "o/r" 10)))
      ;; a bad :status only loses the status, not the PR
      (let ((record (magit-dash-pr-cache-get "o/r" 14)))
        (should record)
        (should (null (plist-get record :status))))
      (should (null (magit-dash-pr-cache-get "o/r" 16)))
      (should (null (magit-dash-pr-cache-get "o/r" 17)))
      ;; missing :version is version 1
      (should (magit-dash-pr-cache-get "o/r" 1)))))

(ert-deftest magit-dash-pr-cache/bad-record-does-not-break-refresh ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-put "o/r" 1 (magit-dash-pr-cache-test/pr 1))
    (magit-dash-pr-cache-test/write-raw "o/r" 2 '(:repo "o/r" :number 2 :payload 5))
    ;; valid shape, but a payload that cannot become a row
    (magit-dash-pr-cache-test/write-raw
     "o/r" 3 '(:repo "o/r" :number 3 :payload ((title . "no number"))))
    (with-temp-buffer
      (magit-dash-pr-dashboard-mode)
      (setq magit-dash-pr-dashboard--filters (list :state "open" :repo "o/r"))
      (cl-letf (((symbol-function 'magit-dash-gh--run-process)
                 (lambda (_args _dir on-success &rest _)
                   (funcall on-success "[{\"number\":1,\"title\":\"new\",\"state\":\"OPEN\"}]"))))
        (magit-dash-pr-dashboard-refresh))
      (should (equal '("new") (magit-dash-pr-cache-test/titles))))
    (should (equal '("new")
                   (seq-map (lambda (e) (map-elt (plist-get (car e) :pr) 'title))
                            (magit-dash-pr-dashboard--cached-entries
                             (list :state "open" :repo "o/r")))))))

(ert-deftest magit-dash-pr-cache/cache-row-never-signals ()
  (magit-dash-pr-cache-test/with-state
    (should (null (magit-dash-pr-dashboard--cache-row '((number . 1)) "bad repo")))
    (should (null (magit-dash-pr-dashboard--cache-row 'garbage "o/r")))))

(ert-deftest magit-dash-pr-cache/build-entry-tolerates-nulls ()
  (magit-dash-pr-cache-test/with-state
    (with-temp-buffer
      (magit-dash-pr-dashboard-mode)
      (seq-do
       (lambda (pr)
         (let ((entry (magit-dash-pr-dashboard--build-entry pr "o/r")))
           (should (vectorp (cadr entry)))
           (should (= 7 (plist-get (car entry) :number)))))
       (list '((number . 7) (author . :null) (title . :null) (baseRefName . :null))
             '((number . "7") (author . ((login . :null))) (title . 5))
             '((number . 7) (author . "bob") (statusCheckRollup . :null)
               (reviewDecision . :null) (createdAt . :null)))))))

(ert-deftest magit-dash-pr-cache/null-payload-stored-as-nil ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-put "o/r" 1 '((number . 1) (author . :null) (title . "t")))
    (should-not (memq :null (flatten-tree (plist-get (magit-dash-pr-cache-get "o/r" 1)
                                                     :payload))))))

;; Hardening: freshness

(ert-deftest magit-dash-pr-cache/merged-old-volatile-field-stays-stale ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-put
     "o/r" 1 '((number . 1) (title . "t") (statusCheckRollup . ((x . 1)))))
    (magit-dash-pr-cache-test/age-file "o/r" 1 7200)
    (let ((record (magit-dash-pr-cache-get "o/r" 1)))
      (magit-dash-pr-cache-test/write-raw
       "o/r" 1 (plist-put (copy-sequence record) :field-times
                          (list (cons 'statusCheckRollup (- (float-time) 7200))))))
    ;; sparse refresh without any volatile field
    (magit-dash-pr-dashboard--cache-row '((number . 1) (title . "t2")) "o/r")
    (let ((record (magit-dash-pr-cache-get "o/r" 1)))
      (should (< (- (float-time) (plist-get record :fetched-at)) 60))
      (should (equal "t2" (map-elt (plist-get record :payload) 'title)))
      (should (magit-dash-pr-cache-stale-p record))
      (should (magit-dash-pr-cache-stale-p
               (list :fetched-at (float-time)
                     :field-times (list (cons 'mergeable (- (float-time) 7200)))))))
    ;; a refresh carrying the volatile field makes it fresh again
    (magit-dash-pr-dashboard--cache-row
     '((number . 1) (statusCheckRollup . ((x . 2)))) "o/r")
    (should-not (magit-dash-pr-cache-stale-p (magit-dash-pr-cache-get "o/r" 1)))))

(ert-deftest magit-dash-pr-cache/merged-stale-row-is-dimmed ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-put "o/r" 1 '((number . 1) (title . "t") (state . "OPEN")
                                       (mergeable . "MERGEABLE")))
    (let ((record (magit-dash-pr-cache-get "o/r" 1)))
      (magit-dash-pr-cache-test/write-raw
       "o/r" 1 (plist-put (copy-sequence record) :field-times
                          (list (cons 'mergeable (- (float-time) 7200))))))
    (let ((entry (car (magit-dash-pr-dashboard--cached-entries
                       (list :state "open" :repo "o/r")))))
      (should (magit-dash-pr-cache-test/stale-p-of entry)))))

(ert-deftest magit-dash-pr-cache/head-sha-cleared-by-newer-payload-without-it ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-put "o/r" 1 '((number . 1) (updatedAt . "2026-01-01T00:00:00Z")
                                       (headRefOid . "abc")))
    (magit-dash-pr-cache-put "o/r" 1 '((number . 1) (updatedAt . "2026-01-02T00:00:00Z")))
    (let ((record (magit-dash-pr-cache-get "o/r" 1)))
      (should (null (plist-get record :head-sha)))
      (should (null (map-elt (plist-get record :payload) 'headRefOid))))
    ;; same updatedAt keeps the old sha
    (magit-dash-pr-cache-put "o/r" 2 '((number . 2) (updatedAt . "u") (headRefOid . "abc")))
    (magit-dash-pr-cache-put "o/r" 2 '((number . 2) (updatedAt . "u") (title . "x")))
    (should (equal "abc" (plist-get (magit-dash-pr-cache-get "o/r" 2) :head-sha)))))

(ert-deftest magit-dash-pr-cache/future-fetched-at-is-stale ()
  (let ((now 10000.0))
    (should (magit-dash-pr-cache-stale-p (list :fetched-at (+ now 100000)) now))
    (should-not (magit-dash-pr-cache-stale-p (list :fetched-at (+ now 10)) now))))

;; Hardening: ordering

(ert-deftest magit-dash-pr-cache/older-payload-does-not-roll-back ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-put "o/r" 1 '((number . 1) (title . "new")
                                       (updatedAt . "2026-02-01T00:00:00Z")))
    (let ((before (magit-dash-pr-cache-get "o/r" 1)))
      (magit-dash-pr-cache-put "o/r" 1 '((number . 1) (title . "old")
                                         (updatedAt . "2026-01-01T00:00:00Z"))
                               '(:ci "S"))
      (let ((after (magit-dash-pr-cache-get "o/r" 1)))
        (should (equal "new" (map-elt (plist-get after :payload) 'title)))
        (should (equal "2026-02-01T00:00:00Z" (plist-get after :updated-at)))
        (should (equal '(:ci "S") (plist-get after :status)))
        (should (equal (plist-get before :fetched-at) (plist-get after :fetched-at)))))))

(ert-deftest magit-dash-pr-cache/slow-discovery-does-not-clobber-newer-row-refresh ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-put "o/r" 1 '((number . 1) (title . "t0") (state . "OPEN")
                                       (updatedAt . "2026-01-01T00:00:00Z")))
    (with-temp-buffer
      (magit-dash-pr-dashboard-mode)
      (setq magit-dash-pr-dashboard--filters (list :state "open" :repo "o/r"))
      (cl-letf (((symbol-function 'magit-dash-gh--run-process)
                 (lambda (_args _dir on-success &rest _)
                   ;; the row refresh lands while discovery is in flight
                   (magit-dash-pr-dashboard--cache-row
                    '((number . 1) (title . "t2") (state . "OPEN")
                      (updatedAt . "2026-03-01T00:00:00Z"))
                    "o/r")
                   (funcall on-success
                            "[{\"number\":1,\"title\":\"t1\",\"state\":\"OPEN\",\"updatedAt\":\"2026-02-01T00:00:00Z\"}]"))))
        (magit-dash-pr-dashboard-refresh)))
    (should (equal "t2" (map-elt (plist-get (magit-dash-pr-cache-get "o/r" 1) :payload)
                                 'title)))))

;; Hardening: ghosts

(defun magit-dash-pr-cache-test/discover (filters json)
  "Run a refresh with FILTERS whose discovery returns JSON."
  (with-temp-buffer
    (magit-dash-pr-dashboard-mode)
    (setq magit-dash-pr-dashboard--filters filters)
    (cl-letf (((symbol-function 'magit-dash-gh--run-process)
               (lambda (_args _dir on-success &rest _)
                 (funcall on-success json))))
      (magit-dash-pr-dashboard-refresh))
    (seq-map (lambda (e) (plist-get (car e) :number)) tabulated-list-entries)))

(ert-deftest magit-dash-pr-cache/discovery-marks-missing-prs-gone ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-put "o/r" 1 (magit-dash-pr-cache-test/pr 1))
    (magit-dash-pr-cache-put "o/r" 2 (magit-dash-pr-cache-test/pr 2))
    (magit-dash-pr-cache-put "o/r" 3 (magit-dash-pr-cache-test/pr 3 "MERGED"))
    (magit-dash-pr-cache-put "o/other" 4 (magit-dash-pr-cache-test/pr 4))
    (should (equal '(1) (magit-dash-pr-cache-test/discover
                         (list :state "open" :repo "o/r")
                         "[{\"number\":1,\"state\":\"OPEN\"}]")))
    (should-not (plist-get (magit-dash-pr-cache-get "o/r" 1) :gone))
    (should (plist-get (magit-dash-pr-cache-get "o/r" 2) :gone))
    ;; out of the open scope, and a different repo: untouched
    (should-not (plist-get (magit-dash-pr-cache-get "o/r" 3) :gone))
    (should-not (plist-get (magit-dash-pr-cache-get "o/other" 4) :gone))
    (should (equal '(1) (seq-map (lambda (e) (plist-get (car e) :number))
                                 (magit-dash-pr-dashboard--cached-entries
                                  (list :state "open" :repo "o/r")))))
    ;; a later sighting revives it
    (magit-dash-pr-cache-put "o/r" 2 (magit-dash-pr-cache-test/pr 2))
    (should-not (plist-get (magit-dash-pr-cache-get "o/r" 2) :gone))))

(defun magit-dash-pr-cache-test/discover-hiding-failing (filters json)
  "Like `magit-dash-pr-cache-test/discover' with `hide failing' on."
  (with-temp-buffer
    (magit-dash-pr-dashboard-mode)
    (setq magit-dash-pr-dashboard--filters filters)
    (setq magit-dash-pr-dashboard--hide-failing t)
    (cl-letf (((symbol-function 'magit-dash-gh--run-process)
               (lambda (_args _dir on-success &rest _)
                 (funcall on-success json))))
      (magit-dash-pr-dashboard-refresh))))

(ert-deftest magit-dash-pr-cache/filtered-or-truncated-discovery-evicts-nothing ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-put "o/r" 2 (magit-dash-pr-cache-test/pr 2))
    (magit-dash-pr-cache-test/discover
     (list :state "open" :repo "o/r" :author "bob") "[]")
    (magit-dash-pr-cache-test/discover
     (list :state "open" :repo "o/r" :conflicts t) "[]")
    (magit-dash-pr-cache-test/discover
     (list :state "open" :repo "o/r" :review-requested "bob") "[]")
    (magit-dash-pr-cache-test/discover-hiding-failing
     (list :state "open" :repo "o/r") "[]")
    (magit-dash-pr-cache-test/discover (list :state "open" :org "o") "[]")
    (magit-dash-pr-cache-test/discover (list :state "open") "[]")
    (let ((magit-dash-pr-fetch-limit 1))
      (magit-dash-pr-cache-test/discover
       (list :state "open" :repo "o/r")
       "[{\"number\":1,\"state\":\"OPEN\"}]"))
    (should-not (plist-get (magit-dash-pr-cache-get "o/r" 2) :gone))
    ;; positive control: an unfiltered, complete discovery of the same
    ;; fixture does evict
    (magit-dash-pr-cache-test/discover
     (list :state "open" :repo "o/r") "[{\"number\":1,\"state\":\"OPEN\"}]")
    (should (plist-get (magit-dash-pr-cache-get "o/r" 2) :gone))))

(ert-deftest magit-dash-pr-cache/delete-and-mark-gone ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-put "o/r" 1 (magit-dash-pr-cache-test/pr 1))
    (should (magit-dash-pr-cache-mark-gone "o/r" 1))
    (should (plist-get (magit-dash-pr-cache-get "o/r" 1) :gone))
    (should (null (magit-dash-pr-cache-mark-gone "o/r" 9)))
    (should (magit-dash-pr-cache-delete "o/r" 1))
    (should (null (magit-dash-pr-cache-get "o/r" 1)))
    (should (null (magit-dash-pr-cache-delete "o/r" 1)))))

;; Hardening: identity, files

(ert-deftest magit-dash-pr-cache/repo-case-folds-to-one-file ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-put "Owner/Repo" 1 (magit-dash-pr-cache-test/pr 1))
    (magit-dash-pr-cache-put "owner/repo" 1 (magit-dash-pr-cache-test/pr 1))
    (should (= 1 (length (magit-dash-pr-cache-test/files))))
    (should (equal "Owner/Repo" (plist-get (magit-dash-pr-cache-get "OWNER/REPO" 1) :repo)))
    (should (= 1 (length (magit-dash-pr-cache-list "OWNER/repo"))))
    (should (= 1 (length (magit-dash-pr-cache-list nil "Owner"))))))

(ert-deftest magit-dash-pr-cache/url-number-mismatch-rejected ()
  (magit-dash-pr-cache-test/with-state
    (should (null (magit-dash-pr-cache-put
                   "o/r" 1 '((number . 1) (url . "https://github.com/o/r/pull/2")))))
    (should-not (file-exists-p (magit-dash-pr-cache--root)))
    (should (magit-dash-pr-cache-put
             "o/r" 1 '((number . 1) (url . "https://github.com/O/R/pull/1"))))))

(ert-deftest magit-dash-pr-cache/renamed-repo-url-accepted ()
  (magit-dash-pr-cache-test/with-state
    (let ((magit-dash-pr-cache--renames-logged nil)
          (logged nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (push (apply #'format fmt args) logged))))
        (should (magit-dash-pr-cache-put
                 "old/name" 1 '((number . 1) (url . "https://github.com/new/name/pull/1"))))
        (magit-dash-pr-cache-put
         "old/name" 1 '((number . 1) (title . "x") (url . "https://github.com/new/name/pull/1"))))
      (should (= 1 (length (seq-filter (lambda (m) (string-match-p "renamed" m)) logged))))
      (let ((record (magit-dash-pr-cache-get "old/name" 1)))
        (should record)
        (should (equal "old/name" (plist-get record :repo)))
        (should (equal "https://github.com/new/name/pull/1" (plist-get record :url))))
      (should (= 1 (length (magit-dash-pr-cache-list "old/name")))))))

(ert-deftest magit-dash-pr-cache/utf-8-round-trips ()
  (magit-dash-pr-cache-test/with-state
    (let ((coding-system-for-write 'iso-8859-1)
          (coding-system-for-read 'iso-8859-1))
      (magit-dash-pr-cache-put "o/r" 1 '((number . 1) (title . "café ✓ \U0001F600")))
      (should (equal "café ✓ \U0001F600"
                     (map-elt (plist-get (magit-dash-pr-cache-get "o/r" 1) :payload)
                              'title))))))

(ert-deftest magit-dash-pr-cache/orphan-temp-files-swept-on-list ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-put "o/r" 1 (magit-dash-pr-cache-test/pr 1))
    (let* ((dir (magit-dash-pr-cache--repo-dir "o/r"))
           (old (expand-file-name ".pr-old.tmp" dir))
           (new (expand-file-name ".pr-new.tmp" dir)))
      (write-region "x" nil old)
      (write-region "x" nil new)
      (set-file-times old (time-subtract (current-time) (* 3 86400)))
      (magit-dash-pr-cache-list)
      (should-not (file-exists-p old))
      (should (file-exists-p new)))))

(defun magit-dash-pr-cache-test/file-text (repo number)
  "Return the on-disk text of the record for REPO and NUMBER."
  (with-temp-buffer
    (insert-file-contents (magit-dash-pr-cache--file repo number))
    (buffer-string)))

(ert-deftest magit-dash-pr-cache/unchanged-put-skips-rewrite ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-put "o/r" 1 (magit-dash-pr-cache-test/pr 1))
    (let ((before (magit-dash-pr-cache-test/file-text "o/r" 1)))
      (magit-dash-pr-cache-put "o/r" 1 (magit-dash-pr-cache-test/pr 1))
      (should (equal before (magit-dash-pr-cache-test/file-text "o/r" 1)))
      (magit-dash-pr-cache-put "o/r" 1 (magit-dash-pr-cache-test/pr 1) '(:ci "x"))
      (should-not (equal before (magit-dash-pr-cache-test/file-text "o/r" 1)))
      (should (equal '(:ci "x") (plist-get (magit-dash-pr-cache-get "o/r" 1) :status))))
    ;; an old record is rewritten so its age refreshes
    (magit-dash-pr-cache-test/age-file "o/r" 1 7200)
    (let ((before (magit-dash-pr-cache-test/file-text "o/r" 1)))
      (magit-dash-pr-cache-put "o/r" 1 (magit-dash-pr-cache-test/pr 1) '(:ci "x"))
      (should-not (equal before (magit-dash-pr-cache-test/file-text "o/r" 1))))
    (should-not (magit-dash-pr-cache-stale-p (magit-dash-pr-cache-get "o/r" 1)))
    (magit-dash-pr-cache-test/age-file "o/r" 1 7200)
    (let ((before (magit-dash-pr-cache-test/file-text "o/r" 1)))
      (magit-dash-pr-cache-put "o/r" 1 (magit-dash-pr-cache-test/pr 1))
      (should-not (equal before (magit-dash-pr-cache-test/file-text "o/r" 1))))))

(ert-deftest magit-dash-pr-cache/same-payload-field-time-change-written-when-old ()
  (magit-dash-pr-cache-test/with-state
    (let ((payload '((number . 1) (title . "t") (mergeable . "MERGEABLE"))))
      (magit-dash-pr-cache-put "o/r" 1 payload)
      ;; younger than a third of the threshold: skipped, field time stays
      (let ((before (magit-dash-pr-cache-test/file-text "o/r" 1)))
        (magit-dash-pr-cache-put "o/r" 1 payload)
        (should (equal before (magit-dash-pr-cache-test/file-text "o/r" 1))))
      ;; 15 minutes old (> 10): same payload, newer field time, rewritten
      (let* ((old (- (float-time) 900))
             (record (magit-dash-pr-cache-get "o/r" 1)))
        (magit-dash-pr-cache--write
         (magit-dash-pr-cache--file "o/r" 1)
         (plist-put (plist-put (copy-sequence record) :fetched-at old)
                    :field-times (list (cons 'mergeable old))))
        (let ((before (magit-dash-pr-cache-test/file-text "o/r" 1)))
          (magit-dash-pr-cache-put "o/r" 1 payload)
          (should-not (equal before (magit-dash-pr-cache-test/file-text "o/r" 1)))
          (should (< (- (float-time)
                        (alist-get 'mergeable
                                   (plist-get (magit-dash-pr-cache-get "o/r" 1)
                                              :field-times)))
                     60)))))))

(ert-deftest magit-dash-pr-cache/list-reads-only-requested-scope ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-put "a/r" 1 (magit-dash-pr-cache-test/pr 1))
    (magit-dash-pr-cache-put "b/r" 2 (magit-dash-pr-cache-test/pr 2))
    (let (read)
      (cl-letf (((symbol-function 'magit-dash-pr-cache--read-file)
                 (lambda (file) (push file read) nil)))
        (magit-dash-pr-cache-list "a/r")
        (magit-dash-pr-cache-list nil "b"))
      (should (= 2 (length read)))
      (should (= 1 (length (seq-filter (lambda (f) (string-match-p "/a/r/" f)) read)))))))


;; Second-review hardening

(ert-deftest magit-dash-pr-cache/null-to-nil-handles-long-lists ()
  (let ((long (make-list 100000 :null)))
    (should (equal (make-list 100000 nil) (magit-dash-pr-cache--null-to-nil long))))
  (should (equal '((a) (b . 1) (c nil))
                 (magit-dash-pr-cache--null-to-nil '((a . :null) (b . 1) (c :null))))))

(ert-deftest magit-dash-pr-cache/huge-rollup-parses-and-bad-row-is-skipped ()
  (magit-dash-pr-cache-test/with-state
    (let* ((rollup (apply #'vector
                          (seq-map (lambda (i)
                                     `((name . ,(format "c%d" i)) (status . "COMPLETED")
                                       (conclusion . "SUCCESS")))
                                   (number-sequence 1 2000))))
           (json (json-serialize
                  (vector `((number . 1) (title . "big") (state . "OPEN")
                            (statusCheckRollup . ,rollup))
                          `((number . :null) (title . "bad"))
                          `((number . 3) (title . "ok") (state . "OPEN")))))
           (logged nil))
      (with-temp-buffer
        (magit-dash-pr-dashboard-mode)
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) logged))))
          (let ((entries (magit-dash-pr-dashboard--parse-output
                          json (list :state "open" :repo "o/r"))))
            (should (equal '(1 3) (seq-map (lambda (e) (plist-get (car e) :number))
                                           entries)))
            (should (= 2000 (length (map-elt (plist-get (car (car entries)) :pr)
                                             'statusCheckRollup))))))
        (should (seq-some (lambda (m) (string-match-p "skipping unusable" m)) logged))))))

(ert-deftest magit-dash-pr-cache/warm-start-keeps-original-case-and-live-swap-keeps-state ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-put "Foo/Bar" 1 (magit-dash-pr-cache-test/pr 1)
                             '(:ci "SUCCESS" :approvals 1 :required 2))
    (should (equal "Foo/Bar" (plist-get (car (magit-dash-pr-cache-list "foo/bar")) :repo)))
    (with-temp-buffer
      (magit-dash-pr-dashboard-mode)
      (setq magit-dash-pr-dashboard--filters (list :state "open" :repo "foo/bar"))
      (cl-letf (((symbol-function 'magit-dash-gh--run-process)
                 (lambda (&rest _) nil)))
        (magit-dash-pr-dashboard-refresh))
      (should (equal "Foo/Bar" (plist-get (car (car tabulated-list-entries)) :repo)))
      (magit-dash-pr-dashboard-mark-all)
      ;; live discovery names the repo in another case
      (cl-letf (((symbol-function 'magit-dash-gh--run-process)
                 (lambda (_args _dir on-success &rest _)
                   (funcall on-success "[{\"number\":1,\"state\":\"OPEN\",\"title\":\"pr 1\"}]"))))
        (magit-dash-pr-dashboard-refresh))
      (let ((id (car (car tabulated-list-entries))))
        (should (equal "foo/bar" (plist-get id :repo)))
        (should (magit-dash-pr-dashboard--entry-marked-p id))
        (should (equal "SUCCESS" (plist-get (magit-dash-pr-dashboard--status "foo/bar" 1) :ci)))
        (should (equal "SUCCESS" (plist-get (magit-dash-pr-dashboard--status "Foo/Bar" 1) :ci)))))))

(defun magit-dash-pr-cache-test/write-legacy (dirname number form)
  "Write FORM as NUMBER.eld in the raw cache directory DIRNAME (any case)."
  (let ((file (expand-file-name (format "%s.eld" number)
                                (expand-file-name dirname (magit-dash-pr-cache--root)))))
    (make-directory (file-name-directory file) t)
    (with-temp-file file (prin1 form (current-buffer)))
    file))

(defun magit-dash-pr-cache-test/legacy-record (number title fetched)
  "Return a legacy-style record for Owner/Repo."
  (list :repo "Owner/Repo" :number number :payload `((number . ,number) (title . ,title))
        :fetched-at fetched))

(ert-deftest magit-dash-pr-cache/legacy-mixed-case-dir-is-migrated ()
  (magit-dash-pr-cache-test/with-state
    (let ((legacy (magit-dash-pr-cache-test/write-legacy
                   "Owner/Repo" 1 (magit-dash-pr-cache-test/legacy-record 1 "legacy" 100.0))))
      (should (= 1 (length (magit-dash-pr-cache-list "owner/repo"))))
      (should-not (file-exists-p legacy))
      (should-not (file-directory-p (expand-file-name "Owner" (magit-dash-pr-cache--root))))
      (should (equal '("owner/repo/1.eld")
                     (seq-map (lambda (f) (file-relative-name f (magit-dash-pr-cache--root)))
                              (magit-dash-pr-cache-test/files))))
      (should (equal "legacy" (map-elt (plist-get (magit-dash-pr-cache-get "OWNER/REPO" 1)
                                                  :payload)
                                       'title))))))

(ert-deftest magit-dash-pr-cache/legacy-dir-merges-newest-without-duplicates ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-put "owner/repo" 1 '((number . 1) (title . "lower")))
    (magit-dash-pr-cache-put "owner/repo" 2 '((number . 2) (title . "lower2")))
    ;; legacy 1 is older, legacy 2 is newer, legacy 3 is new
    (magit-dash-pr-cache-test/write-legacy
     "Owner/Repo" 1 (magit-dash-pr-cache-test/legacy-record 1 "legacy1" 100.0))
    (magit-dash-pr-cache-test/write-legacy
     "Owner/Repo" 2 (magit-dash-pr-cache-test/legacy-record 2 "legacy2" (+ (float-time) 5)))
    (magit-dash-pr-cache-test/write-legacy
     "Owner/Repo" 3 (magit-dash-pr-cache-test/legacy-record 3 "legacy3" 100.0))
    (let ((records (magit-dash-pr-cache-list nil "owner")))
      (should (= 3 (length records)))
      (should (equal '("lower" "legacy2" "legacy3")
                     (seq-map (lambda (n)
                                (map-elt (plist-get (magit-dash-pr-cache-get "owner/repo" n)
                                                    :payload)
                                         'title))
                              '(1 2 3)))))
    (should (= 3 (length (magit-dash-pr-cache-test/files))))
    (should-not (file-directory-p (expand-file-name "Owner" (magit-dash-pr-cache--root))))))

(ert-deftest magit-dash-pr-cache/list-dedupes-case-variants ()
  (let ((a (list :repo "Owner/Repo" :number 1 :fetched-at 1.0))
        (b (list :repo "owner/repo" :number 1 :fetched-at 2.0))
        (c (list :repo "owner/repo" :number 2 :fetched-at 1.0)))
    (should (equal (list b c) (magit-dash-pr-cache--dedupe (list a b c))))))

(ert-deftest magit-dash-pr-cache/search-refresh-does-not-dim-forever ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-put
     "o/r" 1 '((number . 1) (title . "t") (reviewDecision . "APPROVED")
               (statusCheckRollup . ((x . 1)))))
    (let ((old (- (float-time) 7200)))
      (magit-dash-pr-cache--write
       (magit-dash-pr-cache--file "o/r" 1)
       (plist-put (plist-put (copy-sequence (magit-dash-pr-cache-get "o/r" 1))
                             :fetched-at old)
                  :field-times (list (cons 'reviewDecision old)
                                     (cons 'statusCheckRollup old)))))
    (should (magit-dash-pr-cache-stale-p (magit-dash-pr-cache-get "o/r" 1)))
    ;; cross-repo search refresh: sparse payload, no volatile fields
    (magit-dash-pr-dashboard--cache-row '((number . 1) (title . "t2")) "o/r" t)
    (let ((record (magit-dash-pr-cache-get "o/r" 1)))
      (should-not (magit-dash-pr-cache-stale-p record))
      (should (equal "APPROVED" (map-elt (plist-get record :payload) 'reviewDecision))))))

(ert-deftest magit-dash-pr-cache/gone-shown-in-non-open-views ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-put "o/r" 1 (magit-dash-pr-cache-test/pr 1))
    (magit-dash-pr-cache-put "o/r" 2 (magit-dash-pr-cache-test/pr 2 "MERGED"))
    (magit-dash-pr-cache-mark-gone "o/r" 1)
    (magit-dash-pr-cache-mark-gone "o/r" 2)
    (let ((numbers (lambda (state)
                     (seq-map (lambda (e) (plist-get (car e) :number))
                              (magit-dash-pr-dashboard--cached-entries
                               (list :state state :repo "o/r"))))))
      (should (null (funcall numbers "open")))
      (should (equal '(2) (funcall numbers "merged"))))))

(ert-deftest magit-dash-pr-cache/older-payload-revives-gone-record ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-put "o/r" 2 '((number . 2) (title . "new") (state . "OPEN")
                                       (updatedAt . "2026-02-01T00:00:00Z")))
    (magit-dash-pr-cache-mark-gone "o/r" 2)
    (magit-dash-pr-cache-put "o/r" 2 '((number . 2) (title . "old") (state . "OPEN")
                                       (updatedAt . "2026-01-01T00:00:00Z")))
    (let ((record (magit-dash-pr-cache-get "o/r" 2)))
      (should-not (plist-get record :gone))
      (should (equal "new" (map-elt (plist-get record :payload) 'title))))
    ;; and through a live discovery that returns the older payload
    (magit-dash-pr-cache-mark-gone "o/r" 2)
    (should (equal '(2) (magit-dash-pr-cache-test/discover
                         (list :state "open" :repo "o/r")
                         "[{\"number\":2,\"state\":\"OPEN\",\"updatedAt\":\"2025-01-01T00:00:00Z\"}]")))
    (should-not (plist-get (magit-dash-pr-cache-get "o/r" 2) :gone))))

(ert-deftest magit-dash-pr-cache/future-fetched-at-not-hidden-by-old-field-time ()
  (let* ((now 100000.0)
         (record (list :fetched-at (+ now 10000)
                       :field-times (list (cons 'mergeable (- now 60))))))
    (should (< (magit-dash-pr-cache-effective-fetched-at record) now))
    (should (magit-dash-pr-cache-future-p record now))
    (should (magit-dash-pr-cache-stale-p record now))
    (should-not (magit-dash-pr-cache-stale-p
                 (list :fetched-at (+ now 10) :field-times (list (cons 'mergeable (- now 60))))
                 now)))
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-put "o/r" 1 '((number . 1) (state . "OPEN") (mergeable . "X")))
    (magit-dash-pr-cache-test/write-raw
     "o/r" 1 (plist-put (plist-put (copy-sequence (magit-dash-pr-cache-get "o/r" 1))
                                   :fetched-at (+ (float-time) 100000))
                        :field-times (list (cons 'mergeable (- (float-time) 60)))))
    (should (magit-dash-pr-cache-test/stale-p-of
             (car (magit-dash-pr-dashboard--cached-entries (list :state "open" :repo "o/r")))))))

;;; Final-review regressions

(defun magit-dash-pr-cache-test/seed-three ()
  "Cache open PRs 1 through 3 of o/r."
  (seq-do (lambda (n)
            (magit-dash-pr-cache-put "o/r" n (magit-dash-pr-cache-test/pr n)))
          '(1 2 3)))

(ert-deftest magit-dash-pr-cache/unbuildable-row-suppresses-eviction ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-test/seed-three)
    (magit-dash-pr-cache-test/discover
     (list :state "open" :repo "o/r")
     "[{\"number\":1,\"state\":\"OPEN\"},{\"number\":2,\"state\":\"OPEN\",\"comments\":{\"totalCount\":\"x\"}}]")
    (seq-do (lambda (n)
              (should-not (plist-get (magit-dash-pr-cache-get "o/r" n) :gone)))
            '(1 2 3))))

(ert-deftest magit-dash-pr-cache/fractional-number-suppresses-eviction ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-test/seed-three)
    (magit-dash-pr-cache-test/discover
     (list :state "open" :repo "o/r")
     "[{\"number\":1,\"state\":\"OPEN\"},{\"number\":1.5,\"state\":\"OPEN\"}]")
    (seq-do (lambda (n)
              (should-not (plist-get (magit-dash-pr-cache-get "o/r" n) :gone)))
            '(1 2 3))))

(ert-deftest magit-dash-pr-cache/all-rows-skipped-does-not-evict-but-empty-does ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-test/seed-three)
    (magit-dash-pr-cache-test/discover
     (list :state "open" :repo "o/r")
     "[{\"number\":1,\"comments\":{\"totalCount\":\"x\"}}]")
    (seq-do (lambda (n)
              (should-not (plist-get (magit-dash-pr-cache-get "o/r" n) :gone)))
            '(1 2 3))
    (magit-dash-pr-cache-test/discover (list :state "open" :repo "o/r") "[]")
    (seq-do (lambda (n)
              (should (plist-get (magit-dash-pr-cache-get "o/r" n) :gone)))
            '(1 2 3))))

(ert-deftest magit-dash-pr-cache/malformed-legacy-record-moved-aside-once ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-put "owner/repo" 1 '((number . 1) (title . "lower")))
    (magit-dash-pr-cache-test/write-legacy "Owner/Repo" 2 '(:repo "bad repo" :number 2))
    (magit-dash-pr-cache-test/write-legacy
     "Owner/Repo" 1 (magit-dash-pr-cache-test/legacy-record 1 "legacy" (+ (float-time) 5)))
    (magit-dash-pr-cache-reset-migrated)
    (magit-dash-pr-cache-get "owner/repo" 1)
    (should-not (file-directory-p (expand-file-name "Owner" (magit-dash-pr-cache--root))))
    (should (file-exists-p (expand-file-name "owner/repo/2.eld.bad"
                                             (magit-dash-pr-cache--root))))
    (magit-dash-pr-cache-reset-migrated)
    (let ((logged nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (push (apply #'format fmt args) logged))))
        (magit-dash-pr-cache-get "owner/repo" 1)
        (magit-dash-pr-cache-list "owner/repo"))
      (should-not logged))))

(ert-deftest magit-dash-pr-cache/legacy-merge-prefers-updated-at-over-fetched-at ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-put "owner/repo" 1 '((number . 1) (title . "lower")
                                              (updatedAt . "2026-03-01T00:00:00Z")))
    (magit-dash-pr-cache-test/write-legacy
     "Owner/Repo" 1
     (plist-put (magit-dash-pr-cache-test/legacy-record 1 "legacy" (+ (float-time) 500))
                :updated-at "2026-01-01T00:00:00Z"))
    (magit-dash-pr-cache-reset-migrated)
    (should (equal "lower" (map-elt (plist-get (magit-dash-pr-cache-get "owner/repo" 1) :payload)
                                    'title)))
    (should (eq t (magit-dash-pr-cache--better-p '(:updated-at "b") '(:updated-at "a"))))
    (should-not (magit-dash-pr-cache--better-p '(:updated-at "a" :fetched-at 9) '(:updated-at "b")))))

(ert-deftest magit-dash-pr-cache/unreadable-canonical-record-logged-distinctly ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-test/write-raw "owner/repo" 1 '(:repo "bad repo"))
    (magit-dash-pr-cache-test/write-legacy
     "Owner/Repo" 1 (magit-dash-pr-cache-test/legacy-record 1 "legacy" 100.0))
    (magit-dash-pr-cache-reset-migrated)
    (let ((logged nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (push (apply #'format fmt args) logged))))
        (magit-dash-pr-cache-get "owner/repo" 1))
      (should (seq-some (lambda (m) (string-match-p "canonical record .* unreadable" m))
                        logged)))
    (should (equal "legacy" (map-elt (plist-get (magit-dash-pr-cache-get "owner/repo" 1) :payload)
                                     'title)))))

(ert-deftest magit-dash-pr-cache/migration-scan-runs-once-per-repo ()
  (magit-dash-pr-cache-test/with-state
    (seq-do (lambda (n)
              (make-directory (expand-file-name (format "owner%d/repo" n)
                                                (magit-dash-pr-cache--root))
                              t))
            (number-sequence 1 2000))
    (let ((scans 0)
          (orig (symbol-function 'magit-dash-pr-cache--legacy-dirs))
          (start (float-time)))
      (cl-letf (((symbol-function 'magit-dash-pr-cache--legacy-dirs)
                 (lambda (repo)
                   (setq scans (1+ scans))
                   (funcall orig repo))))
        (seq-do (lambda (n)
                  (magit-dash-pr-cache-put "o/r" n (magit-dash-pr-cache-test/pr n)))
                (number-sequence 1 100))
        (magit-dash-pr-cache-get "O/R" 1)
        (magit-dash-pr-cache-mark-gone "o/r" 2)
        (magit-dash-pr-cache-delete "o/r" 3))
      (should (= 1 scans))
      (should (< (- (float-time) start) 30)))))

(ert-deftest magit-dash-pr-cache/replace-row-nil-state-is-open ()
  (magit-dash-pr-cache-test/with-state
    (let ((key (cons "o/r" 1)))
      (with-temp-buffer
        (magit-dash-pr-dashboard-mode)
        (setq magit-dash-pr-dashboard--filters (list :repo "o/r"))
        (setq tabulated-list-entries
              (list (magit-dash-pr-dashboard--build-entry
                     (magit-dash-pr-cache-test/pr 1) "o/r")))
        ;; no :state in the filters, no state in the output: still open
        (magit-dash-pr-dashboard--replace-row
         (current-buffer) key "{\"number\":1,\"title\":\"new\"}")
        (should (equal '("new") (magit-dash-pr-cache-test/titles)))
        (magit-dash-pr-dashboard--replace-row
         (current-buffer) key "{\"number\":1,\"state\":null,\"title\":\"newer\"}")
        (should (equal '("newer") (magit-dash-pr-cache-test/titles)))
        ;; a closed PR drops under the implicit open filter
        (magit-dash-pr-dashboard--replace-row
         (current-buffer) key "{\"number\":1,\"state\":\"CLOSED\"}")
        (should (null tabulated-list-entries))))))

(ert-deftest magit-dash-pr-cache/replace-row-malformed-json-keeps-row ()
  (magit-dash-pr-cache-test/with-state
    (with-temp-buffer
      (magit-dash-pr-dashboard-mode)
      (setq magit-dash-pr-dashboard--filters (list :state "open" :repo "o/r"))
      (setq tabulated-list-entries
            (list (magit-dash-pr-dashboard--build-entry
                   (magit-dash-pr-cache-test/pr 1) "o/r")))
      (let ((logged nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) logged))))
          (magit-dash-pr-dashboard--replace-row
           (current-buffer) (cons "o/r" 1) "{\"number\":1,"))
        (should (seq-some (lambda (m) (string-match-p "malformed output" m)) logged)))
      (should (equal '("pr 1") (magit-dash-pr-cache-test/titles))))))

(ert-deftest magit-dash-pr-cache/store-status-without-record-keeps-row-case ()
  (magit-dash-pr-cache-test/with-state
    (with-temp-buffer
      (magit-dash-pr-dashboard-mode)
      (setq tabulated-list-entries
            (list (magit-dash-pr-dashboard--build-entry
                   (magit-dash-pr-cache-test/pr 1) "Foo/Bar")))
      (should (magit-dash-pr-dashboard--store-status
               "foo/bar" 1 '(:ci "SUCCESS" :approvals 1 :required 2)
               (magit-dash-pr-cache-test/pr 1)))
      (should (equal "Foo/Bar" (plist-get (magit-dash-pr-cache-get "foo/bar" 1) :repo))))))

(ert-deftest magit-dash-pr-cache/non-finite-fetched-at-is-rejected ()
  (seq-do (lambda (bad)
            (should (magit-dash-pr-cache--problem
                     (list :repo "o/r" :number 1 :fetched-at bad)))
            (should-not (magit-dash-pr-cache-effective-fetched-at
                         (list :fetched-at bad)))
            (should (magit-dash-pr-cache-stale-p (list :fetched-at bad)))
            (should (magit-dash-pr-cache--problem
                     (list :repo "o/r" :number 1 :fetched-at 1.0
                           :field-times (list (cons 'mergeable bad))))))
          (list 0.0e+NaN 1.0e+INF -1.0e+INF))
  (should (= 5.0 (magit-dash-pr-cache-effective-fetched-at
                  (list :fetched-at 10.0 :field-times (list (cons 'a 5.0) (cons 'b 0.0e+NaN)))))))

;;; Follow-up cache limits

(ert-deftest magit-dash-pr-cache/prunable-records-filters-gone-closed-merged-and-days ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-put "o/r" 1 (magit-dash-pr-cache-test/pr 1 "OPEN"))
    (magit-dash-pr-cache-put "o/r" 2 (magit-dash-pr-cache-test/pr 2 "OPEN"))
    (magit-dash-pr-cache-mark-gone "o/r" 2)
    (magit-dash-pr-cache-put "o/r" 3 (magit-dash-pr-cache-test/pr 3 "MERGED"))
    (magit-dash-pr-cache-put "o/other" 4 (magit-dash-pr-cache-test/pr 4 "closed"))
    (magit-dash-pr-cache-test/age-file "o/r" 3 (* 10 86400))
    (let ((all-nums (sort (seq-map (lambda (r) (plist-get r :number))
                                   (magit-dash-pr-cache-prunable-records))
                          #'<))
          (repo-nums (seq-map (lambda (r) (plist-get r :number))
                              (magit-dash-pr-cache-prunable-records "O/R")))
          (old-nums (seq-map (lambda (r) (plist-get r :number))
                             (magit-dash-pr-cache-prunable-records nil 5))))
      (should (equal '(2 3 4) all-nums))
      (should (equal '(2 3) (sort repo-nums #'<)))
      (should (equal '(3) old-nums)))))

(ert-deftest magit-dash-pr-cache/prune-uses-hitl-plan-when-available ()
  (require 'hitl-plan)
  (require 'hitl-plan-ui)
  (magit-dash-pr-cache-test/with-state
    (let* ((hitl-plan-directory (expand-file-name "plans/" root))
           (hitl-plan--store (make-hash-table :test #'equal))
           (reviewed nil))
      (make-directory hitl-plan-directory t)
      (magit-dash-pr-cache-put "o/r" 1 (magit-dash-pr-cache-test/pr 1 "OPEN"))
      (magit-dash-pr-cache-put "o/r" 2 (magit-dash-pr-cache-test/pr 2 "OPEN"))
      (magit-dash-pr-cache-mark-gone "o/r" 2)
      (magit-dash-pr-cache-put "o/r" 3 (magit-dash-pr-cache-test/pr 3 "MERGED"))
      (magit-dash-pr-cache-put "o/r" 4 (magit-dash-pr-cache-test/pr 4 "CLOSED"))
      (cl-letf (((symbol-function 'hitl-plan-review)
                 (lambda (plan &optional _display) (setq reviewed plan) nil)))
        (let ((plan (magit-dash-pr-cache-prune)))
          (should (hitl-plan-p plan))
          (should (eq plan reviewed))
          (should (eq 'magit-dash-pr-cache (hitl-plan-domain plan)))
          (should (= 3 (length (hitl-plan-steps plan))))
          (let* ((dom (hitl-plan-get-domain 'magit-dash-pr-cache))
                 (fmt (hitl-plan-domain-format-target dom))
                 (step3 (seq-find (lambda (s) (= 3 (plist-get (hitl-plan-step-target s) :number)))
                                  (hitl-plan-steps plan)))
                 (step4 (seq-find (lambda (s) (= 4 (plist-get (hitl-plan-step-target s) :number)))
                                  (hitl-plan-steps plan))))
            (should (equal '("o/r#3" . "pr 3 (merged)")
                           (funcall fmt (hitl-plan-step-target step3))))
            ;; Skip step 4; revive step 2 before execution to verify re-check.
            (setf (hitl-plan-step-status step4) 'skipped)
            (magit-dash-pr-cache-put "o/r" 2 (magit-dash-pr-cache-test/pr 2 "OPEN"))
            (hitl-plan-execute plan)
            (should (magit-dash-pr-cache-get "o/r" 1))
            (should (magit-dash-pr-cache-get "o/r" 2))
            (should-not (magit-dash-pr-cache-get "o/r" 3))
            (should (magit-dash-pr-cache-get "o/r" 4))))))))

(ert-deftest magit-dash-pr-cache/prune-hitl-marks-failed-when-delete-fails ()
  (require 'hitl-plan)
  (require 'hitl-plan-ui)
  (magit-dash-pr-cache-test/with-state
    (let* ((hitl-plan-directory (expand-file-name "plans/" root))
           (hitl-plan--store (make-hash-table :test #'equal)))
      (make-directory hitl-plan-directory t)
      (magit-dash-pr-cache-put "o/r" 3 (magit-dash-pr-cache-test/pr 3 "MERGED"))
      (cl-letf (((symbol-function 'hitl-plan-review) #'ignore))
        (let ((plan (magit-dash-pr-cache-prune)))
          (cl-letf (((symbol-function 'delete-file)
                     (lambda (&rest _) (error "permission denied"))))
            (hitl-plan-execute plan))
          (should (eq 'complete (hitl-plan-status plan)))
          (let ((step (car (hitl-plan-steps plan))))
            (should (eq 'failed (hitl-plan-step-status step)))
            (should (string-match-p "Could not delete" (hitl-plan-step-message step))))
          (should (magit-dash-pr-cache-get "o/r" 3)))))))

(ert-deftest magit-dash-pr-cache/prune-falls-back-to-y-or-n-p-when-hitl-unavailable ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-put "o/r" 1 (magit-dash-pr-cache-test/pr 1 "OPEN"))
    (magit-dash-pr-cache-put "o/r" 2 (magit-dash-pr-cache-test/pr 2 "OPEN"))
    (magit-dash-pr-cache-mark-gone "o/r" 2)
    (magit-dash-pr-cache-put "o/r" 3 (magit-dash-pr-cache-test/pr 3 "CLOSED"))
    (let ((prompts nil))
      (cl-letf (((symbol-function 'hitl-plan-create) nil)
                ((symbol-function 'y-or-n-p)
                 (lambda (prompt) (push prompt prompts) nil)))
        (should-not (magit-dash-pr-cache-prune))
        (should (= 1 (length prompts)))
        (should (string-match-p "Prune 2" (car prompts)))
        (should (= 3 (length (magit-dash-pr-cache-test/files)))))
      (cl-letf (((symbol-function 'hitl-plan-create) nil)
                ((symbol-function 'y-or-n-p)
                 (lambda (prompt) (push prompt prompts) t)))
        (should (= 2 (magit-dash-pr-cache-prune)))
        (should (magit-dash-pr-cache-get "o/r" 1))
        (should-not (magit-dash-pr-cache-get "o/r" 2))
        (should-not (magit-dash-pr-cache-get "o/r" 3))))))

(ert-deftest magit-dash-pr-cache/search-p-preserves-volatile-field-times ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-put
     "o/r" 1 '((number . 1) (title . "t") (reviewDecision . "APPROVED")
               (statusCheckRollup . ((x . 1)))))
    (let ((old (- (float-time) 7200)))
      (magit-dash-pr-cache--write
       (magit-dash-pr-cache--file "o/r" 1)
       (plist-put (plist-put (copy-sequence (magit-dash-pr-cache-get "o/r" 1))
                             :fetched-at old)
                  :field-times (list (cons 'reviewDecision old)
                                     (cons 'statusCheckRollup old))))
      (magit-dash-pr-dashboard--cache-row '((number . 1) (title . "t2")) "o/r" t)
      (let ((record (magit-dash-pr-cache-get "o/r" 1)))
        (should-not (magit-dash-pr-cache-stale-p record))
        (should (= old (alist-get 'reviewDecision (plist-get record :field-times))))
        (should (= old (alist-get 'statusCheckRollup (plist-get record :field-times))))
        (should (magit-dash-pr-cache-field-stale-p record 'reviewDecision))
        (should (magit-dash-pr-cache-stale-p record (+ (float-time) 7200)))))))

(ert-deftest magit-dash-pr-cache/concurrent-write-does-not-clobber-newer-updated-at ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-put "o/r" 1 '((number . 1) (title . "v1")
                                       (updatedAt . "2026-01-01T00:00:00Z")))
    (let ((stale-prior (magit-dash-pr-cache-get "o/r" 1)))
      ;; Another instance wrote a newer revision to disk.
      (magit-dash-pr-cache-put "o/r" 1 '((number . 1) (title . "v3")
                                         (updatedAt . "2026-03-01T00:00:00Z")))
      (magit-dash-pr-cache-put "o/r" 1 '((number . 1) (title . "v2")
                                         (updatedAt . "2026-02-01T00:00:00Z"))
                               nil stale-prior)
      (let ((on-disk (magit-dash-pr-cache-get "o/r" 1)))
        (should (equal "v3" (map-elt (plist-get on-disk :payload) 'title)))
        (should (equal "2026-03-01T00:00:00Z" (plist-get on-disk :updated-at)))))
    ;; Concurrent write landing mid-put (right before rename).
    (let ((orig-merge (symbol-function 'magit-dash-pr-cache-merge-payload))
          (injected nil))
      (cl-letf (((symbol-function 'magit-dash-pr-cache-merge-payload)
                 (lambda (fresh old)
                   (prog1 (funcall orig-merge fresh old)
                     (unless injected
                       (setq injected t)
                       (magit-dash-pr-cache-put
                        "o/r" 1 '((number . 1) (title . "v5")
                                  (updatedAt . "2026-05-01T00:00:00Z"))))))))
        (magit-dash-pr-cache-put "o/r" 1 '((number . 1) (title . "v4")
                                           (updatedAt . "2026-04-01T00:00:00Z")))))
    (let ((on-disk (magit-dash-pr-cache-get "o/r" 1)))
      (should (equal "v5" (map-elt (plist-get on-disk :payload) 'title)))
      (should (equal "2026-05-01T00:00:00Z" (plist-get on-disk :updated-at))))))

(ert-deftest magit-dash-pr-cache/cold-start-filter-fidelity ()
  (magit-dash-pr-cache-test/with-state
    (magit-dash-pr-cache-put
     "o/r" 1 '((number . 1) (title . "alice open") (state . "OPEN")
               (author . ((login . "Alice")))
               (mergeable . "CONFLICTING")
               (reviewRequests . (((login . "alice"))))))
    (magit-dash-pr-cache-put
     "o/r" 2 '((number . 2) (title . "bob open") (state . "OPEN")
               (author . ((login . "bob")))
               (mergeable . "MERGEABLE")
               (reviewRequests . (((login . "bob"))))))
    (magit-dash-pr-cache-put
     "o/r" 3 '((number . 3) (title . "alice merged") (state . "MERGED")
               (author . ((login . "alice")))))
    (magit-dash-pr-cache-put
     "o/r" 4 '((number . 4) (title . "alice closed") (state . "CLOSED")
               (author . ((login . "alice")))))
    (magit-dash-pr-cache-put
     "o/other" 5 '((number . 5) (title . "search no fields") (state . "OPEN")
                   (author . ((login . "bob")))))
    (let ((nums (lambda (filters)
                  (sort (seq-map (lambda (e) (plist-get (car e) :number))
                                 (magit-dash-pr-dashboard--cached-entries filters))
                        #'<))))
      (cl-letf (((symbol-function 'magit-dash-pr--account-login) (lambda (_) "alice")))
        (should (equal '(1) (funcall nums '(:state "open" :author "@me"))))
        (should (equal '(1) (funcall nums '(:state "open" :repo "o/r" :review-requested "@me"))))
        ;; Cross-repo search preserves entries lacking reviewRequests as fallback.
        (should (equal '(1 5) (funcall nums '(:state "open" :review-requested "@me")))))
      ;; Fallback via magit-dash-pr-dashboard--author-login when account-login is nil.
      (cl-letf (((symbol-function 'magit-dash-pr--account-login) (lambda (_) nil)))
        (let ((magit-dash-pr-dashboard--author-login "bob"))
          (should (equal '(2 5) (funcall nums '(:state "open" :author "@me"))))))
      ;; States: all, closed (per-repo vs cross-repo), merged, conflicts.
      (should (equal '(1 2 3 4 5) (funcall nums '(:state "all"))))
      (should (equal '(4) (funcall nums '(:state "closed" :repo "o/r"))))
      (should (equal '(3 4) (funcall nums '(:state "closed"))))
      (should (equal '(3) (funcall nums '(:state "merged"))))
      (should (equal '(1) (funcall nums '(:state "open" :conflicts t)))))))

(ert-deftest magit-dash-pr-cache/migrate-legacy-handles-concurrent-canon-dir ()
  (magit-dash-pr-cache-test/with-state
    (let ((legacy (magit-dash-pr-cache-test/write-legacy
                   "Owner/Repo" 1 (magit-dash-pr-cache-test/legacy-record 1 "legacy" 100.0)))
          (orig-rename (symbol-function 'rename-file)))
      (cl-letf (((symbol-function 'rename-file)
                 (lambda (old new &optional ok)
                   (if (and (string-match-p "/Owner/Repo\\'" old)
                            (string-match-p "/owner/repo\\'" new))
                       (progn
                         (make-directory new t)
                         (signal 'file-already-exists (list new)))
                     (funcall orig-rename old new ok)))))
        (magit-dash-pr-cache-reset-migrated)
        (should (equal "legacy"
                       (map-elt (plist-get (magit-dash-pr-cache-get "owner/repo" 1) :payload)
                                'title)))
        (should-not (file-exists-p legacy))))))

(provide 'test-magit-dash-pr-cache)
;;; test-magit-dash-pr-cache.el ends here
