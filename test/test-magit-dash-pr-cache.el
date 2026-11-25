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
                    (lambda () root)))
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

(provide 'test-magit-dash-pr-cache)
;;; test-magit-dash-pr-cache.el ends here
