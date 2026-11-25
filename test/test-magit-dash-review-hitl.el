;;; test-magit-dash-review-hitl.el --- ERT tests for magit-dash-review-hitl -*- lexical-binding: t; no-byte-compile: t; -*-

;; Run inside a live Emacs session:
;;   (ert "^magit-dash-review-hitl/")

(require 'ert)
(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'hitl-plan)

(require 'test-helper)
(require 'magit-dash-review)
(require 'magit-dash-review-hitl)

;;; Helpers

(defmacro magit-dash-review-hitl-test/with-temp-dir (&rest body)
  "Execute BODY with `default-directory' and `hitl-plan-directory' in a fresh temp dir."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "magit-dash-review-hitl-test-" t))
          (hitl-plan-directory (expand-file-name "plans/" dir))
          (hitl-plan--store (make-hash-table :test #'equal)))
     (make-directory hitl-plan-directory t)
     (unwind-protect
         (let ((default-directory dir))
           ,@body)
       (delete-directory dir t))))

;;; Domain and Target Sanitization Tests

(ert-deftest magit-dash-review-hitl/domain-registered ()
  "The `code-review' domain is registered in `hitl-plan' with all slots."
  (let ((dom (hitl-plan-get-domain 'code-review)))
    (should dom)
    (should (eq (hitl-plan-domain-name dom) 'code-review))
    (should (functionp (hitl-plan-domain-resolve-target dom)))
    (should (functionp (hitl-plan-domain-format-target dom)))
    (should (functionp (hitl-plan-domain-visit-target dom)))
    (should (functionp (hitl-plan-domain-driver dom)))
    (should (functionp (hitl-plan-domain-accept-buffer dom)))
    (should (functionp (hitl-plan-domain-sanitize-target dom)))))

(ert-deftest magit-dash-review-hitl/sanitize-target-valid-and-snake-case ()
  "magit-dash-review-hitl-sanitize-target normalizes kebab and MCP snake_case keys."
  (let ((clean (magit-dash-review-hitl-sanitize-target
                '(:id "c01"
                  :review_file "/tmp/repo/.git/review/pr-42.json"
                  :worktree "/tmp/repo/.wt/pr-42"
                  :path "lisp/foo.el"
                  :line 10
                  :end_line 15
                  :severity "high"
                  :author "alice"
                  :body "Check bounds here"))))
    (should (equal (plist-get clean :id) "c01"))
    (should (equal (plist-get clean :review-file) "/tmp/repo/.git/review/pr-42.json"))
    (should (equal (plist-get clean :worktree) "/tmp/repo/.wt/pr-42/"))
    (should (equal (plist-get clean :path) "lisp/foo.el"))
    (should (= (plist-get clean :line) 10))
    (should (= (plist-get clean :end-line) 15))
    (should (equal (plist-get clean :severity) "high"))
    (should (equal (plist-get clean :author) "alice"))
    (should (equal (plist-get clean :body) "Check bounds here"))))

(ert-deftest magit-dash-review-hitl/sanitize-target-rejects-unsafe-paths ()
  "magit-dash-review-hitl-sanitize-target rejects TRAMP, `..', and non-review paths."
  ;; Missing review-file
  (should-error (magit-dash-review-hitl-sanitize-target '(:path "foo.el" :line 1)))
  ;; Remote TRAMP review-file
  (should-error (magit-dash-review-hitl-sanitize-target
                 '(:review-file "/ssh:host:/repo/.git/review/pr-1.json" :path "foo.el")))
  ;; Traversal in review-file
  (should-error (magit-dash-review-hitl-sanitize-target
                 '(:review-file "/tmp/repo/.git/review/../secret.json" :path "foo.el")))
  ;; Non-review directory
  (should-error (magit-dash-review-hitl-sanitize-target
                 '(:review-file "/tmp/arbitrary/data.json" :path "foo.el")))
  ;; Traversal in :path
  (should-error (magit-dash-review-hitl-sanitize-target
                 '(:review-file "/tmp/repo/.git/review/pr-1.json" :path "../etc/passwd")))
  ;; Absolute :path
  (should-error (magit-dash-review-hitl-sanitize-target
                 '(:review-file "/tmp/repo/.git/review/pr-1.json" :path "/etc/passwd")))
  ;; Invalid line numbers
  (should-error (magit-dash-review-hitl-sanitize-target
                 '(:review-file "/tmp/repo/.git/review/pr-1.json" :path "foo.el" :line 0)))
  (should-error (magit-dash-review-hitl-sanitize-target
                 '(:review-file "/tmp/repo/.git/review/pr-1.json" :path "foo.el" :line 20 :end-line 10))))

(ert-deftest magit-dash-review-hitl/format-and-accept-buffer ()
  "Format target and accept-buffer predicate work as expected."
  (let ((fmt (magit-dash-review-hitl-format-target
              '(:path "magit-dash-pr.el" :line 185 :end-line 192
                :severity "high" :author "tychoish"
                :body "Cap GraphQL page size to 50.\nMore details."))))
    (should (equal (car fmt) "magit-dash-pr.el:185-192"))
    (should (equal (cdr fmt) "[high] tychoish: Cap GraphQL page size to 50.")))
  (with-temp-buffer
    (should-not (magit-dash-review-hitl-accept-buffer-p (current-buffer)))
    (setq-local diff-review-file-mode t)
    (should (magit-dash-review-hitl-accept-buffer-p (current-buffer)))))

;;; Outbound & Inbound Operation Tests

(ert-deftest magit-dash-review-hitl/outbound-stage-and-send-to-agent ()
  "magit-dash-review-stage-hitl creates a plan; send-to-agent batches non-skipped steps with note overrides."
  (magit-dash-review-hitl-test/with-temp-dir
    (let* ((review-file (expand-file-name "review/pr-55.json" default-directory))
           (c1 (make-hash-table :test #'equal))
           (c2 (make-hash-table :test #'equal))
           (data (make-hash-table :test #'equal))
           (dispatched-batches nil))
      (setf (map-elt c1 "id") "f01")
      (setf (map-elt c1 "path") "a.el")
      (setf (map-elt c1 "lines") [10 12])
      (setf (map-elt c1 "author") "tychoish")
      (setf (map-elt c1 "body") "First finding body")
      (setf (map-elt c1 "resolved") :false)
      (setf (map-elt c1 "replies") [])

      (setf (map-elt c2 "id") "f02")
      (setf (map-elt c2 "path") "b.el")
      (setf (map-elt c2 "lines") [20 20])
      (setf (map-elt c2 "author") "tychoish")
      (setf (map-elt c2 "body") "Second finding body")
      (setf (map-elt c2 "resolved") :false)
      (setf (map-elt c2 "replies") [])

      (setf (map-elt data "format") "diff-review/1")
      (setf (map-elt data "root") default-directory)
      (setf (map-elt data "base") "main")
      (setf (map-elt data "head") "feat")
      (setf (map-elt data "viewed") (make-hash-table :test #'equal))
      (setf (map-elt data "comments") (vector c1 c2))
      (magit-dash-review--write-json-file review-file data)

      (let* ((plan (magit-dash-review-stage-hitl review-file 'send-to-agent))
             (steps (hitl-plan-steps plan)))
        (should (= (length steps) 2))
        ;; Override note on first step, skip second step
        (setf (hitl-plan-step-note (nth 0 steps)) "Refined instruction for agent on a.el")
        (setf (hitl-plan-step-status (nth 1 steps)) 'skipped)

        (let ((magit-dash-review-hitl-dispatch-function
               (lambda (_p findings)
                 (push findings dispatched-batches))))
          (hitl-plan-execute plan)
          (should (eq (hitl-plan-status plan) 'complete))
          (should (= (length dispatched-batches) 1))
          (let ((batch (car dispatched-batches)))
            (should (= (length batch) 1))
            (should (equal (plist-get (car batch) :id) "f01"))
            (should (equal (plist-get (car batch) :path) "a.el"))
            (should (equal (plist-get (car batch) :body)
                           "Refined instruction for agent on a.el"))))))))

(ert-deftest magit-dash-review-hitl/inbound-record-draft-and-verdict ()
  "Executing record-finding, draft-finding, and order-last apply-verdict updates JSON atomically."
  (magit-dash-review-hitl-test/with-temp-dir
    (let* ((review-file (expand-file-name "review/pr-88.json" default-directory))
           (data (make-hash-table :test #'equal)))
      (setf (map-elt data "format") "diff-review/1")
      (setf (map-elt data "root") default-directory)
      (setf (map-elt data "base") "main")
      (setf (map-elt data "head") "feat-88")
      (setf (map-elt data "viewed") (make-hash-table :test #'equal))
      (setf (map-elt data "comments") [])
      (magit-dash-review--write-json-file review-file data)

      ;; Plan with apply-verdict listed first and record-finding second;
      ;; apply-verdict is :order-last so both run and verdict lands cleanly.
      (let* ((t1 (magit-dash-review-hitl-sanitize-target
                  (list :id "agent-1"
                        :review-file review-file
                        :worktree default-directory
                        :path "src/core.el"
                        :line 30
                        :end-line 35
                        :severity "high"
                        :author "agent-reviewer"
                        :body "Unchecked nil return from process")))
             (plan (hitl-plan-create
                    :domain 'code-review
                    :description "Inbound agent review pass"
                    :scope (list :review-file review-file :worktree default-directory)
                    :targets (list t1)
                    :ops (list (list :name 'apply-verdict :args '("changes" "tychoish" "sha888"))
                               (list :name 'record-finding)))))
        (hitl-plan-execute plan)
        (should (eq (hitl-plan-status plan) 'complete))
        (let* ((reloaded (magit-dash-review--read-json-file review-file))
               (comments (map-elt reloaded "comments"))
               (verdict (map-elt reloaded "verdict")))
          (should (= (length comments) 1))
          (let ((c (aref comments 0)))
            (should (equal (map-elt c "id") "agent-1"))
            (should (equal (map-elt c "status") "accepted"))
            (should (equal (map-elt c "path") "src/core.el"))
            (should (equal (map-elt c "lines") [30 35]))
            (should (equal (map-elt c "body") "Unchecked nil return from process")))
          (should verdict)
          (should (equal (map-elt verdict "state") "changes"))
          (should (equal (map-elt verdict "by") "tychoish"))
          (should (equal (map-elt verdict "head") "sha888"))))

      ;; Also test draft-finding
      (let* ((t2 (magit-dash-review-hitl-sanitize-target
                  (list :id "agent-draft-2"
                        :review-file review-file
                        :worktree default-directory
                        :path "src/core.el"
                        :line 45
                        :author "agent-reviewer"
                        :body "Nit: consider thread-last")))
             (plan2 (hitl-plan-create
                     :domain 'code-review
                     :description "Inbound draft finding"
                     :targets (list t2)
                     :ops (list (list :name 'draft-finding)))))
        (hitl-plan-execute plan2)
        (let* ((reloaded (magit-dash-review--read-json-file review-file))
               (comments (append (map-elt reloaded "comments") nil))
               (draft-c (seq-find (lambda (c) (equal (map-elt c "id") "agent-draft-2"))
                                  comments)))
          (should draft-c)
          (should (equal (map-elt draft-c "status") "draft")))))))

(ert-deftest magit-dash-review-hitl/flush-skips-refresh-of-modified-buffer ()
  "MED-2: the batch flush writes the file but leaves a modified buffer alone."
  (magit-dash-review-hitl-test/with-temp-dir
    (let* ((review-file (expand-file-name "review/pr-77.json" default-directory))
           (data (make-hash-table :test #'equal)))
      (setf (map-elt data "format") "diff-review/1")
      (setf (map-elt data "comments") [])
      (magit-dash-review--write-json-file review-file data)
      (let ((buf (find-file-noselect review-file))
            (magit-dash-review-hitl--batch-json-updates nil)
            (c (make-hash-table :test #'equal)))
        (unwind-protect
            (progn
              (setf (map-elt c "id") "f1")
              (setf (map-elt c "body") "flushed")
              (with-current-buffer buf (goto-char (point-max)) (insert "unsaved edit"))
              (magit-dash-review-hitl--queue-json-update review-file (list :type 'comment :comment c))
              (magit-dash-review-hitl--flush-json-updates)
              (with-current-buffer buf
                (should (buffer-modified-p))
                (should (string-suffix-p "unsaved edit" (buffer-string))))
              (should (= 1 (length (map-elt (magit-dash-review--read-json-file review-file) "comments")))))
          (with-current-buffer buf (set-buffer-modified-p nil))
          (kill-buffer buf))))))

(provide 'test-magit-dash-review-hitl)
;;; test-magit-dash-review-hitl.el ends here
