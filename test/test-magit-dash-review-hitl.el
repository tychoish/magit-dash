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

(provide 'test-magit-dash-review-hitl)
;;; test-magit-dash-review-hitl.el ends here
