;;; test-magit-dash-review-analysis.el --- Tests for the analysis overlay -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Decoration data, overlays, the minor mode and the refresh writer, against
;; a fixture review file.  No code-review, no child Emacs, no network.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'test-helper)
(require 'magit-dash-review-analysis)

(defconst magit-dash-review-analysis-test/fixture
  (expand-file-name "fixtures/review-analysis.json"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun magit-dash-review-analysis-test/table ()
  "Parse the fixture review file."
  (magit-dash-review-json-read-file magit-dash-review-analysis-test/fixture))

(defun magit-dash-review-analysis-test/line-pos (line)
  "Start position of LINE in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (point)))

(ert-deftest magit-dash-review-analysis-test/parse-ranges ()
  (should (equal (magit-dash-review-analysis-parse-ranges "-1,3 +1,4") '(1 3 1 4)))
  (should (equal (magit-dash-review-analysis-parse-ranges "-20 +21,2") '(20 1 21 2)))
  (should (equal (magit-dash-review-analysis-parse-ranges "-5,1 +5,0") '(5 1 5 0)))
  (should-not (magit-dash-review-analysis-parse-ranges "0"))
  (should-not (magit-dash-review-analysis-parse-ranges nil)))

(ert-deftest magit-dash-review-analysis-test/text ()
  (let ((by-hunk (magit-dash-review-analysis-for-path
                  (magit-dash-review-analysis-test/table) "src/a.ts")))
    (should (equal (substring-no-properties
                    (magit-dash-review-analysis-text (map-elt by-hunk "-1,3 +1,4")))
                   "r2 [NO-TEST] c1,c2"))
    (should (equal (substring-no-properties
                    (magit-dash-review-analysis-text (map-elt by-hunk "-20 +21,2")))
                   "r5 [CI-GAME] [RISK]"))
    (should (equal (substring-no-properties (magit-dash-review-analysis-file-text by-hunk))
                   "r5 [NO-TEST] [CI-GAME] [RISK] c1,c2"))))

(ert-deftest magit-dash-review-analysis-test/missing-analysis-is-noop ()
  (let ((table (magit-dash-review-analysis-test/table)))
    (should-not (magit-dash-review-analysis-for-path table "src/none.ts"))
    (should-not (magit-dash-review-analysis-for-path (make-hash-table :test #'equal) "src/a.ts"))
    (should-not (magit-dash-review-analysis-for-path nil "src/a.ts"))
    (should-not (magit-dash-review-analysis-file-text nil))
    (should-not (magit-dash-review-analysis-hunk-entries nil '((:line 1 :count 1))))
    (with-temp-buffer
      (insert "a\nb\n")
      (should-not (magit-dash-review-analysis-decorate-buffer
                   nil '((:line 1 :count 1))
                   #'magit-dash-review-analysis-test/line-pos))
      (should-not (overlays-in (point-min) (point-max))))))

(ert-deftest magit-dash-review-analysis-test/hunk-matching ()
  (let* ((by-hunk (magit-dash-review-analysis-for-path
                   (magit-dash-review-analysis-test/table) "src/a.ts"))
         (hunks '((:line 2 :count 2) (:line 21 :count 1) (:line 50 :count 3)))
         (pairs (magit-dash-review-analysis-hunk-entries by-hunk hunks)))
    ;; -90,2 +99,2 matches no hunk and is ignored; line 50 matches nothing.
    (should (equal (seq-map (lambda (pair) (plist-get (car pair) :line)) pairs) '(2 21)))
    (should (equal (map-elt (cdr (nth 1 pairs)) "rank") 5))))

(ert-deftest magit-dash-review-analysis-test/overlays ()
  (let ((by-hunk (magit-dash-review-analysis-for-path
                  (magit-dash-review-analysis-test/table) "src/a.ts")))
    (with-temp-buffer
      (insert "l1\nl2\nl3\nl4\nl5\n")
      (let ((overlays (magit-dash-review-analysis-decorate-buffer
                       by-hunk '((:line 1 :count 2) (:line 30 :count 1))
                       #'magit-dash-review-analysis-test/line-pos)))
        (should (= (length overlays) 1))
        (should (= (overlay-start (car overlays)) 3))
        (should (string-match-p "\\[NO-TEST\\]"
                                (overlay-get (car overlays) 'after-string)))
        (magit-dash-review-analysis-clear-buffer)
        (should-not (overlays-in (point-min) (point-max)))))))

(ert-deftest magit-dash-review-analysis-test/mode-on-off ()
  (message "CI diagnostic: diff-review=%S skip-conditions=%S quit=%S"
           (locate-library "diff-review")
           (get 'ert-test-skipped 'error-conditions) quit-flag)
  (unwind-protect
  (skip-unless (locate-library "diff-review"))
    (message "CI diagnostic: skip check exited"))
  (let ((by-hunk (magit-dash-review-analysis-for-path
                  (magit-dash-review-analysis-test/table) "src/a.ts")))
    (unwind-protect
        (with-temp-buffer
          (insert "l1\nl2\nl3\n")
          (setq-local diff-review-file-mode t)
          (magit-dash-review-analysis-mode 1)
          (should (advice-member-p #'ad:magit-dash-review-analysis-decorate
                                   'diff-review--decorate))
          (should (advice-member-p #'ad:magit-dash-review-analysis-file-line
                                   'diff-review--file-line))
          (magit-dash-review-analysis-decorate-buffer
           by-hunk '((:line 1 :count 1)) #'magit-dash-review-analysis-test/line-pos)
          (should (overlays-in (point-min) (point-max)))
          (magit-dash-review-analysis-mode -1)
          (should-not (overlays-in (point-min) (point-max)))
          (should-not (advice-member-p #'ad:magit-dash-review-analysis-decorate
                                       'diff-review--decorate))
          (should-not (advice-member-p #'ad:magit-dash-review-analysis-file-line
                                       'diff-review--file-line)))
      (magit-dash-review-analysis-mode -1))))

(ert-deftest magit-dash-review-analysis-test/file-line-advice ()
  (let ((table (magit-dash-review-analysis-test/table))
        (review (generate-new-buffer " *review*")))
    (unwind-protect
        (progn
          (with-current-buffer review
            (setq-local diff-review--data table))
          (cl-letf (((symbol-function 'diff-review--review-buffer) (lambda () review))
                    ((symbol-function 'diff-review--file)
                     (lambda (i) (list :path (if (eql i 0) "src/a.ts" "src/z.ts")))))
            (let ((fn (lambda (i _indent) (propertize "M name\n" 'diff-review-index i))))
              (let ((decorated (ad:magit-dash-review-analysis-file-line fn 0 "")))
                (should (string-match-p "r5 \\[NO-TEST\\]" decorated))
                (should (string-suffix-p "\n" decorated))
                (should (eql (get-text-property (1- (length decorated)) 'diff-review-index
                                                decorated)
                             0)))
              (should (equal (ad:magit-dash-review-analysis-file-line fn 1 "") "M name\n")))))
      (kill-buffer review))))

(ert-deftest magit-dash-review-analysis-test/write-replaces-analysis ()
  (let ((file (make-temp-file "analysis-test-" nil ".json"))
        (new (make-hash-table :test #'equal))
        (hunks (make-hash-table :test #'equal))
        (entry (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (copy-file magit-dash-review-analysis-test/fixture file t)
          (setf (map-elt entry "rank") 7)
          (setf (map-elt hunks "-1 +1") entry)
          (setf (map-elt new "src/new.ts") hunks)
          (should-not (magit-dash-review-analysis-write file (make-hash-table :test #'equal)))
          (should (magit-dash-review-analysis-write file new))
          (let ((analysis (map-elt (map-elt (magit-dash-review-json-read-file file) "code_review")
                                   "analysis")))
            (should (map-elt analysis "src/new.ts"))
            (should-not (map-elt analysis "src/a.ts"))))
      (ignore-errors (delete-file file))
      (ignore-errors (delete-file (concat file ".bak"))))))

(ert-deftest magit-dash-review-analysis-test/child-command ()
  (let ((argv (magit-dash-review-analysis-child-command '("o/r" . 1) "/tmp/out.json")))
    (should (member "--batch" argv))
    (should (string-match-p "magit-dash-review-json-export" (car (last argv))))))

(defun magit-dash-review-analysis-test/entries (alist)
  "Build a by-hunk table from ALIST of (KEY . RANK)."
  (let ((by-hunk (make-hash-table :test #'equal)))
    (seq-do (lambda (pair)
              (let ((entry (make-hash-table :test #'equal)))
                (setf (map-elt entry "rank") (cdr pair))
                (setf (map-elt by-hunk (car pair)) entry)))
            alist)
    by-hunk))

(ert-deftest magit-dash-review-analysis-test/deletion-hunks ()
  ;; A deletion at :line N is shown on N+1; the fixture's src/b.ts "-5,1 +5,0".
  (let ((by-hunk (magit-dash-review-analysis-for-path
                  (magit-dash-review-analysis-test/table) "src/b.ts")))
    (should (= (length (magit-dash-review-analysis-hunk-entries
                        by-hunk '((:line 5 :count 0)))) 1))
    (should (= (length (magit-dash-review-analysis-hunk-entries
                        by-hunk '((:line 6 :count 1)))) 1))
    (should-not (magit-dash-review-analysis-hunk-entries by-hunk '((:line 5 :count 1))))
    (should-not (magit-dash-review-analysis-hunk-entries by-hunk '((:line 4 :count 0)))))
  ;; Top of file: :line 0 :count 0 lives on line 1.
  (let ((by-hunk (magit-dash-review-analysis-test/entries '(("-1,2 +0,0" . 4)))))
    (should (= (length (magit-dash-review-analysis-hunk-entries
                        by-hunk '((:line 0 :count 0)))) 1))
    (with-temp-buffer
      (insert "l1\nl2\n")
      (let ((overlays (magit-dash-review-analysis-decorate-buffer
                       by-hunk '((:line 0 :count 0)) #'magit-dash-review-analysis-test/line-pos)))
        (should (= (overlay-start (car overlays)) 3))))))

(ert-deftest magit-dash-review-analysis-test/non-integer-rank ()
  (let ((by-hunk (magit-dash-review-analysis-test/entries
                  '(("-1 +1" . "high") ("-1 +1,2" . 3)))))
    (should (equal (map-elt (cdr (car (magit-dash-review-analysis-hunk-entries
                                       by-hunk '((:line 1 :count 1)))))
                            "rank")
                   3))
    (should-not (magit-dash-review-analysis-text
                 (map-elt (magit-dash-review-analysis-test/entries '(("-1 +1" . "high")))
                          "-1 +1")))))

(ert-deftest magit-dash-review-analysis-test/real-decorate-flow ()
  (skip-unless (locate-library "diff-review"))
  (require 'diff-review)
  (let ((table (magit-dash-review-analysis-test/table))
        (review (generate-new-buffer " *review*")))
    (unwind-protect
        (progn
          (with-current-buffer review
            (setq-local diff-review--data table))
          (cl-letf (((symbol-function 'diff-review--review-buffer) (lambda () review)))
            (with-temp-buffer
              (insert "l1\nl2\nl3\n")
              (setq-local diff-review-file-mode t)
              (setq-local diff-review--file-path "src/a.ts")
              (setq-local diff-review--hunks '((:line 1 :count 2)))
              (setq-local diff-review--overlays nil)
              (ad:magit-dash-review-analysis-decorate)
              (should (= (length diff-review--overlays) 1))
              (let ((overlay (car diff-review--overlays)))
                (diff-review--undecorate)
                (should-not (overlay-buffer overlay))))))
      (kill-buffer review))))

(ert-deftest magit-dash-review-analysis-test/advice-never-breaks-diff-review ()
  (let ((magit-dash-review-analysis--warned nil)
        (messages nil))
    (cl-letf (((symbol-function 'diff-review--review-buffer) (lambda () (error "boom")))
              ((symbol-function 'message)
               (lambda (&rest args) (push (apply #'format args) messages))))
      (let ((fn (lambda (_i _indent) "M name\n")))
        (should (equal (ad:magit-dash-review-analysis-file-line fn 0 "") "M name\n"))
        (should (equal (ad:magit-dash-review-analysis-file-line fn 0 "") "M name\n")))
      (should-not (ad:magit-dash-review-analysis-decorate))
      (should (= (length messages) 1)))))

(ert-deftest magit-dash-review-analysis-test/child-command-quoting ()
  (let* ((load-path (seq-map (lambda (i) (format "/tmp/dir-%d" i)) (number-sequence 1 200)))
         (print-length 3)
         (print-level 1)
         (form (car (last (magit-dash-review-analysis-child-command '("o/r" . 1) "/tmp/o.json")))))
    (should-not (string-match-p "\\.\\.\\." form))
    (should (string-match-p "/tmp/dir-200" form))
    (should (string-match-p "\"/tmp/o.json\"" form))))

(ert-deftest magit-dash-review-analysis-test/write-refuses-corrupt-file-without-prompt ()
  (let ((file (make-temp-file "analysis-test-" nil ".json"))
        (analysis (magit-dash-review-analysis-test/entries '(("-1 +1" . 1)))))
    (unwind-protect
        (progn
          (copy-file magit-dash-review-analysis-test/fixture (concat file ".bak") t)
          (with-temp-file file (insert "{not json"))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) (error "prompted"))))
            (let ((entry (make-hash-table :test #'equal))
                  (by-path (make-hash-table :test #'equal)))
              (setf (map-elt entry "rank") 1)
              (setf (map-elt by-path "-1 +1") entry)
              (setf (map-elt analysis "x") by-path)
              (should-error (magit-dash-review-analysis-write file analysis)
                            :type 'user-error))))
      (ignore-errors (delete-file file))
      (ignore-errors (delete-file (concat file ".bak"))))))

(defun magit-dash-review-analysis-test/run-sentinel (command file output)
  "Run COMMAND to exit, then its sentinel for FILE and OUTPUT; return messages."
  (let* ((log (generate-new-buffer " *analysis-test-log*"))
         (process (make-process :name "analysis-test" :buffer log :command command
                                :noquery t :connection-type 'pipe))
         (messages nil))
    (while (process-live-p process)
      (accept-process-output process 0.1))
    (process-put process 'magit-dash-review-analysis-file file)
    (process-put process 'magit-dash-review-analysis-output output)
    (setf (map-elt magit-dash-review-analysis--processes file) process)
    (cl-letf (((symbol-function 'message)
               (lambda (&rest args) (push (apply #'format args) messages))))
      (magit-dash-review-analysis--sentinel process "finished\n"))
    (should-not (buffer-live-p log))
    (should-not (map-elt magit-dash-review-analysis--processes file))
    (should-not (file-exists-p output))
    (nreverse messages)))

(ert-deftest magit-dash-review-analysis-test/sentinel-flow ()
  (let ((file (make-temp-file "analysis-test-" nil ".json"))
        (output (make-temp-file "analysis-out-" nil ".json")))
    (unwind-protect
        (progn
          (copy-file magit-dash-review-analysis-test/fixture file t)
          (with-temp-file output
            (insert "{\"src/new.ts\": {\"-1 +1\": {\"rank\": 2, \"tags\": [], \"criteria\": []}}}"))
          (let ((messages (magit-dash-review-analysis-test/run-sentinel '("true") file output)))
            (should (string-match-p "analysis stored" (car messages)))
            (should (map-elt (map-elt (map-elt (magit-dash-review-json-read-file file) "code_review")
                                      "analysis")
                             "src/new.ts"))))
      (ignore-errors (delete-file file))
      (ignore-errors (delete-file (concat file ".bak"))))))

(ert-deftest magit-dash-review-analysis-test/sentinel-failure-and-empty ()
  (let ((file (make-temp-file "analysis-test-" nil ".json"))
        (output (make-temp-file "analysis-out-" nil ".json")))
    (unwind-protect
        (progn
          (copy-file magit-dash-review-analysis-test/fixture file t)
          (let ((messages (magit-dash-review-analysis-test/run-sentinel '("false") file output)))
            (should (string-match-p "child Emacs failed" (car messages))))
          (setq output (make-temp-file "analysis-out-" nil ".json"))
          (with-temp-file output (insert "{}"))
          (let ((messages (magit-dash-review-analysis-test/run-sentinel '("true") file output)))
            (should (string-match-p "no analysis produced" (car messages))))
          (should (map-elt (map-elt (map-elt (magit-dash-review-json-read-file file) "code_review")
                                    "analysis")
                           "src/a.ts")))
      (ignore-errors (delete-file file))
      (ignore-errors (delete-file (concat file ".bak"))))))

(provide 'test-magit-dash-review-analysis)
;;; test-magit-dash-review-analysis.el ends here
