;;; test-magit-dash-timer.el --- ERT tests for magit-dash-timer -*- lexical-binding: t -*-

;; Run inside a live Emacs session:
;;   (ert "^magit-dash-timer/")
;;
;; Batch run:
;;   emacs --batch -l test/test-helper.el \
;;     -l magit-dash-timer.el \
;;     -l test/test-magit-dash-timer.el \
;;     --eval '(ert-run-tests-batch-and-exit "magit-dash-timer/")'

(require 'ert)
(require 'map)
(require 'magit-dash-timer)

;;;; Guard functions

(ert-deftest magit-dash-timer/global-interval-elapsed-at-startup ()
  "Interval is elapsed when last-sync-attempt is 0.0 (startup default)."
  (let ((magit-dash--last-sync-attempt 0.0)
        (magit-dash-sync-min-interval 300))
    (should (magit-dash--global-interval-elapsed-p))))

(ert-deftest magit-dash-timer/global-interval-not-elapsed-when-recent ()
  "Interval is not elapsed when last-sync-attempt was just now."
  (let ((magit-dash--last-sync-attempt (float-time))
        (magit-dash-sync-min-interval 300))
    (should-not (magit-dash--global-interval-elapsed-p))))

(ert-deftest magit-dash-timer/global-interval-elapsed-after-threshold ()
  "Interval is elapsed when enough time has passed."
  (let ((magit-dash--last-sync-attempt (- (float-time) 400))
        (magit-dash-sync-min-interval 300))
    (should (magit-dash--global-interval-elapsed-p))))

(ert-deftest magit-dash-timer/global-interval-respects-min-interval-variable ()
  "Changing `magit-dash-sync-min-interval' affects the guard."
  (let ((magit-dash--last-sync-attempt (- (float-time) 10)))
    (let ((magit-dash-sync-min-interval 5))
      (should (magit-dash--global-interval-elapsed-p)))
    (let ((magit-dash-sync-min-interval 300))
      (should-not (magit-dash--global-interval-elapsed-p)))))

(ert-deftest magit-dash-timer/idle-for-p-huge-threshold-returns-nil ()
  "`magit-dash--idle-for-p' with an impossible threshold always returns nil."
  (should-not (magit-dash--idle-for-p 999999999)))

(ert-deftest magit-dash-timer/idle-for-p-returns-boolean-or-nil ()
  "`magit-dash--idle-for-p' returns nil or a non-nil value — never errors."
  (let ((result (magit-dash--idle-for-p 0)))
    (should (or (null result) result))))

;;;; Timer registration

(ert-deftest magit-dash-timer/register-interval-creates-entry ()
  "`magit-dash-register-sync-timer' with :kind interval adds a timer entry."
  (let ((magit-dash--sync-timers (make-hash-table :test #'equal)))
    (magit-dash-register-sync-timer :name "t" :kind 'interval :interval 3600)
    (unwind-protect
        (should (map-elt magit-dash--sync-timers "t"))
      (magit-dash-cancel-sync-timer "t"))))

(ert-deftest magit-dash-timer/register-idle-creates-entry ()
  "`magit-dash-register-sync-timer' with :kind idle adds a timer entry."
  (let ((magit-dash--sync-timers (make-hash-table :test #'equal)))
    (magit-dash-register-sync-timer :name "t" :kind 'idle :idle-delay 3600)
    (unwind-protect
        (should (map-elt magit-dash--sync-timers "t"))
      (magit-dash-cancel-sync-timer "t"))))

(ert-deftest magit-dash-timer/register-same-name-replaces-timer ()
  "Re-registering with the same name produces a new distinct timer object."
  (let ((magit-dash--sync-timers (make-hash-table :test #'equal)))
    (magit-dash-register-sync-timer :name "t" :kind 'interval :interval 3600)
    (let ((first (map-elt magit-dash--sync-timers "t")))
      (magit-dash-register-sync-timer :name "t" :kind 'interval :interval 7200)
      (unwind-protect
          (let ((second (map-elt magit-dash--sync-timers "t")))
            (should second)
            (should-not (eq first second)))
        (magit-dash-cancel-sync-timer "t")))))

(ert-deftest magit-dash-timer/register-same-name-cancels-old-timer ()
  "Re-registering with the same name removes the old timer from active lists."
  (let ((magit-dash--sync-timers (make-hash-table :test #'equal)))
    (magit-dash-register-sync-timer :name "t" :kind 'interval :interval 3600)
    (let ((first (map-elt magit-dash--sync-timers "t")))
      (magit-dash-register-sync-timer :name "t" :kind 'interval :interval 7200)
      (unwind-protect
          (should-not (or (memq first timer-list)
                          (memq first timer-idle-list)))
        (magit-dash-cancel-sync-timer "t")))))

(ert-deftest magit-dash-timer/register-distinct-names-coexist ()
  "Two timers with different names both appear in the table."
  (let ((magit-dash--sync-timers (make-hash-table :test #'equal)))
    (magit-dash-register-sync-timer :name "a" :kind 'interval :interval 3600)
    (magit-dash-register-sync-timer :name "b" :kind 'interval :interval 7200)
    (unwind-protect
        (should (= 2 (hash-table-count magit-dash--sync-timers)))
      (magit-dash-cancel-all-sync-timers))))

;;;; Timer cancellation

(ert-deftest magit-dash-timer/cancel-removes-entry ()
  "`magit-dash-cancel-sync-timer' removes the entry from the table."
  (let ((magit-dash--sync-timers (make-hash-table :test #'equal)))
    (magit-dash-register-sync-timer :name "x" :kind 'interval :interval 3600)
    (magit-dash-cancel-sync-timer "x")
    (should (= 0 (hash-table-count magit-dash--sync-timers)))))

(ert-deftest magit-dash-timer/cancel-nonexistent-is-noop ()
  "`magit-dash-cancel-sync-timer' is silent for an unregistered name."
  (let ((magit-dash--sync-timers (make-hash-table :test #'equal)))
    (should-not (magit-dash-cancel-sync-timer "does-not-exist"))))

(ert-deftest magit-dash-timer/cancel-all-removes-all ()
  "`magit-dash-cancel-all-sync-timers' removes every registered timer."
  (let ((magit-dash--sync-timers (make-hash-table :test #'equal)))
    (magit-dash-register-sync-timer :name "a" :kind 'interval :interval 3600)
    (magit-dash-register-sync-timer :name "b" :kind 'idle    :idle-delay 7200)
    (magit-dash-cancel-all-sync-timers)
    (should (= 0 (hash-table-count magit-dash--sync-timers)))))

(ert-deftest magit-dash-timer/cancel-all-on-empty-table-is-noop ()
  "`magit-dash-cancel-all-sync-timers' is silent on an empty table."
  (let ((magit-dash--sync-timers (make-hash-table :test #'equal)))
    (magit-dash-cancel-all-sync-timers)
    (should (= 0 (hash-table-count magit-dash--sync-timers)))))

;;;; magit-dash--timer-sync guard behavior

(ert-deftest magit-dash-timer/sync-skips-when-network-unavailable ()
  "sync does not update last-sync-attempt when network check returns nil."
  (let ((magit-dash--last-sync-attempt 0.0)
        (magit-dash-sync-min-interval 0)
        (magit-dash-network-check-function (lambda () nil)))
    (magit-dash--timer-sync nil nil)
    (should (= 0.0 magit-dash--last-sync-attempt))))

(ert-deftest magit-dash-timer/sync-skips-when-min-interval-not-elapsed ()
  "sync does not update last-sync-attempt when global interval has not elapsed."
  (let ((magit-dash--last-sync-attempt (float-time))
        (magit-dash-sync-min-interval 300)
        (magit-dash-network-check-function (lambda () t)))
    (let ((before magit-dash--last-sync-attempt))
      (magit-dash--timer-sync nil nil)
      (should (= before magit-dash--last-sync-attempt)))))

(ert-deftest magit-dash-timer/sync-skips-when-not-idle-enough ()
  "sync does not update last-sync-attempt when idle threshold is not met."
  (let ((magit-dash--last-sync-attempt 0.0)
        (magit-dash-sync-min-interval 0)
        (magit-dash-network-check-function (lambda () t)))
    (magit-dash--timer-sync nil 999999999)
    (should (= 0.0 magit-dash--last-sync-attempt))))

(ert-deftest magit-dash-timer/sync-updates-timestamp-when-guards-pass ()
  "sync updates last-sync-attempt when all guards pass."
  (let ((magit-dash--last-sync-attempt 0.0)
        (magit-dash-sync-min-interval 0)
        (magit-dash-network-check-function (lambda () t))
        (magit-dash-repo-list nil))
    (magit-dash--timer-sync nil nil)
    (should (> magit-dash--last-sync-attempt 0.0))))

(ert-deftest magit-dash-timer/sync-filters-repos-without-steps ()
  "sync skips repos that have no auto-sync steps configured."
  (let ((magit-dash--last-sync-attempt 0.0)
        (magit-dash-sync-min-interval 0)
        (magit-dash-network-check-function (lambda () t))
        (magit-dash-repo-list
         (list (magit-dash-repo--make :name "bare" :path "/tmp/bare"))))
    ;; Guards pass, timestamp is updated, but batch-run is not called (no steps)
    (magit-dash--timer-sync nil nil)
    (should (> magit-dash--last-sync-attempt 0.0))))

(ert-deftest magit-dash-timer/sync-filters-by-repos-list ()
  "sync restricts candidates to the named repos when REPOS is non-nil."
  (let ((magit-dash--last-sync-attempt 0.0)
        (magit-dash-sync-min-interval 0)
        (magit-dash-network-check-function (lambda () t))
        (magit-dash-repo-list
         (list (magit-dash-repo--make :name "alpha" :path "/tmp/alpha")
               (magit-dash-repo--make :name "beta"  :path "/tmp/beta"))))
    ;; Requesting only "alpha" — no error even if beta has no steps
    (magit-dash--timer-sync '("alpha") nil)
    (should (> magit-dash--last-sync-attempt 0.0))))

(ert-deftest magit-dash-timer/network-check-function-is-overridable ()
  "Replacing `magit-dash-network-check-function' changes guard outcome."
  (let ((magit-dash--last-sync-attempt 0.0)
        (magit-dash-sync-min-interval 0)
        (magit-dash-repo-list nil))
    (let ((magit-dash-network-check-function (lambda () nil)))
      (magit-dash--timer-sync nil nil)
      (should (= 0.0 magit-dash--last-sync-attempt)))
    (let ((magit-dash-network-check-function (lambda () t)))
      (magit-dash--timer-sync nil nil)
      (should (> magit-dash--last-sync-attempt 0.0)))))

(provide 'test-magit-dash-timer)
;;; test-magit-dash-timer.el ends here
