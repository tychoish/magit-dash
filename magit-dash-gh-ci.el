;;; magit-dash-gh-ci.el --- Lightweight GitHub Actions CI status for magit-dash -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (magit "4.0"))
;; Keywords: vc, tools, magit, github, ci
;; URL: https://github.com/tychoish/dot-emacs

;; This file is not part of GNU Emacs

;;; Commentary:

;; Provides lightweight CI status fetch for the magit-dash main repository
;; dashboard.  Fetches the most recent gh run list for the current branch,
;; caches the result as :ci-status in the shared magit-dash-gh cache, and
;; provides formatting, browser-open, and (stubbed) fix-CI dispatch.
;;
;; This is intentionally separate from magit-dash-gh-actions.el, which
;; downloads full CI logs interactively.  This module only fetches summary
;; data for dashboard rendering.

;;; Code:

(require 'map)
(require 'magit-dash-gh)

(declare-function magit-dash-repo-path "magit-dash")
(declare-function magit-dash-repo-name "magit-dash")
(declare-function magit-dash-repo-branch "magit-dash")
(declare-function magit-dash-repo-include-ci "magit-dash")

;;; Faces

(defface magit-dash-ci-pass-face
  '((t :inherit success))
  "Face for a passing CI run in the repository dashboard.")

(defface magit-dash-ci-fail-face
  '((t :inherit error))
  "Face for a failing CI run in the repository dashboard.")

(defface magit-dash-ci-pending-face
  '((t :inherit warning))
  "Face for an in-progress CI run in the repository dashboard.")

;;; Internal helpers

(defun magit-dash-gh-ci--failure-p (conclusion)
  "Return non-nil when CONCLUSION string indicates a failed run."
  (member conclusion '("failure" "timed_out" "startup_failure")))

(defun magit-dash-gh-ci--parse-runs (runs)
  "Return a CI status plist summarising RUNS (list of alists from gh run list).
Returns nil when RUNS is nil or empty."
  (when runs
    (let* ((latest (car runs))
           (conclusion (map-elt latest 'conclusion))
           (status (map-elt latest 'status))
           (run-id (map-elt latest 'databaseId))
           (url (map-elt latest 'url))
           (pass (seq-count (lambda (r)
                              (equal "success" (map-elt r 'conclusion)))
                            runs))
           (fail (seq-count (lambda (r)
                              (magit-dash-gh-ci--failure-p (map-elt r 'conclusion)))
                            runs)))
      (list :conclusion conclusion
            :status status
            :pass pass
            :fail fail
            :total (length runs)
            :run-id run-id
            :url url))))

;;; Display

(defun magit-dash-gh-ci--format-status (ci-status)
  "Format CI-STATUS plist as a short propertized string for the dashboard column.
Returns a shadow \"—\" when CI-STATUS is nil."
  (if (null ci-status)
      (propertize "—" 'face 'shadow)
    (let ((conclusion (plist-get ci-status :conclusion))
          (status (plist-get ci-status :status)))
      (cond
       ((equal conclusion "success")
        (propertize "✓" 'face 'magit-dash-ci-pass-face))
       ((magit-dash-gh-ci--failure-p conclusion)
        (propertize "x" 'face 'magit-dash-ci-fail-face))
       ((member status '("in_progress" "queued"))
        (propertize "⟳" 'face 'magit-dash-ci-pending-face))
       (t (propertize "—" 'face 'shadow))))))

;;; Async fetch

(defun magit-dash-gh-ci-fetch (repo callback)
  "Fetch CI status for REPO asynchronously and call CALLBACK with the result.
CALLBACK receives a CI status plist (see `magit-dash-gh-ci--parse-runs') or
nil on error.  Does nothing when REPO does not have :include-ci set.
Caches the result as :ci-status in the shared magit-dash-gh cache."
  (when (magit-dash-repo-include-ci repo)
    (let* ((path (magit-dash-repo-path repo))
           (stats (magit-dash-gh--cache-get path :stats))
           (branch (or (and stats (plist-get stats :branch))
                       (magit-dash-repo-branch repo))))
      (if (not branch)
          (funcall callback nil)
        (magit-dash-gh--run-process
         (list "run" "list"
               "--branch" branch
               "--limit" "5"
               "--json" "databaseId,name,status,conclusion,url,workflowName")
         path
         (lambda (output)
           (let* ((runs (condition-case nil
                            (json-parse-string output
                                               :array-type 'list
                                               :object-type 'alist)
                          (error nil)))
                  (ci-status (magit-dash-gh-ci--parse-runs runs)))
             (magit-dash-gh--cache-set path :ci-status ci-status)
             (funcall callback ci-status)))
         (lambda (_ _)
           (funcall callback nil)))))))

;;; Public commands

;;;###autoload
(defun magit-dash-gh-ci-open-last-run (repo)
  "Open the URL of the most recent CI run for REPO in the browser.
Does nothing when no CI status is cached for REPO."
  (when-let* ((path (magit-dash-repo-path repo))
              (ci (magit-dash-gh--cache-get path :ci-status))
              (url (plist-get ci :url)))
    (browse-url url)))

;;;###autoload
(defun magit-dash-gh-ci-fix-ci (repo)
  "Dispatch a fix-CI task for REPO.
Currently stubbed: logs the run URL for the developer to inspect.
Future: dispatch to agent-shell-queue with the failed log as context."
  (let* ((path (magit-dash-repo-path repo))
         (ci (magit-dash-gh--cache-get path :ci-status))
         (url (and ci (plist-get ci :url))))
    (message "magit-dash fix-CI: %s — run URL: %s (TODO: dispatch to ASQ)"
             (magit-dash-repo-name repo)
             (or url "no cached run"))))

(provide 'magit-dash-gh-ci)
;;; magit-dash-gh-ci.el ends here
