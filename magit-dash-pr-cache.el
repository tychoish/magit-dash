;;; magit-dash-pr-cache.el --- Persistent per-PR state cache for magit-dash -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (sprite "0.1"))
;; Keywords: vc, tools, github
;; URL: https://github.com/tychoish/dot-emacs

;; This file is not part of GNU Emacs

;;; Commentary:

;; Stores one plist per pull request at
;; <state>/magit-dash-pr/<owner>/<repo>/<number>.eld, where <state> comes
;; from `sprite-state-path'.  Writes are atomic (temp file, then rename)
;; and touch exactly one file.  Records older than
;; `magit-dash-pr-cache-stale-minutes' are considered stale: callers dim
;; them with `magit-dash-stale' but never expire them.  An unreadable or
;; unwritable file is skipped with a logged message and never signals.

;;; Code:

(require 'map)
(require 'seq)
(require 'subr-x)
(require 'sprite)

(defconst magit-dash-pr-cache-version 1
  "Schema version stamped into every cache record.")

(defconst magit-dash-pr-cache-minimum-stale-minutes 10
  "Lower bound for `magit-dash-pr-cache-stale-minutes'.")

(defun magit-dash-pr-cache--set-stale-minutes (symbol value)
  "Set SYMBOL to VALUE, clamping anything below the minimum.
A non-number VALUE falls back to 30 with a message."
  (cond
   ((numberp value)
    (set-default symbol (max magit-dash-pr-cache-minimum-stale-minutes value)))
   (t
    (message "magit-dash-pr-cache: %S is not a number, using 30" value)
    (set-default symbol 30))))

(defcustom magit-dash-pr-cache-stale-minutes 30
  "Minutes after which a cached PR record is considered stale.
Values below 10 are clamped to 10."
  :type 'number
  :set #'magit-dash-pr-cache--set-stale-minutes
  :group 'magit-dash)

(defface magit-dash-stale
  '((t :inherit shadow))
  "Face for rows whose cached data is older than the stale threshold."
  :group 'magit-dash)

(defun magit-dash-pr-cache-stale-seconds ()
  "Return the stale threshold in seconds."
  (* 60 (max magit-dash-pr-cache-minimum-stale-minutes
             (if (numberp magit-dash-pr-cache-stale-minutes)
                 magit-dash-pr-cache-stale-minutes
               30))))

(defun magit-dash-pr-cache-stale-p (record &optional now)
  "Return non-nil when RECORD is older than the stale threshold at NOW.
NOW defaults to the current `float-time'.  A record without
`:fetched-at' is stale."
  (let ((fetched (plist-get record :fetched-at)))
    (or (not (numberp fetched))
        (> (- (or now (float-time)) fetched)
           (magit-dash-pr-cache-stale-seconds)))))

(defun magit-dash-pr-cache--root ()
  "Return the cache root directory."
  (file-name-as-directory (sprite-state-path "magit-dash-pr/")))

(defun magit-dash-pr-cache--repo-dir (repo)
  "Return the directory holding REPO's records, or nil when REPO is malformed.
REPO must be OWNER/NAME of letters, digits, underscore, dot and dash,
with no \".\" or \"..\" component."
  (when (and (stringp repo)
             (string-match-p "\\`[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+\\'" repo)
             (not (seq-some (lambda (part) (member part '("." "..")))
                            (split-string repo "/"))))
    (file-name-as-directory (expand-file-name repo (magit-dash-pr-cache--root)))))

(defun magit-dash-pr-cache--file (repo number)
  "Return the file for REPO and NUMBER, or nil when either is malformed."
  (when-let* (((integerp number))
              (dir (magit-dash-pr-cache--repo-dir repo)))
    (expand-file-name (format "%s.eld" number) dir)))

(defun magit-dash-pr-cache--read-file (file)
  "Return the record stored in FILE, or nil with a message when unreadable."
  (condition-case err
      (let ((record (with-temp-buffer
                      (insert-file-contents file)
                      (let ((read-eval nil))
                        (read (current-buffer))))))
        (cond
         ((and (listp record) (plist-get record :number) (plist-get record :repo))
          record)
         (t
          (message "magit-dash-pr-cache: skipping malformed %s" file)
          nil)))
    (error
     (message "magit-dash-pr-cache: skipping unreadable %s: %s"
              file (error-message-string err))
     nil)))

(defun magit-dash-pr-cache-get (repo number)
  "Return the cached record for REPO and NUMBER, or nil."
  (when-let* ((file (magit-dash-pr-cache--file repo number))
              ((file-readable-p file)))
    (magit-dash-pr-cache--read-file file)))

(defun magit-dash-pr-cache-list (&optional repo)
  "Return the cached records for REPO, or for every repo when REPO is nil.
Never signals; an unscannable directory yields nil."
  (let ((files (condition-case err
                   (if repo
                       (when-let* ((dir (magit-dash-pr-cache--repo-dir repo))
                                   ((file-directory-p dir)))
                         (directory-files dir t "\\.eld\\'"))
                     (when (file-directory-p (magit-dash-pr-cache--root))
                       (directory-files-recursively
                        (magit-dash-pr-cache--root) "\\.eld\\'")))
                 (error
                  (message "magit-dash-pr-cache: could not scan cache: %s"
                           (error-message-string err))
                  nil))))
    (seq-keep #'magit-dash-pr-cache--read-file files)))

(defun magit-dash-pr-cache--write (file record)
  "Atomically write RECORD to FILE."
  (let ((dir (file-name-directory file)))
    (make-directory dir t)
    (let ((tmp (make-temp-file (expand-file-name ".pr-" dir) nil ".tmp")))
      (condition-case err
          (progn
            (let ((print-length nil)
                  (print-level nil)
                  (print-circle nil))
              (with-temp-file tmp
                (prin1 record (current-buffer))
                (insert "\n")))
            (rename-file tmp file t))
        (error
         (ignore-errors (delete-file tmp))
         (signal (car err) (cdr err)))))))

(defun magit-dash-pr-cache-merge-payload (fresh old)
  "Return FRESH alist extended with the keys of OLD that FRESH lacks.
Keys are deduplicated by `car'; the first (fresh) value always wins, so
repeated merges never grow the alist."
  (let ((unique (seq-uniq fresh (lambda (a b) (equal (car a) (car b))))))
    (append unique
            (seq-uniq (seq-remove (lambda (cell) (assoc (car cell) unique)) old)
                      (lambda (a b) (equal (car a) (car b)))))))

(defun magit-dash-pr-cache-put (repo number payload &optional status prior)
  "Store PAYLOAD (a `gh pr view' alist) for REPO and NUMBER.
Keep the existing `:status' when STATUS is nil.  PRIOR, when non-nil, is
the already-read previous record and saves a second read from disk.
Touch exactly one file.  Return the record, or nil when it could not be
written."
  (condition-case err
      (when-let* ((file (magit-dash-pr-cache--file repo number)))
        (let ((record (list :version magit-dash-pr-cache-version
                            :repo repo
                            :number number
                            :fetched-at (float-time)
                            :updated-at (map-elt payload 'updatedAt)
                            :head-sha (map-elt payload 'headRefOid)
                            :payload payload
                            :status (or status
                                        (plist-get (or prior (magit-dash-pr-cache-get repo number))
                                                   :status)))))
          (magit-dash-pr-cache--write file record)
          record))
    (error
     (message "magit-dash-pr-cache: could not write %s#%s: %s"
              repo number (error-message-string err))
     nil)))

(provide 'magit-dash-pr-cache)
;;; magit-dash-pr-cache.el ends here
