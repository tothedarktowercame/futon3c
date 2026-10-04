;;; session-turn-analysis-jvm-test.el --- ERT tests for the jvm recorder -*- lexical-binding: t; -*-

;;; Commentary:
;; Fixture-only tests for M-象-2000 step 1: with `session-mode-turn-recorder'
;; set to `jvm', a turn is recorded through the futon3c JVM's
;; /api/alpha/xiang routes, the reply is POSTed as happened, and Emacs polls
;; until the reading lands.  The HTTP layer (`session-mode--xiang-request')
;; is stubbed; no JVM runs.  Run:
;;   emacs -Q --batch -L emacs -l emacs/test/session-turn-analysis-jvm-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'session-turn-analysis)

(defvar agent-chat--agent-id)
(defvar agent-chat--session-id)
(defvar agent-chat--current-turn-id)
(defvar agent-chat--last-evidence-id)
(defvar session-mode-turn-vocabulary-version)

(defun session-turn-analysis-jvm-test--record (policy)
  "Record one turn with POLICY; return (PATH CALLS . POLLS) the stub saw."
  (let ((calls nil) (polls nil)
        (session-mode--jvm-turn-state (make-hash-table :test #'equal)))
    (cl-letf ((session-mode-turn-recorder 'jvm)
              (session-mode-turn-analysis-policy policy)
              (agent-chat--agent-id "claude-17")
              (agent-chat--session-id "sess-1")
              (agent-chat--current-turn-id "claude-17-turn-3")
              (agent-chat--last-evidence-id "emacs-abc")
              ((symbol-function 'run-at-time)
               (lambda (&rest args) (push args polls) nil))
              ((symbol-function 'session-mode--structure-turn)
               (lambda (text) `((source_text . ,text) (unmatched . nil))))
              ((symbol-function 'agent-chat-split-surface-marker)
               (lambda (text) (cons nil text)))
              ((symbol-function 'session-mode--redact-secrets)
               (lambda (text) (cons text nil)))
              ((symbol-function 'session-mode--xiang-turn-policy)
               (lambda (&rest _) "ask"))
              ((symbol-function 'session-mode--xiang-request)
               (lambda (method api-path &optional payload _callback)
                 (push (list method api-path payload) calls)
                 '((ok . t) (id . "turn-abc123") (path . "/tmp/turn-abc123.json")))))
      (list (session-mode--record-turn "Please continue.  Then stop." nil nil)
            (nreverse calls)
            (nreverse polls)))))

(ert-deftest session-turn-analysis-jvm-record-posts-turn-with-policy ()
  "Under `jvm', recording POSTs the turn and the policy's analysis-requested."
  (let* ((result (session-turn-analysis-jvm-test--record 'all))
         (path (nth 0 result))
         (calls (nth 1 result))
         (polls (nth 2 result))
         (payload (nth 2 (car calls))))
    (should (equal path "/tmp/turn-abc123.json"))
    (should (equal (length calls) 1))
    (should (equal (car (nth 0 calls)) "POST"))
    (should (equal (nth 1 (car calls)) "/api/alpha/xiang/turns"))
    (should (eq t (alist-get 'analysis-requested payload)))
    (should (equal "soon" (alist-get 'dispatch payload)))
    (should (equal 1 (length polls)))
    (should (equal (alist-get 'agent-id payload) "claude-17"))
    (should (equal (alist-get 'session-id payload) "sess-1"))
    (should (equal (alist-get 'turn-id payload) "claude-17-turn-3"))
    (should (equal (alist-get 'evidence-id payload) "emacs-abc"))
    (should (equal (alist-get 'text payload) "Please continue.  Then stop."))))

(ert-deftest session-turn-analysis-jvm-record-policy-never-sends-false ()
  "The bad case the flag is named for: policy `never' (象-off) POSTs
analysis-requested false, so the JVM record comes back not-requested and
no seat may be belled for it."
  (let* ((result (session-turn-analysis-jvm-test--record 'never))
         (payload (nth 2 (car (nth 1 result)))))
    (should (eq :json-false (alist-get 'analysis-requested payload)))
    (should-not (assq 'dispatch payload))
    (should (equal 0 (length (nth 2 result)))))) ; a dark turn is never polled

(ert-deftest session-turn-analysis-jvm-record-refusal-signals ()
  "A refused record signals, so the caller warns instead of losing it silently."
  (cl-letf ((session-mode-turn-recorder 'jvm)
            (session-mode-turn-analysis-policy 'all)
            (agent-chat--agent-id "claude-17")
            (agent-chat--session-id "sess-1")
            (agent-chat--current-turn-id "claude-17-turn-3")
            (agent-chat--last-evidence-id nil)
            ((symbol-function 'session-mode--redact-secrets)
             (lambda (text) (cons text nil)))
            ((symbol-function 'session-mode--xiang-turn-policy)
             (lambda (&rest _) "ask"))
            ((symbol-function 'session-mode--xiang-request)
             (lambda (&rest _) '((ok . :json-false) (reason . "missing-field")))))
    (should-error (session-mode--record-turn "Please continue." nil nil)
                  :type 'error)))

(defun session-turn-analysis-jvm-test--after-reply (poll-answers)
  "Run `session-mode--dispatch-analysis-after-reply' under the `jvm' recorder.
POLL-ANSWERS is a list of GET answers, one per poll.  Returns (CALLS . LANDED):
the (METHOD API-PATH PAYLOAD) calls in order and the landed-hook paths."
  (let ((calls nil) (landed nil) (answers poll-answers)
        (session-mode--jvm-turn-state (make-hash-table :test #'equal)))
    (cl-letf ((session-mode-turn-recorder 'jvm)
              (session-mode-analysis-agent "象")
              (agent-chat--agent-id "claude-17")
              (session-mode-analysis-landed-functions
               (list (lambda (p) (push p landed))))
              ((symbol-function 'session-mode--record-requests-analysis-p)
               (lambda (_path) t))
              ((symbol-function 'session-mode--turn-commits-snapshot)
               (lambda ()
                 (list '((repo . "futon3c") (repo-path . "/x/futon3c")
                         (sha . "abc123def456") (subject . "fix a thing")))))
              ((symbol-function 'run-at-time)
               (lambda (_delay _repeat fn &rest args) (apply fn args)))
              ((symbol-function 'session-mode--set-analysis-health)
               (lambda (&rest _) nil))
              ((symbol-function 'session-mode--xiang-request)
               (lambda (method api-path &optional payload callback)
                 (push (list method api-path payload) calls)
                 (if callback
                     (funcall callback (or (pop answers)
                                           '((ok . t)
                                             (record . ((analysis_status . "requested"))))))
                   '((ok . t) (dispatched . t))))))
      (session-mode--dispatch-analysis-after-reply "/tmp/turn-abc123.json" "REPLY text"))
    (cons (nreverse calls) (nreverse landed))))

(ert-deftest session-turn-analysis-jvm-happened-poll-lands-hook ()
  "record → happened → poll → the landed hook runs once with the record path."
  (let* ((result (session-turn-analysis-jvm-test--after-reply
                  (list '((ok . t) (record . ((analysis_status . "requested"))))
                        '((ok . t) (record . ((analysis_status . "analyzed")))))))
         (calls (car result))
         (landed (cdr result))
         (happened (car calls))
         (payload (nth 2 happened)))
    (should (equal landed '("/tmp/turn-abc123.json")))
    (should (equal (nth 0 happened) "POST"))
    (should (equal (nth 1 happened) "/api/alpha/xiang/turns/turn-abc123/happened"))
    (should (equal (alist-get 'reply payload) "REPLY text"))
    (let ((commit (car (append (alist-get 'commits payload) nil))))
      (should (equal (alist-get 'repo commit) "futon3c"))
      (should (equal (alist-get 'sha commit) "abc123def456")))
    (should (equal (mapcar #'car (cdr calls))
                   '("GET" "GET")))
    (should (equal (nth 1 (cadr calls)) "/api/alpha/xiang/turns/turn-abc123"))))

(ert-deftest session-turn-analysis-jvm-refused-reading-goes-failing ()
  "A refused reading settles the poll with a failing lighter and no hook."
  (let* ((landed-hook-ran nil)
         (health nil))
    (cl-letf ((session-mode-turn-recorder 'jvm)
              (session-mode-analysis-agent "象")
              (agent-chat--agent-id "claude-17")
              (session-mode-analysis-landed-functions
               (list (lambda (_p) (setq landed-hook-ran t))))
              ((symbol-function 'session-mode--record-requests-analysis-p)
               (lambda (_path) t))
              ((symbol-function 'session-mode--turn-commits-snapshot)
               (lambda () nil))
              ((symbol-function 'run-at-time)
               (lambda (_d _r fn &rest args) (apply fn args)))
              ((symbol-function 'session-mode--set-analysis-health)
               (lambda (h detail) (push (list h detail) health)))
              ((symbol-function 'display-warning) (lambda (&rest _) nil))
              ((symbol-function 'session-mode--xiang-request)
               (lambda (method _path &optional _payload callback)
                 (if callback
                     (funcall callback '((ok . t) (record . ((analysis_status . "refused")))))
                   '((ok . t))))))
      (session-mode--dispatch-analysis-after-reply "/tmp/turn-abc123.json" "REPLY"))
    (should-not landed-hook-ran)
    (should (assq 'failing health))))

(ert-deftest session-turn-analysis-jvm-landed-hook-fires-once ()
  "The reading may land before the reply ends: the hook runs at send-time
polling, and happened arriving afterwards never re-runs it."
  (let ((calls nil) (landed nil)
        (session-mode--jvm-turn-state (make-hash-table :test #'equal)))
    (cl-letf ((session-mode-turn-recorder 'jvm)
              (session-mode-turn-analysis-policy 'all)
              (session-mode-analysis-agent "象")
              (agent-chat--agent-id "claude-17")
              (agent-chat--session-id "sess-1")
              (agent-chat--current-turn-id "claude-17-turn-3")
              (agent-chat--last-evidence-id "emacs-abc")
              (session-mode-analysis-landed-functions
               (list (lambda (p) (push p landed))))
              ((symbol-function 'run-at-time)
               (lambda (_d _r fn &rest args) (apply fn args)))
              ((symbol-function 'session-mode--structure-turn)
               (lambda (text) `((source_text . ,text) (unmatched . nil))))
              ((symbol-function 'agent-chat-split-surface-marker)
               (lambda (text) (cons nil text)))
              ((symbol-function 'session-mode--redact-secrets)
               (lambda (text) (cons text nil)))
              ((symbol-function 'session-mode--xiang-turn-policy)
               (lambda (&rest _) "ask"))
              ((symbol-function 'session-mode--record-requests-analysis-p)
               (lambda (_path) t))
              ((symbol-function 'session-mode--turn-commits-snapshot)
               (lambda () nil))
              ((symbol-function 'session-mode--set-analysis-health)
               (lambda (&rest _) nil))
              ((symbol-function 'session-mode--xiang-request)
               (lambda (method api-path &optional payload callback)
                 (push (list method api-path payload) calls)
                 (if callback
                     (funcall callback '((ok . t) (record . ((analysis_status . "analyzed")))))
                   '((ok . t) (id . "turn-abc123") (path . "/tmp/turn-abc123.json"))))))
      ;; Send: record, then the send-time poll finds the reading already done.
      (let ((path (session-mode--record-turn "Please continue." nil nil)))
        (should (equal landed (list path)))
        ;; Reply ends: happened is POSTed, but nothing polls or lands again.
        (session-mode--dispatch-analysis-after-reply path "REPLY text")
        (should (equal landed (list path)))
        (setq calls (nreverse calls))
        (should (equal (mapcar #'car calls) '("POST" "GET" "POST")))
        (should (equal (nth 1 (nth 2 calls))
                       "/api/alpha/xiang/turns/turn-abc123/happened"))))))

(ert-deftest session-turn-analysis-jvm-not-requested-never-posts-happened ()
  "象-off: a not-requested record never POSTs happened, never polls, never
bothers the JVM at reply end."
  (cl-letf ((session-mode-turn-recorder 'jvm)
            (session-mode-analysis-agent "象")
            (agent-chat--agent-id "claude-17")
            ((symbol-function 'session-mode--record-requests-analysis-p)
             (lambda (_path) nil))
            ((symbol-function 'session-mode--xiang-request)
             (lambda (&rest _) (error "must not be called"))))
    (session-mode--dispatch-analysis-after-reply "/tmp/turn-abc123.json" "REPLY")
    (should t)))

(ert-deftest session-turn-analysis-files-recorder-is-untouched ()
  "The default `files' recorder still writes the file itself, no HTTP."
  (let* ((dir (make-temp-file "sta-jvm-test-" t))
         (session-mode-turn-recorder 'files)
         (session-mode-turn-analysis-directory dir))
    (unwind-protect
        (cl-letf ((session-mode-turn-analysis-policy 'never)
                  (session-mode-turn-vocabulary-version "test")
                  (agent-chat--agent-id "claude-17")
                  (agent-chat--session-id "sess-1")
                  (agent-chat--current-turn-id "claude-17-turn-3")
                  (agent-chat--last-evidence-id "emacs-abc")
                  ((symbol-function 'session-mode--structure-turn)
               (lambda (text) `((source_text . ,text) (unmatched . nil))))
              ((symbol-function 'agent-chat-split-surface-marker)
               (lambda (text) (cons nil text)))
              ((symbol-function 'session-mode--redact-secrets)
                   (lambda (text) (cons text nil)))
                  ((symbol-function 'session-mode--xiang-request)
                   (lambda (&rest _) (error "must not be called"))))
          (let ((path (session-mode--record-turn "Please continue." nil nil)))
            (should (file-exists-p path))
            (should (equal (file-name-directory path)
                           (file-name-as-directory (expand-file-name dir))))
            (let ((json-object-type 'alist))
              (should (equal "not-requested"
                             (alist-get 'analysis_status (json-read-file path)))))))
      (delete-directory dir t))))

(ert-deftest session-turn-analysis-jvm-parse-real-response ()
  "The parser reads a raw http-kit response, unstubbed: LF and CRLF header
ends, and a UTF-8 body arriving as bytes.  The stubbed tests above never
reached it, and a regexp that could not match a blank line made every
answer nil (2026-10-04)."
  (dolist (eol '("\n" "\r\n"))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert (concat "HTTP/1.1 201 Created" eol "Content-Type: application/json" eol eol)
              (encode-coding-string "{\"ok\":true,\"seat\":\"象-1\",\"path\":\"/tmp/turn-x.json\"}" 'utf-8))
      (let ((answer (session-mode--xiang-parse-buffer)))
        (should (equal "/tmp/turn-x.json" (alist-get 'path answer)))
        (should (equal "象-1" (alist-get 'seat answer)))))))

(provide 'session-turn-analysis-jvm-test)
;;; session-turn-analysis-jvm-test.el ends here
