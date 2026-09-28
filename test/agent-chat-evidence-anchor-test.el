;;; agent-chat-evidence-anchor-test.el --- Session-start anchor refresh -*- lexical-binding: t; -*-

;; `agent-chat-emit-session-start-evidence!' runs at the end of every turn and
;; park resume.  Only the first sight of a session needs its answer at once;
;; later refreshes must not block Emacs (13 s under futon1b load, 2026-09-27).

(require 'ert)
(require 'agent-chat)

(defvar-local anchor-test-session nil)
(defvar-local anchor-test-last nil)
(defvar-local anchor-test-emitted nil)

(defun anchor-test--emit ()
  (agent-chat-emit-session-start-evidence!
   "http://example.test/api/alpha/evidence" 1 "sid-1"
   'anchor-test-session 'anchor-test-last 'anchor-test-emitted
   "test" nil))

(defun anchor-test--respond (callback status body &optional error)
  "Run CALLBACK the way url.el does, in a response buffer holding BODY."
  (with-current-buffer (generate-new-buffer " *anchor-test-response*")
    (setq-local url-http-response-status status)
    (insert "HTTP/1.1 " (number-to-string status) " OK\n\n" body)
    (funcall callback (if error (list :error error) nil))))

(defmacro anchor-test--with-stubs (&rest body)
  "Run BODY with the sync fetch, the async request and posting recorded."
  (declare (indent 0))
  `(let (sync-calls async-callbacks posts)
     (cl-letf (((symbol-function 'agent-chat-evidence-enabled-p) (lambda (_) t))
               ((symbol-function 'agent-chat-evidence-fetch-latest-id)
                (lambda (&rest _) (push t sync-calls) "e-server"))
               ((symbol-function 'futon-url-retrieve)
                (lambda (_url _timeout cb) (push cb async-callbacks) nil))
               ((symbol-function 'agent-chat-evidence-post-entry-id)
                (lambda (&rest _) (push t posts) "e-posted")))
       ,@body)))

(ert-deftest agent-chat-anchor-first-sight-is-synchronous ()
  (with-temp-buffer
    (anchor-test--with-stubs
      (anchor-test--emit)
      (should (= 1 (length sync-calls)))
      (should-not async-callbacks)
      (should (equal "e-server" anchor-test-last))
      (should-not posts))))

(ert-deftest agent-chat-anchor-later-turns-refresh-in-background ()
  (with-temp-buffer
    (anchor-test--with-stubs
      (anchor-test--emit)
      (anchor-test--emit)
      (should (= 1 (length sync-calls)))  ; the second turn does not block
      (should (= 1 (length async-callbacks)))
      (anchor-test--respond (car async-callbacks) 200
                            "{\"entries\":[{\"evidence/id\":\"e-newer\"}]}")
      (should (equal "e-newer" anchor-test-last)))))

(ert-deftest agent-chat-anchor-late-answer-does-not-undo-a-post ()
  (with-temp-buffer
    (anchor-test--with-stubs
      (anchor-test--emit)
      (anchor-test--emit)
      ;; A turn posted while the refresh was in flight moved the anchor.
      (setq anchor-test-last "e-mine")
      (anchor-test--respond (car async-callbacks) 200
                            "{\"entries\":[{\"evidence/id\":\"e-older\"}]}")
      (should (equal "e-mine" anchor-test-last)))))

(ert-deftest agent-chat-anchor-failed-refresh-keeps-the-anchor ()
  (with-temp-buffer
    (anchor-test--with-stubs
      (anchor-test--emit)
      (anchor-test--emit)
      (anchor-test--respond (car async-callbacks) 0 "" '(timeout "u"))
      (should (equal "e-server" anchor-test-last)))))

(ert-deftest agent-chat-anchor-reads-the-parsed-entries-vector ()
  ;; The parser returns arrays as vectors; the sync fetch read them with `car'
  ;; and returned nil for every session from 2026-03-10 until this test.
  (cl-letf (((symbol-function 'agent-chat-evidence-enabled-p) (lambda (_) t))
            ((symbol-function 'agent-chat-evidence-request-json)
             (lambda (&rest _)
               (list :status 200
                     :json (agent-chat--parse-json-string
                            "{\"entries\":[{\"evidence/id\":\"e-latest\"}]}")))))
    (should (equal "e-latest"
                   (agent-chat-evidence-fetch-latest-id
                    "http://example.test/api/alpha/evidence" 1 "sid-1")))))

(provide 'agent-chat-evidence-anchor-test)

;;; agent-chat-evidence-anchor-test.el ends here
