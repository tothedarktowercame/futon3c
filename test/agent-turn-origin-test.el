;;; agent-turn-origin-test.el --- P6o-2 origin checks -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'agent-chat)
(require 'claude-repl)
(require 'codex-repl)
(require 'kimi-repl)
(require 'zai-repl)
(require 'session-mode)
(defvar p6o-session nil)
(defvar p6o-last nil)

(ert-deftest p6o-turn-evidence-keeps-author-and-origin ()
  (dolist (case '((operator "joe" "typed" "operator")
                  (operator "joe" "🗣 dictated" "operator")
                  (unsolicited "continuation" "wake" "harness")
                  (unsolicited "followup" "notice" "harness")
                  (agent "claude-17" "agent text" "agent")))
    (with-temp-buffer
      (let ((agent-turn-origin-current (agent-turn-origin-decide (nth 0 case) (nth 1 case))) payload)
        (cl-letf (((symbol-function 'agent-chat-sync-evidence-anchor!) #'ignore)
                  ((symbol-function 'agent-chat-evidence-enabled-p) (lambda (&rest _) t))
                  ((symbol-function 'agent-chat-evidence-post-entry-id)
                   (lambda (_url _timeout p) (setq payload p) "p6o")))
          (agent-chat-emit-turn-evidence! "test" 1 t "p6o" "user" (nth 2 case) "agent" "test" nil 'p6o-session 'p6o-last))
        (should (equal (alist-get 'author payload) (or (getenv "USER") user-login-name "joe")))
        (should (equal (alist-get 'kind (alist-get 'origin payload)) (nth 3 case)))
        (should (equal (alist-get 'actor (alist-get 'origin payload)) (agent-turn-origin-caller)))))))

(ert-deftest p6o-all-four-repl-request-builders-use-source ()
  (dolist (sender '(claude-repl--call-claude-streaming codex-repl--call-codex-async
                   kimi-repl--call-agency-streaming zai-repl--call-agency-streaming))
    (with-temp-buffer
      (let ((agent-turn-origin-current '(:kind "harness" :actor "parked-resume")))
        (cl-letf (((symbol-function 'agent-chat-dispatch-clock-id) (lambda () nil))
                  ((symbol-function 'codex-repl--resolved-api-base) (lambda () "http://test"))
                  ((symbol-function 'codex-repl--frame-start) #'ignore)
                  ((symbol-function 'codex-repl--display-invoke-buffer) #'ignore)
                  ((symbol-function 'codex-repl--append-invoke-trace) #'ignore)
                  ((symbol-function 'codex-repl--record-invoke-timing!) #'ignore)
                  ((symbol-function 'make-process)
                   (lambda (&rest args)
                     (throw 'payload (json-parse-string
                                      (cadr (member "-d" (plist-get args :command)))
                                      :object-type 'plist)))))
          (let ((payload (catch 'payload (funcall sender "p6o" #'ignore))))
            (should (equal (plist-get payload :caller) "parked-resume"))
            (should (equal (plist-get payload :surface) "emacs-repl"))))))))

(ert-deftest p6o-queued-provenance-is-owned-by-the-turn ()
  (with-temp-buffer
    (let ((agent-turn-origin-input '(:kind "harness" :actor "parked-resume" :source-id "park-1")))
      (agent-chat--queue-turn #'ignore "agent" nil "wake" "continuation" 'unsolicited))
    (agent-chat--queue-turn #'ignore "agent" nil "typed" "joe" 'operator)
    (cl-letf (((symbol-function 'agent-chat--start-turn)
               (lambda (&rest _) (setq agent-turn-origin-current agent-turn-origin-input))))
      (agent-chat--drain-queued-operator-turns)
      (should (equal (plist-get agent-turn-origin-current :source-id) "park-1"))
      (agent-chat--drain-queued-operator-turns)
      (should (equal (plist-get agent-turn-origin-current :kind) "operator")))))

(ert-deftest p6o-session-start-is-harness ()
  (with-temp-buffer
    (let (payload)
      (cl-letf (((symbol-function 'agent-chat-sync-evidence-anchor!) #'ignore)
                ((symbol-function 'agent-chat-evidence-enabled-p) (lambda (&rest _) t))
                ((symbol-function 'agent-chat-evidence-post-entry-id)
                 (lambda (_url _timeout p) (setq payload p) "p6o")))
        (let ((p6o-session nil) (p6o-last nil))
          (agent-chat-emit-session-start-evidence! "test" 1 "p6o" 'p6o-session 'p6o-last 'p6o-last "test" nil)))
      (should (equal (alist-get 'kind (alist-get 'origin payload)) "harness")))))

(ert-deftest p6o-session-mode-human-correction-origin ()
  (let ((session-mode-turn-vocabulary nil) (session-mode-turn-corrections nil) records)
    (cl-letf (((symbol-function 'session-mode--load-live-vocabulary) #'ignore)
              ((symbol-function 'session-mode--save-live-vocabulary)
               (lambda (_rules r) (setq records r))))
      (session-mode-correct-sentences "Please do it." '("request")))
    (should (equal (alist-get 'author (car records)) "joe"))
    (should (equal (alist-get 'kind (alist-get 'origin (car records))) "operator"))))

(ert-deftest p6o-before-send-hook-sees-harness-provenance ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'agent-chat--refresh-session-turn-count) #'ignore)
              ((symbol-function 'agent-chat-start-turn-commit-window!) #'ignore)
              ((symbol-function 'agent-chat-scroll-to-bottom) #'ignore))
      (agent-chat-init-buffer (list :title "p6o" :session-id "p6o" :agent-name "agent"
                                   :agent-id "agent" :thinking-text "thinking" :thinking-prop 'p6o))
      (let (seen)
        (agent-chat-send-unsolicited-input
         (lambda (&rest _) nil) "agent" "wake" "continuation"
         (list :before-send (lambda (_) (setq seen (agent-turn-origin-caller)))))
        (should (equal seen "parked-resume"))))))

(ert-deftest p6o-first-user-record-retains-source-while-waiting ()
  (with-temp-buffer
    (setq agent-turn-origin-current '(:kind "harness" :actor "parked-resume"))
    (agent-chat-stage-pending-user-turn "wake")
    (setq agent-turn-origin-current '(:kind "operator" :actor "joe"))
    (should (equal (agent-chat-consume-pending-user-turn) "wake"))
    (should (equal (plist-get agent-turn-origin-evidence-user :kind) "harness"))
    (should (equal (agent-turn-origin-caller) "joe"))))

(ert-deftest p3-3c-turn-payload-harness-is-producer-context ()
  (dolist
      (case
       `(((:kind "operator" :actor "joe") "user"
          ((kind . "none") (basis . "producer-context") (source-ref . "p3-session")))
         ((:kind "agent" :actor "dispatcher" :delivery bell :job-id "invoke-wm"
           :job-harness ((kind . "war-machine") (basis . "producer-context")
                         (execution-id . "wm-run-7")))
          "assistant"
          ((kind . "war-machine") (basis . "producer-context")
           (execution-id . "wm-run-7")))
         ((:kind "agent" :actor "wm-full-loop" :delivery bell :job-id "invoke-plain")
          "assistant"
          ((kind . "unknown") (basis . "producer-context")
           (reason . "Agency job invoke-plain carries no harness")))
         ((:kind "agent" :actor "dispatcher" :delivery bell)
          "assistant"
          ((kind . "unknown") (basis . "producer-context")
           (reason . "bell turn has no bound Agency job id")))))
    (with-temp-buffer
      (let ((agent-turn-origin-current (nth 0 case)) payload)
        (cl-letf (((symbol-function 'agent-chat-sync-evidence-anchor!) #'ignore)
                  ((symbol-function 'agent-chat-evidence-enabled-p) (lambda (&rest _) t))
                  ((symbol-function 'agent-chat-evidence-post-entry-id)
                   (lambda (_url _timeout p) (setq payload p) "p3-3c")))
          (agent-chat-emit-turn-evidence!
           "test" 1 t "p3-session" (nth 1 case) "text" "agent" "test" nil
           'p6o-session 'p6o-last))
        (should (equal (alist-get 'harness payload) (nth 2 case)))))))

(ert-deftest p3-3c-caller-name-never-implies-war-machine ()
  (let ((stamp (agent-turn-harness-stamp
                '(:kind "agent" :actor "wm-full-loop" :delivery bell
                  :job-id "invoke-without-harness")
                "p3-session")))
    (should (equal (alist-get 'kind stamp) "unknown"))
    (should-not (equal (alist-get 'kind stamp) "war-machine"))))

(ert-deftest p3-3c-old-loaded-origin-omits-harness-safely ()
  (with-temp-buffer
    (let ((agent-turn-origin-current '(:kind "operator" :actor "joe"))
          (saved-function (symbol-function 'agent-turn-harness-stamp))
          payload)
      (unwind-protect
          (progn
            (fmakunbound 'agent-turn-harness-stamp)
            (cl-letf (((symbol-function 'agent-chat-sync-evidence-anchor!) #'ignore)
                      ((symbol-function 'agent-chat-evidence-enabled-p)
                       (lambda (&rest _) t))
                      ((symbol-function 'agent-chat-evidence-post-entry-id)
                       (lambda (_url _timeout p) (setq payload p) "p3-3c-old")))
              (agent-chat-emit-turn-evidence!
               "test" 1 t "p3-session" "user" "text" "agent" "test" nil
               'p6o-session 'p6o-last))
            (should-not (assq 'harness payload)))
        (fset 'agent-turn-harness-stamp saved-function)))))

(defmacro p12-5-1c-with-segment-capture (&rest body)
  "Run BODY and return the emitted assistant segment payloads."
  (declare (indent 1))
  `(let ((agent-chat--session-id "sid")
         (agent-chat--unified-turn-id nil)
         (agent-chat--segment-index 0)
         (agent-chat--unified-segments nil)
         (agent-chat--last-assistant-text "")
         (claude-repl-evidence-log-turns t)
         (claude-repl-evidence-url "test")
         (claude-repl-agent-id "claude-17")
         payloads)
     (cl-letf (((symbol-function 'agent-chat-sync-evidence-anchor!) #'ignore)
               ((symbol-function 'agent-chat-evidence-enabled-p)
                (lambda (&rest _) t))
               ((symbol-function 'agent-chat-note-turn-recorded) #'ignore)
               ((symbol-function 'agent-chat-evidence-post-entry-id)
                (lambda (_url _timeout payload)
                  (push payload payloads)
                  (alist-get 'id payload))))
       ,@body)
     (nreverse payloads)))

(ert-deftest p12-5-1c-two-segments-emit-linked-rows ()
  (with-temp-buffer
    (let ((payloads
           (p12-5-1c-with-segment-capture
             (setq agent-turn-origin-current '(:kind "operator" :actor "joe"))
             (claude-repl--emit-assistant-segment-evidence! "first" nil)
             (setq agent-turn-origin-current
                   '(:kind "harness" :actor "parked-resume" :source-id "park-2"))
             (claude-repl--emit-assistant-segment-evidence! "second" t))))
    (should (= 2 (length payloads)))
    (let* ((first (car payloads))
           (second (cadr payloads))
           (first-body (alist-get 'body first))
           (second-body (alist-get 'body second))
           (unified-id (alist-get 'unified-turn-id first-body)))
      (should (equal unified-id (alist-get 'id first)))
      (should (equal unified-id (alist-get 'unified-turn-id second-body)))
      (should (= 0 (alist-get 'segment-index first-body)))
      (should (= 1 (alist-get 'segment-index second-body)))
      (should (eq :json-false (alist-get 'segment-final first-body)))
      (should (eq t (alist-get 'segment-final second-body)))
      (should (equal (alist-get 'id first) (alist-get 'in-reply-to second)))
      (should (equal "park-2"
                     (alist-get 'source-ref (alist-get 'harness second))))
      ;; Whole-turn consumers receive the joined form on the final row.
      (should (equal "first\n\nsecond"
                     (alist-get 'unified-text second-body)))))))

(ert-deftest p12-5-1c-single-segment-names-itself ()
  (with-temp-buffer
    (let ((payloads
           (p12-5-1c-with-segment-capture
             (setq agent-turn-origin-current '(:kind "operator" :actor "joe"))
             (claude-repl--emit-assistant-segment-evidence! "only" t))))
    (let* ((payload (car payloads))
           (body (alist-get 'body payload)))
      (should (= 1 (length payloads)))
      (should (equal (alist-get 'id payload) (alist-get 'unified-turn-id body)))
      (should (= 0 (alist-get 'segment-index body)))
      (should (eq t (alist-get 'segment-final body)))))))

(ert-deftest p12-5-1c-parked-segment-survives-without-final-segment ()
  (with-temp-buffer
    (let ((payloads
           (p12-5-1c-with-segment-capture
             (setq agent-turn-origin-current
                   '(:kind "harness" :actor "parked-resume" :source-id "park-crash"))
             ;; Simulate the process ending here: no final segment is run.
             (claude-repl--emit-assistant-segment-evidence!
              "durable before crash" nil))))
    (should (= 1 (length payloads)))
    (let ((body (alist-get 'body (car payloads))))
      (should (equal "durable before crash" (alist-get 'text body)))
      (should (eq :json-false (alist-get 'segment-final body)))
      (should (equal "park-crash"
                     (alist-get 'source-ref
                                (alist-get 'harness (car payloads)))))))))

(ert-deftest p12-5-1c-operator-turn-starts-new-unified-turn ()
  (with-temp-buffer
    (let ((payloads
           (p12-5-1c-with-segment-capture
             (setq agent-turn-origin-current
                   '(:kind "harness" :actor "parked-resume" :source-id "park-1"))
             (claude-repl--emit-assistant-segment-evidence! "chain" nil)
             (setq agent-turn-origin-current '(:kind "operator" :actor "joe"))
             (claude-repl--emit-user-turn-evidence! "joe speaks")
             (claude-repl--emit-assistant-segment-evidence! "reply" nil))))
      (let* ((rows (seq-filter (lambda (p) (equal "assistant"
                                                 (alist-get 'role (alist-get 'body p))))
                               payloads))
             (chain (alist-get 'body (car rows)))
             (reply (alist-get 'body (cadr rows))))
        (should (= 2 (length rows)))
        (should (= 0 (alist-get 'segment-index reply)))
        (should (equal (alist-get 'id (cadr rows))
                       (alist-get 'unified-turn-id reply)))
        (should-not (equal (alist-get 'unified-turn-id chain)
                           (alist-get 'unified-turn-id reply)))))))

(ert-deftest p12-5-1c-harness-turn-keeps-unified-turn ()
  (with-temp-buffer
    (let ((payloads
           (p12-5-1c-with-segment-capture
             (setq agent-turn-origin-current
                   '(:kind "harness" :actor "parked-resume" :source-id "park-1"))
             (claude-repl--emit-assistant-segment-evidence! "one" nil)
             (claude-repl--emit-user-turn-evidence! "wake")
             (claude-repl--emit-assistant-segment-evidence! "two" nil))))
      (let ((rows (seq-filter (lambda (p) (equal "assistant"
                                                (alist-get 'role (alist-get 'body p))))
                              payloads)))
        (should (= 1 (alist-get 'segment-index (alist-get 'body (cadr rows)))))))))

(ert-deftest p12-5-1c-segment-final-false-encodes-as-json-false ()
  (should (equal "{\"segment-final\":false}"
                 (json-encode (agent-chat--json-encodable
                               '((segment-final . :json-false)))))))
