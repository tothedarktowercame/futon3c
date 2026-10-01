;;; agent-chat-test.el --- Tests for shared chat buffer behavior -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'agent-chat)

(ert-deftest agent-chat-cost-segment-renders-warm-claude ()
  (with-temp-buffer
    (setq agent-chat--cost-basis
          '(:vendor "claude" :model "claude-fable-5" :mult 2.0
            :ctx 312000 :warm_s 240 :cold nil :per_turn_usd 0.62
            :per_turn_cold_usd 6.24 :session_usd 41.2 :turns 130))
    (should (equal (agent-chat-cost-segment)
                   "fable x2 · ctx 312k · warm 4m · ~$0.62/turn · $41 / 130t"))))

(ert-deftest agent-chat-cost-segment-renders-cold-claude ()
  (with-temp-buffer
    (setq agent-chat--cost-basis
          '(:vendor "claude" :model "claude-opus-5" :mult 1.0
            :ctx 700000 :warm_s 244800 :cold t :per_turn_usd 0.7
            :per_turn_cold_usd 7.41))
    (should (equal (agent-chat-cost-segment)
                   "opus x1 · ctx 700k · cold 68h · next ~$7.41"))))

(ert-deftest agent-chat-cost-segment-renders-codex-without-dollars ()
  (with-temp-buffer
    (setq agent-chat--cost-basis
          '(:vendor "codex" :model "gpt-5" :ctx 258000 :warm_s 120 :cold nil))
    (should (equal (agent-chat-cost-segment) "ctx 258k · warm 2m"))))

(ert-deftest agent-chat-cost-flair-suffix-renders-last-turn ()
  (should (equal (agent-chat-cost-flair-suffix
                  '(:vendor "claude" :model "claude-fable-5" :mult 2.0 :ctx 105076
                    :last_turn_usd 1.450742 :last_turn_calls 7 :session_usd 7.68))
                 " · ~$1.45 (7 calls · ctx 105k · fable x2) · session $8"))
  (should (equal (agent-chat-cost-flair-suffix
                  '(:vendor "claude" :model "claude-opus-5" :mult 1.0 :ctx 411000
                    :last_turn_usd 5.14 :last_turn_calls 22 :session_usd 293))
                 " · ~$5.14 (22 calls · ctx 411k · opus x1) · session $293 →  Compaction recommended (C-c C-z)"))
  (should (equal (agent-chat-cost-flair-suffix
                  '(:vendor "claude" :last_turn_usd 0.5 :last_turn_calls 1 :pouch "none"))
                 " · ~$0.50 (1 call) · no pouch"))
  (should (equal (agent-chat-cost-flair-suffix
                  '(:vendor "claude" :last_turn_usd 0.5 :last_turn_calls 1 :pouch "warm"))
                 " · ~$0.50 (1 call) · pouch"))
  (should (equal (agent-chat-cost-flair-suffix '(:vendor "codex" :ctx 1000)) ""))
  (should (equal (agent-chat-cost-flair-suffix nil) "")))

(ert-deftest agent-chat-annotate-turn-flair-is-idempotent ()
  (with-temp-buffer
    (insert "hello\nCooked for 1m 03s\n─── *no mission*\n> ")
    (setq-local agent-chat--prompt-marker (copy-marker (- (point-max) 2)))
    (setq-local agent-chat--cost-basis
                '(:vendor "claude" :last_turn_usd 0.5 :last_turn_calls 1))
    (agent-chat--annotate-turn-flair!)
    (agent-chat--annotate-turn-flair!)
    (should (string-match-p "^Cooked for 1m 03s · ~\\$0\\.50 (1 call)$"
                            (buffer-substring (point-min) (point-max))))))

(ert-deftest agent-chat-annotate-turn-flair-ignores-transcript-cooked-lines ()
  (with-temp-buffer
    (insert "Cooked for 9s\nsome transcript\n> ")
    (setq-local agent-chat--prompt-marker (copy-marker (- (point-max) 2)))
    (setq-local agent-chat--cost-basis
                '(:vendor "claude" :last_turn_usd 0.5 :last_turn_calls 1))
    (agent-chat--annotate-turn-flair!)
    (should (equal (buffer-string) "Cooked for 9s\nsome transcript\n> "))))

(ert-deftest agent-chat-ensure-prompt-markers-ignores-blockquote ()
  (with-temp-buffer
    (insert "claude: quoting\n> `:name \"x\"`\n\nmore text\nCooked for 6m 15s\n")
    (setq-local agent-chat--prompt-marker (copy-marker (point-max) t))
    (setq-local agent-chat--separator-start nil)
    (setq-local agent-chat--input-start nil)
    (should (agent-chat--ensure-prompt-markers!))
    (should (= (marker-position agent-chat--prompt-marker) (- (point-max) 2)))
    (should (string-suffix-p "Cooked for 6m 15s\n> " (buffer-string)))
    (should (= (marker-position agent-chat--input-start) (point-max)))))

(ert-deftest agent-chat-prefixed-prompt-is-wholly-read-only ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'agent-chat--fetch-prompt-line)
               (lambda () "$~x/y> ")))
      (agent-chat-test--init-buffer))
    (should (string-suffix-p "$~x/y> " (buffer-string)))
    (let ((start (save-excursion
                   (goto-char (marker-position agent-chat--input-start))
                   (line-beginning-position)))
          (end (marker-position agent-chat--input-start)))
      (should (equal "$~x/y> "
                     (buffer-substring-no-properties start end)))
      (dotimes (offset (- end start))
        (should (get-text-property (+ start offset) 'read-only)))
      (should-not (get-text-property end 'read-only)))))

(ert-deftest agent-chat-repairs-displaced-prompt-space-after-quoted-input ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'agent-chat--fetch-prompt-line)
               (lambda () "$~devmap-coherence/baseline-freeze> ")))
      (agent-chat-test--init-buffer))
    (insert "I think it's been \"about an hour\"")
    ;; Reproduce the live claude-4 corruption: a prompt-propertized space has
    ;; been displaced to the end of otherwise ordinary pending input.
    (let ((start (point)))
      (let ((inhibit-read-only t)) (insert " "))
      (add-text-properties
       start (point)
       '(read-only "Agent REPL prompt is read-only"
         rear-nonsticky (face read-only))))
    (agent-chat--repair-input-properties!)
    (should (equal "I think it's been \"about an hour\" "
                   (buffer-substring-no-properties
                    agent-chat--input-start (point-max))))
    (should-not (text-property-not-all agent-chat--input-start (point-max)
                                       'read-only nil))
    (should (get-text-property (1- (marker-position agent-chat--input-start))
                               'read-only))))

(ert-deftest agent-chat-send-repairs-prompt-property-after-triple-greater-text ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'agent-chat--fetch-prompt-line)
               (lambda () "$~contracts/holder-states-the-claim> ")))
      (agent-chat-test--init-buffer))
    (insert "quoted block >>> Let me see if I can trigger it")
    (add-text-properties
     (- (point-max) 2) (point-max)
     '(read-only "Agent REPL prompt is read-only"
       rear-nonsticky (face read-only)))
    (let (sent-text)
      (cl-letf (((symbol-function 'agent-chat-start-turn-commit-window!)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-chat-finish-turn-commits)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-chat-scroll-to-bottom)
                 (lambda (&rest _) nil))
                ((symbol-function 'redisplay) (lambda (&rest _) nil))
                ((symbol-function 'url-retrieve) (lambda (&rest _) nil)))
        (agent-chat-send-input
         (lambda (text _callback) (setq sent-text text) nil) "agent"))
      (should (equal "quoted block >>> Let me see if I can trigger it"
                     sent-text)))))

(ert-deftest agent-chat-large-backward-kill-preserves-prompt-wall ()
  (dolist (rendered '(nil "$~x/y> "))
    (with-temp-buffer
      (cl-letf (((symbol-function 'agent-chat--fetch-prompt-line)
                 (lambda () rendered)))
        (agent-chat-test--init-buffer))
      (insert "abc")
      (let* ((prompt-start (marker-position agent-chat--prompt-marker))
             (input-start (marker-position agent-chat--input-start))
             (prompt (buffer-substring-no-properties prompt-start input-start)))
        (condition-case nil
            (kill-word -12)
          (text-read-only nil)
          (beginning-of-buffer nil))
        (should (equal prompt
                       (buffer-substring-no-properties prompt-start input-start)))))))

(ert-deftest agent-chat-prompt-fetch-failure-falls-back-exactly ()
  (dolist (failure '(:error :timeout))
    (with-temp-buffer
      (cl-letf (((symbol-function 'agent-chat--fetch-prompt-line)
                 (lambda ()
                   (if (eq failure :error)
                       (error "unavailable")
                     (signal 'error '("timed out"))))))
        (agent-chat--insert-prompt))
      (should (equal "> " (buffer-string))))))

(defun agent-chat-test--offer-payload (agent session offer-id)
  `((prompt . "$!> ")
    (segments
     . (((segment/id . "offer")
         (segment/basis . ((evidence-ref . ,offer-id)
                           (scope . ((agent-id . ,agent)
                                     (session-id . ,session)))))
         (segment/detail . (,(format "offer %s from agent-a:" offer-id)
                            "  1  grant  — agreement only, no grant")))))))

(ert-deftest agent-chat-offer-detail-is-exact-seat-and-once-per-offer ()
  (with-temp-buffer
    (agent-chat-test--init-buffer)
    (setq-local agent-chat--agent-id "agent-a"
                agent-chat--session-id "session-a"
                agent-chat--shown-offer-ids nil)
    (let (messages)
      (cl-letf (((symbol-function 'agent-chat-insert-message)
                 (lambda (name text) (push (cons name text) messages))))
        (let ((first (agent-chat-test--offer-payload
                      "agent-a" "session-a" "act:offer-1")))
          (agent-chat--show-offer-detail! first)
          (agent-chat--show-offer-detail! first))
        (agent-chat--show-offer-detail!
         (agent-chat-test--offer-payload
          "agent-b" "session-b" "act:offer-foreign"))
        (agent-chat--show-offer-detail!
         (agent-chat-test--offer-payload
          "agent-a" "session-a" "act:offer-2")))
      (should (= 2 (length messages)))
      (should (string-match-p "act:offer-1" (cdr (nth 1 messages))))
      (should (string-match-p "act:offer-2" (cdr (nth 0 messages))))
      (should-not (seq-some (lambda (entry)
                              (string-match-p "foreign" (cdr entry)))
                            messages)))))

(ert-deftest agent-chat-prefixed-prompt-repair-uses-only-last-line ()
  (with-temp-buffer
    (insert "$foo> historical\n> quoted\ntranscript\n$~x/y> typed")
    (setq-local agent-chat--prompt-marker (copy-marker (point-min) t))
    (setq-local agent-chat--separator-start nil)
    (setq-local agent-chat--input-start nil)
    (agent-chat--ensure-prompt-markers!)
    (should (equal "$~x/y> typed"
                   (buffer-substring-no-properties
                    (marker-position agent-chat--prompt-marker) (point-max))))
    (should (equal "typed"
                   (buffer-substring-no-properties
                    (marker-position agent-chat--input-start) (point-max))))))

(ert-deftest agent-chat-send-input-excludes-prefixed-prompt ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'agent-chat--fetch-prompt-line)
               (lambda () "$~x/y> ")))
      (agent-chat-test--init-buffer))
    (insert "hello")
    (let (sent-text)
      (cl-letf (((symbol-function 'agent-chat-start-turn-commit-window!)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-chat-finish-turn-commits)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-chat-scroll-to-bottom)
                 (lambda (&rest _) nil))
                ((symbol-function 'redisplay) (lambda (&rest _) nil)))
        (cl-letf (((symbol-function 'url-retrieve) (lambda (&rest _) nil)))
          (agent-chat-send-input
           (lambda (text _callback) (setq sent-text text) nil)
           "agent"))
        (should (equal "hello" sent-text))
        ;; Sending clears the previous pattern from the last line.
        (goto-char (point-max))
        (should (equal "> " (buffer-substring-no-properties
                             (line-beginning-position) (point-max))))
        (should (= (point-max) (marker-position agent-chat--input-start)))))))

(defun agent-chat-test--agreement-send (text response &optional no-evidence)
  "Send TEXT with stubbed agreement RESPONSE and return observed effects."
  (with-temp-buffer
    (agent-chat-test--init-buffer)
    (setq-local agent-chat--agent-id "agent-a"
                agent-chat--session-id "session-a")
    (insert text)
    (let (events messages sent)
      (cl-letf (((symbol-function 'agent-chat-insert-message)
                 (lambda (name value)
                   (push (cons name value) messages)))
                ((symbol-function 'agent-chat--agreement-post-async)
                 (lambda (_url payload callback)
                   (push (list :request payload) events)
                   (funcall callback (if (eq response :timeout)
                                         (list :status 0 :json nil)
                                       response))))
                ((symbol-function 'agent-chat-start-turn-commit-window!) #'ignore)
                ((symbol-function 'agent-chat--refresh-prompt-line!) #'ignore)
                ((symbol-function 'agent-chat--prefetch-prompt-line!) #'ignore)
                ((symbol-function 'agent-chat-insert-thinking) #'ignore)
                ((symbol-function 'redisplay) #'ignore))
        (agent-chat-send-input
         (lambda (value _callback)
           (setq sent value)
           (push :invoke events)
           nil)
         "agent"
         (list :before-send
               (lambda (_value)
                 (push :evidence events)
                 (unless no-evidence
                   (setq agent-chat--last-evidence-id "e:yes")))))
        (list :events (reverse events) :messages (reverse messages) :sent sent)))))

(ert-deftest agent-chat-agreement-checks-after-evidence-and-always-sends ()
  (dolist
      (case
       `(((:status 200
           :json (:record (:id "act:agreement-1"
                           :agreement/offer "act:offer-1"
                           :agreement/option-id "2")))
          "yes: agreement act:agreement-1 (offer act:offer-1 option 2); agreement only, no grant")
         ((:status 200
           :json (:record (:id "act:agreement-2"
                           :agreement/offer "act:offer-2"
                           :agreement/option-id "1")
                  :grant (:id "act:grant-2" :until "2026-09-29T00:00:00Z")))
          "yes: agreement act:agreement-2 (offer act:offer-2 option 1); grant act:grant-2 until 2026-09-29T00:00:00Z")
         ((:status 200
           :json (:record (:id "act:agreement-3"
                           :agreement/offer "act:offer-3"
                           :agreement/option-id "2")
                  :grant :null :grant-reason "agreement-only"))
          "yes: agreement act:agreement-3 (offer act:offer-3 option 2); agreement only, no grant")
         ((:status 409 :json (:reason "ambiguous"))
          "yes: ambiguous; the agent will ask which")
         ((:status 409 :json (:reason "unknown-option"))
          "yes: not recorded (unknown-option)")
         ((:status 403 :json (:reason "evidence-not-operator-turn"))
          "yes: not checked (http 403, evidence-not-operator-turn)")
         (:timeout "yes: not checked (timeout)")))
    (let* ((result (agent-chat-test--agreement-send "yes 2" (car case)))
           (events (plist-get result :events))
           (system-lines (mapcar #'cdr
                                 (seq-filter (lambda (entry)
                                               (equal "system" (car entry)))
                                             (plist-get result :messages)))))
      (should (equal "yes 2" (plist-get result :sent)))
      (should (= 1 (seq-count (lambda (event) (eq event :invoke)) events)))
      (should (equal '(:evidence :request :invoke)
                     (mapcar (lambda (event) (if (listp event) :request event))
                             events)))
      (should (member (cadr case) system-lines)))))

;; 2026-10-01: the route took 11 s under futon1b load.  The turn must go to the
;; agent without waiting, and the outcome line must land in the buffer that
;; asked when the answer comes back later.
(ert-deftest agent-chat-agreement-answer-arrives-after-the-turn-is-sent ()
  (let ((chat (generate-new-buffer " *agreement-async*"))
        pending sent lines)
    (unwind-protect
        (progn
          (with-current-buffer chat
            (agent-chat-test--init-buffer)
            (setq-local agent-chat--agent-id "agent-a"
                        agent-chat--session-id "session-a")
            (insert "🈸:yes 1")
            (cl-letf (((symbol-function 'agent-chat-insert-message)
                       (lambda (name value)
                         (when (equal name "system")
                           (push (cons (buffer-name) value) lines))))
                      ((symbol-function 'agent-chat--agreement-post-async)
                       (lambda (_url _payload callback) (setq pending callback)))
                      ((symbol-function 'agent-chat-start-turn-commit-window!) #'ignore)
                      ((symbol-function 'agent-chat--refresh-prompt-line!) #'ignore)
                      ((symbol-function 'agent-chat--prefetch-prompt-line!) #'ignore)
                      ((symbol-function 'agent-chat-insert-thinking) #'ignore)
                      ((symbol-function 'redisplay) #'ignore))
              (agent-chat-send-input
               (lambda (value _callback) (setq sent value) nil)
               "agent"
               (list :before-send
                     (lambda (_value) (setq agent-chat--last-evidence-id "e:yes"))))
              (should (equal "🈸:yes 1" sent))
              (should (functionp pending))
              (should-not lines)
              ;; The answer arrives later, while another buffer is current.
              (with-temp-buffer
                (funcall pending
                         '(:status 200
                           :json (:record (:id "act:a" :agreement/offer "act:o"
                                           :agreement/option-id "1")))))
              (should (equal (list (cons (buffer-name chat)
                                         "yes: agreement act:a (offer act:o option 1); agreement only, no grant"))
                             lines)))))
      (kill-buffer chat))))

(ert-deftest agent-chat-agreement-near-miss-makes-no-request ()
  (let* ((result (agent-chat-test--agreement-send
                  "yes please" '(:status 500 :json (:reason "should-not-run"))))
         (events (plist-get result :events)))
    (should (equal "yes please" (plist-get result :sent)))
    (should (equal '(:evidence :invoke) events))))

(ert-deftest agent-chat-agreement-without-acknowledged-evidence-still-sends ()
  (let* ((result (agent-chat-test--agreement-send
                  "yes" '(:status 200 :json nil) t))
         (events (plist-get result :events)))
    (should (equal "yes" (plist-get result :sent)))
    (should (equal '(:evidence :invoke) events))
    (should (member '("system" . "yes: not checked (no evidence id)")
                    (plist-get result :messages)))))

(ert-deftest agent-chat-cost-flair-suffix-shows-cold-resume-cost ()
  (should (equal (agent-chat-cost-flair-suffix
                  '(:vendor "claude" :last_turn_usd 0.5 :last_turn_calls 1
                    :cold t :per_turn_cold_usd 1.56))
                 " · ~$0.50 (1 call) · cold — resuming costs ~$1.56")))

(ert-deftest agent-chat-schedule-cold-notice-marks-cold-now-or-later ()
  (with-temp-buffer
    (insert "Cooked for 9s\n─── *no mission*\n> ")
    (setq-local agent-chat--prompt-marker (copy-marker (- (point-max) 2)))
    ;; already past the TTL: annotate immediately
    (setq-local agent-chat--cost-basis
                '(:vendor "claude" :last_turn_usd 0.5 :last_turn_calls 1
                  :warm_s 4000 :ttl_s 3600 :per_turn_cold_usd 1.56))
    (agent-chat--schedule-cold-notice!)
    (should (null agent-chat--cost-cold-timer))
    (should (string-match-p "resuming costs ~\\$1\\.56" (buffer-string)))
    ;; still warm: a timer is pending, nothing written yet
    (setq-local agent-chat--cost-basis
                '(:vendor "claude" :last_turn_usd 0.5 :last_turn_calls 1
                  :warm_s 10 :ttl_s 3600 :per_turn_cold_usd 1.56))
    (agent-chat--annotate-turn-flair!)
    (agent-chat--schedule-cold-notice!)
    (should (timerp agent-chat--cost-cold-timer))
    (should-not (string-match-p "resuming" (buffer-string)))
    (cancel-timer agent-chat--cost-cold-timer)))

(ert-deftest agent-chat-cost-segment-renders-empty-without-data ()
  (with-temp-buffer
    (setq agent-chat--cost-basis nil)
    (should (equal (agent-chat-cost-segment) ""))))

(defun agent-chat-test--init-buffer ()
  (let ((agent-chat-prompt-line-enabled nil))
    (cl-letf (((symbol-function 'agent-chat--refresh-session-turn-count)
               (lambda (&rest _) nil)))
      (agent-chat-init-buffer
       (list :title "agent chat test"
             :session-id "sid-test"
             :modeline-fn (lambda () "test modeline")
             :agent-name "agent"
             :agent-id "agent-1"
             :face-alist nil
             :thinking-text "agent is thinking..."
             :thinking-prop 'agent-chat-test-thinking)))))

(defun agent-chat-test--send-undo (text response)
  "Send TEXT with a stubbed undo RESPONSE; return captured request/send data."
  (let (request-payload sent-text)
    (insert text)
    (cl-letf (((symbol-function 'agent-chat-evidence-request-json)
               (lambda (_method _url _timeout payload)
                 (setq request-payload payload)
                 (if (eq response :timeout) (error "timed out") response)))
              ((symbol-function 'agent-chat--fetch-prompt-line) (lambda () nil))
              ((symbol-function 'agent-chat-start-turn-commit-window!)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-chat-finish-turn-commits)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-chat-scroll-to-bottom)
               (lambda (&rest _) nil))
              ((symbol-function 'redisplay) (lambda (&rest _) nil)))
      (agent-chat-send-input
       (lambda (sent _callback) (setq sent-text sent) nil)
       "agent"))
    (list :request request-payload :sent sent-text :buffer (buffer-string))))

(ert-deftest agent-chat-operator-undo-restores-without-agent-turn ()
  (with-temp-buffer
    (agent-chat-test--init-buffer)
    (let* ((result (agent-chat-test--send-undo
                    "Undo!"
                    '(:status 200
                      :json (:record (:id "act:reversal")
                             :card-as-of (:active (:pattern-id "card/a"))))))
           (buffer (plist-get result :buffer)))
      (should-not (plist-get result :sent))
      (should (string-match-p
               "undo: card/a restored (reversal act:reversal)" buffer)))))

(ert-deftest agent-chat-operator-undo-ambiguous-names-effects ()
  (with-temp-buffer
    (agent-chat-test--init-buffer)
    (let* ((result (agent-chat-test--send-undo
                    "undo"
                    '(:status 409 :json (:reason "ambiguous"
                                          :effects ("act:a" "act:b")))))
           (buffer (plist-get result :buffer)))
      (should-not (plist-get result :sent))
      (should (string-match-p "act:a, act:b" buffer)))))

(ert-deftest agent-chat-explicit-undo-passes-effect-id ()
  (with-temp-buffer
    (agent-chat-test--init-buffer)
    (let* ((result (agent-chat-test--send-undo
                    "undo act:b."
                    '(:status 200
                      :json (:record (:id "act:reversal")
                             :card-as-of (:active (:pattern-id "card/a"))))))
           (payload (plist-get result :request)))
      (should (equal "act:b" (alist-get 'effect payload)))
      (should-not (plist-get result :sent)))))

(ert-deftest agent-chat-undo-failures-fall-through-unchanged ()
  (dolist (case '(("undo" . (:status 422 :json (:reason "nothing-to-undo")))
                  ("undo" . :timeout)
                  ("undo" . (:status 409 :json (:reason "idempotency-conflict")))
                  ("undo that" . (:status 200 :json nil))
                  ("Undo it" . (:status 200 :json nil))))
    (with-temp-buffer
      (agent-chat-test--init-buffer)
      (let ((result (agent-chat-test--send-undo (car case) (cdr case))))
        (should (equal (car case) (plist-get result :sent)))))))

(ert-deftest agent-chat-init-buffer-preserves-default-face-remapping ()
  (with-temp-buffer
    (setq-local face-remapping-alist '((default custom-existing-face)))
    (agent-chat-test--init-buffer)
    (should (equal face-remapping-alist
                   '((default custom-existing-face))))))

(ert-deftest agent-chat-ensure-prompt-markers-preserves-live-input ()
  (with-temp-buffer
    (agent-chat-test--init-buffer)
    (let ((original-input-start (marker-position agent-chat--input-start)))
      (insert "first line\n> quoted line in pending input\nfinal line")
      (agent-chat--ensure-prompt-markers!)
      (should (= original-input-start
                 (marker-position agent-chat--input-start)))
      (should (equal "first line\n> quoted line in pending input\nfinal line"
                     (buffer-substring-no-properties
                      (marker-position agent-chat--input-start)
                      (point-max)))))))

(ert-deftest agent-chat-send-input-keeps-multiline-prompt-input ()
  (with-temp-buffer
    (agent-chat-test--init-buffer)
    (let (sent-text callback)
      (insert "first line\n> quoted line in prompt input\nfinal line")
      (cl-letf (((symbol-function 'agent-chat-start-turn-commit-window!)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-chat-finish-turn-commits)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-chat-scroll-to-bottom)
                 (lambda (&rest _) nil))
                ((symbol-function 'redisplay)
                 (lambda (&rest _) nil)))
        (agent-chat-send-input
         (lambda (text cb)
           (setq sent-text text)
           (setq callback cb)
           nil)
         "agent")
        (should (equal sent-text
                       "first line\n> quoted line in prompt input\nfinal line"))
        (should callback)))))

(ert-deftest agent-chat-unsolicited-turn-queues-operator-input ()
  (with-temp-buffer
    (agent-chat-test--init-buffer)
    (let ((live-proc nil)
          calls
          first-callback)
      (cl-letf (((symbol-function 'agent-chat-start-turn-commit-window!)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-chat-finish-turn-commits)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-chat-scroll-to-bottom)
                 (lambda (&rest _) nil))
                ((symbol-function 'redisplay)
                 (lambda (&rest _) nil))
                ((symbol-function 'process-live-p)
                 (lambda (proc) (and proc (eq proc live-proc)))))
        (agent-chat-send-unsolicited-input
         (lambda (text cb)
           (push (list :text text :origin agent-chat--pending-turn-origin) calls)
           (setq first-callback cb)
           (setq live-proc 'unsolicited-proc)
           'unsolicited-proc)
         "agent"
         "background wake"
         "continuation")
        (should (eq agent-chat--pending-turn-origin 'unsolicited))
        (insert "fresh operator question")
        (agent-chat-send-input
         (lambda (text cb)
           (push (list :text text :origin agent-chat--pending-turn-origin) calls)
           (setq live-proc 'operator-proc)
           (funcall cb "operator reply")
           'operator-proc)
         "agent")
        (should (= 1 (length calls)))
        (should (= 1 (length agent-chat--queued-operator-turns)))
        (setq live-proc nil)
        (funcall first-callback "wake reply")
        (should (= 2 (length calls)))
        (should-not agent-chat--queued-operator-turns)
        (let ((ordered (reverse calls)))
          (should (equal "background wake" (plist-get (car ordered) :text)))
          (should (eq 'unsolicited (plist-get (car ordered) :origin)))
          (should (equal "fresh operator question" (plist-get (cadr ordered) :text)))
          (should (eq 'operator (plist-get (cadr ordered) :origin))))
        (let ((buf (buffer-string)))
          (should (string-match-p "continuation:" buf))
          (should (string-match-p "joe:" buf))
          (should (string-match-p "wake reply" buf))
          (should (string-match-p "operator reply" buf)))))))

(ert-deftest agent-chat-unsolicited-queues-behind-operator-turn ()
  "The mirror race: an unsolicited resume arriving while an OPERATOR turn is in
flight must QUEUE (and drain after), never signal — the park delivery path has
already recorded the park-id, so refusing here would destroy the resume."
  (with-temp-buffer
    (agent-chat-test--init-buffer)
    (let ((live-proc nil)
          calls
          operator-callback)
      (cl-letf (((symbol-function 'agent-chat-start-turn-commit-window!)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-chat-finish-turn-commits)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-chat-scroll-to-bottom)
                 (lambda (&rest _) nil))
                ((symbol-function 'redisplay)
                 (lambda (&rest _) nil))
                ((symbol-function 'process-live-p)
                 (lambda (proc) (and proc (eq proc live-proc)))))
        (insert "operator question")
        (agent-chat-send-input
         (lambda (text cb)
           (push (list :text text :origin agent-chat--pending-turn-origin) calls)
           (setq operator-callback cb)
           (setq live-proc 'operator-proc)
           'operator-proc)
         "agent")
        (should (eq agent-chat--pending-turn-origin 'operator))
        ;; The resume lands mid-turn: must queue without signalling.
        (agent-chat-send-unsolicited-input
         (lambda (text cb)
           (push (list :text text :origin agent-chat--pending-turn-origin) calls)
           (setq live-proc 'unsolicited-proc)
           (funcall cb "wake reply")
           'unsolicited-proc)
         "agent"
         "background wake"
         "continuation")
        (should (= 1 (length calls)))
        (should (= 1 (length agent-chat--queued-operator-turns)))
        (setq live-proc nil)
        (funcall operator-callback "operator reply")
        (should (= 2 (length calls)))
        (should-not agent-chat--queued-operator-turns)
        (let ((ordered (reverse calls)))
          (should (equal "operator question" (plist-get (car ordered) :text)))
          (should (eq 'operator (plist-get (car ordered) :origin)))
          (should (equal "background wake" (plist-get (cadr ordered) :text)))
          (should (eq 'unsolicited (plist-get (cadr ordered) :origin))))
        (let ((buf (buffer-string)))
          (should (string-match-p "continuation:" buf))
          (should (string-match-p "wake reply" buf))
          (should (string-match-p "operator reply" buf)))))))

(ert-deftest agent-chat-affect-live-runner-builds-command ()
  (let ((agent-chat-affect-live-enabled t)
        (agent-chat-affect-live-directory "/tmp")
        (agent-chat-affect-live-command '("clojure" "-M" "-m" "futon0.rhythm.affect" "--live"))
        (agent-chat--affect-live-process nil)
        captured)
    (cl-letf (((symbol-function 'agent-chat-evidence-enabled-p)
               (lambda (_url) t))
              ((symbol-function 'process-live-p)
               (lambda (_proc) nil))
              ((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq captured args)
                 'fake-affect-process)))
      (agent-chat--maybe-run-affect-live "http://localhost:7070/api/alpha/evidence")
      (should (eq agent-chat--affect-live-process 'fake-affect-process))
      (should (equal (plist-get captured :name) "agent-chat-affect-live"))
      (should (equal (plist-get captured :command)
                     '("clojure" "-M" "-m" "futon0.rhythm.affect" "--live"
                       "--evidence-url" "http://localhost:7070/api/alpha/evidence")))
      (should (null (plist-get captured :buffer))))))

(ert-deftest agent-chat-evidence-outbox-replays-stable-id-after-timeout ()
  (let* ((agent-chat-evidence-outbox-directory
          (make-temp-file "agent-chat-evidence-outbox-" t))
         (responses '((:status 0 :error "timed out")
                      (:status 201 :json (:evidence/id "ignored-server-id"))))
         seen-timeouts
         evidence-id)
    (unwind-protect
        (cl-letf (((symbol-function 'agent-chat-evidence-request-json)
                   (lambda (_method _url timeout &rest _)
                     (push timeout seen-timeouts)
                     (prog1 (car responses) (setq responses (cdr responses)))))
                  ((symbol-function 'agent-chat-evidence-start-outbox!) #'ignore))
          (setq evidence-id
                (agent-chat-evidence-post-entry-id
                 "http://store.test/api/alpha/evidence" 1
                 '((type . "coordination")
                   (claim-type . "observation")
                   (author . "joe")
                   (body . ((event . "chat-turn")))
                   (tags . ["user"]))))
          (should (string-prefix-p "emacs-" evidence-id))
          (should (eq 'retry agent-chat--last-evidence-delivery-outcome))
          (let* ((files (agent-chat-evidence--queue-files))
                 (record (agent-chat-evidence--read-record (car files)))
                 (payload (alist-get 'payload record)))
            (should (= 1 (length files)))
            (should (equal evidence-id (alist-get 'id payload)))
            (should-not (assq 'timeout record)))
          (cl-letf (((symbol-function 'agent-chat-evidence--start-replay!)
                     (lambda (path record)
                       (when (eq 'acked
                                 (agent-chat-evidence--attempt-record record))
                         (delete-file path))
                       (agent-chat-evidence--release-drain-lease))))
            (agent-chat-evidence-drain-outbox!))
          (should (equal (reverse seen-timeouts)
                         (list 1 agent-chat-evidence-outbox-attempt-timeout)))
          (should-not (agent-chat-evidence--queue-files)))
      (delete-directory agent-chat-evidence-outbox-directory t))))

(ert-deftest agent-chat-evidence-outbox-treats-duplicate-as-ack ()
  (let ((agent-chat-evidence-outbox-directory
         (make-temp-file "agent-chat-evidence-outbox-" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-chat-evidence-request-json)
                   (lambda (&rest _) '(:status 409 :json (:error "duplicate")))))
          (should (equal "stable-evidence-id"
                         (agent-chat-evidence-post-entry-id
                          "http://store.test/api/alpha/evidence" 1
                          '((id . "stable-evidence-id")
                            (type . "coordination")
                            (claim-type . "observation")
                            (author . "joe")))))
          (should (eq 'acked agent-chat--last-evidence-delivery-outcome))
          (should-not (agent-chat-evidence--queue-files)))
      (delete-directory agent-chat-evidence-outbox-directory t))))

(ert-deftest agent-chat-evidence-outbox-retains-terminal-rejection ()
  (let ((agent-chat-evidence-outbox-directory
         (make-temp-file "agent-chat-evidence-outbox-" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-chat-evidence-request-json)
                   (lambda (&rest _) '(:status 400 :json (:error "bad shape")))))
          (should-not
           (agent-chat-evidence-post-entry-id
            "http://store.test/api/alpha/evidence" 1
            '((id . "rejected-evidence-id")
              (type . "coordination")
              (claim-type . "observation")
              (author . "joe"))))
          (should-not (agent-chat-evidence--queue-files))
          (should (= 1 (length (agent-chat-evidence--failed-files)))))
      (delete-directory agent-chat-evidence-outbox-directory t))))

(ert-deftest agent-chat-evidence-outbox-retries-reply-not-found ()
  "A 409 reply-not-found is transient (parent in flight / server restarting)."
  (let ((agent-chat-evidence-outbox-directory
         (make-temp-file "agent-chat-evidence-outbox-" t))
        (agent-chat--evidence-outbox-timer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-chat-evidence-request-json)
                   (lambda (&rest _)
                     '(:status 409
                       :json (:ok :false :err "reply-not-found"
                              :error (:error/code "reply-not-found"
                                      :error/message "in-reply-to references missing entry")))))
                  ((symbol-function 'agent-chat-evidence-start-outbox!) #'ignore))
          (should (equal "child-evidence-id"
                         (agent-chat-evidence-post-entry-id
                          "http://store.test/api/alpha/evidence" 1
                          '((id . "child-evidence-id")
                            (in-reply-to . "parent-evidence-id")
                            (type . "coordination")
                            (claim-type . "observation")
                            (author . "joe")))))
          (should (eq 'retry agent-chat--last-evidence-delivery-outcome))
          (should (= 1 (length (agent-chat-evidence--queue-files))))
          (should-not (agent-chat-evidence--failed-files)))
      (delete-directory agent-chat-evidence-outbox-directory t))))

(ert-deftest agent-chat-evidence-reply-not-found-fails-after-bounded-attempts ()
  (let ((response '(:status 409 :json (:err "reply-not-found")))
        (agent-chat-evidence-outbox-reply-not-found-max-attempts 3))
    (should (eq 'retry (agent-chat-evidence--classify-response response)))
    (should (eq 'retry (agent-chat-evidence--classify-response
                        response '((attempts . 2)))))
    (should (eq 'failed (agent-chat-evidence--classify-response
                         response '((attempts . 3)))))
    ;; Other 409s and 4xxs stay terminal.
    (should (eq 'failed (agent-chat-evidence--classify-response
                         '(:status 409 :json (:err "conflict")))))
    (should (eq 'acked (agent-chat-evidence--classify-response
                        '(:status 409 :json (:err "duplicate-id")))))))

(ert-deftest agent-chat-evidence-replay-parses-status-before-process-notice ()
  (should (= 409 (agent-chat-evidence--curl-status
                  "409\n\nProcess agent-chat-evidence-replay finished\n")))
  (should (= 0 (agent-chat-evidence--curl-status "curl transport error"))))

(ert-deftest agent-chat-evidence-retry-deadline-is-persisted-across-emacs-processes ()
  (let* ((agent-chat-evidence-outbox-directory
          (make-temp-file "agent-chat-evidence-outbox-" t))
         (agent-chat-evidence-outbox-retry-seconds 15)
         (path (expand-file-name "record.json"
                                 agent-chat-evidence-outbox-directory))
         (record '((evidence-url . "http://store.test/api/alpha/evidence")
                   (payload . ((evidence-id . "stable-id")))
                   (attempts . 0)
                   (next-at . 0))))
    (unwind-protect
        (cl-letf (((symbol-function 'float-time) (lambda (&rest _) 1000.0)))
          (agent-chat-evidence--schedule-retry path record)
          (let ((saved (agent-chat-evidence--read-record path)))
            (should (= 1 (alist-get 'attempts saved)))
            (should (= 1030.0 (alist-get 'next-at saved)))
            (should-not (agent-chat-evidence--eligible-record))))
      (delete-directory agent-chat-evidence-outbox-directory t))))

(ert-deftest agent-chat-ensure-prompt-markers-repairs-drifting-input-start ()
  (with-temp-buffer
    (agent-chat-test--init-buffer)
    (let ((expected-input-start (marker-position agent-chat--input-start)))
      (insert "pending prompt text")
      (setq agent-chat--input-start (copy-marker (point-max) t))
      (agent-chat--ensure-prompt-markers!)
      (should (= expected-input-start
                 (marker-position agent-chat--input-start)))
      (should-not (marker-insertion-type agent-chat--input-start))
      (should (equal "pending prompt text"
                     (buffer-substring-no-properties
                      (marker-position agent-chat--input-start)
                      (point-max)))))))

(ert-deftest agent-chat-auto-clock-resolves-only-explicit-existing-targets ()
  (cl-letf (((symbol-function 'agent-chat--clock-target-candidates)
               (lambda (kind)
                 (pcase kind
                   ('campaign '("C-substrate-completion"))
                   ('mission '("M-autoclock-in" "M-vsatarcs-invariants-integration"))
                   ('excursion '("E-fix-cx-cr-runners-to-clock-in"))))))
    (should (equal (agent-chat--explicit-clock-target-tokens
                    "please advance M-autoclock-in, then inspect C-substrate-completion.")
                   '("M-autoclock-in" "C-substrate-completion")))
    (should (equal (agent-chat--auto-clock-target-from-text
                    "please advance M-autoclock-in")
                   '(:campaign-id nil
                     :mission-id "M-autoclock-in"
                     :excursion-id nil
                     :tokens ("M-autoclock-in")
                     :rule "explicit-resolved-target")))
    (should (equal (agent-chat--auto-clock-target-from-text
                    "work on C-substrate-completion and M-autoclock-in")
                   '(:campaign-id "C-substrate-completion"
                     :mission-id "M-autoclock-in"
                     :excursion-id nil
                     :tokens ("C-substrate-completion" "M-autoclock-in")
                     :rule "explicit-resolved-target")))
    (should-not (agent-chat--auto-clock-target-from-text
                 "maybe M-does-not-exist"))
    (should-not (agent-chat--auto-clock-target-from-text
                 "compare M-autoclock-in and M-vsatarcs-invariants-integration"))))

;; Tickets are clock targets (Joe, 2026-09-24).
(defmacro agent-chat-test--with-ticket-candidates (&rest body)
  `(cl-letf (((symbol-function 'agent-chat--clock-target-candidates)
              (lambda (kind)
                (pcase kind
                  ('mission '("M-autoclock-in"))
                  ('ticket '("T-agency-desktop-save" "T-other"))))))
     ,@body))

(ert-deftest agent-chat-tickets-parse-label-and-inherit-the-path ()
  (with-temp-buffer
    (should (equal (agent-chat-parse-clock-target "M-autoclock-in > T-agency-desktop-save")
                   '(:campaign-id nil :mission-id "M-autoclock-in" :excursion-id nil
                     :ticket-id "T-agency-desktop-save")))
    (agent-chat-set-clock! "M-autoclock-in" nil t)
    (agent-chat-set-clock! "T-agency-desktop-save" t t)
    (should (equal (agent-chat-mission-label) "M-autoclock-in › T-agency-desktop-save"))
    (should (equal (agent-chat-dispatch-clock-id) "T-agency-desktop-save"))
    (let ((fields (agent-chat--mission-body-fields)))
      (should (equal (alist-get 'ticket-id fields) "T-agency-desktop-save"))
      (should (equal (alist-get 'clocked-ticket fields) "T-agency-desktop-save")))
    (agent-chat-set-clock! "M-autoclock-in" nil t)
    (should-not agent-chat--ticket-id)
    (should (equal (agent-chat-dispatch-clock-id) "M-autoclock-in"))))

(ert-deftest agent-chat-auto-clock-resolves-tickets ()
  (agent-chat-test--with-ticket-candidates
   (should (equal (agent-chat--auto-clock-target-from-text
                   "You requisitioned kimi-1 for T-agency-desktop-save.")
                  '(:campaign-id nil :mission-id nil :excursion-id nil
                    :ticket-id "T-agency-desktop-save"
                    :tokens ("T-agency-desktop-save")
                    :rule "explicit-resolved-target")))
   (should-not (agent-chat--auto-clock-target-from-text "T-agency-desktop-save and T-other"))
   (should-not (agent-chat--auto-clock-target-from-text "T-missing"))
   (with-temp-buffer
     (setq-local agent-chat-auto-clock-enabled t)
     (cl-letf (((symbol-function 'agent-chat-insert-message) #'ignore))
       (agent-chat-set-clock! "M-autoclock-in" nil t)
       (agent-chat--maybe-auto-clock-from-turn "switch → T-agency-desktop-save")
       (should (equal agent-chat--ticket-id "T-agency-desktop-save"))
       ;; An arrow switch to a ticket keeps the parent path.
       (should (equal agent-chat--mission-id "M-autoclock-in"))))))

(ert-deftest agent-chat-auto-clock-promotes-before-evidence-fields-and-clears-witness ()
  (with-temp-buffer
    (agent-chat-test--init-buffer)
    (cl-letf (((symbol-function 'agent-chat--clock-target-candidates)
               (lambda (kind)
                 (pcase kind
                   ('campaign nil)
                   ('mission '("M-autoclock-in"))
                   ('excursion nil))))
              ((symbol-function 'agent-chat-start-turn-commit-window!)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-chat-finish-turn-commits)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-chat-scroll-to-bottom)
               (lambda (&rest _) nil))
              ((symbol-function 'redisplay)
               (lambda (&rest _) nil)))
      (let (fields-at-before-send)
        (insert "please advance M-autoclock-in")
        (agent-chat-send-input
         (lambda (_text cb) cb)
         "agent"
         (list :before-send
               (lambda (_text)
                 (setq fields-at-before-send
                       (agent-chat--mission-body-fields)))))
        (should (equal agent-chat--mission-id "M-autoclock-in"))
        (should (assoc 'auto-clock-witness fields-at-before-send))
        (should-not (assoc 'auto-clock-witness
                           (agent-chat--mission-body-fields)))))))

(ert-deftest agent-chat-auto-clock-only-fires-at-no-target-floor ()
  "Auto-clock fills the no-target floor; it never switches an active clocking.
A mention made while already clocked is left for turn-level capture, not
promoted (and must not wipe a bare campaign down to the bare mention)."
  (with-temp-buffer
    (agent-chat-test--init-buffer)
    (cl-letf (((symbol-function 'agent-chat--clock-target-candidates)
               (lambda (kind)
                 (pcase kind
                   ('campaign '("C-substrate-completion"))
                   ('mission '("M-autoclock-in" "M-differentiable-code"))
                   ('excursion nil))))
              ((symbol-function 'agent-chat-insert-message)
               (lambda (&rest _) nil)))
      ;; clocked on a mission: mentioning another resolved mission must NOT switch
      (setq agent-chat--campaign-id nil
            agent-chat--mission-id "M-autoclock-in"
            agent-chat--excursion-id nil)
      (should-not (agent-chat--maybe-auto-clock-from-turn
                   "this is unrelated to M-differentiable-code"))
      (should (equal agent-chat--mission-id "M-autoclock-in"))
      ;; clocked on a bare campaign: mentioning a mission must NOT wipe the campaign
      (setq agent-chat--campaign-id "C-substrate-completion"
            agent-chat--mission-id nil
            agent-chat--excursion-id nil)
      (should-not (agent-chat--maybe-auto-clock-from-turn
                   "see M-differentiable-code"))
      (should (equal agent-chat--campaign-id "C-substrate-completion"))
      (should-not agent-chat--mission-id)
      ;; at the no-target floor: promotion fires
      (setq agent-chat--campaign-id nil
            agent-chat--mission-id nil
            agent-chat--excursion-id nil)
      (should (agent-chat--maybe-auto-clock-from-turn
               "let us work on M-differentiable-code"))
      (should (equal agent-chat--mission-id "M-differentiable-code")))))

(ert-deftest agent-chat-creation-clock-switches-after-mission-exists ()
  "Creation-clock is a separate rule: it resolves after creation and may switch."
  (with-temp-buffer
    (agent-chat-test--init-buffer)
    (let ((missions '("M-autoclock-in"))
          inserted)
      (cl-letf (((symbol-function 'agent-chat--clock-target-candidates)
                 (lambda (kind)
                   (pcase kind
                     ('mission missions)
                     (_ nil))))
                ((symbol-function 'agent-chat-insert-message)
                 (lambda (name text)
                   (push (list name text) inserted))))
        (setq agent-chat--campaign-id nil
              agent-chat--mission-id "M-autoclock-in"
              agent-chat--excursion-id nil)
        (should-not (agent-chat-creation-clock-mission!
                     "M-creation-clock" "eoi-new-head"))
        (should (equal agent-chat--mission-id "M-autoclock-in"))
        (push "M-creation-clock" missions)
        (let ((witness (agent-chat-creation-clock-mission!
                        "creation-clock" "eoi-new-head")))
          (should witness)
          (should (equal agent-chat--mission-id "M-creation-clock"))
          (should (equal (alist-get 'rule witness) "creation-clock"))
          (should (equal (alist-get 'source witness) "eoi-new-head"))
          (should (equal (alist-get 'old-target witness) "M-autoclock-in"))
          (should inserted))))))

;;; agent-chat-test.el ends here

(ert-deftest agent-chat-surface-marker-splits-only-a-prefix ()
  "A leading surface marker is metadata; the same glyph mid-sentence is text.
voxterm prepends the speaking-head marker to a dictated turn so the operator
can see which surface he is on. It must not reach the evidence text, and it
must not eat a character he meant to write."
  (should (equal (agent-chat-split-surface-marker "🗣 the turn text")
                 '(dictated . "the turn text")))
  (should (equal (agent-chat-split-surface-marker "the turn text")
                 '(nil . "the turn text")))
  (should (equal (agent-chat-split-surface-marker "talk about 🗣 emoji")
                 '(nil . "talk about 🗣 emoji")))
  ;; the marker's trailing space goes with it, not into the text
  (should (equal (cdr (agent-chat-split-surface-marker "🗣    spaced out"))
                 "spaced out")))

;;; Evidence requests must not leave requests behind when they time out.
;; 2026-09-27: `url-retrieve-synchronously' abandons a timed-out request, and
;; the graph daemon had accumulated 20 orphaned evidence responses.

(defun agent-chat-test--http-server (delay)
  "Start a server answering each request with a JSON body after DELAY seconds.
DELAY nil means never answer.  Return the server process."
  (make-network-process
   :name "agent-chat-test-http" :server t :host "127.0.0.1" :service t
   :family 'ipv4 :noquery t :sentinel #'ignore
   :filter (lambda (proc _)
             (when delay
               (run-at-time delay nil
                            (lambda ()
                              (when (process-live-p proc)
                                (process-send-string
                                 proc (concat "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n"
                                              "Content-Length: 11\r\n\r\n{\"ok\":true}")))))))))

(defun agent-chat-test--leftovers (port)
  "Open client connections and response buffers belonging to PORT."
  (list :connections
        (cl-count-if (lambda (p)
                       (and (not (process-contact p :server))
                            (eq (process-status p) 'open)
                            (equal (plist-get (process-contact p t) :service) port)))
                     (process-list))
        :buffers
        (cl-count-if (lambda (b)
                       (string-prefix-p (format " *http 127.0.0.1:%d" port) (buffer-name b)))
                     (buffer-list))))

(defmacro agent-chat-test--with-http-server (delay port-var &rest body)
  (declare (indent 2))
  (let ((server (make-symbol "server")))
    `(let* ((,server (agent-chat-test--http-server ,delay))
            (,port-var (process-contact ,server :service))
            (url-show-status nil))
       (unwind-protect (progn ,@body)
         (dolist (p (process-list))
           (when (equal (plist-get (process-contact p t) :service) ,port-var)
             (set-process-sentinel p #'ignore)
             (delete-process p)))))))

(defun agent-chat-test--settle (seconds)
  (let ((end (+ (float-time) seconds)))
    (while (< (float-time) end) (accept-process-output nil 0.05))))

(ert-deftest agent-chat-evidence-request-returns-json-and-cleans-up ()
  (agent-chat-test--with-http-server 0 port
    (let ((resp (agent-chat-evidence-request-json
                 "GET" (format "http://127.0.0.1:%d/api/alpha/evidence" port) 2)))
      (should (equal 200 (plist-get resp :status)))
      (should (plist-get resp :json))
      (agent-chat-test--settle 0.2)
      (should (equal 0 (plist-get (agent-chat-test--leftovers port) :buffers))))))

(ert-deftest agent-chat-evidence-request-cancels-an-unanswered-request ()
  (agent-chat-test--with-http-server nil port
    (let ((resp (agent-chat-evidence-request-json
                 "GET" (format "http://127.0.0.1:%d/api/alpha/evidence" port) 0.3)))
      (should (equal 0 (plist-get resp :status)))
      (should (equal '(:connections 0 :buffers 0) (agent-chat-test--leftovers port))))))

(ert-deftest agent-chat-evidence-request-leaves-no-buffer-for-a-late-answer ()
  (agent-chat-test--with-http-server 0.6 port
    (let ((resp (agent-chat-evidence-request-json
                 "GET" (format "http://127.0.0.1:%d/api/alpha/evidence" port) 0.3)))
      (should (equal 0 (plist-get resp :status)))
      ;; Give the server time to send its late answer.
      (agent-chat-test--settle 1.0)
      (should (equal '(:connections 0 :buffers 0) (agent-chat-test--leftovers port))))))

(ert-deftest agent-chat-evidence-prunes-failed-records-past-retention ()
  "A failed record older than the retention window is deleted and no longer
counted; a fresh one is kept for inspection."
  (let* ((agent-chat-evidence-outbox-directory (make-temp-file "outbox" t))
         (agent-chat-evidence-failed-retention-seconds 86400)
         (failed (expand-file-name "failed" agent-chat-evidence-outbox-directory))
         (old (expand-file-name "old.json" failed))
         (fresh (expand-file-name "fresh.json" failed)))
    (unwind-protect
        (progn
          (make-directory failed t)
          (with-temp-file old (insert "{}"))
          (with-temp-file fresh (insert "{}"))
          (set-file-times old (time-subtract nil (* 29 86400)))
          (agent-chat-evidence--refresh-delivery-status)
          (should-not (file-exists-p old))
          (should (file-exists-p fresh))
          (should (equal (list fresh) (agent-chat-evidence--failed-files))))
      (delete-directory agent-chat-evidence-outbox-directory t))))

(ert-deftest agent-chat-prompt-fetch-decodes-utf8-body ()
  (with-temp-buffer
    (setq agent-chat--agent-id "claude-17"
          agent-chat--session-id "s1")
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _)
                 (let ((buf (generate-new-buffer " *p7a1c-utf8*")))
                   (with-current-buffer buf
                     (set-buffer-multibyte nil)
                     (setq-local url-http-response-status 200)
                     (insert "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n"
                             (encode-coding-string
                              "{\"prompt\":\"$~象/诺必践> \"}" 'utf-8)))
                   buf))))
      (should (equal (agent-chat--prompt-line) "$~象/诺必践> ")))))

(ert-deftest agent-chat-turn-end-redraws-prompt-prefix-keeping-input ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'agent-chat--fetch-prompt-line)
               (lambda () "> ")))
      (agent-chat-test--init-buffer))
    (insert "abc")
    (cl-letf (((symbol-function 'agent-chat--fetch-prompt-line)
               (lambda () (error "turn end must not fetch")))
              ((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _) (error "turn end must not fetch"))))
      (setq agent-chat--prefetched-prompt-line "$~x/y> ")
      (agent-chat--insert-turn-end-flair 3)
      (goto-char (point-max))
      (should (equal "$~x/y> abc"
                     (buffer-substring-no-properties
                      (line-beginning-position) (point-max))))
      (should (equal "abc" (buffer-substring-no-properties
                            agent-chat--input-start (point-max))))
      (should (null agent-chat--prefetched-prompt-line))
      (let ((start (marker-position agent-chat--prompt-marker)))
        (should (equal "$~x/y> " (buffer-substring-no-properties
                                  start agent-chat--input-start)))
        (dotimes (i 7) (should (get-text-property (+ start i) 'read-only))))
      (should-not (get-text-property agent-chat--input-start 'read-only))
      ;; No prefetched value (not arrived, or failed): the prompt stays as is.
      (agent-chat--insert-turn-end-flair 4)
      (goto-char (point-max))
      (should (equal "$~x/y> abc" (buffer-substring-no-properties
                                   (line-beginning-position) (point-max))))
      ;; A later turn with no pattern returns to the plain prompt.
      (setq agent-chat--prefetched-prompt-line "> ")
      (agent-chat--insert-turn-end-flair 5)
      (goto-char (point-max))
      (should (equal "> abc" (buffer-substring-no-properties
                              (line-beginning-position) (point-max))))
      (should (equal "abc" (buffer-substring-no-properties
                            agent-chat--input-start (point-max))))
      (should (= 1 (how-many "^Cooked for 5s" (point-min) (point-max)))))))

(ert-deftest agent-chat-prefetch-stores-prompt-without-blocking ()
  (with-temp-buffer
    (setq agent-chat--agent-id "claude-17"
          agent-chat--session-id "s1")
    (let (callback)
      (cl-letf (((symbol-function 'url-retrieve)
                 (lambda (_url cb &rest _) (setq callback cb) nil)))
        (agent-chat--prefetch-prompt-line!))
      ;; Nothing is stored until the response arrives.
      (should (functionp callback))
      (should (null agent-chat--prefetched-prompt-line))
      (let ((chat (current-buffer)))
        (with-current-buffer (generate-new-buffer " *p7a1c-prefetch*")
          (set-buffer-multibyte nil)
          (setq-local url-http-response-status 200)
          (insert "HTTP/1.1 200 OK\r\n\r\n"
                  (encode-coding-string "{\"prompt\":\"$~象/诺必践> \"}" 'utf-8))
          (funcall callback nil))
        (should (equal "$~象/诺必践> "
                       (buffer-local-value 'agent-chat--prefetched-prompt-line chat)))))))

(ert-deftest agent-chat-turn-flair-width-leaves-room-for-line-numbers ()
  (let ((buf (generate-new-buffer " *flair-width*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buf)
          (let ((plain (agent-chat--turn-flair-width)))
            (cl-letf (((symbol-function 'line-number-display-width)
                       (lambda (&rest _) 6)))
              (should (= (- plain 6) (agent-chat--turn-flair-width))))
            (should (<= plain (window-body-width)))))
      (kill-buffer buf))))

(ert-deftest agent-chat-done-prompt-line-wins-over-prefetch ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'agent-chat--fetch-prompt-line)
               (lambda () "> ")))
      (agent-chat-test--init-buffer))
    (insert "abc")
    ;; Invalid values are ignored.
    (agent-chat-note-done-prompt-line '((type . "done") (prompt-line . "no prompt")))
    (should (null agent-chat--done-prompt-line))
    (agent-chat-note-done-prompt-line '((type . "done") (prompt-line . "$~this/turn> ")))
    ;; A late turn-start prefetch lands after the done event.
    (setq agent-chat--prefetched-prompt-line "$~previous/turn> ")
    (agent-chat--insert-turn-end-flair 2)
    (goto-char (point-max))
    (should (equal "$~this/turn> abc"
                   (buffer-substring-no-properties
                    (line-beginning-position) (point-max))))
    (should (null agent-chat--done-prompt-line))
    (should (null agent-chat--prefetched-prompt-line))))
