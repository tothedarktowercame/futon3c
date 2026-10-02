;;; session-mode-test.el --- Local draft tag tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'session-mode)

(ert-deftest session-mode-tags-mixed-and-negative-phrases ()
  (let* ((text "I agree with Foo but I disagree with Bar. I don’t agree with Baz.")
         (hits (session-mode--turn-matches text)))
    (should (equal (mapcar (lambda (h) (nth 2 h)) hits)
                   '("approve" "disagree" "disagree")))
    (dolist (h hits)
      (should (equal (substring text (car h) (cadr h)) (nth 3 h))))))

(ert-deftest session-mode-tags-boundaries-and-apostrophes ()
  (should-not (session-mode--turn-matches "I agreement butterfly undergo on"))
  (should (equal (mapcar (lambda (h) (nth 2 h))
                        (session-mode--turn-matches "I DON'T AGREE; that's wrong; that’s a good fit"))
                 '("disagree" "disagree" "approve"))))

(ert-deftest session-mode-tags-withdraw-and-rejects-disagreement-near-miss ()
  (let ((session-mode-turn-vocabulary session-mode-turn-intent-vocabulary))
    (should (equal (mapcar (lambda (hit) (nth 2 hit))
                           (session-mode--turn-matches
                            "I withdraw that pattern."))
                   '("withdraw")))
    (should (equal (mapcar (lambda (hit) (nth 2 hit))
                           (session-mode--turn-matches
                            "I disagree with withdrawing."))
                   '("disagree")))))

(ert-deftest session-mode-withdraw-brief-defines-target-without-effect ()
  (let ((session-mode-turn-vocabulary session-mode-turn-intent-vocabulary)
        (brief (session-mode--analysis-instruction "/tmp/request.json")))
    (should (string-match-p "withdraw means the operator ends or takes back" brief))
    (should (string-match-p "seat-active-card" brief))
    (should (string-match-p "otherwise set target to null" brief))
    (should (string-match-p "terminates nothing" brief))
    (should (string-match-p
             (format "interpretation version %d" session-mode-turn-interpretation-version)
             brief))))

(ert-deftest session-mode-analysis-record-stores-interpretation-versions ()
  (let ((session-mode-turn-analysis-directory
         (make-temp-file "turn-version-test-" t)))
    (unwind-protect
        (with-temp-buffer
          (session-mode-test--init)
          (setq-local agent-chat--last-evidence-id "emacs-operator-1")
          (let* ((path (session-mode--record-turn "Withdraw that pattern."))
                 (json-object-type 'alist)
                 (record (json-read-file path)))
            (should (= (alist-get 'vocabulary_version record)
                       session-mode-turn-vocabulary-version))
            (should (= (alist-get 'interpretation_version record)
                       session-mode-turn-interpretation-version))
            (should (equal (alist-get 'evidence_id record)
                           "emacs-operator-1"))))
      (delete-directory session-mode-turn-analysis-directory t))))

(defconst session-mode-test--fake-aws-key "AKIAFAKE1234567890XY")

(defun session-mode-test--record-json (text &optional original-text)
  "Record TEXT and return its decoded JSON object."
  (let ((json-object-type 'alist)
        (json-array-type 'list))
    (json-read-file (session-mode--record-turn text nil original-text))))

(ert-deftest session-mode-secret-redaction-precedes-recording-and-warns-safely ()
  (let ((session-mode-turn-analysis-directory (make-temp-file "turn-secret-test-" t))
        warnings)
    (unwind-protect
        (with-temp-buffer
          (session-mode-test--init)
          (cl-letf (((symbol-function 'display-warning)
                     (lambda (_type message &rest _)
                       (push message warnings))))
            (let* ((text (format "my key is %s please" session-mode-test--fake-aws-key))
                   (record (session-mode-test--record-json text text))
                   (source (alist-get 'source_text record))
                   (original (alist-get 'original_text record)))
              (should (string-match-p "\\[REDACTED:aws-access-key\\]" source))
              (should (string-match-p "\\[REDACTED:aws-access-key\\]" original))
              (dotimes (index (- (length session-mode-test--fake-aws-key) 7))
                (let ((part (substring session-mode-test--fake-aws-key index (+ index 8))))
                  (should-not (string-match-p (regexp-quote part) source))
                  (should-not (string-match-p (regexp-quote part) original))
                  (should-not (string-match-p (regexp-quote part) (car warnings)))))
              (should (equal '("aws-access-key")
                             (alist-get 'secrets_redacted record)))
              (should (= 1 (length warnings)))
              (should (string-match-p "aws-access-key" (car warnings))))))
      (delete-directory session-mode-turn-analysis-directory t))))

(ert-deftest session-mode-clean-turn-is-unchanged-and-silent ()
  (let ((session-mode-turn-analysis-directory (make-temp-file "turn-clean-test-" t))
        warnings)
    (unwind-protect
        (with-temp-buffer
          (session-mode-test--init)
          (cl-letf (((symbol-function 'display-warning)
                     (lambda (&rest args) (push args warnings))))
            (let ((record (session-mode-test--record-json "A clean operator turn.")))
              (should (equal "A clean operator turn." (alist-get 'source_text record)))
              (should (equal "A clean operator turn." (alist-get 'original_text record)))
              (should (equal nil (alist-get 'secrets_redacted record)))
              (should-not warnings))))
      (delete-directory session-mode-turn-analysis-directory t))))

(ert-deftest session-mode-secret-scan-fails-closed-before-record-or-dispatch ()
  (let ((session-mode-turn-analysis-directory (make-temp-file "turn-fail-closed-" t))
        (session-mode-secret-scan-script "/definitely/missing/secret_scan.py")
        (session-mode-analysis-agent "xiang")
        dispatches)
    (unwind-protect
        (cl-letf (((symbol-function 'session-mode--dispatch-analysis)
                   (lambda (&rest args) (push args dispatches))))
          (should-error
           (session-mode-record-external-turn
            "unscanned text" "claude-3" "session-3" "turn-3")
           :type 'user-error)
          (should-not dispatches)
          (should-not (directory-files session-mode-turn-analysis-directory nil "^turn-")))
      (delete-directory session-mode-turn-analysis-directory t))))

(ert-deftest session-mode-secret-redaction-precedes-sentence-offsets ()
  (let ((session-mode-turn-analysis-directory (make-temp-file "turn-offset-test-" t)))
    (unwind-protect
        (with-temp-buffer
          (session-mode-test--init)
          (let* ((text (format "First sentence. My key is %s. Final sentence."
                               session-mode-test--fake-aws-key))
                 (record (session-mode-test--record-json text))
                 (source (alist-get 'source_text record))
                 (sentences (alist-get 'sentences record))
                 (last-sentence (car (last sentences)))
                 (start (alist-get 'start last-sentence))
                 (end (alist-get 'end last-sentence)))
            (should (equal "Final sentence." (substring source start end)))
            (should (equal "Final sentence." (alist-get 'text last-sentence)))))
      (delete-directory session-mode-turn-analysis-directory t))))

(defun session-mode-test--withdrawal-files (origin target &optional evidence-id text)
  "Return (DIRECTORY RECORD-PATH), containing one analysed withdraw TARGET."
  (let* ((directory (make-temp-file "withdraw-reap-test-" t))
         (path (expand-file-name "turn-stable.json" directory))
         (record `((origin . ,origin) (agent_id . "claude-17")
                   (session_id . "session-17") (turn_id . "turn-17")
                   (interpretation_version . 3) (analysis_status . "analyzed")))
         (record (if evidence-id
                     (append record `((evidence_id . ,evidence-id)))
                   record))
         (fragment `((start . 0) (end . 8) (text . ,(or text "withdraw"))
                     (intent . "withdraw") (target . ,target)))
         (analysis `((status . "analyzed")
                     (sentences . [((id . "s1") (fragments . [,fragment]))]))))
    (with-temp-file path (insert (json-encode record)))
    (with-temp-file (concat path ".analysis.json") (insert (json-encode analysis)))
    (list directory path)))

(ert-deftest session-mode-negation-posts-exact-fragment-and-act-target-once ()
  (pcase-let* ((`(,directory ,path)
                (session-mode-test--withdrawal-files
                 "operator" "act:choice" "emacs:joe-1" "take that choice back"))
               (negation-calls nil))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-chat-evidence-request-json)
                   (lambda (_method url _timeout payload)
                     (cond
                      ((string-suffix-p "/interpretation/negation" url)
                       (push payload negation-calls)
                       '(:status 201
                         :json (:entry (:evidence/id "interpretation:neg-1"
                                        :evidence/body (:resolution "explicit-id")))))
                      ((string-suffix-p "/turn-notice" url)
                       '(:status 200 :json (:result "queued")))
                      (t '(:status 403 :json (:reason "no-grant")))))))
          (session-mode--process-withdrawals path)
          (session-mode--process-withdrawals path)
          (should (= 1 (length negation-calls)))
          (let ((payload (car negation-calls)))
            (should (equal "emacs:joe-1"
                           (alist-get 'operator-evidence-id payload)))
            (should (equal "s1:0" (alist-get 'fragment-id payload)))
            (should (equal "take that choice back"
                           (alist-get 'fragment-text payload)))
            (should (equal "act:choice" (alist-get 'target payload)))
            (should (= 3 (alist-get 'analysis-version payload))))
          (let* ((json-object-type 'alist) (json-array-type 'list)
                 (record (json-read-file path))
                 (outcome (car (alist-get 'negation_interpretations record))))
            (should (= 201 (alist-get 'status outcome)))
            (should (equal "interpretation:neg-1"
                           (alist-get 'evidence_id outcome)))
            (should (equal "explicit-id" (alist-get 'resolution outcome)))))
      (delete-directory directory t))))

(ert-deftest session-mode-negation-omits-seat-target ()
  (let (request)
    (cl-letf (((symbol-function 'agent-chat-evidence-request-json)
               (lambda (_method _url _timeout payload)
                 (setq request payload)
                 '(:status 201 :json (:entry (:evidence/id "interpretation:n"
                                              :evidence/body
                                              (:resolution "single-standing")))))))
      (session-mode--post-negation-interpretation
       "s1:0" '((evidence_id . "emacs:joe") (interpretation_version . 3))
       '((text . "withdraw this card") (target . "seat-active-card")))
      (should-not (assq 'target request)))))

(ert-deftest session-mode-negation-without-evidence-id-records-local-outcome ()
  (let (called)
    (cl-letf (((symbol-function 'agent-chat-evidence-request-json)
               (lambda (&rest _) (setq called t))))
      (let ((outcome
             (session-mode--post-negation-interpretation
              "s1:0" '((evidence_id . :null) (interpretation_version . 3))
              '((text . "withdraw") (target . :null)))))
        (should-not called)
        (should (= 0 (alist-get 'status outcome)))
        (should (equal "no-evidence-id" (alist-get 'reason outcome)))))))

(ert-deftest session-mode-withdraw-reap-success-and-stable-idempotency ()
  (pcase-let* ((`(,directory ,path)
                (session-mode-test--withdrawal-files "operator" "seat-active-card"))
               (requests nil))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-chat-evidence-request-json)
                   (lambda (_method url _timeout payload)
                     (push payload requests)
                     (if (string-suffix-p "/turn-notice" url)
                         '(:status 200 :json (:result "queued"))
                       '(:status 200 :json (:record (:id "act:effect")))))))
          (session-mode--process-withdrawals path)
          (session-mode--process-withdrawals path)
          (should (= (length requests) 2))
          (let ((withdrawal (cl-find-if
                             (lambda (payload) (alist-get 'interpretation-id payload))
                             requests))
                (notice (cl-find-if
                         (lambda (payload) (alist-get 'notice-id payload))
                         requests)))
            (should-not (alist-get 'target withdrawal))
            (should (equal (alist-get 'idempotency-key withdrawal)
                           "turn-stable:s1:0"))
            (should (equal (alist-get 'notice-id notice) "turn-stable:s1:0")))
          (let* ((json-object-type 'alist) (json-array-type 'list)
                 (outcome (car (alist-get 'withdrawal_effects (json-read-file path)))))
            (should (= (alist-get 'status outcome) 200))
            (should (equal (alist-get 'effect_id outcome) "act:effect"))))
      (delete-directory directory t))))

(ert-deftest session-mode-withdraw-reap-records-refusals-and-null-target ()
  (clrhash session-mode--withdrawal-disabled-messaged-sessions)
  (pcase-let* ((`(,directory ,path)
               (session-mode-test--withdrawal-files "operator" "act:target"))
               (messages nil) (request nil) (notice nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'agent-chat-evidence-request-json)
                     (lambda (_method url _timeout payload)
                       (if (string-suffix-p "/turn-notice" url)
                           (progn (setq notice payload)
                                  '(:status 200 :json (:result "queued")))
                         (setq request payload)
                         '(:status 403 :json (:reason "no-grant")))))
                    ((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (push (apply #'format format-string args) messages))))
            (session-mode--process-withdrawals path))
          (should (equal (alist-get 'target request) "act:target"))
          (should (equal (alist-get 'kind notice) "no-grant"))
          (should (= (length messages) 1))
          (let* ((json-object-type 'alist) (json-array-type 'list)
                 (outcome (car (alist-get 'withdrawal_effects (json-read-file path)))))
            (should (= (alist-get 'status outcome) 403))
            (should (equal (alist-get 'reason outcome) "no-grant"))))
      (delete-directory directory t)))
  (pcase-let* ((`(,directory ,path)
                (session-mode-test--withdrawal-files "operator" nil))
               (notice nil))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-chat-evidence-request-json)
                   (lambda (_method _url _timeout payload)
                     (setq notice payload)
                     '(:status 200 :json (:result "queued")))))
          (session-mode--process-withdrawals path)
          (should (equal (alist-get 'kind notice) "unresolved"))
          (let* ((json-object-type 'alist) (json-array-type 'list)
                 (outcome (car (alist-get 'withdrawal_effects (json-read-file path)))))
            (should (= (alist-get 'status outcome) 422))
            (should (equal (alist-get 'reason outcome) "target-unresolved"))))
      (delete-directory directory t))))

(ert-deftest session-mode-withdraw-reap-skips-non-operator ()
  (pcase-let* ((`(,directory ,path)
                (session-mode-test--withdrawal-files "harness" "act:target"))
               (called nil))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-chat-evidence-request-json)
                   (lambda (&rest _) (setq called t))))
          (session-mode--process-withdrawals path)
          (should-not called)
          (let ((json-object-type 'alist))
            (should-not (alist-get 'withdrawal_effects (json-read-file path)))))
      (delete-directory directory t))))

(ert-deftest session-mode-withdraw-reap-records-route-422-without-retry ()
  (pcase-let* ((`(,directory ,path)
                (session-mode-test--withdrawal-files "operator" "act:hidden"))
               (calls 0))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-chat-evidence-request-json)
                   (lambda (_method url &rest _)
                     (setq calls (1+ calls))
                     (if (string-suffix-p "/turn-notice" url)
                         '(:status 200 :json (:result "queued"))
                       '(:status 422 :json (:reason "target-not-visible"))))))
          (session-mode--process-withdrawals path)
          (session-mode--process-withdrawals path)
          (should (= calls 2))
          (let* ((json-object-type 'alist) (json-array-type 'list)
                 (outcome (car (alist-get 'withdrawal_effects (json-read-file path)))))
            (should (= (alist-get 'status outcome) 422))
            (should (equal (alist-get 'reason outcome) "target-not-visible"))))
      (delete-directory directory t))))

(ert-deftest session-mode-withdraw-reap-no-grant-message-once-per-session ()
  (clrhash session-mode--withdrawal-disabled-messaged-sessions)
  (pcase-let* ((`(,directory-a ,path-a)
                (session-mode-test--withdrawal-files "operator" "act:a"))
               (`(,directory-b ,path-b)
                (session-mode-test--withdrawal-files "operator" "act:b"))
               (messages nil))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-chat-evidence-request-json)
                   (lambda (&rest _) '(:status 403 :json (:reason "no-grant"))))
                  ((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (push (apply #'format format-string args) messages))))
          (session-mode--process-withdrawals path-a)
          (session-mode--process-withdrawals path-b)
          (should (= (length messages) 1)))
      (delete-directory directory-a t)
      (delete-directory directory-b t))))

(ert-deftest session-mode-withdraw-reap-error-is-recorded-and-cleared ()
  ;; P12-5-9: the error was discarded and health said ok.
  (pcase-let ((`(,directory ,path)
               (session-mode-test--withdrawal-files "operator" "seat-active-card")))
    (unwind-protect
        (let ((session-mode--analysis-health nil) (fail t))
          (cl-letf (((symbol-function 'session-mode--process-withdrawals)
                     (lambda (_path) (when fail (error "route failed"))))
                    ((symbol-function 'session-mode--retry-pending-withdrawal-notices)
                     #'ignore)
                    ((symbol-function 'message) #'ignore)
                    ((symbol-function 'force-mode-line-update) #'ignore))
            (session-mode--handle-reap-output path "1 analyzed")
            (should (eq session-mode--analysis-health 'failing))
            (let ((err (alist-get 'withdrawal_processing_error
                                  (json-read-file path))))
              (should (equal "route failed" (alist-get 'message err))))
            (setq fail nil)
            (session-mode--handle-reap-output path "1 analyzed")
            (should (eq session-mode--analysis-health 'ok))
            (should-not (assq 'withdrawal_processing_error (json-read-file path)))))
      (delete-directory directory t))))

(ert-deftest session-mode-withdraw-notice-kind-mapping-is-closed ()
  (should (equal "effect" (alist-get 'kind
                                     (session-mode--withdrawal-notice
                                      '((status . 200) (effect_id . "act:e"))))))
  (should (equal "no-grant" (alist-get 'kind
                                       (session-mode--withdrawal-notice
                                        '((status . 403) (reason . "no-grant"))))))
  (dolist (reason '("target-unresolved" "target-not-visible"))
    (should (equal "unresolved" (alist-get 'kind
                                           (session-mode--withdrawal-notice
                                            `((status . 422) (reason . ,reason)))))))
  ;; Transport failures, other refusals, conflicts, store errors, and a 200
  ;; lacking its effect id do not assert a notice.
  (dolist (outcome '(((status . 0) (reason . "timeout"))
                     ((status . 403) (reason . "other"))
                     ((status . 409) (reason . "conflict"))
                     ((status . 500) (reason . "store"))
                     ((status . 200))))
    (should-not (session-mode--withdrawal-notice outcome))))

(defun session-mode-test--put-withdrawal-outcome (path outcome)
  (let ((record (with-temp-buffer
                  (insert-file-contents path)
                  (json-parse-buffer :object-type 'alist :array-type 'array
                                     :null-object :null :false-object :false))))
    (setf (alist-get 'withdrawal_effects record) (vector outcome))
    (session-mode--write-analysis-record path record)))

(ert-deftest session-mode-withdraw-notice-non-2xx-remains-pending ()
  (pcase-let* ((`(,directory ,path)
                (session-mode-test--withdrawal-files "operator" "act:target"))
               (outcome '((fragment_id . "s1:0") (status . 403)
                          (reason . "no-grant")
                          (idempotency_key . "turn-stable:s1:0"))))
    (unwind-protect
        (progn
          (session-mode-test--put-withdrawal-outcome path outcome)
          (cl-letf (((symbol-function 'agent-chat-evidence-request-json)
                     (lambda (&rest _) '(:status 503 :json (:reason "busy")))))
            (session-mode--process-withdrawals path))
          (let* ((json-object-type 'alist) (json-array-type 'list)
                 (saved (car (alist-get 'withdrawal_effects (json-read-file path)))))
            (should-not (alist-get 'header_notice_published_at saved))
            (should (= 1 (alist-get 'header_notice_attempts saved)))))
      (delete-directory directory t))))

(ert-deftest session-mode-withdraw-notice-repl-line-and-second-reap-are-once ()
  (pcase-let* ((`(,directory ,path)
                (session-mode-test--withdrawal-files "operator" "act:target"))
               (outcome '((fragment_id . "s1:0") (status . 200)
                          (effect_id . "act:e")
                          (idempotency_key . "turn-stable:s1:0")))
               (chat (generate-new-buffer " *withdraw-notice-seat*"))
               (posts 0) (lines nil))
    (unwind-protect
        (progn
          (session-mode-test--put-withdrawal-outcome path outcome)
          (with-current-buffer chat
            (setq-local agent-chat--agent-id "claude-17")
            (setq-local agent-chat--session-id "session-17"))
          (cl-letf (((symbol-function 'agent-chat-evidence-request-json)
                     (lambda (_method _url _timeout payload)
                       (setq posts (1+ posts))
                       (should (equal (alist-get 'kind payload) "effect"))
                       (should (equal (alist-get 'effect-id payload) "act:e"))
                       '(:status 200 :json (:result "queued"))))
                    ((symbol-function 'agent-chat-insert-message)
                     (lambda (_speaker text) (push (cons (current-buffer) text) lines))))
            (session-mode--process-withdrawals path)
            (session-mode--process-withdrawals path))
          (should (= posts 1))
          (should (equal lines `((,chat . "withdraw inferred: effect act:e (undo to reverse)"))))
          (let* ((json-object-type 'alist) (json-array-type 'list)
                 (saved (car (alist-get 'withdrawal_effects (json-read-file path)))))
            (should (alist-get 'header_notice_published_at saved))
            (should (alist-get 'repl_notice_delivered_at saved))))
      (when (buffer-live-p chat) (kill-buffer chat))
      (delete-directory directory t))))

(ert-deftest session-mode-withdraw-notice-absent-buffer-retries-then-stops ()
  (pcase-let* ((`(,directory ,path)
                (session-mode-test--withdrawal-files "operator" "act:target"))
               (outcome '((fragment_id . "s1:0") (status . 403)
                          (reason . "no-grant")
                          (idempotency_key . "turn-stable:s1:0")))
               (posts 0))
    (unwind-protect
        (let ((session-mode-withdrawal-notice-max-attempts 2))
          (session-mode-test--put-withdrawal-outcome path outcome)
          (cl-letf (((symbol-function 'agent-chat-evidence-request-json)
                     (lambda (&rest _) (setq posts (1+ posts))
                       '(:status 503 :json (:reason "busy")))))
            (dotimes (_ 3) (session-mode--process-withdrawals path)))
          (should (= posts 2))
          (let* ((json-object-type 'alist) (json-array-type 'list)
                 (saved (car (alist-get 'withdrawal_effects (json-read-file path)))))
            (should-not (alist-get 'repl_notice_delivered_at saved))
            (should (= 2 (alist-get 'repl_notice_attempts saved)))
            (should (equal "no-matching-buffer"
                           (alist-get 'repl_notice_give_up_reason saved)))
            (should (= 2 (alist-get 'header_notice_attempts saved)))
            (should (alist-get 'header_notice_give_up_reason saved))))
      (delete-directory directory t))))

(defun session-mode-test--init ()
  ;; The real chat initializer establishes prompt markers and text properties.
  (agent-chat-init-buffer '(:title "tag-test" :agent-name "codex"))
  (session-mode-turn-tags-mode 1))

(ert-deftest session-mode-tags-real-chat-insertion-preserves-text ()
  (with-temp-buffer
    (session-mode-test--init)
    (insert "I agree with Foo but I disagree with Bar")
    (let ((before (buffer-string))
          (marker (marker-position agent-chat--input-start)))
      (session-mode-turn-tags-refresh)
      (should (equal session-mode--draft-tags '("approve" "disagree")))
      (should (equal before (buffer-string)))
      (should (= marker (marker-position agent-chat--input-start)))
      ;; Real incoming message insertion shifts the existing draft marker.
      (agent-chat-insert-message "codex" "I approve")
      (session-mode-turn-tags-refresh)
      (should (equal session-mode--draft-tags '("approve" "disagree")))
      (should-not session-mode--sent-tag-overlays)
      (let ((text (buffer-substring-no-properties agent-chat--input-start (point-max))))
        (delete-region agent-chat--input-start (point-max))
        (agent-chat-insert-message "joe" text)
        (should-not session-mode--draft-tags)
        (should (= 2 (length session-mode--sent-tag-overlays)))
        (should (equal (reverse (mapcar (lambda (o) (overlay-get o 'session-mode-turn-tag))
                                       session-mode--sent-tag-overlays))
                       '("approve" "disagree")))))
    (session-mode-turn-tags-mode -1)
    (should-not session-mode--draft-tag-overlays)
    (should-not session-mode--sent-tag-overlays)
    (should-not session-mode--tag-timer)))

(ert-deftest session-mode-tags-edit-and-clear-stale-draft ()
  (with-temp-buffer
    (session-mode-test--init)
    (insert "I agree")
    (session-mode-turn-tags-refresh)
    (should (equal session-mode--draft-tags '("approve")))
    (delete-region agent-chat--input-start (point-max))
    (insert "I disagree")
    (session-mode-turn-tags-refresh)
    (should (equal session-mode--draft-tags '("disagree")))
    (delete-region agent-chat--input-start (point-max))
    (session-mode-turn-tags-refresh)
    (should-not session-mode--draft-tags)
    (should-not session-mode--draft-tag-overlays)))

(ert-deftest session-mode-tags-timers-independent-and-cleaned ()
  (let ((a (generate-new-buffer " *tag-a*")) (b (generate-new-buffer " *tag-b*")) ta tb)
    (unwind-protect
        (progn
          (with-current-buffer a (session-mode-test--init) (insert "I agree")
                               (setq ta session-mode--tag-timer))
          (with-current-buffer b (session-mode-test--init) (insert "I disagree")
                               (setq tb session-mode--tag-timer))
          (should (timerp ta)) (should (timerp tb)) (should-not (eq ta tb))
          (kill-buffer a)
          (should-not (memq ta timer-idle-list))
          (should (memq tb timer-idle-list)))
      (when (buffer-live-p a) (kill-buffer a))
      (when (buffer-live-p b) (kill-buffer b))) ))

(ert-deftest session-mode-tags-reject-foreign-input-marker ()
  (let ((other (generate-new-buffer " *foreign-prompt*")))
    (unwind-protect
        (with-temp-buffer
          (setq-local agent-chat--input-start (with-current-buffer other (point-marker)))
          (insert "I agree")
          (session-mode-turn-tags-mode 1)
          (should-not session-mode--draft-tags)
          (should-not session-mode--draft-tag-overlays))
      (kill-buffer other))))

(ert-deftest session-mode-tags-draft-skips-transcript-refresh ()
  (with-temp-buffer
    (session-mode-test--init)
    (insert "I agree")
    (session-mode--after-change (marker-position agent-chat--input-start) (point-max) 0)
    (should-not session-mode--idle-timer)))

(ert-deftest session-mode-tags-global-covers-new-chat ()
  (let ((global-session-mode-turn-tags-mode t))
    (with-temp-buffer
      (agent-chat-init-buffer '(:title "new tag chat" :agent-name "codex"))
      (should session-mode-turn-tags-mode)
      (insert "rather than")
      (session-mode-turn-tags-refresh)
      (should (equal session-mode--draft-tags '("redirect"))))))

(defmacro session-mode-test--with-rules (&rest body)
  "Keep all persistence tests out of the user's real vocabulary."
  `(let* ((directory (make-temp-file "turn-rules-test-" t))
          (session-mode-turn-rules-file (expand-file-name "rules.json" directory))
          (session-mode--vocabulary-loaded-file nil)
          (session-mode-turn-corrections nil)
          (session-mode-turn-vocabulary '(("agree" "I agree"))))
     (unwind-protect (progn ,@body) (delete-directory directory t))))

(ert-deftest session-mode-correction-labels-sentences-not-words ()
  (session-mode-test--with-rules
   (with-temp-buffer
     (session-mode-test--init)
     (insert "I agree with Foo. We could extend it.\n!c approve extend")
     (agent-chat-send-input (lambda (&rest _) (ert-fail "Correction invoked agent")) "codex")
     (should (equal session-mode--draft-tags '("approve" "extend")))
     (should (equal (buffer-substring-no-properties agent-chat--input-start (point-max))
                    "I agree with Foo. We could extend it.\n"))
     (should-not (assoc "agree" session-mode-turn-vocabulary))
     (should (member "i agree" (cdr (assoc "approve" session-mode-turn-vocabulary))))
     ;; This is the exact previous misunderstanding: no extend -> approve rule.
     (should-not (member "extend" (cdr (assoc "approve" session-mode-turn-vocabulary))))
     (let* ((record (car session-mode-turn-corrections))
            (pairs (alist-get 'sentences record)))
       (should (equal (mapcar (lambda (pair) (alist-get 'label pair)) (append pairs nil))
                      '("approve" "extend"))))
     (setq session-mode-turn-vocabulary nil session-mode-turn-corrections nil
           session-mode--vocabulary-loaded-file nil)
     (session-mode--load-live-vocabulary)
     (should (= 1 (length session-mode-turn-corrections)))
     (should (assoc "approve" session-mode-turn-vocabulary)))))

(ert-deftest session-mode-correction-mismatch-is-not-consumed ()
  (session-mode-test--with-rules
   (with-temp-buffer
     (session-mode-test--init)
     (insert "One sentence.\n!c disagree redirect explain")
     (let ((before (buffer-string)))
       (should-error (agent-chat-send-input (lambda (&rest _) (ert-fail "Invoked agent")) "codex") :type 'user-error)
       (should (equal before (buffer-string)))
       (should-not (file-exists-p session-mode-turn-rules-file))))))

(ert-deftest session-mode-correction-standalone-targets-latest-operator-only ()
  (session-mode-test--with-rules
   (with-temp-buffer
     (session-mode-test--init)
     (agent-chat-insert-message "joe" "I agree. Please expand.")
     (agent-chat-insert-message "codex" "An assistant response.")
     (insert "!c approve extend")
     (agent-chat-send-input (lambda (&rest _) (ert-fail "Invoked agent")) "codex")
     (should (equal (sort (delete-dups (mapcar
                                       (lambda (o) (overlay-get o 'session-mode-turn-tag))
                                       session-mode--sent-tag-overlays)) #'string<)
                    '("approve" "extend")))
     (should (equal (alist-get 'text (car session-mode-turn-corrections))
                    "I agree. Please expand."))
     (should (string-empty-p (buffer-substring-no-properties agent-chat--input-start (point-max)))))))

(ert-deftest session-mode-correction-save-failure-preserves-state ()
  (session-mode-test--with-rules
   (with-temp-buffer
     (session-mode-test--init)
     (with-temp-file session-mode-turn-rules-file (insert "occupied"))
     (setq session-mode-turn-rules-file (concat session-mode-turn-rules-file "/rules.json")
           session-mode--vocabulary-loaded-file session-mode-turn-rules-file)
     (insert "I agree.\n!c approve")
     (let ((before (buffer-string)) (rules (copy-tree session-mode-turn-vocabulary)))
       (should-error (agent-chat-send-input (lambda (&rest _) (ert-fail "Invoked agent")) "codex") :type 'file-error)
       (should (equal before (buffer-string)))
       (should (equal rules session-mode-turn-vocabulary))
       (should-not session-mode-turn-corrections)))))

(ert-deftest session-mode-tags-never-insert-layout-changing-display-text ()
  (with-temp-buffer
    (session-mode-test--init)
    (insert "I agree with Foo.\nI disagree with Bar.")
    (let ((before (buffer-string)) (position (point))
          (modified (buffer-chars-modified-tick)))
      (session-mode-turn-tags-refresh)
      (should (= position (point)))
      (should (= modified (buffer-chars-modified-tick)))
      (should (equal before (buffer-string)))
      (should session-mode--draft-tag-overlays)
      (agent-chat-insert-message "joe" "I agree with Foo.")
      (dolist (ov (append session-mode--draft-tag-overlays session-mode--sent-tag-overlays))
        (should (< (overlay-start ov) (overlay-end ov)))
        (dolist (prop '(before-string after-string display line-prefix wrap-prefix))
          (should-not (overlay-get ov prop)))))))

(ert-deftest session-mode-intent-conjunction-alone-is-not-a-classification ()
  (should-not (session-mode--turn-matches "but"))
  (should-not (session-mode--turn-matches "The notebook is small but readable.")))

(ert-deftest session-mode-intent-real-mixed-request-has-specific-evidence ()
  (should (equal (mapcar (lambda (hit) (nth 2 hit))
                        (session-mode--turn-matches
                         "The new diagram looks good, but we should ask codex-28 to update the shared diagram."))
                 '("approve" "delegate")))
  (should (member "clarify" (mapcar (lambda (hit) (nth 2 hit))
                                  (session-mode--turn-matches "Can you please help me understand the situation?")))))

(ert-deftest session-mode-structure-retains-unmatched-sentences-and-offsets ()
  (let* ((text "  I agree with Foo.\nA naïve 🐈 needs a different route.")
         (record (session-mode--structure-turn text))
         (sentences (append (alist-get 'sentences record) nil)))
    (should (equal (append (alist-get 'unmatched record) nil) '("s2")))
    (should (equal (alist-get 'status (cadr sentences)) "unresolved"))
    (dolist (sentence sentences)
      (should (equal (substring text (alist-get 'start sentence) (alist-get 'end sentence))
                     (alist-get 'text sentence))))))

(ert-deftest session-mode-structure-real-send-keeps-visible-text-and-evidence ()
  (let ((session-mode-turn-analysis-directory (make-temp-file "turn-analysis-test" t)))
    (unwind-protect
        (with-temp-buffer
          (session-mode-test--init)
          (let ((text "A novel structural request.") sent observed callback)
            ;; Only lifecycle effects unrelated to sending are disabled. Real
            ;; agent-chat--start-turn, insertion and our advice execute together.
            (cl-letf (((symbol-function 'agent-chat--maybe-auto-clock-from-turn) #'ignore)
                      ((symbol-function 'agent-chat-start-turn-commit-window!) #'ignore))
              (agent-chat--start-turn
               (lambda (prompt reply) (setq sent prompt callback reply) nil)
               "test-agent" (list :before-send (lambda (s) (setq observed s)))
               text "joe" 'operator))
            (should (equal observed text))
            (should (equal session-mode--last-operator-text text))
            (should (string-prefix-p text sent))
            (should (string-match-p "structural analysis request" sent))
            (should-not (string-match-p "structural analysis request" (buffer-string)))
            (should callback)
            (should (file-exists-p session-mode--last-analysis-request))
            (should-not (file-exists-p (concat session-mode--last-analysis-request ".analysis.json")))
            (let* ((json-object-type 'alist)
                   (record (json-read-file session-mode--last-analysis-request)))
              (should (equal (alist-get 'analysis_status record) "requested"))
              (should (equal (alist-get 'source_text record) text))
              (should (= (alist-get 'vocabulary_version record)
                         session-mode-turn-vocabulary-version))
              (should (= (alist-get 'interpretation_version record)
                         session-mode-turn-interpretation-version))
              (should (equal (alist-get 'turn_id record) "test-agent-turn-1")))))
      (delete-directory session-mode-turn-analysis-directory t))))

(ert-deftest session-mode-structure-does-not-analyze-agent-bells ()
  (with-temp-buffer
    (session-mode-test--init)
    (let ((call #'ignore) captured)
      (session-mode--analyze-start-turn
       (lambda (actual &rest _) (setq captured actual)) call "agent" nil "No cues here" "continuation" 'unsolicited)
      (should (eq captured call))
      (should-not session-mode--last-analysis-request))))

(ert-deftest session-mode-structure-inferred-spans-do-not-reflow ()
  (let ((session-mode-turn-analysis-directory (make-temp-file "turn-display-test" t)))
    (unwind-protect
        (with-temp-buffer
          (session-mode-test--init)
          (agent-chat-insert-message "joe" "An unfamiliar request.")
          (let ((path (session-mode--record-turn "An unfamiliar request."))
                (before (buffer-string)))
            (with-temp-file (concat path ".analysis.json")
              (insert (json-encode
                       '((status . "analyzed") (source_text . "An unfamiliar request.") (labeller . "test-agent")
                         (sentences . [((fragments . [((start . 0) (end . 21) (text . "An unfamiliar request")
                                                      (intent . "propose") (rationale . "fixture")
                                                      (display_cues . [((start . 3) (end . 13) (text . "unfamiliar"))]))]))])))))
            (session-mode--display-analysis path)
            (should (equal before (buffer-string)))
            (should (= 1 (length session-mode--sent-tag-overlays)))
            (dolist (ov session-mode--sent-tag-overlays)
              (should (equal (overlay-get ov 'session-mode-turn-tag) "propose"))
              (should (equal (buffer-substring-no-properties (overlay-start ov) (overlay-end ov)) "unfamiliar"))
              (should-not (overlay-get ov 'after-string))
              (should-not (overlay-get ov 'display)))))
      (delete-directory session-mode-turn-analysis-directory t))))

(ert-deftest session-mode-structure-cued-turn-does-not-request-extra-analysis ()
  (let ((session-mode-turn-analysis-policy 'unmatched)
        (session-mode-turn-analysis-directory (make-temp-file "turn-cued-test" t)))
    (unwind-protect
        (with-temp-buffer
          (session-mode-test--init)
          (let (sent)
            (session-mode--analyze-start-turn
             (lambda (call _name _hooks text _speaker _origin) (funcall call text #'ignore))
             (lambda (prompt _callback) (setq sent prompt)) "agent" nil "I agree." "joe" 'operator)
            (should (equal sent "I agree."))
            (let* ((json-object-type 'alist)
                   (record (json-read-file session-mode--last-analysis-request)))
              (should (equal (alist-get 'analysis_status record) "not-requested")))))
      (delete-directory session-mode-turn-analysis-directory t))))

(ert-deftest session-mode-structure-storage-failure-still-delivers-turn ()
  ;; A real non-directory parent causes the real file operation to fail.
  (let* ((file (make-temp-file "turn-storage-failure"))
         (session-mode-turn-analysis-directory (expand-file-name "child" file)))
    (unwind-protect
        (with-temp-buffer
          (session-mode-test--init)
          (let (sent warning)
            (cl-letf (((symbol-function 'display-warning)
                       (lambda (_type message &rest _) (setq warning message))))
              (session-mode--analyze-start-turn
               (lambda (call _name _hooks text _speaker _origin) (funcall call text #'ignore))
               (lambda (prompt _callback) (setq sent prompt)) "agent" nil "Unfamiliar request." "joe" 'operator))
            (should (equal sent "Unfamiliar request."))
            (should (string-match-p "NOT recorded" warning))
            (should-not session-mode--last-analysis-request)))
      (delete-file file))))

(ert-deftest session-mode-redirection-cues-while-typing ()
  (with-temp-buffer
    (session-mode-test--init)
    (insert "Even if the whole turn is processed I would want keyword based analysis.")
    (session-mode-turn-tags-refresh)
    (should (member "redirect" session-mode--draft-tags))
    (should (equal (mapcar (lambda (ov) (buffer-substring-no-properties (overlay-start ov) (overlay-end ov)))
                          session-mode--draft-tag-overlays) '("I would want")))))

(ert-deftest session-mode-structure-default-interprets-even-cued-turns ()
  (let ((session-mode-turn-analysis-policy 'all))
    (should (session-mode--analysis-requested-p (session-mode--structure-turn "I agree.")))))

(ert-deftest session-mode-pattern-help-uses-target-and-fit-not-opener ()
  (let ((help (session-mode--fragment-help
               '((intent . "verify") (target . "remaining proof obligations")
                 (pattern_refs . (((id . "agent/evidence-over-assertion")
                                   (rationale . "Require proof artifacts as evidence for completion."))))) "test-agent")))
    (should (string-match-p "remaining proof obligations" help))
    (should (string-match-p "Flexiarg candidate agent/evidence-over-assertion" help))
    (should (string-match-p "proof artifacts" help)))
  (should (string-match-p "No justified flexiarg alignment"
                         (session-mode--fragment-help '((intent . "propose") (target . "an experiment")) "test-agent"))))

(ert-deftest session-mode-navigation-loads-late-analysis-without-repainting-typing ()
  (let ((session-mode-turn-analysis-directory (make-temp-file "late-analysis" t)))
    (unwind-protect
        (with-temp-buffer
          (session-mode-test--init)
          (agent-chat-insert-message "joe" "An unfamiliar request.")
          (let ((path (session-mode--record-turn "An unfamiliar request."))
                (this-command 'forward-char))
            (session-mode--refresh-analysis-on-navigation)
            (should-not session-mode--analysis-display-stamp)
            (with-temp-file (concat path ".analysis.json")
              (insert (json-encode
                       '((status . "analyzed") (source_text . "An unfamiliar request.") (labeller . "test-agent")
                         (sentences . [((fragments . [((start . 0) (end . 22) (text . "An unfamiliar request.")
                                                      (intent . "propose") (target . "request")
                                                      (display_cues . [((start . 3) (end . 13) (text . "unfamiliar"))]))]))])))))
            (let ((this-command 'self-insert-command))
              (session-mode--refresh-analysis-on-navigation)
              (should-not session-mode--analysis-display-stamp))
            (session-mode--refresh-analysis-on-navigation)
            (should session-mode--analysis-display-stamp)
            (should (= 1 (length session-mode--sent-tag-overlays)))
            (let ((ov (car session-mode--sent-tag-overlays)))
              (session-mode--refresh-analysis-on-navigation)
              (should (eq ov (car session-mode--sent-tag-overlays))))))
      (delete-directory session-mode-turn-analysis-directory t))))

(ert-deftest session-mode-failure-marker-forces-analysis-and-records-feedback ()
  (let ((session-mode-turn-analysis-directory (make-temp-file "failed-tag-test" t))
        (session-mode-turn-analysis-policy 'never))
    (unwind-protect
        (with-temp-buffer
          (session-mode-test--init)
          (let (sent observed)
            (cl-letf (((symbol-function 'agent-chat--maybe-auto-clock-from-turn) #'ignore)
                      ((symbol-function 'agent-chat-start-turn-commit-window!) #'ignore))
              (agent-chat--start-turn
               (lambda (prompt _reply) (setq sent prompt) nil)
               "test-agent" (list :before-send (lambda (s) (setq observed s)))
               "A new learning loop. !x" "joe" 'operator))
            (should (equal observed "A new learning loop."))
            (should (equal session-mode--last-operator-text observed))
            (should (string-match-p "Operator !x feedback: tagging failed" sent))
            (let* ((json-object-type 'alist) (json-false nil)
                   (record (json-read-file session-mode--last-analysis-request)))
              (should (eq t (alist-get 'tagging_failed record)))
              (should (equal (alist-get 'original_text record) "A new learning loop. !x"))
              (should (equal (alist-get 'analysis_status record) "requested")))))
      (delete-directory session-mode-turn-analysis-directory t))))

(ert-deftest session-mode-failure-marker-is-only-a-final-token ()
  (should (equal (session-mode--split-failure-marker "Try this.\n!x\n") '("Try this." . t)))
  (should (equal (session-mode--split-failure-marker "Could I use !x or something?") '("Could I use !x or something?")))
  (should (equal (session-mode--split-failure-marker "The literal is \"!x\"") '("The literal is \"!x\"")))
  (should-error (session-mode--split-failure-marker "!x") :type 'user-error))

(ert-deftest session-mode-bare-failure-marker-preserves-draft ()
  (with-temp-buffer
    (session-mode-test--init)
    (insert "!x")
    (let ((before (buffer-string)) called)
      (should-error (session-mode--consume-tag-command (lambda (&rest _) (setq called t))) :type 'user-error)
      (should-not called)
      (should (equal before (buffer-string))))))

(ert-deftest session-mode-learned-cue-closes-next-draft-loop-and-persists ()
  (let* ((dir (make-temp-file "learned-cue-test" t))
         (session-mode-turn-rules-file (expand-file-name "rules.json" dir))
         (session-mode--vocabulary-loaded-file nil)
         (session-mode-turn-vocabulary '(("approve" "I agree")))
         (session-mode-turn-corrections nil) (session-mode-learned-cues nil))
    (unwind-protect
        (progn
          (session-mode--learn-analysis-cues
           '((labeller . "test-agent")
             (reusable_cues . (((text . "just testing") (intent . "verify")
                               (rationale . "Explicit test intent"))))) "/example.analysis.json")
          (should (equal (mapcar (lambda (h) (nth 2 h)) (session-mode--turn-matches "just testing")) '("verify")))
          (setq session-mode-turn-vocabulary nil session-mode-learned-cues nil
                session-mode--vocabulary-loaded-file nil)
          (session-mode--load-live-vocabulary)
          (should (equal (alist-get 'source (car session-mode-learned-cues)) "/example.analysis.json"))
          (with-temp-buffer
            (session-mode-test--init)
            (insert "I am just testing again.")
            (session-mode-turn-tags-refresh)
            (should (equal session-mode--draft-tags '("verify"))))
          ;; A conflicting later proposal cannot displace the existing assignment.
          (session-mode--learn-analysis-cues
           '((labeller . "other-agent")
             (reusable_cues . (((text . "just testing") (intent . "clarify")
                               (rationale . "Quoted here"))))) "/other.analysis.json")
          (should (= 1 (length session-mode-learned-cues)))
          (should (equal (mapcar (lambda (h) (nth 2 h)) (session-mode--turn-matches "just testing")) '("verify"))))
      (delete-directory dir t))))

(ert-deftest session-mode-withdraw-reap-keeps-empty-and-false-fields ()
  ;; The turn record is rewritten; [] {} false and null must survive, since the
  ;; Python readers iterate quotes/cues/unmatched.
  (pcase-let* ((`(,directory ,path)
                (session-mode-test--withdrawal-files "operator" "seat-active-card")))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "{\"origin\":\"operator\",\"agent_id\":\"claude-17\","
                    "\"session_id\":\"session-17\",\"turn_id\":\"turn-17\","
                    "\"interpretation_version\":3,\"quotes\":[],\"unmatched\":[],"
                    "\"analysis_dispatch\":{},\"tagging_failed\":false,"
                    "\"note\":null}"))
          (cl-letf (((symbol-function 'agent-chat-evidence-request-json)
                     (lambda (&rest _) '(:status 200 :json (:record (:id "act:e"))))))
            (session-mode--process-withdrawals path))
          (let ((back (with-temp-buffer
                        (insert-file-contents path)
                        (json-parse-buffer :object-type 'hash-table
                                           :null-object :null :false-object :false))))
            (should (equal [] (gethash "quotes" back)))
            (should (equal [] (gethash "unmatched" back)))
            (should (hash-table-p (gethash "analysis_dispatch" back)))
            (should (eq :false (gethash "tagging_failed" back)))
            (should (eq :null (gethash "note" back)))
            (should (= 1 (length (gethash "withdrawal_effects" back))))))
      (delete-directory directory t))))

(ert-deftest session-mode-withdraw-notice-pending-is-retried-by-a-later-reap ()
  ;; A record is reaped once; its failed notice is retried when another
  ;; record's reap completes, not by calling it again directly.
  (setq session-mode--withdrawal-pending-paths nil)
  (pcase-let* ((`(,dir-a ,path-a)
                (session-mode-test--withdrawal-files "operator" "act:a"))
               (`(,dir-b ,path-b)
                (session-mode-test--withdrawal-files "operator" "act:b"))
               (notice-keys nil)
               (up nil))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-chat-evidence-request-json)
                   (lambda (_m url _t payload)
                     (if (string-match-p "turn-notice" url)
                         (progn (push (alist-get 'notice-id payload) notice-keys)
                                (if up '(:status 200 :json (:ok t))
                                  '(:status 503 :json (:reason "busy"))))
                       '(:status 403 :json (:reason "no-grant")))))
                  ((symbol-function 'session-mode--set-analysis-health)
                   (lambda (&rest _) nil))
                  ((symbol-function 'message) (lambda (&rest _) nil)))
          (session-mode--handle-reap-output path-a "analyzed")
          (should (member path-a session-mode--withdrawal-pending-paths))
          (setq up t notice-keys nil)
          (session-mode--handle-reap-output path-b "analyzed")
          (should (= 2 (length notice-keys)))
          (let* ((json-object-type 'alist) (json-array-type 'list)
                 (saved (car (alist-get 'withdrawal_effects (json-read-file path-a)))))
            (should (alist-get 'header_notice_published_at saved))))
      (setq session-mode--withdrawal-pending-paths nil)
      (delete-directory dir-a t)
      (delete-directory dir-b t))))

(ert-deftest session-mode-command-keywords-red-vs-underline ()
  "A whole-message command turns red; the keyword inside prose is underlined;
agent text is left alone."
  (with-temp-buffer
    (insert "joe: yes 2.\n\nclaude: yes, sure\njoe: I think yes is fine, no undo\n"
            "claude: ok\njoe: undo\n")
    (let ((session-mode--overlays nil) (n 0))
      (session-mode--mark-commands (lambda (_k) (setq n (1+ n))))
      (let ((by-type (lambda (type)
                       (sort (mapcar (lambda (o) (buffer-substring-no-properties
                                                  (overlay-start o) (overlay-end o)))
                                     (seq-filter (lambda (o) (equal type (overlay-get o 'session-mode-type)))
                                                 session-mode--overlays))
                             #'string<))))
        (should (= 2 n))
        (should (equal '("undo" "yes 2.") (funcall by-type "command")))
        (should (equal '("undo" "yes") (funcall by-type "command-word")))))))

(ert-deftest agent-chat-acceptance-command-mirrors-clojure-grammar ()
  (dolist (case '(("yes" nil . nil) (" YES. " nil . nil) ("yes 2" nil . "2")
                  ("yes act:offer-a" "act:offer-a" . nil)
                  ("Yes act:offer-a 2." "act:offer-a" . "2")))
    (should (equal (cdr case) (agent-chat--acceptance-command (car case)))))
  (dolist (miss '("yes please" "yes, but" "yes 2 3" "not yes" "yes?" "yes!!"))
    (should-not (agent-chat--acceptance-command miss))))

(ert-deftest session-mode-external-turn-never-takes-the-calling-buffers-evidence-id ()
  ;; emacsclient may evaluate in a REPL buffer whose last acknowledged id is
  ;; another turn; only the id passed in may be recorded.
  (let ((session-mode-turn-analysis-directory
         (make-temp-file "turn-external-test-" t))
        (session-mode-analysis-agent nil)
        (json-object-type 'alist))
    (unwind-protect
        (with-temp-buffer
          (session-mode-test--init)
          (setq-local agent-chat--last-evidence-id "emacs-some-other-turn")
          (let ((no-id (json-read-file
                        (session-mode-record-external-turn
                         "No." "claude-17" "sid" "claude-17-turn-1")))
                (given (json-read-file
                        (session-mode-record-external-turn
                         "No." "claude-17" "sid" "claude-17-turn-2"
                         "emacs-given"))))
            (should (null (alist-get 'evidence_id no-id)))
            (should (equal "emacs-given" (alist-get 'evidence_id given)))))
      (delete-directory session-mode-turn-analysis-directory t))))

(ert-deftest session-mode-marks-take-their-stage-face ()
  (require 'xiaoxiang-preview)
  ;; The two mark tables are kept by hand; they must name the same marks and intents.
  (should (equal (sort (mapcar (lambda (m) (list (nth 0 m) (nth 1 m) (nth 2 m))) session-mode--marks)
                       (lambda (a b) (string< (car a) (car b))))
                 (sort (mapcar (lambda (k) (list (nth 2 k) (nth 1 k) (nth 3 k))) xiaoxiang-mark-keys)
                       (lambda (a b) (string< (car a) (car b))))))
  (with-temp-buffer
    (insert "㊥ (gist) fine. 🈸:yes ㊟ but 🈲 not that")
    (let ((session-mode--missions (make-hash-table :test 'equal))
          (session-mode--patterns (make-hash-table :test 'equal))
          (session-mode--overlays nil))
      (should (= 4 (alist-get 'mark (session-mode--scan))))
      (let ((faces (mapcar (lambda (o) (cons (overlay-get o 'session-mode-token) (overlay-get o 'face)))
                           (seq-filter (lambda (o) (equal "mark" (overlay-get o 'session-mode-type)))
                                       (overlays-in (point-min) (point-max))))))
        (should (eq 'session-mode-mark-annotator-face (cdr (assoc "㊥" faces))))
        (should (eq 'session-mode-mark-act-face (cdr (assoc "🈸" faces))))
        (should (eq 'session-mode-mark-believe-face (cdr (assoc "㊟" faces))))
        (should (eq 'session-mode-mark-evaluate-face (cdr (assoc "🈲" faces))))))))

(ert-deftest session-mode-mark-hydra-is-pbase-ordered-and-coloured ()
  (require 'xiaoxiang-preview)
  (let* ((hint (xiaoxiang--mark-hydra-hint))
         (headings '("PERCEIVE" "BELIEVE" "EVALUATE" "SELECT" "ACT" "OTHER"))
         (positions (mapcar (lambda (heading) (string-match heading hint))
                            headings)))
    (should (equal positions (sort (copy-sequence positions) #'<)))
    ;; Transposed: all stage labels occupy one heading line, not one row each.
    (let ((heading-line (seq-find (lambda (line) (string-match-p "PERCEIVE" line))
                                  (split-string hint "\n"))))
      (dolist (heading headings)
        (should (string-match-p heading heading-line)))
      ;; Each later heading is preceded by an absolute display anchor.  This
      ;; survives fallback-font glyphs whose pixel widths are not cell widths.
      (should
       (equal '((space :align-to 23) (space :align-to 40) (space :align-to 58)
                (space :align-to 77) (space :align-to 96))
              (mapcar (lambda (heading)
                        (get-text-property (1- (string-match heading heading-line))
                                           'display heading-line))
                      (cdr headings)))))
    (dolist (stage xiaoxiang-mark-stage-order)
      (let* ((heading (if (eq stage 'annotator)
                          "OTHER" (upcase (symbol-name stage))))
             (pos (string-match heading hint)))
        (should (eq (xiaoxiang--stage-face stage)
                    (get-text-property pos 'face hint)))))))

(ert-deftest session-mode-mark-hydra-anchors-after-fallback-font-glyphs ()
  "A variable-width mark must not push the following PBASE column rightward."
  (require 'xiaoxiang-preview)
  (let* ((left (xiaoxiang--hydra-cell "_g_ ㊥ gist" 'annotator 23))
         (row (xiaoxiang--hydra-row (list left "_a_ ㊣ approve" "_d_ 🈚 disagree")
                                    '(0 23 40))))
    (should (equal '(space :align-to 23)
                   (get-text-property (1- (string-match "_a_" row)) 'display row)))
    (should (equal '(space :align-to 40)
                   (get-text-property (1- (string-match "_d_" row)) 'display row)))))

(ert-deftest session-mode-turn-tags-paints-marks ()
  (with-temp-buffer
    (insert "㊬ (checked) yes. 🈸:go")
    (session-mode--paint-marks (point-min) (point-max))
    (session-mode--paint-marks (point-min) (point-max))   ; repaint does not stack
    (let ((os (seq-filter (lambda (o) (overlay-get o 'session-mode-mark))
                          (overlays-in (point-min) (point-max)))))
      (should (= 2 (length os)))
      (should (eq 'session-mode-mark-act-face (overlay-get (car (overlays-at 1)) 'face))))))

(defun session-mode-test--rnode-vocabulary ()
  "Write and return a minimal generated R-node vocabulary fixture."
  (let ((path (make-temp-file "rnode-vocabulary-" nil ".json")))
    (with-temp-file path
      (insert
       (json-serialize
        '((version . 1)
          (nodes . [((id . "R14") (label . "Commitment temperature")
                     (stage . "select") (cues . ["for now"]))])))))
    path))

(defun session-mode-test--rnode-overlays ()
  "Return the R-node overlays in the current buffer."
  (seq-filter (lambda (o) (overlay-get o 'session-mode-rnode-tag))
              (overlays-in (point-min) (point-max))))

(ert-deftest session-mode-rnode-tags-only-operator-regions ()
  (let ((path (session-mode-test--rnode-vocabulary))
        (session-mode--rnode-vocabulary nil)
        (session-mode--rnode-vocabulary-key nil)
        (session-mode-turn-vocabulary nil))
    (unwind-protect
        (with-temp-buffer
          (let ((session-mode-rnode-vocabulary-file path)
                (session-mode-rnode-red nil))
            (insert "joe: let's go with option 2 for now\ncodex: for now ok\n")
            (session-mode--paint-rnode-tags (point-min) (point-max))
            (let ((overlays (session-mode-test--rnode-overlays)))
              (should (= 1 (length overlays)))
              (let ((underline (plist-get (overlay-get (car overlays) 'face) :underline)))
                (should (eq 'dots (plist-get underline :style)))
                (should (equal (face-foreground 'session-mode-mark-select-face nil t)
                               (plist-get underline :color))))
              (should (equal "for now" (buffer-substring-no-properties
                                         (overlay-start (car overlays))
                                         (overlay-end (car overlays)))))
              (should (equal
                       "R14 Commitment temperature (SELECT) — cue “for now” — provisional"
                       (overlay-get (car overlays) 'help-echo))))))
      (delete-file path))))

(ert-deftest session-mode-rnode-tags-ignore-quoted-tail ()
  (let ((path (session-mode-test--rnode-vocabulary))
        (session-mode--rnode-vocabulary nil)
        (session-mode--rnode-vocabulary-key nil)
        (session-mode-turn-vocabulary nil))
    (unwind-protect
        (with-temp-buffer
          (let ((session-mode-rnode-vocabulary-file path))
            (insert "joe: consider this\n>>> quoted material\nfor now\ncodex: ok\n")
            (session-mode--paint-rnode-tags (point-min) (point-max))
            (should-not (session-mode-test--rnode-overlays))))
      (delete-file path))))

(ert-deftest session-mode-rnode-tags-repaint-does-not-stack ()
  (let ((path (session-mode-test--rnode-vocabulary))
        (session-mode--rnode-vocabulary nil)
        (session-mode--rnode-vocabulary-key nil)
        (session-mode-turn-vocabulary nil))
    (unwind-protect
        (with-temp-buffer
          (let ((session-mode-rnode-vocabulary-file path))
            (insert "joe: for now\ncodex: ok\n")
            (session-mode--paint-rnode-tags (point-min) (point-max))
            (session-mode--paint-rnode-tags (point-min) (point-max))
            (should (= 1 (length (session-mode-test--rnode-overlays))))))
      (delete-file path))))

(ert-deftest session-mode-rnode-tags-missing-vocabulary-fails-soft ()
  (with-temp-buffer
    (let ((session-mode-rnode-vocabulary-file "/definitely/missing/rnode-vocabulary.json")
          (session-mode--rnode-vocabulary nil)
          (session-mode--rnode-vocabulary-key nil)
          (session-mode--rnode-missing-reported nil)
          messages)
      (insert "joe: for now\ncodex: ok\n")
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (should-not (session-mode--paint-rnode-tags (point-min) (point-max)))
        (should-not (session-mode--paint-rnode-tags (point-min) (point-max))))
      (should-not (session-mode-test--rnode-overlays))
      (should (= 1 (length messages))))))

(ert-deftest session-mode-rnode-tags-red-while-trialling ()
  (let ((path (session-mode-test--rnode-vocabulary))
        (session-mode--rnode-vocabulary nil)
        (session-mode--rnode-vocabulary-key nil)
        (session-mode-turn-vocabulary nil))
    (unwind-protect
        (with-temp-buffer
          (let ((session-mode-rnode-vocabulary-file path)
                (session-mode-rnode-red t))
            (insert "joe: for now\ncodex: for now\n")
            (session-mode--paint-rnode-tags (point-min) (point-max))
            (let ((overlays (session-mode-test--rnode-overlays)))
              (should (= 1 (length overlays)))
              (should (eq 'session-mode-rnode-red-face (overlay-get (car overlays) 'face)))
              ;; red replaces, not adds to, an intent underline on the same words
              (should (> (overlay-get (car overlays) 'priority) 30))
              (should (null (face-attribute 'session-mode-rnode-red-face :underline))))))
      (delete-file path))))

(ert-deftest session-mode-rnode-tags-skip-intent-phrases ()
  (let ((path (session-mode-test--rnode-vocabulary))
        (session-mode--rnode-vocabulary nil)
        (session-mode--rnode-vocabulary-key nil)
        (session-mode-turn-vocabulary nil))
    (unwind-protect
        (with-temp-buffer
          (let ((session-mode-rnode-vocabulary-file path)
                (session-mode-turn-vocabulary '(("defer" "for now"))))
            (insert "joe: for now\ncodex: ok\n")
            (session-mode--paint-rnode-tags (point-min) (point-max))
            (should (null (session-mode-test--rnode-overlays)))))
      (delete-file path))))
