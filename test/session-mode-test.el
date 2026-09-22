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
