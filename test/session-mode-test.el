;;; session-mode-test.el --- Local draft tag tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'session-mode)

(ert-deftest session-mode-tags-mixed-and-negative-phrases ()
  (let* ((text "I agree with Foo but I disagree with Bar. I don’t agree with Baz.")
         (hits (session-mode--turn-matches text)))
    (should (equal (mapcar (lambda (h) (nth 2 h)) hits)
                   '("agree" "qualify" "object" "object")))
    (dolist (h hits)
      (should (equal (substring text (car h) (cadr h)) (nth 3 h))))))

(ert-deftest session-mode-tags-boundaries-and-apostrophes ()
  (should-not (session-mode--turn-matches "I agreement butterfly undergo on"))
  (should (equal (mapcar (lambda (h) (nth 2 h))
                        (session-mode--turn-matches "I DON'T AGREE; that's wrong; that’s a good fit"))
                 '("object" "object" "agree"))))

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
      (should (equal session-mode--draft-tags '("agree" "qualify" "object")))
      (should (equal before (buffer-string)))
      (should (= marker (marker-position agent-chat--input-start)))
      ;; Real incoming message insertion shifts the existing draft marker.
      (agent-chat-insert-message "codex" "I approve")
      (session-mode-turn-tags-refresh)
      (should (equal session-mode--draft-tags '("agree" "qualify" "object")))
      (should-not session-mode--sent-tag-overlays)
      (let ((text (buffer-substring-no-properties agent-chat--input-start (point-max))))
        (delete-region agent-chat--input-start (point-max))
        (agent-chat-insert-message "joe" text)
        (should-not session-mode--draft-tags)
        (should (= 4 (length session-mode--sent-tag-overlays)))
        (should (string-match-p (regexp-quote "turn tags: agree + qualify + object")
                                (overlay-get (car session-mode--sent-tag-overlays) 'after-string)))))
    (session-mode-turn-tags-mode -1)
    (should-not session-mode--draft-tag-overlays)
    (should-not session-mode--sent-tag-overlays)
    (should-not session-mode--tag-timer)))

(ert-deftest session-mode-tags-edit-and-clear-stale-draft ()
  (with-temp-buffer
    (session-mode-test--init)
    (insert "I agree")
    (session-mode-turn-tags-refresh)
    (should (equal session-mode--draft-tags '("agree")))
    (delete-region agent-chat--input-start (point-max))
    (insert "I disagree")
    (session-mode-turn-tags-refresh)
    (should (equal session-mode--draft-tags '("object")))
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
     (should (string-match-p (regexp-quote "turn tags: approve + extend")
                            (overlay-get (car session-mode--sent-tag-overlays) 'after-string)))
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
