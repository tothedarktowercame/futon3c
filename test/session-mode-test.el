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
          (session-mode-turn-vocabulary '(("agree" "I agree"))))
     (unwind-protect (progn ,@body) (delete-directory directory t))))

(ert-deftest session-mode-live-command-preserves-unsent-draft-and-saves ()
  (session-mode-test--with-rules
   (with-temp-buffer
     (session-mode-test--init)
     (insert "Please extend this idea.\n!c approve extend")
     ;; Real send entry point: the callback must never run for this local command.
     (agent-chat-send-input (lambda (&rest _) (ert-fail "Command invoked agent")) "codex")
     (should (equal (buffer-substring-no-properties agent-chat--input-start (point-max))
                    "Please extend this idea.\n"))
     (should (equal session-mode--draft-tags '("approve")))
     (should (= agent-chat--turn-counter 0))
     (should (file-exists-p session-mode-turn-rules-file))
     (setq session-mode-turn-vocabulary nil session-mode--vocabulary-loaded-file nil)
     (session-mode--load-live-vocabulary)
     (should (equal (cdr (assoc "approve" session-mode-turn-vocabulary)) '("extend")))
     (session-mode-turn-add-rule "approve" "EXTEND")
     (should (= 1 (length (cdr (assoc "approve" session-mode-turn-vocabulary))))))))

(ert-deftest session-mode-live-malformed-command-never-sends ()
  (session-mode-test--with-rules
   (with-temp-buffer
     (session-mode-test--init)
     (insert "!c approve")
     (should-error (agent-chat-send-input (lambda (&rest _) (ert-fail "Invalid command invoked agent")) "codex") :type 'user-error)
     (should (equal (buffer-substring-no-properties agent-chat--input-start (point-max)) "!c approve"))
     (should-not (file-exists-p session-mode-turn-rules-file)))))

(ert-deftest session-mode-live-save-failure-leaves-draft-and-rules ()
  (session-mode-test--with-rules
   (with-temp-buffer
     (session-mode-test--init)
     ;; A real non-directory parent makes persistence fail, without a stub.
     (with-temp-file session-mode-turn-rules-file (insert "occupied"))
     (setq session-mode-turn-rules-file (concat session-mode-turn-rules-file "/rules.json")
           session-mode--vocabulary-loaded-file session-mode-turn-rules-file)
     (insert "!c approve extend")
     (let ((before (copy-tree session-mode-turn-vocabulary)))
       (should-error (agent-chat-send-input (lambda (&rest _) (ert-fail "Failed save invoked agent")) "codex") :type 'file-error)
       (should (equal session-mode-turn-vocabulary before))
       (should (equal (buffer-substring-no-properties agent-chat--input-start (point-max)) "!c approve extend"))))))

(ert-deftest session-mode-live-multiword-literal-and-ordinary-send-delegation ()
  (session-mode-test--with-rules
   (with-temp-buffer
     (session-mode-test--init)
     (insert "!c redirect take another approach\n\n")
     (agent-chat-send-input (lambda (&rest _) (ert-fail "Command invoked agent")) "codex")
     (should (equal (cdr (assoc "redirect" session-mode-turn-vocabulary)) '("take another approach")))
     (insert "!continue is ordinary text")
     (let (received)
       (session-mode--consume-tag-command (lambda (&rest args) (setq received args)) 'normal 'args)
       (should (equal received '(normal args)))))))
