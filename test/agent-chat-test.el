;;; agent-chat-test.el --- Tests for shared chat buffer behavior -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'agent-chat)

(defun agent-chat-test--init-buffer ()
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
           :thinking-prop 'agent-chat-test-thinking))))

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
