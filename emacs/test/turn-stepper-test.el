;;; turn-stepper-test.el --- ERT tests for turn-stepper.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Fixture-only tests; no live calls to turn_frames.py or futon1b.
;; Run:
;;   emacs -Q --batch -L emacs -l emacs/test/turn-stepper-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'turn-stepper)

(defconst turn-stepper-test--fixture
  (json-encode
   [((turn . ((evidence_id . "emacs-t1")
              (at . "2026-09-29T10:00:00Z")
              (text . "do the thing please and then report back")))
     (parse . ((status . "analyzed")
               (fragments . [((sentence . "s1")
                              (text . "do the thing")
                              (labels . [((source . "象/claude-15")
                                          (intent . "ask-action"))
                                         ((source . "negation")
                                          (intent . "withdraw"))])
                              (combined . nil)
                              (disagree . t))
                             ((sentence . "s1")
                              (text . "then report back")
                              (labels . [((source . "象/claude-15")
                                          (intent . "ask-action"))])
                              (combined . "ask-action")
                              (disagree . :json-false))])))
     (patterns . ((matched . [((id . "discourse/ask-plainly"))])
                  (rejected . [((id . "p/x") (reason . "nope"))])
                  (proposed_by_parent . (("p/parent" . [((id . "p/a")
                                                          (title . "A")
                                                          (fragment . "s1"))])))))
     (happened . [((at . "2026-09-29T10:15:00Z")
                   (type . "coordination")
                   (summary . ((event . "turn-commits")
                               (commits . [((repo . "futon3c")
                                            (sha . "abc123def456")
                                            (author . "Someone Else")
                                            (subject . "fix a thing"))]))))
                  ((at . "2026-09-29T10:20:00Z")
                   (type . "promise/park-made")
                   (summary . ((event . "promise/park-made"))))]))
    ((turn . ((evidence_id . "emacs-t2")
              (at . "2026-09-29T11:00:00Z")
              (text . "now stop")))
     (parse . ((status . "missing") (fragments . [])))
     (patterns . ((matched . []) (rejected . [])
                  (proposed_by_parent . nil)))
     (happened . []))])
  "Two-frame fixture in the shape turn_frames.py emits.")

(ert-deftest turn-stepper-test-parse-frames ()
  (let ((frames (turn-stepper--parse-frames turn-stepper-test--fixture)))
    (should (= 2 (length frames)))
    (should (equal "emacs-t1"
                   (cdr (assq 'evidence_id
                              (cdr (assq 'turn (car frames)))))))))

(ert-deftest turn-stepper-test-render-frame-sections ()
  (let* ((frames (turn-stepper--parse-frames turn-stepper-test--fixture))
         (text (turn-stepper--render-frame (car frames) 0 2 "session-1234")))
    (dolist (heading '("SAID" "PARSE" "PATTERNS" "HAPPENED"))
      (should (string-match-p heading text)))
    (should (string-match-p "do the thing please" text))
    ;; disagreeing fragment renders as "disagree: a / b"
    (should (string-match-p "disagree: ask-action / withdraw" text))
    ;; agreeing fragment shows the combined intent
    (should (string-match-p "\\[ask-action\\] then report back" text))
    ;; sources shown after each fragment
    (should (string-match-p "象/claude-15→ask-action" text))
    ;; patterns
    (should (string-match-p "matched: discourse/ask-plainly" text))
    (should (string-match-p "proposed under p/parent" text))
    (should (string-match-p "rejected: 1" text))
    ;; happened: turn-commits line shows repo, short sha, author, subject
    (should (string-match-p "futon3c abc123de Someone Else: fix a thing" text))
    (should-not (string-match-p "agent's commits" text))))

(ert-deftest turn-stepper-test-render-missing-analysis ()
  (let* ((frames (turn-stepper--parse-frames turn-stepper-test--fixture))
         (text (turn-stepper--render-frame (cadr frames) 1 2 "session-1234")))
    (should (string-match-p "(missing)" text))
    (should (string-match-p "matched: (none)" text))
    (should (string-match-p "rejected: 0" text))
    (should (string-match-p "nothing between this turn" text))))

(ert-deftest turn-stepper-test-n-p-bounds ()
  (should (= 0 (turn-stepper--clamp-index -1 3)))
  (should (= 2 (turn-stepper--clamp-index 5 3)))
  (should (= 1 (turn-stepper--clamp-index 1 3)))
  (should (= 0 (turn-stepper--clamp-index 4 0)))
  ;; stepping in a live stepper buffer stays in range
  (let ((frames (turn-stepper--parse-frames turn-stepper-test--fixture)))
    (with-temp-buffer
      (turn-stepper-mode)
      (setq turn-stepper--session-id "session-1234"
            turn-stepper--frames frames
            turn-stepper--index 0)
      (turn-stepper-previous)
      (should (= 0 turn-stepper--index))
      (turn-stepper-next)
      (turn-stepper-next)
      (turn-stepper-next)
      (should (= 1 turn-stepper--index))
      (should (string-match-p "frame 2 / 2 · session session-" header-line-format)))))

(ert-deftest turn-stepper-test-visit-turn-miss-leaves-point ()
  (let ((source (generate-new-buffer " *turn-stepper-test-source*")))
    (unwind-protect
        (with-current-buffer source
          (insert "some unrelated repl text\n")
          (goto-char (point-min))
          (forward-char 5)
          (let ((before (point)))
            (should-not (turn-stepper--goto-turn-in-buffer
                         "this text does not occur anywhere" source))
            (should (= before (point))))))
    (kill-buffer source)))

(ert-deftest turn-stepper-test-visit-turn-hit ()
  (let ((source (generate-new-buffer " *turn-stepper-test-source*")))
    (unwind-protect
        (progn
          (with-current-buffer source
            (insert "joe: do the thing please and then report back\nagent: ok\n"))
          (should (turn-stepper--goto-turn-in-buffer
                   "do the thing please and then report back" source))
          (with-current-buffer source
            (should (looking-at-p "do the thing"))))
      (kill-buffer source))))

(provide 'turn-stepper-test)
;;; turn-stepper-test.el ends here

(ert-deftest turn-stepper-visit-prefers-the-operator-line ()
  "Planted in review (claude-17): the turn quoted later must not win."
  (with-temp-buffer
    (insert "joe: Please keep the underlines on old turns\n\nclaude: You said \"Please keep the underlines on old turns\"\n")
    (goto-char (point-max))
    (let ((pos (turn-stepper--goto-turn-in-buffer
                "Please keep the underlines on old turns" (current-buffer))))
      (should (= pos 6)))))

(ert-deftest turn-stepper-renders-operators ()
  (let ((out (turn-stepper--render-operators
              '((hits . (((ibol . "KEYPRESS") (text . "wait") (cue_intent . "constrain")
                          (cue_text . "I refuse to wait") (agree . nil) (chip_intent . "defer"))
                         ((ibol . "LOOK") (text . "look for") (cue_intent . nil) (agree . nil))))
                (cues_without_operator . (((intent . "clarify") (text . "is that"))))))))
    (should (string-match-p "KEYPRESS.*\"wait\".*chip says defer" out))
    (should (string-match-p "LOOK.*outside 象's marks" out))
    (should (string-match-p "clarify \"is that\"" out))))
