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

(ert-deftest turn-stepper-underlines-cues-in-parse ()
  (let ((out (turn-stepper--underline-cues "I refuse to wait for it" '("refuse to wait" "absent"))))
    (should (equal "I refuse to wait for it" (substring-no-properties out)))
    (should (get-text-property 2 'face out))
    (should-not (get-text-property 0 'face out))
    (should-not (get-text-property 17 'face out))))

(ert-deftest turn-stepper-renders-operators ()
  (let ((out (turn-stepper--render-operators
              '((hits . (((ibol . "KEYPRESS") (text . "wait") (cue_intent . "constrain")
                          (cue_text . "I refuse to wait") (agree . nil) (chip_intent . "defer"))
                         ((ibol . "LOOK") (text . "look for") (cue_intent . nil) (agree . nil))))
                (cues_without_operator . (((intent . "clarify") (text . "is that"))))))))
    (should (string-match-p "KEYPRESS.*\"wait\".*chip says defer" out))
    (should (string-match-p "LOOK.*outside 象's marks" out))
    (should-not (string-match-p "is that" out))))

(ert-deftest turn-stepper-keeps-provisional-newest-turns ()
  "小象's provisional parse shows at once; only a bare missing turn drops."
  (let ((f (lambda (st) `((parse . ((status . ,st)))))))
    (should (equal (list (funcall f "analyzed") (funcall f "drafted-provisional"))
                   (turn-stepper--ready-frames
                    (list (funcall f "analyzed") (funcall f "drafted-provisional")
                          (funcall f "missing")))))
    (should (equal (list (funcall f "missing") (funcall f "drafted-provisional"))
                   (turn-stepper--ready-frames
                    (list (funcall f "missing") (funcall f "drafted-provisional")))))))

(ert-deftest turn-stepper-renders-provisional-parse-distinctly ()
  (let* ((parse '((status . "drafted-provisional")
                  (fragments . (((text . "do the thing")
                                 (cues . nil)
                                 (combined . "continue")
                                 (labels . (((source . "小象") (intent . "continue") (weak . t)))))))))
         (out (turn-stepper--render-parse parse)))
    (should (string-match-p "小象, provisional" out))
    (should (string-match-p "小象→continue\\?" out))
    (should (get-text-property 0 'face out))
    (should (eq 'turn-stepper-provisional-face (get-text-property 2 'face out)))))

(ert-deftest turn-stepper-analyzed-parse-renders-as-before ()
  (let* ((parse '((status . "analyzed")
                  (fragments . (((text . "do the thing")
                                 (cues . nil)
                                 (combined . "continue")
                                 (labels . (((source . "象/象-1") (intent . "continue")))))))))
         (out (turn-stepper--render-parse parse)))
    (should (string-match-p "象/象-1→continue" out))
    (should-not (string-match-p "provisional" out))
    (should-not (get-text-property 0 'face out))))

(ert-deftest turn-stepper-reading-on-an-older-frame-keeps-the-index ()
  "象's reading lands for the frame the user is reading back on: the
reload fires and the stepper stays on that frame."
  (let ((buf (get-buffer-create turn-stepper-buffer-name))
        (rec (make-temp-file "ts-rec" nil ".json" "{\"session_id\": \"s1\"}"))
        (fetched nil))
    (unwind-protect
        (cl-letf (((symbol-function 'turn-stepper--start-fetch)
                   (lambda (sid _src quiet) (push (list sid quiet) fetched))))
          (delete-other-windows)
          (display-buffer buf)
          (with-current-buffer buf
            (setq turn-stepper--session-id "s1"
                  turn-stepper--frames '(f0 f1 f2 f3)
                  turn-stepper--index 1))
          (turn-stepper--reading-landed rec)
          (should (equal fetched '(("s1" t))))
          (should (= 1 (turn-stepper--index-after-reload "s1" 4))))
      (delete-other-windows)
      (kill-buffer buf) (delete-file rec))))

(ert-deftest turn-stepper-drops-unread-newest-turns ()
  (let ((f (lambda (st) `((parse . ((status . ,st)))))))
    (should (equal (list (funcall f "missing") (funcall f "analyzed"))
                   (turn-stepper--ready-frames
                    (list (funcall f "missing") (funcall f "analyzed")
                          (funcall f "missing") (funcall f "missing")))))))

(ert-deftest turn-stepper-one-window-beside-its-repl ()
  "Planted in review (claude-17): a stepper in a frame side window and in a
second window must end as ONE window, right of the REPL it reads."
  (let ((src (get-buffer-create " *ts-repl*"))
        (buf (get-buffer-create turn-stepper-buffer-name)))
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer src)
          (display-buffer-in-side-window buf '((side . right)))
          (let ((extra (split-window (selected-window) nil 'below)))
            (set-window-buffer extra buf))
          (should (= 2 (length (get-buffer-window-list buf nil t))))
          (turn-stepper--show-window buf src)
          (let ((ws (get-buffer-window-list buf nil t)))
            (should (= 1 (length ws)))
            (should-not (window-parameter (car ws) 'window-side))
            (should (eq (car ws) (window-in-direction 'right (get-buffer-window src)))))
          ;; Again: reused, not duplicated.
          (turn-stepper--show-window buf src)
          (should (= 1 (length (get-buffer-window-list buf nil t)))))
      (delete-other-windows)
      (kill-buffer buf) (kill-buffer src))))

(ert-deftest turn-stepper-reload-keeps-an-older-frame ()
  (let ((buf (get-buffer-create turn-stepper-buffer-name)))
    (unwind-protect
        (with-current-buffer buf
          (setq turn-stepper--session-id "s1" turn-stepper--frames '(a b c))
          (setq turn-stepper--index 2)
          (should (= 3 (turn-stepper--index-after-reload "s1" 4)))
          (setq turn-stepper--index 0)
          (should (= 0 (turn-stepper--index-after-reload "s1" 4)))
          (should (= 3 (turn-stepper--index-after-reload "other" 4))))
      (kill-buffer buf))))

(ert-deftest turn-stepper-reloads-only-its-own-visible-session ()
  (let ((buf (get-buffer-create turn-stepper-buffer-name))
        (rec (make-temp-file "ts-rec" nil ".json" "{\"session_id\": \"s1\"}"))
        (fetched nil))
    (unwind-protect
        (cl-letf (((symbol-function 'turn-stepper--start-fetch)
                   (lambda (sid _src quiet) (push (list sid quiet) fetched))))
          (with-current-buffer buf (setq turn-stepper--session-id "s1"))
          (turn-stepper--reading-landed rec)          ; not on screen
          (should-not fetched)
          (delete-other-windows)
          (display-buffer buf)
          (turn-stepper--reading-landed rec)
          (should (equal fetched '(("s1" t))))
          (with-current-buffer buf (setq turn-stepper--session-id "s2"))
          (turn-stepper--reading-landed rec)
          (should (= 1 (length fetched))))
      (delete-other-windows)
      (kill-buffer buf) (delete-file rec))))

(ert-deftest turn-stepper-never-takes-over-another-window ()
  "Planted in review (claude-17): with no room to split, display-buffer's
fallback replaced other REPLs' windows in Joe's frames."
  (let ((src (get-buffer-create " *ts-repl*"))
        (other (get-buffer-create " *ts-other*"))
        (buf (get-buffer-create turn-stepper-buffer-name)))
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer other)
          (set-window-buffer (split-window nil nil 'right) src)
          (cl-letf (((symbol-function 'display-buffer-in-direction) (lambda (&rest _) nil)))
            (should-not (turn-stepper--show-window buf src)))
          (should (get-buffer-window other))
          (should (get-buffer-window src))
          (should-not (get-buffer-window buf)))
      (delete-other-windows)
      (kill-buffer buf) (kill-buffer src) (kill-buffer other))))

(defun turn-stepper-test--repo-with-commits ()
  "A scratch repo with commits at 10:00, 10:10 and 10:20 UTC."
  (let* ((root (make-temp-file "ts-code" t))
         (repo (expand-file-name "r" root))
         (process-environment (append '("GIT_AUTHOR_NAME=A" "GIT_AUTHOR_EMAIL=a@x"
                                        "GIT_COMMITTER_NAME=A" "GIT_COMMITTER_EMAIL=a@x")
                                      process-environment)))
    (make-directory repo)
    (let ((default-directory (file-name-as-directory repo)))
      (call-process "git" nil nil nil "init" "-q")
      (dolist (tm '("10:00" "10:10" "10:20"))
        (with-temp-file (expand-file-name "f" repo) (insert tm))
        (call-process "git" nil nil nil "add" "f")
        (let ((process-environment
               (append (list (format "GIT_COMMITTER_DATE=2026-09-29T%s:00Z" tm)
                             (format "GIT_AUTHOR_DATE=2026-09-29T%s:00Z" tm))
                       process-environment)))
          (call-process "git" nil nil nil "commit" "-q" "-m" (concat "at " tm)))))
    root))

(ert-deftest turn-stepper-rewind-pin-and-window ()
  "The pin is the last commit before the turn; the window runs to the next turn.
Planted: a commit AFTER the next turn (10:20) must not be listed."
  (let* ((root (turn-stepper-test--repo-with-commits))
         (turn-stepper-code-root root)
         (frame '((turn . ((at . "2026-09-29T10:05:00Z")))
                  (happened . (((summary . ((event . "turn-commits")
                                            (commits . (((repo . "r")))))))))))
         (plan (turn-stepper--rewind-plan frame "2026-09-29T10:15:00Z"))
         (p (car plan)))
    (unwind-protect
        (progn
          (should (= 1 (length plan)))
          (should (equal "at 10:00" (turn-stepper--git (plist-get p :path) "log" "-1"
                                                       "--format=%s" (plist-get p :pin))))
          (should (equal '("at 10:10") (mapcar (lambda (c) (car (last (split-string c "\t"))))
                                               (plist-get p :commits)))))
      (delete-directory root t))))

(ert-deftest turn-stepper-rewind-nothing-without-commits ()
  (should-not (turn-stepper--rewind-plan '((turn . ((at . "2026-09-29T10:05:00Z")))
                                           (happened . nil))
                                         nil)))

(ert-deftest turn-stepper-failed-reload-retries-then-stops ()
  "Planted: a reload that keeps failing is retried the configured number of
times and then stops, rather than never (futon1b busy) or forever."
  (let ((buf (get-buffer-create turn-stepper-buffer-name))
        (timers nil) (fetches 0)
        (real-make-process (symbol-function 'make-process))
        (turn-stepper-reload-retries 2))
    (unwind-protect
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (_d _r fn &rest args) (push (cons fn args) timers)))
                  ((symbol-function 'make-process)
                   (lambda (&rest plist)
                     (cl-incf fetches)
                     ;; a process that has already failed
                     (let ((p (funcall real-make-process :name "ts-fail" :command '("false"))))
                       (while (process-live-p p) (accept-process-output p 0.05))
                       (set-process-buffer p (plist-get plist :buffer))
                       (funcall (plist-get plist :sentinel) p "exited abnormally")
                       p))))
          (delete-other-windows)
          (with-current-buffer buf (setq turn-stepper--session-id "s1"))
          (display-buffer buf)
          (turn-stepper--start-fetch "s1" nil t)
          (while timers
            (let ((tm (pop timers))) (apply (car tm) (cdr tm))))
          (should (= 3 fetches)))
      (delete-other-windows)
      (kill-buffer buf))))

(defun turn-stepper-test--mixed-repo ()
  "Scratch repo: base 10:00, then this session's, another seat's and an
unsigned commit, each touching its own file."
  (let* ((root (make-temp-file "ts-code" t))
         (repo (expand-file-name "r" root))
         (process-environment (append '("GIT_AUTHOR_NAME=A" "GIT_AUTHOR_EMAIL=a@x"
                                        "GIT_COMMITTER_NAME=A" "GIT_COMMITTER_EMAIL=a@x"
                                        "CLAUDE_CODE_SESSION_ID=")
                                      process-environment)))
    (make-directory repo)
    (let ((default-directory (file-name-as-directory repo)))
      (call-process "git" nil nil nil "init" "-q")
      (cl-loop for (tm file msg) in
               '(("10:00" "base" "base")
                 ("10:10" "mine" "mine\n\nAgent-Session: S")
                 ("10:11" "theirs" "theirs\n\nAgent-Session: OTHER")
                 ("10:14" "kimis" "dispatched\n\nAgent-Session: K\nDispatched-By: claude-17/S")
                 ("10:15" "stranger" "dispatched by someone else\n\nAgent-Session: K2\nDispatched-By: claude-9/OTHER")
                 ("10:12" "joes" "joe at a terminal"))
               do (with-temp-file (expand-file-name file repo) (insert tm))
               (call-process "git" nil nil nil "add" file)
               (let ((process-environment
                      (append (list (format "GIT_COMMITTER_DATE=2026-09-29T%s:00Z" tm)
                                    (format "GIT_AUTHOR_DATE=2026-09-29T%s:00Z" tm))
                              process-environment)))
                 (call-process "git" nil nil nil "commit" "-q" "-m" msg))))
    root))

(defun turn-stepper-test--mixed-plan (root)
  (let ((turn-stepper-code-root root))
    (car (turn-stepper--rewind-plan
          '((turn . ((at . "2026-09-29T10:05:00Z")))
            (happened . (((summary . ((event . "turn-commits")
                                      (commits . (((repo . "r"))))))))))
          nil))))

(ert-deftest turn-stepper-R-reverts-only-this-sessions-commits ()
  "Planted: another seat's commit and an unsigned one sit in the same window."
  (let* ((root (turn-stepper-test--mixed-repo))
         (p (turn-stepper-test--mixed-plan root))
         (repo (plist-get p :path)))
    (unwind-protect
        (progn
          (should (= 5 (length (plist-get p :commits))))
          ;; This session's commit and the one it dispatched; not claude-9's.
          (should (= 2 (length (turn-stepper--own-commits p "S"))))
          (should (equal 2 (plist-get (turn-stepper--revert p "S") :reverted)))
          (should-not (file-exists-p (expand-file-name "mine" repo)))
          (should-not (file-exists-p (expand-file-name "kimis" repo)))
          (should (file-exists-p (expand-file-name "stranger" repo)))
          (should (file-exists-p (expand-file-name "theirs" repo)))
          (should (file-exists-p (expand-file-name "joes" repo))))
      (delete-directory root t))))

(ert-deftest turn-stepper-R-ignores-unrelated-edits ()
  (let* ((root (turn-stepper-test--mixed-repo))
         (p (turn-stepper-test--mixed-plan root))
         (repo (plist-get p :path)))
    (unwind-protect
        (progn
          ;; Another seat mid-edit on an unrelated file: the revert goes ahead.
          (with-temp-file (expand-file-name "theirs" repo) (insert "mid-edit"))
          (should (equal 2 (plist-get (turn-stepper--revert p "S") :reverted)))
          (should (equal "mid-edit" (with-temp-buffer
                                      (insert-file-contents (expand-file-name "theirs" repo))
                                      (buffer-string)))))
      (delete-directory root t))))

(ert-deftest turn-stepper-R-refuses-edits-to-the-same-file ()
  (let* ((root (turn-stepper-test--mixed-repo))
         (p (turn-stepper-test--mixed-plan root))
         (repo (plist-get p :path)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "mine" repo) (insert "mid-edit"))
          (should (string-match-p "mine" (plist-get (turn-stepper--revert p "S") :refused)))
          (should (file-exists-p (expand-file-name "mine" repo))))
      (delete-directory root t))))

(ert-deftest turn-stepper-R-aborts-a-conflict ()
  "A later commit edits the same file: the revert conflicts and is aborted."
  (let* ((root (turn-stepper-test--mixed-repo))
         (repo (expand-file-name "r" root)))
    (unwind-protect
        (let ((default-directory (file-name-as-directory repo))
              (process-environment (append '("GIT_AUTHOR_NAME=A" "GIT_AUTHOR_EMAIL=a@x"
                                             "GIT_COMMITTER_NAME=A" "GIT_COMMITTER_EMAIL=a@x"
                                             "CLAUDE_CODE_SESSION_ID="
                                             "GIT_COMMITTER_DATE=2026-09-29T10:13:00Z")
                                           process-environment)))
          (with-temp-file (expand-file-name "mine" repo) (insert "changed later"))
          (call-process "git" nil nil nil "commit" "-qam" "later edit")
          (let* ((p (turn-stepper-test--mixed-plan root))
                 (head (turn-stepper--git repo "rev-parse" "HEAD")))
            (should (plist-get (turn-stepper--revert p "S") :refused))
            (should (equal head (turn-stepper--git repo "rev-parse" "HEAD")))
            (should (string-empty-p (turn-stepper--git repo "status" "--porcelain")))))
      (delete-directory root t))))

(ert-deftest turn-stepper-reload-adds-new-keys ()
  "Planted in review (claude-17): Joe's Emacs had the maps from the first
load, so r and R were undefined after a reload.  Simulate an old map."
  (let ((turn-stepper-mode-map (make-sparse-keymap))
        (turn-stepper-rewind-mode-map (make-sparse-keymap)))
    (load (locate-library "turn-stepper.el") nil t)
    (should (eq (lookup-key turn-stepper-mode-map (kbd "r")) #'turn-stepper-rewind))
    (should (eq (lookup-key turn-stepper-rewind-mode-map (kbd "R")) #'turn-stepper-rewind-apply))))

(defvar turn-stepper-test--sent nil)

(defun turn-stepper-test--repl ()
  "A fake REPL: two operator turns, replies, a prompt; RET records the input."
  (let ((b (generate-new-buffer "*claude-repl:test*")))
    (with-current-buffer b
      (insert "joe: first turn\nclaude: reply one\njoe: second turn, the one to rewind\nclaude: reply two\n> ")
      (setq-local agent-chat--pending-process nil)
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "RET")
                    (lambda () (interactive)
                      (setq turn-stepper-test--sent
                            (buffer-substring-no-properties
                             (save-excursion (goto-char (point-max)) (line-beginning-position))
                             (point-max)))))
        (use-local-map map)))
    b))

(defun turn-stepper-test--rewind-buffer (source plan)
  (let ((b (get-buffer-create "*象 rewind test*")))
    (with-current-buffer b
      (turn-stepper-rewind-mode)
      (setq turn-stepper--rewind-plan plan
            turn-stepper--rewind-session "S"
            turn-stepper--rewind-context
            (list :first 5 :last 5 :at "2026-09-30T02:40:00Z" :source source
                  :text "second turn, the one to rewind")))
    b))

(ert-deftest turn-stepper-R-cuts-the-repl-and-tells-the-agent ()
  (let* ((root (turn-stepper-test--mixed-repo))
         (plan (list (turn-stepper-test--mixed-plan root)))
         (repl (turn-stepper-test--repl))
         (rw (turn-stepper-test--rewind-buffer repl plan))
         (turn-stepper-test--sent nil))
    (unwind-protect
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                  ((symbol-function 'quit-window) #'ignore))
          (with-current-buffer rw (turn-stepper-rewind-apply))
          (with-current-buffer repl
            (should (string-match-p "reply one" (buffer-string)))
            (should-not (string-match-p "second turn\\|reply two" (buffer-string))))
          (should (string-match-p "\\`\\(> \\)?Operator reverted frame 5 (from the turn at 2026-09-30T02:40:00Z): 2 commit(s) reverted in r"
                                  turn-stepper-test--sent)))
      (kill-buffer repl) (kill-buffer rw) (delete-directory root t))))

(ert-deftest turn-stepper-R-refused-revert-leaves-the-repl-alone ()
  "Planted: a revert that is refused must not cut the REPL or tell the agent."
  (let* ((root (turn-stepper-test--mixed-repo))
         (plan (list (turn-stepper-test--mixed-plan root)))
         (repl (turn-stepper-test--repl))
         (rw (turn-stepper-test--rewind-buffer repl plan))
         (before (with-current-buffer repl (buffer-string)))
         (turn-stepper-test--sent nil))
    (unwind-protect
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
          (with-temp-file (expand-file-name "r/mine" root) (insert "mid-edit"))
          (with-current-buffer rw (turn-stepper-rewind-apply))
          (should (equal before (with-current-buffer repl (buffer-string))))
          (should-not turn-stepper-test--sent))
      (kill-buffer repl) (kill-buffer rw) (delete-directory root t))))

(ert-deftest turn-stepper-R-without-commits-still-cuts ()
  "A turn with no commits (Joe's \"say hi\") still rewinds the REPL."
  (let* ((repl (turn-stepper-test--repl))
         (rw (turn-stepper-test--rewind-buffer repl nil))
         (turn-stepper-test--sent nil))
    (unwind-protect
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                  ((symbol-function 'quit-window) #'ignore))
          (with-current-buffer rw (turn-stepper-rewind-apply))
          (should (string-match-p "no commits of this session to revert" turn-stepper-test--sent))
          (should-not (string-match-p "reply two" (with-current-buffer repl (buffer-string)))))
      (kill-buffer repl) (kill-buffer rw))))

(ert-deftest turn-stepper-follows-its-repl-window ()
  "Joe 2026-09-30: switching the REPL's window to another REPL hides the
stepper; switching back shows it again."
  (let ((repl (get-buffer-create "*claude-repl:a*"))
        (other (get-buffer-create "*claude-repl:b*"))
        (buf (get-buffer-create turn-stepper-buffer-name))
        (turn-stepper--hidden-anchor nil))
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer repl)
          (with-current-buffer buf (setq turn-stepper--source-buffer repl))
          (should (turn-stepper--show-window buf repl))
          (should (get-buffer-window buf))
          ;; Same window, another REPL: the stepper goes.
          (set-window-buffer (get-buffer-window repl) other)
          (turn-stepper--follow-source)
          (should-not (get-buffer-window buf))
          ;; Back to its REPL: it returns, beside it.
          (set-window-buffer (get-buffer-window other) repl)
          (turn-stepper--follow-source)
          (should (get-buffer-window buf))
          (should (eq (window-buffer (window-in-direction turn-stepper-window-side
                                                          (get-buffer-window repl)))
                      buf)))
      (delete-other-windows)
      (mapc #'kill-buffer (list repl other buf)))))

(ert-deftest turn-stepper-other-windows-do-not-hide-it ()
  "Planted: a buffer switch in some unrelated window must not hide the stepper."
  (let ((repl (get-buffer-create "*claude-repl:a*"))
        (other (get-buffer-create "*scratch-x*"))
        (buf (get-buffer-create turn-stepper-buffer-name))
        (turn-stepper--hidden-anchor nil))
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer repl)
          (let ((below (split-window-below)))
            (with-current-buffer buf (setq turn-stepper--source-buffer repl))
            (turn-stepper--show-window buf repl)
            (set-window-buffer below other)
            (turn-stepper--follow-source)
            (should (get-buffer-window buf))))
      (delete-other-windows)
      (mapc #'kill-buffer (list repl other buf)))))

(ert-deftest turn-stepper-renders-ports ()
  (let* ((frame '((turn . ((at . "2026-09-29T11:00:00Z") (text . "t")))
                  (parse . ((status . "missing") (fragments . [])))
                  (patterns . ((matched . []) (rejected . [])
                               (proposed_by_parent . nil)))
                  (happened . [])
                  (ports . ((closed_this_turn . [((act . "a1") (kind . "offer")
                                                  (text . "Reply yes 1 or yes 2"))])
                            (still_open . [((act . "a2") (kind . "ask-action")
                                            (text . "Shall I read P11?")
                                            (since . "2026-09-29T10:45:00Z"))])))))
         (text (turn-stepper--render-frame frame 0 1 "s")))
    (should (string-match-p "closed:" text))
    (should (string-match-p "offer: Reply yes 1 or yes 2" text))
    (should (string-match-p "still open:" text))
    (should (string-match-p "ask-action (15m): Shall I read P11?" text))))

(ert-deftest turn-stepper-no-ports-renders-nothing ()
  (let* ((frame '((turn . ((at . "2026-09-29T11:00:00Z") (text . "t")))
                  (parse . ((status . "missing") (fragments . [])))
                  (patterns . ((matched . []) (rejected . [])
                               (proposed_by_parent . nil)))
                  (happened . [])))
         (text (turn-stepper--render-frame frame 0 1 "s")))
    (should-not (string-match-p "closed:" text))
    (should-not (string-match-p "still open:" text))))
