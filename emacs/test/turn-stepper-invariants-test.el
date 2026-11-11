;;; turn-stepper-invariants-test.el --- ERT tests for the stepper attachment invariant -*- lexical-binding: t; -*-

;;; Commentary:
;; M-repl-circus invariant 1 (Joe, 2026-11-11): indexed steppers, one per
;; REPL, plus the Reazon attachment invariant.  Frames come from the recorded
;; fixture in turn-stepper-test.el (loaded for `turn-stepper-test--fixture'),
;; not invented ones.  Run:
;;   emacs -Q --batch -L emacs \
;;     -l emacs/test/turn-stepper-test.el \
;;     -l emacs/test/turn-stepper-invariants-test.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'turn-stepper)
(require 'turn-stepper-invariants)

(defmacro turn-stepper-invariants-test--with-repl (name agent session &rest body)
  "Create a REPL buffer named NAME with AGENT and SESSION locals; run BODY."
  (declare (indent 3))
  `(let ((repl (get-buffer-create ,name)))
     (unwind-protect
         (with-current-buffer repl
           (setq-local agent-chat--session-id ,session)
           (setq-local claude-repl-agent-id ,agent)
           ,@body)
       (when (buffer-live-p repl) (kill-buffer repl)))))

(defun turn-stepper-invariants-test--frames ()
  "The recorded two-frame fixture as parsed frames (unfiltered: the second
frame's parse status is \"missing\", which `turn-stepper--ready-frames'
drops, and these tests need two frames to step back into)."
  (turn-stepper--parse-frames turn-stepper-test--fixture))

(defmacro turn-stepper-invariants-test--with-stepper (session source frames index &rest body)
  "Open a stepper on SESSION from SOURCE with FRAMES at INDEX, visible;
run BODY with the stepper buffer current."
  (declare (indent 4))
  `(let ((buf (turn-stepper--open ,session ,source 0 t)))
     (unwind-protect
         (with-current-buffer buf
           (setq turn-stepper--frames ,frames
                 turn-stepper--index ,index
                 turn-stepper--fetched-at (current-time))
           (switch-to-buffer buf)
           ,@body)
       (when (buffer-live-p buf) (kill-buffer buf)))))

;;; ------------------------------------------------------------ indexed names

(ert-deftest turn-stepper-indexed-two-repls-get-own-steppers ()
  "Two REPLs each get their own indexed stepper buffer."
  (turn-stepper-invariants-test--with-repl " *ts-inv-A*" "claude-13" "s-A"
    (let ((buf-a (turn-stepper--open "s-A" repl 0 t)))
      (unwind-protect
          (progn
            (should (equal "*象 stepper:claude-13*" (buffer-name buf-a)))
            (with-current-buffer repl
              (setq claude-repl-agent-id "claude-9"
                    agent-chat--session-id "s-B"))
            (let ((buf-b (turn-stepper--open "s-B" repl 0 t)))
              (unwind-protect
                  (progn
                    (should (equal "*象 stepper:claude-9*" (buffer-name buf-b)))
                    (should-not (eq buf-a buf-b))
                    (should (equal "s-A" (buffer-local-value
                                          'turn-stepper--session-id buf-a)))
                    (should (equal "s-B" (buffer-local-value
                                          'turn-stepper--session-id buf-b))))
                (when (buffer-live-p buf-b) (kill-buffer buf-b)))))
        (when (buffer-live-p buf-a) (kill-buffer buf-a))))))

(ert-deftest turn-stepper-indexed-legacy-buffer-renamed-on-open ()
  "A bare *象 stepper* attached to an identified REPL is renamed on its
next open; an unidentified source keeps the bare name."
  (turn-stepper-invariants-test--with-repl " *ts-inv-legacy*" nil "s-L"
    (let ((buf (turn-stepper--open "s-L" repl 0 t)))
      (unwind-protect
          (progn
            (should (equal turn-stepper-buffer-name (buffer-name buf)))
            (with-current-buffer repl
              (setq claude-repl-agent-id "claude-13"))
            (turn-stepper--open "s-L" repl 0 t)
            (should (equal "*象 stepper:claude-13*" (buffer-name buf))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

;;; ------------------------------------------------------------ the invariant

(ert-deftest turn-stepper-invariants-good-case-is-quiet ()
  "Two indexed steppers, current and attached: no violations."
  (turn-stepper-invariants-test--with-repl " *ts-inv-gA*" "claude-13" "s-GA"
    (turn-stepper-invariants-test--with-repl " *ts-inv-gB*" "claude-9" "s-GB"
      (let* ((frames (turn-stepper-invariants-test--frames))
             (last-i (1- (length frames)))
             (buf-a (turn-stepper--open "s-GA"
                                        (get-buffer " *ts-inv-gA*") 0 t))
             (buf-b nil))
        (unwind-protect
            (progn
              (with-current-buffer buf-a
                (setq turn-stepper--frames frames
                      turn-stepper--index last-i
                      turn-stepper--fetched-at (current-time)))
              (setq buf-b (turn-stepper--open "s-GB"
                                              (get-buffer " *ts-inv-gB*") 0 t))
              (with-current-buffer buf-b
                (setq turn-stepper--frames frames
                      turn-stepper--index last-i
                      turn-stepper--fetched-at (current-time)))
              (switch-to-buffer buf-b)
              (cl-letf (((symbol-function 'turn-stepper--store-changed-since)
                         (lambda (_) nil)))
                (should (null (turn-stepper-invariants-check)))))
          (when (buffer-live-p buf-a) (kill-buffer buf-a))
          (when (buffer-live-p buf-b) (kill-buffer buf-b)))))))

(ert-deftest turn-stepper-invariants-dead-source ()
  "A stepper whose source REPL was killed is :stepper-detached source-dead."
  (let ((repl (get-buffer-create " *ts-inv-dead*")))
    (with-current-buffer repl
      (setq-local agent-chat--session-id "s-D")
      (setq-local claude-repl-agent-id "claude-13"))
    (let ((buf (turn-stepper--open "s-D" repl 0 t)))
      (unwind-protect
          (progn
            (kill-buffer repl)
            (cl-letf (((symbol-function 'turn-stepper--store-changed-since)
                       (lambda (_) nil)))
              (let ((vs (turn-stepper-invariants-check)))
                (should (seq-find (lambda (v) (eq (car v) :stepper-detached)) vs))
                (should (cl-every (lambda (v)
                                    (not (eq (car v) :stepper-duplicate)))
                                  vs)))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest turn-stepper-invariants-session-mismatch ()
  "A stepper whose REPL moved to another session is :stepper-detached
session-mismatch."
  (turn-stepper-invariants-test--with-repl " *ts-inv-mm*" "claude-13" "s-M1"
    (let ((buf (turn-stepper--open "s-M1" repl 0 t)))
      (unwind-protect
          (progn
            (with-current-buffer repl
              (setq agent-chat--session-id "s-M2"))
            (cl-letf (((symbol-function 'turn-stepper--store-changed-since)
                       (lambda (_) nil)))
              (let ((vs (turn-stepper-invariants-check)))
                (should (seq-find
                         (lambda (v)
                           (and (eq (car v) :stepper-detached)
                                (string-match-p "session changed" (cadr v))))
                         vs)))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest turn-stepper-invariants-two-steppers-one-repl ()
  "Two stepper buffers attached to one REPL is :stepper-duplicate."
  (turn-stepper-invariants-test--with-repl " *ts-inv-dup*" "claude-13" "s-DUP"
    (let ((buf-a (turn-stepper--open "s-DUP" repl 0 t))
          (buf-b (get-buffer-create "*象 stepper:claude-13-extra*")))
      (unwind-protect
          (progn
            (with-current-buffer buf-b
              (turn-stepper-mode)
              (setq turn-stepper--session-id "s-DUP"
                    turn-stepper--source-buffer repl))
            (cl-letf (((symbol-function 'turn-stepper--store-changed-since)
                       (lambda (_) nil)))
              (let ((vs (turn-stepper-invariants-check)))
                (should (seq-find
                         (lambda (v) (eq (car v) :stepper-duplicate)) vs)))))
        (when (buffer-live-p buf-a) (kill-buffer buf-a))
        (when (buffer-live-p buf-b) (kill-buffer buf-b))))))

(ert-deftest turn-stepper-invariants-unnamed-stepper ()
  "A stepper on an identified REPL whose name lacks the agent id is
:stepper-unnamed."
  (turn-stepper-invariants-test--with-repl " *ts-inv-nn*" "claude-13" "s-NN"
    (let ((buf (get-buffer-create "*象 stepper*")))
      (unwind-protect
          (with-current-buffer buf
            (turn-stepper-mode)
            (setq turn-stepper--session-id "s-NN"
                  turn-stepper--source-buffer repl
                  turn-stepper--fetched-at (current-time))
            (cl-letf (((symbol-function 'turn-stepper--store-changed-since)
                       (lambda (_) nil)))
              (let ((vs (turn-stepper-invariants-check)))
                (should (seq-find
                         (lambda (v) (eq (car v) :stepper-unnamed)) vs)))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest turn-stepper-invariants-stale-visible-stepper ()
  "A visible stepper on its newest frame whose store changed since the last
fetch is :stepper-stale."
  (turn-stepper-invariants-test--with-repl " *ts-inv-st*" "claude-13" "s-ST"
    (let* ((frames (turn-stepper-invariants-test--frames))
           (buf (turn-stepper--open "s-ST" repl 0 t)))
      (unwind-protect
          (with-current-buffer buf
            (setq turn-stepper--frames frames
                  turn-stepper--index (1- (length frames))
                  turn-stepper--fetched-at (current-time))
            (switch-to-buffer buf)
            (cl-letf (((symbol-function 'turn-stepper--store-changed-since)
                       (lambda (_) t)))
              (let ((vs (turn-stepper-invariants-check)))
                (should (seq-find
                         (lambda (v) (eq (car v) :stepper-stale)) vs)))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest turn-stepper-invariants-deliberately-older-not-stale ()
  "A visible stepper deliberately on an older frame is not :stepper-stale,
even when the store changed."
  (turn-stepper-invariants-test--with-repl " *ts-inv-old*" "claude-13" "s-OLD"
    (let* ((frames (turn-stepper-invariants-test--frames))
           (buf (turn-stepper--open "s-OLD" repl 0 t)))
      (unwind-protect
          (with-current-buffer buf
            (setq turn-stepper--frames frames
                  turn-stepper--index 0 ; stepped back from the newest
                  turn-stepper--fetched-at (current-time))
            (switch-to-buffer buf)
            (cl-letf (((symbol-function 'turn-stepper--store-changed-since)
                       (lambda (_) t)))
              (let ((vs (turn-stepper-invariants-check)))
                (should-not (seq-find
                             (lambda (v) (eq (car v) :stepper-stale)) vs)))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(provide 'turn-stepper-invariants-test)
;;; turn-stepper-invariants-test.el ends here
