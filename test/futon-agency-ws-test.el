;;; futon-agency-ws-test.el --- Tests for Agency WS transition bubbles -*- lexical-binding: t; -*-

(require 'ert)
(load-file (expand-file-name "../emacs/futon-agency-ws.el" (file-name-directory load-file-name)))

(defvar futon-agency-completion--previous-status)
(defvar futon-agency-completion-watched-agents)
(declare-function futon-agency-completion--done-agents "futon-agency-ws" (previous current &optional watched))
(declare-function futon-agency-completion--handle-agents-status "futon-agency-ws" (frame))
(declare-function futon-agency-completion--show "futon-agency-ws" (agent))
(declare-function futon-agency-completion--status-map "futon-agency-ws" (frame))

(defun futon-agency-ws-test--frame (&rest pairs)
  `((type . "agents_status")
    (agents . ,(mapcar (lambda (pair)
                         (cons (intern (car pair))
                               `((status . ,(cdr pair)))))
                       pairs))))

(ert-deftest futon-agency-completion-detects-watched-invoking-to-idle ()
  (let* ((prev '(("fable-2" . "invoking")
                 ("claude-3" . "busy")
                 ("codex-1" . "invoking")))
         (cur '(("fable-2" . "idle")
                ("claude-3" . "idle")
                ("codex-1" . "idle"))))
    (should (equal '("fable-2")
                   (futon-agency-completion--done-agents
                    prev cur '("fable-2" "claude-3"))))))

(ert-deftest futon-agency-completion-status-map-normalizes-agent-ids ()
  (should (equal '(("fable-2" . "invoking")
                   ("claude-3" . "idle"))
                 (futon-agency-completion--status-map
                  (futon-agency-ws-test--frame '("fable-2" . "invoking")
                                               '("claude-3" . "idle"))))))

(ert-deftest futon-agency-completion-handler-fires-once-per-transition ()
  (let ((futon-agency-completion--previous-status nil)
        (futon-agency-completion-watched-agents '("fable-2"))
        (shown nil))
    (cl-letf (((symbol-function 'futon-agency-completion--show)
               (lambda (agent) (push agent shown))))
      (futon-agency-completion--handle-agents-status
       (futon-agency-ws-test--frame '("fable-2" . "invoking")))
      (should (equal nil shown))
      (futon-agency-completion--handle-agents-status
       (futon-agency-ws-test--frame '("fable-2" . "idle")))
      (should (equal '("fable-2") shown))
      (futon-agency-completion--handle-agents-status
       (futon-agency-ws-test--frame '("fable-2" . "idle")))
      (should (equal '("fable-2") shown)))))

(ert-deftest futon-agency-completion-fallback-message-has-required-text ()
  (let (messages)
    (cl-letf (((symbol-function 'futon-agency-completion--cursor-marker)
               (lambda () nil))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (futon-agency-completion--show "fable-2")
      (should (equal '("fable-2 done — what next?") messages)))))

(ert-deftest futon-agency-hud-renders-invoke-preview-for-busy-agents ()
  (let ((futon-agency-hud-buffer-name " *futon-agency-hud-test* ")
        (futon-agency-hud--render-count 0))
    (unwind-protect
        (progn
          (futon-agency-hud--render
           '((type . "agents_status")
             (count . 1)
             (agents . ((codex-4 . ((status . "invoking")
                                    (type . "codex")
                                    (invoke-activity . "using bash")
                                    (invoke-prompt-preview . "--- CURRENT TURN ---\nSurface: emacs-repl\nCaller: joe")))))))
          (with-current-buffer futon-agency-hud-buffer-name
            (let ((text (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-match-p "codex-4" text))
              (should (string-match-p "using bash" text))
              (should (string-match-p "--- CURRENT TURN ---" text)))))
      (when (get-buffer futon-agency-hud-buffer-name)
        (kill-buffer futon-agency-hud-buffer-name)))))

(provide 'futon-agency-ws-test)
;;; futon-agency-ws-test.el ends here
