;;; M-smart-emacs-cursor.verify.el --- scene invariants as Reazon relations -*- lexical-binding: t; -*-

;; VERIFY gate for M-smart-emacs-cursor: logic-model-before-code, in the
;; substrate of the build itself — Emacs verifies Emacs (Joe, spoken,
;; 2026-06-11; first misheard as plain reason, struck and corrected).
;; A scene-trace is a list of ground events over a closed alphabet:
;;   (voice routed) (voice at-focus) (agent place) (agent edit)
;;   (operator request) (agent move-operator) (hud ws-render) (hud push-render)
;; Each invariant is a VIOLATION pattern: an empty answer set on the
;; conforming trace is the proof; a non-empty answer set on its
;; adversarial trace is the catch. Typed into existence by the agent
;; cursor, which hopped here from the mission buffer to do it.

(require 'reazon)
(require 'cl-lib)

;; I1 non-impersonation: an edit before any placed body is a violation.
(reazon-defrel msec-edit-unplaced-o (tr)
  (reazon-fresh (h rest)
    (reazon-conso h rest tr)
    (reazon-conde
     ((reazon-== h (list 'agent 'edit)))
     ((reazon-conde
       ((reazon-== h (list 'voice 'routed)))
       ((reazon-== h (list 'voice 'at-focus)))
       ((reazon-== h (list 'operator 'request)))
       ((reazon-== h (list 'agent 'move-operator)))
       ((reazon-== h (list 'hud 'ws-render)))
       ((reazon-== h (list 'hud 'push-render))))
      (msec-edit-unplaced-o rest)))))

;; I2 consent: moving the operator before any request is a violation.
(reazon-defrel msec-move-unrequested-o (tr)
  (reazon-fresh (h rest)
    (reazon-conso h rest tr)
    (reazon-conde
     ((reazon-== h (list 'agent 'move-operator)))
     ((reazon-conde
       ((reazon-== h (list 'voice 'routed)))
       ((reazon-== h (list 'voice 'at-focus)))
       ((reazon-== h (list 'agent 'place)))
       ((reazon-== h (list 'agent 'edit)))
       ((reazon-== h (list 'hud 'ws-render)))
       ((reazon-== h (list 'hud 'push-render))))
      (msec-move-unrequested-o rest)))))

(defun msec--query (goal-fn trace)
  "Run GOAL-FN as a violation query over ground TRACE."
  (reazon-run* (q)
    (funcall goal-fn trace)
    (reazon-== q 'violation)))

(defun msec--member-query (event trace)
  "Violation query: EVENT occurs anywhere in TRACE."
  (reazon-run* (q)
    (reazon-membero q trace)
    (reazon-== q event)))

(defconst msec-conforming-trace
  (list (list 'voice 'routed) (list 'agent 'place) (list 'agent 'edit)
        (list 'operator 'request) (list 'agent 'move-operator)
        (list 'hud 'ws-render))
  "The six-beat scene, lawful.")

(defconst msec-adversarial-traces
  (list (cons "I1 edit-without-body"
              (list (list 'voice 'routed) (list 'agent 'edit) (list 'agent 'place)))
        (cons "I2 move-without-request"
              (list (list 'agent 'place) (list 'agent 'move-operator)))
        (cons "I3 voice-at-focus"
              (list (list 'voice 'at-focus)))
        (cons "I4 racing-push-render"
              (list (list 'hud 'push-render))))
  "One trace per invariant, each built to be caught.")

(defun msec-invariant-queries (trace)
  "All four violation queries over TRACE, as (NAME . ANSWERS)."
  (list (cons "I1" (msec--query #'msec-edit-unplaced-o trace))
        (cons "I2" (msec--query #'msec-move-unrequested-o trace))
        (cons "I3" (msec--member-query (list 'voice 'at-focus) trace))
        (cons "I4" (msec--member-query (list 'hud 'push-render) trace))))

(defun msec-verify-certificate ()
  "Run the gate: conforming trace clean AND every adversary caught.
Returns the certificate as a string."
  (let* ((conforming (msec-invariant-queries msec-conforming-trace))
         (clean (cl-every (lambda (cell) (null (cdr cell))) conforming))
         (catches
          (mapcar (lambda (adv)
                    (let* ((name (car adv))
                           (trace (cdr adv))
                           (key (substring name 0 2))
                           (answers (cdr (assoc key (msec-invariant-queries trace)))))
                      (cons name (and answers t))))
                  msec-adversarial-traces))
         (all-caught (cl-every #'cdr catches)))
    (concat
     (format "CERTIFICATE — M-smart-emacs-cursor VERIFY (Reazon)\n")
     (format "conforming trace: %s\n" (if clean "CLEAN (0 violations)" "VIOLATIONS FOUND"))
     (mapconcat (lambda (c) (format "adversary %s: %s" (car c)
                                    (if (cdr c) "CAUGHT" "MISSED")))
                catches "\n")
     (format "\nverdict: %s\n" (if (and clean all-caught) "GATE OPEN" "GATE SHUT")))))

(provide 'M-smart-emacs-cursor-verify)
;;; M-smart-emacs-cursor.verify.el ends here
