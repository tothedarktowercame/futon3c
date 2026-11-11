;;; turn-stepper-invariants.el --- Reazon attachment invariant for 象 steppers -*- lexical-binding: t; -*-

;; Author: kimi-2 (for claude-13)
;; Description: M-repl-circus invariant 1 (Joe, 2026-11-11): "the *象 stepper*
;;   buffer should be attached exactly to one REPL at a time ... if we're
;;   modelling the steps for two or more buffers they should be
;;   *象 stepper:claude-13* or some such indexed referent.  That's a layout
;;   and attachment invariant (which could be enforced with reazon)."
;;
;; Facts are gathered from the live buffers (stepper buffers and their agent
;; chat REPL sources); the relations are stated in Reazon; violations are
;; (TAG MSG) pairs returned by a 0-arg check registered with
;; `agent-chat-invariants-register-reazon-check', so the check runs inside
;; the invariant cycle under `agent-chat-invariants-reazon-timeout' (0.1 s).
;;
;; The currency clause is mtime-only, like `turn-stepper--reconcile': a
;; visible stepper that is not deliberately on an older frame is stale when
;; the record store changed since its last successful fetch.  It never runs
;; the python reader per check.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'turn-stepper)
(require 'agent-chat-invariants)

(agent-chat-invariants--ensure-reazon)

;;; ------------------------------------------------------------ fact gather

;; One fact per stepper buffer, a plain list:
;;   (name session source-name source-live source-session agent visible
;;         stepped-back stale)

(defun turn-stepper-invariants--gather ()
  "Snapshot the live stepper facts.
Cheap: buffer-locals and one directory stat per stepper (the record store's
mtime, as `turn-stepper--reconcile' reads it)."
  (mapcar
   (lambda (b)
     (let* ((name (buffer-name b))
            (session (buffer-local-value 'turn-stepper--session-id b))
            (source (buffer-local-value 'turn-stepper--source-buffer b))
            (source-live (and (buffer-live-p source) t))
            (source-name (and source-live (buffer-name source)))
            (source-session (and source-live
                                 (turn-stepper--session-id-in-buffer source)))
            (agent (and source-live
                        (turn-stepper--agent-id-in-buffer source)))
            (visible (and (get-buffer-window b t) t))
            (stepped-back (with-current-buffer b
                            (and turn-stepper--frames
                                 (< turn-stepper--index
                                    (1- (length turn-stepper--frames))))))
            (stale (and visible session (not stepped-back)
                        (with-current-buffer b
                          (turn-stepper--store-changed-since
                           turn-stepper--fetched-at))
                        t)))
       (list name session source-name source-live source-session
             agent visible stepped-back stale)))
   (turn-stepper--buffers)))

;;; ------------------------------------------------------------ relations

(reazon-defrel turn-stepper-invariants--membero (x xs)
  "X is an element of XS."
  (reazon-fresh (hd tl)
    (reazon-conde
     ((reazon-conso hd tl xs)
      (reazon-== hd x))
     ((reazon-conso hd tl xs)
      (turn-stepper-invariants--membero x tl)))))

(defmacro turn-stepper-invariants--facto (name session sname slive ssession
                                               agent vis sb stale facts)
  "Fact NAME ... STALE is a gathered stepper fact of FACTS."
  `(turn-stepper-invariants--membero
    (list ,name ,session ,sname ,slive ,ssession ,agent ,vis ,sb ,stale)
    ,facts))

(reazon-defrel turn-stepper-invariants--detachedo (name why facts)
  "Stepper NAME's source REPL is dead, or its live session id is not the
stepper's session id."
  (reazon-fresh (session sname slive ssession agent vis sb stale)
    (turn-stepper-invariants--facto
     name session sname slive ssession agent vis sb stale facts)
    (reazon-conde
     ((reazon-project (slive)
        (if slive #'reazon-!U #'reazon-!S))
      (reazon-== why 'source-dead))
     ((reazon-project (slive ssession session)
        (if (and slive session ssession (not (equal ssession session)))
            #'reazon-!S #'reazon-!U))
      (reazon-== why 'session-mismatch)))))

(reazon-defrel turn-stepper-invariants--attachedo (name sname facts)
  "Stepper NAME is attached to the live REPL named SNAME."
  (reazon-fresh (session slive ssession agent vis sb stale)
    (turn-stepper-invariants--facto
     name session sname slive ssession agent vis sb stale facts)
    (reazon-project (slive sname)
      (if (and slive sname) #'reazon-!S #'reazon-!U))))

(reazon-defrel turn-stepper-invariants--duplicateo (n1 n2 sname facts)
  "Steppers N1 and N2 (distinct, ordered) are attached to the same REPL."
  (turn-stepper-invariants--attachedo n1 sname facts)
  (turn-stepper-invariants--attachedo n2 sname facts)
  (reazon-project (n1 n2)
    (if (string< n1 n2) #'reazon-!S #'reazon-!U)))

(reazon-defrel turn-stepper-invariants--unnamedo (name agent facts)
  "Stepper NAME's REPL has agent id AGENT but NAME is not the indexed
`*象 stepper:<agent>*' name."
  (reazon-fresh (session sname slive ssession vis sb stale)
    (turn-stepper-invariants--facto
     name session sname slive ssession agent vis sb stale facts)
    (reazon-project (name agent slive)
      (if (and slive agent
               (not (equal name
                           (format "%s:%s*"
                                   (string-remove-suffix
                                    "*" turn-stepper-buffer-name)
                                   agent))))
          #'reazon-!S #'reazon-!U))))

(reazon-defrel turn-stepper-invariants--staleo (name facts)
  "Stepper NAME is visible, not deliberately on an older frame, and the
record store changed since its last successful fetch (mtime-only, as
`turn-stepper--reconcile')."
  (reazon-fresh (session sname slive ssession agent vis sb stale)
    (turn-stepper-invariants--facto
     name session sname slive ssession agent vis sb stale facts)
    (reazon-project (vis sb stale)
      (if (and vis (not sb) stale) #'reazon-!S #'reazon-!U))))

;;; ------------------------------------------------------------ the check

(defun turn-stepper-invariants-check ()
  "Reazon attachment invariant for 象 steppers: a list of (TAG MSG).
TAG is one of :stepper-detached :stepper-duplicate :stepper-unnamed
:stepper-stale."
  (let ((facts (turn-stepper-invariants--gather)))
    (when facts
      (append
       (mapcar
        (lambda (r)
          (list :stepper-detached
                (pcase-exhaustive (cadr r)
                  ('source-dead
                   (format "象 stepper %s: source REPL is dead" (car r)))
                  ('session-mismatch
                   (format "象 stepper %s: source REPL session changed (stepper reads %s)"
                           (car r)
                           (cadr (cl-find (car r) facts
                                          :key #'car :test #'equal)))))))
        (reazon-run* q
          (reazon-fresh (n why)
            (turn-stepper-invariants--detachedo n why facts)
            (reazon-== q (list n why)))))
       (mapcar
        (lambda (r)
          (list :stepper-duplicate
                (format "象 steppers %s and %s both attach to %s"
                        (car r) (cadr r) (caddr r))))
        (reazon-run* q
          (reazon-fresh (n1 n2 sname)
            (turn-stepper-invariants--duplicateo n1 n2 sname facts)
            (reazon-== q (list n1 n2 sname)))))
       (mapcar
        (lambda (r)
          (list :stepper-unnamed
                (format "象 stepper %s: should be named *象 stepper:%s*"
                        (car r) (cadr r))))
        (reazon-run* q
          (reazon-fresh (n agent)
            (turn-stepper-invariants--unnamedo n agent facts)
            (reazon-== q (list n agent)))))
       (mapcar
        (lambda (n)
          (list :stepper-stale
                (format "象 stepper %s: record store changed since its last fetch and it is not on an older frame"
                        n)))
        (reazon-run* n
          (turn-stepper-invariants--staleo n facts)))))))

(agent-chat-invariants-register-reazon-check #'turn-stepper-invariants-check)

(provide 'turn-stepper-invariants)
;;; turn-stepper-invariants.el ends here
