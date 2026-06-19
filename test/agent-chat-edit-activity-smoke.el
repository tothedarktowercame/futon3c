;;; agent-chat-edit-activity-smoke.el --- batch smoke checks -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar agent-chat-edit-activity-smoke-failures nil)

(defun agent-chat-edit-activity-smoke--fail (label expected actual)
  (push (format "%s expected %S got %S" label expected actual)
        agent-chat-edit-activity-smoke-failures))

(defun agent-chat-edit-activity-smoke--assert (label value)
  (unless value
    (agent-chat-edit-activity-smoke--fail label t value)))

(defun agent-chat-edit-activity-smoke--assert-equal (label expected actual)
  (unless (equal expected actual)
    (agent-chat-edit-activity-smoke--fail label expected actual)))

(defun agent-chat-edit-activity-smoke--reset ()
  (setq agent-chat--edit-activity-save-events (make-hash-table :test 'equal)))

(defun agent-chat-edit-activity-smoke--with-chat-buffer (fn)
  (with-temp-buffer
    (setq agent-chat--agent-name "codex")
    (setq agent-chat--campaign-id nil)
    (setq agent-chat--mission-id nil)
    (setq agent-chat--excursion-id nil)
    (setq agent-chat--last-auto-clock-witness nil)
    (setq agent-chat--edit-activity-last-reclock-target nil)
    (setq agent-chat--prompt-marker (point-marker))
    (setq agent-chat--input-start (point-marker))
    (funcall fn (current-buffer))))

(defun agent-chat-edit-activity-smoke--file (id)
  (format "/tmp/futon-smoke/holes/missions/%s.md" id))

(defun agent-chat-edit-activity-smoke--save (id time buffer)
  (agent-chat--record-edit-activity-save
   (agent-chat-edit-activity-smoke--file id)
   time
   (list buffer)))

(defun agent-chat-edit-activity-smoke--run ()
  (let ((agent-chat-edit-activity-clock-enabled t)
        (agent-chat-edit-activity-clock-threshold 3)
        (agent-chat-edit-activity-clock-window-seconds 600))
    (cl-letf (((symbol-function 'agent-chat--clock-target-candidates)
               (lambda (kind)
                 (pcase kind
                   ('campaign '("C-camp"))
                   ('mission '("M-foo" "M-bar"))
                   ('excursion '("E-hop")))))
              ((symbol-function 'agent-chat-insert-message)
               (lambda (&rest _args) nil))
              ((symbol-function 'agent-chat--update-session-header-line)
               (lambda () nil)))
      (agent-chat-edit-activity-smoke--reset)
      (agent-chat-edit-activity-smoke--with-chat-buffer
       (lambda (buf)
         (agent-chat-edit-activity-smoke--save "M-foo" 1000 buf)
         (agent-chat-edit-activity-smoke--save "M-foo" 1001 buf)
         (agent-chat-edit-activity-smoke--save "M-foo" 1002 buf)
         (agent-chat-edit-activity-smoke--assert-equal
          "3 saves switch to M-foo" "M-foo" agent-chat--mission-id)
         (agent-chat-edit-activity-smoke--assert-equal
          "3 saves witness rule"
          "edit-activity"
          (cdr (assoc 'rule agent-chat--last-auto-clock-witness)))
         (agent-chat-edit-activity-smoke--assert-equal
          "3 saves witness count"
          3
          (cdr (assoc 'edit-count agent-chat--last-auto-clock-witness)))))

      (agent-chat-edit-activity-smoke--reset)
      (agent-chat-edit-activity-smoke--with-chat-buffer
       (lambda (buf)
         (agent-chat-edit-activity-smoke--save "M-foo" 2000 buf)
         (agent-chat-edit-activity-smoke--assert-equal
          "1 save no switch" nil agent-chat--mission-id)))

      (agent-chat-edit-activity-smoke--reset)
      (agent-chat-edit-activity-smoke--with-chat-buffer
       (lambda (buf)
         (agent-chat-edit-activity-smoke--save "M-foo" 3000 buf)
         (agent-chat-edit-activity-smoke--save "M-bar" 3001 buf)
         (agent-chat-edit-activity-smoke--save "M-foo" 3002 buf)
         (agent-chat-edit-activity-smoke--save "M-bar" 3003 buf)
         (agent-chat-edit-activity-smoke--save "M-foo" 3004 buf)
         (agent-chat-edit-activity-smoke--save "M-bar" 3005 buf)
         (agent-chat-edit-activity-smoke--assert-equal
          "alternating saves no thrash" nil agent-chat--mission-id)))

      (agent-chat-edit-activity-smoke--reset)
      (agent-chat-edit-activity-smoke--with-chat-buffer
       (lambda (buf)
         (setq agent-chat--mission-id "M-bar")
         (agent-chat-edit-activity-smoke--save "M-foo" 4000 buf)
         (agent-chat-edit-activity-smoke--save "M-foo" 4001 buf)
         (agent-chat-edit-activity-smoke--save "M-foo" 4002 buf)
         (agent-chat-edit-activity-smoke--assert-equal
          "active M-bar switches to M-foo" "M-foo" agent-chat--mission-id)))

      (agent-chat-edit-activity-smoke--reset)
      (agent-chat-edit-activity-smoke--with-chat-buffer
       (lambda (buf)
         (agent-chat-edit-activity-smoke--save "C-camp" 5000 buf)
         (agent-chat-edit-activity-smoke--save "C-camp" 5001 buf)
         (agent-chat-edit-activity-smoke--save "C-camp" 5002 buf)
         (agent-chat-edit-activity-smoke--assert-equal
          "single-active campaign set" "C-camp" agent-chat--campaign-id)
         (agent-chat-edit-activity-smoke--assert-equal
          "single-active mission nil" nil agent-chat--mission-id)
         (agent-chat-edit-activity-smoke--assert-equal
          "single-active excursion nil" nil agent-chat--excursion-id))))))

(agent-chat-edit-activity-smoke--run)

(if agent-chat-edit-activity-smoke-failures
    (progn
      (dolist (failure (nreverse agent-chat-edit-activity-smoke-failures))
        (princ (format "FAIL %s\n" failure)))
      (kill-emacs 1))
  (princ "PASS agent-chat edit-activity smoke checks: 5 scenarios\n"))

;;; agent-chat-edit-activity-smoke.el ends here
