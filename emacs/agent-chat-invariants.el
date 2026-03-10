;;; agent-chat-invariants.el --- Timing and runtime invariants for agent chat -*- lexical-binding: t; -*-

;; Author: Claude + Joe
;; Description: Turn timing ("Cooked for 2m 58s"), buffer-state invariants,
;;   and optional Reazon-based relational checks for agent chat buffers.
;;   Loaded by claude-repl.el and codex-repl.el; requires agent-chat.el.
;;
;; Usage:
;;   (require 'agent-chat-invariants)
;;   ;; In your REPL --init:
;;   (agent-chat-invariants-setup)
;;
;; Modular control:
;;   (setq agent-chat-invariants-enabled-groups '(timing markers session))
;;   ;; Or toggle at runtime:
;;   M-x agent-chat-invariants-toggle-group RET markers RET

(require 'cl-lib)
(require 'agent-chat)

;;; Configuration

(defgroup agent-chat-invariants nil
  "Runtime invariants and timing for agent chat buffers."
  :group 'agent-chat)

(defcustom agent-chat-invariants-enabled-groups '(timing)
  "List of enabled invariant groups.
Available groups:
  timing   — show cook time after each turn
  markers  — check buffer markers are valid and ordered
  session  — check session state consistency
  reazon   — relational invariants via Reazon (requires reazon on load-path)

Add/remove groups to control what runs after each turn."
  :type '(repeat (choice (const timing)
                         (const markers)
                         (const session)
                         (const reazon)
                         symbol))
  :group 'agent-chat-invariants)

(defcustom agent-chat-invariants-failure-action 'message
  "How to report failed invariants.
`message' warns in echo area, `error' raises, nil silently ignores.
`insert' inserts a line into the chat buffer."
  :type '(choice (const :tag "Message" message)
                 (const :tag "Insert in buffer" insert)
                 (const :tag "Error" error)
                 (const :tag "Ignore" nil))
  :group 'agent-chat-invariants)

(defcustom agent-chat-invariants-reazon-timeout 0.1
  "Max time in seconds for Reazon invariant queries."
  :type 'number
  :group 'agent-chat-invariants)

(defface agent-chat-invariants-timing-face
  '((t :foreground "#6272a4" :slant italic))
  "Face for cook time display."
  :group 'agent-chat-invariants)

(defface agent-chat-invariants-violation-face
  '((t :foreground "#ff5555" :slant italic))
  "Face for invariant violation messages."
  :group 'agent-chat-invariants)

;;; Internal state

(defvar-local agent-chat-invariants--extra-checks nil
  "List of (GROUP . FN) pairs for REPL-specific invariant checks.
FN takes no arguments, returns a list of (TAG MSG) violation pairs or nil.")

(defvar-local agent-chat-invariants--turn-count 0
  "Number of completed turns in this buffer.")

;;; Group management

(defun agent-chat-invariants-group-enabled-p (group)
  "Return non-nil when GROUP is in `agent-chat-invariants-enabled-groups'."
  (memq group agent-chat-invariants-enabled-groups))

(defun agent-chat-invariants-enable-group (group)
  "Enable invariant GROUP."
  (cl-pushnew group agent-chat-invariants-enabled-groups)
  (message "agent-chat-invariants: %s enabled" group))

(defun agent-chat-invariants-disable-group (group)
  "Disable invariant GROUP."
  (setq agent-chat-invariants-enabled-groups
        (remq group agent-chat-invariants-enabled-groups))
  (message "agent-chat-invariants: %s disabled" group))

(defun agent-chat-invariants-toggle-group (group)
  "Toggle invariant GROUP on or off."
  (interactive
   (list (intern (completing-read
                  "Toggle invariant group: "
                  '("timing" "markers" "session" "reazon")
                  nil t))))
  (if (agent-chat-invariants-group-enabled-p group)
      (agent-chat-invariants-disable-group group)
    (agent-chat-invariants-enable-group group)))

;;; Timing

(defun agent-chat-invariants--format-elapsed (seconds)
  "Format SECONDS as human-readable duration."
  (let ((s (truncate seconds)))
    (cond
     ((>= s 3600)
      (format "%dh %dm %ds" (/ s 3600) (/ (mod s 3600) 60) (mod s 60)))
     ((>= s 60)
      (format "%dm %ds" (/ s 60) (mod s 60)))
     (t
      (format "%ds" s)))))

(defun agent-chat-invariants--show-cook-time (elapsed)
  "Insert cook time line above the prompt."
  (when (and (agent-chat-invariants-group-enabled-p 'timing)
             (> elapsed 0))
    (let ((inhibit-read-only t)
          (text (format "Cooked for %s"
                        (agent-chat-invariants--format-elapsed elapsed))))
      (save-excursion
        (goto-char (marker-position agent-chat--prompt-marker))
        (insert (propertize (concat text "\n")
                            'face 'agent-chat-invariants-timing-face
                            'agent-chat-invariant t))))))

;;; Buffer state invariants — markers group

(defun agent-chat-invariants--check-markers ()
  "Check that buffer markers are valid and ordered.
Returns list of (TAG MSG) violations."
  (let ((violations nil))
    (unless (and (markerp agent-chat--prompt-marker)
                 (marker-position agent-chat--prompt-marker))
      (push (list :markers "prompt-marker is dead or unset") violations))
    (unless (and (markerp agent-chat--input-start)
                 (marker-position agent-chat--input-start))
      (push (list :markers "input-start is dead or unset") violations))
    (when (and (markerp agent-chat--prompt-marker)
               (marker-position agent-chat--prompt-marker)
               (markerp agent-chat--input-start)
               (marker-position agent-chat--input-start)
               (> (marker-position agent-chat--prompt-marker)
                  (marker-position agent-chat--input-start)))
      (push (list :markers
                  (format "prompt-marker (%d) > input-start (%d)"
                          (marker-position agent-chat--prompt-marker)
                          (marker-position agent-chat--input-start)))
            violations))
    (nreverse violations)))

;;; Buffer state invariants — session group

(defun agent-chat-invariants--check-session ()
  "Check session state consistency.
Returns list of (TAG MSG) violations."
  (let ((violations nil))
    ;; Orphan process: pending-process set but dead
    (when (and agent-chat--pending-process
               (not (process-live-p agent-chat--pending-process)))
      (push (list :session "orphan pending-process (dead but not cleared)")
            violations))
    (nreverse violations)))

;;; Reazon relational invariants (soft dependency)

(defvar agent-chat-invariants--reazon-available nil
  "Non-nil when Reazon has been loaded successfully.")
(defvar agent-chat-invariants--reazon-tried nil
  "Non-nil after first attempt to load Reazon.")

(defun agent-chat-invariants--ensure-reazon ()
  "Load Reazon if available. Return non-nil on success."
  (or agent-chat-invariants--reazon-available
      (unless agent-chat-invariants--reazon-tried
        (setq agent-chat-invariants--reazon-tried t)
        ;; Try futon4 vendor path
        (let ((vendor (expand-file-name
                       "dev/vendor/reazon"
                       (or (getenv "FUTON4_ROOT")
                           "/home/joe/code/futon4"))))
          (when (file-directory-p vendor)
            (add-to-list 'load-path vendor)))
        (setq agent-chat-invariants--reazon-available
              (require 'reazon nil t)))))

(defvar agent-chat-invariants--reazon-checks nil
  "List of reazon check functions registered at load time.
Each is a 0-arg function that returns a list of (TAG MSG) violations.
Modules that depend on reazon should `eval-after-load' to register
their checks so that reazon macros expand correctly.")

(defun agent-chat-invariants-register-reazon-check (check-fn)
  "Register CHECK-FN as a reazon-based invariant.
CHECK-FN should use reazon macros directly (compiled with reazon loaded).
It takes no args and returns a list of (TAG MSG) violations."
  (cl-pushnew check-fn agent-chat-invariants--reazon-checks))

(defvar reazon-timeout) ;; forward-declare dynamic var used by reazon-run

(defun agent-chat-invariants--check-reazon ()
  "Run Reazon-based relational invariants.
Returns list of (TAG MSG) violations."
  (when (and (agent-chat-invariants--ensure-reazon)
             agent-chat-invariants--reazon-checks)
    (let ((violations nil)
          (reazon-timeout agent-chat-invariants-reazon-timeout))
      (dolist (check-fn agent-chat-invariants--reazon-checks)
        (condition-case err
            (setq violations (append violations (funcall check-fn)))
          (error
           (push (list :reazon
                       (format "reazon check error: %s"
                               (error-message-string err)))
                 violations))))
      (nreverse violations))))

;;; Reporting

(defun agent-chat-invariants--report (tag msg)
  "Report invariant violation TAG: MSG per `agent-chat-invariants-failure-action'."
  (let ((formatted (format "invariant [%s]: %s" tag msg)))
    (pcase agent-chat-invariants-failure-action
      ('message (message "agent-chat %s" formatted))
      ('insert
       (when (and (markerp agent-chat--prompt-marker)
                  (marker-position agent-chat--prompt-marker))
         (let ((inhibit-read-only t))
           (save-excursion
             (goto-char (marker-position agent-chat--prompt-marker))
             (insert (propertize (concat formatted "\n")
                                 'face 'agent-chat-invariants-violation-face
                                 'agent-chat-invariant t))))))
      ('error (error "agent-chat %s" formatted))
      (_ nil))))

;;; Core check runner

(defun agent-chat-invariants--run-checks ()
  "Run all enabled invariant groups. Report violations."
  (let ((violations nil))
    (when (agent-chat-invariants-group-enabled-p 'markers)
      (setq violations (append violations
                               (agent-chat-invariants--check-markers))))
    (when (agent-chat-invariants-group-enabled-p 'session)
      (setq violations (append violations
                               (agent-chat-invariants--check-session))))
    (when (agent-chat-invariants-group-enabled-p 'reazon)
      (setq violations (append violations
                               (agent-chat-invariants--check-reazon))))
    ;; REPL-specific checks
    (dolist (entry agent-chat-invariants--extra-checks)
      (let ((group (car entry))
            (fn (cdr entry)))
        (when (agent-chat-invariants-group-enabled-p group)
          (condition-case err
              (setq violations (append violations (funcall fn)))
            (error
             (push (list group
                         (format "check error: %s" (error-message-string err)))
                   violations))))))
    (dolist (v violations)
      (agent-chat-invariants--report (car v) (cadr v)))
    violations))

;;; Turn lifecycle hook

(defun agent-chat-invariants--on-turn-end (elapsed)
  "Called after each agent turn. ELAPSED is seconds as a float."
  (setq agent-chat-invariants--turn-count
        (1+ agent-chat-invariants--turn-count))
  (agent-chat-invariants--show-cook-time elapsed)
  (agent-chat-invariants--run-checks))

;;; Registration API for REPL-specific checks

(defun agent-chat-invariants-register-check (group check-fn)
  "Register CHECK-FN under GROUP for this buffer.
CHECK-FN takes no args, returns list of (TAG MSG) violations or nil.
GROUP must be in `agent-chat-invariants-enabled-groups' to run."
  (cl-pushnew (cons group check-fn) agent-chat-invariants--extra-checks
              :test #'equal))

;;; Setup

(defun agent-chat-invariants-setup ()
  "Enable invariants and timing for the current chat buffer.
Call from REPL init functions after `agent-chat-init-buffer'."
  (setq-local agent-chat-invariants--turn-count 0)
  (setq-local agent-chat-invariants--extra-checks nil)
  (setq-local agent-chat--on-turn-end
              'agent-chat-invariants--on-turn-end))

(defun agent-chat-invariants-turn-ended ()
  "Notify invariants that a turn completed.
Call from streaming sentinels that bypass the `agent-chat-send-input'
callback (i.e. when the REPL handled response display directly).
Computes elapsed from `agent-chat--turn-start-time' automatically."
  (let ((elapsed (if agent-chat--turn-start-time
                     (- (float-time) agent-chat--turn-start-time)
                   0)))
    (setq agent-chat--turn-start-time nil)
    (when (functionp agent-chat--on-turn-end)
      (condition-case err
          (funcall agent-chat--on-turn-end elapsed)
        (error
         (message "agent-chat turn-end hook error: %s"
                  (error-message-string err)))))))

;;; Interactive diagnostics

(defun agent-chat-invariants-check-now ()
  "Run all enabled invariant checks immediately and report results."
  (interactive)
  (let ((violations (agent-chat-invariants--run-checks)))
    (if violations
        (message "agent-chat-invariants: %d violation(s) found" (length violations))
      (message "agent-chat-invariants: all checks passed (groups: %s)"
               (mapconcat #'symbol-name
                          agent-chat-invariants-enabled-groups ", ")))))

(defun agent-chat-invariants-status ()
  "Display current invariant configuration."
  (interactive)
  (message "groups: [%s] | turns: %d | failure: %s | reazon: %s"
           (mapconcat #'symbol-name
                      agent-chat-invariants-enabled-groups " ")
           agent-chat-invariants--turn-count
           agent-chat-invariants-failure-action
           (cond
            (agent-chat-invariants--reazon-available "loaded")
            (agent-chat-invariants--reazon-tried "unavailable")
            (t "not tried"))))

(provide 'agent-chat-invariants)
;;; agent-chat-invariants.el ends here
