# Design: agent-chat-invariants.el

## Goals

1. Turn timing display — "Cooked for 2m 58s" after each agent response
2. Runtime invariant checking on buffer state
3. Optional reazon-based relational invariants (soft dependency)
4. Works for both claude-repl and codex-repl via agent-chat hooks

## Changes needed in agent-chat.el (minimal)

Two new hooks, ~10 lines:

```elisp
(defvar-local agent-chat--turn-start-time nil
  "Epoch time when the current turn started (set by send-input).")

(defvar-local agent-chat--turn-hooks nil
  "Plist of lifecycle hooks. Keys:
  :on-turn-start  (fn text)    — called when user sends input
  :on-turn-end    (fn elapsed) — called when agent response arrives
  :on-invariant-violation (fn tag msg) — called when check fails")
```

Wire into `agent-chat-send-input`:
- Set `agent-chat--turn-start-time` to `(float-time)` before calling async fn
- In the response callback, compute elapsed and call `:on-turn-end`
- Pass `:on-turn-start` through to the `:before-send` timing

That's it. The invariants module hooks in via these.

## agent-chat-invariants.el — structure (~180 lines)

### Configuration

```elisp
(defgroup agent-chat-invariants nil
  "Runtime invariants and timing for agent chat buffers."
  :group 'agent-chat)

(defcustom agent-chat-invariants-enable t
  "When non-nil, run invariant checks after each turn."
  :type 'boolean)

(defcustom agent-chat-invariants-show-timing t
  "When non-nil, display cook time after each agent response."
  :type 'boolean)

(defcustom agent-chat-invariants-timing-face
  '(:foreground "#6272a4" :slant italic)
  "Face for cook time display."
  :type 'sexp)

(defcustom agent-chat-invariants-failure-action 'message
  "How to report failed invariants: message, error, or nil."
  :type '(choice (const message) (const error) (const nil)))

(defcustom agent-chat-invariants-reazon-timeout 0.1
  "Max time in seconds for reazon queries."
  :type 'number)
```

### Timing display (~30 lines)

```elisp
(defun agent-chat-invariants--format-elapsed (seconds)
  "Format SECONDS as Xm Ys or Xs."
  (if (>= seconds 60)
      (format "%dm %ds" (/ seconds 60) (mod seconds 60))
    (format "%ds" seconds)))

(defun agent-chat-invariants--show-cook-time (elapsed)
  "Insert cook time line above the prompt after agent response."
  (when (and agent-chat-invariants-show-timing (> elapsed 0))
    (let ((inhibit-read-only t)
          (text (format "Cooked for %s"
                        (agent-chat-invariants--format-elapsed elapsed))))
      (save-excursion
        (goto-char (marker-position agent-chat--prompt-marker))
        (insert (propertize (concat text "\n")
                            'face agent-chat-invariants-timing-face))))))
```

This replaces codex-repl's 23-line thinking heartbeat timer for the
final display. The timer during thinking remains in agent-chat's
progress line; this just adds the final summary.

### Buffer state invariants (~50 lines)

These are cheap checks that catch real bugs:

```elisp
(defun agent-chat-invariants--check-markers ()
  "Check that buffer markers are valid and ordered."
  (let ((violations nil))
    (unless (and (markerp agent-chat--prompt-marker)
                 (marker-position agent-chat--prompt-marker))
      (push '(:marker "prompt-marker is dead") violations))
    (unless (and (markerp agent-chat--input-start)
                 (marker-position agent-chat--input-start))
      (push '(:marker "input-start is dead") violations))
    (when (and (marker-position agent-chat--prompt-marker)
               (marker-position agent-chat--input-start)
               (> (marker-position agent-chat--prompt-marker)
                  (marker-position agent-chat--input-start)))
      (push '(:marker "prompt-marker > input-start") violations))
    violations))

(defun agent-chat-invariants--check-session ()
  "Check session state consistency."
  (let ((violations nil))
    (when (and (process-live-p agent-chat--pending-process)
               (null agent-chat--session-id))
      ;; Live process with no session — might be fine on first turn
      nil)
    ;; Orphan process: pending-process is dead but not nil
    (when (and agent-chat--pending-process
               (not (process-live-p agent-chat--pending-process)))
      (push '(:session "orphan pending-process (dead but not cleared)")
            violations))
    violations))

(defun agent-chat-invariants--check-all ()
  "Run all invariant checks. Return list of violations."
  (append (agent-chat-invariants--check-markers)
          (agent-chat-invariants--check-session)))
```

### Reazon relational invariants (~40 lines, soft dep)

Following the futon4 pattern:

```elisp
(defvar agent-chat-invariants--reazon-available nil)
(defvar agent-chat-invariants--reazon-tried nil)

(defun agent-chat-invariants--ensure-reazon ()
  "Load reazon if available. Return non-nil on success."
  (or agent-chat-invariants--reazon-available
      (unless agent-chat-invariants--reazon-tried
        (setq agent-chat-invariants--reazon-tried t)
        ;; Try futon4 vendor path first
        (let ((vendor (expand-file-name
                       "dev/vendor/reazon"
                       "/home/joe/code/futon4")))
          (when (file-directory-p vendor)
            (add-to-list 'load-path vendor)))
        (setq agent-chat-invariants--reazon-available
              (require 'reazon nil t)))))

;; Example relational invariant: marker ordering
;; This is intentionally simple — the value of reazon here is
;; as an extension point for REPL-specific relational checks.
(defun agent-chat-invariants--define-relations ()
  "Define reazon relations for chat buffer invariants."
  (when (agent-chat-invariants--ensure-reazon)
    (reazon-defrel agent-chat-invariants--markers-consistento
        (prompt input ok)
      (reazon-project (prompt input)
        (reazon-== ok (and (integerp prompt)
                           (integerp input)
                           (<= prompt input)))))))
```

The real payoff of reazon comes when REPL-specific modules register
additional relations — e.g., "if agent-id is claude-N then session-file
must be /tmp/futon-session-id-claude-N", or "if transport is IRC then
surface-contract must include channel name."

### Hook wiring (~30 lines)

```elisp
(defun agent-chat-invariants--on-turn-end (elapsed)
  "Called after each agent turn completes."
  ;; Show cook time
  (agent-chat-invariants--show-cook-time elapsed)
  ;; Run invariants
  (when agent-chat-invariants-enable
    (let ((violations (agent-chat-invariants--check-all)))
      (dolist (v violations)
        (agent-chat-invariants--report (car v) (cadr v))))))

(defun agent-chat-invariants--report (tag msg)
  "Report invariant violation TAG: MSG."
  (pcase agent-chat-invariants-failure-action
    ('message (message "agent-chat invariant [%s]: %s" tag msg))
    ('error (error "agent-chat invariant [%s]: %s" tag msg))
    (_ nil)))

(defun agent-chat-invariants-setup ()
  "Enable invariants and timing for the current chat buffer.
Called from REPL init functions."
  (setq-local agent-chat--turn-hooks
              (plist-put agent-chat--turn-hooks
                         :on-turn-end
                         #'agent-chat-invariants--on-turn-end)))
```

### REPL integration (1 line each)

In `claude-repl--init` and `codex-repl--init`, after `agent-chat-init-buffer`:

```elisp
(agent-chat-invariants-setup)
```

### Extension point for REPL-specific invariants

```elisp
(defvar-local agent-chat-invariants--extra-checks nil
  "List of (fn) to call during check-all. Each returns violations list.")

(defun agent-chat-invariants-register-check (check-fn)
  "Register CHECK-FN as an additional invariant check."
  (cl-pushnew check-fn agent-chat-invariants--extra-checks))
```

codex-repl could register:
```elisp
(agent-chat-invariants-register-check
 (lambda ()
   (when (and codex-repl-session-id
              (not (file-exists-p codex-repl-session-file)))
     '((:codex-session "session-id set but file missing")))))
```

claude-repl could register:
```elisp
(agent-chat-invariants-register-check
 (lambda ()
   (when (and (claude-repl--workspace)
              (string= claude-repl-agent-id "claude-1")
              (not (string= claude-repl-session-file
                             "/tmp/futon-session-id-claude-1")))
     '((:claude-workspace "workspace agent-id/session-file mismatch")))))
```

## What this replaces

| Current code | Replaced by |
|---|---|
| codex-repl thinking timer (23 lines) | shared timing in agent-chat |
| codex-repl elapsed tracking (15 lines) | `agent-chat--turn-start-time` |
| Ad-hoc `codex-repl--ui-state-valid-p` (6 lines) | `--check-markers` |
| No timing in claude-repl | Free — just works |

## File budget

| Section | Lines |
|---|---|
| Config | 25 |
| Timing display | 25 |
| Buffer invariants | 50 |
| Reazon integration | 40 |
| Hook wiring + setup | 30 |
| Extension point | 15 |
| **Total** | **~185** |
