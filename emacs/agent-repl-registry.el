;;; agent-repl-registry.el --- Common capability store for agent REPL frontends -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; A frontend-agnostic place for agent REPL buffers to publish the abilities
;; that shared machinery needs to drive them.  Written 2026-08-20 (claude-10)
;; after `claude-repl-park.el' was found to be unable to resume a park in a
;; zai buffer: the parked-on RESUME path had claude's sender and evidence
;; hooks hard-coded, so non-claude buffers could never wake.
;;
;; The three frontends already agree on shape.  Each one sends through
;;
;;     (agent-chat-send-input SENDER AGENT-NAME (list :before-send ... :on-response ...))
;;
;; with SENDER of signature (TEXT CALLBACK).  That triple IS the capability;
;; this file just gives it a name and a lookup so shared code can ask for it
;; rather than assume claude's.
;;
;; Identity is deliberately NOT stored here: `agent-chat--agent-id' and
;; `agent-chat--session-id' are already generic buffer-locals populated by
;; every frontend via `agent-chat' setup.  Use those.
;;
;; Registration is additive and idempotent.  A frontend may register itself at
;; load time; until it does, the built-in defaults below cover the three known
;; modes, and every default is guarded by `fboundp' so a sender is never
;; published unless its function actually exists in this Emacs.

;;; Code:

(require 'seq)

(defvar agent-repl-registry nil
  "Alist of (MAJOR-MODE . PLIST) describing agent REPL frontends.

PLIST keys:
  :agent-name  - display name passed to `agent-chat-send-input' (\"claude\")
  :sender      - function (TEXT CALLBACK) that performs the turn
  :hooks       - plist (:before-send FN :on-response FN), may be nil
  :api-url-var - symbol of a buffer-local/defcustom holding this frontend's
                 API base URL, or nil to fall back to the agency base URL")

(defun agent-repl-register! (mode &rest plist)
  "Register MODE's capabilities from PLIST, replacing any previous entry.
Returns the stored plist."
  (setq agent-repl-registry
        (cons (cons mode plist)
              (assq-delete-all mode agent-repl-registry)))
  plist)

(defun agent-repl-capabilities (&optional mode)
  "Return the capability plist for MODE, defaulting to `major-mode'."
  (alist-get (or mode major-mode) agent-repl-registry))

(defun agent-repl-capability (key &optional mode)
  "Return capability KEY for MODE, defaulting to `major-mode'."
  (plist-get (agent-repl-capabilities mode) key))

(defun agent-repl-registered-modes ()
  "Return the list of major-modes that have published capabilities."
  (mapcar #'car agent-repl-registry))

(defun agent-repl-drivable-p (&optional mode)
  "Non-nil when MODE has published a usable :sender.

Shared machinery that CONSUMES a one-shot resource (the parked-on ready-inbox
is poll-and-consume) must gate on this.  Polling a buffer we cannot then drive
destroys the item: worse than never polling at all."
  (let ((sender (agent-repl-capability :sender mode)))
    (and sender (functionp sender))))

(defun agent-repl-api-base-url (&optional mode)
  "Best API base URL for MODE: its own if published, else the agency default."
  (let* ((var (agent-repl-capability :api-url-var mode))
         (own (and var (boundp var) (symbol-value var))))
    (or (and (stringp own) (not (string-empty-p own)) own)
        (and (boundp 'agent-chat-agency-base-url) agent-chat-agency-base-url))))

;;;; Built-in defaults ---------------------------------------------------------
;;
;; Guarded by `fboundp' so nothing is published for a frontend that is not
;; loaded, and so a rename upstream degrades to "not drivable" rather than to a
;; void-function at resume time.  A frontend that would rather own its entry can
;; simply call `agent-repl-register!' itself; the last registration wins.

(defun agent-repl-registry-install-defaults! ()
  "Register the three known frontends, skipping any whose sender is absent."
  (when (fboundp 'claude-repl--call-claude-streaming)
    (agent-repl-register!
     'claude-repl-mode
     :agent-name "claude"
     :sender #'claude-repl--call-claude-streaming
     :api-url-var 'claude-repl-api-url
     :hooks (list :before-send
                  (lambda (text)
                    (when (fboundp 'claude-repl--emit-user-turn-evidence!)
                      (claude-repl--emit-user-turn-evidence! text))
                    (when (fboundp 'claude-repl--store-upsert-session)
                      (claude-repl--store-upsert-session))
                    (when (fboundp 'claude-repl--open-frame)
                      (claude-repl--open-frame text)))
                  :on-response
                  (lambda (text)
                    (when (fboundp 'claude-repl--emit-assistant-turn-evidence!)
                      (claude-repl--emit-assistant-turn-evidence! text))
                    (when (fboundp 'claude-repl--emit-turn-commits-evidence!)
                      (claude-repl--emit-turn-commits-evidence!))
                    (when (fboundp 'claude-repl--close-frame)
                      (claude-repl--close-frame "done"))))))
  (when (fboundp 'zai-repl--call)
    (agent-repl-register!
     'zai-repl-mode
     :agent-name "zai"
     :sender #'zai-repl--call
     :api-url-var nil
     :hooks nil))
  (when (fboundp 'codex-repl--call-codex-async)
    (agent-repl-register!
     'codex-repl-mode
     :agent-name "codex"
     :sender #'codex-repl--call-codex-async
     :api-url-var nil
     :hooks (list :before-send
                  (lambda (text)
                    (when (fboundp 'codex-repl--emit-user-turn-evidence!)
                      (codex-repl--emit-user-turn-evidence! text)))
                  :on-response
                  (lambda (text)
                    (when (fboundp 'codex-repl--emit-assistant-turn-evidence!)
                      (codex-repl--emit-assistant-turn-evidence! text))
                    (when (fboundp 'codex-repl--emit-turn-commits-evidence!)
                      (codex-repl--emit-turn-commits-evidence!))))))
  agent-repl-registry)

;; Install now for whatever is already loaded, and again after any frontend
;; loads later, so ordering does not matter.
(agent-repl-registry-install-defaults!)
(with-eval-after-load 'claude-repl (agent-repl-registry-install-defaults!))
(with-eval-after-load 'zai-repl (agent-repl-registry-install-defaults!))
(with-eval-after-load 'codex-repl (agent-repl-registry-install-defaults!))

(provide 'agent-repl-registry)
;;; agent-repl-registry.el ends here
