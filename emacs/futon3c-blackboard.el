;;; futon3c-blackboard.el --- Generic Emacs controls for futon3c blackboards -*- lexical-binding: t; -*-

;; Author: Codex + Joe
;; Description: Wrapper commands for server-side blackboard behavior that is
;; not tied to any specific REPL buffer.

(require 'cl-lib)
(require 'json)
(require 'server)
(require 'subr-x)
(require 'url)

(defgroup futon3c-blackboard nil
  "Generic controls for futon3c blackboard buffers."
  :group 'applications)

(defcustom futon3c-blackboard-drawbridge-url "http://localhost:6768"
  "Drawbridge REPL URL for blackboard control commands."
  :type 'string
  :group 'futon3c-blackboard)

(defcustom futon3c-blackboard-drawbridge-token nil
  "Admin token for Drawbridge REPL.
If nil, reads from `.admintoken' in a nearby project root at first use."
  :type '(choice (const nil) string)
  :group 'futon3c-blackboard)

(defcustom futon3c-blackboard-agents-hud-side 'right
  "Side used when displaying the local `*agents*' HUD."
  :type '(choice (const left) (const right) (const bottom) (const top))
  :group 'futon3c-blackboard)

(defcustom futon3c-blackboard-agents-hud-width 0.22
  "Width used for the local `*agents*' HUD on left/right sides."
  :type 'number
  :group 'futon3c-blackboard)

(defcustom futon3c-blackboard-agents-hud-height 0.18
  "Height used for the local `*agents*' HUD on top/bottom sides."
  :type 'number
  :group 'futon3c-blackboard)

(defcustom futon3c-blackboard-use-external-hud
  (let ((raw (getenv "FUTON3C_EXTERNAL_HUD")))
    (and raw
         (not (member (downcase (string-trim raw))
                      '("0" "false" "no" "off")))))
  "When non-nil, route `*agents*' and `*invoke:*' interaction through a
dedicated terminal HUD instead of local side windows."
  :type 'boolean
  :group 'futon3c-blackboard)

(defcustom futon3c-blackboard-hud-socket-name
  (or (getenv "FUTON3C_HUD_SOCKET")
      (getenv "FUTON3C_EMACS_SOCKET")
      (getenv "EMACS_SOCKET_NAME"))
  "Emacs server socket name used by the terminal HUD launcher."
  :type '(choice (const nil) string)
  :group 'futon3c-blackboard)

(defcustom futon3c-blackboard-hud-frame-name "Agency HUD"
  "Frame name used for the dedicated terminal HUD."
  :type 'string
  :group 'futon3c-blackboard)

(defcustom futon3c-blackboard-hud-launch-script nil
  "Optional path to the terminal HUD launcher script.
When nil, uses `scripts/agency-hud-terminal' from the futon3c project root."
  :type '(choice (const nil) file)
  :group 'futon3c-blackboard)

(defconst futon3c-blackboard--hud-frame-parameter 'futon3c-hud
  "Frame parameter marking the dedicated Agency HUD frame.")

(defconst futon3c-blackboard--source-file
  (or load-file-name (buffer-file-name))
  "Path used to recover the workspace root for blackboard helpers.")

(defvar futon3c-blackboard--hud-focus-buffer "*agents*"
  "Buffer name that newly created HUD frames should show prominently.")

(defvar futon3c-blackboard-hud-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'futon3c-blackboard-quit-hud)
    map)
  "Keymap active in Agency HUD buffers.")

(define-minor-mode futon3c-blackboard-hud-mode
  "Minor mode for transient Agency HUD buffers."
  :init-value nil
  :lighter " HUD"
  :keymap futon3c-blackboard-hud-mode-map
  (setq-local cursor-type nil))

(defun futon3c-blackboard--project-root ()
  "Return the futon3c project root."
  (or (and futon3c-blackboard--source-file
           (locate-dominating-file futon3c-blackboard--source-file ".git"))
      "/home/joe/code/futon3c/"))

(defun futon3c-blackboard--hud-launch-script ()
  "Return the launcher script used for the terminal HUD."
  (or futon3c-blackboard-hud-launch-script
      (expand-file-name "scripts/agency-hud-terminal"
                        (futon3c-blackboard--project-root))))

(defun futon3c-blackboard--drawbridge-token ()
  "Return the Drawbridge admin token, reading `.admintoken' if needed."
  (or futon3c-blackboard-drawbridge-token
      (let* ((roots (delete-dups
                     (delq nil
                           (list (locate-dominating-file default-directory ".admintoken")
                                 (and futon3c-blackboard--source-file
                                      (locate-dominating-file futon3c-blackboard--source-file
                                                              ".admintoken"))
                                 (locate-dominating-file
                                  "/home/joe/code/futon3c/emacs/futon3c-blackboard.el"
                                  ".admintoken")))))
             (token-file (cl-find-if
                          #'file-exists-p
                          (mapcar (lambda (root)
                                    (expand-file-name ".admintoken" root))
                                  roots))))
        (when token-file
          (setq futon3c-blackboard-drawbridge-token
                (string-trim (with-temp-buffer
                               (insert-file-contents token-file)
                               (buffer-string))))))))

(defun futon3c-blackboard-eval (clj-code &optional timeout-seconds)
  "Evaluate CLJ-CODE via Drawbridge and return parsed JSON response, or nil."
  (let* ((url (format "%s/eval" futon3c-blackboard-drawbridge-url))
         (token (futon3c-blackboard--drawbridge-token))
         (payload (string-as-unibyte (encode-coding-string clj-code 'utf-8)))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("x-admin-token" . ,token)
            ("Content-Type" . "text/plain; charset=utf-8")))
         (url-request-data payload)
         (buffer (condition-case nil
                     (url-retrieve-synchronously url t t (or timeout-seconds 2))
                   (error nil))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char (point-min))
        (re-search-forward "\n\n" nil 'move)
        (let* ((body (buffer-substring-no-properties (point) (point-max)))
               (parsed
                (let* ((ok (and (string-match-p ":ok[[:space:]]+true" body) t))
                       (value
                        (cond
                         ((string-match ":value[[:space:]]+\"\\([^\"]*\\)\"" body)
                          (match-string 1 body))
                         ((string-match ":value[[:space:]]+\\([^,}\n]+\\)" body)
                          (let ((raw (string-trim (match-string 1 body))))
                            (cond
                             ((string= raw "nil") nil)
                             ((string= raw "true") t)
                             ((string= raw "false") nil)
                             ((string-match-p "\\`[0-9]+\\'" raw) (string-to-number raw))
                             (t raw))))
                         (t nil))))
                  (list :ok ok :value value))))
          (kill-buffer buffer)
          parsed)))))

(defun futon3c-blackboard-close-agents-windows ()
  "Dismiss all visible windows showing `*agents*' or `*invoke:*' buffers."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((name (buffer-name buf)))
      (when (or (string= name "*agents*")
                (string-prefix-p "*invoke:" name))
        (dolist (win (get-buffer-window-list buf t t))
          (when (window-live-p win)
            (condition-case nil
                (quit-window nil win)
              (error nil))))))))

(defun futon3c-blackboard--agents-window-visible-p ()
  "Return non-nil when `*agents*' or any `*invoke:*' buffer is visible."
  (cl-some (lambda (buf)
             (let ((name (buffer-name buf)))
               (when (or (string= name "*agents*")
                         (string-prefix-p "*invoke:" name))
                 (cl-some #'window-live-p
                          (get-buffer-window-list buf t t)))))
           (buffer-list)))

(defun futon3c-blackboard--hud-frame-p (&optional frame)
  "Return non-nil when FRAME is the dedicated HUD frame."
  (let ((frame (or frame (selected-frame))))
    (or (frame-parameter frame futon3c-blackboard--hud-frame-parameter)
        (string= (frame-parameter frame 'name)
                 futon3c-blackboard-hud-frame-name))))

(defun futon3c-blackboard--hud-frame ()
  "Return the live HUD frame, if any."
  (cl-find-if #'futon3c-blackboard--hud-frame-p (frame-list)))

(defun futon3c-blackboard--latest-invoke-buffer ()
  "Return the most-recently-used `*invoke:*' buffer, if any."
  (cl-find-if (lambda (buf)
                (string-prefix-p "*invoke:" (buffer-name buf)))
              (buffer-list)))

(defun futon3c-blackboard-quit-hud ()
  "Dismiss the transient Agency HUD.
When invoked inside the HUD frame, close that frame; otherwise fall back to
`quit-window'."
  (interactive)
  (if (futon3c-blackboard--hud-frame-p)
      (delete-frame)
    (quit-window nil)))

(defun futon3c-blackboard--prepare-hud-buffer (buffer)
  "Enable HUD-local behavior for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (unless futon3c-blackboard-hud-mode
        (futon3c-blackboard-hud-mode 1)))))

(defun futon3c-blackboard--collect-hud-buffers (focus-buffer-name)
  "Collect available HUD buffers in display order.
Returns a list of live buffers: *agents*, *processes*, *context*, then focus."
  (let ((bufs nil))
    ;; Focus buffer (invoke or explicit) — goes last (bottom)
    (let ((focus (or (and focus-buffer-name (get-buffer focus-buffer-name))
                     (futon3c-blackboard--latest-invoke-buffer))))
      (when focus (push focus bufs)))
    ;; *context* — above focus
    (let ((ctx (get-buffer "*context*")))
      (when (and ctx (> (buffer-size ctx) 0))
        (push ctx bufs)))
    ;; *processes* — above context
    (let ((proc (get-buffer "*processes*")))
      (when proc (push proc bufs)))
    ;; *agents* — always at top
    (push (get-buffer-create "*agents*") bufs)
    bufs))

(defun futon3c-blackboard-arrange-hud-frame (&optional frame focus-buffer-name)
  "Arrange FRAME as the Agency HUD with all available HUD buffers.
Layout stacks vertically: *agents* | *processes* | *context* | focus."
  (interactive)
  (let* ((frame (or frame (selected-frame)))
         (bufs (futon3c-blackboard--collect-hud-buffers focus-buffer-name)))
    (dolist (b bufs)
      (futon3c-blackboard--prepare-hud-buffer b))
    (with-selected-frame frame
      (set-frame-parameter frame futon3c-blackboard--hud-frame-parameter t)
      (set-frame-name futon3c-blackboard-hud-frame-name)
      (delete-other-windows)
      (let* ((n (length bufs))
             (window (selected-window)))
        (if (= n 1)
            (set-window-buffer window (car bufs))
          ;; Split into N panes: first buffer gets top portion, rest split evenly
          (let* ((total-height (max 12 (window-total-height window)))
                 (top-height (max 5 (min (- total-height (* 4 (1- n)))
                                         (floor (* total-height (/ 1.0 n))))))
                 (rest-window (split-window window top-height 'below)))
            (set-window-buffer window (car bufs))
            ;; Split remaining panes
            (let ((remaining (cdr bufs))
                  (current-window rest-window))
              (while (cdr remaining)
                (let* ((rest-h (window-total-height current-window))
                       (pane-h (max 4 (floor (/ rest-h (length remaining)))))
                       (next-window (split-window current-window pane-h 'below)))
                  (set-window-buffer current-window (car remaining))
                  (setq remaining (cdr remaining))
                  (setq current-window next-window)))
              ;; Last buffer gets the remaining space
              (set-window-buffer current-window (car remaining))
              (select-window current-window)))))
      (redraw-frame frame))))

(defun futon3c-blackboard--maybe-configure-hud-frame (&optional frame)
  "Initialize FRAME when it was created for the Agency HUD.
When FRAME is nil, use the currently selected frame."
  (let ((frame (or frame (selected-frame))))
    (when (frame-live-p frame)
      (with-selected-frame frame
        (when (futon3c-blackboard--hud-frame-p frame)
          (futon3c-blackboard-arrange-hud-frame
           frame
           futon3c-blackboard--hud-focus-buffer))))))

(unless (memq #'futon3c-blackboard--maybe-configure-hud-frame
              after-make-frame-functions)
  (add-hook 'after-make-frame-functions
            #'futon3c-blackboard--maybe-configure-hud-frame))

(unless (memq #'futon3c-blackboard--maybe-configure-hud-frame
              server-after-make-frame-hook)
  (add-hook 'server-after-make-frame-hook
            #'futon3c-blackboard--maybe-configure-hud-frame))

(defun futon3c-blackboard--launch-external-hud ()
  "Raise or spawn the terminal HUD via the configured launcher script."
  (let ((script (futon3c-blackboard--hud-launch-script)))
    (unless (file-executable-p script)
      (user-error "HUD launcher is not executable: %s" script))
    (let ((process-environment (copy-sequence process-environment)))
      (when futon3c-blackboard-hud-socket-name
        (setenv "FUTON3C_HUD_SOCKET" futon3c-blackboard-hud-socket-name))
      (start-process "futon3c-agency-hud" nil script))))

(defun futon3c-blackboard-display-buffer-in-hud (buffer-or-name &optional raise)
  "Show BUFFER-OR-NAME in the terminal HUD. When RAISE is non-nil, raise or
spawn the HUD terminal window as well."
  (interactive
   (list (or (buffer-name (current-buffer)) "*agents*") t))
  (let* ((buffer (get-buffer-create buffer-or-name))
         (buffer-name (buffer-name buffer))
         (frame (futon3c-blackboard--hud-frame)))
    (setq futon3c-blackboard--hud-focus-buffer buffer-name)
    (when (frame-live-p frame)
      (futon3c-blackboard-arrange-hud-frame frame buffer-name))
    (when raise
      (futon3c-blackboard--launch-external-hud))
    buffer))

(defun futon3c-blackboard-show-invoke-hud (&optional raise)
  "Show the latest `*invoke:*' buffer in the terminal HUD.
When RAISE is non-nil, raise or spawn the HUD terminal too."
  (interactive "p")
  (let ((buf (futon3c-blackboard--latest-invoke-buffer)))
    (unless buf
      (user-error "No `*invoke:*' buffer is available yet"))
    (futon3c-blackboard-display-buffer-in-hud buf raise)))

(defun futon3c-blackboard-toggle-external-hud-mode ()
  "Toggle external HUD mode for blackboard and REPL side buffers."
  (interactive)
  (let* ((enabled? (not futon3c-blackboard-use-external-hud))
         (result (futon3c-blackboard-eval
                  (concat
                   "(do "
                   "(require 'futon3c.blackboard) "
                   "(futon3c.blackboard/set-external-hud-enabled! "
                   (if enabled? "true" "false")
                   ") "
                   (when enabled?
                     "(futon3c.blackboard/set-agents-window-display! false) ")
                   "true)")
                  5)))
    (if (plist-get result :ok)
        (progn
          (setq futon3c-blackboard-use-external-hud enabled?)
          (when enabled?
            (futon3c-blackboard-close-agents-windows))
          (message "External HUD mode %s"
                   (if enabled? "enabled" "disabled")))
      (message "Failed to toggle external HUD mode via Drawbridge"))))

(defun futon3c-blackboard-show-agents-hud ()
  "Show `*agents*' as a compact local HUD without changing server popup policy."
  (interactive)
  (if futon3c-blackboard-use-external-hud
      (progn
        (futon3c-blackboard-display-buffer-in-hud "*agents*" t)
        (message "*agents* HUD raised"))
    (let ((buf (get-buffer "*agents*")))
      (unless buf
        (user-error "`*agents*' is not available yet"))
      (let* ((side futon3c-blackboard-agents-hud-side)
             (size-pair (if (memq side '(left right))
                            (cons 'window-width futon3c-blackboard-agents-hud-width)
                          (cons 'window-height futon3c-blackboard-agents-hud-height)))
             (params `((side . ,side)
                       ,size-pair
                       (slot . 0)
                       (window-parameters . ((no-other-window . t)
                                             (no-delete-other-windows . t))))))
        (display-buffer-in-side-window buf params)
        (message "*agents* HUD shown")))))

(defun futon3c-blackboard-toggle-agents-hud ()
  "Toggle the local `*agents*' HUD window."
  (interactive)
  (if futon3c-blackboard-use-external-hud
      (futon3c-blackboard-show-agents-hud)
    (if (futon3c-blackboard--agents-window-visible-p)
        (progn
          (futon3c-blackboard-close-agents-windows)
          (message "*agents* HUD hidden"))
      (futon3c-blackboard-show-agents-hud))))

(defun futon3c-blackboard-toggle-agents-window-display ()
  "Toggle whether `*agents*' force-displays as a side window.
Flips the JVM-side blackboard setting via Drawbridge and immediately
re-projects the current registry so the change is visible at once.
When disabling, also dismiss any existing local `*agents*' windows."
  (interactive)
  (let* ((result (futon3c-blackboard-eval
                  (concat
                   "(do "
                   "(require 'futon3c.blackboard) "
                   "(require 'futon3c.agency.registry) "
                   "(let [enabled? "
                   "(futon3c.blackboard/set-agents-window-display! "
                   "(not (futon3c.blackboard/agents-window-display-enabled?)))] "
                   "(futon3c.blackboard/project-agents! "
                   "(futon3c.agency.registry/registry-status)) "
                   "enabled?))")
                  5))
         (ok (plist-get result :ok))
         (enabled? (plist-get result :value)))
    (if ok
        (progn
          (when (not enabled?)
            (futon3c-blackboard-close-agents-windows))
          (message "*agents* persistent popup %s"
                   (if enabled? "enabled" "disabled")))
      (message "Failed to toggle *agents* popup via Drawbridge"))))

;; =============================================================================
;; Side-window suppression for external HUD mode
;; =============================================================================
;; When external HUD mode is on, prevent *agents*, *invoke:*, and *processes*
;; from popping up as side windows in the main frame.  The server's blackboard!
;; calls `display-buffer-in-side-window' directly (bypassing display-buffer-alist),
;; so we advise that function to intercept HUD buffers.

(defun futon3c-blackboard--hud-buffer-p (buffer)
  "Return non-nil if BUFFER should be routed to the HUD frame."
  (let ((name (if (bufferp buffer) (buffer-name buffer) buffer)))
    (or (string= name "*agents*")
        (string= name "*processes*")
        (string= name "*context*")
        (string-prefix-p "*invoke:" name))))

(defun futon3c-blackboard--suppress-hud-side-window (orig-fn buffer alist &rest args)
  "Around advice for `display-buffer-in-side-window'.
When external HUD mode is on and BUFFER is a HUD buffer, suppress
the side window in the main frame."
  (if (and futon3c-blackboard-use-external-hud
           (futon3c-blackboard--hud-buffer-p buffer)
           (not (futon3c-blackboard--hud-frame-p)))
      ;; Suppress: return nil (no window created) or existing window
      (get-buffer-window buffer t)
    (apply orig-fn buffer alist args)))

(advice-add 'display-buffer-in-side-window
            :around #'futon3c-blackboard--suppress-hud-side-window)

(provide 'futon3c-blackboard)

;;; futon3c-blackboard.el ends here
