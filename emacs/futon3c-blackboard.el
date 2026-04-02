;;; futon3c-blackboard.el --- Generic Emacs controls for futon3c blackboards -*- lexical-binding: t; -*-

;; Author: Codex + Joe
;; Description: Wrapper commands for server-side blackboard behavior that is
;; not tied to any specific REPL buffer.

(require 'cl-lib)
(require 'json)
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

(defconst futon3c-blackboard--source-file
  (or load-file-name (buffer-file-name))
  "Path used to recover the workspace root for blackboard helpers.")

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

(defun futon3c-blackboard-show-agents-hud ()
  "Show `*agents*' as a compact local HUD without changing server popup policy."
  (interactive)
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
      (message "*agents* HUD shown"))))

(defun futon3c-blackboard-toggle-agents-hud ()
  "Toggle the local `*agents*' HUD window."
  (interactive)
  (if (futon3c-blackboard--agents-window-visible-p)
      (progn
        (futon3c-blackboard-close-agents-windows)
        (message "*agents* HUD hidden"))
    (futon3c-blackboard-show-agents-hud)))

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

(provide 'futon3c-blackboard)

;;; futon3c-blackboard.el ends here
