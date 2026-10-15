;;; xiang-scope-overlay.el --- Scope edges for the current REPL turn -*- lexical-binding: t; -*-

;;; Commentary:
;; Draws the bounded (current turn plus previous reply) scope graph returned by
;; /api/alpha/xiang/scopes.  The fetch is deliberately injectable for ERT.

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'subr-x)
(require 'url)
(require 'url-http)

(defgroup xiang-scope-overlay nil "Scope overlays for 象 readings." :group 'session-mode)
(defface xiang-scope-operator-face '((t :underline t)) "Operator acts.")
(defface xiang-scope-agent-face '((t :underline (:style wave :color "#8ec07c"))) "Agent acts.")
(defface xiang-scope-open-face '((t :box (:line-width 1 :color "#d79921"))) "OPEN parent acts.")

(defcustom xiang-scope-overlay-base-url "http://127.0.0.1:7070"
  "Base URL for the scope route." :type 'string)
(defvar xiang-scope-overlay-fetch-function #'url-retrieve)
(defvar xiang-scope-overlay-retry-seconds 4
  "After a 404 on the current turn, refresh once more this many seconds later.
nil disables the retry (the retry itself runs with it nil, so it is one-shot).")
(defvar agent-chat--session-id)
(defvar agent-chat--current-turn-id)
(defvar-local session-mode--last-operator-region nil)
(defvar-local xiang-scope-overlay--overlays nil)
(defvar-local xiang-scope-overlay--parents nil)
(defvar-local xiang-scope-overlay--missing 0)

(defun xiang-scope-overlay--delete ()
  (mapc #'delete-overlay xiang-scope-overlay--overlays)
  (setq xiang-scope-overlay--overlays nil
        xiang-scope-overlay--parents nil))

(defun xiang-scope-overlay--regions ()
  (or (and (boundp 'session-mode--turn-text-regions)
           session-mode--turn-text-regions)
      (and (fboundp 'session-mode--scope-text-regions)
           (session-mode--scope-text-regions))
      (delq nil (list (and session-mode--last-operator-region
                           (cons "operator" session-mode--last-operator-region))
                      (and (boundp 'session-mode--last-agent-region)
                           session-mode--last-agent-region
                           (cons "agent" session-mode--last-agent-region))))))

(defun xiang-scope-overlay--position (region offset)
  (when (and region (integerp offset))
    (let ((beg (car region)) (end (cdr region)))
      (when (and (markerp beg) (markerp end)
                 (<= (+ (marker-position beg) offset) (marker-position end)))
        (+ (marker-position beg) offset)))))

(defun xiang-scope-overlay--act-turn (id)
  (when (string-match "\\`\\(.*\\)-[fr]-" id)
    (match-string 1 id)))

(defun xiang-scope-overlay--agent-position (offset entries)
  (catch 'found
    (let ((remaining offset))
      (dolist (entry (sort (copy-sequence entries)
                           (lambda (a b) (< (marker-position (car (cddr a)))
                                             (marker-position (car (cddr b)))))))
      (let* ((region (cddr entry))
             (length (- (marker-position (cdr region))
                        (marker-position (car region)))))
        (if (< remaining length)
            (throw 'found (+ (marker-position (car region)) remaining))
          (setq remaining (- remaining length 2)))))
      nil)))

(defun xiang-scope-overlay--search-key (text)
  "The first line of TEXT, up to 40 characters, as a literal search key."
  (let ((line (car (split-string (or text "") "\n" t))))
    (when line
      (let ((key (string-trim line)))
        (when (>= (length key) 8)
          (substring key 0 (min 40 (length key))))))))

(defun xiang-scope-overlay--find-text (text entries)
  "Position of TEXT's first words inside the buffer spans of ENTRIES, or nil.
The record's reply_text is the assistant segments joined by blank lines;
the buffer shows the same segments with tool-call blocks and a speaker
prefix between and before them (turn 151: 2,047 chars recorded, 5,762 in
the buffer), so offsets cannot be trusted across them and the paragraph is
found by its own words instead."
  (when-let ((key (xiang-scope-overlay--search-key text)))
    (catch 'found
      (dolist (entry entries)
        (let ((beg (marker-position (car (cddr entry))))
              (end (marker-position (cdr (cddr entry)))))
          (when (and beg end (< beg end))
            (save-excursion
              (goto-char beg)
              (when (search-forward key end t)
                (throw 'found (match-beginning 0)))))))
      nil)))

(defun xiang-scope-overlay--act-position (act regions)
  (let* ((side (alist-get 'side act nil nil #'equal))
         (turn (xiang-scope-overlay--act-turn (alist-get 'id act)))
         (entries (cl-remove-if-not (lambda (r)
                                      (and (equal turn (car r))
                                           (equal side (cadr r))))
                                    regions))
         (start (alist-get 'start act)))
    (if (equal side "agent")
        (and entries
             (or (xiang-scope-overlay--find-text (alist-get 'text act) entries)
                 (xiang-scope-overlay--agent-position start entries)))
      (xiang-scope-overlay--position (and (car entries) (cddr (car entries))) start))))

(defun xiang-scope-overlay--parent-text (id acts)
  "The governing act's first words, or its id when it lies outside the acts
the route returned (a reply paragraph's parent is often an operator
fragment of the previous turn, which is outside the this-turn-plus-previous-
reply bound)."
  (let ((parent (cl-find id acts :key (lambda (a) (alist-get 'id a)) :test #'equal)))
    (if (and parent (alist-get 'text parent))
        (truncate-string-to-width (alist-get 'text parent) 60 nil nil t)
      (or id "?"))))

(defun xiang-scope-overlay--apply (payload)
  (let* ((acts (alist-get 'acts payload))
         (edges (alist-get 'edges payload))
         (regions (xiang-scope-overlay--regions))
         (by-id (make-hash-table :test #'equal)))
    (setq xiang-scope-overlay--missing 0)
    (let* ((reply-text (alist-get 'reply_text payload))
           (reply-turn (alist-get 'turn payload))
           (agent-regions (cl-remove-if-not
                           (lambda (r) (and (equal "agent" (cadr r))
                                            (equal reply-turn (car r))))
                           regions))
           (recorded-length (apply #'+ (mapcar (lambda (r)
                                                 (- (marker-position (cdr (cddr r)))
                                                    (marker-position (car (cddr r)))))
                                               agent-regions))))
      (when (and reply-text (/= (length reply-text)
                                (+ recorded-length (* 2 (max 0 (1- (length agent-regions)))))))
        (message "xiang scope reply length mismatch: recorded=%d route=%d"
                 recorded-length (length reply-text))))
    (setq xiang-scope-overlay--parents (make-hash-table :test #'equal))
    (dolist (act acts) (puthash (alist-get 'id act) act by-id))
    (dolist (act acts)
      (if-let ((pos (xiang-scope-overlay--act-position act regions)))
        (let* ((id (alist-get 'id act))
               (edge (cl-find id edges :key (lambda (e) (alist-get 'act e)) :test #'equal))
               (parent (and edge (gethash (alist-get 'within edge) by-id)))
               (ov (make-overlay pos
                                 (+ pos (length (or (alist-get 'text act) ""))))))
          (overlay-put ov 'face (if (equal "operator" (alist-get 'side act))
                                   'xiang-scope-operator-face
                                 'xiang-scope-agent-face))
          (overlay-put ov 'xiang-scope-act id)
          (overlay-put ov 'help-echo
                       (if edge
                           (format "%s · within %s → %s"
                                   (or (alist-get 'kind act) "act")
                                   (alist-get 'rule edge)
                                   (xiang-scope-overlay--parent-text
                                    (alist-get 'within edge) acts))
                         ;; the turn's first act, or an act no rule reached
                         (format "%s · no governing act in this turn"
                                 (or (alist-get 'kind act) "act"))))
          (when (equal "OPEN" (alist-get 'rule edge))
            (overlay-put ov 'face 'xiang-scope-open-face))
          (push ov xiang-scope-overlay--overlays)
          (when parent (puthash id (alist-get 'within edge) xiang-scope-overlay--parents))))
        (setq xiang-scope-overlay--missing (1+ xiang-scope-overlay--missing))))
    (when (> xiang-scope-overlay--missing 0)
      (message "%d acts had no region" xiang-scope-overlay--missing)))

(defun xiang-scope-overlay--url (&optional turn)
  (format "%s/api/alpha/xiang/scopes?session=%s&turn=%s"
          (string-remove-suffix "/" xiang-scope-overlay-base-url)
          (url-hexify-string agent-chat--session-id)
          (url-hexify-string (or turn agent-chat--current-turn-id ""))))

(defun xiang-scope-overlay--callback (buffer status)
  (let ((body (unless (plist-get status :error)
                (goto-char (point-min))
                (re-search-forward "^$" nil t)
                (buffer-substring-no-properties (point) (point-max)))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (if (plist-get status :error)
            (message "xiang scope fetch failed: %s" (plist-get status :error))
          (condition-case err
              (xiang-scope-overlay--apply
               (json-parse-string body :object-type 'alist :array-type 'list))
            (error (message "xiang scope fetch failed: %s"
                            (error-message-string err)))))))))

(defun xiang-scope-overlay-refresh ()
  "Asynchronously refresh scope overlays for the current turn."
  (interactive)
  (xiang-scope-overlay--delete)
  (let* ((buffer (current-buffer))
         (current (or agent-chat--current-turn-id ""))
         (session (or agent-chat--session-id ""))
         (previous (cl-loop for r in (xiang-scope-overlay--regions)
                            for turn = (car r)
                            unless (equal turn current) return turn))
         (current-url (format "%s/api/alpha/xiang/scopes?session=%s&turn=%s"
                              (string-remove-suffix "/" xiang-scope-overlay-base-url)
                              (url-hexify-string session)
                              (url-hexify-string current)))
         (latest-url (format "%s/api/alpha/xiang/scopes?session=%s&turn=latest"
                            (string-remove-suffix "/" xiang-scope-overlay-base-url)
                            (url-hexify-string session))))
    (cl-labels ((fetch (url fallback)
                  (condition-case err
                      (funcall xiang-scope-overlay-fetch-function
                               url
                               (lambda (status)
                                 (if (and fallback (plist-get status :error)
                                          (string-match-p "404"
                                                          (format "%s" (plist-get status :error))))
                                     (progn
                                       (message "xiang scope 404; falling back to turn=latest")
                                       (fetch latest-url nil)
                                       ;; The analysis file can say "analyzed" a moment before the
                                       ;; route's view of the session has the turn's acts (seen on
                                       ;; turn 151: the landing refresh painted the previous turn).
                                       ;; Ask once more for the current turn after a short wait.
                                       (when (and xiang-scope-overlay-retry-seconds
                                                  (not (string-empty-p current)))
                                         (run-at-time xiang-scope-overlay-retry-seconds nil
                                                      (lambda ()
                                                        (when (buffer-live-p buffer)
                                                          (with-current-buffer buffer
                                                            (when xiang-scope-overlay-mode
                                                              (let ((xiang-scope-overlay-retry-seconds nil))
                                                                (xiang-scope-overlay-refresh)))))))))
                                   (xiang-scope-overlay--callback buffer status))))
                    (error (message "xiang scope fetch failed: %s"
                                    (error-message-string err))))))
      (fetch current-url latest-url))))

(defun xiang-scope-overlay-jump-to-parent ()
  "Jump to the governing act at point." 
  (interactive)
  (let ((id (cl-some (lambda (ov)
                       (and (overlay-buffer ov)
                            (<= (overlay-start ov) (point))
                            (< (point) (overlay-end ov))
                            (overlay-get ov 'xiang-scope-act)))
                     xiang-scope-overlay--overlays)))
    (if-let ((parent (and id (gethash id xiang-scope-overlay--parents))))
        (when-let ((ov (cl-find parent xiang-scope-overlay--overlays
                                :key (lambda (o) (overlay-get o 'xiang-scope-act))
                                :test #'equal)))
          (goto-char (overlay-start ov)))
      (message "No governing scope act at point"))))

(defun xiang-scope-overlay--at-point ()
  "The scope overlay under point, or nil."
  (cl-find-if (lambda (ov)
                (and (overlay-buffer ov)
                     (<= (overlay-start ov) (point))
                     (< (point) (overlay-end ov))))
              xiang-scope-overlay--overlays))

(defun xiang-scope-overlay-describe ()
  "Say in the echo area what the act at point is and what governs it.
The same text the GUI shows on hover; a terminal Emacs has no hover
(Joe, 2026-10-15), so this is the way to read an underline there."
  (interactive)
  (if-let ((ov (xiang-scope-overlay--at-point)))
      (message "%s" (overlay-get ov 'help-echo))
    (message "No scope act at point")))

(defvar-local xiang-scope-overlay--last-echoed nil)

(defun xiang-scope-overlay--echo-on-move ()
  "Echo the act under point when point enters a new scope overlay.
Silent while the minibuffer is active or when point leaves an overlay,
so it never fights another echo."
  (when (and xiang-scope-overlay-mode (not (minibufferp)))
    (let ((ov (xiang-scope-overlay--at-point)))
      (unless (eq ov xiang-scope-overlay--last-echoed)
        (setq xiang-scope-overlay--last-echoed ov)
        (when (and ov (not (current-message)))
          (message "%s" (overlay-get ov 'help-echo)))))))

(defun xiang-scope-overlay-list ()
  "List this turn's scope acts and edges as plain text in a side buffer."
  (interactive)
  (let ((rows (mapcar (lambda (ov)
                        (list (overlay-start ov)
                              (overlay-get ov 'xiang-scope-act)
                              (overlay-get ov 'help-echo)
                              (buffer-substring-no-properties
                               (overlay-start ov)
                               (min (overlay-end ov) (+ (overlay-start ov) 50)))))
                      (seq-filter #'overlay-buffer xiang-scope-overlay--overlays)))
        (source (current-buffer)))
    (with-current-buffer (get-buffer-create "*xiang scopes*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Scope acts in %s (%d), in buffer order. Each line: act id, then kind · within <rule> → governing act, then the act's first words.\n\n"
                        (buffer-name source) (length rows)))
        (dolist (row (sort rows (lambda (a b) (< (car a) (car b)))))
          (insert (format "%s\n    %s\n    \"%s\"\n" (nth 1 row) (nth 2 row)
                          (replace-regexp-in-string "\n" " " (nth 3 row)))))
        (special-mode)))
    (display-buffer "*xiang scopes*")))

(defvar xiang-scope-overlay-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-^") #'xiang-scope-overlay-jump-to-parent)
    (define-key map (kbd "C-c C-?") #'xiang-scope-overlay-describe)
    (define-key map (kbd "C-c C-|") #'xiang-scope-overlay-list)
    map))

;;;###autoload
(define-minor-mode xiang-scope-overlay-mode
  "Show scope acts and within edges in a REPL buffer."
  :lighter " scope"
  :keymap xiang-scope-overlay-mode-map
  (if xiang-scope-overlay-mode
      (progn
        (add-hook 'post-command-hook #'xiang-scope-overlay--echo-on-move nil t)
        (xiang-scope-overlay-refresh))
    (remove-hook 'post-command-hook #'xiang-scope-overlay--echo-on-move t)
    (xiang-scope-overlay--delete)))

(provide 'xiang-scope-overlay)
;;; xiang-scope-overlay.el ends here
