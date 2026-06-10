;;; mission-mode.el --- Live mission scope view for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Read-only Emacs view over substrate-2 mission-scope hyperedges.
;; Entry point: M-x mission-mode

;;; Code:

(require 'cl-lib)
(require 'button)
(require 'json)
(require 'subr-x)

(defgroup mission-mode nil
  "Live mission scope view."
  :group 'applications)

(defcustom mission-mode-project-dir "/home/joe/code/futon3c"
  "Directory from which to run the mission scope projection command."
  :type 'directory
  :group 'mission-mode)

(defcustom mission-mode-code-root "/home/joe/code"
  "Workspace root used when opening mission files."
  :type 'directory
  :group 'mission-mode)

(defcustom mission-mode-buffer-name "*mission-mode*"
  "Buffer name for the live mission scope view."
  :type 'string
  :group 'mission-mode)

(defcustom mission-mode-default-mission "M-mission-scopes-into-substrate-2"
  "Fallback mission ID when no chat buffer mission is clocked."
  :type 'string
  :group 'mission-mode)

(defcustom mission-mode-command
  '("bash" "scripts/mission-scope-view-fast.sh")
  "Command prefix used to fetch mission scope JSON."
  :type '(repeat string)
  :group 'mission-mode)

(defface mission-mode-title-face
  '((t :inherit font-lock-function-name-face :weight bold :height 1.2))
  "Face for mission-mode title lines."
  :group 'mission-mode)

(defface mission-mode-meta-face
  '((t :inherit font-lock-comment-face))
  "Face for mission-mode metadata."
  :group 'mission-mode)

(defface mission-mode-type-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for mission scope type labels."
  :group 'mission-mode)

(defface mission-mode-scope-face
  '((t :inherit font-lock-variable-name-face))
  "Face for mission scope IDs."
  :group 'mission-mode)

(defface mission-mode-warning-face
  '((t :inherit font-lock-warning-face :weight bold))
  "Face for detached or missing scope state."
  :group 'mission-mode)

;; Depth-aware scope region faces, following the futon6 showcase convention
;; (batch-008 viewer): outer = amber, nested levels shift hue through rose,
;; violet, indigo, slate; depth 5 caps the palette.
(defface mission-mode-depth-1-face
  '((((background light)) :background "#fdf3df" :extend t)
    (((background dark)) :background "#3a3322" :extend t))
  "Outermost scope region (amber)." :group 'mission-mode)
(defface mission-mode-depth-2-face
  '((((background light)) :background "#fcebf2" :extend t)
    (((background dark)) :background "#3a2531" :extend t))
  "Depth-2 scope region (rose)." :group 'mission-mode)
(defface mission-mode-depth-3-face
  '((((background light)) :background "#efe9fc" :extend t)
    (((background dark)) :background "#2e2742" :extend t))
  "Depth-3 scope region (violet)." :group 'mission-mode)
(defface mission-mode-depth-4-face
  '((((background light)) :background "#e5eafb" :extend t)
    (((background dark)) :background "#252d46" :extend t))
  "Depth-4 scope region (indigo)." :group 'mission-mode)
(defface mission-mode-depth-5-face
  '((((background light)) :background "#e8edf2" :extend t)
    (((background dark)) :background "#2b3138" :extend t))
  "Depth-5+ scope region (slate)." :group 'mission-mode)

;; Badge (scope-label) faces per binder class, echoing the showcase's
;; colored scope-label chips.
(defface mission-mode-badge-phase-face
  '((t :background "#0f766e" :foreground "white" :weight bold :height 0.85))
  "Badge for eightfold-phase scopes." :group 'mission-mode)
(defface mission-mode-badge-section-face
  '((t :background "#b45309" :foreground "white" :weight bold :height 0.85))
  "Badge for loose-section scopes." :group 'mission-mode)
(defface mission-mode-badge-pattern-face
  '((t :background "#7c3aed" :foreground "white" :weight bold :height 0.85))
  "Badge for pattern/psr/pur scopes." :group 'mission-mode)
(defface mission-mode-badge-capability-face
  '((t :background "#15803d" :foreground "white" :weight bold :height 0.85))
  "Badge for capability scopes." :group 'mission-mode)
(defface mission-mode-badge-map-face
  '((t :background "#c2570a" :foreground "white" :weight bold :height 0.85))
  "Badge for map-item scopes." :group 'mission-mode)
(defface mission-mode-badge-other-face
  '((t :background "#475569" :foreground "white" :weight bold :height 0.85))
  "Badge for other binder types." :group 'mission-mode)

(defvar mission-mode--posframe-available
  (or (require 'posframe nil t)
      ;; The serving daemons don't always carry the posframe build dir on
      ;; load-path even when straight has built it — add it defensively.
      (let ((dir "/home/joe/.emacs-graph/straight/build/posframe"))
        (when (file-directory-p dir)
          (add-to-list 'load-path dir)
          (require 'posframe nil t))))
  "Cached availability of the posframe feature (require once, not per hover).")

(defvar-local mission-mode--mission nil)
(defvar-local mission-mode--data nil)
(defvar-local mission-mode--overlays nil)
(defvar-local mission-mode--base-header-line nil)

(defvar mission-mode-minor-mode-map
  (let ((map (make-sparse-keymap)))
    ;; `C-c m' prefix — NOT `C-c C-m', which is `C-c RET'.
    (define-key map (kbd "C-c m r") #'mission-mode-refresh)
    (define-key map (kbd "C-c m R") #'mission-mode-reingest)
    (define-key map (kbd "C-c m v") #'mission-mode-open-view)
    (define-key map (kbd "C-c m i") #'mission-mode-inspect-at-point)
    (define-key map (kbd "C-c m o") #'mission-mode-overview)
    map)
  "Keymap for `mission-mode-minor-mode'.")

(defun mission-mode-reingest ()
  "Re-detect + re-ingest this mission's scopes (via the live JVM), then refresh.
The write-a-section → save → `C-c m R' loop is what lets the overview panel
build up live as a greenfield mission grows."
  (interactive)
  (let* ((doc (buffer-file-name))
         (buf (current-buffer))
         (proc (start-process "mission-reingest" "*mission-reingest*" "bash"
                              "/home/joe/code/futon3c/scripts/mission-scope-reingest.sh"
                              doc)))
    (message "mission-mode: reingest started…")
    (set-process-sentinel
     proc
     (lambda (p _e)
       (when (memq (process-status p) '(exit signal))
         (if (and (eq (process-status p) 'exit)
                  (zerop (process-exit-status p)))
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (mission-mode-refresh)
                 (when (get-buffer "*mission-overview*")
                   (mission-mode-overview))
                 (message "mission-mode: reingest done")))
           (message "mission-mode: reingest FAILED — see *mission-reingest*")))))))

(defvar mission-scope-view-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'mission-mode-refresh)
    (define-key map (kbd "o") #'mission-mode-open-mission-file)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for mission scope view buffers.")

(define-derived-mode mission-scope-view-mode special-mode "Mission"
  "Major mode for the live mission scope view."
  (setq-local truncate-lines nil
              buffer-read-only t)
  (when (fboundp 'arxana-ui-mark-managed)
    (arxana-ui-mark-managed "Mission Mode")))

(define-minor-mode mission-mode-minor-mode
  "Show live substrate-2 mission scopes as read-only overlays."
  :lighter " Mission"
  :keymap mission-mode-minor-mode-map
  (if mission-mode-minor-mode
      (progn
        (setq mission-mode--base-header-line header-line-format)
        (add-hook 'post-command-hook #'mission-mode--hover-post-command nil t)
        (mission-mode-refresh))
    (remove-hook 'post-command-hook #'mission-mode--hover-post-command t)
    (mission-mode--hover-hide)
    (mission-mode--clear-overlays)
    (setq header-line-format mission-mode--base-header-line)))

(defun mission-mode--field (object key)
  "Return KEY from OBJECT, accepting alists and hash tables."
  (let* ((raw (symbol-name key))
         (plain (string-remove-prefix ":" raw))
         (plain-symbol (intern plain)))
    (cond
     ((hash-table-p object) (gethash plain object))
     ((listp object)
      (or (alist-get key object nil nil #'eq)
          (alist-get plain-symbol object nil nil #'eq)
          (alist-get plain object nil nil #'equal)))
     (t nil))))

(defun mission-mode--as-list (value)
  "Return VALUE as a list."
  (cond
   ((null value) nil)
   ((vectorp value) (append value nil))
   ((listp value) value)
   (t (list value))))

(defun mission-mode--string (value)
  "Return VALUE as a display string."
  (cond
   ((null value) "")
   ((stringp value) value)
   ((symbolp value) (symbol-name value))
   (t (format "%s" value))))

(defun mission-mode--clocked-mission ()
  "Return the mission clocked in the current chat buffer, if any."
  (and (boundp 'agent-chat--mission-id)
       (stringp agent-chat--mission-id)
       (not (string-empty-p agent-chat--mission-id))
       agent-chat--mission-id))

(defun mission-mode--read-mission ()
  "Prompt for a mission ID, defaulting to the clocked mission."
  (read-string "Mission: "
               (or (mission-mode--clocked-mission)
                   mission-mode-default-mission)))

(defun mission-mode--mission-from-path (&optional path)
  "Return mission ID implied by PATH, or nil."
  (let ((path (or path buffer-file-name)))
    (when (and path
               ;; M- missions and E- excursions both carry scope structure.
               (string-match-p "/holes/missions/[ME]-[^/]+\\.md\\'" path))
      (file-name-sans-extension (file-name-nondirectory path)))))

(defun mission-mode--current-mission ()
  "Return best mission ID for the current buffer."
  (or mission-mode--mission
      (mission-mode--mission-from-path)
      (mission-mode--clocked-mission)))

(defun mission-mode--fetch (mission)
  "Fetch live mission scope JSON for MISSION."
  (let* ((args (append mission-mode-command (list "--mission" mission)))
         (program (car args))
         (program-args (cdr args))
         (default-directory mission-mode-project-dir))
    (with-temp-buffer
      (let ((status (apply #'process-file program nil t nil program-args)))
        (unless (and (integerp status) (= status 0))
          (error "mission scope fetch failed: %s" (buffer-string))))
      (let ((json-object-type 'alist)
            (json-array-type 'list)
            (json-false nil)
            (json-null nil))
        (json-read-from-string (buffer-string))))))

(defun mission-mode--state-face (state)
  "Return face for anchor or parent STATE."
  (if (member state '("detached" "missing" "nil" ""))
      'mission-mode-warning-face
    'mission-mode-meta-face))

(defun mission-mode--clear-overlays ()
  "Remove mission-mode overlays from the current buffer."
  (mapc #'delete-overlay mission-mode--overlays)
  (setq mission-mode--overlays nil))

(defun mission-mode--find-passage-start (passage)
  "Return line-beginning position matching PASSAGE in the current buffer."
  (when (not (string-empty-p passage))
    (save-excursion
      (goto-char (point-min))
      (when (search-forward passage nil t)
        (goto-char (match-beginning 0))
        (line-beginning-position)))))

(defun mission-mode--heading-region (pos)
  "If POS is on a markdown heading line, return its section as (BEG . END).
The section runs from the heading line to just before the next heading of
the same or shallower level."
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (when (looking-at "^\\(#+\\)[ \t]")
      (let ((level (length (match-string 1)))
            (beg (point)))
        (forward-line 1)
        (let ((end (catch 'mission-mode--section-end
                     (while (re-search-forward "^\\(#+\\)[ \t]" nil t)
                       (when (<= (length (match-string 1)) level)
                         (throw 'mission-mode--section-end (match-beginning 0))))
                     (point-max))))
          (cons beg end))))))

(defun mission-mode--scope-region (pos)
  "Region for a scope anchored at POS: its section if a heading, else its line."
  (or (mission-mode--heading-region pos)
      (save-excursion
        (goto-char pos)
        (cons (line-beginning-position)
              (min (point-max) (1+ (line-end-position)))))))

(defun mission-mode--depth-face (depth)
  (pcase (min depth 5)
    (1 'mission-mode-depth-1-face)
    (2 'mission-mode-depth-2-face)
    (3 'mission-mode-depth-3-face)
    (4 'mission-mode-depth-4-face)
    (_ 'mission-mode-depth-5-face)))

(defun mission-mode--badge-face (type)
  (pcase type
    ("eightfold-phase" 'mission-mode-badge-phase-face)
    ("loose-section" 'mission-mode-badge-section-face)
    ((or "pattern" "psr" "pur") 'mission-mode-badge-pattern-face)
    ("capability-scope" 'mission-mode-badge-capability-face)
    ("map-item" 'mission-mode-badge-map-face)
    (_ 'mission-mode-badge-other-face)))

(defun mission-mode--scope-short-id (scope)
  (let ((id (mission-mode--string (mission-mode--field scope :id))))
    (if (string-match "[^/]+\\'" id) (match-string 0 id) id)))

(defun mission-mode--badge (scope)
  "Return a propertized scope-label chip for SCOPE."
  (let* ((type (mission-mode--string (mission-mode--field scope :type)))
         (anchor-state (mission-mode--string (mission-mode--field scope :anchor_state)))
         (bad (member anchor-state '("detached" "missing" "nil" "")))
         (face (if bad 'mission-mode-warning-face (mission-mode--badge-face type))))
    (propertize (format " %s·%s%s " type (mission-mode--scope-short-id scope)
                        (if bad "!" ""))
                'face face)))

(defun mission-mode--annotate-current-buffer (data)
  "Annotate the current mission buffer with scope DATA.
Renders Arxana-style anchored region overlays: depth-tinted backgrounds for
nested scopes, plus a colored scope-label chip at each anchor line."
  (mission-mode--clear-overlays)
  (let* ((raw-scopes (mission-mode--as-list (mission-mode--field data :scopes)))
         ;; The store can hold several id-generations of the same scope
         ;; (canonical + raw ids; no ingest-side GC yet) — dedupe by
         ;; (binder-type . anchor-passage) so unchanged headings don't
         ;; double-chip. Proper fix is ingest-side retraction.
         (seen (make-hash-table :test #'equal))
         (scopes (seq-filter
                  (lambda (scope)
                    (let ((key (cons (mission-mode--string (mission-mode--field scope :type))
                                     (mission-mode--string (mission-mode--field scope :passage)))))
                      (unless (gethash key seen)
                        (puthash key t seen))))
                  raw-scopes))
         (placed nil)   ;; (scope beg . end)
         (found 0))
    (dolist (scope scopes)
      (let* ((passage (mission-mode--string (mission-mode--field scope :passage)))
             (pos (mission-mode--find-passage-start passage)))
        (when pos
          (push (cons scope (mission-mode--scope-region pos)) placed)
          (cl-incf found))))
    ;; Region overlays, depth = number of strictly-containing other regions.
    (dolist (entry placed)
      (let* ((beg (cadr entry)) (end (cddr entry))
             (depth (1+ (cl-count-if
                         (lambda (other)
                           (and (not (eq other entry))
                                (<= (cadr other) beg) (>= (cddr other) end)
                                (or (< (cadr other) beg) (> (cddr other) end))))
                         placed)))
             (ov (make-overlay beg end nil t nil)))
        (overlay-put ov 'mission-mode t)
        (overlay-put ov 'mission-mode-scope (car entry))
        (overlay-put ov 'face (mission-mode--depth-face depth))
        (overlay-put ov 'priority (+ 10 depth))
        (overlay-put ov 'evaporate t)
        (push ov mission-mode--overlays)))
    ;; Scope-label chips, grouped per anchor line, at end of that line.
    (let ((by-line (make-hash-table :test #'eql)))
      (dolist (entry placed)
        (let ((line-end (save-excursion (goto-char (cadr entry)) (line-end-position))))
          (puthash line-end (append (gethash line-end by-line) (list (car entry)))
                   by-line)))
      (maphash
       (lambda (line-end group)
         (let ((ov (make-overlay line-end line-end nil t t)))
           (overlay-put ov 'mission-mode t)
           (overlay-put ov 'priority 30)
           (overlay-put ov 'after-string
                        (concat " " (mapconcat #'mission-mode--badge group " ")))
           (push ov mission-mode--overlays)))
       by-line))
    (setq header-line-format
          (propertize
           (format " @mission %s  @live substrate-2  @scopes %s  @shown %s  C-c m r refresh · o overview · i inspect · v view "
                   (mission-mode--field data :mission)
                   (mission-mode--field data :scope_count)
                   found)
           'face 'mission-mode-meta-face))
    found))

;; --- Hover (quick annotations): posframe on cursor-over, echo-area fallback.

(defvar-local mission-mode--hover-last nil)

(defconst mission-mode--hover-buffer " *mission-mode-hover*")

(defun mission-mode--scopes-at-point ()
  (delq nil (mapcar (lambda (o) (overlay-get o 'mission-mode-scope))
                    (overlays-at (point)))))

(defun mission-mode--hover-text (scopes)
  (mapconcat
   (lambda (scope)
     (concat
      (mission-mode--badge scope) "\n"
      (format "  id      %s\n" (mission-mode--field scope :id))
      (format "  parent  %s (%s)\n"
              (or (mission-mode--field scope :parent) "—")
              (or (mission-mode--field scope :parent_state) "?"))
      (format "  anchor  %s via %s"
              (or (mission-mode--field scope :anchor_state) "?")
              (or (mission-mode--field scope :anchor_resolve_by) "?"))))
   scopes "\n"))

(defun mission-mode--hover-hide ()
  (when (and mission-mode--posframe-available (display-graphic-p))
    (posframe-hide mission-mode--hover-buffer)))

(defun mission-mode--hover-show (scopes)
  (if (and mission-mode--posframe-available (display-graphic-p))
      ;; Float at the window's lower-right corner, away from point, so the
      ;; card never occludes the cursor or the region being read.
      (posframe-show mission-mode--hover-buffer
                     :string (mission-mode--hover-text scopes)
                     :position (point)
                     :poshandler #'posframe-poshandler-window-bottom-right-corner
                     :max-width 70
                     :border-width 1
                     :border-color "#9a8c78"
                     :background-color (face-attribute 'default :background)
                     :timeout 12)
    ;; Echo-area fallback: one compact line, not the full card.
    (let ((scope (car scopes)))
      (message "%s %s · anchor %s%s"
               (mission-mode--string (mission-mode--field scope :type))
               (mission-mode--scope-short-id scope)
               (or (mission-mode--field scope :anchor_state) "?")
               (if (cdr scopes) (format " (+%d more)" (length (cdr scopes))) "")))))

(defun mission-mode--hover-post-command ()
  "Show/hide the quick scope annotation as point moves between scopes."
  (let ((scopes (mission-mode--scopes-at-point)))
    (cond
     ((null scopes)
      (when mission-mode--hover-last
        (setq mission-mode--hover-last nil)
        (mission-mode--hover-hide)))
     ((not (equal scopes mission-mode--hover-last))
      (setq mission-mode--hover-last scopes)
      (mission-mode--hover-show scopes)))))

(defun mission-mode-inspect-at-point ()
  "Show full detail for the scopes at point in a side window (not selected).
The long-annotation counterpart of the quick hover posframe."
  (interactive)
  (let ((scopes (mission-mode--scopes-at-point))
        (buf (get-buffer-create "*mission-mode-inspect*")))
    (if (null scopes)
        (message "mission-mode: no scope at point")
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (special-mode)
          (dolist (scope scopes)
            (insert (mission-mode--badge scope) "\n")
            (dolist (key '(:id :type :parent :parent_state
                           :anchor_state :anchor_resolve_by :polarity :passage))
              (let ((v (mission-mode--field scope key)))
                (when v
                  (insert (propertize (format "  %-18s" (substring (symbol-name key) 1))
                                      'face 'mission-mode-type-face)
                          (format "%s\n" v)))))
            (insert "\n"))))
      (display-buffer buf '((display-buffer-in-side-window)
                            (side . right) (window-width . 52)
                            (inhibit-same-window . t))))))

;; --- Downstream lane: what substrate-2 associates with this mission beyond
;; its own text — the code files it touched (code/v05/file→mission) and the
;; missions it cross-references. Patterns are the upstream; this is the
;; downstream. Fetched from the WebArxana JSON proxy (futon1a speaks EDN).

(defcustom mission-mode-proxy-url "http://localhost:3100"
  "Base URL of the WebArxana proxy used for JSON hyperedge queries."
  :type 'string :group 'mission-mode)

(defun mission-mode--mission-entity-ids ()
  "Candidate substrate-2 mission entity ids for the buffer's file.
The watcher's repo labels are irregular (futon6 → futon6-py-d,
futon4 → futon4-elisp-d, futon5 → futon5-d2), so return candidates in
likelihood order; the fetch tries each until one has edges. Ids are
lowercased to match the watcher's entity convention."
  (when-let ((file (buffer-file-name)))
    (when (string-match "/code/\\([a-z0-9]+\\)/holes/missions/[ME]-\\(.+?\\)\\.md\\'" file)
      (let* ((repo (match-string 1 file))
             (file-name (file-name-sans-extension
                         (file-name-nondirectory file)))  ; e.g. E-mission-head
             (mid (downcase (match-string 2 file))))
        ;; Two id conventions coexist: the scope ingest lowercases and strips
        ;; the prefix (futon6-d/mission/e-mission-head); the doc watcher keeps
        ;; the full case-preserved filename (futon6-py-d/mission/E-mission-head).
        (apply #'append
               (mapcar (lambda (suffix)
                         (list (format "%s%s/mission/%s" repo suffix file-name)
                               (format "%s%s/mission/%s" repo suffix mid)))
                       '("-d" "-py-d" "-elisp-d" "-d2")))))))

(defun mission-mode--mission-entity-id ()
  "First candidate entity id (back-compat)."
  (car (mission-mode--mission-entity-ids)))

(defun mission-mode--fetch-downstream (entity-id)
  "Fetch (FILES . MISSIONS) associated with ENTITY-ID from substrate-2.
FILES are absolute paths under ~/code; MISSIONS are entity ids."
  (when entity-id
    (condition-case nil
        (let* ((url (format "%s/api/futon/hyperedges?end=%s&limit=300"
                            mission-mode-proxy-url (url-hexify-string entity-id)))
               (buf (url-retrieve-synchronously url t t 4))
               files missions)
          (when buf
            (with-current-buffer buf
              (goto-char (point-min))
              (when (search-forward "\n\n" nil t)
                ;; url-retrieve buffers are raw bytes: decode before parsing,
                ;; or the `→' in code/v05/file→mission never matches.
                (let* ((json-str (decode-coding-string
                                  (buffer-substring-no-properties (point) (point-max))
                                  'utf-8))
                       ;; json.el: objects → alists with symbol keys (hx/type, hx/ends).
                       (hxs (append (cdr (assq 'hyperedges
                                               (json-read-from-string json-str)))
                                    nil)))
                  (dolist (hx hxs)
                    (let ((type (format "%s" (or (cdr (assq 'hx/type hx)) "")))
                          (ends (append (cdr (assq 'hx/ends hx)) nil)))
                      (cond
                       ((string-match-p "file→mission\\|file->mission" type)
                        (dolist (end ends)
                          (let ((eid (cdr (assq 'entity-id end))))
                            (when (and (stringp eid)
                                       (string-match "\\`\\([a-z0-9]+\\)-[a-z]/file/\\(.+\\)\\'" eid))
                              (push (format "/home/joe/code/%s/%s"
                                            (match-string 1 eid) (match-string 2 eid))
                                    files)))))
                       ((string-match-p "mission-cross-ref" type)
                        (dolist (end ends)
                          (let ((eid (cdr (assq 'entity-id end))))
                            (when (and (stringp eid)
                                       (string-match-p "/mission/" eid)
                                       (not (equal eid entity-id)))
                              (push eid missions))))))))))
              (kill-buffer buf)))
          (cons (delete-dups (nreverse files))
                (delete-dups (nreverse missions))))
      (error nil))))

(defun mission-mode--mission-entity->file (eid)
  "Best-effort path of the mission doc for entity id EID."
  (when (string-match "\\`\\([a-z0-9]+\\)-[a-z]/mission/\\(.+\\)\\'" eid)
    (format "/home/joe/code/%s/holes/missions/M-%s.md"
            (match-string 1 eid) (match-string 2 eid))))

;; --- Overview panel: a "visual abstract" of the mission's nested scopes.
;; Scratch-like colored blocks, indented by nesting depth; moving point in
;; the panel scrolls the mission buffer to the scope under point (Arxana-style
;; linked navigation). RET jumps; q quits.

(defvar-local mission-mode--overview-source nil
  "The mission buffer this overview panel mirrors.")
(defvar-local mission-mode--overview-last-line nil)
(defvar-local mission-mode--overview-index nil
  "Alist of (DOC-OVERLAY-START . PANEL-POSITION) for scope blocks.")
(defvar-local mission-mode--overview-hl-overlays nil
  "Active cross-highlight overlays in the panel.")
(defvar-local mission-mode--overview-blocks nil
  "Foldable blocks: list of plists (:parent P :beg B :end E :ov OV :pinned BOOL).
OV is the invisibility overlay when folded, nil when open.")

(defface mission-mode-ref-highlight-face
  '((t :box (:line-width -1 :color "#0f766e") :weight bold))
  "Highlight for scope blocks referenced by the code line at point."
  :group 'mission-mode)

(defface mission-mode-ghost-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for ghost lines (canonical phases absent from the mission)."
  :group 'mission-mode)

(defconst mission-mode--health-sections
  '("if" "however" "then" "because")
  "Ordered sections shown in the overview vitals line.")

(defun mission-mode--overview-health-file (source-buffer)
  "Return the health artifact path for SOURCE-BUFFER, if any."
  (when-let ((file (buffer-local-value 'buffer-file-name source-buffer)))
    (concat (file-name-sans-extension file) ".health.json")))

(defun mission-mode--read-health-artifact (source-buffer)
  "Read SOURCE-BUFFER's adjacent health artifact, or nil.
The health artifact is optional; missing or malformed files produce no panel
noise."
  (when-let ((file (mission-mode--overview-health-file source-buffer)))
    (when (file-readable-p file)
      (condition-case nil
          (let ((json-object-type 'alist)
                (json-array-type 'list)
                (json-false nil)
                (json-null nil)
                (coding-system-for-read 'utf-8))
            (json-read-file file))
        (error nil)))))

(defun mission-mode--health-value (object key)
  "Return KEY from health artifact OBJECT."
  (mission-mode--field object key))

(defun mission-mode--health-section-name (value)
  "Normalize health section VALUE to a lowercase name."
  (let ((s (mission-mode--string value)))
    (downcase (string-remove-prefix ":" s))))

(defun mission-mode--health-section-set (values)
  "Return a hash set for normalized health section VALUES."
  (let ((set (make-hash-table :test #'equal)))
    (dolist (value (mission-mode--as-list values))
      (puthash (mission-mode--health-section-name value) t set))
    set))

(defun mission-mode--health-mark-string (anchor-proximity)
  "Return per-section vitals marks for ANCHOR-PROXIMITY, or nil.
Strong anchors render as check marks; weak anchors render as crossed marks."
  (when anchor-proximity
    (let ((strong (mission-mode--health-section-set
                   (mission-mode--health-value anchor-proximity :strong)))
          (weak (mission-mode--health-section-set
                 (mission-mode--health-value anchor-proximity :weak))))
      (string-join
       (mapcar
        (lambda (section)
          (format "%s%s"
                  (cond
                   ((gethash section strong) "✓")
                   ((gethash section weak) "⊗")
                   (t "·"))
                  (upcase section)))
        mission-mode--health-sections)
       " "))))

(defun mission-mode--health-percent (value)
  "Format VALUE as a whole percentage."
  (if (numberp value)
      (format "%d%%" (round (* 100 value)))
    "--"))

(defun mission-mode--health-confidence (value)
  "Format VALUE as a two-decimal confidence."
  (if (numberp value)
      (format "%.2f" value)
    "--"))

(defun mission-mode--overview-insert-vitals (source-buffer)
  "Insert SOURCE-BUFFER's mission health vitals, when available."
  (when-let* ((artifact (mission-mode--read-health-artifact source-buffer))
              (health (mission-mode--health-value artifact :health)))
    (let* ((confidence (mission-mode--health-value health :bit-confidence))
           (xeno (mission-mode--health-value health :xenotype-completeness))
           (anchor-proximity (mission-mode--health-value health :anchor-proximity))
           (marks (mission-mode--health-mark-string anchor-proximity))
           (reading (mission-mode--string
                     (mission-mode--health-value health :reading)))
           (generated-at (mission-mode--string
                          (mission-mode--health-value artifact :generated-at)))
           (generator (mission-mode--string
                       (mission-mode--health-value artifact :generator)))
           (help (string-trim
                  (string-join
                   (delq nil
                         (list (unless (string-empty-p generated-at)
                                 (format "generated-at: %s" generated-at))
                               (unless (string-empty-p generator)
                                 (format "generator: %s" generator))))
                   "\n")))
           (line-start (point)))
      (insert " ♥ vitals  ")
      (insert (propertize
               (format "conf %s" (mission-mode--health-confidence confidence))
               'face (if (and (numberp confidence) (< confidence 0.30))
                         'mission-mode-warning-face
                       'mission-mode-meta-face)))
      (insert (format " · xeno %s" (mission-mode--health-percent xeno)))
      (when marks
        (insert " · " marks))
      (insert "\n")
      (unless (string-empty-p reading)
        (insert "   "
                (propertize (format "\"%s\"" reading)
                            'face 'mission-mode-ghost-face)
                "\n"))
      (unless (string-empty-p help)
        (add-text-properties line-start (point)
                             (list 'help-echo help)))
      (insert "\n"))))

(defconst mission-mode--canon-phases
  '("head" "identify" "map" "derive" "argue" "verify" "instantiate" "document")
  "The eightfold canon, in order.")

(defun mission-mode--scope-canon-phase (scope title)
  "Canon phase name for SCOPE (an eightfold scope), from id tail or TITLE."
  (let ((id (downcase (mission-mode--string (mission-mode--field scope :id))))
        (title (downcase (or title ""))))
    (or (seq-find (lambda (p) (string-suffix-p (concat "/" p) id))
                  mission-mode--canon-phases)
        (seq-find (lambda (p) (string-match-p (concat "\\_<" p "\\_>") title))
                  mission-mode--canon-phases))))

(defun mission-mode--region-overlays ()
  "Mission-mode region overlays in the current buffer, sorted by position."
  (sort (seq-filter (lambda (o) (overlay-get o 'mission-mode-scope))
                    mission-mode--overlays)
        (lambda (a b) (or (< (overlay-start a) (overlay-start b))
                          (and (= (overlay-start a) (overlay-start b))
                               (> (overlay-end a) (overlay-end b)))))))

(defun mission-mode--overview-title (ov)
  "Readable title for the scope overlay OV: its anchor line, de-markdowned."
  (with-current-buffer (overlay-buffer ov)
    (save-excursion
      (goto-char (overlay-start ov))
      (let ((line (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position))))
        (string-trim (replace-regexp-in-string "^#+[ \t]*" "" line))))))

(defun mission-mode--overview-clear-highlights ()
  (mapc #'delete-overlay mission-mode--overview-hl-overlays)
  (setq mission-mode--overview-hl-overlays nil))

(defun mission-mode--overview-sync ()
  "Scroll the mission buffer to the entry on the current panel line, and
cross-highlight the scope blocks that mention the code file at point."
  (let ((line (line-number-at-pos)))
    (unless (eql line mission-mode--overview-last-line)
      (setq mission-mode--overview-last-line line)
      ;; Linked scroll (scopes and code lines both carry a doc target).
      (when-let* ((target (get-text-property (line-beginning-position)
                                             'mission-mode-target))
                  (buf mission-mode--overview-source)
                  (win (and (buffer-live-p buf)
                            (or (get-buffer-window buf)
                                (display-buffer buf '(display-buffer-reuse-window))))))
        (set-window-point win target)
        (with-selected-window win
          (goto-char target)
          (recenter 2)))
      ;; Peek folding: reveal the folded block under point, refold others.
      (mission-mode--overview-peek)
      ;; Cross-highlight: code line → the scope blocks that mention it.
      (mission-mode--overview-clear-highlights)
      (when-let ((refs (get-text-property (line-beginning-position)
                                          'mission-mode-scope-refs)))
        (dolist (ref refs)
          (when-let ((pos (cdr (assq ref mission-mode--overview-index))))
            (let ((ov (make-overlay pos
                                    (save-excursion (goto-char pos)
                                                    (line-end-position)))))
              (overlay-put ov 'face 'mission-mode-ref-highlight-face)
              (push ov mission-mode--overview-hl-overlays))))))))

;; --- Folding: phase blocks fold their subsections; point on a folded
;; parent peeks its children open; TAB pins; `t' toggles all.

(defun mission-mode--block-fold (block)
  "Hide BLOCK's children with an invisibility overlay."
  (unless (plist-get block :ov)
    (let ((ov (make-overlay (plist-get block :beg) (plist-get block :end))))
      (overlay-put ov 'invisible t)
      (overlay-put ov 'mission-mode-fold t)
      (plist-put block :ov ov))))

(defun mission-mode--block-unfold (block)
  (when-let ((ov (plist-get block :ov)))
    (delete-overlay ov)
    (plist-put block :ov nil)))

(defun mission-mode--block-at (pos)
  "The block whose parent line or child region contains POS."
  (seq-find (lambda (b)
              (or (and (>= pos (plist-get b :parent))
                       (< pos (plist-get b :beg)))
                  (and (>= pos (plist-get b :beg))
                       (< pos (plist-get b :end)))))
            mission-mode--overview-blocks))

(defun mission-mode-overview-toggle ()
  "Toggle (pin) the fold of the block at point."
  (interactive)
  (if-let ((block (mission-mode--block-at (point))))
      (if (plist-get block :ov)
          (progn (mission-mode--block-unfold block)
                 (plist-put block :pinned t))
        (plist-put block :pinned nil)
        (mission-mode--block-fold block))
    (message "mission-mode: no foldable block at point")))

(defun mission-mode-overview-toggle-all ()
  "Show or hide all subsections."
  (interactive)
  (let ((any-folded (seq-some (lambda (b) (plist-get b :ov))
                              mission-mode--overview-blocks)))
    (dolist (b mission-mode--overview-blocks)
      (if any-folded
          (progn (mission-mode--block-unfold b) (plist-put b :pinned t))
        (plist-put b :pinned nil)
        (mission-mode--block-fold b)))))

(defun mission-mode--overview-peek ()
  "Peek-open the folded block under point; re-fold unpinned blocks left behind."
  (let ((here (mission-mode--block-at (point))))
    (dolist (b mission-mode--overview-blocks)
      (cond
       ((eq b here)
        (mission-mode--block-unfold b))
       ((and (not (plist-get b :pinned)) (not (plist-get b :ov)))
        (mission-mode--block-fold b))))))

(defun mission-mode-overview-visit ()
  "Visit the thing under point: scope in the mission buffer, or a
downstream file / related mission in another window."
  (interactive)
  (let ((file (get-text-property (line-beginning-position) 'mission-mode-file))
        (target (get-text-property (line-beginning-position) 'mission-mode-target)))
    (cond
     (file
      (if (file-exists-p file)
          (find-file-other-window file)
        (message "mission-mode: %s not found on disk" file)))
     (target
      (when-let* ((buf mission-mode--overview-source)
                  (win (get-buffer-window buf)))
        (select-window win)
        (goto-char target))))))

(defvar mission-mode-overview-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'mission-mode-overview-visit)
    (define-key map (kbd "TAB") #'mission-mode-overview-toggle)
    (define-key map (kbd "t") #'mission-mode-overview-toggle-all)
    (define-key map (kbd "g") #'mission-mode-overview)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `mission-mode-overview-mode'.")

(defun mission-mode--tree-head-concepts (mission)
  "Concept terms the detector attached to MISSION's HEAD scope, from the
scope-tree JSON on disk (the view projection drops concept ends)."
  (let ((file (format "/home/joe/code/futon6/data/mission-scope-trees/%s.json"
                      mission)))
    (when (file-readable-p file)
      (condition-case nil
          (let* ((tree (json-read-file file))
                 (hxs (append (cdr (assq 'scope-hyperedges tree)) nil)))
            (catch 'mission-mode--head
              (dolist (hx hxs)
                (when (equal "eightfold-phase" (cdr (assq 'binder-type hx)))
                  (let ((ends (append (cdr (assq 'ends hx)) nil)))
                    (when (seq-some (lambda (e) (equal "head" (cdr (assq 'phase e))))
                                    ends)
                      (throw 'mission-mode--head
                             (delq nil (mapcar (lambda (e) (cdr (assq 'term e)))
                                               ends)))))))
              nil))
        (error nil)))))

(define-derived-mode mission-mode-overview-mode special-mode "MissionOverview"
  "Visual abstract of a mission's nested scopes; point motion scrolls the mission."
  (setq-local truncate-lines t cursor-in-non-selected-windows nil)
  (add-hook 'post-command-hook #'mission-mode--overview-sync nil t))

(defun mission-mode-overview ()
  "Show the mission's scopes as a nested, colored visual abstract.
Moving point in the panel scrolls the mission buffer to the scope under
point; RET jumps to it."
  (interactive)
  (let* ((src (if mission-mode--overview-source
                  mission-mode--overview-source
                (current-buffer)))
         (ovs (with-current-buffer src (mission-mode--region-overlays)))
         (mission (with-current-buffer src mission-mode--mission))
         (buf (get-buffer-create "*mission-overview*")))
    (if (null ovs)
        (message "mission-mode: no scopes annotated (run mission-mode first)")
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (mission-mode-overview-mode)
          (setq mission-mode--overview-source src
                mission-mode--overview-last-line nil)
          (insert (propertize (format " %s — %d scopes\n"
                                      (or mission "mission") (length ovs))
                              'face 'mission-mode-title-face))
          (insert (propertize
                   " keys: TAB fold/pin · t expand/collapse all · RET visit · g rebuild · q quit\n\n"
                   'face 'mission-mode-meta-face))
          (mission-mode--overview-insert-vitals src)
          (setq mission-mode--overview-index nil)
          ;; Which canon phases are placed at all (for ghost interleaving).
          (let* ((placed-phases
                  (delq nil (mapcar
                             (lambda (ov)
                               (let ((s (overlay-get ov 'mission-mode-scope)))
                                 (when (equal "eightfold-phase"
                                              (mission-mode--string (mission-mode--field s :type)))
                                   (mission-mode--scope-canon-phase
                                    s (mission-mode--overview-title ov)))))
                             ovs)))
                 (ghosted nil)
                 (emit-ghosts-before
                  (lambda (phase)
                    ;; Ghost every canon phase that precedes PHASE in the canon,
                    ;; is absent from the doc, and hasn't been ghosted yet.
                    (dolist (p (seq-take-while (lambda (p) (not (equal p phase)))
                                               mission-mode--canon-phases))
                      (unless (or (member p placed-phases) (member p ghosted))
                        (push p ghosted)
                        (insert (propertize (format " ∅ %s — phase absent\n" (upcase p))
                                            'face 'mission-mode-ghost-face)))))))
            (setq mission-mode--overview-blocks nil)
            (let ((line-entries nil))   ;; (panel-pos depth eightfold?)
              (dolist (ov ovs)
                (let* ((scope (overlay-get ov 'mission-mode-scope))
                       (type (mission-mode--string (mission-mode--field scope :type)))
                       (depth (max 1 (- (or (overlay-get ov 'priority) 11) 10)))
                       (indent (make-string (* 2 (1- depth)) ?\s))
                       (title (mission-mode--overview-title ov))
                       (chip (mission-mode--badge scope))
                       (phase (and (equal type "eightfold-phase")
                                   (mission-mode--scope-canon-phase scope title)))
                       (start nil))
                  (when phase (funcall emit-ghosts-before phase))
                  (setq start (point))
                  (insert indent chip " "
                          (propertize (truncate-string-to-width title 46 nil nil "…")
                                      'face (mission-mode--depth-face depth))
                          "\n")
                  (push (list start depth (and phase t)) line-entries)
                  (push (cons (overlay-start ov) start) mission-mode--overview-index)
                  (add-text-properties start (point)
                                       (list 'mission-mode-target (overlay-start ov)))
                  ;; HEAD carries its extracted concepts — the seed vocabulary
                  ;; the mission improvises around.
                  (when (equal phase "head")
                    (when-let ((concepts (with-current-buffer src
                                           (mission-mode--tree-head-concepts
                                            mission-mode--mission))))
                      (let ((cstart (point)))
                        (insert indent "  "
                                (propertize (concat "⊙ " (string-join concepts " · "))
                                            'face 'mission-mode-meta-face)
                                "\n")
                        (push (list cstart (1+ depth) nil) line-entries))))))
              ;; Trailing ghosts (canon phases after the last placed one).
              (dolist (p mission-mode--canon-phases)
                (unless (or (member p placed-phases) (member p ghosted))
                  (push p ghosted)
                  (insert (propertize (format " ∅ %s — phase absent\n" (upcase p))
                                      'face 'mission-mode-ghost-face))))
              ;; Foldable blocks: each eightfold line folds the run of deeper
              ;; lines that follows it.
              (let ((entries (nreverse line-entries))
                    (scope-area-end (point)))
                (while entries
                  (let* ((entry (pop entries))
                         (pos (nth 0 entry))
                         (depth (nth 1 entry))
                         (phase? (nth 2 entry)))
                    (when phase?
                      (let* ((child-beg (or (caar entries) scope-area-end))
                             (child-end (or (caar (seq-drop-while
                                                   (lambda (e) (> (nth 1 e) depth))
                                                   entries))
                                            scope-area-end)))
                        (when (> child-end child-beg)
                          (push (list :parent pos :beg child-beg :end child-end
                                      :ov nil :pinned nil)
                                mission-mode--overview-blocks))))))
                (setq mission-mode--overview-blocks
                      (nreverse mission-mode--overview-blocks)))))
          ;; Downstream lane: substrate-2 associations beyond the doc's own
          ;; text — the code this mission touched, and lateral missions.
          (let* ((down (with-current-buffer src
                         (seq-some (lambda (eid)
                                     (let ((d (mission-mode--fetch-downstream eid)))
                                       (and d (or (car d) (cdr d)) d)))
                                   (mission-mode--mission-entity-ids))))
                 (files (car down))
                 (missions (cdr down)))
            (when files
              (insert "\n" (propertize (format " ↓ code (%d files, substrate-2)\n"
                                               (length files))
                                       'face 'mission-mode-title-face))
              (dolist (f files)
                (let* ((start (point))
                       (basename (file-name-nondirectory f))
                       ;; Which scope regions mention this file in the doc?
                       (mentions
                        (with-current-buffer src
                          (save-excursion
                            (goto-char (point-min))
                            (let (refs first-pos)
                              (while (search-forward basename nil t)
                                (unless first-pos (setq first-pos (match-beginning 0)))
                                (dolist (o (overlays-at (match-beginning 0)))
                                  (when (overlay-get o 'mission-mode-scope)
                                    (cl-pushnew (overlay-start o) refs))))
                              (cons first-pos refs)))))
                       (first-pos (car mentions))
                       (refs (cdr mentions)))
                  (insert "  "
                          (propertize (string-remove-prefix "/home/joe/code/" f)
                                      'face (if (file-exists-p f)
                                                'mission-mode-scope-face
                                              'mission-mode-warning-face))
                          (if refs (format "  ·%d" (length refs)) "")
                          "\n")
                  (add-text-properties
                   start (point)
                   (append (list 'mission-mode-file f)
                           (when first-pos (list 'mission-mode-target first-pos))
                           (when refs (list 'mission-mode-scope-refs refs)))))))
            (when missions
              (insert "\n" (propertize (format " ↔ missions (%d cross-refs)\n"
                                               (length missions))
                                       'face 'mission-mode-title-face))
              (dolist (m missions)
                (let ((start (point))
                      (mf (mission-mode--mission-entity->file m)))
                  (insert "  " (propertize m 'face 'mission-mode-meta-face) "\n")
                  (when mf
                    (add-text-properties start (point)
                                         (list 'mission-mode-file mf)))))))
          ;; Default: subsections folded; point peeks, TAB pins, `t' toggles all.
          (dolist (b mission-mode--overview-blocks)
            (mission-mode--block-fold b))
          (goto-char (point-min))))
      (display-buffer buf '((display-buffer-in-side-window)
                            (side . right) (window-width . 56)
                            (inhibit-same-window . t)))
      (select-window (get-buffer-window buf)))))

(defun mission-mode--insert-counts (data)
  "Insert type counts for DATA."
  (let ((counts (mission-mode--as-list (mission-mode--field data :type_counts))))
    (dolist (row counts)
      (insert (propertize (format "%-19s" (mission-mode--field row :type))
                          'face 'mission-mode-type-face))
      (insert (format " %s\n" (mission-mode--field row :count))))))

(defun mission-mode--insert-scope (scope)
  "Insert one SCOPE row."
  (let* ((id (mission-mode--string (mission-mode--field scope :id)))
         (title (mission-mode--string (mission-mode--field scope :title)))
         (parent (mission-mode--string (mission-mode--field scope :parent)))
         (anchor (mission-mode--string (mission-mode--field scope :anchor_state)))
         (parent-state (mission-mode--string (mission-mode--field scope :parent_state)))
         (passage (mission-mode--string (mission-mode--field scope :passage))))
    (insert "  ")
    (insert-text-button id
                        'face 'mission-mode-scope-face
                        'follow-link t
                        'help-echo passage
                        'action (lambda (_button)
                                  (message "%s" passage)))
    (when (not (string-empty-p title))
      (insert (format "  %s" title)))
    (insert "\n")
    (insert (propertize "    parent " 'face 'mission-mode-meta-face))
    (insert (if (string-empty-p parent) "-" parent))
    (insert (propertize "  anchor " 'face 'mission-mode-meta-face))
    (insert (propertize (if (string-empty-p anchor) "-" anchor)
                        'face (mission-mode--state-face anchor)))
    (insert (propertize "  parent-state " 'face 'mission-mode-meta-face))
    (insert (propertize (if (string-empty-p parent-state) "-" parent-state)
                        'face (mission-mode--state-face parent-state)))
    (when (not (string-empty-p passage))
      (insert "\n")
      (insert (propertize "    :: " 'face 'mission-mode-meta-face))
      (insert passage))
    (insert "\n")))

(defun mission-mode--render (data)
  "Render mission scope DATA in the current buffer."
  (let* ((mission (mission-mode--field data :mission))
         (scopes (mission-mode--as-list (mission-mode--field data :scopes)))
         (grouped (make-hash-table :test #'equal)))
    (dolist (scope scopes)
      (let ((type (mission-mode--string (mission-mode--field scope :type))))
        (puthash type (append (gethash type grouped) (list scope)) grouped)))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize (format "@mission %s\n" mission)
                          'face 'mission-mode-title-face))
      (insert (propertize
               (format "@live substrate-2  @scopes %s  @generated %s\n"
                       (mission-mode--field data :scope_count)
                       (mission-mode--field data :generated_at))
               'face 'mission-mode-meta-face))
      (insert (propertize "keys: g refresh  o open mission  RET show passage  q quit\n\n"
                          'face 'mission-mode-meta-face))
      (insert (propertize "@counts\n" 'face 'mission-mode-title-face))
      (mission-mode--insert-counts data)
      (insert "\n")
      (dolist (count-row (mission-mode--as-list (mission-mode--field data :type_counts)))
        (let* ((type (mission-mode--string (mission-mode--field count-row :type)))
               (group (gethash type grouped)))
          (insert (propertize (format "@scope-type %s\n" type)
                              'face 'mission-mode-type-face))
          (dolist (scope group)
            (mission-mode--insert-scope scope))
          (insert "\n")))
      (goto-char (point-min)))))

(defun mission-mode--render-error (mission err)
  "Render ERR for MISSION in the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize (format "@mission %s\n" mission)
                        'face 'mission-mode-title-face))
    (insert (propertize "@live unavailable\n\n" 'face 'mission-mode-warning-face))
    (insert (error-message-string err))
    (insert "\n")
    (goto-char (point-min))))

(defun mission-mode--show-minor-error (mission err)
  "Show refresh ERR for MISSION in the current buffer."
  (mission-mode--clear-overlays)
  (setq header-line-format
        (propertize
         (format " @mission %s  @live unavailable: %s "
                 mission
                 (error-message-string err))
         'face 'mission-mode-warning-face)))

;;;###autoload
(defun mission-mode-refresh ()
  "Refresh the current mission scope view."
  (interactive)
  (let ((mission (or (mission-mode--current-mission)
                     (mission-mode--read-mission))))
    (condition-case err
        (let ((data (mission-mode--fetch mission)))
          (setq mission-mode--mission mission
                mission-mode--data data)
          (if mission-mode-minor-mode
              (mission-mode--annotate-current-buffer data)
            (mission-mode--render data)))
      (error
       (if mission-mode-minor-mode
           (mission-mode--show-minor-error mission err)
         (mission-mode--render-error mission err))))))

(defun mission-mode--mission-file-candidates (mission)
  "Return likely markdown files for MISSION."
  (let ((name (concat mission ".md")))
    (directory-files-recursively mission-mode-code-root
                                 (concat "/holes/missions/" (regexp-quote name) "\\'"))))

;;;###autoload
(defun mission-mode-open-mission-file ()
  "Open the markdown file for the current mission."
  (interactive)
  (let* ((mission (or mission-mode--mission (mission-mode--read-mission)))
         (candidates (mission-mode--mission-file-candidates mission)))
    (if candidates
        (find-file (car candidates))
      (user-error "No mission file found for %s" mission))))

;;;###autoload
(defun mission-mode-open-view (&optional mission)
  "Open the full live mission scope view for MISSION."
  (interactive)
  (let* ((mission (or mission
                      (mission-mode--current-mission)
                      (mission-mode--read-mission)))
         (buf (get-buffer-create mission-mode-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'mission-scope-view-mode)
        (mission-scope-view-mode))
      (setq mission-mode--mission mission)
      (mission-mode-refresh))
    (pop-to-buffer buf)))

;;;###autoload
(defun mission-mode (&optional mission open-view)
  "Enable live scope overlays for MISSION, or open full view with OPEN-VIEW.
Interactively, `C-u M-x mission-mode' opens the full scope view.  Without a
prefix in a mission markdown buffer, this enables `mission-mode-minor-mode'
and annotates the current buffer."
  (interactive (list nil current-prefix-arg))
  (let ((mission (or mission (mission-mode--current-mission))))
    (if (or open-view (not (mission-mode--mission-from-path)))
        (mission-mode-open-view mission)
      (setq mission-mode--mission mission)
      (mission-mode-minor-mode 1))))

(provide 'mission-mode)
;;; mission-mode.el ends here
