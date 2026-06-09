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
  '("clojure" "-M" "-m" "futon3c.scripts.mission-scope-view")
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

(defvar-local mission-mode--mission nil)
(defvar-local mission-mode--data nil)
(defvar-local mission-mode--overlays nil)
(defvar-local mission-mode--base-header-line nil)

(defvar mission-mode-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-m r") #'mission-mode-refresh)
    (define-key map (kbd "C-c C-m v") #'mission-mode-open-view)
    map)
  "Keymap for `mission-mode-minor-mode'.")

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
        (mission-mode-refresh))
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
               (string-match-p "/holes/missions/M-[^/]+\\.md\\'" path))
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

(defun mission-mode--find-passage-position (passage)
  "Return line-end position matching PASSAGE in the current buffer."
  (when (not (string-empty-p passage))
    (save-excursion
      (goto-char (point-min))
      (when (search-forward passage nil t)
        (line-end-position)))))

(defun mission-mode--overlay-label (scopes)
  "Return after-string label for SCOPES."
  (let ((labels
         (mapconcat
          (lambda (scope)
            (format "@scope %s %s"
                    (mission-mode--field scope :type)
                    (mission-mode--field scope :id)))
          scopes
          "  ")))
    (propertize (concat "  " labels)
                'face 'mission-mode-meta-face)))

(defun mission-mode--annotate-current-buffer (data)
  "Annotate the current mission buffer with scope DATA."
  (mission-mode--clear-overlays)
  (let* ((scopes (mission-mode--as-list (mission-mode--field data :scopes)))
         (by-passage (make-hash-table :test #'equal))
         (found 0))
    (dolist (scope scopes)
      (let ((passage (mission-mode--string (mission-mode--field scope :passage))))
        (when (not (string-empty-p passage))
          (puthash passage
                   (append (gethash passage by-passage) (list scope))
                   by-passage))))
    (maphash
     (lambda (passage group)
       (when-let ((pos (mission-mode--find-passage-position passage)))
         (let ((ov (make-overlay pos pos nil t t)))
           (overlay-put ov 'mission-mode t)
           (overlay-put ov 'after-string (mission-mode--overlay-label group))
           (push ov mission-mode--overlays)
           (cl-incf found))))
     by-passage)
    (setq header-line-format
          (propertize
           (format " @mission %s  @live substrate-2  @scopes %s  @shown %s  C-c C-m r refresh  C-c C-m v view "
                   (mission-mode--field data :mission)
                   (mission-mode--field data :scope_count)
                   found)
           'face 'mission-mode-meta-face))
    found))

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
