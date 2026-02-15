;;; futon3c-code-blocks.el --- Sliding blackboard for code blocks -*- lexical-binding: t; -*-

;; Author: Claude + Joe
;; Description: Minor mode that intercepts markdown code fences in chat
;;   messages and extracts them to fontified side-window panels.
;;
;; Usage:
;;   (require 'futon3c-code-blocks)
;;   In a futon3c-chat or codex-repl buffer:
;;     M-x futon3c-code-blocks-mode

(require 'cl-lib)
(require 'futon3c-ui)

;;; Configuration

(defgroup futon3c-code-blocks nil
  "Sliding blackboard for code block extraction."
  :group 'futon3c-ui)

(defcustom futon3c-code-blocks-side 'right
  "Side of frame for code block panels."
  :type '(choice (const left) (const right) (const top) (const bottom))
  :group 'futon3c-code-blocks)

(defcustom futon3c-code-blocks-window-width 0.4
  "Width of code block side windows (fraction of frame)."
  :type 'float
  :group 'futon3c-code-blocks)

;;; Language -> mode mapping

(defvar futon3c-code-blocks-mode-alist
  '(("clojure" . clojure-mode) ("clj" . clojure-mode)
    ("elisp" . emacs-lisp-mode) ("emacs-lisp" . emacs-lisp-mode)
    ("lisp" . lisp-mode)
    ("python" . python-mode) ("py" . python-mode)
    ("javascript" . js-mode) ("js" . js-mode)
    ("typescript" . typescript-mode) ("ts" . typescript-mode)
    ("bash" . sh-mode) ("sh" . sh-mode) ("shell" . sh-mode) ("zsh" . sh-mode)
    ("json" . js-mode)
    ("yaml" . yaml-mode) ("yml" . yaml-mode)
    ("markdown" . markdown-mode) ("md" . markdown-mode)
    ("diff" . diff-mode) ("patch" . diff-mode)
    ("html" . html-mode) ("xml" . xml-mode)
    ("css" . css-mode) ("scss" . css-mode)
    ("sql" . sql-mode)
    ("rust" . rust-mode) ("rs" . rust-mode)
    ("go" . go-mode)
    ("ruby" . ruby-mode) ("rb" . ruby-mode)
    ("c" . c-mode) ("cpp" . c++-mode) ("c++" . c++-mode)
    ("java" . java-mode)
    ("toml" . conf-toml-mode)
    ("ini" . conf-mode)
    ("makefile" . makefile-mode))
  "Alist mapping language tags to Emacs major modes.")

;;; Faces

(defface futon3c-code-blocks-ref-face
  '((t :foreground "#bd93f9" :underline t))
  "Face for code block references in chat."
  :group 'futon3c-code-blocks)

(defface futon3c-code-blocks-header-face
  '((t :foreground "#6272a4" :weight bold))
  "Face for code block panel headers."
  :group 'futon3c-code-blocks)

;;; Internal state

(defvar-local futon3c-code-blocks--registry nil
  "List of code block entries.
Each entry is a plist: (:index N :lang STRING :content STRING
                        :buffer BUFFER :marker MARKER).")

(defvar-local futon3c-code-blocks--counter 0
  "Counter for code block numbering.")

(defvar-local futon3c-code-blocks--visible t
  "Whether code block panels are currently visible.")

;;; Code fence regex

(defconst futon3c-code-blocks--fence-re
  "```\\([a-zA-Z0-9_+-]*\\)\n\\(\\(?:.*\n\\)*?\\)```"
  "Regex matching markdown code fences.
Group 1: language tag (may be empty).
Group 2: code content.")

;;; Core functions

(defun futon3c-code-blocks--lang-mode (lang)
  "Return the major mode function for LANG, or `fundamental-mode'."
  (let ((mode (cdr (assoc (downcase (or lang "")) futon3c-code-blocks-mode-alist))))
    (if (and mode (fboundp mode))
        mode
      'fundamental-mode)))

(defun futon3c-code-blocks--create-panel (index lang content)
  "Create a buffer for code block INDEX with LANG and CONTENT.
Returns the buffer."
  (let* ((name (format "*code-%d%s*" index
                       (if (and lang (not (string-empty-p lang)))
                           (format " [%s]" lang)
                         "")))
         (buf (get-buffer-create name))
         (mode-fn (futon3c-code-blocks--lang-mode lang)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Header line
        (insert (propertize (format "--- code-%d: %s ---\n"
                                    index (or lang "text"))
                            'face 'futon3c-code-blocks-header-face
                            'read-only t
                            'front-sticky '(read-only)))
        ;; Content
        (insert content))
      ;; Set mode after inserting content
      (condition-case nil
          (funcall mode-fn)
        (error (fundamental-mode)))
      (goto-char (point-min))
      (forward-line 1))
    buf))

(defun futon3c-code-blocks--display-panel (buf index)
  "Display code block BUF in a side window with slot INDEX."
  (when futon3c-code-blocks--visible
    (display-buffer-in-side-window
     buf
     `((side . ,futon3c-code-blocks-side)
       (slot . ,index)
       (window-width . ,futon3c-code-blocks-window-width)
       (preserve-size . (t . nil))))))

(defun futon3c-code-blocks--make-ref (index lang)
  "Create a clickable reference string for code block INDEX with LANG."
  (let ((label (format "[code-%d: %s]" index (or lang "text"))))
    (propertize label
                'face 'futon3c-code-blocks-ref-face
                'futon3c-code-block-index index
                'mouse-face 'highlight
                'help-echo (format "Click to view code-%d" index)
                'keymap futon3c-code-blocks--ref-map)))

(defvar futon3c-code-blocks--ref-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'futon3c-code-blocks--click-ref)
    (define-key map (kbd "RET") #'futon3c-code-blocks--click-ref)
    map)
  "Keymap active on code block references.")

(defun futon3c-code-blocks--click-ref (event)
  "Handle click on a code block reference."
  (interactive "e")
  (let* ((pos (if (mouse-event-p event)
                  (posn-point (event-start event))
                (point)))
         (index (get-text-property pos 'futon3c-code-block-index)))
    (when index
      (futon3c-code-blocks-visit index))))

;;; Message interception

(defun futon3c-code-blocks--intercept (name text)
  "Hook function for `futon3c-ui--insert-message-hook'.
Replaces code fences in TEXT with clickable references when NAME
is the agent. Returns transformed text or nil."
  (when (and futon3c-code-blocks-mode
             (not (string= name "joe")))
    ;; First pass: find all code fences (using temp buffer for regex)
    (let ((matches nil))
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (while (re-search-forward futon3c-code-blocks--fence-re nil t)
          (push (list (match-beginning 0) (match-end 0)
                      (match-string 1) (string-trim-right (match-string 2)))
                matches)))
      ;; Second pass: create panels and build replacement text
      ;; (back in chat buffer, so buffer-local vars are correct)
      (when matches
        (setq matches (nreverse matches))
        (let ((result text)
              (shift 0))
          (dolist (m matches)
            (cl-destructuring-bind (beg end lang content) m
              (let* ((index (cl-incf futon3c-code-blocks--counter))
                     (buf (futon3c-code-blocks--create-panel index lang content))
                     (entry (list :index index :lang lang :content content
                                  :buffer buf))
                     (ref (futon3c-code-blocks--make-ref index lang))
                     (old-len (- end beg))
                     (new-len (length ref)))
                (push entry futon3c-code-blocks--registry)
                (futon3c-code-blocks--display-panel buf index)
                ;; Replace in result string, adjusting for prior shifts
                (setq result (concat (substring result 0 (+ beg shift))
                                     ref
                                     (substring result (+ end shift))))
                (setq shift (+ shift (- new-len old-len))))))
          result)))))

;;; Navigation

(defun futon3c-code-blocks-visit (index)
  "Switch to the side window showing code block INDEX."
  (interactive "nCode block number: ")
  (let ((entry (cl-find-if (lambda (e) (= (plist-get e :index) index))
                           futon3c-code-blocks--registry)))
    (if (and entry (buffer-live-p (plist-get entry :buffer)))
        (let ((win (get-buffer-window (plist-get entry :buffer))))
          (if win
              (select-window win)
            ;; Panel was closed; re-display it
            (futon3c-code-blocks--display-panel (plist-get entry :buffer) index)
            (when-let ((w (get-buffer-window (plist-get entry :buffer))))
              (select-window w))))
      (message "Code block %d not found" index))))

(defun futon3c-code-blocks-next ()
  "Navigate to the next code block panel."
  (interactive)
  (when futon3c-code-blocks--registry
    (let* ((current-idx (or (get-text-property (point) 'futon3c-code-block-index) 0))
           (sorted (sort (mapcar (lambda (e) (plist-get e :index))
                                 futon3c-code-blocks--registry)
                         #'<))
           (next (cl-find-if (lambda (i) (> i current-idx)) sorted)))
      (futon3c-code-blocks-visit (or next (car sorted))))))

(defun futon3c-code-blocks-prev ()
  "Navigate to the previous code block panel."
  (interactive)
  (when futon3c-code-blocks--registry
    (let* ((current-idx (or (get-text-property (point) 'futon3c-code-block-index)
                            (1+ futon3c-code-blocks--counter)))
           (sorted (sort (mapcar (lambda (e) (plist-get e :index))
                                 futon3c-code-blocks--registry)
                         #'>))
           (prev (cl-find-if (lambda (i) (< i current-idx)) sorted)))
      (futon3c-code-blocks-visit (or prev (car sorted))))))

(defun futon3c-code-blocks-toggle ()
  "Toggle visibility of all code block panels."
  (interactive)
  (if futon3c-code-blocks--visible
      (progn
        (dolist (entry futon3c-code-blocks--registry)
          (when-let ((buf (plist-get entry :buffer))
                     (win (get-buffer-window buf)))
            (delete-window win)))
        (setq futon3c-code-blocks--visible nil)
        (message "Code panels hidden"))
    (dolist (entry futon3c-code-blocks--registry)
      (when (buffer-live-p (plist-get entry :buffer))
        (futon3c-code-blocks--display-panel
         (plist-get entry :buffer)
         (plist-get entry :index))))
    (setq futon3c-code-blocks--visible t)
    (message "Code panels shown")))

(defun futon3c-code-blocks-clear ()
  "Close all panels and clear the registry."
  (interactive)
  (dolist (entry futon3c-code-blocks--registry)
    (when-let ((buf (plist-get entry :buffer)))
      (when (get-buffer-window buf)
        (delete-window (get-buffer-window buf)))
      (kill-buffer buf)))
  (setq futon3c-code-blocks--registry nil)
  (setq futon3c-code-blocks--counter 0)
  (message "Code blocks cleared"))

;;; Minor mode

(defvar futon3c-code-blocks-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n") #'futon3c-code-blocks-next)
    (define-key map (kbd "M-p") #'futon3c-code-blocks-prev)
    (define-key map (kbd "C-c b") #'futon3c-code-blocks-toggle)
    (define-key map (kbd "C-c B") #'futon3c-code-blocks-clear)
    map)
  "Keymap for `futon3c-code-blocks-mode'.")

;;;###autoload
(define-minor-mode futon3c-code-blocks-mode
  "Minor mode to extract code blocks to side-window panels.
When enabled, markdown code fences in agent messages are replaced
with clickable references and displayed in fontified side panels."
  :lighter " Blocks"
  :keymap futon3c-code-blocks-mode-map
  (if futon3c-code-blocks-mode
      (add-hook 'futon3c-ui--insert-message-hook
                #'futon3c-code-blocks--intercept nil t)
    (remove-hook 'futon3c-ui--insert-message-hook
                 #'futon3c-code-blocks--intercept t)
    ;; Clean up panels when mode is disabled
    (futon3c-code-blocks-clear)))

(provide 'futon3c-code-blocks)
;;; futon3c-code-blocks.el ends here
