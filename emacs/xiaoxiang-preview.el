;;; xiaoxiang-preview.el --- 小象 fast mode: mark a draft turn before sending  -*- lexical-binding: t; -*-

;; C-c x p   mark the draft in the input area (or the region) with 小象's reading
;; C-c x c   correct the fragment at point; the correction is trained on next rebuild
;; C-c x k   clear the marks
;;
;; Marks are the reply-proforma marks from CLAUDE.md.  A fragment gets a mark
;; only when 小象's prediction for that intent is right at least half the time
;; in cross-validation; otherwise it shows "?" and the two likeliest intents
;; are in the tooltip.  Backend: scripts/xiaoxiang_preview.py (about 0.1 s).

(require 'json)
(require 'subr-x)
(require 'session-mode)

(defvar xiaoxiang-preview-script
  (expand-file-name "../scripts/xiaoxiang_preview.py"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to xiaoxiang_preview.py.")

(defconst xiaoxiang-preview-marks
  '(("approve" . "㊣") ("disagree" . "🈚") ("clarify" . "🈯") ("report" . "㊢")
    ("report-problem" . "㊩") ("verify" . "㊬") ("retract" . "🈹") ("withdraw" . "🈡")
    ("propose" . "㊭") ("qualify" . "㊟") ("explain" . "🈖") ("constrain" . "🈲")
    ("ask-action" . "🈸") ("delegate" . "㊯") ("prioritize" . "㊝") ("collect" . "㊮")
    ("extend" . "🈕") ("continue" . "🈰") ("defer" . "🈝") ("redirect" . "🈘")
    ("explore" . "㊫"))
  "Intent to mark, as in the CLAUDE.md reply proforma key.")

(defgroup xiaoxiang-preview nil "小象 fast mode." :group 'tools)

(defface xiaoxiang-preview-sure '((t :foreground "#b8431f" :weight bold))
  "Mark for a fragment 小象 labels outright." :group 'xiaoxiang-preview)
(defface xiaoxiang-preview-unsure '((t :foreground "grey55"))
  "Mark for a fragment 小象 is not sure about." :group 'xiaoxiang-preview)

(defun xiaoxiang-preview--bounds ()
  "The draft to read: the active region, else the REPL input area."
  (cond ((use-region-p) (cons (region-beginning) (region-end)))
        ((and (boundp 'agent-chat--input-start) (markerp agent-chat--input-start))
         (cons (marker-position agent-chat--input-start) (point-max)))
        (t (user-error "No region and no REPL input area here"))))

(defun xiaoxiang-preview-clear ()
  "Remove 小象's marks from this buffer."
  (interactive)
  (remove-overlays (point-min) (point-max) 'xiaoxiang t))

(defun xiaoxiang-preview--label (ov)
  (let* ((intent (overlay-get ov 'xiaoxiang-intent))
         (mark (if intent (or (cdr (assoc intent xiaoxiang-preview-marks)) "·") "?")))
    (overlay-put ov 'before-string
                 (propertize mark 'face (if intent 'xiaoxiang-preview-sure
                                          'xiaoxiang-preview-unsure)))
    (overlay-put ov 'help-echo
                 (format "小象: %s (likeliest: %s)  C-c x c to correct"
                         (or intent "not sure")
                         (string-join (overlay-get ov 'xiaoxiang-guesses) ", ")))))

(defun xiaoxiang-preview ()
  "Mark the draft turn with 小象's reading, one mark per fragment."
  (interactive)
  (xiaoxiang-preview-clear)
  (pcase-let* ((`(,beg . ,end) (xiaoxiang-preview--bounds))
               (text (buffer-substring-no-properties beg end)))
    (when (string-blank-p text) (user-error "Nothing to read"))
    (let* ((out (with-temp-buffer
                  (insert text)
                  (unless (zerop (call-process-region (point-min) (point-max) "python3" t t nil
                                                      xiaoxiang-preview-script "preview" "-"))
                    (error "小象 preview failed: %s" (buffer-string)))
                  (goto-char (point-min))
                  (json-parse-buffer :object-type 'alist :array-type 'list :null-object nil)))
           (draft text))
      (dolist (frag out)
        ;; Offsets are code points into the draft, as Emacs positions are.
        (let ((ov (make-overlay (+ beg (alist-get 'start frag)) (+ beg (alist-get 'end frag)))))
          (overlay-put ov 'xiaoxiang t)
          (overlay-put ov 'evaporate t)
          (overlay-put ov 'modification-hooks (list (lambda (o &rest _) (delete-overlay o))))
          (overlay-put ov 'xiaoxiang-intent (alist-get 'intent frag))
          (overlay-put ov 'xiaoxiang-predicted (car (alist-get 'guesses frag)))
          (overlay-put ov 'xiaoxiang-guesses (alist-get 'guesses frag))
          (overlay-put ov 'xiaoxiang-draft draft)
          (xiaoxiang-preview--label ov)))
      (message "小象: %d fragments, %d marked; C-c x c on one to correct it"
               (length out) (seq-count (lambda (f) (alist-get 'intent f)) out)))))

(defun xiaoxiang-preview-correct ()
  "Give the fragment at point its right intent, and record the correction."
  (interactive)
  (let ((ov (seq-find (lambda (o) (overlay-get o 'xiaoxiang)) (overlays-at (point)))))
    (unless ov (user-error "No 小象 fragment at point"))
    (let ((intent (completing-read
                   (format "Intent for \"%s\": "
                           (truncate-string-to-width
                            (buffer-substring-no-properties (overlay-start ov) (overlay-end ov)) 40 nil nil "…"))
                   (mapcar #'car xiaoxiang-preview-marks) nil t)))
      (unless (zerop (call-process "python3" nil nil nil xiaoxiang-preview-script "correct"
                                   "--text" (overlay-get ov 'xiaoxiang-draft)
                                   "--fragment" (buffer-substring-no-properties
                                                 (overlay-start ov) (overlay-end ov))
                                   "--predicted" (or (overlay-get ov 'xiaoxiang-predicted) "")
                                   "--intent" intent))
        (error "小象 could not record the correction"))
      (overlay-put ov 'xiaoxiang-intent intent)
      (xiaoxiang-preview--label ov)
      (message "Recorded: %s" intent))))

;;; Typing the marks: C-c ; opens a menu of the reply-proforma marks.
;; Joe (2026-10-01): answering an agent's 🈸 with "🈸:yes" makes the pairing of
;; proposal and answer easy to code, so the marks should be quick to type.

(defconst xiaoxiang-mark-keys
  '(("!" "report-problem" "㊩" perceive)
    ("e" "explain" "🈖" perceive)
    ("r" "report" "㊢" perceive)
    ("c" "clarify" "🈯" believe)
    ("f" "qualify" "㊟" believe)
    ("a" "approve" "㊣" believe)
    ("d" "disagree" "🈚" believe)
    ("o" "collect" "㊮" believe)
    ("t" "retract" "🈹" believe)
    ("n" "constrain" "🈲" evaluate)
    ("x" "extend" "🈕" evaluate)
    ("h" "explore" "㊫" evaluate)
    ("p" "propose" "㊭" select)
    ("i" "prioritize" "㊝" select)
    ("j" "redirect" "🈘" select)
    ("z" "defer" "🈝" select)
    ("l" "delegate" "㊯" select)
    ("w" "withdraw" "🈡" select)
    ("s" "ask-action" "🈸" act)
    ("u" "continue" "🈰" act)
    ("v" "verify" "㊬" act)
    ("g" "gist" "㊥" annotator)
    ("." "unresolved" "🈳" annotator))
  "Key, intent, mark and PBASE stage for `xiaoxiang-insert-mark'.")

(defconst xiaoxiang-mark-stage-order
  '(perceive believe evaluate select act annotator)
  "PBASE display order, followed by the two non-PBASE annotation marks.")

(defun xiaoxiang--stage-face (stage)
  "Return the transcript mark face for STAGE."
  (intern (format "session-mode-mark-%s-face" stage)))

(defun xiaoxiang--mark-hydra-hint ()
  "Build the stage-coloured mark menu with PBASE stages as columns."
  (let* ((width 23)
         (columns
          (mapcar
           (lambda (stage)
             (let ((face (xiaoxiang--stage-face stage)))
               (cons
                (propertize
                 (format (format "%%-%ds" width)
                         (if (eq stage 'annotator)
                             "OTHER" (upcase (symbol-name stage))))
                 'face face)
                (mapcar
                 (lambda (k)
                   (propertize
                    (format (format "%%-%ds" width)
                            (format "_%s_ %s %s" (nth 0 k) (nth 2 k) (nth 1 k)))
                    'face face))
                 (seq-filter (lambda (k) (eq stage (nth 3 k)))
                             xiaoxiang-mark-keys)))))
           xiaoxiang-mark-stage-order))
         (height (apply #'max (mapcar #'length columns))))
    (concat
     "\nMarks by PBASE stage. Type a colon after a mark to answer it (🈸:yes).\n\n"
     (mapconcat
      (lambda (row)
        (mapconcat (lambda (column) (or (nth row column) (make-string width ?\s)))
                   columns ""))
      (number-sequence 0 (1- height)) "\n")
     "\n\n_q_ quit\n")))

(declare-function xiaoxiang-mark-hydra/body "xiaoxiang-preview")

(defun xiaoxiang--insert (mark)
  (insert mark))

(defun xiaoxiang-insert-mark ()
  "Insert a reply-proforma mark at point, chosen from a menu of keys and intents.
Type a colon after it to answer the agent's paragraph with that mark: 🈸:yes."
  (interactive)
  (if (or (fboundp 'defhydra) (require 'hydra nil t))
      (progn
        (eval
         `(defhydra xiaoxiang-mark-hydra (:hint nil :color blue)
            ;; Hydra needs the hint to start with a newline, or it errors on display.
            ,(xiaoxiang--mark-hydra-hint)
            ,@(mapcar (lambda (k) (list (nth 0 k) `(xiaoxiang--insert ,(nth 2 k)))) xiaoxiang-mark-keys)
            ("q" nil)))
        (xiaoxiang-mark-hydra/body))
    (let* ((choices (mapcar (lambda (k) (cons (format "%s %s" (nth 2 k) (nth 1 k)) (nth 2 k)))
                            xiaoxiang-mark-keys))
           (pick (completing-read "Mark: " choices nil t)))
      (insert (cdr (assoc pick choices))))))

(dolist (feature-map '((claude-repl . claude-repl-mode-map) (codex-repl . codex-repl-mode-map)
                       (kimi-repl . kimi-repl-mode-map) (zai-repl . zai-repl-mode-map)))
  (with-eval-after-load (car feature-map)
    (when (boundp (cdr feature-map))
      (define-key (symbol-value (cdr feature-map)) (kbd "C-c ;") #'xiaoxiang-insert-mark))))

;; Some REPL buffers run on their own copy of the mode map (*claude-repl:claude-17*
;; did on 2026-10-01: its map lacked even C-a), so binding the mode map misses them.
(dolist (buf (buffer-list))
  (with-current-buffer buf
    (when (and (memq major-mode '(claude-repl-mode codex-repl-mode kimi-repl-mode zai-repl-mode))
               (current-local-map)
               (not (eq (lookup-key (current-local-map) (kbd "C-c ;")) #'xiaoxiang-insert-mark)))
      (define-key (current-local-map) (kbd "C-c ;") #'xiaoxiang-insert-mark)
      (define-key (current-local-map) (kbd "C-c x p") #'xiaoxiang-preview)
      (define-key (current-local-map) (kbd "C-c x c") #'xiaoxiang-preview-correct)
      (define-key (current-local-map) (kbd "C-c x k") #'xiaoxiang-preview-clear))))

(with-eval-after-load 'claude-repl
  (when (boundp 'claude-repl-mode-map)
    (define-key claude-repl-mode-map (kbd "C-c x p") #'xiaoxiang-preview)
    (define-key claude-repl-mode-map (kbd "C-c x c") #'xiaoxiang-preview-correct)
    (define-key claude-repl-mode-map (kbd "C-c x k") #'xiaoxiang-preview-clear)))

(provide 'xiaoxiang-preview)
;;; xiaoxiang-preview.el ends here
