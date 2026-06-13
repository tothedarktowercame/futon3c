;;; paper-anatomy.el --- Mission-mode-style anatomy over papers, with proofreading -*- lexical-binding: t; -*-

;; The golden walk moves into the cockpit (Joe, 2026-06-12): render a paper's
;; scope anatomy as overlays in an Emacs buffer — the same idiom as
;; mission-mode — and extend the futon-look channel with TYPE-BASED
;; proofreading: the operator tags the mark at point (incomplete / overrun /
;; unanchored / invented / missed) and each tag lands in the agent's inbox
;; as golden data, exactly like a futon-look.
;;
;; Data: fable-<paper>-emacs.json from futon6 build_fable_golden
;;   {"paper", "text", "marks": [{"start","end","layer","kind","tip"}...]}
;;
;; Keys (read-only buffer; letters allowed per the viewer convention —
;; the no-letter-keys rule governs EDITABLE buffers):
;;   n/p   next/previous mark        TAB   next mark of same kind
;;   i     tag INCOMPLETE            o     tag OVERRUN (span too wide)
;;   u     tag UNANCHORED            x     tag INVENTED (should not exist)
;;   m     tag MISSED (something HERE should be marked — uses region if active)
;;   c     tag CUSTOM (prompts for a note)
;;   l     futon-look at point (the ordinary summons)
;;   q     quit window

;;; Code:

(require 'json)

(defgroup paper-anatomy nil
  "Scope anatomy over papers with operator proofreading."
  :group 'applications)

(defcustom paper-anatomy-dir
  "/home/joe/code/futon6/data/showcases/ct-anatomy/golden/"
  "Directory holding fable-<paper>-emacs.json files."
  :type 'directory :group 'paper-anatomy)

(defcustom paper-anatomy-inbox "/tmp/futon-voice-inbox.jsonl"
  "Where proofreading tags land — the futon-look channel."
  :type 'file :group 'paper-anatomy)

(defface paper-anatomy-bind '((t :background "#dde7fb" :underline "#2a4d9a"))
  "bind/* scopes.")
(defface paper-anatomy-constrain '((t :background "#efe3f7" :underline "#7a3ba8"))
  "constrain/* scopes.")
(defface paper-anatomy-quant '((t :background "#e3eef7" :underline "#1a6a9a"))
  "quant/* + assume/* scopes.")
(defface paper-anatomy-mexpr '((t :background "#eef0f7"))
  "math expression envelopes.")
(defface paper-anatomy-sub '((t :foreground "#7851a9" :weight bold))
  "math subterms.")
(defface paper-anatomy-defined '((t :background "#d3f3df" :underline "#0f766e"))
  "golden: defined-in-paper.")
(defface paper-anatomy-hole
  '((t :background "#fdf3d7" :underline (:color "#9a7b1a" :style wave)))
  "golden: needs canon link.")
(defface paper-anatomy-envtex '((t :background "#e4ecf4" :underline "#1d3a4d"))
  "golden: real TeX environment head.")

;; DP-run classification overlay (M-distributed-proofreaders): each control
;; sequence in a math span coloured by its current classification.
(defface paper-anatomy-dp-classified '((t :background "#d3f3df"))
  "DP: recognised + role-typed (green).")
(defface paper-anatomy-dp-role-gap
  '((t :background "#fdf3d7" :underline (:color "#9a7b1a" :style wave)))
  "DP: recognised but role-gap (amber).")
(defface paper-anatomy-dp-unknown '((t :background "#fbdcdc" :underline "#b03030"))
  "DP: genuine unknown (red).")
(defface paper-anatomy-dp-concept-typed '((t :background "#bfe8e0" :underline "#0d7a6e"))
  "DP: role-gap resolved to a concept via the authority (teal).")
(defface paper-anatomy-dp-let-binder '((t :background "#dde7fb" :underline "#2a4d9a" :weight bold))
  "DP: a Let-binder scope (blue, echoing mission-mode bind).")

(defun paper-anatomy--face (layer kind)
  (pcase layer
    ("sub" 'paper-anatomy-sub)
    ("dp" (pcase kind
            ("classified" 'paper-anatomy-dp-classified)
            ("role-gap" 'paper-anatomy-dp-role-gap)
            ("unknown" 'paper-anatomy-dp-unknown)
            ("concept-typed" 'paper-anatomy-dp-concept-typed)
            ("let-binder" 'paper-anatomy-dp-let-binder)
            (_ 'paper-anatomy-dp-classified)))
    ("golden" (pcase kind
                ("defined" 'paper-anatomy-defined)
                ("hole" 'paper-anatomy-hole)
                ("envtex" 'paper-anatomy-envtex)
                (_ 'paper-anatomy-defined)))
    (_ (pcase (car (split-string kind "/"))
         ("bind" 'paper-anatomy-bind)
         ("constrain" 'paper-anatomy-constrain)
         ((or "quant" "assume") 'paper-anatomy-quant)
         ("mexpr" 'paper-anatomy-mexpr)
         (_ 'paper-anatomy-mexpr)))))

(defvar-local paper-anatomy--paper nil)

;;;###autoload
(defun paper-anatomy-open (paper)
  "Open PAPER's anatomy (e.g. \"0809.2517\") with proofreading keys."
  (interactive
   (list (completing-read "Paper: "
                          (mapcar (lambda (f)
                                    (string-remove-suffix
                                     "-emacs.json"
                                     (string-remove-prefix "fable-" f)))
                                  (directory-files paper-anatomy-dir nil
                                                   "-emacs\\.json$")))))
  (let* ((file (expand-file-name (format "fable-%s-emacs.json" paper)
                                 paper-anatomy-dir))
         (json-object-type 'alist)
         (data (json-read-file file))
         (text (alist-get 'text data))
         (marks (alist-get 'marks data))
         (buf (get-buffer-create (format "*Paper Anatomy: %s*" paper))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text)
        (goto-char (point-min))
        (dotimes (i (length marks))
          (let* ((m (aref marks i))
                 (start (1+ (alist-get 'start m)))
                 (end (min (point-max) (1+ (alist-get 'end m))))
                 (layer (alist-get 'layer m))
                 (kind (alist-get 'kind m)))
            (when (< start end)
              (let ((ov (make-overlay start end)))
                (overlay-put ov 'face (paper-anatomy--face layer kind))
                (overlay-put ov 'paper-anatomy (list :layer layer :kind kind
                                                     :tip (alist-get 'tip m)))
                (overlay-put ov 'help-echo
                             (format "[%s] %s" layer (alist-get 'tip m))))))))
      (paper-anatomy-mode)
      (setq paper-anatomy--paper paper))
    (pop-to-buffer buf)))

(defun paper-anatomy--mark-at-point ()
  (seq-find (lambda (ov) (overlay-get ov 'paper-anatomy))
            (overlays-at (point))))

(defun paper-anatomy--emit (verdict &optional note)
  "Append a proofreading tag for the mark (or region) at point to the inbox."
  (let* ((ov (paper-anatomy--mark-at-point))
         (meta (and ov (overlay-get ov 'paper-anatomy)))
         (span (cond (ov (buffer-substring-no-properties
                          (overlay-start ov) (overlay-end ov)))
                     ((use-region-p) (buffer-substring-no-properties
                                      (region-beginning) (region-end)))
                     (t (thing-at-point 'line t))))
         (payload `((at . ,(format-time-string "%Y-%m-%dT%H:%M:%S.%3NZ" nil t))
                    (kind . "proofread")
                    (paper . ,paper-anatomy--paper)
                    (verdict . ,verdict)
                    (scope-layer . ,(plist-get meta :layer))
                    (scope-kind . ,(plist-get meta :kind))
                    (scope-tip . ,(plist-get meta :tip))
                    (position . ,(1- (or (and ov (overlay-start ov)) (point))))
                    (span . ,(string-trim (or span "")))
                    (note . ,(or note "")))))
    (with-temp-buffer
      (insert (json-encode payload) "\n")
      (append-to-file (point-min) (point-max) paper-anatomy-inbox))
    (message "proofread: %s %s [%s]" verdict
             (plist-get meta :kind) (truncate-string-to-width (or span "") 40))))

(defun paper-anatomy-tag-incomplete () (interactive) (paper-anatomy--emit "incomplete"))
(defun paper-anatomy-tag-overrun () (interactive) (paper-anatomy--emit "overrun"))
(defun paper-anatomy-tag-unanchored () (interactive) (paper-anatomy--emit "unanchored"))
(defun paper-anatomy-tag-invented () (interactive) (paper-anatomy--emit "invented"))
(defun paper-anatomy-tag-missed () (interactive) (paper-anatomy--emit "missed"))
(defun paper-anatomy-tag-custom (note)
  (interactive "sNote for the agent: ")
  (paper-anatomy--emit "custom" note))

(defun paper-anatomy-next-mark ()
  (interactive)
  (let ((pos (next-single-char-property-change (point) 'paper-anatomy)))
    (when (< pos (point-max)) (goto-char pos))))
(defun paper-anatomy-prev-mark ()
  (interactive)
  (let ((pos (previous-single-char-property-change (point) 'paper-anatomy)))
    (when (> pos (point-min)) (goto-char pos))))
(defun paper-anatomy-next-same-kind ()
  (interactive)
  (let* ((ov (paper-anatomy--mark-at-point))
         (kind (and ov (plist-get (overlay-get ov 'paper-anatomy) :kind))))
    (when kind
      (let ((pos (point)) found)
        (while (and (not found)
                    (< (setq pos (next-single-char-property-change pos 'paper-anatomy))
                       (point-max)))
          (let ((o (seq-find (lambda (o) (overlay-get o 'paper-anatomy))
                             (overlays-at pos))))
            (when (and o (equal kind (plist-get (overlay-get o 'paper-anatomy) :kind)))
              (setq found pos))))
        (when found (goto-char found))))))

(defvar paper-anatomy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'paper-anatomy-next-mark)
    (define-key map (kbd "p") #'paper-anatomy-prev-mark)
    (define-key map (kbd "TAB") #'paper-anatomy-next-same-kind)
    (define-key map (kbd "i") #'paper-anatomy-tag-incomplete)
    (define-key map (kbd "o") #'paper-anatomy-tag-overrun)
    (define-key map (kbd "u") #'paper-anatomy-tag-unanchored)
    (define-key map (kbd "x") #'paper-anatomy-tag-invented)
    (define-key map (kbd "m") #'paper-anatomy-tag-missed)
    (define-key map (kbd "c") #'paper-anatomy-tag-custom)
    (define-key map (kbd "l") #'futon-look)
    (define-key map (kbd "q") #'quit-window)
    map))

(define-derived-mode paper-anatomy-mode special-mode "PaperAnatomy"
  "Read-only paper anatomy with proofreading tags.
\\{paper-anatomy-mode-map}"
  (setq truncate-lines nil)
  (visual-line-mode 1))

(provide 'paper-anatomy)
;;; paper-anatomy.el ends here

;;;; The block panel — mission-mode's Scratch-style side lane (Joe, spoken
;;;; 2026-06-12: "the same type of scratch-based markup along the side as
;;;; Mission Mode does... maybe in a different buffer").

(defface paper-anatomy-block-bind
  '((t :background "#2a4d9a" :foreground "white" :weight bold))
  "Block chip: bind family.")
(defface paper-anatomy-block-constrain
  '((t :background "#7a3ba8" :foreground "white" :weight bold))
  "Block chip: constrain family.")
(defface paper-anatomy-block-math
  '((t :background "#46506b" :foreground "white" :weight bold))
  "Block chip: math family.")
(defface paper-anatomy-block-golden
  '((t :background "#0f766e" :foreground "white" :weight bold))
  "Block chip: golden layer.")
(defface paper-anatomy-block-hole
  '((t :background "#9a7b1a" :foreground "white" :weight bold))
  "Block chip: hole.")

(defun paper-anatomy--block-face (meta)
  (let ((kind (plist-get meta :kind))
        (layer (plist-get meta :layer)))
    (cond ((equal kind "hole") 'paper-anatomy-block-hole)
          ((equal layer "golden") 'paper-anatomy-block-golden)
          ((string-prefix-p "bind" kind) 'paper-anatomy-block-bind)
          ((string-prefix-p "constrain" kind) 'paper-anatomy-block-constrain)
          (t 'paper-anatomy-block-math))))

;;;; Capability dashboard — the Paper Blocks panel as MVC. The header shows
;;;; which capabilities are demonstrated on THIS paper (✓), which can be
;;;; toggled in (○ → RET reprocesses the paper into a gold for it), and which
;;;; are not yet built (·).

(defcustom paper-anatomy-futon6 "/home/joe/code/futon6"
  "futon6 root, where the capability generators live."
  :type 'directory :group 'paper-anatomy)

(defvar paper-anatomy-capabilities
  '((dp-classification
     :label "math-lexeme classification"
     :detect (:layer "dp" :kinds ("classified" "role-gap" "unknown" "concept-typed"))
     :status :base)
    (concept-typing
     :label "role-gap → concept (NNexus/nLab)"
     :detect (:layer "dp" :kinds ("concept-typed"))
     :gen ("python3" "scripts/dp_paper_view.py" :base "--with-concept-authority")
     :status :togglable)
    (let-binders
     :label "Let-binder scopes (Let $X$ be a …)"
     :detect (:layer "dp" :kinds ("let-binder"))
     :gen ("python3" "scripts/dp_paper_view.py" :base "--with-binders" "--with-concept-authority")
     :status :togglable)
    (golden-scopes :label "scope anatomy (bind/constrain/mexpr)"
                   :detect (:layer "base") :status :other-view)
    (definienda :label "defined-in-paper"
                :detect (:layer "golden" :kinds ("defined")) :status :other-view)
    (canon-holes :label "canon holes"
                 :detect (:layer "golden" :kinds ("hole")) :status :other-view)
    (env-tex :label "TeX environments"
             :detect (:layer "golden" :kinds ("envtex")) :status :other-view)
    (latexml-deep :label "latexml deep parse" :status :not-yet)
    (authored-layer :label "label/ref/cite harvest" :status :not-yet))
  "Capabilities the paper-anatomy surface can demonstrate. Each entry:
:label, optional :detect {:layer L :kinds (...)} predicate over the loaded
marks, optional :gen generator (:base → the base paper id), and :status one
of :base :togglable :other-view :not-yet.")

(defun paper-anatomy--present-layer-kinds ()
  "Hash-set of \"layer\" and \"layer/kind\" strings present in this buffer."
  (let ((s (make-hash-table :test 'equal)))
    (dolist (o (overlays-in (point-min) (point-max)))
      (when-let ((m (overlay-get o 'paper-anatomy)))
        (puthash (plist-get m :layer) t s)
        (puthash (format "%s/%s" (plist-get m :layer) (plist-get m :kind)) t s)))
    s))

(defun paper-anatomy--capability-demonstrated-p (cap present)
  (let* ((d (plist-get (cdr cap) :detect))
         (layer (plist-get d :layer))
         (kinds (plist-get d :kinds)))
    (cond ((null d) nil)
          (kinds (seq-some (lambda (k) (gethash (format "%s/%s" layer k) present)) kinds))
          (t (gethash layer present)))))

(defun paper-anatomy--base-paper ()
  (replace-regexp-in-string "-dp\\'" "" (or paper-anatomy--paper "")))

(defun paper-anatomy-toggle-capability (cap-id)
  "Fold capability CAP-ID into this paper (the Controller): run its generator
and reload, turning the paper into a gold demonstrating that capability."
  (interactive
   (list (intern (completing-read
                  "Fold in capability: "
                  (mapcar (lambda (c) (symbol-name (car c)))
                          (seq-filter (lambda (c) (plist-get (cdr c) :gen))
                                      paper-anatomy-capabilities))))))
  (let* ((cap (assq cap-id paper-anatomy-capabilities))
         (gen (plist-get (cdr cap) :gen))
         (base (paper-anatomy--base-paper))
         (args (mapcar (lambda (a) (if (eq a :base) base a)) (cdr gen)))
         (default-directory paper-anatomy-futon6))
    (message "paper-anatomy: folding in %s …" cap-id)
    (let ((rc (apply #'call-process (car gen) nil "*paper-anatomy-gen*" nil args)))
      (if (zerop rc)
          (progn (paper-anatomy-open (format "%s-dp" base))
                 (message "paper-anatomy: %s folded in — reloaded" cap-id))
        (message "paper-anatomy: generator for %s failed (rc %s) — see *paper-anatomy-gen*"
                 cap-id rc)))))

(defun paper-anatomy--insert-capability-header (paper present)
  "Insert the capability dashboard for PAPER given PRESENT layer/kind set."
  (insert (propertize (format "CAPABILITIES · %s\n" paper) 'face 'bold))
  (dolist (cap paper-anatomy-capabilities)
    (let* ((id (car cap)) (pl (cdr cap))
           (demoed (paper-anatomy--capability-demonstrated-p cap present))
           (status (plist-get pl :status))
           (mark (cond (demoed "✓")
                       ((eq status :togglable) "○")
                       ((eq status :not-yet) "·")
                       (t "–")))
           (face (cond (demoed 'success)
                       ((eq status :togglable) 'warning)
                       (t 'shadow))))
      (insert (propertize (format " %s " mark) 'face face))
      (if (and (not demoed) (plist-get pl :gen))
          (insert-text-button (plist-get pl :label)
                              'action (let ((cid id))
                                        (lambda (_) (paper-anatomy-toggle-capability cid)))
                              'help-echo "RET: fold this capability in (reprocess)")
        (insert (propertize (plist-get pl :label) 'face (if demoed 'default 'shadow))))
      (insert (cond (demoed "")
                    ((eq status :togglable) "  [fold in]")
                    ((eq status :not-yet) "  (not yet)")
                    ((eq status :other-view) "  (other view)")
                    (t "")))
      (insert "\n")))
  (insert (propertize "─────────────\n" 'face 'shadow)))

(defvar-local paper-anatomy--panel-key nil)

(defun paper-anatomy--panel-render ()
  "Render the block panel for the scopes at point (outermost first)."
  (let* ((src (current-buffer))
         (here (point))
         (paper paper-anatomy--paper)
         (present (paper-anatomy--present-layer-kinds))
         (ovs (seq-filter (lambda (o) (overlay-get o 'paper-anatomy))
                          (overlays-at here)))
         (near (seq-filter (lambda (o) (overlay-get o 'paper-anatomy))
                           (overlays-in (line-beginning-position)
                                        (line-end-position))))
         (key (mapcar #'overlay-start (append ovs near))))
    (unless (equal key paper-anatomy--panel-key)
      (setq paper-anatomy--panel-key key)
      (let ((buf (get-buffer-create (format "*Paper Blocks: %s*"
                                            paper-anatomy--paper))))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (special-mode)
            (paper-anatomy--insert-capability-header paper present)
            (insert (propertize "AT POINT\n" 'face 'bold))
            (dolist (o (sort ovs (lambda (a b)
                                   (< (- (overlay-end b) (overlay-start b))
                                      (- (overlay-end a) (overlay-start a))))))
              (paper-anatomy--insert-block o src))
            (when (cl-set-difference near ovs)
              (insert (propertize "\nTHIS LINE\n" 'face 'bold))
              (dolist (o (cl-set-difference near ovs))
                (paper-anatomy--insert-block o src)))))
        (display-buffer buf '((display-buffer-in-side-window)
                              (side . right) (window-width . 46)
                              (inhibit-same-window . t)))))))

(defun paper-anatomy--insert-block (o src)
  (let* ((meta (overlay-get o 'paper-anatomy))
         (kind (plist-get meta :kind))
         (span (with-current-buffer src
                 (buffer-substring-no-properties
                  (overlay-start o)
                  (min (overlay-end o) (+ (overlay-start o) 120)))))
         (start (overlay-start o)))
    (insert (propertize (format " %s " kind)
                        'face (paper-anatomy--block-face meta))
            " ")
    (insert-text-button
     (truncate-string-to-width (replace-regexp-in-string "\n" "⏎" span) 38)
     'action (lambda (_) (let ((w (get-buffer-window src t)))
                           (when w (select-window w) (goto-char start))))
     'help-echo (plist-get meta :tip))
    (insert "\n")
    (let ((tip (plist-get meta :tip)))
      (when (and tip (not (equal tip kind)))
        (insert (propertize (format "   %s\n"
                                    (truncate-string-to-width tip 42))
                            'face 'shadow))))))

(defun paper-anatomy--panel-post-command ()
  (when (derived-mode-p 'paper-anatomy-mode)
    (ignore-errors (paper-anatomy--panel-render))))

(add-hook 'paper-anatomy-mode-hook
          (lambda ()
            (add-hook 'post-command-hook
                      #'paper-anatomy--panel-post-command nil t)))
