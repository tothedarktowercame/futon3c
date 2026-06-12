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

(defun paper-anatomy--face (layer kind)
  (pcase layer
    ("sub" 'paper-anatomy-sub)
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
