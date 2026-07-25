;;; standoff-overlay.el --- render standoff learning annotations as overlays -*- lexical-binding: t; -*-

;; Renders a standoff annotation file (JSON, converted from the miner's EDN)
;; as overlays in the annotated buffer. Sibling of session-mode.el: same
;; tier convention — :recognized kinds get solid colour cues, :cued kinds get
;; faint wavy underlines (they are low-precision candidates).
;;
;; Usage:
;;   (standoff-overlay-apply "/tmp/zai1-standoff.json" "*zai-repl:zai-1*")
;;   C-c C-. / C-c C-,  — jump to next / previous annotation
;;   (standoff-overlay-clear "*zai-repl:zai-1*")  — remove them

(require 'json)
(require 'cl-lib)

(defface standoff-error-face
  '((t :background "#4a1f1f" :underline (:color "tomato" :style line)))
  "Learning incidence: error signal.")
(defface standoff-fix-face
  '((t :background "#1f3a24" :underline (:color "SpringGreen3" :style line)))
  "Learning incidence: fix signal.")
(defface standoff-sorry-face
  '((t :background "#3a301f" :underline (:color "orange" :style line)))
  "Learning incidence: sorry event.")
(defface standoff-tactic-face
  '((t :underline (:color "SteelBlue1" :style line)))
  "Learning incidence: tactic mention.")
(defface standoff-cued-face
  '((t :underline (:color "gray55" :style wave)))
  "Cued (low-precision) learning candidate.")

(defvar standoff-overlay--kind-faces
  '(("error-signal" . standoff-error-face)
    ("fix-signal" . standoff-fix-face)
    ("sorry-event" . standoff-sorry-face)
    ("tactic-mention" . standoff-tactic-face)
    ("correction-cue" . standoff-cued-face)
    ("reach-cue" . standoff-cued-face)))

(defvar-local standoff-overlay--overlays nil)

(defun standoff-overlay-clear (buffer-name)
  "Remove all standoff overlays from BUFFER-NAME."
  (interactive "bBuffer: ")
  (with-current-buffer buffer-name
    (mapc #'delete-overlay standoff-overlay--overlays)
    (setq standoff-overlay--overlays nil)
    (message "standoff: cleared")))

(defun standoff-overlay-apply (json-file buffer-name)
  "Render annotations from JSON-FILE as overlays in BUFFER-NAME."
  (interactive "fStandoff JSON: \nbBuffer: ")
  (let* ((json-object-type 'alist)
         (json-array-type 'list)
         (data (json-read-file json-file))
         (anns (alist-get 'annotations data))
         (n 0))
    (with-current-buffer buffer-name
      (mapc #'delete-overlay standoff-overlay--overlays)
      (setq standoff-overlay--overlays nil)
      (dolist (a anns)
        (let* ((range (alist-get 'range a))
               (beg (min (car range) (point-max)))
               (end (min (cadr range) (point-max)))
               (kind (alist-get 'type a))
               (face (or (cdr (assoc kind standoff-overlay--kind-faces))
                         'standoff-cued-face)))
          (when (< beg end)
            (let ((ov (make-overlay beg end)))
              (overlay-put ov 'face face)
              (overlay-put ov 'standoff-kind kind)
              (overlay-put ov 'help-echo
                           (format "[%s/%s] %s" (alist-get 'tier a) kind
                                   (or (alist-get 'excerpt a) "")))
              (push ov standoff-overlay--overlays)
              (cl-incf n)))))
      (local-set-key (kbd "C-c C-.") #'standoff-overlay-next)
      (local-set-key (kbd "C-c C-,") #'standoff-overlay-prev)
      (message "standoff: %d annotations rendered (C-c C-. next, C-c C-, prev, hover for kind)" n))))

(defun standoff-overlay--positions ()
  (sort (mapcar #'overlay-start standoff-overlay--overlays) #'<))

(defun standoff-overlay-next ()
  "Jump to the next standoff annotation."
  (interactive)
  (if-let* ((pos (cl-find-if (lambda (p) (> p (point))) (standoff-overlay--positions))))
      (progn (goto-char pos)
             (let ((ov (cl-find-if (lambda (o) (= (overlay-start o) pos))
                                   standoff-overlay--overlays)))
               (message "standoff: %s" (overlay-get ov 'help-echo))))
    (message "standoff: no further annotations")))

(defun standoff-overlay-prev ()
  "Jump to the previous standoff annotation."
  (interactive)
  (if-let* ((pos (cl-find-if (lambda (p) (< p (point)))
                             (reverse (standoff-overlay--positions)))))
      (progn (goto-char pos)
             (let ((ov (cl-find-if (lambda (o) (= (overlay-start o) pos))
                                   standoff-overlay--overlays)))
               (message "standoff: %s" (overlay-get ov 'help-echo))))
    (message "standoff: no earlier annotations")))

(provide 'standoff-overlay)
;;; standoff-overlay.el ends here
