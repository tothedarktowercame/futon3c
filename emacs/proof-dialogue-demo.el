;;; proof-dialogue-demo.el --- Local trace demo for proof dialogues -*- lexical-binding: t; -*-

;; This viewer deliberately consumes the canonical local JSONL artifact.  It
;; has no Matrix, browser, or model dependency, so transport is not confused
;; with capture and presentation in infrastructure demonstrations.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defconst proof-dialogue-demo--root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))

(defcustom proof-dialogue-demo-trace
  (expand-file-name
   "holes/labs/M-diagramprover/proof-dialogues/t94a01-pilot/trace.jsonl"
   proof-dialogue-demo--root)
  "Canonical trace used by `proof-dialogue-demo'."
  :type 'file :group 'tools)

(defvar-local proof-dialogue-demo--trace-file nil)

(defvar proof-dialogue-demo-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'proof-dialogue-demo-revert)
    map))

(define-derived-mode proof-dialogue-demo-mode special-mode "Proof-Dialogue"
  "Major mode for a frozen canonical dialogue trace.")

(defun proof-dialogue-demo--records (file)
  "Read canonical JSONL records from FILE, preserving order."
  (with-temp-buffer
    (insert-file-contents file)
    (mapcar (lambda (line)
              (json-parse-string line :object-type 'alist :array-type 'list
                                 :null-object nil :false-object nil))
            (split-string (buffer-string) "\n" t))))

(defun proof-dialogue-demo--render (file)
  "Render canonical dialogue FILE in the current buffer."
  (let* ((records (proof-dialogue-demo--records file))
         (turns (cl-remove-if-not
                 (lambda (r) (equal "turn" (alist-get 'record_type r))) records))
         (deliveries (cl-remove-if-not
                      (lambda (r) (equal "delivery" (alist-get 'record_type r))) records))
         (delivered (mapcar (lambda (r) (alist-get 'move_id r)) deliveries))
         (head (alist-get 'record_sha256 (car (last records))))
         (inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "Frozen dialogue trace\n" 'face '(:height 1.3 :weight bold)))
    (insert (format "%s\n%d moves · %d receipts · head %s\n\n"
                    (file-relative-name file proof-dialogue-demo--root)
                    (length turns) (length deliveries)
                    (if head (substring head 0 (min 12 (length head))) "missing")))
    (dolist (turn turns)
      (let ((id (alist-get 'move_id turn))
            (parent (or (alist-get 'parent_move_id turn) "root")))
        (insert (propertize
                 (format "%s  %-8s %-11s  parent %-5s  %s\n"
                         (if (member id delivered) "✓" "! ")
                         id (alist-get 'role turn) parent (alist-get 'move_kind turn))
                 'face 'font-lock-keyword-face))
        (insert (string-trim (alist-get 'body turn)) "\n\n")))
    (goto-char (point-min))))

;;;###autoload
(defun proof-dialogue-demo (&optional file)
  "Open canonical dialogue trace FILE in an Emacs demonstration buffer."
  (interactive (list (when current-prefix-arg
                       (read-file-name "Trace JSONL: " nil nil t))))
  (let ((file (expand-file-name (or file proof-dialogue-demo-trace))))
    (unless (file-readable-p file)
      (user-error "Trace is not readable: %s" file))
    (with-current-buffer (get-buffer-create "*proof dialogue demo*")
      (proof-dialogue-demo-mode)
      (setq proof-dialogue-demo--trace-file file)
      (proof-dialogue-demo--render file)
      (pop-to-buffer (current-buffer)))))

(defun proof-dialogue-demo-revert ()
  "Reload the current local trace artifact."
  (interactive)
  (unless proof-dialogue-demo--trace-file
    (user-error "This buffer has no trace file"))
  (proof-dialogue-demo--render proof-dialogue-demo--trace-file))

(provide 'proof-dialogue-demo)
;;; proof-dialogue-demo.el ends here
