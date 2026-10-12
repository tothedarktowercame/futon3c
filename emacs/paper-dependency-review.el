;;; paper-dependency-review.el --- review claim dependency edges -*- lexical-binding: t; -*-

;; Review JSON is produced by futon6's paper_dependency_view.py.  Verdicts are
;; append-only observations: the last verdict for an edge wins, and `u' removes
;; all observations for that edge.  The source JSON is rewritten atomically.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'tabulated-list)

(defgroup paper-dependency-review nil
  "Review KnowTeX-style dependency edges."
  :group 'paper-anatomy)

(defcustom paper-dependency-review-dir
  "/home/joe/code/futon6/data/showcases/ct-anatomy/golden/"
  "Directory containing <paper>-dependency-review.json files."
  :type 'directory :group 'paper-dependency-review)

(defcustom paper-dependency-review-eprints
  "/home/joe/code/storage/futon6/data/arxiv-math-ct-eprints/"
  "Directory resolved by futon6's eprint configuration."
  :type 'directory :group 'paper-dependency-review)

(defcustom paper-dependency-review-cache
  (expand-file-name "~/.cache/futon6/eprints/")
  "Cache directory for extracted paper eprints."
  :type 'directory :group 'paper-dependency-review)

(defvar-local paper-dependency-review--paper nil)
(defvar-local paper-dependency-review--file nil)
(defvar-local paper-dependency-review--data nil)

(defun paper-dependency-review--directory ()
  "Return the anatomy directory when paper-anatomy.el has defined it."
  (if (boundp 'paper-anatomy-dir)
      paper-anatomy-dir
    paper-dependency-review-dir))

(defun paper-dependency-review--read (file)
  "Read FILE as an alist-backed review object."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'symbol)
        (json-null :json-null)
        (json-false :json-false))
    (json-read-file file)))

(defun paper-dependency-review--write (file data)
  "Atomically write DATA to FILE, preserving JSON object and array values."
  (cl-labels ((json-value (value key)
                (cond
                 ((vectorp value)
                  (vconcat (mapcar (lambda (item) (json-value item nil)) value)))
                 ((memq key '(nodes edges verdicts char_span))
                  (vconcat (mapcar (lambda (item) (json-value item nil)) value)))
                 ((and (listp value)
                       (or (null value) (consp (car value))))
                  (mapcar (lambda (pair)
                            (cons (car pair) (json-value (cdr pair) (car pair))))
                          value))
                 (t value))))
    (setq data (json-value data nil)))
  (let ((tmp (make-temp-file (file-name-nondirectory file) nil ".json")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (set-buffer-file-coding-system 'utf-8-unix)
            (insert (json-serialize data :null-object :json-null :false-object :json-false))
            (insert "\n"))
          (rename-file tmp file t))
      (when (file-exists-p tmp)
        (delete-file tmp)))))

(defun paper-dependency-review--verdicts (data)
  "Return the last verdict per edge id from DATA's append-only log."
  (let ((last (make-hash-table :test #'equal)))
    (dolist (verdict (alist-get 'verdicts data))
      (puthash (alist-get 'edge_id verdict) verdict last))
    last))

(defun paper-dependency-review--node-map (data)
  "Return a node-id hash table for DATA."
  (let ((nodes (make-hash-table :test #'equal)))
    (dolist (node (alist-get 'nodes data))
      (puthash (alist-get 'id node) node nodes))
    nodes))

(defun paper-dependency-review--node-label (node)
  "Format NODE's label and type for a table cell."
  (format "%s (%s)"
          (or (alist-get 'label node) (alist-get 'id node))
          (or (alist-get 'type node) "?")))

(defun paper-dependency-review--location (location)
  "Format LOCATION as file:line, or a deterministic offset."
  (cond
   ((listp location)
    (format "%s:%s" (or (alist-get 'file location) "?")
            (or (alist-get 'line location) "?")))
   ((numberp location) (format "offset %s" location))
   (t "?")))

(defun paper-dependency-review--edge-row (edge nodes verdicts)
  "Build one tabulated-list row for EDGE."
  (let* ((source (gethash (alist-get 'source edge) nodes))
         (target (gethash (alist-get 'target edge) nodes))
         (verdict (gethash (alist-get 'id edge) verdicts))
         (glyph (pcase (alist-get 'verdict verdict)
                   ("right" "✓")
                   ("wrong" "✗")
                   (_ "")))
         (edge-glyph (if (equal (alist-get 'style edge) "solid") "──" "╌╌")))
    (list (alist-get 'id edge)
          (vector glyph
                  (or (alist-get 'rule edge) "")
                  (if (eq (alist-get 'cycle edge) t)
                      (propertize edge-glyph 'face 'error)
                    edge-glyph)
                  (paper-dependency-review--node-label source)
                  (paper-dependency-review--node-label target)
                  (paper-dependency-review--location (alist-get 'location edge))))))

(defun paper-dependency-review--rows ()
  "Return sorted table rows for the current review data."
  (let ((nodes (paper-dependency-review--node-map paper-dependency-review--data))
        (verdicts (paper-dependency-review--verdicts paper-dependency-review--data)))
    (mapcar (lambda (edge)
              (paper-dependency-review--edge-row edge nodes verdicts))
            (sort (copy-sequence (alist-get 'edges paper-dependency-review--data))
                  (lambda (a b)
                    (let* ((at (alist-get 'target a))
                           (bt (alist-get 'target b))
                           (ar (or (alist-get 'rule a) ""))
                           (br (or (alist-get 'rule b) "")))
                      (or (string-lessp at bt)
                          (and (equal at bt) (string-lessp ar br)))))))))

(defun paper-dependency-review--header ()
  "Return the review header line for the current data."
  (let* ((counts (alist-get 'counts paper-dependency-review--data))
         (verdicts (hash-table-count
                    (paper-dependency-review--verdicts paper-dependency-review--data))))
    (format " %s · %s · cycles %s · verdicts %s"
            paper-dependency-review--paper
            (mapconcat (lambda (pair) (format "%s %s" (car pair) (cdr pair)))
                       counts ", ")
            (or (alist-get 'cycle_scc_count paper-dependency-review--data) 0)
            verdicts)))

(defun paper-dependency-review--refresh (&optional id)
  "Refresh rows from current DATA and return to ID when possible."
  (setq tabulated-list-entries (paper-dependency-review--rows)
        header-line-format (paper-dependency-review--header))
  (tabulated-list-print t)
  (when id
    (goto-char (point-min))
    (let ((found nil))
      (while (and (not found) (< (point) (point-max)))
        (when (equal id (ignore-errors (tabulated-list-get-id)))
          (setq found t))
        (unless found
          (forward-line 1))))))

(defun paper-dependency-review--current-edge ()
  "Return the edge represented by the current table row."
  (let ((id (tabulated-list-get-id)))
    (or (cl-find id (alist-get 'edges paper-dependency-review--data)
                 :key (lambda (edge) (alist-get 'id edge)) :test #'equal)
        (user-error "No dependency edge on this row"))))

(defun paper-dependency-review--save-verdict (verdict)
  "Append VERDICT for the current edge, then refresh the table."
  (let* ((edge (paper-dependency-review--current-edge))
         (id (alist-get 'id edge))
         (entries (copy-sequence (alist-get 'verdicts paper-dependency-review--data)))
         (record `((edge_id . ,id)
                   (verdict . ,verdict)
                   (at . ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)))))
    (push record entries)
    (setf (alist-get 'verdicts paper-dependency-review--data) (nreverse entries))
    (paper-dependency-review--write paper-dependency-review--file paper-dependency-review--data)
    (paper-dependency-review--refresh id)
    (message "%s: %s" id verdict)))

(defun paper-dependency-review--clear-verdict ()
  "Remove every verdict for the current edge and refresh the table."
  (let* ((edge (paper-dependency-review--current-edge))
         (id (alist-get 'id edge)))
    (setf (alist-get 'verdicts paper-dependency-review--data)
          (cl-remove-if (lambda (verdict) (equal (alist-get 'edge_id verdict) id))
                        (alist-get 'verdicts paper-dependency-review--data)))
    (paper-dependency-review--write paper-dependency-review--file paper-dependency-review--data)
    (paper-dependency-review--refresh id)
    (message "%s: cleared" id)))

(defun paper-dependency-review-mark-right ()
  "Mark the current edge right and persist the verdict."
  (interactive)
  (paper-dependency-review--save-verdict "right"))

(defun paper-dependency-review-mark-wrong ()
  "Mark the current edge wrong and persist the verdict."
  (interactive)
  (paper-dependency-review--save-verdict "wrong"))

(defun paper-dependency-review-clear-verdict ()
  "Clear all verdicts for the current edge and persist the change."
  (interactive)
  (paper-dependency-review--clear-verdict))

(defun paper-dependency-review-reload ()
  "Reload the current review JSON from disk."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (setq paper-dependency-review--data
          (paper-dependency-review--read paper-dependency-review--file))
    (paper-dependency-review--refresh id)))

(defun paper-dependency-review--tarball (paper)
  "Return PAPER's configured eprint tarball path."
  (expand-file-name (format "%s.tar.gz" paper) paper-dependency-review-eprints))

(defun paper-dependency-review--extract (paper tarball)
  "Extract TARBALL for PAPER once and return its cache directory."
  (let ((directory (expand-file-name paper paper-dependency-review-cache)))
    (unless (file-directory-p directory)
      (make-directory directory t)
      (unless (zerop (call-process "tar" nil nil nil "-xzf" tarball "-C" directory))
        (delete-directory directory t)
        (user-error "Could not extract eprint %s" tarball)))
    directory))

(defun paper-dependency-review-jump ()
  "Visit the current edge's source file and line in another window."
  (interactive)
  (let* ((edge (paper-dependency-review--current-edge))
         (location (alist-get 'location edge))
         (file (and (listp location) (alist-get 'file location)))
         (line (and (listp location) (alist-get 'line location)))
         (tarball (paper-dependency-review--tarball paper-dependency-review--paper)))
    (unless (and file line)
      (user-error "Edge has no file:line location"))
    (unless (file-readable-p tarball)
      (user-error "Eprint tarball not found: %s" tarball))
    (let* ((directory (paper-dependency-review--extract paper-dependency-review--paper tarball))
           (matches (directory-files-recursively directory (regexp-quote (file-name-nondirectory file)))))
      (unless matches
        (user-error "Source file not found in eprint: %s" file))
      (find-file-other-window (car matches))
      (goto-char (point-min))
      (forward-line (1- line)))))

(defvar paper-dependency-review-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "r") #'paper-dependency-review-mark-right)
    (define-key map (kbd "w") #'paper-dependency-review-mark-wrong)
    (define-key map (kbd "u") #'paper-dependency-review-clear-verdict)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "g") #'paper-dependency-review-reload)
    (define-key map (kbd "RET") #'paper-dependency-review-jump)
    (define-key map (kbd "q") #'quit-window)
    map))

(define-derived-mode paper-dependency-review-mode tabulated-list-mode "DependencyReview"
  "Tabulated review of one paper's dependency edges."
  (setq tabulated-list-format [
        ("✓" 3 t) ("rule" 5 t) ("edge" 5 t)
        ("source" 34 t) ("target" 34 t) ("where" 30 t)]
        tabulated-list-padding 2
        truncate-lines t)
  (tabulated-list-init-header))

;;;###autoload
(defun paper-dependency-review (paper)
  "Open PAPER's dependency review table."
  (interactive
   (list (completing-read
          "Paper: "
          (mapcar (lambda (file)
                    (string-remove-suffix "-dependency-review.json" file))
                  (directory-files (paper-dependency-review--directory) nil
                                   "-dependency-review\.json\\'"))
          nil t)))
  (let* ((file (expand-file-name (format "%s-dependency-review.json" paper)
                                (paper-dependency-review--directory)))
         (buffer (get-buffer-create (format "*deps %s*" paper))))
    (unless (file-readable-p file)
      (user-error "No dependency review for %s: %s" paper file))
    (with-current-buffer buffer
      (paper-dependency-review-mode)
      (setq paper-dependency-review--paper paper
            paper-dependency-review--file file
            paper-dependency-review--data (paper-dependency-review--read file))
      (paper-dependency-review--refresh))
    (pop-to-buffer buffer)))

(provide 'paper-dependency-review)
;;; paper-dependency-review.el ends here
