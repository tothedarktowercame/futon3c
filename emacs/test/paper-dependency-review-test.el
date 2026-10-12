;;; paper-dependency-review-test.el --- ERT for dependency review -*- lexical-binding: t; -*-

(require 'ert)
(require 'json)
(require 'paper-dependency-review)

(defconst paper-dependency-review-test--fixture
  '((paper . "fixture")
    (built_at . "2026-10-12T00:00:00Z")
    (nodes . (((id . "claim:a") (type . "claim") (label . "A"))
              ((id . "claim:b") (type . "claim") (label . "B"))
              ((id . "definition:d") (type . "definition") (label . "D"))))
    (edges . (((id . "e1") (source . "claim:a") (target . "claim:b")
               (rule . "D2") (style . "dashed") (cycle . t)
               (location . ((file . "a.tex") (line . 4))) (evidence . "a"))
              ((id . "e2") (source . "claim:b") (target . "claim:a")
               (rule . "D1") (style . "solid") (cycle . t)
               (location . ((file . "b.tex") (line . 8))) (evidence . "b"))
              ((id . "e3") (source . "definition:d") (target . "claim:a")
               (rule . "D2") (style . "dashed") (cycle . :json-false)
               (location . ((file . "d.tex") (line . 2))) (evidence . "d"))))
    (unresolved_refs . ())
    (counts . ((D1 . 1) (D2 . 2)))
    (cycle_scc_count . 1)
    (verdicts . [])))

(defmacro paper-dependency-review-test--with-fixture (&rest body)
  "Run BODY with a temporary fixture review directory."
  (declare (indent 0) (debug t))
  `(let* ((directory (make-temp-file "dependency-review-" t))
          (paper-dependency-review-dir directory)
          (paper-dependency-review-eprints (expand-file-name "eprints" directory))
          (file (expand-file-name "fixture-dependency-review.json" directory)))
     (unwind-protect
         (progn
           (with-temp-file file
             (insert (json-encode paper-dependency-review-test--fixture)))
           ,@body)
       (when (get-buffer "*deps fixture*")
         (kill-buffer "*deps fixture*"))
       (delete-directory directory t))))

(defun paper-dependency-review-test--open ()
  "Open the fixture and return its buffer."
  (paper-dependency-review "fixture")
  (get-buffer "*deps fixture*"))

(ert-deftest paper-dependency-review-renders-cycle-faces ()
  (paper-dependency-review-test--with-fixture
    (let ((buffer (paper-dependency-review-test--open)))
      (with-current-buffer buffer
        (should (= 3 (length tabulated-list-entries)))
        (should (equal 'error (get-text-property 0 'face
                                                  (aref (cadr (assoc "e1" tabulated-list-entries)) 2))))
        (should-not (get-text-property 0 'face
                                       (aref (cadr (assoc "e3" tabulated-list-entries)) 2)))))))

(ert-deftest paper-dependency-review-verdicts-are-append-only-and-clearable ()
  (paper-dependency-review-test--with-fixture
    (let ((buffer (paper-dependency-review-test--open)))
      (with-current-buffer buffer
        (goto-char (point-min))
        (forward-line 2)
        (let ((edge-id (tabulated-list-get-id)))
        (paper-dependency-review-mark-right)
        (paper-dependency-review-mark-wrong)
        (let* ((data (paper-dependency-review--read paper-dependency-review--file))
               (verdicts (alist-get 'verdicts data)))
          (should (= 2 (length verdicts)))
          (should (equal "wrong" (alist-get 'verdict (car (last verdicts)))))
          (should (date-to-time (alist-get 'at (car verdicts))))
          (should (equal "✗" (aref (cadr (assoc edge-id tabulated-list-entries)) 0))))
        (paper-dependency-review-clear-verdict)
        (should-not (alist-get 'verdicts
                               (paper-dependency-review--read paper-dependency-review--file))))))))

(ert-deftest paper-dependency-review-preserves-graph-values ()
  (paper-dependency-review-test--with-fixture
    (let ((buffer (paper-dependency-review-test--open)))
      (with-current-buffer buffer
        (paper-dependency-review-mark-right)
        (let ((after (paper-dependency-review--read paper-dependency-review--file)))
          (should (equal (alist-get 'nodes paper-dependency-review-test--fixture)
                         (alist-get 'nodes after)))
          (should (equal (alist-get 'edges paper-dependency-review-test--fixture)
                         (alist-get 'edges after))))))))

(ert-deftest paper-dependency-review-jump-reports-missing-tarball ()
  (paper-dependency-review-test--with-fixture
    (let ((buffer (paper-dependency-review-test--open)))
      (with-current-buffer buffer
        (goto-char (point-min))
        (forward-line 2)
        (let ((message (should-error (paper-dependency-review-jump) :type 'user-error)))
          (should (string-match-p "fixture\.tar\.gz" (error-message-string message))))))))

;;; paper-dependency-review-test.el ends here
