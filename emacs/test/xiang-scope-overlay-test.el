;;; xiang-scope-overlay-test.el --- Scope overlay tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(defconst xiang-scope-overlay-test--dir
  (file-name-directory (or load-file-name buffer-file-name)))
(load-file (expand-file-name "../xiang-scope-overlay.el"
                            xiang-scope-overlay-test--dir))

(defun xiang-scope-overlay-test--fixture ()
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "fixtures/xiang-scopes.json"
                       xiang-scope-overlay-test--dir))
    (json-parse-string (buffer-string) :object-type 'alist :array-type 'list)))

(defun xiang-scope-overlay-test--setup ()
  (let ((buffer (generate-new-buffer " *xiang-scope-test*")))
    (with-current-buffer buffer
      (insert "openreply")
      (setq-local agent-chat--session-id "s")
      (setq-local agent-chat--current-turn-id "turn-1")
      (setq-local session-mode--last-operator-region
                  (cons (copy-marker 1) (copy-marker 5)))
      (setq-local session-mode--last-agent-region
                  (cons (copy-marker 5) (copy-marker 10))))
    buffer))

(ert-deftest xiang-scope-overlay-offsets-and-help-echo ()
  (let ((buffer (xiang-scope-overlay-test--setup)))
    (unwind-protect
        (with-current-buffer buffer
          (xiang-scope-overlay--apply (xiang-scope-overlay-test--fixture))
          (should (= 2 (length xiang-scope-overlay--overlays)))
          (should (cl-some
                   (lambda (ov)
                     (string-match-p "R3" (or (overlay-get ov 'help-echo) "")))
                   xiang-scope-overlay--overlays)))
      (kill-buffer buffer))))

(ert-deftest xiang-scope-overlay-refresh-removes-duplicates-and-errors-cleanly ()
  (let ((buffer (xiang-scope-overlay-test--setup)) messages)
    (unwind-protect
        (with-current-buffer buffer
          (setq xiang-scope-overlay--overlays (list (make-overlay 1 2)))
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) messages)))
                    ((symbol-function 'url-retrieve)
                     (lambda (_url callback)
                       (funcall callback '(:error "offline")))))
            (xiang-scope-overlay-refresh)
            (should (= 0 (length xiang-scope-overlay--overlays)))
            (should (= 1 (length messages)))))
      (kill-buffer buffer))))

;;; xiang-scope-overlay-test.el ends here
