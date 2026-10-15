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
      (insert "op0rep0op1seg0TOOLseg1TOOLseg2")
      (setq-local agent-chat--session-id "s")
      (setq-local agent-chat--current-turn-id "turn-1")
      (setq-local session-mode--turn-text-regions
                  (list (cons "turn-0" (cons "agent"
                                                (cons (copy-marker 4) (copy-marker 8))))
                        (cons "turn-1" (cons "operator"
                                                (cons (copy-marker 8) (copy-marker 11))))
                        (cons "turn-1" (cons "agent"
                                                (cons (copy-marker 11) (copy-marker 15))))
                        (cons "turn-1" (cons "agent"
                                                (cons (copy-marker 19) (copy-marker 23))))
                        (cons "turn-1" (cons "agent"
                                                (cons (copy-marker 27) (copy-marker 31))))))
      (setq-local session-mode--last-operator-region
                  (cons (copy-marker 8) (copy-marker 11))))
    buffer))

(ert-deftest xiang-scope-overlay-offsets-and-help-echo ()
  (let ((buffer (xiang-scope-overlay-test--setup)))
    (unwind-protect
        (with-current-buffer buffer
          (xiang-scope-overlay--apply (xiang-scope-overlay-test--fixture))
          (should (= 4 (length xiang-scope-overlay--overlays)))
          (should (= 4 (overlay-start (cl-find-if
                                       (lambda (ov)
                                         (equal "turn-0-r-0"
                                                (overlay-get ov 'xiang-scope-act)))
                                       xiang-scope-overlay--overlays))))
          (should (cl-some (lambda (ov)
                             (and (equal "turn-1-r-2"
                                         (overlay-get ov 'xiang-scope-act))
                                  (= 27 (overlay-start ov))))
                           xiang-scope-overlay--overlays))
          (should (cl-some
                   (lambda (ov)
                     (string-match-p "R3" (or (overlay-get ov 'help-echo) "")))
                   xiang-scope-overlay--overlays)))
      (kill-buffer buffer))))

(ert-deftest xiang-scope-overlay-describe-and-list-read-without-hover ()
  "A terminal Emacs has no hover: the echo and the list carry the same text."
  (with-temp-buffer
    (insert "alpha beta gamma")
    (let ((ov (make-overlay 1 6)))
      (overlay-put ov 'help-echo "propose · within R3 → the parent act")
      (overlay-put ov 'xiang-scope-act "t-f-s1-0")
      (setq-local xiang-scope-overlay--overlays (list ov))
      (goto-char 3)
      (let ((echoed nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
          (xiang-scope-overlay-describe)
          (should (equal "propose · within R3 → the parent act" echoed))
          (goto-char 10)
          (xiang-scope-overlay-describe)
          (should (equal "No scope act at point" echoed))))
      (cl-letf (((symbol-function 'display-buffer) #'ignore))
        (xiang-scope-overlay-list))
      (with-current-buffer "*xiang scopes*"
        (should (string-match-p "t-f-s1-0" (buffer-string)))
        (should (string-match-p "within R3" (buffer-string)))
        (should (string-match-p "\"alpha\"" (buffer-string)))))))

(ert-deftest xiang-scope-overlay-404-schedules-one-retry-for-the-current-turn ()
  "The landing refresh can race the route; after the fallback, one retry is
scheduled for the current turn and that retry schedules none."
  (with-temp-buffer
    (setq-local agent-chat--session-id "s")
    (setq-local agent-chat--current-turn-id "seat-turn-9")
    (setq-local xiang-scope-overlay-mode t)
    (let ((scheduled nil)
          (fetched nil))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (secs _repeat fn &rest _) (push (list secs fn) scheduled) 'timer))
                ((symbol-function 'message) #'ignore)
                (xiang-scope-overlay-fetch-function
                 (lambda (url callback)
                   (push url fetched)
                   (if (string-match-p "turn=seat-turn-9" url)
                       (funcall callback '(:error (error http 404)))
                     (funcall callback '(:error (error http 404)))))))
        (xiang-scope-overlay-refresh)
        (should (= 1 (length scheduled)))
        (should (= 4 (car (car scheduled))))
        ;; running the retry asks for the current turn again and schedules nothing more
        (setq fetched nil)
        (funcall (cadr (car scheduled)))
        (should (cl-some (lambda (u) (string-match-p "turn=seat-turn-9" u)) fetched))
        (should (= 1 (length scheduled)))))))

(ert-deftest xiang-scope-overlay-refresh-removes-duplicates-and-errors-cleanly ()
  (let ((buffer (xiang-scope-overlay-test--setup)) messages)
    (unwind-protect
        (with-current-buffer buffer
          (setq xiang-scope-overlay--overlays (list (make-overlay 1 2)))
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) messages)))
                    ((symbol-function 'url-retrieve)
                     (let ((calls 0))
                       (lambda (_url callback)
                         (setq calls (1+ calls))
                         (if (= calls 1)
                             (funcall callback '(:error "(error http 404)"))
                           (with-temp-buffer
                             (insert "HTTP/1.1 200 OK\n\n")
                             (insert-file-contents
                              (expand-file-name "fixtures/xiang-scopes.json"
                                                xiang-scope-overlay-test--dir))
                             (funcall callback nil)))))))
            (xiang-scope-overlay-refresh)
            (should (= 4 (length xiang-scope-overlay--overlays)))
            (xiang-scope-overlay-refresh)
            (should (= 4 (length xiang-scope-overlay--overlays)))
            (should (cl-some (lambda (m) (string-match-p "falling back" m)) messages))))
      (kill-buffer buffer))))

;;; xiang-scope-overlay-test.el ends here
