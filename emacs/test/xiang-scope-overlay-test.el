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

(ert-deftest xiang-scope-overlay-agent-acts-are-found-by-their-words ()
  "The buffer shows tool-call blocks and a speaker prefix the record does not,
so an agent paragraph is placed by its first words, not by offset."
  (with-temp-buffer
    (insert "claude: Narration before the tools.\n\n[Bash] cd somewhere && run a long command\n  output line one\n  output line two\n\n")
    (insert "㊢ (what was done) The first marked paragraph of the reply.\n\n")
    (insert "[Bash] another tool call\n\n")
    (insert "㊬ (checked) The second marked paragraph, after a tool block.\n\n")
    (let* ((beg (copy-marker 1 t)) (end (copy-marker (point-max) nil))
           (entries (list (cons "seat-turn-4" (cons "agent" (cons beg end)))))
           (act-a '((id . "seat-turn-4-r-0") (side . "agent") (start . 40)
                    (text . "(what was done) The first marked paragraph of the reply.")))
           (act-b '((id . "seat-turn-4-r-1") (side . "agent") (start . 98)
                    (text . "(checked) The second marked paragraph, after a tool block."))))
      (should (equal "(what was done) The first marked par"
                     (buffer-substring-no-properties
                      (xiang-scope-overlay--act-position act-a entries)
                      (+ (xiang-scope-overlay--act-position act-a entries) 36))))
      (should (equal "(checked) The second marked paragrap"
                     (buffer-substring-no-properties
                      (xiang-scope-overlay--act-position act-b entries)
                      (+ (xiang-scope-overlay--act-position act-b entries) 36))))
      ;; the underline for an agent paragraph is its opening mark, one character
      (let ((pos (xiang-scope-overlay--act-position act-a entries)))
        (should (equal (cons (- pos 2) (- pos 1)) (xiang-scope-overlay--span act-a pos)))
        (should (equal "㊢" (buffer-substring-no-properties (- pos 2) (- pos 1)))))
      ;; an operator fragment keeps its whole text
      (should (equal (cons 5 11) (xiang-scope-overlay--span
                                  '((id . "t-f-s1-0") (side . "operator") (text . "abcdef")) 5)))
      ;; the offset fallback is still there for a text that cannot be found
      (should (integerp (xiang-scope-overlay--act-position
                         '((id . "seat-turn-4-r-2") (side . "agent") (start . 3) (text . "zz"))
                         entries))))))

(ert-deftest xiang-scope-overlay-dead-region-entries-are-skipped ()
  "An entry whose markers died (cleared or text deleted) is dropped, not fed
to arithmetic, so one stale entry cannot abort the paint."
  (with-temp-buffer
    (insert "joe: hello there\n\n")
    (let* ((dead-beg (make-marker)) (dead-end (make-marker))   ; never set: no position
           (live-beg (copy-marker 6 t)) (live-end (copy-marker 17 nil)))
      (setq-local session-mode--turn-text-regions
                  (list (cons "seat-turn-2" (cons "operator" (cons dead-beg dead-end)))
                        (cons "seat-turn-3" (cons "operator" (cons live-beg live-end)))))
      (should (equal '("seat-turn-3") (mapcar #'car (xiang-scope-overlay--regions))))
      (should (null (xiang-scope-overlay--position (cons dead-beg dead-end) 0)))
      (should (= 6 (xiang-scope-overlay--position (cons live-beg live-end) 0))))))

(ert-deftest xiang-scope-overlay-stale-reply-is-dropped ()
  "A reply from an earlier refresh must not paint over a later one."
  (with-temp-buffer
    (setq-local agent-chat--session-id "s")
    (setq-local agent-chat--current-turn-id "seat-turn-5")
    (setq-local xiang-scope-overlay-mode t)
    (insert "joe: one two\n\n")
    (setq-local session-mode--turn-text-regions
                (list (cons "seat-turn-5" (cons "operator" (cons (copy-marker 6 t) (copy-marker 13 nil))))))
    (let ((callbacks nil)
          (payload "{\"ok\":true,\"turn\":\"seat-turn-5\",\"edges\":[],\"acts\":[{\"id\":\"seat-turn-5-f-s1-0\",\"kind\":\"report\",\"side\":\"operator\",\"start\":0,\"end\":7,\"text\":\"one two\"}]}"))
      (cl-letf (((symbol-function 'message) #'ignore)
                ((symbol-function 'run-at-time) #'ignore)
                (xiang-scope-overlay-fetch-function
                 (lambda (_url callback) (push callback callbacks))))
        (xiang-scope-overlay-refresh)      ; generation 1, reply held
        (xiang-scope-overlay-refresh)      ; generation 2, reply held
        (should (= 2 (length callbacks)))
        (let ((reply (lambda (cb)
                       (with-temp-buffer
                         (insert "HTTP/1.1 200 OK\n\n" payload)
                         (funcall cb nil)))))
          (funcall reply (car callbacks))        ; newest first in the list: generation 2
          (should (= 1 (length xiang-scope-overlay--overlays)))
          (funcall reply (cadr callbacks))       ; the stale generation-1 reply
          (should (= 1 (length xiang-scope-overlay--overlays))))))))

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
