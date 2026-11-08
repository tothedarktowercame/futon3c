;;; session-mode-lexicon-test.el --- Deterministic lexicon tier tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'json)
(require 'seq)
(require 'session-mode)
(require 'agent-chat)

(defmacro session-mode-lexicon-test--with-vocab (&rest body)
  "Run BODY with a small generated lexicon vocabulary."
  (declare (indent 0) (debug t))
  `(let* ((file (make-temp-file "session-lexicon" nil ".json"))
          (session-mode-lexicon-file file)
          (session-mode-show-lexicon t)
          (session-mode--lexicon-key nil)
          (session-mode--lexicon-regexp nil)
          (session-mode--lexicon-forms nil)
          (session-mode--lexicon-entities nil)
          (session-mode--lexicon-levels nil))
     (unwind-protect
         (progn
           (with-temp-file file
             (insert (json-encode
                      '((forms . [((form . "pattern cascade") (entity . "cascade") (common . :json-false))
                                   ((form . "pattern") (entity . "pattern") (common . :json-false))
                                   ((form . "park") (entity . "park") (common . t))
                                   ((form . "象") (entity . "xiang") (common . :json-false))])
                        (entities . ((cascade . ((name . "Pattern cascade") (status . "live")
                                                  (level . 7) (what . "Connected patterns")
                                                  (path . "futon3") (url . "https://example/cascade")))
                                     (pattern . ((name . "Pattern") (status . "live")
                                                  (level . 6) (what . "Reusable method")
                                                  (path . "futon3") (url . "https://example/pattern")))
                                     (park . ((name . "Park") (status . "live")
                                               (level . 8) (what . "Defer a turn")
                                               (path . "agency") (url . "https://example/park")))
                                     (xiang . ((name . "象") (status . "live")
                                                (level . 7) (what . "Turn reader")
                                                (path . "futon3c") (url . "https://example/xiang")))))
                        (levels . (("6" . ((label . "on demand") (colour . "#28ae80")))
                                   ("7" . ((label . "automatic") (colour . "#5ec962")))
                                   ("8" . ((label . "regular use") (colour . "#addc30")))
                                   (none . ((label . "concept") (colour . "#a0a0a0")))))))))
           ,@body)
       (delete-file file))))

(defun session-mode-lexicon-test--overlays ()
  "Return lexicon overlays in the current buffer, ordered by position."
  (sort (seq-filter (lambda (overlay) (overlay-get overlay 'session-mode-lexicon))
                    (overlays-in (point-min) (point-max)))
        (lambda (a b) (< (overlay-start a) (overlay-start b)))))

(ert-deftest session-mode-lexicon-longest-whole-word-and-cjk ()
  (session-mode-lexicon-test--with-vocab
    (with-temp-buffer
      (insert "A pattern cascade uses pattern; patterned does not. 象 can park.")
      (session-mode--mark-lexicon-region (point-min) (point-max))
      (let ((overlays (session-mode-lexicon-test--overlays)))
        (should (equal '("pattern cascade" "pattern" "象" "park")
                       (mapcar (lambda (overlay)
                                 (buffer-substring-no-properties
                                  (overlay-start overlay) (overlay-end overlay)))
                               overlays)))
        (should (eq 'dots
                    (plist-get (plist-get (overlay-get (car (last overlays)) 'face)
                                         :underline)
                               :style)))
        (should (string-match-p "liveness: automatic"
                                (overlay-get (car overlays) 'help-echo)))
        (should (equal "https://example/cascade"
                       (overlay-get (car overlays) 'session-mode-lexicon-url)))
        (should (keymapp (overlay-get (car overlays) 'keymap)))))))

(ert-deftest session-mode-lexicon-skips-operator-input-code-and-links ()
  (session-mode-lexicon-test--with-vocab
    (with-temp-buffer
      (insert "joe: pattern\nagent: pattern cascade park pattern 象\n$> pattern")
      (put-text-property 1 2 'agent-chat-message-start t)
      (let ((agent-start (progn (goto-char (point-min)) (search-forward "agent:")
                                (line-beginning-position))))
        (put-text-property agent-start (1+ agent-start) 'agent-chat-message-start t))
      (setq-local agent-chat--agent-name "agent")
      (setq-local agent-chat--input-start
                  (copy-marker (progn (goto-char (point-min)) (search-forward "$> "))))
      (goto-char (point-min))
      (search-forward "pattern cascade")
      (put-text-property (match-beginning 0) (match-end 0) 'agent-chat-in-fence t)
      (search-forward "park")
      (let ((link (make-overlay (match-beginning 0) (match-end 0))))
        (overlay-put link 'agent-chat-link t))
      (dolist (region (session-mode--agent-regions))
        (session-mode--mark-lexicon-region (car region) (cdr region)))
      (should (equal '("pattern" "象")
                     (mapcar (lambda (overlay)
                               (buffer-substring-no-properties
                                (overlay-start overlay) (overlay-end overlay)))
                             (session-mode-lexicon-test--overlays)))))))

(ert-deftest session-mode-lexicon-turn-end-marks-current-turn-only ()
  (session-mode-lexicon-test--with-vocab
    (with-temp-buffer
      (insert "old pattern\nnew pattern cascade")
      (let* ((old-beg (copy-marker (point-min)))
             (old-end (copy-marker 12))
             (new-beg (copy-marker 13))
             (new-end (copy-marker (point-max)))
             (agent-chat--current-turn-id "turn-2")
             (session-mode--turn-text-regions
              (list (cons "turn-2" (cons "agent" (cons new-beg new-end)))
                    (cons "turn-1" (cons "agent" (cons old-beg old-end))))))
        (session-mode--mark-current-turn-lexicon)
        (let ((overlays (session-mode-lexicon-test--overlays)))
          (should (= 1 (length overlays)))
          (should (equal "pattern cascade"
                         (buffer-substring-no-properties
                          (overlay-start (car overlays)) (overlay-end (car overlays))))))))))

(ert-deftest session-mode-lexicon-renderer-cleanup-recognises-overlay ()
  (with-temp-buffer
    (insert "pattern")
    (let ((overlay (make-overlay 1 8)))
      (overlay-put overlay 'session-mode-lexicon t)
      (should (agent-chat--renderer-overlay-p overlay))
      (agent-chat--delete-renderer-overlays (point-min) (point-max))
      (should-not (overlay-buffer overlay)))))

(provide 'session-mode-lexicon-test)
;;; session-mode-lexicon-test.el ends here
