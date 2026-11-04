;;; agent-chat-large-buffer-test.el --- Large REPL redisplay tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'agent-chat)

(ert-deftest agent-chat-large-buffer-strips-history-and-keeps-tail ()
  (with-temp-buffer
    (insert (make-string 200 ?x))
    (add-text-properties 1 201 '(face bold font-lock-face bold display "x"))
    (let ((old-overlay (make-overlay 10 20))
          (tail-overlay (make-overlay 180 190))
          (agent-chat-large-buffer-threshold 100)
          (agent-chat-large-buffer-decorated-tail 50))
      (overlay-put old-overlay 'agent-chat-code t)
      (overlay-put tail-overlay 'agent-chat-link t)
      (font-lock-mode 1)
      (agent-chat-maybe-degrade-large-buffer)
      (should agent-chat--large-buffer-degraded-p)
      (should-not font-lock-mode)
      (should-not (overlay-buffer old-overlay))
      (should-not (overlay-buffer tail-overlay))
      (should-not (get-text-property 20 'face))
      (should (eq (get-text-property 180 'face) 'bold)))))

(ert-deftest agent-chat-large-buffer-progressively-strips-new-history ()
  (with-temp-buffer
    (let ((agent-chat-large-buffer-threshold 10)
          (agent-chat-large-buffer-decorated-tail 5))
      (insert (propertize (make-string 20 ?a) 'face 'bold))
      (agent-chat-maybe-degrade-large-buffer)
      (let ((first-cutoff agent-chat--large-buffer-stripped-through))
        (goto-char (point-max))
        (insert (propertize (make-string 10 ?b) 'face 'italic))
        (agent-chat-maybe-degrade-large-buffer)
        (should (> agent-chat--large-buffer-stripped-through first-cutoff))
        (should-not (get-text-property 20 'face))
        (should (eq (get-text-property 28 'face) 'italic))))))

(ert-deftest agent-chat-small-buffer-is-unchanged ()
  (with-temp-buffer
    (let ((agent-chat-large-buffer-threshold 100)
          (agent-chat-large-buffer-decorated-tail 10))
      (insert (propertize "small" 'face 'bold))
      (agent-chat-maybe-degrade-large-buffer)
      (should-not agent-chat--large-buffer-degraded-p)
      (should (eq (get-text-property 1 'face) 'bold)))))

(ert-deftest agent-chat-buffer-health-counts-owned-overlays ()
  (with-temp-buffer
    (rename-buffer " *large-repl-health-test*")
    (insert "hello")
    (let ((owned (make-overlay 1 2))
          (_foreign (make-overlay 2 3)))
      (overlay-put owned 'agent-chat-link t)
      (let ((health (agent-chat--buffer-health (current-buffer))))
        (should (= (plist-get health :bytes) 5))
        (should (= (plist-get health :overlays) 2))
        (should (= (plist-get health :renderer-overlays) 1))))))

(ert-deftest agent-chat-pager-mode-trims-at-boundary-and-restores ()
  (with-temp-buffer
    (let ((agent-chat-pager-maximum-size 12)
          (original "joe: first\nagent: second\njoe: third\n"))
      (insert original)
      (dolist (start '(1 12 26))
        (put-text-property start (1+ start) 'agent-chat-message-start t))
      (agent-chat-pager-mode 1)
      (should (equal (buffer-string) "joe: third\n"))
      (should (equal agent-chat--pager-archive
                     "joe: first\nagent: second\n"))
      (agent-chat-pager-mode -1)
      (should (equal (buffer-string) original))
      (should-not agent-chat--pager-archive))))

(ert-deftest agent-chat-pager-mode-is-opt-in ()
  (with-temp-buffer
    (let ((agent-chat-pager-maximum-size 5))
      (insert "one\ntwo\nthree\n")
      (agent-chat-maybe-degrade-large-buffer)
      (should (equal (buffer-string) "one\ntwo\nthree\n")))))

(provide 'agent-chat-large-buffer-test)
;;; agent-chat-large-buffer-test.el ends here
