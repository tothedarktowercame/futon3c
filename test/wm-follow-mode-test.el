;;; wm-follow-mode-test.el --- Tests for wm-follow-mode -*- lexical-binding: t; -*-

(require 'ert)
(require 'json)
(require 'wm-follow-mode)

(defconst wm-follow-test--root
  (expand-file-name "fixtures" (file-name-directory (or load-file-name buffer-file-name))))

(defmacro wm-follow-test--with-buffer (&rest body)
  `(with-temp-buffer
     (wm-follow-mode)
     (setq wm-follow--job-cursors (make-hash-table :test 'equal)
           wm-follow--job-marked (make-hash-table :test 'equal)
           wm-follow--tallies (make-hash-table :test 'equal)
           wm-follow--section-markers nil
           wm-follow--ended-clicks nil)
     ,@body))

(ert-deftest wm-follow-phase-line-parses-real-fixture ()
  (let* ((line (car (split-string
                     (with-temp-buffer
                       (insert-file-contents
                        (expand-file-name "wm-follow-phases.edn.log"
                                          wm-follow-test--root))
                       (buffer-string)) "\n" t)))
         (phase (wm-follow--phase-line-parse line)))
    (should (equal (alist-get 'phase phase) ":author-dispatch"))
    (should (equal (alist-get 'transition phase) ":start"))
    (should (equal (alist-get 'attempt-id phase) "attempt-002"))
    (should (equal (wm-follow--phase-time (alist-get 'at phase)) "22:26:35"))
    (should (string-match-p
             "author-dispatch  start"
             (wm-follow--phase-display-line phase)))))

(ert-deftest wm-follow-offset-tail-reads-only-appended-bytes ()
  (let ((file (make-temp-file "wm-follow-tail-")) first second)
    (unwind-protect
        (progn
          (write-region "first\n" nil file nil 'silent)
          (setq first (wm-follow--tail-file file 0))
          (write-region "second\n" nil file t 'silent)
          (setq second (wm-follow--tail-file file (cdr first)))
          (should (equal (car first) "first\n"))
          (should (equal (car second) "second\n")))
      (delete-file file))))

(ert-deftest wm-follow-job-cursor-prevents-duplicate-events ()
  (wm-follow-test--with-buffer
   (let* ((job (json-parse-string
                (with-temp-buffer
                  (insert-file-contents
                   (expand-file-name "wm-follow-job.json" wm-follow-test--root))
                  (buffer-string))
                :object-type 'alist :array-type 'list
                :null-object nil :false-object nil))
          (wm-follow--click-started "2026-11-05T04:43:00Z"))
     (wm-follow--render-jobs (list job))
     (let ((once (buffer-string)))
       (wm-follow--render-jobs (list job))
       (should (equal once (buffer-string)))
       (should (= 1 (length (wm-follow--tally-snapshot))))
       (let ((row (cdar (wm-follow--tally-snapshot))))
         (should (= (plist-get row :turns) 1))
         (should (= (plist-get row :prompt) 73))
         (should (= (plist-get row :text) 28))
         (should (= (plist-get row :tools) 1)))))))

(ert-deftest wm-follow-tally-arithmetic-adds-events ()
  (wm-follow-test--with-buffer
   (wm-follow--tally-add "wm-reviewer" "prompt" "abcd" nil)
   (wm-follow--tally-add "wm-reviewer" "text" "123456" nil)
   (wm-follow--tally-add "wm-reviewer" "tool_use" nil '("bash" "grep"))
   (should (equal (cdar (wm-follow--tally-snapshot))
                  '(:turns 1 :prompt 4 :text 6 :tools 2)))))

(ert-deftest wm-follow-click-change-starts-a-new-section ()
  (wm-follow-test--with-buffer
   (let ((wm-follow-phase-log "/file/that/does/not/exist"))
     (wm-follow--handle-click '((click-id . "click-1")
                                (started-at . "2026-11-05T01:00:00Z")
                                (running? . t) (phase . "selection")
                                (phase-transition . "start")))
     (wm-follow--handle-click '((click-id . "click-2")
                                (started-at . "2026-11-05T02:00:00Z")
                                (running? . t) (phase . "author-wait")
                                (phase-transition . "start")))
     (should (= 2 (length wm-follow--section-markers)))
     (should (string-match-p "click-1" (buffer-string)))
     (should (string-match-p "click-2" (buffer-string))))))

(provide 'wm-follow-mode-test)
;;; wm-follow-mode-test.el ends here
