;;; mission-mode-test.el --- Tests for mission-mode.el -*- lexical-binding: t; -*-

(require 'ert)
(load-file "/home/joe/code/futon3c/emacs/mission-mode.el")

(ert-deftest mission-mode-field-reads-json-alists ()
  (let ((row '((mission . "M-demo")
               (type_counts . (((type . "eightfold-phase") (count . 1)))))))
    (should (equal "M-demo" (mission-mode--field row :mission)))
    (should (equal "eightfold-phase"
                   (mission-mode--field
                    (car (mission-mode--field row :type_counts))
                    :type)))))

(ert-deftest mission-mode-render-shows-arxana-markup-and-scope-state ()
  (let ((data '((mission . "M-demo")
                (generated_at . "2026-06-09T00:00:00Z")
                (scope_count . 1)
                (type_counts . (((type . "eightfold-phase") (count . 1))))
                (scopes . (((id . "demo/identify")
                            (type . "eightfold-phase")
                            (title . "IDENTIFY")
                            (parent . "demo/root")
                            (anchor_state . "anchored")
                            (parent_state . "linked")
                            (passage . "## IDENTIFY")))))))
    (with-temp-buffer
      (mission-scope-view-mode)
      (mission-mode--render data)
      (let ((text (buffer-string)))
        (should (string-match-p "@mission M-demo" text))
        (should (string-match-p "@scope-type eightfold-phase" text))
        (should (string-match-p "demo/identify" text))
        (should (string-match-p ":: ## IDENTIFY" text))))))

(ert-deftest mission-mode-infers-mission-from-mission-file-path ()
  (should (equal "M-pattern-application-diagnostic"
                 (mission-mode--mission-from-path
                  "/home/joe/code/futon3c/holes/missions/M-pattern-application-diagnostic.md"))))

(ert-deftest mission-mode-annotates-current-buffer-with-live-scopes ()
  (let ((data '((mission . "M-demo")
                (generated_at . "2026-06-09T00:00:00Z")
                (scope_count . 1)
                (type_counts . (((type . "eightfold-phase") (count . 1))))
                (scopes . (((id . "demo/identify")
                            (type . "eightfold-phase")
                            (title . "IDENTIFY")
                            (anchor_state . "anchored")
                            (parent_state . "linked")
                            (passage . "## IDENTIFY")))))))
    (with-temp-buffer
      (insert "# Mission: M-demo\n\n## IDENTIFY\n\nbody\n")
      (mission-mode--annotate-current-buffer data)
      (let ((labels (mapconcat
                     (lambda (ov)
                       (or (overlay-get ov 'after-string) ""))
                     mission-mode--overlays
                     "\n")))
        (should (string-match-p "@scope eightfold-phase demo/identify" labels))
        (should (string-match-p "@shown 1"
                                (substring-no-properties header-line-format))))
      (mission-mode--clear-overlays)
      (should (null mission-mode--overlays)))))

;;; mission-mode-test.el ends here
