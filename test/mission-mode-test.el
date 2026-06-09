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

;;; mission-mode-test.el ends here
