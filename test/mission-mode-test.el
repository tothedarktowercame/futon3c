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
        (should (string-match-p "eightfold-phase·identify" labels))
        (should (string-match-p "@shown 1"
                                (substring-no-properties header-line-format))))
      (mission-mode--clear-overlays)
      (should (null mission-mode--overlays)))))


(ert-deftest mission-mode-keeps-distinct-patterns-on-the-same-table-row ()
  (with-temp-buffer
    (insert "## ARGUE\n| `aif/admissibility` · `aif/no-self-certification` | eval |\n")
    (let* ((passage "| `aif/admissibility` · `aif/no-self-certification` | eval |")
           (a `((id . "a") (type . "pattern") (title . "aif/admissibility")
                (passage . ,passage)))
           (b `((id . "b") (type . "pattern") (title . "aif/no-self-certification")
                (passage . ,passage)))
           (old `((id . "old-a") (type . "pattern") (title . "aif/admissibility")
                  (passage . ,passage))))
      (should (= 2 (mission-mode--annotate-current-buffer
                    `((mission . "M-table") (scope_count . 3)
                      (scopes . (,a ,b ,old))))))
      (should (= 2 (length (mission-mode--region-overlays)))))))

(ert-deftest mission-mode-disable-clears-owned-overlays-with-lost-tracking ()
  (with-temp-buffer
    (insert "## IDENTIFY\nbody\n")
    (let ((foreign (make-overlay (point-min) (point-max)))
          (base-header "Original header"))
      (mission-mode--annotate-current-buffer
       '((mission . "M-demo") (scope_count . 1)
         (scopes . (((id . "demo/identify") (type . "eightfold-phase")
                     (title . "IDENTIFY") (passage . "## IDENTIFY"))))))
      ;; Reproduce the live failure: owned overlays survive, but their
      ;; buffer-local tracking list has already been cleared.
      (setq mission-mode--overlays nil
            mission-mode-minor-mode t
            mission-mode--base-header-line base-header)
      (let ((badge (make-overlay (point-max) (point-max))))
        (overlay-put badge 'mission-mode t)
        (overlay-put badge 'after-string "badge"))
      (narrow-to-region 3 5)
      (mission-mode-minor-mode -1)
      (should-not mission-mode-minor-mode)
      (should-not mission-mode--overlays)
      (should (equal header-line-format base-header))
      (should (eq (overlay-buffer foreign) (current-buffer)))
      (let ((remaining (append (car (overlay-lists)) (cdr (overlay-lists)))))
        (should (equal remaining (list foreign)))))))

;;; mission-mode-test.el ends here
