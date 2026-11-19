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

(defun mission-mode-test--fetch (command)
  "Run `mission-mode--fetch-async' with COMMAND; return (ok . value)."
  (let ((mission-mode-command command)
        (mission-mode-project-dir temporary-file-directory)
        (result nil))
    (mission-mode--fetch-async "M-x"
                               (lambda (data) (setq result (cons 'ok data)))
                               (lambda (err) (setq result (cons 'err err))))
    (with-timeout (10 (error "fetch never finished"))
      (while (null result) (accept-process-output nil 0.05)))
    result))

(ert-deftest mission-mode-fetch-keeps-stderr-out-of-the-json ()
  ;; 2026-11-19: a bash error on stderr was parsed as JSON
  ;; ("JSON readtable error: 115").
  (let ((r (mission-mode-test--fetch
            '("sh" "-c" "echo 'script: Argument list too long' >&2; echo '{\"mission\":\"M-x\"}'" "sh"))))
    (should (eq 'ok (car r)))
    (should (equal "M-x" (mission-mode--field (cdr r) :mission)))))

(ert-deftest mission-mode-fetch-reports-a-failed-command ()
  (let ((r (mission-mode-test--fetch '("sh" "-c" "echo boom >&2; exit 3" "sh"))))
    (should (eq 'err (car r)))
    (should (string-match-p "boom" (error-message-string (cdr r))))))

(ert-deftest mission-mode-fetch-passes-the-scope-type-filter ()
  (with-temp-buffer
    (setq-local mission-mode-scope-types '("pattern" "psr"))
    (let ((r (mission-mode-test--fetch
              '("sh" "-c" "printf '{\"args\":\"%s\"}' \"$*\"" "sh"))))
      (should (eq 'ok (car r)))
      (should (equal "--mission M-x --types pattern,psr"
                     (mission-mode--field (cdr r) :args))))))

(ert-deftest mission-mode-downstream-async-tries-candidates-in-order ()
  (let ((asked nil) (got 'pending))
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url cb &rest _)
                 (push url asked)
                 (with-temp-buffer
                   (insert "HTTP/1.1 200 OK\n\n"
                           (if (string-match-p "second" url)
                               "{\"hyperedges\":[{\"hx/type\":\"mission-cross-ref\",\"hx/ends\":[{\"entity-id\":\"r-d/mission/M-other\"}]}]}"
                             "{\"hyperedges\":[]}"))
                   (funcall cb nil))
                 nil)))
      (mission-mode--fetch-downstream-async '("first" "second" "third")
                                            (lambda (d) (setq got d))))
    (should (= 2 (length asked)))
    (should (equal '(nil "r-d/mission/M-other") got))))

(ert-deftest mission-mode-overview-filtered-has-no-absent-phases ()
  (let ((src (generate-new-buffer "mm-src")))
    (unwind-protect
        (progn
          (with-current-buffer src
            (insert "# M-invented\n\nSome text.\n\nA pattern passage here.\n")
            (setq-local mission-mode-scope-types '("pattern"))
            (setq mission-mode--mission "M-invented")
            (let ((ov (make-overlay 30 53)))
              (overlay-put ov 'mission-mode-scope
                           '((id . "p1") (type . "pattern") (title . "p-invented")))
              (overlay-put ov 'priority 13)
              (setq mission-mode--overlays (list ov))))
          (cl-letf (((symbol-function 'display-buffer) #'ignore)
                    ((symbol-function 'select-window) #'ignore)
                    ((symbol-function 'get-buffer-window) #'ignore)
                    ((symbol-function 'mission-mode--fetch-downstream-async)
                     (lambda (&rest _) (error "filtered view must not fetch downstream"))))
            (with-current-buffer src (mission-mode-overview)))
          (with-current-buffer "*mission-overview*"
            (let ((text (buffer-string)))
              (should (string-match-p "M-invented — 1 pattern · 1 citations" text))
              (should-not (string-match-p "absent" text)))))
      (kill-buffer src)
      (when (get-buffer "*mission-overview*") (kill-buffer "*mission-overview*")))))

(ert-deftest mission-mode-overview-filtered-and-empty-replaces-the-panel ()
  ;; A filtered mission with no matching scopes must not leave the previous
  ;; mission's panel standing (2026-11-19).
  (let ((src (generate-new-buffer "mm-src")))
    (unwind-protect
        (progn
          (with-current-buffer (get-buffer-create "*mission-overview*")
            (insert "M-previous — 9 patterns"))
          (with-current-buffer src
            (setq-local mission-mode-scope-types '("pattern"))
            (setq mission-mode--mission "M-empty"
                  mission-mode--overlays nil))
          (cl-letf (((symbol-function 'display-buffer) #'ignore)
                    ((symbol-function 'select-window) #'ignore)
                    ((symbol-function 'get-buffer-window) #'ignore))
            (with-current-buffer src (mission-mode-overview)))
          (with-current-buffer "*mission-overview*"
            (should (string-match-p "M-empty — 0 pattern/\?[a-z]* · 0 citations" (buffer-string)))
            (should-not (string-match-p "M-previous" (buffer-string)))))
      (kill-buffer src)
      (when (get-buffer "*mission-overview*") (kill-buffer "*mission-overview*")))))

(ert-deftest mission-mode-overview-groups-repeated-patterns ()
  (let ((src (generate-new-buffer "mm-src")))
    (unwind-protect
        (progn
          (with-current-buffer src
            (insert "# M-invented\nfirst cites alpha\nsecond cites beta\nthird cites alpha again\n")
            (setq-local mission-mode-scope-types '("pattern"))
            (setq mission-mode--mission "M-invented" mission-mode--overlays nil)
            (dolist (spec '((2 "ns/alpha" "a1") (3 "ns/beta" "b1") (4 "ns/alpha" "a2")))
              (save-excursion
                (goto-char (point-min)) (forward-line (1- (nth 0 spec)))
                (let ((ov (make-overlay (point) (line-end-position))))
                  (overlay-put ov 'mission-mode-scope
                               `((id . ,(nth 2 spec)) (pattern_id . ,(nth 1 spec)) (type . "pattern")))
                  (push ov mission-mode--overlays)))))
          (cl-letf (((symbol-function 'display-buffer) #'ignore)
                    ((symbol-function 'select-window) #'ignore)
                    ((symbol-function 'get-buffer-window) #'ignore))
            (with-current-buffer src (mission-mode-overview)))
          (with-current-buffer "*mission-overview*"
            (let ((text (buffer-string)))
              (should (= 1 (how-many "ns/alpha" (point-min) (point-max))))
              (should (string-match-p "ns/alpha  ×2\n    ↳ first cites alpha\n    ↳ third cites alpha again\n pattern  ns/beta  second cites beta" text)))))
      (kill-buffer src)
      (when (get-buffer "*mission-overview*") (kill-buffer "*mission-overview*")))))

(ert-deftest mission-mode-overview-citation-drops-the-pattern-name ()
  (with-temp-buffer
    (insert "- `ns/alpha`: \"what alpha says here\"\n- `ns/beta` and `ns/alpha` — both\n")
    (goto-char (point-min))
    (let ((one (make-overlay 1 (line-end-position 1)))
          (two (make-overlay (line-beginning-position 2) (line-end-position 2))))
      (should (equal "what alpha says here\"" (mission-mode--overview-citation one "ns/alpha")))
      ;; Short tail: fall back to the words before the mention.
      (should (equal "… ns/beta and" (mission-mode--overview-citation two "ns/alpha")))
      (should (equal "and ns/alpha — both" (mission-mode--overview-citation two "ns/beta"))))))
