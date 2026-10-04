;;; evidence-landscape-search-test.el --- Tests for evidence search -*- lexical-binding: t; -*-

(require 'ert)
(require 'evidence-landscape-search)

(ert-deftest evidence-landscape-search-url-default-is-bounded ()
  (let ((evidence-landscape-search-url "http://store.test/text-search")
        (evidence-landscape-search-limit 17))
    (cl-letf (((symbol-function 'evidence-landscape-search--since)
               (lambda () "2026-10-03T12:00:00Z")))
      (should (equal (evidence-landscape-search--url "war machine" nil)
                     (concat "http://store.test/text-search?q=war%20machine&limit=17"
                             "&since=2026-10-03T12%3A00%3A00Z"))))))

(ert-deftest evidence-landscape-search-url-prefix-is-unbounded ()
  (let ((evidence-landscape-search-url "http://store.test/text-search"))
    (should-not (string-match-p
                 "since=" (evidence-landscape-search--url "breakpoint" t)))))

(ert-deftest evidence-landscape-search-renders-real-response-shape ()
  (let* ((payload
          (parseedn-read-str
           (concat "{:ok true :results [{:score -2.5 :entry "
                   "{:evidence/id \"e-1\" :evidence/at \"2026-10-03T23:33:18Z\" "
                   ":evidence/author \"codex-10\" :evidence/type :coordination "
                   ":evidence/session-id \"session-10\" "
                   ":evidence/body {:text \"breakpoint at the outer loop\"}}}]}")))
         (display-buffer-overriding-action '((display-buffer-no-window))))
    (unwind-protect
        (progn
          (evidence-landscape-search--render "breakpoint" nil payload)
          (with-current-buffer "*Evidence Landscape Search*"
            (should (derived-mode-p 'evidence-landscape-search-mode))
            (should (string-match-p "last 24 hours" (buffer-string)))
            (should (string-match-p "codex-10" (buffer-string)))
            (should (string-match-p "session-10" (buffer-string)))
            (should (string-match-p "breakpoint at the outer loop"
                                    (buffer-string)))
            (should (text-property-any (point-min) (point-max)
                                       'evidence-landscape-match t))))
      (when-let* ((buffer (get-buffer "*Evidence Landscape Search*")))
        (kill-buffer buffer)))))

(ert-deftest evidence-landscape-search-shows-only-matching-lines ()
  (let* ((payload
          (parseedn-read-str
           (concat "{:ok true :results [{:entry {:evidence/id \"e-1\" "
                   ":evidence/author \"象-2\" :evidence/body "
                   "{:text \"irrelevant first line\\nbreakpoint here\\nother tail\"}}}]}")))
         (display-buffer-overriding-action '((display-buffer-no-window))))
    (unwind-protect
        (progn
          (evidence-landscape-search--render "breakpoint" nil payload)
          (with-current-buffer "*Evidence Landscape Search*"
            (should (string-match-p "象-2" (buffer-string)))
            (should (string-match-p "      2:breakpoint here" (buffer-string)))
            (should-not (string-match-p "irrelevant first line" (buffer-string)))
            (should-not (string-match-p "other tail" (buffer-string)))))
      (when-let* ((buffer (get-buffer "*Evidence Landscape Search*")))
        (kill-buffer buffer)))))

(ert-deftest evidence-landscape-search-decodes-http-body-as-utf8 ()
  (let* ((edn "{:ok true :results [] :label \"象 — arrow →\"}")
         (raw (encode-coding-string edn 'utf-8))
         (payload (parseedn-read-str (decode-coding-string raw 'utf-8))))
    (should (equal (gethash :label payload) "象 — arrow →"))))

;;; evidence-landscape-search-test.el ends here
