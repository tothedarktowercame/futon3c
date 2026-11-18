;;; arxana-design-catalogue-reazon-test.el --- Shared fixture agreement -*- lexical-binding: t; -*-
(require 'ert)
(require 'arxana-design-catalogue-reazon)

(defconst arxana-design-catalogue-test--fixture
  (expand-file-name "../../test/resources/arxana/design-catalogue-checks.json"
                    (file-name-directory (or load-file-name buffer-file-name))))
(defun arxana-design-catalogue-test--canonical (x)
  "Canonical JSON value, sorting object keys but preserving array order."
  (cond
   ((vectorp x) (vconcat (mapcar #'arxana-design-catalogue-test--canonical x)))
   ((consp x) (sort (mapcar (lambda (kv) (cons (car kv) (arxana-design-catalogue-test--canonical (cdr kv)))) x)
                   (lambda (a b) (string< (symbol-name (car a)) (symbol-name (car b))))))
   (t x)))
(defun arxana-design-catalogue-test--set (xs)
  (sort (delete-dups (mapcar (lambda (x) (prin1-to-string (arxana-design-catalogue-test--canonical x))) xs)) #'string<))

(ert-deftest arxana-design-catalogue-shared-core-logic-agreement ()
  (let ((data (arxana-design-catalogue-read-fixtures arxana-design-catalogue-test--fixture)))
    (should (equal "design-catalogue-ground-fixtures-v1" (alist-get 'format data)))
    (mapc
     (lambda (case)
       (ert-info ((alist-get 'id case))
         (let* ((actual (arxana-design-catalogue-check case)) (expected (alist-get 'expected case)))
           (should (equal (car actual) (alist-get 'result expected)))
           (should (equal (arxana-design-catalogue-test--set (cdr actual))
                          (arxana-design-catalogue-test--set (append (alist-get 'violations expected) nil)))))))
     (alist-get 'cases data))))

(ert-deftest arxana-design-catalogue-emacs-only-plant ()
  (let* ((case '((check . "coverage") (facts . ((documented . ["known"]) (citations . ["emacs-only-missing"])))))
         (actual (arxana-design-catalogue-check case)))
    (should (equal actual '("violation" "emacs-only-missing")))
    ;; Mutation: dropping the absence check hides the exact planted defect.
    ;; Assert that the normal 'this case passes' expectation really fails.
    (should-error (should (equal "pass" (car actual))) :type 'ert-test-failed)
    (cl-letf (((symbol-function 'arxana-design-catalogue--uncoveredo) (lambda (&rest _) #'reazon-!U)))
      (should-not (equal actual (arxana-design-catalogue-check case))))))

(provide 'arxana-design-catalogue-reazon-test)
;;; arxana-design-catalogue-reazon-test.el ends here
