;;; arxana-design-catalogue-reazon.el --- Ground catalogue checks -*- lexical-binding: t; -*-

;; Mirrors the JSON projection of design-catalogue's core.logic checks.
;; This is a fixture/snapshot interpreter, not a store client or a view.
(require 'cl-lib)
(require 'json)
(require 'reazon)

(reazon-defrel arxana-design-catalogue--membero (x xs)
  "Relate X to an element of finite proper list XS."
  (reazon-fresh (head tail)
    (reazon-conso head tail xs)
    (reazon-conde
     ((reazon-== head x))
     ((arxana-design-catalogue--membero x tail)))))

(reazon-defrel arxana-design-catalogue--absento (x xs)
  "Ground negation as failure, on finite XS only.
All callers bind X before this goal.  Committed choice fails if membership
succeeds and succeeds otherwise, matching core.logic nafc on these inputs."
  (reazon-conda
   ((arxana-design-catalogue--membero x xs) #'reazon-!U)
   (#'reazon-!S)))

(reazon-defrel arxana-design-catalogue--uncoveredo (feature documented citations)
  "FEATURE is cited but undocumented."
  (arxana-design-catalogue--membero feature citations)
  (arxana-design-catalogue--absento feature documented))

(reazon-defrel arxana-design-catalogue--missing-nameo (id rules valid)
  "ID has a qualname absent from the list of nonempty string names."
  (reazon-fresh (name)
    (arxana-design-catalogue--membero (list id name) rules)
    (arxana-design-catalogue--absento name valid)))

(reazon-defrel arxana-design-catalogue--duplicate-nameo (a b name rules)
  "Distinct rule ids A and B name NAME; both orders are retained."
  (arxana-design-catalogue--membero (list a name) rules)
  (arxana-design-catalogue--membero (list b name) rules)
  ;; Both ids are now ground: this is the ground case of core.logic !=.
  (reazon-project (a b)
    (if (equal a b) #'reazon-!U #'reazon-!S)))

(reazon-defrel arxana-design-catalogue--bad-porto (id role target links choices checks rulings refs)
  "A catalogue link ID has ROLE pointing outside its allowed target set."
  (reazon-fresh (type from to)
    (arxana-design-catalogue--membero (list id type from to) links)
    (reazon-conde
     ((arxana-design-catalogue--absento from choices)
      (reazon-== role "choice") (reazon-== target from))
     ((reazon-== type "design/checked-by")
      (arxana-design-catalogue--absento to checks)
      (reazon-== role "check") (reazon-== target to))
     ((reazon-== type "design/shaped-by")
      (arxana-design-catalogue--absento to rulings)
      (reazon-== role "ruling") (reazon-== target to))
     ((reazon-== type "design/constrains")
      (arxana-design-catalogue--absento to refs)
      (reazon-== role "target") (reazon-== target to)))))

(defun arxana-design-catalogue-read-fixtures (path)
  "Read PATH without conflating JSON null, false and empty arrays.
Object keys are symbols; arrays are vectors; enum values remain strings."
  (json-parse-string (with-temp-buffer (insert-file-contents path) (buffer-string))
                     :object-type 'alist :array-type 'array
                     :null-object :json-null :false-object :json-false))

(defun arxana-design-catalogue--list (array)
  "Turn an outer fact ARRAY into a proper list; keep nested values typed."
  (append array nil))
(defun arxana-design-catalogue--ground (value)
  "Encode JSON arrays as tagged lists: Reazon treats every vector as a variable.
The tag also keeps an empty array distinct from JSON null."
  (cond ((vectorp value) (cons :json-array (mapcar #'arxana-design-catalogue--ground value)))
        ((consp value) (cons :json-object
                            (sort (mapcar (lambda (kv) (cons (car kv) (arxana-design-catalogue--ground (cdr kv)))) value)
                                  (lambda (a b) (string< (symbol-name (car a)) (symbol-name (car b)))))))
        (t value)))
(defun arxana-design-catalogue--json (value)
  "Decode a ground literal after the relation has finished."
  (cond ((eq (car-safe value) :json-array) (vconcat (mapcar #'arxana-design-catalogue--json (cdr value))))
        ((eq (car-safe value) :json-object)
         (mapcar (lambda (kv) (cons (car kv) (arxana-design-catalogue--json (cdr kv)))) (cdr value)))
        (t value)))

(defun arxana-design-catalogue--rows (array)
  "Convert fact rows to proper lists without altering their values."
  (mapcar (lambda (row) (mapcar #'arxana-design-catalogue--ground row))
          (arxana-design-catalogue--list array)))

(defun arxana-design-catalogue-check (case)
  "Return (RESULT . VIOLATIONS) for a shared ground fixture CASE.
RESULT is the JSON enum string.  Violations are strings or symbol-keyed
alists, matching the core.logic JSON result modulo order and duplicates.
Unknown checks and Reazon errors propagate, never becoming a passing result."
  (let* ((reazon-timeout nil) ; finite ground facts; never accept a partial timeout result
         (facts (alist-get 'facts case))
         (violations
          (pcase (alist-get 'check case)
            ("coverage"
             (let ((documented (arxana-design-catalogue--list (alist-get 'documented facts)))
                   (citations (arxana-design-catalogue--list (alist-get 'citations facts))))
               (reazon-run* feature
                 (arxana-design-catalogue--uncoveredo feature documented citations))))
            ("rule-function"
             (let* ((rules (arxana-design-catalogue--rows (alist-get 'rules facts)))
                    (valid (cl-remove-if-not (lambda (x) (and (stringp x) (> (length x) 0)))
                                             (mapcar #'cadr rules))))
               (append
                (mapcar (lambda (id) `((rule . ,id) (reason . "missing-qualname")))
                        (reazon-run* id
                          (arxana-design-catalogue--missing-nameo id rules valid)))
                (mapcar (lambda (row) `((rule-a . ,(nth 0 row)) (rule-b . ,(nth 1 row))
                                       (qualname . ,(arxana-design-catalogue--json (nth 2 row)))))
                        (reazon-run* q
                          (reazon-fresh (a b name)
                            (arxana-design-catalogue--duplicate-nameo a b name rules)
                            (reazon-== q (list a b name))))))))
            ("links"
             (let ((links (arxana-design-catalogue--rows (alist-get 'links facts)))
                   (choices (arxana-design-catalogue--list (alist-get 'choices facts)))
                   (checks (arxana-design-catalogue--list (alist-get 'checks facts)))
                   (rulings (arxana-design-catalogue--list (alist-get 'rulings facts)))
                   (refs (arxana-design-catalogue--list (alist-get 'refs facts))))
               (mapcar (lambda (row) `((link . ,(nth 0 row)) (role . ,(nth 1 row))
                                      (target . ,(nth 2 row))))
                       (reazon-run* q
                         (reazon-fresh (id role target)
                           (arxana-design-catalogue--bad-porto id role target links choices checks rulings refs)
                           (reazon-== q (list id role target)))))))
            (_ (error "Unknown catalogue check: %S" (alist-get 'check case))))))
    (setq violations (cl-remove-duplicates violations :test #'equal))
    (cons (if violations "violation" "pass") violations)))

(provide 'arxana-design-catalogue-reazon)
;;; arxana-design-catalogue-reazon.el ends here
