;;; xiaoxiang-animal-test.el --- C-c l, the REPL Circus animal menu -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'xiaoxiang-preview)

(ert-deftest xiaoxiang-animal-keys-cover-the-circus ()
  ;; The animals of M-repl-circus's table, each on its own key; Joe's two
  ;; examples (x=xiang, m=ma) as stated.
  (let ((keys (mapcar #'car xiaoxiang-animal-keys))
        (texts (mapcar #'cadr xiaoxiang-animal-keys)))
    (should (equal keys (cl-remove-duplicates keys :test #'equal)))
    (dolist (a '("驴" "象" "狗" "馬" "猴" "Giraffe"))
      (should (member a texts)))
    (should (equal "象" (cadr (assoc "x" xiaoxiang-animal-keys))))
    (should (equal "馬" (cadr (assoc "m" xiaoxiang-animal-keys))))
    (should-not (assoc "q" xiaoxiang-animal-keys))))

(ert-deftest xiaoxiang-animal-hydra-inserts-the-animal ()
  (skip-unless (require 'hydra nil t))
  (with-temp-buffer
    (cl-letf (((symbol-function 'xiaoxiang-animal-hydra/body) #'ignore))
      (xiaoxiang-insert-animal))
    ;; Each head inserts its animal at point.
    (dolist (k xiaoxiang-animal-keys)
      (erase-buffer)
      (funcall (intern (format "xiaoxiang-animal-hydra/lambda-%s-and-exit" (car k))))
      (should (equal (buffer-string) (cadr k))))))

(ert-deftest xiaoxiang-animal-without-hydra-uses-completion ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'fboundp)
               (let ((orig (symbol-function 'fboundp)))
                 (lambda (f) (and (not (eq f 'defhydra)) (funcall orig f)))))
              ((symbol-function 'require)
               (let ((orig (symbol-function 'require)))
                 (lambda (feature &rest args)
                   (unless (eq feature 'hydra) (apply orig feature args)))))
              ((symbol-function 'completing-read)
               (lambda (_prompt choices &rest _)
                 (car (cl-find-if (lambda (c) (string-prefix-p "猴" (car c))) choices)))))
      (xiaoxiang-insert-animal))
    (should (equal (buffer-string) "猴"))))

(provide 'xiaoxiang-animal-test)
;;; xiaoxiang-animal-test.el ends here
