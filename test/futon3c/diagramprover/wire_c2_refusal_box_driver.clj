;; WIRE-23-C2 refusal-box driver (a script, not a namespace).
;;
;; Run from the futon2 checkout:
;;   clojure -M:test /home/joe/code/futon3c/test/futon3c/diagramprover/wire_c2_refusal_box_driver.clj <gate|judge> <none|absent|different>
;;
;; The futon2 refusal-box namespaces cannot load from futon3c's cwd (the
;; require chain reaches learning-trial-test's load-time slurp of a
;; futon2-relative fixture, and the boxes' own live pins are futon2-relative),
;; so the futon3c wire test drives the box here, in futon2's JVM: the named
;; futon2 test var runs with the REAL war-machine/cascade-decision wrapped by
;; with-redefs (the writer's thrown ex-data tampered per MUTATION), and the
;; value the box consumed is read from its clojure.test report
;; (wm-wire-kernel-out-support/order-read's technique: the box's first
;; pass/fail report is the kind equality, its last form element the consumed
;; value). Prints one ":wire-result <edn>" line on stdout.
(require '[clojure.test :as t]
         '[futon2.report.war-machine :as wm]
         '[futon2.aif.gate-refusal-abstention-test :as gate-test]
         '[futon2.aif.judge-refusal-abstention-test :as judge-test])

(let [which (first *command-line-args*)
      mutation (second *command-line-args*)
      field (if (= which "judge") :kind :reason)
      test-var (if (= which "judge")
                 #'judge-test/the-real-judge-refusal-is-the-ticks-typed-abstention
                 #'gate-test/the-real-gate-refusal-is-the-ticks-typed-abstention)
      real wm/cascade-decision
      written (atom nil)
      reports (atom [])]
  (with-redefs [wm/cascade-decision
                (fn [& args]
                  (try
                    (apply real args)
                    (catch clojure.lang.ExceptionInfo e
                      (reset! written (get (ex-data e) field))
                      (throw (ex-info (ex-message e)
                                      (case mutation
                                        "none" (ex-data e)
                                        "absent" (dissoc (ex-data e) field)
                                        "different" (assoc (ex-data e) field
                                                           :different-refusal-kind)))))))
                t/report (fn [m] (swap! reports conj m))]
    (test-var))
  (let [report (first (filter #(#{:pass :fail} (:type %)) @reports))
        form (:actual report)
        equality (if (= 'not (first form)) (second form) form)]
    (println ":wire-result"
             (pr-str {:writer @written
                      :reader (last equality)
                      :report-type (:type report)}))))
