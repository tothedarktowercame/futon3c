(ns futon3c.diagramprover.wm-wire-failure-products-15c
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon2.aif.full-loop-runner :as runner]
            [futon2.aif.repair-obligation :as repair]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-r9-support :as support]))

(defn typed-exception [field cause]
  (case field
    :failure-kind (@#'runner/phase-kind-failure
                   (ex-info "phase failure" {:kind :invalid-temperature} cause))
    :outcome (let [e (ex-info "cascade refused" {:kind :live-c-stale :target "M-t"} cause)]
               (@#'runner/judge-refusal-abstention (runner/judge-refusal e "M-t") e))))

(defn precedence [field]
  (let [transport (java.net.ConnectException. "fixture transport")
        a (Exception. "untyped wrapper" (typed-exception field nil))
        b (Exception. "untyped wrapper" (typed-exception field transport))
        c (Exception. "untyped wrapper" transport)
        ;; Additional control: first typed entry wins over a later typed entry.
        d (ex-info "outer kind" {field :outer-kind} (typed-exception field transport))]
    (mapv (fn [e] {:explicit (@#'runner/explicit-failure-kind e)
                   :classified (@#'runner/failure-kind-from e)}) [a b c d])))

(defn store-products []
  (let [dirs (atom []) make-dir w/tmp-dir]
    (try
      (with-redefs [w/tmp-dir (fn [prefix] (let [p (make-dir prefix)] (swap! dirs conj p) p))]
        (let [findings (mapv #(-> (ex-info "cascade refused" {:kind :live-c-stale :target "M-t"}
                                           (RuntimeException. %)) support/run-tick :finding)
                             ["beneath" "elsewhere"])
              values (mapv :failure-cause findings)
              inputs (mapv #(assoc (first findings) :failure-cause %) values)
              stored (mapv (fn [finding]
                             (let [root (w/tmp-dir "failure-products-15c")
                                   result (repair/record-system-failure! root finding)]
                               (edn/read-string
                                (slurp (io/file root "findings" (str (:repair/id result) ".edn")))))) inputs)]
          {:values values :inputs inputs :stored stored
           :read (mapv runner/finding-failure-cause stored)
           :missing (runner/finding-failure-cause (dissoc (first stored) :failure-cause))
           :classification (mapv #(select-keys % [:failure-kind :failure-outcome :repair/class]) stored)}))
      (finally
        (doseq [dir @dirs file (reverse (file-seq (io/file dir)))] (io/delete-file file true))))))
