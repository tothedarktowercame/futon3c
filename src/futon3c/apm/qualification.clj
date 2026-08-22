(ns futon3c.apm.qualification
  "Executable six-part behavioural bridge qualification."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [futon3c.apm.campaign-machine :as machine]))

(defn file-digest [path]
  (machine/ledger-digest [(slurp path)]))

(defn validate-plan [plan]
  (let [classes (set (keys (:mutation-classes plan)))
        required (:required-mutation-classes plan)
        holes (:residual-holes plan)
        findings (cond-> []
                   (empty? (:positive-fixtures plan)) (conj :vacuous-positive-set)
                   (not= required classes) (conj :mutation-class-coverage-incomplete)
                   (some (comp empty? val) (:mutation-classes plan))
                   (conj :mutation-class-unwitnessed)
                   (not (keyword? (:bounds-executable-test plan)))
                   (conj :numeric-bounds-not-executable)
                   (not= (:generated-contract-digest plan)
                         (file-digest (:generated-contract plan)))
                   (conj :generated-artifact-stale)
                   (some #(not (and (.isFile (io/file (:path %)))
                                    (keyword? (:test-id %)))) holes)
                   (conj :residual-hole-test-missing))]
    (if (seq findings)
      {:ok false :error/code :qualification-plan-invalid :findings findings}
      {:ok true :plan plan})))

(defn qualify
  ([plan] (qualify plan #(apply shell/sh %)))
  ([plan command-fn]
   (let [validated (validate-plan plan)]
     (if-not (:ok validated)
       validated
       (let [artifact (:generated-contract plan)
             registered-digest (file-digest artifact)
             results (mapv (fn [{:keys [id argv]}]
                             (let [result (command-fn argv)]
                               {:gate/id id :command-own-exit (:exit result)
                                :pass? (zero? (:exit result))}))
                           (:commands plan))
             observed-digest (file-digest artifact)
             digest-match? (= registered-digest observed-digest)
             pass? (and digest-match? (every? :pass? results))]
         {:ok pass? :qualification/id (:qualification/id plan)
          :generated-contract {:path artifact :registered-digest registered-digest
                               :observed-digest observed-digest
                               :digest-match? digest-match?}
          :non-vacuity {:positive-witness-count (count (:positive-fixtures plan))
                        :witnessed? (pos? (count (:positive-fixtures plan)))}
          :mutation-coverage (into {} (map (fn [[k v]] [k (count v)]))
                                   (:mutation-classes plan))
          :residual-hole-tests (mapv #(select-keys % [:path :test-id])
                                     (:residual-holes plan))
          :bounds-executable-test (:bounds-executable-test plan)
          :gates results})))))

(defn run-qualification! [plan-path report-path]
  (let [report (qualify (edn/read-string (slurp plan-path)))]
    (spit report-path (str (pr-str report) "\n"))
    report))
