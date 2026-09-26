(ns futon3c.apm.qualification
  "Executable six-part behavioural bridge qualification."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.set :as set]
            [clojure.string :as str]
            [futon3c.apm.campaign-machine :as machine]))

(def qualification-test-root "test/futon3c/apm")

(defn qualification-test-namespaces
  "Derive APM test namespaces from checked-in source files."
  ([] (qualification-test-namespaces qualification-test-root))
  ([root]
   (->> (file-seq (io/file root))
        (filter #(.isFile %))
        (filter #(str/ends-with? (.getName %) "_test.clj"))
        (keep (fn [file]
                (some-> (re-find #"\(ns\s+([^\s\)]+)" (slurp file)) second)))
        sort vec)))

(defn with-derived-clojure-qualification
  "Replace the qualification command's namespace arguments with the complete
  on-disk inventory minus explicitly reasoned exclusions."
  [plan]
  (let [discovered (qualification-test-namespaces)
        exclusions (:clojure-qualification/exclusions plan)
        excluded (set (keys exclusions))
        included (remove excluded discovered)
        argv (into ["clojure" "-M:test"] (mapcat #(vector "-n" %) included))]
    (update plan :commands
            (fn [commands]
              (mapv (fn [command]
                      (if (= :clojure-qualification (:id command))
                        (assoc command :argv argv
                               :derived-namespace-count (count discovered)
                               :excluded-namespaces exclusions)
                        command))
                    commands)))))

(defn file-digest [path]
  (machine/ledger-digest [(slurp path)]))

(defn validate-plan [plan]
  (let [classes (set (keys (:mutation-classes plan)))
        required (:required-mutation-classes plan)
        holes (:residual-holes plan)
        discovered (set (qualification-test-namespaces))
        exclusions (:clojure-qualification/exclusions plan)
        excluded (set (keys exclusions))
        qualification-command (some #(when (= :clojure-qualification (:id %)) %)
                                    (:commands plan))
        declared (->> (:argv qualification-command)
                      (drop 2)
                      (partition 2)
                      (keep (fn [[flag value]] (when (= "-n" flag) value)))
                      set)
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
                   (conj :residual-hole-test-missing)
                   (not (and (map? exclusions)
                             (every? #(and (string? %) (not (str/blank? %)))
                                     (vals exclusions))))
                   (conj :qualification-exclusions-invalid)
                   (not (set/subset? excluded discovered))
                   (conj :qualification-exclusion-not-found)
                   (not= (set/difference discovered excluded) declared)
                   (conj :qualification-namespace-coverage-incomplete))]
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
  (let [report (qualify (with-derived-clojure-qualification
                         (edn/read-string (slurp plan-path))))]
    (spit report-path (str (pr-str report) "\n"))
    report))

(defn validate-report [report artifact-path]
  (let [observed (file-digest artifact-path)
        findings (cond-> []
                   (not (true? (:ok report))) (conj :qualification-not-passing)
                   (not= observed (get-in report [:generated-contract
                                                  :registered-digest]))
                   (conj :qualification-registered-artifact-stale)
                   (not= observed (get-in report [:generated-contract
                                                  :observed-digest]))
                   (conj :qualification-observed-artifact-stale)
                   (not (true? (get-in report [:non-vacuity :witnessed?])))
                   (conj :qualification-vacuous)
                   (not (every? (fn [[_ n]] (pos? n))
                                (:mutation-coverage report)))
                   (conj :qualification-mutation-coverage-empty)
                   (not= 3 (count (:residual-hole-tests report)))
                   (conj :qualification-residual-holes-unpinned)
                   (not (keyword? (:bounds-executable-test report)))
                   (conj :qualification-bounds-not-executable)
                   (not (every? #(and (:pass? %)
                                      (zero? (:command-own-exit %)))
                                (:gates report)))
                   (conj :qualification-gate-failed))]
    (if (seq findings)
      {:ok false :error/code :apm-qualification-report-invalid
       :findings findings :observed-digest observed}
      {:ok true :qualification/id (:qualification/id report)
       :artifact-digest observed})))
