(ns futon3c.diagramprover.r20-field-flow-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wiring :as wiring])
  (:import [java.security MessageDigest]))

(def map-path "holes/labs/M-wm-wiring/r20-field-flow.edn")
(def map-sha256 "4cfc9c6cf4e0099414a8c42ca2035d214bfc17c0bae2533756d9c4c626b9752b")
(def repos {"futon2" "b6d557aa073ca52939f64c0267754bf8286d84ed"})

(defn- sha256 [path]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256")
                        (java.nio.file.Files/readAllBytes (.toPath (io/file path))))]
    (apply str (map #(format "%02x" %) digest))))

(defn- spec [] (edn/read-string (slurp map-path)))

(defn- identity-declaration-errors [s]
  (vec (keep (fn [[field {:keys [status id-paths witness-when]}]]
               (cond
                 (and (= :present status) (not (map? id-paths)))
                 {:kind :present-missing-id-paths :field field}

                 (and (not= :present status) (some? id-paths))
                 {:kind :id-paths-on-non-present :field field :status status}

                 (and (= :present status) (not (map? witness-when)))
                 {:kind :present-missing-witness-condition :field field}))
             (:r20/field-identity s))))

(defn- producer-source [root s field]
  (let [box (first (filter #(some #{field} (:writes %)) (:boxes s)))]
    (slurp (io/file root (get-in box [:site :file])))))

(defn- model-usage-row-writes?
  "The pinned producer's returned `:jobs` row has these literal keys. This is
  deliberately a source witness, not an inference from a prose identity note."
  [source keys]
  (let [compact (str/replace source #"\s+" "")
        written {:run-id ":run-id(:run-identry)"
                 :click-id ":click-id(:click-identry)"
                 :status ":status(ifreason"}]
    (every? #(let [fragment (get written %)]
               (and (string? fragment) (str/includes? compact fragment)))
            keys)))

(defn- identity-source-errors [root s]
  (let [field [:jobs {:record :model-usage}]
        identity (get-in s [:r20/field-identity field])]
    (if (= :present (:status identity))
      (let [paths (:id-paths identity)
            leaves (set (map (comp keyword name last) (vals paths)))
            needed (conj leaves :status)]
        (if (model-usage-row-writes? (producer-source root s field) needed)
          []
          [{:kind :id-path-not-produced :field field :paths paths}]))
      [])))

(defn- identity-errors [root s]
  (vec (concat (identity-declaration-errors s)
               (identity-source-errors root s))))

(defn- materialise [s]
  (let [root (.toFile
              (java.nio.file.Files/createTempDirectory
               "r20-field-flow" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (doseq [file (distinct (keep #(get-in % [:site :file]) (:boxes s)))
            :let [[repo path] (str/split file #"/" 2)
                  {:keys [exit out err]}
                  (sh/sh "git" "-C" (str "/home/joe/code/" repo) "show"
                         (str (get (:repos s) repo) ":" path))]]
      (when-not (zero? exit)
        (throw (ex-info "pinned source unavailable"
                        {:repo repo :path path :error err})))
      (io/make-parents (io/file root file))
      (spit (io/file root file) out))
    (str root)))

(defn- comparable [finding]
  (cond-> finding
    (contains? finding :error) (-> (dissoc :error) (assoc :error? true))))

(defn report [root s]
  (let [g (wiring/ingest s)]
    (mapv comparable
          (concat (wiring/written-never-read g)
                  (wiring/read-never-written g)
                  (wiring/multiply-written g)
                  (wiring/conformance root s {:heuristic? true})))))

(deftest the-pins-hold
  (is (= map-sha256 (sha256 map-path)))
  (is (= repos (:repos (spec)))))

(deftest the-report-equals-the-declared-expected-findings
  (let [s (spec)
        got (report (materialise s) s)
        want (:expected-findings s)]
    (is (= (set want) (set got))
        (pr-str {:unexpected (remove (set want) got)
                 :missing (remove (set got) want)}))
    (is (= (count want) (count got)) "no finding reported twice")))

(deftest precision-retrospective-f-conforms-while-scorer-f-control-still-fails
  (let [s (spec)
        findings (report (materialise s) s)
        true-edge #{[:precision-retrospective-f [:f :precision-retrospective] :writes]
                    [:precision-carry-advance [:f :precision-retrospective] :reads]}
        changed (update s :boxes
                        (fn [boxes]
                          (mapv (fn [box]
                                  (if (= :precision-carry-advance (:box/id box))
                                    (update box :reads conj [:f-consumed {:record :phase8-scorer}])
                                    box))
                                boxes)))
        scorer-findings (report (materialise changed) changed)]
    (is (not-any? #(contains? true-edge [(:box/id %) (:field %) (:role %)]) findings)
        "the true retrospective-F write/read pair has site evidence")
    (is (= 15 (count (filter #(= :declaration-without-occurrence (:finding %)) findings)))
        "11 existing declaration findings plus four documented cross-function focus declarations = 15")
    (is (some #(and (= :declaration-without-occurrence (:finding %))
                    (= :precision-carry-advance (:box/id %))
                    (= [:f-consumed :phase8-scorer] (:field %)))
              scorer-findings)
        "the planted scorer-F read remains distinguishable from the true F edge")))

(deftest precision-family-g-read-disappears-when-the-pinned-selector-omits-g
  (let [s (spec)
        root (materialise s)
        file (io/file root "futon2/src/futon2/aif/policy_precision_carry.clj")
        source (slurp file)]
    (is (str/includes? source "(select-keys % [:id :g :habit])"))
    (spit file (str/replace source
                            "(select-keys % [:id :g :habit])"
                            "(select-keys % [:id :habit])"))
    (is (some #(and (= :declaration-without-occurrence (:finding %))
                    (= :precision-family (:box/id %))
                    (= [:g :selection-candidate] (:field %))
                    (= :reads (:role %)))
              (report root s))
        "the G read needs the selected candidate key at the family site")))

(deftest one-authority-per-field
  (is (= [] (wiring/multiply-written (wiring/ingest (spec))))))

(deftest field-metadata-covers-every-wire
  (let [s (spec)
        fields (set (mapcat #(concat (:reads %) (:writes %)) (:boxes s)))]
    (is (= fields (set (keys (:r20/field-types s)))))
    (is (= fields (set (keys (:r20/field-identity s)))))
    (is (every? #{:present :absent :unknown}
                (map :status (vals (:r20/field-identity s)))))))

(deftest present-run-identity-is-declared-and-pinned-to-its-producer
  (let [s (spec)
        root (materialise s)
        jobs [:jobs {:record :model-usage}]
        focus [:focus-receipt {:record :selection-certificate}]]
    (is (empty? (identity-errors root s))
        "the only present identity is a complete job-row run/click witness")
    (testing "(a) a present value without paths is refused"
      (is (= [{:kind :present-missing-id-paths :field jobs}]
             (identity-errors root (assoc-in s [:r20/field-identity jobs :id-paths] nil)))))
    (testing "(b) a declared leaf must be written by the pinned row literal"
      (is (= [{:kind :id-path-not-produced :field jobs
               :paths {:run [:jobs :* :execution-id] :click [:jobs :* :click-id]}}]
             (identity-errors root (assoc-in s [:r20/field-identity jobs :id-paths :run]
                                             [:jobs :* :execution-id])))))
    (testing "(c) absent values cannot claim identity paths"
      (is (= [{:kind :id-paths-on-non-present :field focus :status :absent}]
             (identity-errors root (assoc-in s [:r20/field-identity focus :id-paths]
                                             {:run [:run-id]})))))))

(deftest precision-carry-cannot-claim-the-scorers-f
  (let [s (spec)
        scorer-f [:f-consumed {:record :phase8-scorer}]
        scorer-vertex (wiring/vertex-key scorer-f)
        changed (update s :boxes
                        (fn [boxes]
                          (mapv (fn [box]
                                  (if (= :precision-carry-advance (:box/id box))
                                    (update box :reads conj scorer-f)
                                    box))
                                boxes)))
        findings (report (materialise changed) changed)]
    (testing "the planted wrong-F edge is named"
      (is (some #(and (= :declaration-without-occurrence (:finding %))
                      (= scorer-vertex (:field %))
                      (= :precision-carry-advance (:box/id %)))
                findings)
          (pr-str findings)))))

(deftest precision-carry-cannot-claim-the-class-preference-g
  (let [s (spec)
        scorer-g [:scoring {:record :selection-certificate}]
        scorer-vertex (wiring/vertex-key scorer-g)
        changed (update s :boxes
                        (fn [boxes]
                          (mapv (fn [box]
                                  (if (= :precision-carry-advance (:box/id box))
                                    (update box :reads conj scorer-g)
                                    box))
                                boxes)))
        findings (report (materialise changed) changed)]
    (is (some #(and (= :declaration-without-occurrence (:finding %))
                    (= scorer-vertex (:field %))
                    (= :precision-carry-advance (:box/id %)))
              findings)
        "the planted scorer-G read remains distinct from the frozen candidate G")))

(defn- constant-hole-call [node missing]
  (str "(constant-row " (pr-str node) " " (pr-str missing) ")"))

(defn- constant-holes-match?
  "Every pinned hole is a call in the receipt source, and the source has no
   constant-row call the map does not pin."
  [source s]
  (and (every? (fn [[node missing]]
                 (str/includes? source (constant-hole-call node missing)))
               (:r20/constant-holes s))
       (= (count (:r20/constant-holes s))
          (count (re-seq #"\(constant-row :" source)))))

(deftest constant-holes-are-pinned-to-receipt-calls
  (let [s (spec)
        source (slurp (io/file (materialise s)
                               "futon2/src/futon2/aif/wm/apparatus_certificates.clj"))]
    (is (constant-holes-match? source s))
    (is (not (constant-holes-match?
              source (assoc-in s [:r20/constant-holes :evidence] [:wrong])))
        "a changed literal hole vector is rejected by the pinned receipt source")
    (is (not (constant-holes-match?
              (str source "\n(constant-row :extra [:x])") s))
        "a constant hole added to receipt but not to the map is rejected")))
