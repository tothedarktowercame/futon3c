(ns futon3c.diagramprover.wm-flight-wiring-test
  "The machine's component map (holes/labs/M-wm-wiring/wm-flight-wiring.edn)
  checked against the files of each repo at the map's :repos shas,
  materialised from git under a per-repo prefix into a temp root, so the
  repos moving on does not move the check. The report must EQUAL the map's
  :expected-findings: wiring a row means deleting its expected findings in the
  same commit as the code. A :not-built box has no :site; the test asserts its
  :intended-site is absent at the pinned sha, so building it fails the test
  until the box is switched to a real site. The exemplar trace is checked:
  every step names a box, each field-connected pair shares a field written by
  the first and read by the second, and every built box on it resolves in a
  registered load closure."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wiring :as wiring])
  (:import [java.security MessageDigest]))

(def map-path "holes/labs/M-wm-wiring/wm-flight-wiring.edn")
(def closure-path "test/futon3c/diagramprover/fixtures/load-closure@test-registry-307b8969.edn")

;; the pins: the map's bytes and the repo shas its sites were drawn against
(def map-sha256 "2f7df0ce798c97a91cccaaa6534ff203958ba0ec48dd053d17ed6e52f1796014")
(def repos {"futon2" "5c6acc21" "futon3c" "58f1a8cb"})

(defn- sha256 [path]
  (let [d (.digest (MessageDigest/getInstance "SHA-256")
                   (java.nio.file.Files/readAllBytes (.toPath (io/file path))))]
    (apply str (map #(format "%02x" %) d))))

(defn- spec [] (edn/read-string (slurp map-path)))

(defn- materialise
  "Copy every site and intended-site file that exists at its repo's sha into
  ROOT/<repo>/<path>. A file absent at the sha stays absent."
  [s]
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "wm-flight-wiring" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (doseq [f (distinct (keep #(:file (or (:site %) (:intended-site %))) (:boxes s)))
            :let [[repo path] (str/split f #"/" 2)
                  {:keys [exit out]} (sh/sh "git" "-C" (str "/home/joe/code/" repo) "show"
                                            (str (get (:repos s) repo) ":" path))]
            :when (zero? exit)]
      (io/make-parents (io/file root f))
      (spit (io/file root f) out))
    (str root)))

(defn- comparable [f]
  (cond-> (dissoc f :rows :kind :note)
    (contains? f :error) (-> (dissoc :error) (assoc :error? true))))

(defn report [root s]
  (let [g (wiring/ingest s)]
    (mapv comparable
          (concat (wiring/written-never-read g) (wiring/read-never-written g)
                  (wiring/multiply-written g)
                  (wiring/conformance root s {:heuristic? true})))))

(defn not-built-present
  "The :not-built boxes whose intended file and var EXIST under ROOT."
  [root s]
  (vec (for [b (:boxes s)
             :when (= :not-built (:status b))
             :let [{:keys [file var]} (:intended-site b)
                   f (io/file root file)]
             :when (and (.isFile f) (or (nil? var) (wiring/var-form (slurp f) var)))]
         (:box/id b))))

(defn trace-findings
  "Findings for TRACE over S's boxes: an unknown box, or a field-connected
  step whose box reads nothing the previous box writes."
  [s {:keys [boxes]}]
  (let [by-id (into {} (map (juxt :box/id identity)) (:boxes s))
        steps (mapv #(if (map? %) % {:box %}) boxes)]
    (vec (concat
          (for [{:keys [box]} steps :when (not (by-id box))]
            {:finding :trace-box-unknown :box box})
          (for [[a b] (partition 2 1 steps)
                :when (and (by-id (:box a)) (by-id (:box b)) (not= :positional (:hop b)))
                ;; compare vertices, not raw entries: [field {:record r}]
                ;; is the vertex [field r] (WM-PROVER-RECORD-SCOPE-I)
                :when (empty? (filter (set (map wiring/vertex-key (:writes (by-id (:box a)))))
                                      (map wiring/vertex-key (:reads (by-id (:box b))))))]
            {:finding :trace-not-connected :from (:box a) :to (:box b)})))))

(defn trace-gaps [s {:keys [boxes]}]
  (let [nb (set (map :box/id (filter #(= :not-built (:status %)) (:boxes s))))]
    (vec (distinct (filter nb (map #(if (map? %) (:box %) %) boxes))))))

(deftest the-pins-hold
  (is (= map-sha256 (sha256 map-path)) "the map changed: re-pin with the change that changed it")
  (is (= repos (:repos (spec)))))

(deftest the-report-equals-the-declared-expected-findings
  (let [s (spec)
        got (report (materialise s) s)
        want (mapv comparable (:expected-findings s))]
    (is (= (set want) (set got))
        (pr-str {:unexpected (remove (set want) got) :missing (remove (set got) want)}))
    (is (= (count want) (count got)) "no finding reported twice")))

(deftest one-authority-per-field
  (is (= [] (wiring/multiply-written (wiring/ingest (spec))))))

(deftest not-built-boxes-are-absent-at-the-pinned-shas
  ;; the outer cascade was the last :not-built box; select is built (H-T-CALLER-I,
  ;; futon2 b66369d3). The check stays: it must still report a :not-built box
  ;; whose intended var exists.
  (let [s (spec) root (materialise s)]
    (is (= 0 (count (filter #(= :not-built (:status %)) (:boxes s)))) "nothing is :not-built on the map")
    (is (= [] (not-built-present root s)))
    (testing "planted: a :not-built box whose intended var exists is reported"
      (let [ghost {:box/id :ghost :status :not-built
                   :intended-site {:file "futon2/src/futon2/aif/outer_cascade.clj" :var "select"}}]
        (is (= [:ghost] (not-built-present root (update s :boxes conj ghost))))))))

(deftest the-exemplar-trace
  (let [s (spec) t (first (:traces s))]
    (is (= "M-autoclock-in" (:target t)))
    (is (= [] (trace-findings s t)))
    (is (= 5 (count (filter #(= :positional (:hop %)) (filter map? (:boxes t)))))
        "positional hops, checked only as declared")
    (is (= [] (trace-gaps s t)) "no unbuilt box on the trace: the outer cascade is built")
    (testing "planted: an unknown box and a non-adjacent pair are caught"
      (is (= [{:finding :trace-box-unknown :box :no-such-box}]
             (trace-findings s (update t :boxes conj :no-such-box))))
      (is (= [{:finding :trace-not-connected :from :loop-entry :to :flight-entry}]
             (trace-findings s (update t :boxes #(into [(first %)] (drop 2 %)))))))))

(deftest trace-findings-compare-vertices
  ;; WM-HANDOFF-H8-I: a scoped write connects to the same scoped read, and
  ;; not to the unscoped field of the same name (the click's :wants)
  (let [s {:boxes [{:box/id :w :writes [[:wants {:record :sources}]]}
                   {:box/id :r-scoped :reads [[:wants {:record :sources}]]}
                   {:box/id :r-plain :reads [:wants]}]}]
    (is (= [] (trace-findings s {:boxes [:w :r-scoped]})))
    (is (= [{:finding :trace-not-connected :from :w :to :r-plain}]
           (trace-findings s {:boxes [:w :r-plain]})))))

(def expected-outside-closure
  ;; built component sites not in the load closure of test-registry-307b8969
  ;; (mission-reading-c8-test, futon2 d5320918)
  #{"futon2/scripts/futon2/wm/extract_outcomes.clj"
    "futon2/scripts/wm_scheduled_run.clj"
    "futon2/src/futon2/aif/enactment_habit.clj"
    ;; WM-HABIT-FOLD-CALL-I: the tick's reader of the flights' receipts,
    ;; written after that closure was registered
    "futon2/src/futon2/aif/enactment_fold_source.clj"
    "futon2/src/futon2/aif/flight_driver.clj"
    "futon2/src/futon2/aif/grain_gate.clj"
    ;; H-T-CALLER-I: the outer cascade's select and the loop entry's plan
    ;; mode, written after that closure was registered
    "futon2/src/futon2/aif/outer_cascade.clj"
    "futon2/src/futon2/aif/outer_loop.clj"
    "futon2/src/futon2/aif/served_by_reading.clj"
    "futon2/src/futon2/aif/target_field.clj"
    "futon3c/src/futon3c/agency/clock_lineage.clj"
    ;; WM-CAST-I: the click endpoint, :click-start (futon3c, not futon2)
    "futon3c/src/futon3c/transport/http.clj"})

(deftest component-sites-against-a-registered-load-closure
  (let [s (spec)
        root (materialise s)
        {:keys [load-closure] :as fx} (edn/read-string (slurp closure-path))
        components {:boxes (filterv #(and (= :component (:box/kind %)) (:site %)) (:boxes s))}
        findings (wiring/load-closure-findings root components load-closure
                                               {:closure-root (str root "/futon2")})
        rel (fn [p] (subs p (inc (count root))))
        t (first (:traces s))
        on-trace (set (map #(if (map? %) (:box %) %) (:boxes t)))
        trace-sites (set (keep #(when (on-trace (:box/id %)) (some-> (:site %) :file)) (:boxes s)))]
    (is (str/starts-with? (:evidence/id fx) "test-registry-307b8969"))
    (is (every? #(= :site-not-in-load-closure (:finding %)) findings))
    (is (= expected-outside-closure (set (map (comp rel :path) findings))))
    (testing "built boxes on the trace outside the closure (stated)"
      (is (= (set (filter trace-sites (map (comp rel :path) findings)))
             (set (filter trace-sites expected-outside-closure)))))
    (is (false? (wiring/sites-resolve? root components load-closure {:closure-root (str root "/futon2")}))
        "N0 does not hold yet for the whole map: stated, not hidden")))
