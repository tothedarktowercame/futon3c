(ns futon3c.diagramprover.wm-flight-wiring-test
  "The M-wm-wiring component map (holes/labs/M-wm-wiring/wm-flight-wiring.edn)
  checked against futon2's files at the map's :futon2-sha, materialised from
  git into a temp root so futon2 moving on does not move the check. The
  report must EQUAL the map's :expected-findings: a finding that appears or
  disappears without the map changing fails, so wiring a row means deleting
  its expected findings in the same commit as the code."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wiring :as wiring])
  (:import [java.security MessageDigest]))

(def map-path "holes/labs/M-wm-wiring/wm-flight-wiring.edn")
(def closure-path "test/futon3c/diagramprover/fixtures/load-closure@test-registry-307b8969.edn")

;; (d) the pins: the map's bytes and the futon2 sha its sites were drawn against
(def map-sha256 "d5d1eee696f7c9f7197a954ae1d3427d82ef6de4a96e72fa6e95cc53e9706878")
(def futon2-sha "dd1c0561")

(defn- sha256 [path]
  (let [d (.digest (MessageDigest/getInstance "SHA-256")
                   (java.nio.file.Files/readAllBytes (.toPath (io/file path))))]
    (apply str (map #(format "%02x" %) d))))

(defn- site-file [{:keys [file ns]}]
  (or file (str "src/" (-> ns (str/replace "." "/") (str/replace "-" "_")) ".clj")))

(defn- materialise
  "Copy every site file that exists at SHA from futon2's git into a temp root.
  A site absent at SHA stays absent: that absence is a finding, not an error."
  [spec sha]
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "wm-flight-wiring" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (doseq [p (distinct (keep #(some-> (:site %) site-file) (:boxes spec)))
            :let [{:keys [exit out]} (sh/sh "git" "-C" "/home/joe/code/futon2" "show" (str sha ":" p))]
            :when (zero? exit)]
      (io/make-parents (io/file root p))
      (spit (io/file root p) out))
    (str root)))

(defn- comparable [f]
  (cond-> (dissoc f :rows :kind :note)
    (contains? f :error) (-> (dissoc :error) (assoc :error? true))))

(defn report [root spec]
  (let [g (wiring/ingest spec)]
    (mapv comparable
          (concat (wiring/written-never-read g) (wiring/read-never-written g)
                  (wiring/multiply-written g)
                  (wiring/conformance root spec {:heuristic? true})))))

(defn- spec [] (edn/read-string (slurp map-path)))

(deftest the-pins-hold
  (is (= map-sha256 (sha256 map-path)) "the map changed: re-pin with the change that changed it")
  (is (= futon2-sha (:futon2-sha (spec)))))

(deftest the-report-equals-the-declared-expected-findings
  (let [s (spec)
        root (materialise s futon2-sha)
        got (report root s)
        want (mapv comparable (:expected-findings s))]
    (is (= (set want) (set got))
        (pr-str {:unexpected (remove (set want) got) :missing (remove (set got) want)}))
    (is (= (count want) (count got)) "no finding reported twice")))

(deftest one-authority-per-field
  (is (= [] (wiring/multiply-written (wiring/ingest (spec))))))

(def expected-outside-closure
  ;; component sites not in the load closure of test-registry-307b8969
  ;; (mission-reading-c8-test at futon2 d5320918): files that postdate it,
  ;; the extractor script (load-filed, not required), and the unbuilt outer
  ;; cascade
  #{"scripts/wm/extract-outcomes.clj"
    "src/futon2/aif/enactment_habit.clj"
    "src/futon2/aif/grain_gate.clj"
    "src/futon2/aif/outer_cascade.clj"
    "src/futon2/aif/served_by_reading.clj"
    "src/futon2/aif/target_field.clj"})

(deftest component-sites-against-a-registered-load-closure
  (let [s (spec)
        root (materialise s futon2-sha)
        {:keys [load-closure] :as fx} (edn/read-string (slurp closure-path))
        components {:boxes (filterv #(= :component (:box/kind %)) (:boxes s))}
        findings (wiring/load-closure-findings root components load-closure)
        rel (fn [p] (subs p (inc (count root))))]
    (is (str/starts-with? (:evidence/id fx) "test-registry-307b8969"))
    (testing "the flight and tick sites the closure loads resolve (N0 for those rows)"
      (is (every? #(= :site-not-in-load-closure (:finding %)) findings))
      (is (= expected-outside-closure (set (map (comp rel :path) findings)))))
    (is (false? (wiring/sites-resolve? root components load-closure))
        "N0 does not hold yet for the whole map: stated, not hidden")))
