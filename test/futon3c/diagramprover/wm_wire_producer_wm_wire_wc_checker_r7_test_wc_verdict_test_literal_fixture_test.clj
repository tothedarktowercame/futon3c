(ns futon3c.diagramprover.wm-wire-producer-wm-wire-wc-checker-r7-test-wc-verdict-test-literal-fixture-test
  "Producer for the [:wc-checker :r7-test :wc-verdict] wire. Runs the named
  futon2 test futon2.aif.selection-reads-fold-test/real-checker-verdict-into-increment
  with capture wrapped around the real checker-verdict (bb W_c over the two
  pinned exemplars) and the real enactment-habit/increment, and asserts the
  observation equals the committed record. The requisition named operation
  'cascade-decision' and inputs '.../literal-fixture'; neither exists: the
  operation producing the checked values is the one above, driven over
  fold-test's two pinned exemplars (click-001.edn, click-001-enactment.edn)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :as t :refer [deftest is testing]]
            [futon2.aif.enactment-habit :as eh]
            [futon2.aif.selection-reads-fold-test :as fold-test]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-selection-out-support :as support])
  (:import [java.security MessageDigest]))

(def producer 'futon3c.diagramprover.wm-wire-producer-wm-wire-wc-checker-r7-test-wc-verdict-test-literal-fixture-test)
(def operation 'futon2.aif.selection-reads-fold-test/real-checker-verdict-into-increment)
(def stem "wm-wire-wc-checker-r7-test-wc-verdict-test-literal-fixture")
(def wire-id [:wc-checker :r7-test :wc-verdict])
(def mutations [:absent :different])

(defn- slim-reports [reports]
  {:count (count reports)
   :bad-count (count (filter #(#{:fail :error} (:type %)) reports))
   :some-fail? (boolean (some #(= :fail (:type %)) reports))})

(defn- obs-entry [o]
  {:writer (:writer o)
   :reader (:reader o)
   :received? (w/received? o)
   :result-delta (get-in o [:result :delta])
   :result-wc-failures (get-in o [:result :wc-failures])})

(defn- observe [mutation]
  (let [real-checker fold-test/checker-verdict real-increment eh/increment
        printed (atom nil) observed (atom []) reports (atom [])]
    (with-redefs [fold-test/checker-verdict
                  (fn [record enactment]
                    (let [verdict (real-checker record enactment)]
                      (reset! printed verdict)
                      verdict))
                  eh/increment
                  (fn [enactment identity verdict]
                    (let [received (case mutation :none verdict :absent {:status :absent}
                                         :different ["different-checker-verdict"])
                          result (real-increment enactment identity received)]
                      (swap! observed conj {:writer @printed :reader received :result result})
                      result))
                  t/report #(swap! reports conj %)]
      (fold-test/real-checker-verdict-into-increment))
    (let [o (assoc (first @observed) :observations @observed :reports @reports)]
      {:writer (:writer o)
       :reader (:reader o)
       :received? (w/received? o)
       :result-delta (get-in o [:result :delta])
       :result-wc-failures (get-in o [:result :wc-failures])
       :observations (mapv obs-entry (:observations o))
       :reports (slim-reports (:reports o))})))

(defn build-record []
  {:producer producer
   :operation operation
   :inputs {:named-test operation
            :checker 'futon2.aif.selection-reads-fold-test/checker-verdict
            :increment 'futon2.aif.enactment-habit/increment
            :pinned-exemplars ["click-001.edn" "click-001-enactment.edn"]
            :wire-id wire-id :mutations (into [:none] mutations)}
   :wires {wire-id {:primary (observe :none)
                    :interventions (into {} (map (juxt identity observe) mutations))}}
   :live-records-read support/live-records-read
   :left-out {:full-report-forms "readers check only the report count, the fail/error count, and whether any :fail report appears"
              :full-increment-results "readers check only each result's :delta and :wc-failures"
              :live-record-verification "readers check nothing about the live pins; the entries are carried only for the wire map's :live-records-read"
              :temporary-paths "the checker's temporary carrier directory differs per run; no recorded value contains a path"}})

(defn- record-text [record] (str (pr-str record) "\n"))
(defn- sha256 [text]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256") (.getBytes text "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))
(defn- fixture-files []
  (filter #(.startsWith (.getName %) (str stem "@"))
          (.listFiles (io/file "test/fixtures/wire-producers"))))
(defn- write-record! [record]
  (let [text (record-text record) sha (sha256 text)
        file (io/file "test/fixtures/wire-producers"
                      (str stem "@" (subs sha 0 12) ".edn"))]
    (.mkdirs (.getParentFile file))
    (when (.exists file)
      (throw (ex-info "producer record already exists" {:file (str file)})))
    (spit file text)
    (println (.getPath file))))
(defn- leaf-paths [value]
  (letfn [(walk [path x]
            (if (map? x) (mapcat (fn [[k v]] (walk (conj path k) v)) x) [path]))]
    (walk [] value)))

(deftest wc-verdict-producer
  (let [actual (build-record)]
    (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE"))
      (write-record! actual)
      (let [files (fixture-files)]
        (is (= 1 (count files)) "exactly one immutable wc-verdict record")
        (let [expected (edn/read-string (slurp (first files)))]
          (doseq [path (leaf-paths (:wires expected))]
            (testing (pr-str path)
              (is (= (get-in expected (into [:wires] path))
                     (get-in actual (into [:wires] path)))))))))))
