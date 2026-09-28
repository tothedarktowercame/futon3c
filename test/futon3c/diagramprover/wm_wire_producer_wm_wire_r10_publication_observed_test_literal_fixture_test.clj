(ns futon3c.diagramprover.wm-wire-producer-wm-wire-r10-publication-observed-test-literal-fixture-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon2.aif.flight :as flight]
            [futon2.aif.flight-runner :as fr]
            [futon3c.diagramprover.wm-wire :as w])
  (:import [java.security MessageDigest]))

(def producer 'futon3c.diagramprover.wm-wire-producer-wm-wire-r10-publication-observed-test-literal-fixture-test)
;; The packet named the operation "flight/run!" with inputs
;; wm-wire-r10-publication-observed-test/literal-fixture. The source has no
;; literal-fixture: the reader's support is its own observe, whose writer
;; end calls fr/observe-publication-fn and whose reader end is the
;; :publication-observed of the enactment record written by a real
;; one-click flight/run! with fr/enact-fn. The record names what the
;; source says.
(def operation ['futon2.aif.flight-runner/observe-publication-fn
                'futon2.aif.flight/run!])

(def run-record
  {:repair/publication [{:status :receipt-committed :repair/id "occ-published" :repair/discharged? true}
                        {:status :publication-refused :repair/id "occ-refused" :reason :publication-error}]})

(def refused-record
  {:repair/publication [{:status :publication-refused :repair/id "occ-published" :reason :publication-error}]})

(defn- writer-value
  "observe-publication-fn's value over RECORD for REPAIR-ID."
  [repair-id record]
  (:publication-observed
   ((fr/observe-publication-fn {:fetch-run-record (fn [_] record)
                                :repair-id-fn (constantly repair-id)})
    {:target "T-repair-x"} {:click-id "run-p"})))

(defn- enactment-record
  "The enactment record enact-fn writes over RECORD for REPAIR-ID, through
  a real one-click flight (publication_observed_test's one-authority run)."
  [repair-id record]
  (let [enact (fr/enact-fn {:dispatch-step! (fn [_] {:commit "c" :produced :t :check {:class :fixture}})
                            :check-fn (constantly {:observed true})
                            :interpretations (constantly {:p/a {:produces #{:t}}})
                            :fetch-run-record (constantly record)
                            :repair-id-fn (constantly repair-id)
                            :record-dir (w/tmp-dir "wire-pub")})
        f (flight/run! (flight/start {:target "T-repair-x" :chosen-because {:kind :requested}}
                                     {:kind :a-exits :repo "futon2" :path "p" :read-text (fn [& _] "")}
                                     {:id "flight-wire-pub"})
                       {:click-fn (constantly {:click-id "run-p" :chosen {:candidate :cand/p :precedence [:p/a]}})
                        :enact-fn enact
                        :observe-fn (fn [_ _] {})
                        :sources-fn (constantly {})
                        :max-clicks 1})]
    (edn/read-string (slurp (:record-path (first (:enactments f)))))))

(defn- observe
  "The writer's observation over WRITER-RECORD, and the reader's read of it
  off the enactment record written over READER-RECORD: {:writer :reader}."
  [repair-id writer-record reader-record]
  {:writer (writer-value repair-id writer-record)
   :reader (:publication-observed (enactment-record repair-id reader-record))})

(def live-records-read
  [{:path (str w/spike-dir "/flight-278b6988.edn")
    :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"
    :why "its one enactment is {:absent :no-dispatch-configured}: no enactment record was written, so no :publication-observed on either end"}
   {:path "holes/labs/M-futon-seams/exemplar/click-001-enactment.edn"
    :sha256 "e51063896e2a42096718d902e0b4dfe0e4652323de0b42848c2c0cf318bf6c89"
    :why "the hand-authored exemplar enactment (claude-10, 2026-09-24) predates H-publish: it carries no :publication-observed"}])

(def case-inputs
  {:committed {:repair-id "occ-published" :writer-record run-record :reader-record run-record}
   :no-repair {:repair-id nil :writer-record run-record :reader-record run-record}
   :refused {:repair-id "occ-published" :writer-record run-record :reader-record refused-record}})

(defn- case-fields [{:keys [repair-id writer-record reader-record]}]
  (observe repair-id writer-record reader-record))

(defn build-record []
  {:producer producer
   :operation operation
   :inputs {:support 'futon3c.diagramprover.wm-wire-r10-publication-observed-test/observe
            :run-record run-record
            :refused-record refused-record
            :cases (into {} (map (fn [[k v]] [k (select-keys v [:repair-id])])) case-inputs)}
   :live-records-read live-records-read
   :cases (into {} (map (juxt identity (comp case-fields case-inputs))) [:committed :no-repair :refused])
   :left-out {:enactment-record-other-keys "the enactment record enact-fn writes also carries dispatch, check and click fields; the reader checks only its :publication-observed"
              :record-dir "enact-fn writes into a per-run temporary directory (w/tmp-dir); the path differs from run to run and no reader checks it"
              :flight-record "the one-click flight record itself is not read back; only the enactment record's :publication-observed is checked"}})

(defn- record-text [record] (str (pr-str record) "\n"))
(defn- sha256 [text]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256") (.getBytes text "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))
(defn- fixture-files []
  (filter #(.startsWith (.getName %) "wm-wire-r10-publication-observed-test-literal-fixture@")
          (.listFiles (io/file "test/fixtures/wire-producers"))))
(defn- write-record! [record]
  (let [text (record-text record) sha (sha256 text)
        file (io/file "test/fixtures/wire-producers"
                      (str "wm-wire-r10-publication-observed-test-literal-fixture@" (subs sha 0 12) ".edn"))]
    (.mkdirs (.getParentFile file))
    (when (.exists file)
      (throw (ex-info "producer record already exists" {:file (str file)})))
    (spit file text)
    (println (.getPath file))))
(defn- leaf-paths [value]
  (letfn [(walk [path x]
            (if (map? x) (mapcat (fn [[k v]] (walk (conj path k) v)) x) [path]))]
    (walk [] value)))

(deftest wm-wire-r10-publication-observed-test-literal-fixture-producer
  (let [actual (build-record)]
    (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE"))
      (write-record! actual)
      (let [files (fixture-files)]
        (is (= 1 (count files)) "exactly one immutable wm-wire-r10-publication-observed-test-literal-fixture record")
        (let [expected (edn/read-string (slurp (first files)))]
          (doseq [path (leaf-paths expected)]
            (testing (pr-str path)
              (is (= (get-in expected path)
                     (get-in actual path))))))))))
