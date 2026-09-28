(ns futon3c.diagramprover.wm-wire-producer-wm-wire-r7-fold-call-enactment-fold-test-literal-fixture
  "Producer for the [:r7-fold-call :habit-fold-call-test :enactment-fold] wire.
  Runs the real war-machine/judge over a temp store with one flight record
  carrying one real increment receipt (the writer's enactment fold), then the
  real wm-cd/select-and-record-cascade! over the cascade-decision fixture's
  tick-1 family with that fold (the reader's consumed habit state), and
  asserts the observation equals the committed record. The requisition named
  inputs '.../literal-fixture'; no such var exists: the inputs are judge over
  the synthetic one-flight store plus futon2.report.cascade-decision-test's
  tick-1 target/sources."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk]
            [futon2.aif.cascade-problems :as problems]
            [futon2.aif.enactment-habit :as eh]
            [futon2.aif.locator-fixtures :as locators]
            [futon2.aif.mission-registry :as mr]
            [futon2.aif.scoring-input-receipts :as receipts]
            [futon2.aif.ticket-queue :as ticket-queue]
            [futon2.report.cascade-decision-test :as fixture]
            [futon2.report.war-machine :as wm]
            [futon2.aif.wm.cascade-decision :as wm-cd]
            [futon3c.diagramprover.wm-wire :as w])
  (:import [java.security MessageDigest]))

(def producer 'futon3c.diagramprover.wm-wire-producer-wm-wire-r7-fold-call-enactment-fold-test-literal-fixture-test)
(def operation 'futon2.report.war-machine/judge)
(def stem "wm-wire-r7-fold-call-enactment-fold-test-literal-fixture")
(def wire-id [:r7-fold-call :habit-fold-call-test :enactment-fold])

(def pkey [:pattern-cascade "M-t" [:p/a] {}])

(defn- receipt [click]
  (eh/increment {:click click :candidate :cas/a :attempts [{:pattern :p/a}]}
                pkey []))

(defn- write-flight! [store id rs]
  (let [f (io/file store "flights" (str id ".edn"))]
    (io/make-parents f)
    (spit f (pr-str {:plan {} :flight {:flight/id id
                                       :enactments (mapv (fn [r] {:click-id (first (:record-id r))
                                                                  :wc {:verdict []} :increment r})
                                                         rs)}}))
    (str f)))

(defn- judge-opts
  "The options judge hands select-and-record-cascade!, over STORE."
  [store]
  (let [captured (atom nil)
        tmp (w/tmp-dir "wire-fold-judge")]
    (with-redefs-fn {#'mr/load-missions (fn [& _] {:missions []})
                     #'mr/load-tickets (fn [& _] {:tickets []})
                     #'wm-cd/select-and-record-cascade!
                     (fn [_ opts] (reset! captured opts) (throw (ex-info "stop" {::stop true})))}
      #(try (wm/judge {} {:cascade-sources-dir tmp :cascade-proposals-dir tmp
                          :repair-obligations-root tmp
                          :machine-interpretations-dir store
                          :ticket-queue ticket-queue/empty-declaration})
            (catch clojure.lang.ExceptionInfo e
              (when-not (::stop (ex-data e)) (throw e)))))
    @captured))

(defn- habit-read
  "The joint selection's habit-read receipt when the real
  select-and-record-cascade! runs over the tick-1 family with FOLD as
  :enactment-fold (:none: no key, the pre-fix production state)."
  [fold]
  (let [reads (atom [])
        assembled (problems/assemble {:targets [fixture/tick-1-target]
                                      :sources (locators/locate-all fixture/tick-1-sources)})
        opts (cond-> (merge fixture/live-c-opts
                            {:cascade-habit-path (str (io/file (w/tmp-dir "wire-fold-habit") "absent.edn"))})
               (not= :none fold) (assoc :enactment-fold fold))]
    (binding [receipts/*habit-reads* reads]
      (wm-cd/select-and-record-cascade! assembled opts))
    (:receipt (first (filter #(= :joint-selection (:purpose %)) @reads)))))

(defn- normalize-paths
  "The per-run temporary store directory is the only value that differs from
  run to run; replace it with the stated token <run-store>."
  [v]
  (walk/postwalk (fn [x]
                   (if (and (string? x) (str/includes? x "/wire-fold-store"))
                     (str/replace x #"/tmp/wire-fold-store[^/]*" "<run-store>")
                     x))
                 v))

(defn- observe
  "judge over a store with one flight record; the writer's fold and the
  reader's consumed :state. SUPPLIED :judge passes judge's fold on (the
  wire), :none passes no fold (the typed absence), or a fold value passes
  that instead (a different value than the writer's)."
  [supplied]
  (let [store (w/tmp-dir "wire-fold-store")
        _ (write-flight! store "flight-a" [(receipt "click-1")])
        opts (judge-opts store)
        written (:enactment-fold opts)
        fold (case supplied :judge written :none :none supplied)
        read (habit-read fold)
        o {:writer written
           :reader (if (w/typed-absence? read) read (:state read))}]
    (normalize-paths
     (cond-> {:writer (:writer o)
              :reader (:reader o)
              :received? (w/received? o)}
       (not (w/typed-absence? (:reader o)))
       (assoc :writer-record-count (count (:enactment-records (:writer o)))
              :reader-record-count (count (:enactment-records (:reader o)))
              :reader-samples (get-in (:reader o) [:samples])
              :reader-folded-from-present? (some? (get-in (:reader o) [:folded-from])))))))

(def live-records-read
  [{:path (str w/spike-dir "/tick-run-record-2026-09-26-flight-278b6988-click-1.edn")
    :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"
    :why "predates WM-HABIT-FOLD-CALL-I: both habit reads {:status :absent :reason :no-enactment-fold}; judge passed no fold live"}])

(defn- live-pins []
  (mapv (fn [{:keys [path sha256]}]
          (let [occ (get-in (w/read-record path) [:habit-reads :occurrences])]
            {:sha256 sha256
             :sha-matches? (= sha256 (w/sha256-file path))
             :habit-read-occurrences (count occ)
             :all-no-enactment-fold? (every? #(= :no-enactment-fold (get-in % [:receipt :reason])) occ)}))
        live-records-read))

(defn build-record []
  {:producer producer
   :operation operation
   :inputs {:writer-operation 'futon2.report.war-machine/judge
            :reader-operation 'futon2.aif.wm.cascade-decision/select-and-record-cascade!
            :target :futon2.report.cascade-decision-test/tick-1-target
            :sources :futon2.report.cascade-decision-test/tick-1-sources
            :store "one flight record (flight-a) carrying one real eh/increment receipt for click-1"
            :wire-id wire-id :supplied [:judge :none :different-fold]}
   :wires {wire-id {:primary (observe :judge)
                    :interventions {:absent (observe :none)
                                    :different (observe (eh/fold nil [(receipt "click-9") (receipt "click-8")]))}}}
   :live-records-read live-records-read
   :live-records {:count (count live-records-read) :pins (live-pins)}
   :left-out {:run-store-directory "the temporary flights directory differs per run; every recorded path carries the stated token <run-store> in its place, and the relation (one read entry, sha256 f8619c306ace884d0373db4a8fe1fc9d2c82766a4fe79a29c29d459e38a7516c, 1 receipt, :unread empty) is fully recorded"
              :judge-opts-beyond-enactment-fold "readers check only the :enactment-fold judge handed, not the other captured options"
              :live-record-paths-and-why "readers check only the count, the pinned sha256 match, and that every habit-read occurrence has reason :no-enactment-fold"}})

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

(deftest enactment-fold-producer
  (let [actual (build-record)]
    (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE"))
      (write-record! actual)
      (let [files (fixture-files)]
        (is (= 1 (count files)) "exactly one immutable enactment-fold record")
        (let [expected (edn/read-string (slurp (first files)))]
          (doseq [section [:wires :live-records]]
            (doseq [path (leaf-paths (get expected section))]
              (testing (pr-str path)
                (is (= (get-in expected (into [section] path))
                       (get-in actual (into [section] path))))))))))))
