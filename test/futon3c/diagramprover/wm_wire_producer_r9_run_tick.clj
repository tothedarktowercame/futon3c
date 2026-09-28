(ns futon3c.diagramprover.wm-wire-producer-r9-run-tick
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon2.aif.full-loop-runner :as runner]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-failure-products-15c :as products]
            [futon3c.diagramprover.wm-wire-r9-support :as support])
  (:import [java.security MessageDigest]))

(def producer 'futon3c.diagramprover.wm-wire-producer-r9-run-tick-test)
(def operation 'futon2.aif.full-loop-runner/run-opportunity!)

(def wire-ids
  [[:r9-close-cause :failure-cause-record-test :failure-cause]
   [:r9-close-cause :r9-finding-cause-read :failure-cause]
   [:r9-close-cause :r9-finding-store :failure-cause]
   [:r9-judge-refusal-abstention :r9-failure-classifier :outcome]
   [:r9-judge-refusal :r9-abstention-carrier :judge-refusal]
   [:r9-judge-refusal :r9-judge-refusal-test :judge-refusal]
   [:r9-phase-kind :phase-kind-test :failure-kind]
   [:r9-phase-kind :r9-failure-classifier :failure-kind]])

(def ^:private judge-refusal-abstention* @#'runner/judge-refusal-abstention)
(def ^:private judge-refusal-sorry* @#'runner/judge-refusal-sorry)
(def ^:private phase-kind-failure* @#'runner/phase-kind-failure)
(def ^:private explicit-failure-kind* @#'runner/explicit-failure-kind)

(defn- refusal [kind]
  (ex-info "cascade decision refused" {:kind kind :target "M-t"}))

(defn- caused-refusal [message]
  (ex-info "cascade decision refused" {:kind :live-c-stale :target "M-t"}
           (RuntimeException. message)))

(defn- kind-throw [kind]
  (ex-info "substrate-2 mission registry returned no missions" {:kind kind}))

(defn- written-outcome [kind]
  (let [je (refusal kind)]
    (:outcome
     (ex-data
      (try
        (throw (judge-refusal-abstention* (runner/judge-refusal je "M-t") je))
        (catch clojure.lang.ExceptionInfo x x))))))

(defn- close-values [{:keys [result finding stored]}]
  {:result-reader (get-in result [:data :cause])
   :writer (:failure-cause finding)
   :stored-value (:failure-cause stored)
   :stored-read (runner/finding-failure-cause stored)})

(defn- judge-values [kind {:keys [result record]}]
  (let [je (refusal kind)
        cell (judge-refusal-sorry* (runner/judge-refusal je "M-t"))
        target (get-in record [:decision :abstention :targets 0])]
    {:outcome {:writer (written-outcome kind)
               :reader (get-in result [:data :failure-kind])}
     :carrier {:writer (get-in result [:checkpoints :selection :sorry :judge-refusal])
               :reader (if target
                         (select-keys target [:kind :target :missing :data])
                         {:absent :no-judge-refusal})
               :carrier-status (get-in record [:decision :abstention :status])}
     :component {:writer (get-in cell [:sorry :judge-refusal])
                 :reader (or (get-in result [:checkpoints :selection :sorry :judge-refusal])
                             {:absent :no-judge-refusal})}}))

(defn- phase-values [kind {:keys [result]}]
  (let [rethrown (phase-kind-failure* (kind-throw kind))]
    {:writer (:failure-kind (ex-data rethrown))
     :reader (get-in result [:data :failure-kind])
     :classified (explicit-failure-kind* rethrown)}))

(defn- pinned-live-controls []
  (let [p #(str w/spike-dir "/" %)
        finding-path (p "flight-ada87008/repair-occ-036463620c0c032c9e46aa44b6a6d6b35ed3e0ceb27dc58030f07d8fc2e6c747.edn")
        finding (w/read-record finding-path)
        old-refusal-path (p "tick-run-record-2026-09-26-flight-e70b4baf-click-1.edn")
        old-abstention-path (p "tick-run-record-2026-09-25-flight-ffcd772b-click-1.edn")
        old-refusal (w/read-record old-refusal-path)
        old-abstention (w/read-record old-abstention-path)]
    {:finding {:sha-ok? (= "95bbb9c5476aedaadc50125068fff5f0a7e773c29a7a7494d84be58a57d893ad"
                           (w/sha256-file finding-path))
               :cause-key-absent? (not (contains? finding :failure-cause))
               :cause-read (runner/finding-failure-cause finding)
               :failure-data (:failure-data finding)
               :failure-kind (:failure-kind finding)}
     :old-refusal {:sha-ok? (= "241b10a024020344eba5d444c12fb33ad6afe107724dec95c81229b422bc5feb"
                               (w/sha256-file old-refusal-path))
                   :abstention (get-in old-refusal [:decision :abstention])}
     :old-abstention {:sha-ok? (= "8ab0db5d770085f17bb341293a92424149bf2388ae7a6dd3e724887c9a44eba2"
                                 (w/sha256-file old-abstention-path))
                      :status (get-in old-abstention [:decision :abstention :status])
                      :targets-have-no-data? (every? #(not (contains? % :data))
                                                     (get-in old-abstention [:decision :abstention :targets]))
                      :sorry-has-no-refusal? (not (contains? (get-in old-abstention [:checkpoints :selection :sorry] {})
                                                            :judge-refusal))}}))

(defn- store-relations []
  (let [{:keys [values inputs stored read missing classification]} (products/store-products)]
    {:values-differ? (not= (first values) (second values))
     :values-preserved? (= values (mapv :failure-cause stored) read)
     :other-inputs-equal? (= (dissoc (first inputs) :failure-cause)
                             (dissoc (second inputs) :failure-cause))
     :classification-equal? (= (first classification) (second classification))
     :missing missing}))

(defn build-record []
  (let [beneath (close-values (support/run-tick (caused-refusal "beneath")))
        elsewhere (close-values (support/run-tick (caused-refusal "elsewhere")))
        causeless (close-values (support/run-tick (refusal :live-c-stale)))
        live-judge (judge-values :live-c-stale (support/run-tick (refusal :live-c-stale)))
        other-judge (judge-values :incommensurable-family
                                  (support/run-tick (refusal :incommensurable-family)))
        boom-tick (support/run-tick (ex-info "boom" {:x 1}))
        boom (judge-values :untyped boom-tick)
        substrate (phase-values :substrate-mission-registry-empty
                                (support/run-tick (kind-throw :substrate-mission-registry-empty)))
        invalid (phase-values :invalid-temperature
                              (support/run-tick (kind-throw :invalid-temperature)))
        typed-cause {:writer nil :reader {:absent :not-typed}}
        typed-outcome {:writer :abstained :reader {:absent :no-outcome}}]
    {:producer producer
     :operation operation
     :inputs {:run-tick {:opts :fixed-hermetic-r9-support
                         :judge-throws [:caused-refusal :typed-refusal :untyped-throw :bare-kind-throw]}}
     :wires
     {(nth wire-ids 0) {:primary {:writer (:writer beneath) :reader (:result-reader beneath)
                                  :stored-read (:stored-read beneath)}
                        :typed-absence {:writer (:writer causeless) :reader (:result-reader causeless)}
                        :different {:writer (:writer beneath) :reader (:result-reader elsewhere)}}
      (nth wire-ids 1) {:primary {:writer (:writer beneath) :reader (:stored-read beneath)}
                        :typed-absence {:writer (:writer causeless) :reader (:stored-read causeless)}
                        :different {:writer (:writer beneath) :reader (:stored-read elsewhere)}
                        :second-layer (store-relations)}
      (nth wire-ids 2) {:primary {:writer (:writer beneath) :reader (:stored-value beneath)}
                        :typed-absence {:writer (:writer causeless) :reader (:stored-value causeless)}
                        :different {:writer (:writer beneath) :reader (:stored-value elsewhere)}
                        :second-layer (store-relations)}
      (nth wire-ids 3) {:primary (get-in live-judge [:outcome])
                        :typed-absence typed-outcome
                        :different {:writer :abstained :reader :grounded-change}
                        :second-layer {:rows (products/precedence :outcome)}}
      (nth wire-ids 4) {:primary (get-in live-judge [:carrier])
                        :typed-absence {:writer (get-in boom [:carrier :writer])
                                        :reader (get-in boom [:carrier :reader])}
                        :different {:writer (get-in live-judge [:carrier :writer])
                                    :reader (get-in other-judge [:carrier :reader])}}
      (nth wire-ids 5) {:primary (get-in live-judge [:component])
                        :typed-absence {:writer (get-in boom-tick [:result :checkpoints :selection :sorry :judge-refusal])
                                        :reader {:absent :no-judge-refusal}}
                        :different {:writer (get-in live-judge [:component :writer])
                                    :reader (get-in other-judge [:component :reader])}}
      (nth wire-ids 6) {:primary {:writer (:writer substrate) :reader (:reader substrate)}
                        :typed-absence typed-cause
                        :different {:writer (:writer substrate) :reader (:reader invalid)}}
      (nth wire-ids 7) {:primary {:writer (:writer substrate) :reader (:classified substrate)
                                  :closed (:reader substrate)}
                        :typed-absence typed-cause
                        :different {:writer (:writer substrate) :reader (:classified invalid)}
                        :second-layer {:rows (products/precedence :failure-kind)}}}
     :live-controls (pinned-live-controls)
     :left-out {:timestamps "clock values differ per hermetic run and no reader checks them"
                :ids "generated run, click, attempt and occurrence ids differ per run and no reader checks them"
                :temporary-paths "hermetic store paths differ per run and no reader checks them"}}))

(defn- record-text [record] (str (pr-str record) "\n"))

(defn- sha256 [text]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256") (.getBytes text "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))

(defn- fixture-files []
  (filter #(.startsWith (.getName %) "r9-run-tick@")
          (.listFiles (io/file "test/fixtures/wire-producers"))))

(defn- write-record! [record]
  (let [text (record-text record)
        sha (sha256 text)
        file (io/file "test/fixtures/wire-producers" (str "r9-run-tick@" (subs sha 0 12) ".edn"))]
    (.mkdirs (.getParentFile file))
    (when (.exists file)
      (throw (ex-info "producer record already exists" {:file (str file)})))
    (spit file text)
    (println (.getPath file))))

(defn- leaf-paths [value]
  (letfn [(walk [path x]
            (if (map? x)
              (mapcat (fn [[k v]] (walk (conj path k) v)) x)
              [path]))]
    (walk [] value)))

(deftest r9-run-tick-producer
  (let [actual (build-record)]
    (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE"))
      (write-record! actual)
      (let [files (fixture-files)]
        (is (= 1 (count files)) "exactly one immutable r9-run-tick record")
        (let [expected (edn/read-string (slurp (first files)))]
          (doseq [path (leaf-paths (select-keys expected [:wires :live-controls]))]
            (testing (pr-str path)
              (is (= (get-in expected path) (get-in actual path))))))))))
