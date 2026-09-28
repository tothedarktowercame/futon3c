(ns futon3c.diagramprover.wm-wire-producer-wm-wire-flight-record-summary-click-reason-test-failure-test-test
  "Producer for the [:flight-record-summary :click-reason-test :failure]
  wire. Runs the real full-loop-runner/run-opportunity! in hermetic stores
  with a judge that throws (the click_reason_test box's eighth-flight
  shape: a substrate-unavailable close on a ConnectException), reads the
  run record with the real flight-runner/record-summary (the writer), and
  performs the reader box's own read of the click entry's :failure beside
  flight/click-failure. Also records the pinned eighth run record (no
  :failure key) through the same vars, and a second throwing judge (a
  parse failure) as the different-value intervention. Asserts the
  observation equals the committed record."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon2.aif.flight :as flight]
            [futon2.aif.flight-runner :as fr]
            [futon2.aif.full-loop-runner :as runner]
            [futon2.aif.hermetic-repair-fixture :as hermetic]
            [futon2.aif.learning-trial-ledger :as learning-ledger]
            [futon2.aif.trace :as trace]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-r9-support :as r9-support])
  (:import [java.security MessageDigest]))

(def producer 'futon3c.diagramprover.wm-wire-producer-wm-wire-flight-record-summary-click-reason-test-failure-test-test)
(def operation '[futon2.aif.full-loop-runner/run-opportunity!
                 futon2.aif.flight-runner/record-summary
                 futon2.aif.flight/record-click
                 futon2.aif.flight/click-failure])
(def stem "wm-wire-flight-record-summary-click-reason-test-failure-test")
(def wire-id [:flight-record-summary :click-reason-test :failure])

(def eighth-run-record
  {:path (str w/spike-dir "/flight-ada87008/tick-run-record-2026-09-26-flight-ada87008-click-1.edn")
   :sha256 "df01831c24a7042d66b6ef2c38d82cdfbd0994a03b5539f3112db7dc41894970"})

(defn- runner-opts [throw-fn]
  (merge (hermetic/runner-repair-options)
         r9-support/hermetic-runner-defaults
         {:cohort? false :author "zai-5" :reviewer "codex-7" :repair-reviewer "codex-1"
          :phase-log-fn (fn [_])
          :roster-fn (fn [_] {:zai-5 {:status "idle" :invoke-ready? true}
                              :codex-7 {:status "idle" :invoke-ready? true}
                              :codex-1 {:status "idle" :invoke-ready? true}})
          :refresh-fn (fn [])
          :substrate-preflight-fn (fn [_] {:route :test})
          :code-state-fn (fn [] {:repo "/futon2" :git-sha "head" :git-dirty? false :repo-heads {}})
          :mode-flags-fn (fn [] {}) :version-stamp-fn identity :mission-fn (fn [t] {:id t})
          :repair-open-fn (constantly [])
          :repair-system-record-fn (fn [m] {:repair/id "repair-wire-1" :repair/class (:repair-class m)})
          :r16-park-fn (fn [_ _] {:ok true :id "park-wire" :status :parked})
          :delivery-qa-fn (fn [_ _] {:morning-brief/addendum-id "qa-wire"})
          :queue-fn identity
          :judge-fn (fn [_] (throw (throw-fn)))
          :construct-fn (fn [& _] (throw (ex-info "no construction expected" {})))}))

(defn- run-record [throw-fn]
  (with-redefs-fn {#'trace/default-trace-dir (w/tmp-dir "wire-trace")
                   #'runner/default-run-record-dir (w/tmp-dir "wire-run-records")
                   #'learning-ledger/default-root (w/tmp-dir "wire-learning")}
    #(binding [runner/*wm-status-reporting?* false]
       (edn/read-string (slurp (:run-record (runner/run-opportunity! (runner-opts throw-fn))))))))

(defn- substrate-throw []
  (ex-info "substrate-2 mission registry unreachable" {}
           (java.net.ConnectException. "Connection refused")))

(defn- summarize
  "record-summary over RECORD (the writer), then the reader box's read: the
  click entry record-click writes over the summary, its :failure beside
  flight/click-failure."
  [target record]
  (let [summary (fr/record-summary target "click-1" record)
        e (first (:clicks (flight/record-click
                           (flight/start {:target target :chosen-because {:kind :requested}}
                                         {:kind :a-exits :repo "futon3c" :path "p" :read-text (fn [& _] "")}
                                         {:id "flight-wire"})
                           (merge summary {:wants [:t/b] :before {} :after {}}))))]
    {:writer (:failure summary)
     :reader (:failure e)
     :reader-agrees? (= (:failure e) (flight/click-failure e))}))

(defn- primary []
  (let [o (summarize "M-t" (run-record substrate-throw))]
    (assoc o :received? (w/received? o))))

(defn- different []
  (let [o (summarize "M-t" (run-record (fn [] (ex-info "the judge's model returned no parseable decision" {}))))]
    (assoc o :received? (w/received? o))))

(defn- pinned []
  (let [o (summarize "M-autoclock-in" (w/read-record (:path eighth-run-record)))]
    (assoc o :received? (w/received? o))))

(defn build-record []
  {:producer producer
   :operation operation
   :inputs {:runner-opts "hermetic-runner-repair options with judge-fn throwing; see producer source"
            :primary-throw "ex-info \"substrate-2 mission registry unreachable\" over a java.net.ConnectException \"Connection refused\""
            :different-throw "ex-info \"the judge's model returned no parseable decision\""
            :pinned eighth-run-record
            :wire-id wire-id
            :mutations [:none :different :pinned]}
   :wires {wire-id {:primary (primary)
                    :different (different)
                    :pinned (pinned)}}
   :left-out {:run-record-bytes "the hermetic run record's bytes carry per-run clock timestamps, uuids and temporary store paths; only its :failure summary (pure data derived from the thrown exception) and the reader's entry :failure are reader-checked, so only those are recorded"
              :live-record-verification "the reader re-verifies the live pins itself (sha256 and the absence of :failure keys) through w/sha256-file and w/read-record; the entries are carried for the wire map's :live-records-read"}})

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

(deftest wm-wire-flight-record-summary-click-reason-test-failure-test-producer
  (let [actual (build-record)]
    (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE"))
      (write-record! actual)
      (let [files (fixture-files)]
        (is (= 1 (count files)) "exactly one immutable record for this stem")
        (let [expected (edn/read-string (slurp (first files)))]
          (doseq [path (leaf-paths (:wires expected))]
            (testing (pr-str path)
              (is (= (get-in expected (into [:wires] path))
                     (get-in actual (into [:wires] path)))))))))))
