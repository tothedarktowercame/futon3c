(ns futon3c.diagramprover.wm-wire-r9-close-cause-r9-finding-store-failure-cause-test
  "Wire [:r9-close-cause :r9-finding-store :failure-cause]: the close's
  cause chain (run-opportunity-core!, WM-CAUSE-ON-RECORD-I, onto the repair
  finding as :failure-cause) reaching repair-obligation/record-system-failure!,
  which keeps the key on the durable record it writes (\"kept when the
  writer supplied one\").

  No live record carries both ends: every finding under spike/ predates
  WM-CAUSE-ON-RECORD-I (live-records-read, pinned: no :failure-cause key).
  So the wire is WITNESSED-HERMETICALLY: one hermetic tick whose judge
  throws a refusal with a cause beneath it; the writer's value is the
  :failure-cause run-opportunity-core! put on the finding it handed the
  store; the reader's value is the :failure-cause on the record the real
  record-system-failure! durably wrote of that finding."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-r9-support :as sup]))

(def live-record
  {:path (str w/spike-dir "/flight-ada87008/repair-occ-036463620c0c032c9e46aa44b6a6d6b35ed3e0ceb27dc58030f07d8fc2e6c747.edn")
   :sha256 "95bbb9c5476aedaadc50125068fff5f0a7e773c29a7a7494d84be58a57d893ad"
   :why "the eighth flight's finding, pre-WM-CAUSE-ON-RECORD-I: no :failure-cause key for the store to have kept"})

(defn- caused-refusal [message]
  (ex-info "cascade decision refused" {:kind :live-c-stale :target "M-t"}
           (RuntimeException. message)))

(defn observe
  "One tick whose judge throws (caused-refusal MESSAGE): {:writer the
  finding's :failure-cause as handed to the store, :reader the
  :failure-cause the store kept on the durable record}."
  [message]
  (let [{:keys [finding stored]} (sup/run-tick (caused-refusal message))]
    {:writer (:failure-cause finding)
     :reader (:failure-cause stored)}))

(defn check [] (observe "beneath"))

(def wire
  {:wire [:r9-close-cause :r9-finding-store :failure-cause]
   :kind :witnessed-hermetically
   :test `the-close-cause-survives-the-finding-store
   :check check
   :live-records-read [live-record]})

(deftest the-close-cause-survives-the-finding-store
  (let [o (check)]
    (is (= {:cause [{:class "java.lang.RuntimeException" :message "beneath"}]} (:writer o)))
    (is (w/received? o))))

(deftest a-typed-absence-is-kept-and-fails-the-wire
  ;; a causeless refusal: the writer supplies {:absent :no-cause} and the
  ;; store keeps that typed absence
  (let [{:keys [finding stored]} (sup/run-tick (ex-info "cascade decision refused"
                                                        {:kind :live-c-stale :target "M-t"}))
        o {:writer (:failure-cause finding) :reader (:failure-cause stored)}]
    (is (= {:absent :no-cause} (:reader o)))
    (is (not (w/received? o)))))

(deftest a-different-cause-fails-the-wire
  (let [a (observe "beneath")
        b (observe "elsewhere")]
    (is (some? (:reader b)))
    (is (not= (:writer a) (:reader b)))
    (is (not (w/received? {:writer (:writer a) :reader (:reader b)})))))

(deftest the-live-finding-carries-no-failure-cause
  (is (= (:sha256 live-record) (w/sha256-file (:path live-record))))
  (is (not (contains? (w/read-record (:path live-record)) :failure-cause))))
