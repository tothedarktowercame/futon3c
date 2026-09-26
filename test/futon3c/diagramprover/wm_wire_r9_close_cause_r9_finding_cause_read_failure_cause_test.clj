(ns futon3c.diagramprover.wm-wire-r9-close-cause-r9-finding-cause-read-failure-cause-test
  "Wire [:r9-close-cause :r9-finding-cause-read :failure-cause]: the
  close's cause chain (run-opportunity-core!, WM-CAUSE-ON-RECORD-I, onto
  the repair finding as :failure-cause) reaching finding-failure-cause,
  the typed reader of a durable finding's :failure-cause.

  No live record carries both ends: every finding under spike/ predates
  WM-CAUSE-ON-RECORD-I (live-records-read, pinned), so
  finding-failure-cause reads each of them {:absent :cause-not-on-record}.
  So the wire is WITNESSED-HERMETICALLY: one hermetic tick whose judge
  throws a refusal with a cause beneath it; the writer's value is the
  :failure-cause run-opportunity-core! put on the finding it handed the
  store; the reader's value is finding-failure-cause of the record the
  real repair/record-system-failure! durably wrote of that finding."
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.full-loop-runner :as runner]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-r9-support :as sup]))

(def live-record
  {:path (str w/spike-dir "/flight-ada87008/repair-occ-036463620c0c032c9e46aa44b6a6d6b35ed3e0ceb27dc58030f07d8fc2e6c747.edn")
   :sha256 "95bbb9c5476aedaadc50125068fff5f0a7e773c29a7a7494d84be58a57d893ad"
   :why "the eighth flight's finding, pre-WM-CAUSE-ON-RECORD-I: no :failure-cause key, so the reader reads it typed absent"})

(defn- caused-refusal [message]
  (ex-info "cascade decision refused" {:kind :live-c-stale :target "M-t"}
           (RuntimeException. message)))

(defn observe
  "One tick whose judge throws (caused-refusal MESSAGE), its finding stored
  by the real record-system-failure!: {:writer the finding's
  :failure-cause, :reader finding-failure-cause of the stored record}."
  [message]
  (let [{:keys [finding stored]} (sup/run-tick (caused-refusal message))]
    {:writer (:failure-cause finding)
     :reader (runner/finding-failure-cause stored)}))

(defn check [] (observe "beneath"))

(def wire
  {:wire [:r9-close-cause :r9-finding-cause-read :failure-cause]
   :kind :witnessed-hermetically
   :test `the-close-cause-reaches-finding-failure-cause
   :check check
   :live-records-read [live-record]})

(deftest the-close-cause-reaches-finding-failure-cause
  (let [o (check)]
    (is (= {:cause [{:class "java.lang.RuntimeException" :message "beneath"}]} (:writer o)))
    (is (w/received? o))))

(deftest a-typed-absence-at-the-reader-fails-the-wire
  ;; a causeless refusal: the writer writes {:absent :no-cause} and the
  ;; reader reads that typed absence off the stored record
  (let [{:keys [finding stored]} (sup/run-tick (ex-info "cascade decision refused"
                                                        {:kind :live-c-stale :target "M-t"}))
        o {:writer (:failure-cause finding)
           :reader (runner/finding-failure-cause stored)}]
    (is (= {:absent :no-cause} (:reader o)))
    (is (not (w/received? o)))))

(deftest a-different-cause-fails-the-wire
  (let [a (observe "beneath")
        b (observe "elsewhere")]
    (is (some? (:reader b)))
    (is (not= (:writer a) (:reader b)))
    (is (not (w/received? {:writer (:writer a) :reader (:reader b)})))))

(deftest the-live-finding-reads-typed-absent
  (is (= (:sha256 live-record) (w/sha256-file (:path live-record))))
  (let [r (w/read-record (:path live-record))]
    (is (not (contains? r :failure-cause)))
    (is (= {:absent :cause-not-on-record} (runner/finding-failure-cause r)))))
