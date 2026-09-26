(ns futon3c.diagramprover.wm-wire-r9-close-cause-failure-cause-record-test-test
  "Wire [:r9-close-cause :failure-cause-record-test :failure-cause]: the
  close's cause chain (run-opportunity-core!, WM-CAUSE-ON-RECORD-I)
  reaching the component's own test,
  futon2/test/futon2/aif/failure_cause_record_test.clj, whose reads of the
  field are

    (:failure-cause finding)                       ; on the handed finding
    (get-in result [:data :cause])                 ; the close map's copy
    (runner/finding-failure-cause stored)          ; off the durable record

  A test box has no runtime var to drive through, so the hermetic witness
  performs exactly those reads on one hermetic tick whose judge throws a
  refusal with a cause beneath it: the writer's value is the
  :failure-cause run-opportunity-core! put on the finding it handed the
  store; the reader's value is the close map's [:data :cause] the test
  equates it with.

  No live record carries either end: every finding under spike/ predates
  WM-CAUSE-ON-RECORD-I (live-records-read, both pinned: neither carries a
  :failure-cause key). So the wire is WITNESSED-HERMETICALLY."
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.full-loop-runner :as runner]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-r9-support :as sup]))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "flight-ada87008/repair-occ-036463620c0c032c9e46aa44b6a6d6b35ed3e0ceb27dc58030f07d8fc2e6c747.edn")
      :sha256 "95bbb9c5476aedaadc50125068fff5f0a7e773c29a7a7494d84be58a57d893ad"
      :why "the eighth flight's finding, pre-WM-CAUSE-ON-RECORD-I: no :failure-cause key (finding-failure-cause reads it {:absent :cause-not-on-record})"}
     {:path (p "flight-7f89646a-repair-finding.edn")
      :sha256 "758eaeb64b3c0f0ef3733a1725c9a8a54f8a23701698a26373fc44b29279e264"
      :why "the fifth flight's finding, likewise pre-fix: no :failure-cause key"}]))

(defn- caused-refusal [message]
  (ex-info "cascade decision refused" {:kind :live-c-stale :target "M-t"}
           (RuntimeException. message)))

(def ^:private expected-cause
  {:cause [{:class "java.lang.RuntimeException" :message "beneath"}]})

(defn observe
  "One tick whose judge throws (caused-refusal MESSAGE): {:writer the
  finding's :failure-cause, :reader the close map's [:data :cause],
  :stored-read finding-failure-cause off the durable record}."
  [message]
  (let [{:keys [result finding stored]} (sup/run-tick (caused-refusal message))]
    {:writer (:failure-cause finding)
     :reader (get-in result [:data :cause])
     :stored-read (runner/finding-failure-cause stored)}))

(defn check [] (observe "beneath"))

(def wire
  {:wire [:r9-close-cause :failure-cause-record-test :failure-cause]
   :kind :witnessed-hermetically
   :test `the-close-cause-reaches-the-components-test
   :check check
   :live-records-read live-records-read})

(deftest the-close-cause-reaches-the-components-test
  (let [o (check)]
    (is (= expected-cause (:writer o)))
    (is (= (:writer o) (:stored-read o))
        "the reader's third read: the durable finding keeps it")
    (is (w/received? o))))

(deftest no-cause-is-a-typed-absence-and-fails-the-wire
  (let [{:keys [result finding]} (sup/run-tick (ex-info "cascade decision refused"
                                                        {:kind :live-c-stale :target "M-t"}))]
    (is (= {:absent :no-cause} (:failure-cause finding)))
    (is (= {:absent :no-cause} (get-in result [:data :cause])))
    (is (not (w/received? {:writer (:failure-cause finding)
                           :reader (get-in result [:data :cause])})))))

(deftest a-different-cause-fails-the-wire
  (let [a (observe "beneath")
        b (observe "elsewhere")]
    (is (= {:cause [{:class "java.lang.RuntimeException" :message "elsewhere"}]} (:reader b)))
    (is (not (w/received? {:writer (:writer a) :reader (:reader b)})))))

(deftest the-live-findings-carry-no-failure-cause
  (doseq [{:keys [path sha256]} live-records-read]
    (is (= sha256 (w/sha256-file path)) path)
    (let [r (w/read-record path)]
      (is (not (contains? r :failure-cause)) path)
      (is (= {:absent :cause-not-on-record} (runner/finding-failure-cause r)) path))))
