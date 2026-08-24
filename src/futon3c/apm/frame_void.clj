(ns futon3c.apm.frame-void
  "Explicit, pinned terminal transition for a typed invalid frame."
  (:require [futon3c.apm.campaign-ledger :as ledger]
            [futon3c.apm.campaign-machine :as machine])
  (:import [java.time Instant]))

(def void-classifications
  {:known-failing-baseline :known-failing-baseline
   :apparatus-invalidated :apparatus-invalidated})

(defn prepare
  [{:keys [projection events]} {:keys [frame-id problem-id expected-version
                                       expected-ledger-digest failures now actor
                                       classification]
                                :or {classification :known-failing-baseline}}]
  (let [active (:active/frame projection)]
    (cond
      (not= :valid (:projection/status projection))
      {:ok false :error/code :frame-void-ledger-invalid}
      (not= expected-version (:campaign/version projection))
      {:ok false :error/code :frame-void-version-mismatch}
      (not= expected-ledger-digest (:ledger/digest projection))
      {:ok false :error/code :frame-void-digest-mismatch}
      (not= frame-id (:frame-id active))
      {:ok false :error/code :frame-void-frame-not-active}
      (not= problem-id (:problem-id active))
      {:ok false :error/code :frame-void-problem-mismatch}
      (not (and (vector? failures) (seq failures)
                (every? keyword? failures)))
      {:ok false :error/code :frame-void-failures-required}
      (not (contains? void-classifications classification))
      {:ok false :error/code :frame-void-classification-invalid}
      :else
      (let [certificate-body
            {:certificate/type :frame-void
             :frame/id frame-id :problem/id problem-id
             :classification classification
             :failed-invariants failures
             :source/version expected-version
             :source/ledger-digest expected-ledger-digest}
            certificate (assoc certificate-body :certificate/id
                               (machine/ledger-digest [certificate-body]))
            obligation-body
            {:obligation/type :campaign
             :obligation/action {:kind :void-frame :role :ground-control
                                 :frame-id frame-id :problem-id problem-id}
             :obligation/preconditions
             {:campaign/id (:campaign/id projection)
              :campaign/version expected-version
              :ledger/digest expected-ledger-digest}}
            obligation (assoc obligation-body :obligation/id
                              (machine/ledger-digest [obligation-body]))
            claim-base {:event/seq (count events) :event/type :obligation/claimed
                        :event/campaign-id (:campaign/id projection)
                        :event/actor (or actor "ground-control")
                        :event/at (str (or now (Instant/now)))
                        :event/expected-version expected-version
                        :event/body {:obligation obligation}}
            claim-event (assoc claim-base :event/id
                               (machine/ledger-digest [claim-base]))
            event-base {:event/seq (inc (count events)) :event/type :frame/stopped
                        :event/campaign-id (:campaign/id projection)
                        :event/actor (or actor "ground-control")
                        :event/at (str (or now (Instant/now)))
                        :event/expected-version (inc expected-version)
                        :event/body {:frame-id frame-id
                                     :reason (get void-classifications classification)
                                     :obligation/id (:obligation/id obligation)
                                     :certificate certificate}}
            event (assoc event-base :event/id (machine/ledger-digest [event-base]))
            successor (when (every? :event/id events)
                        (machine/projection (into events [claim-event event])))]
        (if (or (nil? successor) (= :valid (:projection/status successor)))
          {:ok true :certificate certificate :obligation obligation
           :claim-event claim-event :event event}
          {:ok false :error/code :frame-void-successor-invalid
           :projection successor})))))

(defn void!
  [{:keys [ledger-path] :as options}]
  (let [loaded (ledger/read-ledger ledger-path)]
    (if-not (:ok loaded)
      loaded
      (let [prepared (prepare loaded options)]
        (if-not (:ok prepared)
          prepared
          (let [projection (:projection loaded)
                claimed (ledger/compare-and-append!
                          ledger-path (:campaign/version projection)
                          (:ledger/digest projection) (:claim-event prepared))]
            (if-not (:ok claimed)
              claimed
              (let [completed (ledger/compare-and-append!
                               ledger-path (get-in claimed [:after :version])
                               (get-in claimed [:after :digest]) (:event prepared))]
                (if (:ok completed)
                  {:ok true :certificate (:certificate prepared)
                   :claim-event (:claim-event prepared) :event (:event prepared)
                   :claim claimed :completion completed}
                  {:ok false :error/code :frame-void-completion-refused
                   :claim-persisted? true :completion completed})))))))))
