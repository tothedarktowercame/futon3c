(ns futon3c.apm.live-batch-supervisor-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.apm.campaign-batch :as batch]
            [futon3c.apm.live-batch-supervisor :as sut]))

(def units (mapv #(hash-map :frame/id (str "f" %)) (range 19 28)))
(def certificate
  {:campaign/id "apm-countdown-r4" :campaign/manifest-hash "manifest-v2"
   :campaign/version 5 :ledger/digest "ledger-5"
   :campaign/permit-usage {} :generated-at "2026-08-21T10:00:00Z"})
(def obligation
  {:obligation/action {:kind :preflight :frame-id "f20"}})
(def permit
  (batch/issue
   {:campaign-id "apm-countdown-r4" :manifest-hash "manifest-v2"
    :start-version 5 :start-ledger-digest "ledger-5"
    :issuer "joe" :actor "countdown-batch" :max-actions 60
    :allowed-kinds [:open-frame :preflight :solve :verify :student-attempt
                    :guide-intervention :scribe-reduce :close-frame]
    :issued-at "2026-08-21T09:00:00Z"
    :valid-before "2026-08-22T09:00:00Z"}))

(defn options [state calls]
  {:units units :start-frame "f20" :end-frame "f25" :permit permit
   :trusted-permit-id (:permit/id permit) :trusted-issuer "joe"
   :actor "countdown-batch"
   :inspect-fn (fn [] {:ok true :stepper/status :ready
                       :checkpoint {:certificate certificate}
                       :obligation obligation})
   :frame-tick-fn (fn [request] (swap! calls conj [:tick request])
                    {:ok true :status :parked :job-id "job-20"})
   :cursor-read-fn (fn [] @state)
   :cursor-persist-fn (fn [cursor] (reset! state cursor)
                        (swap! calls conj [:persist cursor]) {:ok true})
   :continue-fn (fn [] (swap! calls conj [:continue]) {:ok true})})

(deftest contiguous-range-is-inclusive-and-manifest-ordered
  (is (= ["f20" "f21" "f22" "f23" "f24" "f25"]
         (:frames (sut/contiguous-range units "f20" "f25"))))
  (is (= :live-batch-range-invalid
         (:error/code (sut/contiguous-range units "f25" "f20")))))

(deftest tick-persists-content-addressed-cursor-before-delegation
  (let [state (atom nil) calls (atom []) result (sut/tick! (options state calls))]
    (is (= :parked (:status result)))
    (is (= "f20" (:batch/frame result)))
    (is (sut/valid-cursor? @state))
    (is (= :persist (ffirst @calls)))
    (is (= :tick (first (second @calls))))))

(deftest restart-reuses-matching-cursor-and-refuses-drift
  (let [state (atom nil) calls (atom []) opts (options state calls)]
    (is (:ok (sut/tick! opts)))
    (is (:ok (sut/tick! opts)))
    (swap! state assoc :frames ["f20" "f21"])
    (is (= :live-batch-cursor-invalid (:error/code (sut/tick! opts))))))

(deftest same-version-ledger-fork-is-refused
  (let [state (atom nil) calls (atom []) opts (options state calls)]
    (is (:ok (sut/tick! opts)))
    (let [forked (assoc certificate :ledger/digest "other-ledger")
          result (sut/tick!
                  (assoc opts :inspect-fn
                         (fn [] {:ok true :stepper/status :ready
                                 :checkpoint {:certificate forked}
                                 :obligation obligation})))]
      (is (= :live-batch-cursor-ledger-regression (:error/code result))))))

(deftest permit-and-range-fail-before-frame-effects
  (testing "tampered permit"
    (let [state (atom nil) calls (atom [])
          result (sut/tick! (assoc (options state calls) :permit
                                   (assoc permit :permit/max-actions 600)))]
      (is (= :live-batch-permit-content-invalid (:error/code result)))
      (is (empty? @calls))))
  (testing "ledger points outside approved frame range"
    (let [state (atom nil) calls (atom [])
          result (sut/tick!
                  (assoc (options state calls)
                         :inspect-fn
                         (fn [] {:ok true :stepper/status :ready
                                 :checkpoint {:certificate certificate}
                                 :obligation {:obligation/action
                                              {:kind :preflight
                                               :frame-id "f27"}}})))]
      (is (= :live-batch-frame-outside-range (:error/code result)))
      (is (empty? @calls)))))

(deftest exact-successor-after-end-closes-batch-without-running-next-frame
  (let [state (atom nil) calls (atom [])
        result (sut/tick!
                (assoc (options state calls)
                       :inspect-fn
                       (fn [] {:ok true :stepper/status :ready
                               :checkpoint {:certificate certificate}
                               :obligation {:obligation/action
                                            {:kind :open-frame
                                             :frame-id "f26"}}})))]
    (is (= :batch-complete (:status result)))
    (is (= :complete (:cursor/status @state)))
    (is (sut/valid-cursor? @state))
    (is (not-any? #(= :tick (first %)) @calls))))

(deftest completed-frame-schedules-the-next-batch-tick
  (let [state (atom nil) calls (atom [])
        result (sut/tick!
                (assoc (options state calls)
                       :frame-tick-fn
                       (fn [_] (swap! calls conj [:tick])
                         {:ok true :status :frame-complete})))]
    (is (= :batch-advanced (:status result)))
    (is (= [:continue] (last @calls)))))

(deftest awaiting-substrate-is-a-deliberate-wait
  (let [state (atom nil) calls (atom [])
        result (sut/tick!
                (assoc (options state calls)
                       :frame-tick-fn
                       (fn [_]
                         (swap! calls conj [:tick])
                         {:ok true :status :awaiting-substrate
                          :resume-at "2026-08-28T12:00:00Z"})))]
    (is (= :awaiting-substrate (:status result)))
    (is (= "2026-08-28T12:00:00Z" (:resume-at result)))
    (is (not-any? #(= :continue (first %)) @calls))))
