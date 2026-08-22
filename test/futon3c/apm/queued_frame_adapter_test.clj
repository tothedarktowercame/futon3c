(ns futon3c.apm.queued-frame-adapter-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.queued-frame-adapter :as sut]))

(def problem {:problem/id "p1" :repository "/repo" :revision "r"
              :path "p1.lean" :blob "b" :classification :non-excluded})
(def frame (:frame (sut/mint {:problem problem :ordinal 0 :queue/id "queue"
                              :frame-number-base 30})))
(def digest (apply str (repeat 64 "a")))

(deftest deterministic-mint-and-qualification
  (is (= "f30" (:frame/id frame)))
  (is (sut/valid-mint? frame))
  (is (:ok (sut/qualify {:frame frame :generated-contract-digest digest
                         :qualification-digest digest}))))

(deftest open-precedes-all-resource-effects
  (let [calls (atom [])
        body {:preparation/version 2 :frame/id "f30" :problem/id "p1"}
        preparation (assoc body :preparation/id (machine/ledger-digest [body]))
        result
        (sut/open-and-prepare!
         {:frame frame
          :open-frame-fn (fn [_] (swap! calls conj :open) {:ok true})
          :preparation-observation-fn
          (fn [_] (swap! calls conj :observe)
            {:ok true :version 5 :phase :preflight :claim nil
             :frame-id "f30" :problem-id "p1"})
          :prepare-frame-fn (fn [_ _] (swap! calls conj :prepare)
                              {:ok true :preparation preparation})
          :persist-preparation-fn (fn [_ _] (swap! calls conj :persist)
                                    {:ok true})})]
    (is (:ok result))
    (is (= [:open :observe :prepare :persist] @calls))))

(deftest no-provisioning-before-authoritative-preflight
  (let [calls (atom [])
        result
        (sut/open-and-prepare!
         {:frame frame :open-frame-fn (constantly {:ok true})
          :preparation-observation-fn
          (constantly {:ok true :version 4 :phase :open-frame :claim nil
                       :frame-id "f30" :problem-id "p1"})
          :prepare-frame-fn #(do (swap! calls conj :prepare) {:ok true})
          :persist-preparation-fn #(do (swap! calls conj :persist) {:ok true})})]
    (is (= :queued-frame-preparation-authority-invalid (:error/code result)))
    (is (empty? @calls))))
