(ns futon3c.apm.campaign-machine-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.apm.campaign-machine :as machine]))

(def phases [:probe :freeze :solve :verify :close])

(defn event [seq type body]
  {:event/id (str "e" seq) :event/seq seq :event/type type
   :event/campaign-id "apm-200" :event/actor "regulator"
   :event/at (str "2026-08-20T00:00:0" seq "Z")
   :event/expected-version seq :event/body body})

(def prefix
  [(event 0 :campaign/registered
          {:series :apm :manifest-hash "manifest" :phase-order phases})
   (event 1 :block/opened {:block-id "b1" :ordinal 1})
   (event 2 :frame/opened
          {:frame-id "f1" :block-id "b1" :problem-id "m97A06"
           :registration-hash "registration" :harness-hash "harness"})])

(deftest valid-ledger-produces-certificate-projection
  (let [events (into prefix
                     [(event 3 :frame/advanced {:frame-id "f1" :from :probe :to :freeze
                                                :certificate {:probe :pass}})
                      (event 4 :frame/advanced {:frame-id "f1" :from :freeze :to :solve
                                                :certificate {:frozen true}})
                      (event 5 :frame/advanced {:frame-id "f1" :from :solve :to :verify
                                                :certificate {:solver :done}})
                      (event 6 :frame/advanced {:frame-id "f1" :from :verify :to :close
                                                :certificate {:proctor :pass}})
                      (event 7 :frame/closed {:frame-id "f1"
                                              :certificate {:axioms :clean}})
                      (event 8 :block/closed {:block-id "b1"
                                              :certificate {:frames 1}})])
        projected (machine/projection events)]
    (is (= :valid (:projection/status projected)))
    (is (= 9 (:ledger/event-count projected)))
    (is (= 1 (get-in projected [:counts :closed-frames])))
    (is (nil? (:active/frame projected)))
    (is (= 64 (count (:ledger/digest projected))))
    (is (= projected (machine/projection events (:ledger/digest projected))))))

(deftest ledger-refuses-gaps-stale-versions-and-duplicates
  (testing "sequence gap"
    (is (= :campaign-sequence-gap
           (:error/code (machine/projection [(event 1 :campaign/registered
                                                    {:phase-order phases})])))))
  (testing "stale version"
    (let [bad (assoc (event 1 :block/opened {:block-id "b1"})
                     :event/expected-version 0)]
      (is (= :campaign-version-stale
             (:error/code (machine/projection [(first prefix) bad]))))))
  (testing "duplicate id"
    (let [bad (assoc (second prefix) :event/id "e0")]
      (is (= :campaign-event-duplicate
             (:error/code (machine/projection [(first prefix) bad])))))))

(deftest frame-transitions-and-sequentiality-are-enforced
  (testing "cannot skip a phase"
    (is (= :frame-transition-illegal
           (:error/code
            (machine/projection
             (conj prefix
                   (event 3 :frame/advanced
                          {:frame-id "f1" :from :probe :to :solve})))))))
  (testing "cannot open a second active frame"
    (is (= :campaign-active-frame-conflict
           (:error/code
            (machine/projection
             (conj prefix
                   (event 3 :frame/opened
                          {:frame-id "f2" :block-id "b1"
                           :problem-id "m98A01"}))))))))

(deftest terminal-events-require-certificates
  (let [advanced (into prefix
                       (map-indexed
                        (fn [index [from to]]
                          (event (+ 3 index) :frame/advanced
                                 {:frame-id "f1" :from from :to to
                                  :certificate {:ok true}}))
                        (partition 2 1 phases)))]
    (is (= :frame-close-certificate-required
           (:error/code
            (machine/projection
             (conj advanced
                   (event 7 :frame/closed {:frame-id "f1"}))))))))

(deftest pinned-ledger-digest-refuses-different-history
  (let [digest (:ledger/digest (machine/projection prefix))
        changed (update-in prefix [2 :event/body :problem-id] str "-changed")]
    (is (= :campaign-ledger-digest-mismatch
           (:error/code (machine/projection changed digest))))))
