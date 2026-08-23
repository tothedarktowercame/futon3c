(ns futon3c.apm.frame-cycle-handlers-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.bank :as bank]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.frame-cycle-handlers :as handlers]))

(def cycle-contract
  (edn/read-string
   (slurp "holes/labs/M-apm-demonstration/frame-cycle-contract-v1.edn")))

(defn addressed [body]
  (assoc body :receipt/id (machine/ledger-digest [body])))

(defn student-receipt [ordinal session]
  (addressed
   {:receipt/type :student-attempt :receipt/frame-id "f18"
    :receipt/problem-id "a97J07" :receipt/attempt-ordinal ordinal
    :receipt/fresh-session-id session :receipt/job-id (str "student-" ordinal)
    :receipt/outcome :stuck
    :receipt/failure-account {:tried "x" :expected "y" :actual "z"}
    :receipt/memory-use {:surfaced-ids []}}))

(def preflight
  (addressed {:receipt/type :frame-preflight :receipt/frame-id "f18"
              :receipt/problem-id "a97J07" :receipt/result :passed}))

(def solve
  (addressed {:receipt/type :frame-solve :receipt/frame-id "f18"
              :receipt/problem-id "a97J07" :receipt/final-head "abc"
              :receipt/lean {:exit 0}}))

(def verify
  (addressed {:receipt/type :frame-verify :receipt/frame-id "f18"
              :receipt/problem-id "a97J07"
              :receipt/solve-receipt-id (:receipt/id solve)
              :receipt/mathematical-sound? true}))

(def attempt-1 (student-receipt 1 "fresh-1"))

(def guide-1
  (addressed
   {:receipt/type :guide-intervention :receipt/frame-id "f18"
    :receipt/problem-id "a97J07" :receipt/intervention-ordinal 1
    :receipt/mode :store-mode :receipt/input-attempt-id (:receipt/id attempt-1)
    :receipt/effect {:memory-id "m1"}
    :receipt/channel-audit {:direct-student-contact? false}}))

(def attempt-2 (student-receipt 2 "fresh-2"))

(def guide-2
  (addressed
   {:receipt/type :guide-intervention :receipt/frame-id "f18"
    :receipt/problem-id "a97J07" :receipt/intervention-ordinal 2
    :receipt/mode :store-mode :receipt/input-attempt-id (:receipt/id attempt-2)
    :receipt/effect {:memory-id "m2"}
    :receipt/channel-audit {:direct-student-contact? false}}))

(def attempt-3 (student-receipt 3 "fresh-3"))

(def prior-through-students
  {:preflight preflight :solve solve :verify verify
   :student-attempt-1 attempt-1 :guide-intervention-1 guide-1
   :student-attempt-2 attempt-2 :guide-intervention-2 guide-2
   :student-attempt-3 attempt-3})

(def scribe-input-ids
  (set (map :receipt/id (vals (dissoc prior-through-students :preflight)))))

(def scribe
  (addressed
   {:receipt/type :scribe-reduce :receipt/frame-id "f18"
    :receipt/problem-id "a97J07" :receipt/input-receipt-ids scribe-input-ids
    :receipt/lanes #{:solve :arc :trajectory :challenge}
    :receipt/dispositions [] :receipt/promotion-reviews []}))

(deftest executable-handlers-certify-valid-student-guide-and-scribe-data
  (let [receipts {:student-attempt-1 attempt-1
                  :guide-intervention-1 guide-1
                  :scribe-reduce scribe}
        built (handlers/make-handlers
               {:cycle-contract cycle-contract
                :receipt-provider (fn [phase _] (get receipts phase))
                :prior-receipts-provider
                (fn [{:keys [phase]}]
                  (case phase
                    :student-attempt-1 {:preflight preflight}
                    :guide-intervention-1
                    {:preflight preflight :student-attempt-1 attempt-1}
                    :scribe-reduce prior-through-students))})
        hs (:handlers built)]
    (is (:ok built))
    (is (:ok ((:student-attempt hs)
              {:kind :student-attempt :role :student
               :phase :student-attempt-1 :frame-id "f18"
               :problem-id "a97J07"})))
    (is (:ok ((:guide-intervention hs)
              {:kind :guide-intervention :role :guide
               :phase :guide-intervention-1 :frame-id "f18"
               :problem-id "a97J07"})))
    (is (:ok ((:scribe-reduce hs)
              {:kind :scribe-reduce :role :scribe :phase :scribe-reduce
               :frame-id "f18" :problem-id "a97J07"})))))

(deftest handler-refuses-missing-dependency-and-direct-guide-channel
  (is (= :frame-cycle-required-receipts-missing
         (:error/code
          (handlers/validate-completion
           cycle-contract
           {:kind :student-attempt :role :student :phase :student-attempt-1
            :frame-id "f18" :problem-id "a97J07"}
           attempt-1 {}))))
  (let [bad-body (-> guide-1
                     (dissoc :receipt/id)
                     (assoc-in [:receipt/channel-audit
                                :direct-student-contact?] true))
        bad (addressed bad-body)]
    (is (= :frame-cycle-guide-direct-channel-not-refuted
           (:error/code
            (handlers/validate-completion
             cycle-contract
             {:kind :guide-intervention :role :guide
              :phase :guide-intervention-1 :frame-id "f18"
              :problem-id "a97J07"}
             bad {:preflight preflight :student-attempt-1 attempt-1}))))))

(deftest close-refuses-reused-student-session
  (let [duplicate-attempt (student-receipt 3 "fresh-2")
        prior (assoc prior-through-students :student-attempt-3 duplicate-attempt
                     :scribe-reduce scribe)
        required (get-in cycle-contract [:phases :close-frame :requires])
        producers (into #{} (keep (fn [artifact]
                                    (some (fn [[phase spec]]
                                            (when (contains? (:produces spec)
                                                             artifact)
                                              (get-in prior [phase :receipt/id])))
                                          (:phases cycle-contract))))
                        required)
        close (addressed
               {:receipt/type :frame-close :receipt/frame-id "f18"
                :receipt/problem-id "a97J07"
                :receipt/input-receipt-ids producers :receipt/trace-id "trace"
                :receipt/result :closed})]
    (is (= :frame-cycle-student-sessions-not-distinct
           (:error/code
            (handlers/validate-completion
             cycle-contract
             {:kind :close-frame :role :guide :phase :close-frame
              :frame-id "f18" :problem-id "a97J07"}
             close prior))))))

(deftest partial-frame-records-reused-student-session-without-calling-it-closed
  (let [duplicate-attempt (student-receipt 3 "fresh-2")
        prior (assoc prior-through-students :student-attempt-3 duplicate-attempt
                     :scribe-reduce scribe)
        required (get-in cycle-contract [:phases :close-frame :requires])
        producers (into #{} (keep (fn [artifact]
                                    (some (fn [[phase spec]]
                                            (when (contains? (:produces spec) artifact)
                                              (get-in prior [phase :receipt/id])))
                                          (:phases cycle-contract))))
                        required)
        close (addressed
               {:receipt/type :frame-close :receipt/frame-id "f18"
                :receipt/problem-id "a97J07"
                :receipt/input-receipt-ids producers :receipt/trace-id "trace"
                :receipt/result :partial})]
    (is (:ok (handlers/validate-completion
              cycle-contract
              {:kind :close-frame :role :guide :phase :close-frame
               :frame-id "f18" :problem-id "a97J07"}
              close prior)))))

(deftest providers-fail-closed
  (is (= :frame-cycle-handler-provider-required
         (:error/code (handlers/make-handlers
                       {:cycle-contract cycle-contract}))))
  (let [built (handlers/make-handlers
               {:cycle-contract cycle-contract
                :receipt-provider (fn [_ _] nil)
                :prior-receipts-provider (fn [_] {})})]
    (is (= :frame-cycle-handler-evidence-unavailable
           (:error/code
            ((get-in built [:handlers :student-attempt])
             {:phase :student-attempt-1}))))))

(deftest bank-handler-rejects-a-different-frames-verify-receipt
  (let [contract (edn/read-string
                  (slurp (str "holes/labs/M-apm-demonstration/"
                              "frame-cycle-contract-codex-only-v1.edn")))
        verify-id (:receipt/id verify)
        receipt (:receipt
                 (bank/build-receipt
                  {:receipt/type :frame-bank
                   :receipt/frame-id "f18"
                   :receipt/problem-id "a97J07"
                   :receipt/verify-receipt-id (apply str (repeat 64 "f"))
                   :receipt/ruling :blocked
                   :receipt/lane-transition {:from :proof :to :library}
                   :receipt/seam "fixture seam"}))
        result (handlers/validate-completion
                contract
                {:kind :bank :role :proctor :phase :bank
                 :frame-id "f18" :problem-id "a97J07"}
                receipt {:verify verify})]
    (is (not= verify-id (:receipt/verify-receipt-id receipt)))
    (is (false? (:ok result)))
    (is (= :frame-cycle-input-receipt-set-mismatch (:error/code result)))
    (is (= #{verify-id} (get-in result [:finding :expected])))))

(def v2-contract
  (edn/read-string
   (slurp "holes/labs/M-apm-demonstration/frame-cycle-contract-v2.edn")))

(defn- v2-student-receipt [ordinal session binding]
  (addressed
   {:receipt/type :student-attempt :receipt/frame-id "f18"
    :receipt/problem-id "a97J07" :receipt/attempt-ordinal ordinal
    :receipt/fresh-session-id session :receipt/job-id (str "student-" ordinal)
    :receipt/outcome :stuck :receipt/failure-account []
    :receipt/memory-use {:surfaced-ids [] :used-ids [] :queries []}
    :receipt/memory-snapshot binding}))

(def v2-promotion
  (addressed
   {:receipt/type :solver-promotion :receipt/frame-id "f18"
    :receipt/problem-id "a97J07" :receipt/input-receipt-ids #{}
    :receipt/lanes [] :receipt/dispositions [] :receipt/promotion-reviews []
    :receipt/snapshot-id "snap-solver" :receipt/snapshot-digest "snap-solver"
    :receipt/snapshot-path "/tmp/snap-solver.edn"
    :receipt/reviewed-memory-ids [] :receipt/independent-review? true}))

(def v2-guide-1-with-union
  (addressed
   {:receipt/type :guide-intervention :receipt/frame-id "f18"
    :receipt/problem-id "a97J07" :receipt/intervention-ordinal 1
    :receipt/mode :store-mode :receipt/input-attempt-id "attempt-1"
    :receipt/effect {:channel "memory-store"}
    :receipt/channel-audit {:direct-student-contact? false}
    :receipt/snapshot-id "snap-guide-1" :receipt/snapshot-digest "snap-guide-1"
    :receipt/snapshot-path "/tmp/snap-guide-1.edn"
    :receipt/reviewed-memory-ids ["e-guide"] :receipt/promotion-reviews []
    :receipt/independent-review? true}))

(deftest student-binds-to-the-latest-reviewed-snapshot
  ;; f27: all three attempts bound to the Solver promotion; the Guide's
  ;; deposits never entered the accessible set.
  (let [receipts {:preflight preflight :solve solve :verify verify
                  :promote-solver v2-promotion
                  :student-attempt-1 (v2-student-receipt
                                      1 "fresh-1" (handlers/snapshot-binding v2-promotion))
                  :guide-intervention-1 v2-guide-1-with-union}
        action {:kind :student-attempt :role :student :phase :student-attempt-2
                :frame-id "f18" :problem-id "a97J07"}]
    (is (= v2-promotion (handlers/latest-snapshot-receipt receipts 1)))
    (is (= v2-guide-1-with-union (handlers/latest-snapshot-receipt receipts 2)))
    (is (= v2-guide-1-with-union (handlers/latest-snapshot-receipt receipts 3))
        "a Guide that published nothing does not hide an earlier union")
    (is (:ok (handlers/validate-completion
              v2-contract action
              (v2-student-receipt 2 "fresh-2"
                                  (handlers/snapshot-binding v2-guide-1-with-union))
              receipts)))
    (is (= :frame-cycle-student-memory-snapshot-mismatch
           (:error/code (handlers/validate-completion
                         v2-contract action
                         (v2-student-receipt 2 "fresh-2"
                                             (handlers/snapshot-binding v2-promotion))
                         receipts))))))

(deftest guide-union-snapshot-needs-complete-review-evidence
  (let [receipts {:preflight preflight :solve solve :verify verify
                  :promote-solver v2-promotion
                  :student-attempt-1 (v2-student-receipt
                                      1 "fresh-1" (handlers/snapshot-binding v2-promotion))}
        action {:kind :guide-intervention :role :guide :phase :guide-intervention-1
                :frame-id "f18" :problem-id "a97J07"}
        partial (addressed (dissoc v2-guide-1-with-union :receipt/id
                                   :receipt/promotion-reviews))]
    (is (:ok (handlers/validate-completion v2-contract action v2-guide-1-with-union
                                           receipts)))
    (is (= :frame-cycle-guide-snapshot-evidence-invalid
           (:error/code (handlers/validate-completion v2-contract action partial
                                                      receipts))))))
