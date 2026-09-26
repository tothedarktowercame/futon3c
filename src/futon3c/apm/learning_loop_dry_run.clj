(ns futon3c.apm.learning-loop-dry-run
  "No-dispatch integration proof for the repaired learning dataflow."
  (:require [futon3c.apm.analyst-campaign :as analyst]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.live-learning-phases :as learning]
            [futon3c.apm.memory-snapshot :as snapshot]))

(defn- addressed [body]
  (assoc body :receipt/id (machine/ledger-digest [body])))

(defn dry-run!
  [{:keys [contract snapshot-path candidates]}]
  (let [frame-id "dry-f1" problem-id "dry-p1"
        preflight (addressed {:receipt/type :frame-preflight
                              :receipt/frame-id frame-id
                              :receipt/problem-id problem-id
                              :receipt/result :passed})
        solve (addressed {:receipt/type :frame-solve :receipt/frame-id frame-id
                          :receipt/problem-id problem-id :receipt/final-head "head"
                          :receipt/lean {:exit 0 :sorry-warnings 0}})
        verify (addressed {:receipt/type :frame-verify :receipt/frame-id frame-id
                           :receipt/problem-id problem-id
                           :receipt/solve-receipt-id (:receipt/id solve)
                           :receipt/mathematical-sound? true})
        published (snapshot/publish!
                   {:frame-id frame-id :problem-id problem-id
                    :candidates candidates :path snapshot-path
                    :evidence-visible? (constantly true)})]
    (if-not (:ok published)
      published
      (let [snap (:snapshot published)
            promotion
            (addressed
             {:receipt/type :solver-promotion :receipt/frame-id frame-id
              :receipt/problem-id problem-id
              :receipt/input-receipt-ids #{(:receipt/id solve) (:receipt/id verify)}
              :receipt/lanes [:solve] :receipt/dispositions [:approve]
              :receipt/promotion-reviews [:independent]
              :receipt/snapshot-id (:snapshot/id snap)
              :receipt/snapshot-digest (:snapshot/digest snap)
              :receipt/snapshot-path (str snapshot-path)
              :receipt/reviewed-memory-ids (mapv :memory-id candidates)
              :receipt/independent-review? true
              :receipt/independence :asserted-unverified})
            access (snapshot/verify-student-access
                    {:path snapshot-path :expected (:snapshot/digest snap)
                     :frame-id frame-id :problem-id problem-id
                     :accessible-memory-ids (mapv :memory-id candidates)})
            student
            (learning/build-request
             {:contract contract
              :action {:kind :student-attempt :phase :student-attempt-1
                       :role :student :ordinal 1 :frame-id frame-id
                       :problem-id problem-id}
              :ledger {:digest "dry-ledger"}
              :unit {:frame/id frame-id :problem/id problem-id}
              :role-card {:path "student.md" :blob "student-blob"}
              :seat {:agent-id "dry-f1-student" :invoke-ready? true}
              :workspace {:workspace/path "/dry/student"}
              :receipts {:preflight preflight :solve solve :verify verify
                         :promote-solver promotion}
              :snapshot-access access})
            close-1 (addressed {:receipt/type :frame-close
                                :receipt/frame-id frame-id
                                :receipt/problem-id problem-id
                                :receipt/input-receipt-ids #{}
                                :receipt/trace-id "trace-1"
                                :receipt/result :closed})
            close-2 (addressed {:receipt/type :frame-close
                                :receipt/frame-id "dry-f2"
                                :receipt/problem-id "dry-p2"
                                :receipt/input-receipt-ids #{}
                                :receipt/trace-id "trace-2"
                                :receipt/result :closed})
            initial (:state (analyst/register
                             {:campaign-id "dry-campaign"
                              :analyst-seat "analyst-1"
                              :analyst-card-path "analyst.md"
                              :analyst-card-blob "analyst-blob"}))
            w1 (analyst/wake-after-close initial close-1)
            base-report {:analyst-seat "analyst-1"
                         :analyst-card {:path "analyst.md" :blob "analyst-blob"}
                         :series-entry {:frame frame-id}
                         :findings []
                         :implementation-packets
                         [{:packet-id "packet-1"
                           :proposed-regime-id "future-regime-1"}]}
            a1 (analyst/accept-analysis (:state w1) (:obligation w1) base-report)
            w2 (analyst/wake-after-close (:state a1) close-2)
            a2 (analyst/accept-analysis
                (:state w2) (:obligation w2)
                (assoc base-report
                       :series-entry {:frame "dry-f2"}
                       :handoff {:successor-seat "analyst-2"
                                 :successor-card {:path "analyst-2.md"
                                                  :blob "analyst-2-blob"}
                                 :handoff-receipt-id "handoff-1"}))]
        {:ok (every? :ok [published access student w1 a1 w2 a2])
         :proof {:solve solve :verify verify :promotion promotion
                 :snapshot snap :student-request (:request student)
                 :analyst-first (:receipt a1) :analyst-second (:receipt a2)
                 :analyst-final-state (:state a2)}}))))
