;; Read-only recovery observation. Run with scripts/proof-eval.sh -f this-file.
(do
  (require 'clojure.edn 'futon3c.apm.campaign-ledger)
(let [read-doc #(clojure.edn/read-string (slurp %))
      root "data/apm-campaigns/jit-all-open-v3/"
      q (read-doc (str root "queue-state.edn"))
      c (read-doc (str root "coordinator.edn"))
      entry (get-in (read-doc "data/apm-coordinators/registry.edn")
                    [:entries "jit-queue:jit-all-open-v3"])
      observation
      {:at (str (java.time.Instant/now))
       :coordinator/enabled? (:coordinator/enabled? entry)
       :regulator/status (:regulator/status c)
       :regulator/tick-claimed? (boolean (:regulator/tick-claim c))
       :active/frame-id (get-in q [:active :frame :frame/id])
       :resumption/frame-ids (mapv #(get-in % [:frame :frame/id]) (:resumption-queue q))
       :unresolved-parks (mapv #(select-keys % [:frame/id :problem/id :error/code])
                               (filter #(= :awaiting-decision (:decision/status %)) (:parked q)))
       :recovered/frame-ids (mapv #(get-in % [:active :frame :frame/id]) (:park-recoveries q))
       :closures
       (mapv (fn [frame]
               (let [ledger (futon3c.apm.campaign-ledger/read-ledger
                             (str root "jit-all-open-v3-" frame "/ledger.edn"))
                     close (last (filter #(= :frame-close (:receipt/type %))
                                         (keep #(get-in % [:event/body :certificate])
                                               (:events ledger))))]
                 {:frame/id frame :ledger/valid? (:ok ledger)
                  :close (select-keys close [:receipt/id :receipt/result :receipt/learning-outcome])}))
             ["f223" "f224" "f225"])
       :phases (mapv (fn [[frame phase]]
                       (let [s (read-doc (str root "jit-all-open-v3-" frame "/live/" phase ".edn"))]
                         {:frame/id frame :phase phase :state/type (:state/type s)
                          :stage (:stage s) :error/code (:error/code s)
                          :receipt/id (get-in s [:receipt :receipt/id])}))
                     [["f223" "scribe-reduce"] ["f224" "guide-intervention-2-review"]
                      ["f225" "guide-intervention-1-review"]])}]
  observation)
)
