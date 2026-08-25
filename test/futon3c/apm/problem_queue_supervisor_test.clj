(ns futon3c.apm.problem-queue-supervisor-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.apm.problem-queue-supervisor :as sut]))

(def problems
  (mapv (fn [n] {:problem/id (str "p" n) :repository "/repo"
                  :revision (str "revision-" n) :path (str "p" n ".lean")
                  :blob (str "blob-" n) :classification :non-excluded})
        (range 1 6)))

(defn harness []
  (let [plan (sut/queue-plan problems) state (atom nil) calls (atom [])]
    {:plan plan :state state :calls calls
     :providers
     {:plan plan :state-provider #(deref state)
      :persist-state-fn #(do (reset! state %) (swap! calls conj [:persist])
                             {:ok true})
      :mint-frame-fn
      (fn [{:keys [problem ordinal]}]
        (swap! calls conj [:mint (:problem/id problem)])
        {:ok true :frame {:frame/id (str "q" (inc ordinal))
                          :problem/id (:problem/id problem) :problem problem}})
      :qualify-frame-fn #(do (swap! calls conj [:qualify (:frame/id %)])
                             {:ok true})
      :prepare-frame-fn #(do (swap! calls conj [:prepare (:frame/id %)])
                             {:ok true :preparation/id
                              (str "prep-" (:frame/id %))})
      :frame-tick-fn (fn [frame] (swap! calls conj [:tick (:frame/id frame)])
                       {:ok true :status :frame-complete :frame/result :closed
                        :terminal-receipt {:receipt/id
                                           (str "close-" (:frame/id frame))}})
      :retire-frame-fn #(do (swap! calls conj [:retire
                                               (get-in % [:frame :frame/id])])
                            {:ok true})}}))

(deftest five-problem-chain-provisions-only-current-frame
  (let [{:keys [providers state calls]} (harness)]
    (is (= :frame-prepared (:status (sut/tick! providers))))
    (is (= "p1" (get-in @state [:active :frame :problem/id])))
    (is (= 1 (count (filter #(= :prepare (first %)) @calls))))
    (dotimes [_ 4]
      (is (= :frame-prepared (:status (sut/tick! providers))))
      (is (= 1 (- (count (filter #(= :prepare (first %)) @calls))
                    (count (filter #(= :retire (first %)) @calls))))))
    (is (= :batch-complete (:status (sut/tick! providers))))
    (is (= 5 (count (:completed @state))))
    (is (nil? (:active @state)))
    (is (= ["p1" "p2" "p3" "p4" "p5"]
           (mapv :problem/id (:completed @state))))))

(deftest successor-is-not-minted-before-terminal-result
  (let [{:keys [providers calls]} (harness)]
    (is (= :frame-prepared (:status (sut/tick! providers))))
    (let [result (sut/tick! (assoc providers :frame-tick-fn
                                   (constantly {:ok true :status :parked})))]
      (is (= :parked (:status result)))
      (is (= 1 (count (filter #(= :mint (first %)) @calls)))))))

(deftest solver-round-exhaustion-parks-frame-and-prepares-successor
  (let [{:keys [providers state calls]} (harness)
        park {:state/type :solver-human-intervention-frame-park
              :frame/id "q1" :problem/id "p1"
              :solver/rounds-completed 50
              :solver/final-head "head-50"
              :solver/branch "exp/q1-solver"
              :solver/state-path "/campaign/q1/live/solve.edn"
              :last-valid-receipt/id "receipt-49"
              :residual "prove the missing Jordan ordering theorem"
              :student/decision :operator-required}]
    (is (= :frame-prepared (:status (sut/tick! providers))))
    (let [result
          (sut/tick!
           (assoc providers :frame-tick-fn
                  (constantly {:ok true :status :frame-parked
                               :frame/park park})))]
      (is (= :frame-prepared (:status result)))
      (is (= "p2" (get-in @state [:active :frame :problem/id])))
      (is (= [park] (:parked @state)))
      (is (empty? (filter #(= :retire (first %)) @calls)))
      (is (= ["p1" "p2"]
             (mapv second (filter #(= :mint (first %)) @calls)))))))

(deftest invalid-frame-park-does-not-advance-queue
  (let [{:keys [providers state calls]} (harness)]
    (sut/tick! providers)
    (let [before @state
          result (sut/tick!
                  (assoc providers :frame-tick-fn
                         (constantly {:ok true :status :frame-parked
                                      :frame/park
                                      {:state/type
                                       :solver-human-intervention-frame-park
                                       :frame/id "q1" :problem/id "p1"}})))]
      (is (= :problem-queue-frame-park-invalid (:error/code result)))
      (is (= before @state))
      (is (= 1 (count (filter #(= :mint (first %)) @calls)))))))

(deftest pause-after-active-retires-current-frame-without-minting-successor
  (let [{:keys [providers state calls]} (harness)]
    (is (= :frame-prepared (:status (sut/tick! providers))))
    (let [requested (sut/pause-after-active @state)]
      (is (:ok requested))
      ((:persist-state-fn providers) (:state requested)))
    (let [result (sut/tick! providers)]
      (is (= :batch-paused (:status result)))
      (is (= :paused (:status @state)))
      (is (nil? (:active @state)))
      (is (= 1 (:next-index @state)))
      (is (= 1 (count (filter #(= :mint (first %)) @calls))))
      (is (= 1 (count (:completed @state)))))))

(deftest resume-paused-preserves-cursor-and-mints-exact-successor
  (let [{:keys [providers state calls]} (harness)]
    (is (= :frame-prepared (:status (sut/tick! providers))))
    ((:persist-state-fn providers)
     (:state (sut/pause-after-active @state)))
    (is (= :batch-paused (:status (sut/tick! providers))))
    (let [paused @state
          resumed (sut/resume-paused paused)]
      (is (:ok resumed))
      (is (= (:next-index paused) (get-in resumed [:state :next-index])))
      (is (= (:completed paused) (get-in resumed [:state :completed])))
      (is (nil? (get-in resumed [:state :active])))
      ((:persist-state-fn providers) (:state resumed)))
    (is (= :frame-prepared (:status (sut/tick! providers))))
    (is (= "p2" (get-in @state [:active :frame :problem/id])))
    (is (= ["p1" "p2"]
           (mapv second (filter #(= :mint (first %)) @calls))))))

(deftest durable-intermediate-collection-statuses-remain-nonterminal
  (doseq [status [:terminal-collected :claim-recovered]]
    (let [{:keys [providers calls]} (harness)]
      (sut/tick! providers)
      (let [result (sut/tick! (assoc providers :frame-tick-fn
                                     (constantly {:ok true :status status})))]
        (is (= status (:status result)))
        (is (= 1 (count (filter #(= :mint (first %)) @calls))))))))

(deftest same-problem-retry-clears-active-without-advancing-or-minting
  (let [{:keys [providers state calls]} (harness)
        _ (sut/tick! providers)
        before-index (:next-index @state)
        receipt {:receipt/id (apply str (repeat 64 "a"))
                 :frame/result :partial :problem/outcome :partial
                 :retry/same-problem? true}
        result (sut/complete-active-without-successor @state receipt)]
    (is (:ok result))
    (is (= before-index (get-in result [:state :next-index])))
    (is (nil? (get-in result [:state :active])))
    (is (= :retry-superseded (get-in result [:state :status])))
    (is (= 1 (count (filter #(= :mint (first %)) @calls))))))

(deftest queue-plan-preserves-explicit-retained-branch
  (let [problem (assoc (first problems) :base-branch "exp/retained")]
    (is (= "exp/retained"
           (get-in (sut/queue-plan [problem]) [:problems 0 :base-branch])))))

(deftest queue-and-terminal-invariants-fail-closed
  (testing "duplicate problem"
    (let [plan (sut/queue-plan [(first problems) (first problems)])]
      (is (= :problem-queue-invalid
             (:error/code (sut/tick! (assoc (:providers (harness))
                                           :plan plan)))))))
  (testing "nonterminal completion result"
    (let [{:keys [providers]} (harness)]
      (sut/tick! providers)
      (is (= :problem-queue-terminal-result-invalid
             (:error/code
              (sut/tick! (assoc providers :frame-tick-fn
                                (constantly {:ok true :status :frame-complete
                                             :frame/result :running})))))))))
