(ns futon3c.apm.queued-frame-terminal-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.problem-queue-supervisor :as queue]
            [futon3c.apm.queued-frame-adapter :as adapter]
            [futon3c.apm.queued-frame-terminal :as sut]))

(def digest (apply str (repeat 64 "a")))
(def head (apply str (repeat 40 "b")))

(defn terminal-receipt [frame]
  (let [body {:receipt/type :frame-terminal :frame/id (:frame/id frame)
              :problem/id (:problem/id frame) :frame/result :closed
              :problem/outcome :solved :learning/outcome :observed
              :verify-receipt/id digest
              :solver {:branch (str "exp/" (:frame/id frame)) :head head}
              :workspace/terminal-heads {:solver head :student head}}]
    (assoc body :receipt/id (machine/ledger-digest [body]))))

(defn leases [frame]
  (into {} (map (fn [role]
                  [role {:workspace/id (str (:frame/id frame) "-" (name role))
                         :frame/id (:frame/id frame) :problem/id (:problem/id frame)
                         :role role}]) [:solver :student])))

(deftest terminal-banking-retains-exact-branch-and-needs-independent-audits
  (let [frame {:frame/id "f40" :problem/id "p0"}
        calls (atom [])
        result
        (sut/retire!
         {:frame frame :terminal-receipt (terminal-receipt frame)
          :leases (leases frame)
          :persist-bank-fn #(do (swap! calls conj [:bank (:receipt/id %2)])
                                {:ok true})
          :audit-fn (fn [_ _ role _]
                      (swap! calls conj [:audit role])
                      {:ok true :audit {:audit/id (str "audit-" (name role))}})
          :retire-workspace-fn
          #(do (swap! calls conj [:retire (:role %1) (:audit/id %2)])
               {:ok true :receipt {:receipt/id (str "retired-" (:role %1))}})
          :retire-seats-fn (fn [retired-frame _]
                             (swap! calls conj [:seats (:frame/id retired-frame)])
                             {:ok true})})]
    (is (:ok result))
    (is (true? (get-in result [:bank-receipt :branch-retained?])))
    (is (= "exp/f40" (get-in result [:bank-receipt :solver/branch])))
    (is (= [:bank :audit :retire :audit :retire :seats]
           (mapv first @calls)))))

(deftest frozen-f25-shaped-solved-partial-is-bankable-without-being-closed
  (let [frame {:frame/id "fixture-f25" :problem/id "m94A02"}
        terminal (-> (terminal-receipt frame)
                     (dissoc :receipt/id)
                     (assoc :frame/result :partial
                            :learning/outcome :partially-observed))
        terminal (assoc terminal :receipt/id (machine/ledger-digest [terminal]))
        checked (sut/validate-terminal frame terminal)
        bank (sut/build-problem-bank frame terminal)]
    (is (:ok checked))
    (is (= :solved (:problem/outcome bank)))
    (is (= :partial (:frame/result bank)))
    (is (= :partially-observed (:learning/outcome bank)))
    (is (not= :closed (:frame/result bank)))))

(deftest frozen-f26-shaped-unsolved-partial-is-a-retry-not-a-solved-bank
  (let [frame {:frame/id "fixture-f26" :problem/id "m94A03"}
        body {:receipt/type :frame-terminal :frame/id (:frame/id frame)
              :problem/id (:problem/id frame) :frame/result :partial
              :problem/outcome :partial :learning/outcome :skipped
              :solver-progress-receipt/id digest
              :solver {:branch "exp/countdown-f26-m94A03-solver" :head head}
              :workspace/terminal-heads {:solver head :student head}}
        terminal (assoc body :receipt/id (machine/ledger-digest [body]))
        checked (sut/validate-terminal frame terminal)
        bank (sut/build-problem-bank frame terminal)]
    (is (:ok checked))
    (is (= :queued-solver-progress-bank (:receipt/type bank)))
    (is (true? (:retry/same-problem? bank)))
    (is (= :partial (:problem/outcome bank)))
    (is (nil? (:verify-receipt/id bank)))))

(deftest invalid-terminal-evidence-performs-no-effects
  (let [calls (atom [])
        frame {:frame/id "f40" :problem/id "p0"}
        result (sut/retire!
                {:frame frame :terminal-receipt {:receipt/type :frame-terminal}
                 :leases (leases frame)
                 :persist-bank-fn #(swap! calls conj :bank)
                 :audit-fn #(swap! calls conj :audit)
                 :retire-workspace-fn #(swap! calls conj :retire)
                 :retire-seats-fn #(swap! calls conj :seats)})]
    (is (= :queued-frame-terminal-invalid (:error/code result)))
    (is (empty? @calls))))

(deftest five-problem-no-dispatch-terminal-qualification
  (let [problems (mapv (fn [n] {:problem/id (str "p" n) :repository "/repo"
                                 :revision "r" :path "Main.lean" :blob "b"
                                 :classification :non-excluded}) (range 5))
        plan (queue/queue-plan problems) state (atom nil) calls (atom [])
        prepared (atom #{}) retired (atom #{}) banks (atom [])
        providers
        {:plan plan :state-provider #(deref state)
         :persist-state-fn #(do (reset! state %) {:ok true})
         :mint-frame-fn
         (fn [{:keys [problem ordinal]}]
           (swap! calls conj [:mint ordinal])
           (adapter/mint {:problem problem :ordinal ordinal :queue/id (:queue/id plan)
                          :frame-number-base 40}))
         :qualify-frame-fn
         #(adapter/qualify-current
           {:frame %
            :generated-contract-path
            "holes/labs/M-apm-demonstration/generated/apm-cycle-contract-v3.json"
            :qualification-report-path
            "data/apm-validation/qualification-report-v1.edn"})
         :prepare-frame-fn
         (fn [frame]
           (swap! prepared conj (:frame/id frame))
           (swap! calls conj [:prepare (:frame/id frame)])
           {:ok true :preparation/id digest})
         :frame-tick-fn
         (fn [frame]
           {:ok true :status :frame-complete :frame/result :closed
            :terminal-receipt (terminal-receipt frame)})
         :retire-frame-fn
         (fn [{:keys [frame terminal-receipt]}]
           (let [result
                 (sut/retire!
                  {:frame frame :terminal-receipt terminal-receipt
                   :leases (leases frame)
                   :persist-bank-fn #(do (swap! banks conj %2) {:ok true})
                   :audit-fn (fn [_ _ role _]
                               {:ok true :audit {:audit/id (str "audit-" role)}})
                   :retire-workspace-fn
                   (fn [lease _]
                     {:ok true :receipt {:receipt/id (:workspace/id lease)}})
                   :retire-seats-fn (constantly {:ok true})})]
             (when (:ok result) (swap! retired conj (:frame/id frame)))
             (when (:ok result) (swap! calls conj [:retire (:frame/id frame)]))
             result))}]
    (is (= :frame-prepared (:status (queue/tick! providers))))
    (dotimes [_ 4]
      (let [result (queue/tick! providers)]
        (is (= :frame-prepared (:status result)))
        (is (= 1 (- (count @prepared) (count @retired))))))
    (is (= :batch-complete (:status (queue/tick! providers))))
    (is (= 5 (count @banks)))
    (is (every? :branch-retained? @banks))
    (is (= 5 (count (:completed @state))))
    (is (nil? (:active @state)))
    (is (= [0 1 2 3 4]
           (mapv second (filter #(= :mint (first %)) @calls))))
    (doseq [n (range 1 5)]
      (let [previous-retire (.indexOf @calls [:retire (str "f" (+ 39 n))])
            successor-mint (.indexOf @calls [:mint n])]
        (is (<= 0 previous-retire))
        (is (< previous-retire successor-mint))))))
