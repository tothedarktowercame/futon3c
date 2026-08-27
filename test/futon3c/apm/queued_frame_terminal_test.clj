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
          :pin-solve-fn
          (fn [_ terminal]
            (swap! calls conj [:pin (get-in terminal [:solver :head])])
            {:status :pinned :ref (str "refs/apm/banked-solves/f40/p0/" head)})
          :persist-bank-fn #(do (swap! calls conj [:bank (:receipt/id %2)])
                                {:ok true})
          :retirement-status-fn
          (fn [_ _] {:ok true :status :not-retired})
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
    (is (= :pinned (get-in result [:bank-receipt :solve/pin-status])))
    (is (= (str "refs/apm/banked-solves/f40/p0/" head)
           (get-in result [:bank-receipt :solve/pin-ref])))
    (is (= "exp/f40" (get-in result [:bank-receipt :solver/branch])))
    (is (= [:pin head] (first @calls)))
    (is (= [:pin :bank :audit :retire :audit :retire :seats]
           (mapv first @calls)))))

(deftest solved-partial-banking-preserves-independent-learning-outcomes
  (doseq [[frame-id problem-id learning-outcome]
          [["fixture-f25" "m94A02" :partially-observed]
           ["fixture-f28" "a01A12" :observed]]]
    (let [frame {:frame/id frame-id :problem/id problem-id}
          terminal (-> (terminal-receipt frame)
                       (dissoc :receipt/id)
                       (assoc :frame/result :partial
                              :learning/outcome learning-outcome))
          terminal (assoc terminal :receipt/id
                          (machine/ledger-digest [terminal]))
          checked (sut/validate-terminal frame terminal)
          bank (sut/build-problem-bank frame terminal)]
      (is (:ok checked))
      (is (= :solved (:problem/outcome bank)))
      (is (= :partial (:frame/result bank)))
      (is (= learning-outcome (:learning/outcome bank)))
      (is (not= :closed (:frame/result bank))))))

(deftest frozen-f26-shaped-unsolved-partial-is-a-retry-not-a-solved-bank
  (let [frame {:frame/id "fixture-f26" :problem/id "m94A03"}
        body {:receipt/type :frame-terminal :frame/id (:frame/id frame)
              :problem/id (:problem/id frame) :frame/result :partial
              :problem/outcome :unsolved :learning/outcome :skipped
              :solver-progress-receipt/id digest
              :solver {:branch "exp/countdown-f26-m94A03-solver" :head head}
              :workspace/terminal-heads {:solver head :student head}}
        terminal (assoc body :receipt/id (machine/ledger-digest [body]))
        checked (sut/validate-terminal frame terminal)
        bank (sut/build-problem-bank frame terminal)]
    (is (:ok checked))
    (is (= :queued-solver-progress-bank (:receipt/type bank)))
    (is (true? (:retry/same-problem? bank)))
    (is (= :unsolved (:problem/outcome bank)))
    (is (nil? (:verify-receipt/id bank)))))

(deftest invalid-terminal-evidence-performs-no-effects
  (let [calls (atom [])
        frame {:frame/id "f40" :problem/id "p0"}
        result (sut/retire!
                {:frame frame :terminal-receipt {:receipt/type :frame-terminal}
                 :leases (leases frame)
                 :pin-solve-fn #(swap! calls conj :pin)
                 :persist-bank-fn #(swap! calls conj :bank)
                 :retirement-status-fn #(swap! calls conj :status)
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
            "holes/labs/M-apm-demonstration/generated/apm-cycle-contract-v4.json"
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
                   :pin-solve-fn
                   (fn [_ _]
                     {:status :pinned
                      :ref (str "refs/apm/banked-solves/" (:frame/id frame)
                                "/" (:problem/id frame) "/" head)})
                   :persist-bank-fn #(do (swap! banks conj %2) {:ok true})
                   :retirement-status-fn
                   (fn [_ _] {:ok true :status :not-retired})
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

(deftest f30-shaped-replay-skips-durably-retired-solver-before-student-retry
  (let [frame {:frame/id "fixture-f30" :problem/id "a01J06"}
        terminal (terminal-receipt frame)
        retired (atom {})
        audits (atom [])
        removals (atom [])
        student-attempts (atom 0)
        invoke
        (fn []
          (sut/retire!
           {:frame frame :terminal-receipt terminal :leases (leases frame)
            :pin-solve-fn
            (fn [_ _]
              {:status :pinned
               :ref (str "refs/apm/banked-solves/fixture-f30/a01J06/" head)})
            :persist-bank-fn (fn [_ _] {:ok true})
            :retirement-status-fn
            (fn [lease _]
              (if-let [receipt (get @retired (:role lease))]
                {:ok true :status :already-retired :receipt receipt}
                {:ok true :status :not-retired}))
            :audit-fn
            (fn [_ _ role _]
              (swap! audits conj role)
              (if (and (= :student role)
                       (= 1 (swap! student-attempts inc)))
                {:ok false :error/code :workspace-retirement-audit-invalid
                 :validation/findings [:workspace-dirty]}
                {:ok true :audit {:audit/id (str "audit-" (name role))}}))
            :retire-workspace-fn
            (fn [lease _]
              (swap! removals conj (:role lease))
              (let [receipt {:receipt/id (str "retired-" (name (:role lease)))}]
                (swap! retired assoc (:role lease) receipt)
                {:ok true :receipt receipt}))
            :retire-seats-fn (fn [_ _] {:ok true})}))]
    (is (= [:workspace-dirty]
           (:validation/findings (invoke))))
    (is (= [:solver] @removals))
    (let [replayed (invoke)]
      (is (:ok replayed) (pr-str replayed))
      (is (= [:solver :student] @removals))
      (is (= [:solver :student :student] @audits))
      (is (= "retired-solver"
             (get-in replayed [:workspace-receipts :solver :receipt/id]))))))

(defn- closing-effects
  [frame terminal pin-solve-fn persisted]
  {:frame frame :terminal-receipt terminal :leases (leases frame)
   :pin-solve-fn pin-solve-fn
   :persist-bank-fn (fn [_ bank]
                      (reset! persisted bank)
                      {:ok true})
   :retirement-status-fn
   (fn [_ _] {:ok true :status :not-retired})
   :audit-fn
   (fn [_ _ role _]
     {:ok true :audit {:audit/id (str "audit-" (name role))}})
   :retire-workspace-fn
   (fn [lease _]
     {:ok true :receipt {:receipt/id (str "retired-" (name (:role lease)))}})
   :retire-seats-fn (fn [_ _] {:ok true})})

(deftest refused-solve-pin-is-recorded-without-blocking-close
  (let [frame {:frame/id "f42" :problem/id "a97J07"}
        persisted (atom nil)
        result (sut/retire!
                (closing-effects
                 frame (terminal-receipt frame)
                 (fn [_ _] {:status :refused :reason :sorry-ax}) persisted))]
    (is (:ok result) (pr-str result))
    (is (= :refused (get-in result [:bank-receipt :solve/pin-status])))
    (is (= :sorry-ax (get-in result [:bank-receipt :solve/pin-reason])))
    ;; The branch is retained whether or not the solve passed the gate; the
    ;; refusal is carried by :solve/pin-status, not by restating it here.
    (is (true? (get-in result [:bank-receipt :branch-retained?])))
    (is (= #{:solver :student} (set (keys (:workspace-receipts result)))))
    (is (= (:bank-receipt result) @persisted))))

(deftest non-solved-terminal-skips-pin-effect
  (let [frame {:frame/id "f43" :problem/id "a97J08"}
        body (-> (terminal-receipt frame)
                 (dissoc :receipt/id :verify-receipt/id)
                 (assoc :frame/result :partial :problem/outcome :unsolved
                        :learning/outcome :skipped
                        :solver-progress-receipt/id digest))
        terminal (assoc body :receipt/id (machine/ledger-digest [body]))
        calls (atom 0)
        persisted (atom nil)
        result (sut/retire!
                (closing-effects frame terminal
                                 (fn [& _] (swap! calls inc)) persisted))]
    (is (:ok result) (pr-str result))
    (is (zero? @calls))
    (is (= :skipped (get-in result [:bank-receipt :solve/pin-status])))
    ;; A partial frame's branch IS retained -- this must not read false.
    (is (true? (get-in result [:bank-receipt :branch-retained?])))))

(deftest throwing-pin-effect-is-recorded-without-blocking-close
  (let [frame {:frame/id "f44" :problem/id "a97J09"}
        persisted (atom nil)
        result (sut/retire!
                (closing-effects
                 frame (terminal-receipt frame)
                 (fn [& _] (throw (ex-info "lake unavailable" {}))) persisted))]
    (is (:ok result) (pr-str result))
    (is (= :refused (get-in result [:bank-receipt :solve/pin-status])))
    (is (= :pin-effect-threw
           (get-in result [:bank-receipt :solve/pin-reason])))))

(deftest problem-bank-id-covers-solve-pin-evidence
  (let [frame {:frame/id "f45" :problem/id "a97J10"}
        terminal (terminal-receipt frame)
        pinned (sut/build-problem-bank
                frame terminal
                {:status :pinned :ref (str "refs/apm/banked-solves/f45/a97J10/" head)})
        refused (sut/build-problem-bank
                 frame terminal {:status :refused :reason :sorry-ax})]
    (is (not= (:receipt/id pinned) (:receipt/id refused)))
    (is (sut/addressed? pinned :receipt/id))
    (is (sut/addressed? refused :receipt/id))))
