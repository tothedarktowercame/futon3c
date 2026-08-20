(ns futon3c.aif.instrumented-campaign-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.aif.instrumented-campaign :as campaign]))

(def close-action {:type :close-hole :target "close"})
(def advance-action {:type :advance-capability :target "advance"})

(def base-judgement
  {:ranked-actions
   [{:rank 1 :action close-action :G-efe -0.3 :controller-score -0.3}
    {:rank 2 :action advance-action :G-efe -0.4 :controller-score -0.4}]
   :decision {:action close-action :rank 1 :source :ordinary}
   :belief {} :belief-pre {} :observation {} :free-energy {}
   :mode :maintain})

(def r11-request
  {:root-id :campaign/shared
   :shared-budget 9
   :fields
   [{:id :repair-planner
     :budget 6
     :proposals
     [{:id :repair-deep :action {:type :unused :target "deep"}
       :cost 6 :utility 10}
      {:id :repair-compact :action close-action
       :cost 3 :utility 8}]}
    {:id :evidence-planner
     :budget 6
     :proposals
     [{:id :evidence-deep :action advance-action
       :cost 6 :utility 9}
      {:id :evidence-compact :action {:type :unused :target "compact"}
       :cost 3 :utility 6}]}]})

(def fast-state
  {:arrows {}
   :cap-overlay
   {"agency" {:id "scope/capability/agency"
              :props {:capability/frontier? false
                      :capability/status :held}}}
   :reachable #{"root"}})

(def fast-moves
  [{:move/id "close-fast" :proposal/id :repair-compact
    :move/class :close-hole :have "root" :want "fast-win"
    :score 1.0 :step-score-delta -0.3 :rank 1 :move/terminal? false}
   {:move/id "advance-cap" :proposal/id :evidence-deep
    :move/class :advance-capability :have "root" :want "cap-win"
    :advances-cap "agency" :score 1.0 :step-score-delta -0.4
    :rank 2 :move/terminal? false}])

(def accepted-corpus
  {:capabilities ["A" "B" "C"]
   :edges (vec (concat
                (repeat 10 ["A" "m1"]) (repeat 10 ["A" "m2"])
                (repeat 1 ["A" "m3"]) (repeat 10 ["B" "m1"])
                (repeat 10 ["B" "m2"]) (repeat 1 ["B" "m3"])
                (repeat 1 ["C" "m1"]) (repeat 1 ["C" "m2"])
                (repeat 20 ["C" "m3"])))
   :discharges []})

(defn tick [id]
  {:tick/id id
   :tick/as-of (str "campaign-time/" (name id))
   :runner/opts {:author "author-agent" :reviewer "reviewer-agent"}
   :r11/request r11-request
   :r15/input
   (fn [_judgement _arbitration context]
     {:state fast-state
      :moves fast-moves
      :opts {:slow-mode (get-in context [:slow-state :slow/mode])
             :horizon 1
             :top-k 5}})})

(defn mock-runner [attempts]
  (fn [opts]
    (let [transformed ((:judgement-transform-fn opts) base-judgement)
          action (get-in transformed [:decision :action])
          n (swap! attempts inc)
          attempt-id (str "attempt-" n)]
      {:attempt-id attempt-id
       :outcome :grounded-change
       :checkpoints {:selection {:judgment {:selected-action action}}}
       :data {:review-job {:job-id (str "independent-review-" n)}
              :witness {:resolved? true
                        :dial-moved? true
                        :implementation-id (str "implementation-" n)
                        :discharge-id (str "discharge-" n)}}})))

(defn campaign-input [record-fn]
  {:campaign/id "campaign-r11-r12-r15-r17"
   :initial-slow-state {:slow/mode :exploration :slow/intrinsics {}}
   :tick-a (tick :tick-a)
   :tick-b (fn [_context] (tick :tick-b))
   :r17/run
   (fn [_tick-a]
     {:run-id "campaign-r11-r12-r15-r17/intertick"
      :parent-model {:id "capability-model/v17"
                     :revision "sha256:parent"}
      :corpus accepted-corpus})
   :runner-fn (mock-runner (atom 0))
   :record-fn record-fn})

(deftest two-tick-campaign-integrates-all-four-patterns
  (let [written (atom nil)
        result (campaign/run-two-tick!
                (campaign-input
                 (fn [record]
                   (reset! written record)
                   {:ok true :evidence-id "campaign-evidence-1"})))
        [tick-a tick-b] (:campaign/ticks result)]
    (testing "R11 chooses a globally feasible portfolio on both ticks"
      (doseq [tick [tick-a tick-b]]
        (is (= #{:repair-compact :evidence-deep}
               (get-in tick [:r11 :selected-ids])))
        (is (true? (get-in tick [:r11 :within-all-budgets?])))))
    (testing "R15's witnessed Tick A changes the slow state used by Tick B"
      (is (= advance-action
             (get-in tick-a [:r15 :primary-proposal :proposal/action])))
      (is (= :consolidation
             (get-in result [:campaign/slow-state-after-tick-a :slow/mode])))
      (is (= close-action
             (get-in tick-b [:r15 :primary-proposal :proposal/action]))))
    (testing "R12 independently clears both grounded ticks"
      (is (every? #(true? (get-in % [:r12/report :gate/clear?]))
                  [tick-a tick-b])))
    (testing "R17 records a replayable accepted structural reduction"
      (is (= :structure-reduced
             (get-in result [:campaign/r17 :r17/decision :outcome])))
      (is (true? (get-in result [:campaign/replay :r17/identical?]))))
    (is (= :two-tick-evidence-complete (:campaign/status result)))
    (is (true? (:campaign/compliant? result)))
    (is (true? (get-in result [:campaign/replay :all-identical?])))
    (is (= @written (dissoc result :campaign/storage-receipt)))
    (is (= {:ok true :evidence-id "campaign-evidence-1"}
           (:campaign/storage-receipt result)))))

(deftest unconfirmed-campaign-record-is-not-success
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"record was not confirmed"
       (campaign/run-two-tick!
        (campaign-input (constantly {:ok false :error :store-unreachable}))))))
