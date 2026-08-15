(ns futon3c.smoke.traverse-test
  "Runner-only smoke traversal for the problem cycle."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [futon3c.peripheral.problem :as problem]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]))

(def ^:private env-revision
  "a92ffb6c9cda32a33df0d259df552b1dbc611daf")
(def ^:private harness-revision
  "7c743f777ccdd2b023149149f375e17bf1b1f949")
(def ^:private registration
  (edn/read-string
   (slurp "holes/labs/M-apm-demonstration/round1-registration.edn")))

(defn- smoke-problem []
  (let [clock (atom 0)]
    (problem/make-problem
     (tools/make-mock-backend)
     (fn [& _]
       {:ok true :job-id "smoke" :environment-checkout
        {:checkout "/tmp/smoke/student" :base-revision env-revision}})
     "/tmp/smoke-state"
     (fn [{:keys [arm]}]
       {:checkout (str "/tmp/smoke/" (name (or arm :solver)))
        :base-revision env-revision})
     (fn [_]
       {:harness-revision harness-revision :harness-tree-dirty? false})
     (constantly ["memory/smoke-a" "memory/smoke-b"])
     #(swap! clock inc))))

(def ^:private phase-payloads
  {:register
   {:registration registration
    :environment-revision env-revision
    :harness-revision harness-revision
    :environment-checkouts {}}
   :frame
   {:frame {:frame/id "frame-1" :frame/scaffold-hash "s"
            :frame/closing-hash "c"}
    :containment-probe {:cprobe/id "cp-1" :cprobe/frame "frame-1"
                        :cprobe/claimed? true :cprobe/recorded? true}}
   :guided-solve
   {:solver-attempt {:attempt/id "a-solver" :attempt/seq 0
                     :cycle/regime "r" :cycle/store-revision "s"
                     :cycle/runner-freshness :cold}
    :ground-control-events [] :memory-offers []}
   :intervene {:intervention {:kind :store-write}}
   :student-attempts
   {:student-attempts [{:attempt/id "a-student" :attempt/seq 1
                        :cycle/regime "r" :cycle/store-revision "s"
                        :cycle/runner-freshness :cold}]
    :memory-uses []}
   :adjudicate {:disposition [{:disp/id "d-1" :disp/cycle "c"}]}
   :promote {:promotion-result []}
   :close {}})

(def ^:private phase-tools
  (assoc
   (into {}
         (for [[phase tool-set] problem/base-phase-tools]
           [phase (vec (remove #{problem/advance} tool-set))]))
   :register
   [:read-registration :validate-registration :snapshot-store
    :freeze-stratum :assign-checkouts]))

(defn- run-phase-tools [p state phase]
  (let [tool-args
        {:assign-checkouts
         [{:problem "t94J02" :batch "frame-1" :base-rev env-revision
           :solver-seat "codex-4" :student-seat "zai-1"
           :recall-system "futon1b"}]}]
    (reduce
     (fn [{:keys [state] :as walk} tool]
       (if (:stop walk)
         (reduced walk)
         (let [result (runner/step p state
                                   {:tool tool :args (get tool-args tool [])})]
           (if (:ok result)
             {:state (:state result)}
             {:state state
              :stop {:phase phase :tool tool :code (:error/code result)}}))))
     {:state state}
     (get phase-tools phase))))

(deftest smoke-traverse-through-runner-state
  (let [p (smoke-problem)
        context {:session-id "smoke" :problem-id "t94J02"
                 :cycle/mode :store-mode
                 :harness-repo "/home/joe/code/futon3c"
                 :lean-repo "/home/joe/code/mathlib4"
                 :agency-endpoint "http://localhost:7070/api/alpha/invoke/jobs?limit=200"
                 :authorization-revision (apply str (repeat 40 "a"))
                 :authorization-output "/tmp/smoke-auth.edn"}
        begun (runner/step p (:state (runner/start p context))
                           {:tool :begin-problem-cycle :args ["M" "C"]})
        register-facts (atom nil)
        outcome
        (loop [state (:state begun), advances 0]
          (if-let [phase (:current-phase state)]
            (if (> advances 12)
              {:stop {:phase phase :code :traversal-limit}}
              (let [{tool-state :state tool-stop :stop}
                    (run-phase-tools p state phase)]
                (if tool-stop
                  {:stop tool-stop}
                  (let [advanced
                        (runner/step
                         p tool-state
                         {:tool problem/advance
                          :args ["M" "C" (get phase-payloads phase {})]})]
                    (if (:ok advanced)
                      (do
                        (when (= phase :register)
                          (reset! register-facts
                                  (select-keys
                                   (get-in advanced [:state :cycle/outputs])
                                   [:store-snapshot :stratum-frozen-at
                                    :assigned-at])))
                        (recur (:state advanced) (inc advances)))
                      {:stop {:phase phase :tool problem/advance
                              :code (:error/code advanced)}})))))
            {:completed? true :advances advances}))]
    (is (= :register (get-in begun [:state :current-phase])))
    (println "SMOKE register facts" (pr-str @register-facts))
    (println "SMOKE traversal" (pr-str outcome))
    (is (= ["memory/smoke-a" "memory/smoke-b"]
           (get-in @register-facts [:store-snapshot :snap/memory-ids])))
    (is (< (:stratum-frozen-at @register-facts)
           (:assigned-at @register-facts)))
    (is (= {:completed? true :advances 8} outcome)
        "all eight real phases advance through :completed, which clears state")))
