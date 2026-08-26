(ns futon3c.apm.conductor-test
  (:require [clojure.edn :as edn]
            [cheshire.core :as json]
            [clojure.test :refer [deftest is]]
            [futon3c.agency.registry :as agency]
            [futon3c.apm.conductor :as conductor]
            [futon3c.apm.conductor-binding :as binding]
            [futon3c.apm.conductor-surface :as conductor-surface]
            [futon3c.peripheral.problem :as problem]
            [futon3c.peripheral.tools :as tools]
            [futon3c.transport.http :as http])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute FileTime]))

(def ^:private registration-path
  "holes/labs/M-apm-demonstration/round1-registration.edn")

(def ^:private registration (edn/read-string (slurp registration-path)))
(def ^:private environment-revision (:reg/environment-revision registration))
(def ^:private harness-revision (:reg/harness-revision registration))

(defn- cascade-edge [memory-id pattern-id problem-id]
  {:hx/type :memory/assert
   :hx/props {:attachment-status :reviewed
              :state :current
              :roles {:entry memory-id
                      :patterns [pattern-id]
                      :subjects [problem-id pattern-id]}}})

(defn- cascade-readers [attachments why]
  {:attachments-fn #(get attachments % [])
   :why-targets-fn #(get why % [])})

(deftest minimum-cascade-leaf-only
  (let [edge (cascade-edge "memory/leaf" "pattern/seed" "a01A01")
        result (conductor/expand-memory-cascade
                ["memory/leaf"]
                (merge (cascade-readers
                        {"memory/leaf" [edge] "pattern/seed" [edge]
                         "a01A01" [edge]}
                        {})
                       {:cap 10}))]
    (is (= [["memory/leaf" {:route :leaf :hops 0}]] (:routes result)))
    (is (= #{:why-hop :co-incidence} (:routes-enabled result)))
    (is (= 1 (:patterns-per-problem result)))
    (is (false? (:truncated? result)))))

(deftest minimum-cascade-sibling-route-finds-other-seed-pattern-attachments
  (let [leaf (cascade-edge "memory/leaf" "pattern/seed" "a01A01")
        sibling (cascade-edge "memory/sibling" "pattern/seed" "a02A02")
        result (conductor/expand-memory-cascade
                ["memory/leaf"]
                (merge (cascade-readers
                        {"memory/leaf" [leaf]
                         "pattern/seed" [leaf sibling]}
                        {})
                       {:routes #{:sibling} :cap 10}))]
    (is (= [["memory/leaf" {:route :leaf :hops 0}]
            ["memory/sibling" {:route :sibling :hops 1
                               :pattern "pattern/seed"}]]
           (:routes result)))
    (is (= #{:sibling} (:routes-enabled result)))))

(deftest minimum-cascade-sibling-wins-an-equal-hop-why-route
  (let [leaf (cascade-edge "memory/leaf" "pattern/seed" "a01A01")
        as-sibling (cascade-edge "memory/shared" "pattern/seed" "a02A02")
        as-why (cascade-edge "memory/shared" "pattern/why" "a03A03")
        result (conductor/expand-memory-cascade
                ["memory/leaf"]
                (merge (cascade-readers
                        {"memory/leaf" [leaf]
                         "pattern/seed" [leaf as-sibling]
                         "pattern/why" [as-why]}
                        {"pattern/seed" ["pattern/why"]})
                       {:routes #{:sibling :why-hop} :cap 10}))]
    (is (= {:route :sibling :hops 1 :pattern "pattern/seed"}
           (second (some #(when (= "memory/shared" (first %)) %)
                         (:routes result)))))))

(deftest minimum-cascade-empty-route-set-yields-only-leaves
  (let [leaf (cascade-edge "memory/leaf" "pattern/seed" "a01A01")
        sibling (cascade-edge "memory/sibling" "pattern/seed" "a02A02")
        result (conductor/expand-memory-cascade
                ["memory/leaf"]
                (merge (cascade-readers
                        {"memory/leaf" [leaf]
                         "pattern/seed" [leaf sibling]}
                        {"pattern/seed" ["pattern/why"]})
                       {:routes #{} :cap 10}))]
    (is (= [["memory/leaf" {:route :leaf :hops 0}]] (:routes result)))
    (is (zero? (:expanded-available result)))
    (is (= #{} (:routes-enabled result)))))

(deftest minimum-cascade-follows-authored-why-hops
  (let [leaf (cascade-edge "memory/leaf" "pattern/seed" "a01A01")
        hop1 (cascade-edge "memory/one" "pattern/one" "a02A02")
        hop2 (cascade-edge "memory/two" "pattern/two" "a03A03")
        result (conductor/expand-memory-cascade
                ["memory/leaf"]
                (merge (cascade-readers
                        {"memory/leaf" [leaf] "pattern/seed" [leaf]
                         "a01A01" [leaf]
                         "pattern/one" [hop1] "pattern/two" [hop2]}
                        {"pattern/seed" ["pattern/one"]
                         "pattern/one" ["pattern/two"]})
                       {:cap 10}))]
    (is (= [["memory/leaf" {:route :leaf :hops 0}]
            ["memory/one" {:route :why-hop :hops 1
                           :pattern "pattern/one"}]
            ["memory/two" {:route :why-hop :hops 2
                           :pattern "pattern/two"}]]
           (:routes result)))))

(deftest minimum-cascade-keeps-the-cheapest-route
  (let [leaf (cascade-edge "memory/leaf" "pattern/seed" "a01A01")
        via-why (cascade-edge "memory/shared" "pattern/why" "a02A02")
        via-coincidence (cascade-edge "memory/shared" "pattern/co" "a01A01")
        result (conductor/expand-memory-cascade
                ["memory/leaf"]
                (merge (cascade-readers
                        {"memory/leaf" [leaf] "pattern/seed" [leaf]
                         "a01A01" [leaf via-coincidence]
                         "pattern/why" [via-why]
                         "pattern/co" [via-coincidence]}
                        {"pattern/seed" ["pattern/why"]})
                       {:cap 10}))]
    (is (= {:route :why-hop :hops 1 :pattern "pattern/why"}
           (second (some #(when (= "memory/shared" (first %)) %) (:routes result)))))))

(deftest minimum-cascade-receipts-its-cap
  (let [leaf (cascade-edge "memory/leaf" "pattern/seed" "a01A01")
        edges (into {} (for [n (range 3)]
                         [(str "pattern/" n)
                          [(cascade-edge (str "memory/" n)
                                         (str "pattern/" n)
                                         (str "a0" (inc n) "A0" (inc n)))]]))
        result (conductor/expand-memory-cascade
                ["memory/leaf"]
                (merge (cascade-readers
                        (merge {"memory/leaf" [leaf]
                                "pattern/seed" [leaf]
                                "a01A01" [leaf]}
                               edges)
                        {"pattern/seed" ["pattern/0" "pattern/1" "pattern/2"]})
                       {:cap 2}))]
    (is (= 3 (:expanded-available result)))
    (is (= 2 (:expanded-count result)))
    (is (= 3 (count (:routes result))) "leaf plus two expanded memories")
    (is (true? (:truncated? result)))
    (is (= 2 (:cap result)))))

(deftest enabled-cascade-offer-exposes-density-and-truncation
  (let [expansion-opts (atom nil)]
    (with-redefs [conductor/expand-memory-cascade
                  (fn [_ opts]
                    (reset! expansion-opts opts)
                    {:routes [["memory/leaf" {:route :leaf :hops 0}]
                              ["memory/extra" {:route :why-hop :hops 1}]]
                     :patterns-per-problem 3
                     :cap (:cap opts)
                     :expanded-available 101
                     :truncated? true})]
      (let [offers (conductor/cascade-receipt-offers
                    {:body {:job-id "job-cascade"
                            :memory-use {:memory-use/surfaced-ids
                                         ["memory/leaf"]}}}
                    {:memory-cascade-enabled? true
                     :memory-cascade-cap 37
                     :memory-cascade-routes #{:sibling}})]
        (is (= 37 (:cap @expansion-opts)))
        (is (= #{:sibling} (:routes @expansion-opts)))
        (is (= [:leaf :why-hop] (mapv :offer/route offers)))
        (is (= [0 1] (mapv :offer/hops offers)))
        (is (every? #(= 3 (:offer/patterns-per-problem %)) offers))
        (is (every? #(= 37 (:offer/cascade-cap %)) offers))
        (is (every? true? (map :offer/cascade-truncated? offers)))
        (is (every? #(= 101 (:offer/cascade-expanded-available %)) offers))))))

(deftest domain-general-pattern-family-classification
  (let [cases
        {"math-formalization-CA/measure-integration-api" false
         "math-strategy/missing-dependency-protocol" true
         "math-strategy/proof-architecture" true
         "math-formalization-CV/entire-and-singularity-api" false
         "math-formalization-FA/weak-convergence-hilbert" false
         "math-formalization-CA/series-evaluation-api" false
         "math-formalization-FA/inner-product-space-api" false
         "math-formalization-CA/uniform-continuity-boundedness" false
         "math-informal/convert-growth-counts-to-summability" true
         "math-strategy/structural-obstruction-as-theorem" true
         "math-formalization/separate-proof-transfer-from-artifact-replay" true}]
    (doseq [[pattern-id expected] cases]
      (is (= expected (conductor/domain-general-pattern-id? pattern-id))
          pattern-id))))

(deftest cascade-offers-domain-general-patterns-before-routed-memories
  (with-redefs [conductor/expand-memory-cascade
                (fn [_ _]
                  {:routes
                   [["memory/leaf" {:route :leaf :hops 0}]
                    ["memory/general-1"
                     {:route :co-incidence :hops 2
                      :pattern "math-strategy/x"}]
                    ["memory/specific"
                     {:route :co-incidence :hops 2
                      :pattern "math-formalization-CA/y"}]
                    ["memory/general-2"
                     {:route :co-incidence :hops 2
                      :pattern "math-strategy/x"}]]
                   :pattern-surfaces
                   {"math-strategy/x"
                    {:entity
                     {:entity/props
                      {:pattern/id "math-strategy/x"
                       :pattern/context "Recognize the transferable context."
                       :pattern/then "Apply the general move."}}}}
                   :patterns-per-problem 2
                   :cap 2
                   :expanded-available 3
                   :truncated? true})]
    (let [offers
          (vec
           (conductor/cascade-receipt-offers
            {:body {:job-id "job-patterns"
                    :memory-use
                    {:memory-use/surfaced-ids ["memory/leaf"]}}}
            {:memory-cascade-enabled? true :memory-cascade-cap 2}))
          pattern-offers (filterv #(= :pattern (:offer/route %)) offers)
          positions (into {} (map-indexed (fn [i offer]
                                            [(or (:offer/pattern-id offer)
                                                 (:offer/memory-id offer)) i])
                                          offers))]
      (is (= [:leaf :pattern :co-incidence :co-incidence :co-incidence]
             (mapv :offer/route offers)))
      (is (= ["math-strategy/x"] (mapv :offer/pattern-id pattern-offers)))
      (is (= 2 (:offer/routed-count (first pattern-offers))))
      (is (nil? (:offer/memory-id (first pattern-offers))))
      (is (= "Apply the general move."
             (get-in (first pattern-offers)
                     [:offer/pattern-content :pattern/then])))
      (is (< (get positions "math-strategy/x")
             (get positions "memory/general-1")))
      (is (not-any? #(= "math-formalization-CA/y"
                        (:offer/pattern-id %))
                    offers))
      (is (= 2 (:offer/cascade-cap (first pattern-offers)))
          "pattern offers are added after capped memory expansion"))))

(deftest cascade-pattern-offer-promotes-flat-hook-and-body
  (with-redefs [conductor/expand-memory-cascade
                (fn [_ _]
                  {:routes
                   [["memory/one"
                     {:route :co-incidence :hops 2
                      :pattern "math-strategy/flat"}]]
                   :pattern-surfaces
                   {"math-strategy/flat"
                    {:hook "Notice the reusable move."
                     :body "Apply it independently of the subject."}}
                   :patterns-per-problem 1
                   :cap 100
                   :expanded-available 1
                   :truncated? false})]
    (let [offer (first
                 (conductor/cascade-receipt-offers
                  {:body {:job-id "job-flat"
                          :memory-use {:memory-use/surfaced-ids []}}}
                  {:memory-cascade-enabled? true}))]
      (is (= :pattern (:offer/route offer)))
      (is (= "Notice the reusable move." (:offer/pattern-hook offer)))
      (is (= "Apply it independently of the subject."
             (:offer/pattern-body offer))))))

;; The frozen round-1 EDN predates the seat-key gate (:unstaffed-carded-seat,
;; merged with feat/registration-seat-keys) and must not be edited, so the
;; fixture stages a staffed copy under a temp path for the machine to read.
(def ^:private staffed-registration
  (assoc registration
         :reg/guide-seat "conductor-test"
         :reg/proctor-seat "proctor-test"
         :reg/scribe-seat "scribe-test"
         :reg/student-seat "zai-1"))

(defn- fixture []
  (let [state-root (.toFile
                    (Files/createTempDirectory
                     "conductor-state-" (make-array FileAttribute 0)))
        scaffold (Files/createTempFile "conductor-scaffold-" ".lean"
                                       (make-array FileAttribute 0))
        closing (Files/createTempFile "conductor-closing-" ".lean"
                                      (make-array FileAttribute 0))
        witness (Files/createTempFile "conductor-witness-" ".edn"
                                      (make-array FileAttribute 0))
        authorization (Files/createTempFile "conductor-authorization-" ".edn"
                                            (make-array FileAttribute 0))
        staffed-reg (Files/createTempFile "conductor-registration-" ".edn"
                                          (make-array FileAttribute 0))
        deposit-seq (atom 0)
        dispatch-fn
        (fn [opts _]
          {:ok true :job-id "job-test" :sent-opts opts
           :evidence {:body {:job-id "job-test"
                             :eligible-memory-ids ["memory/a" "memory/b"]
                             :memory-use
                             {:memory-use/surfaced-ids ["memory/a"]}}}})
        provisioner
        (fn [{:keys [arm branch batch]}]
          {:checkout (str "/tmp/conductor/" arm)
           :base-revision environment-revision
           :branch branch :frame/id (str batch "-" arm) :batch batch})
        peripheral
        (problem/make-problem
         (tools/make-mock-backend) dispatch-fn (.getPath state-root) provisioner
         (fn [_] {:harness-revision harness-revision
                  :harness-tree-dirty? false})
         (constantly ["memory/a" "memory/b"])
         (constantly 0)
         (fn [_ _]
           {:ok true :id (str "memory/deposit-" (swap! deposit-seq inc))}))]
    (spit (.toFile scaffold) "scaffold\n")
    (spit (.toFile closing) "closing\n")
    (spit (.toFile witness) "{:contained? true}\n")
    (spit (.toFile staffed-reg) (pr-str staffed-registration))
    (Files/setLastModifiedTime scaffold (FileTime/fromMillis 1000))
    (Files/setLastModifiedTime closing (FileTime/fromMillis 2000))
    {:config
     {:session-id "conductor-test" :problem-id "t94J02" :mode :store-mode
      :registration-path (str staffed-reg)
      :frame {:scaffold-path scaffold :closing-path closing
              :witness-path witness}
      :checkout {:batch "conductor-test" :base-rev environment-revision
                 :solver-seat "codex-4" :student-seat "zai-1"
                 :recall-system "futon1b"}
      :evidence-store (atom {:entries {} :order []})
      :harness-repo "/harness" :lean-repo "/lean"
      :agency-endpoint "http://127.0.0.1:1/unreachable"
      :authorization-revision (apply str (repeat 40 "a"))
      :authorization-output (str authorization)
      :conductor "conductor-test" :peripheral peripheral}
     :paths [scaffold closing witness authorization staffed-reg]}))

(defn- solver-attempt []
  {:attempt/id "attempt/solver" :attempt/seq 0
   :cycle/regime "round-1" :cycle/store-revision "store-1"
   :cycle/runner-freshness :cold})

(defn- student-attempt []
  {:attempt/id "attempt/student" :attempt/seq 1
   :cycle/regime "round-1" :cycle/store-revision "store-2"
   :cycle/runner-freshness :cold})

(defn- close-ready-handle [config]
  (let [opened (conductor/open-frame! config)
        solver (conductor/dispatch-solver! opened {:mission "M-test"} "packet")
        solver-recorded (conductor/record-solver-attempt!
                         solver (solver-attempt) {})
        intervened (conductor/deposit!
                    solver-recorded
                    {:name "close-test-deposit" :kind :feedback :hook "test"
                     :body {:lesson "advance through intervene"}
                     :subjects [{:ref/type :problem :ref/id "t94J02"}]})
        student (conductor/dispatch-student!
                 intervened {:mission "M-test"} "student packet")
        students-recorded (conductor/record-students!
                           student [(student-attempt)] [])]
    (conductor/adjudicate!
     students-recorded
     {:outcome :tier-a :residual-sorries 1 :axiom-clean? false})))

(deftest close-emits-one-analyst-wake-even-for-valid-failure-envelope
  (let [{:keys [config paths]} (fixture)
        wakes (atom [])
        config (assoc config
                      :analyst-seat "analyst-test"
                      :close-hook (fn [wake]
                                    (swap! wakes conj wake)
                                    {:status :sent}))]
    (try
      (with-redefs [agency/get-agent (fn [seat]
                                      (when (= "analyst-test" seat)
                                        {:agent/id seat}))]
        (let [closed (conductor/close! (close-ready-handle config))
              wake (first @wakes)
              payload (:payload wake)]
          (is (:ok closed) (pr-str (:error closed)))
          (is (= 1 (count @wakes)))
          (is (= :sent (get-in closed [:analyst-wake :status])))
          (is (= "t94J02" (:problem-id payload)))
          (is (= (:cycle-id closed) (:cycle-id payload)))
          (is (false? (:launchable? payload)))
          (is (= (count (get-in closed [:envelope :failures]))
                 (:failure-count payload)))
          (is (pos? (:failure-count payload))
              "a completed refusal envelope still wakes the Analyst")))
      (finally
        (doseq [path paths] (Files/deleteIfExists path))))))

(deftest close-without-analyst-seat-still-completes
  (let [{:keys [config paths]} (fixture)]
    (try
      (let [closed (conductor/close! (close-ready-handle config))]
        (is (:ok closed) (pr-str (:error closed)))
        (is (nil? (get-in closed [:state :current-phase])))
        (is (= {:status :skipped
                :reason :analyst-seat-not-configured}
               (select-keys (:analyst-wake closed) [:status :reason]))))
      (finally
        (doseq [path paths] (Files/deleteIfExists path))))))

(deftest unregistered-analyst-seat-is-loud-and-non-fatal
  (let [{:keys [config paths]} (fixture)
        hook-called? (atom false)
        config (assoc config
                      :analyst-seat "missing-analyst"
                      :close-hook (fn [_]
                                    (reset! hook-called? true)
                                    {:status :sent}))]
    (try
      (with-redefs [agency/get-agent (constantly nil)]
        (let [closed (conductor/close! (close-ready-handle config))]
          (is (:ok closed) (pr-str (:error closed)))
          (is (= {:status :skipped
                  :reason :analyst-seat-unregistered
                  :analyst-seat "missing-analyst"}
                 (select-keys (:analyst-wake closed)
                              [:status :reason :analyst-seat])))
          (is (false? @hook-called?))))
      (finally
        (doseq [path paths] (Files/deleteIfExists path))))))

(deftest incomplete-close-never-wakes-analyst
  (let [wakes (atom [])
        closed (with-redefs [agency/get-agent (constantly {:agent/id "analyst-test"})]
                 (conductor/close!
                  {:ok true :peripheral nil :state nil :log [] :deposits []
                   :config {:problem-id "broken"
                            :analyst-seat "analyst-test"
                            :close-hook #(swap! wakes conj %)}}))]
    (is (false? (:ok closed)))
    (is (= :close-incomplete (get-in closed [:analyst-wake :reason])))
    (is (empty? @wakes))))

(deftest conductor-runs-a-refused-cycle-and-keeps-its-rider-ledger
  (let [{:keys [config paths]} (fixture)]
    (try
      (let [opened (conductor/open-frame! config)
            missing-mission (conductor/dispatch-solver! opened {} "packet")
            solver (conductor/dispatch-solver! opened {:mission "M-test"} "packet")
            intervening (conductor/record-solver-attempt!
                         solver (solver-attempt) {})
            deposited (conductor/deposit!
                       intervening
                       {:name "deposit" :kind :feedback :hook "test"
                        :body {:lesson "ledger"}
                        :subjects [{:ref/type :problem :ref/id "t94J02"}]})
            student (conductor/dispatch-student!
                     deposited {:mission "M-test" :to "caller-spoofed-seat"}
                     "student packet")
            adjudicating (conductor/record-students!
                          student [(student-attempt)] [])
            closing (conductor/adjudicate!
                     adjudicating
                     {:outcome :tier-a :residual-sorries 1 :axiom-clean? false
                      :promotion-result
                      [{:artifact-id "artifact/backward-compatible"
                        :importable? true :need-tags ["compat"]}]})
            closed (conductor/close! closing)]
        (is (:ok opened) (pr-str (:error opened)))
        (is (= :guided-solve (get-in opened [:state :current-phase])))
        (is (= :mission-absent (get-in missing-mission [:error :error/code])))
        (is (= ["memory/deposit-1"] (:deposits deposited)))
        (is (= :promote (get-in closing [:state :current-phase]))
            "adjudicate parks at the explicit post-adjudication work phase")
        (is (= ["artifact/backward-compatible"]
               (->> (get-in closing [:state :steps])
                    (filter #(= :promote-artifact (:tool %)))
                    (mapv #(get-in % [:result :promo/artifact-id]))))
            "legacy adjudication promotions are recorded without consuming the phase")
        (is (= "zai-1"
               (->> (get-in student [:state :steps])
                    (filter #(= :dispatch-student-fresh (:tool %)))
                    last :result :sent-opts :to))
            "the registered student seat overrides a caller-supplied :to")
        (is (nil? (get-in closed [:state :current-phase]))
            "the final advance reaches the terminal sentinel")
        (is (false? (get-in closed [:envelope :launchable?])))
        (is (seq (get-in closed [:envelope :failures]))
            "round-one closes with an honest refusal envelope")
        (is (= [{:offer/id "offer/job-test/0"
                 :offer/memory-id "memory/a"
                 :offer/route :leaf
                 :offer/hops 0}]
               (get-in closed [:state :cycle/outputs :memory-offers]))
            "cascade-off preserves the old offer and labels it as a leaf")
        (is (not-any? #{:malformed-memory-offers}
                      (get-in closed [:envelope :failures]))
            "conductor-collected receipts validate as memory-offer entities"))
      (finally
        (doseq [path paths] (Files/deleteIfExists path))))))

(deftest resume-loads-a-checkpoint-and-can-continue
  (let [{:keys [config paths]} (fixture)]
    (try
      (let [opened (conductor/open-frame! config)
            version (count (filter #(= :problem-save (:tool %)) (:log opened)))
            resumed (conductor/resume opened (:cycle-id opened) version)
            continued (conductor/dispatch-solver!
                       resumed {:mission "M-test"} "continued packet")]
        (is (:ok opened) (pr-str (:error opened)))
        (is (:ok resumed) (pr-str (:error resumed)))
        (is (= :guided-solve (get-in resumed [:state :current-phase])))
        (is (:ok continued) (pr-str (:error continued)))
        (is (= :dispatch-solver (:tool (last (remove #(= :problem-save (:tool %))
                                                     (:log continued)))))))
      (finally
        (doseq [path paths] (Files/deleteIfExists path))))))

(deftest conductor-requires-and-records-typed-guidance
  (let [{:keys [config paths]} (fixture)]
    (try
      (let [opened (conductor/open-frame! config)
            untyped (conductor/guide-solver!
                     opened {:mission "M-test"} "untyped guidance")
            typed (conductor/guide-solver!
                   opened :suggest {:mission "M-test"} "typed guidance")]
        (is (= :guidance-type-absent
               (get-in untyped [:error :error/code])))
        (is (:ok typed) (pr-str (:error typed)))
        (is (= :suggest
               (->> (get-in typed [:state :steps])
                    (filter #(= :guide-solver (:tool %)))
                    last :result :ground-control/type))))
      (finally
        (doseq [path paths] (Files/deleteIfExists path))))))

(deftest conductor-dispatches-the-registered-scribe-only-at-promote
  (let [{:keys [config paths]} (fixture)
        agent-id "scribe-dispatch-guide"
        session-id "scribe-dispatch-session"]
    (try
      (let [opened (conductor/open-frame! config)
            out-of-phase (conductor/dispatch-scribe!
                          opened {:mission "M-test"} "mine this cycle")
            promoted (-> opened
                         (assoc-in [:state :current-phase] :promote)
                         (assoc-in [:state :cycle/outputs :registration
                                    :reg/role-cards :scribe]
                                   "02441d9df4b8a05355790a51f1e535bf9e9465d4")
                         (update-in [:state :steps] conj
                                    {:tool :dispatch-solver
                                     :result {:job-id "solver-job"}}
                                    {:tool :dispatch-student-fresh
                                     :result {:job-id "student-job"}}))
            dispatched (conductor/dispatch-scribe!
                        promoted {:mission "M-test"} "mine this cycle")]
        (is (false? (:ok out-of-phase)))
        (is (:ok dispatched) (pr-str (:error dispatched)))
        (is (= :dispatch-scribe
               (->> (get-in dispatched [:state :steps])
                    (remove #(= :problem-save (:tool %))) last :tool)))
        (is (= "scribe-test"
               (->> (get-in dispatched [:state :steps])
                    (filter #(= :dispatch-scribe (:tool %))) last
                    :result :ground-control/recipient)))
        (let [sent-opts (->> (get-in dispatched [:state :steps])
                             (filter #(= :dispatch-scribe (:tool %))) last
                             :result :sent-opts)]
          (is (= "t94J02" (:problem-id sent-opts)))
          (is (= (:cycle-id promoted) (:cycle-id sent-opts)))
          (is (= ["solver-job"] (:solver-job-ids sent-opts)))
          (is (= ["student-job"] (:student-job-ids sent-opts)))
          (is (= "/home/joe/code/futon3c/holes/labs/M-apm-demonstration/role-cards/scribe-v2.md"
                 (:scribe-card-path sent-opts))))
        (let [unresolved (conductor/dispatch-scribe!
                          (assoc-in promoted
                                    [:state :cycle/outputs :registration
                                     :reg/role-cards :scribe]
                                    (apply str (repeat 40 "f")))
                          {:mission "M-test"} "mine this cycle")]
          (is (false? (:ok unresolved)))
          (is (= :scribe-card-unresolved (get-in unresolved [:error :error/code])))
          (is (= (apply str (repeat 40 "f"))
                 (get-in unresolved [:error :error/context :pinned-blob]))))
        (agency/register-agent!
         {:agent-id agent-id :type :claude
          :invoke-fn (fn [_ _] {:result "unused" :session-id session-id})})
        (agency/update-agent! agent-id :agent/session-id session-id)
        (is (:ok (binding/install! agent-id session-id promoted)))
        (let [{:keys [cycle-id version]} (binding/status agent-id session-id)
              routed
              (conductor-surface/execute-action!
               agent-id session-id
               {:action-id "scribe-1" :cycle-id cycle-id :version version
                :operation :dispatch-scribe
                :args [{:mission "M-test"} "mine through surface"]})]
          (is (:ok routed) (pr-str routed))))
      (finally
        (binding/reset-bindings!)
        (agency/unregister-agent! agent-id)
        (doseq [path paths] (Files/deleteIfExists path))))))

(deftest open-frame-refuses-invalid-mode-and-threads-conductor
  (let [{:keys [config]} (fixture)]
    (let [bad (conductor/open-frame! (assoc config :mode nil))]
      (is (false? (:ok bad)))
      (is (re-find #"store-mode" (or (:error/message bad) (str bad)))))
    (let [h (conductor/open-frame! (assoc config
                                          :mode :store-mode
                                          :deposit-state :with-deposit))]
      (is (not (false? (:ok h))))
      (is (= :store-mode (get-in h [:state :cycle/mode])))
      (is (= :with-deposit (get-in h [:state :cycle/deposit-state]))))))

(deftest conductor-surface-authenticates-promotion-reviewer
  (let [agent-id "claude-review-actor"
        session-id "review-actor-session"
        captured (atom nil)
        promotion {:memory-id "e-memory"
                   :pattern-id "p4ng/pattern"
                   :reviewer agent-id}
        action {:action-id "review-action"
                :cycle-id "cycle-review"
                :version 1
                :operation :adjudicate
                :args [{:outcome :closed :promotion-result [promotion]}]}]
    (agency/register-agent!
     {:agent-id agent-id :type :claude
      :invoke-fn (fn [_ _] {:result "unused" :session-id session-id})
      :session-id session-id})
    (with-redefs [binding/execute!
                  (fn [_ _ routed _ _]
                    (reset! captured routed)
                    {:ok true})]
      (is (:ok (conductor-surface/execute-action!
                agent-id session-id action)))
      (is (= agent-id
             (get-in @captured [:args 0 :promotion-result 0
                                :acting-identity])))
      (let [mismatched (assoc-in action
                                 [:args 0 :promotion-result 0 :reviewer]
                                 "some-other-reviewer")
            result (conductor-surface/execute-action!
                    agent-id session-id mismatched)]
        (is (false? (:ok result)))
        (is (= :reviewer-not-actor (:error/code result)))
        (is (= :reviewer-not-actor
               (get-in result [:finding :failure])))))))

(deftest conductor-surface-decodes-and-validates-promotion-verdict
  (let [agent-id "claude-review-verdict"
        session-id "review-verdict-session"
        reached-verdict (atom nil)
        action {:action-id "review-verdict-action"
                :cycle-id "cycle-review" :version 1
                :operation :promote-artifact
                :args [{:artifact-id "artifact/reviewed"
                        :reviewer agent-id
                        :verdict "approve"}]}]
    (agency/register-agent!
     {:agent-id agent-id :type :claude
      :invoke-fn (fn [_ _] {:result "unused" :session-id session-id})
      :session-id session-id})
    (try
      (with-redefs [conductor/promote-artifact!
                    (fn [handle opts]
                      (reset! reached-verdict (:verdict opts))
                      handle)
                    binding/execute!
                    (fn [_ _ routed executor _]
                      (executor {:ok true} (:operation routed) (:args routed))
                      {:ok true})]
        (is (:ok (conductor-surface/execute-action!
                  agent-id session-id action)))
        (is (= :approve @reached-verdict)
            "the promotion lifecycle receives the keyword verdict")
        (let [invalid (conductor-surface/execute-action!
                       agent-id session-id
                       (assoc-in action [:args 0 :verdict] "rubber-stamp"))]
          (is (false? (:ok invalid)))
          (is (= :promotion-verdict-invalid (:error/code invalid)))
          (is (= :promotion-verdict-invalid
                 (get-in invalid [:finding :failure])))))
      (finally
        (agency/unregister-agent! agent-id)))))

(deftest promote-phase-tools-are-conductor-and-surface-routable
  (let [{:keys [config paths]} (fixture)
        agent-id "promote-guide"
        session-id "promote-guide-session"]
    (try
      (let [opened (conductor/open-frame! config)
            refused (conductor/promote-artifact!
                     opened {:artifact-id "artifact/too-early"})
            promote-state (assoc-in opened [:state :current-phase]
                                    :promote-solver)
            promoted (conductor/promote-artifact!
                      promote-state
                      {:artifact-id "artifact/solver"
                       :importable? true :need-tags ["solver"]})
            wrong-author
            (conductor/record-scribe-lanes!
             promoted {:lane :solve :ran? true :yield []
                       :author "not-the-scribe"})
            recorded
            (conductor/record-scribe-lanes!
             promoted {:lane :solve :ran? true
                       :yield ["memory/solver"] :author "scribe-test"})]
        (is (false? (:ok refused)) "the engine keeps phase authority")
        (is (:ok promoted) (pr-str (:error promoted)))
        (is (= :promote-artifact
               (->> (get-in promoted [:state :steps])
                    (remove #(= :problem-save (:tool %))) last :tool)))
        (is (false? (:ok wrong-author)) "P4 rejects a non-scribe author")
        (is (:ok recorded) (pr-str (:error recorded)))
        (is (= "scribe-test"
               (->> (get-in recorded [:state :steps])
                    (filter #(= :record-scribe-lanes (:tool %)))
                    last :result :author)))

        (agency/register-agent!
         {:agent-id agent-id :type :claude
          :invoke-fn (fn [_ _] {:result "unused" :session-id session-id})
          :session-id session-id})
        (is (:ok (binding/install! agent-id session-id promote-state)))
        (let [{:keys [cycle-id version]} (binding/status agent-id session-id)
              mismatched
              (conductor-surface/execute-action!
               agent-id session-id
               {:action-id "promotion-wrong-reviewer"
                :cycle-id cycle-id :version version
                :operation :promote-artifact
                :args [{:artifact-id "artifact/reviewed"
                        :reviewer "scribe-test"}]})]
          (is (= :reviewer-not-actor (:error/code mismatched))
              "P14 forbids the guide from impersonating the scribe")
          (let [routed-promotion
                (conductor-surface/execute-action!
                 agent-id session-id
                 {:action-id "promotion-by-actor"
                  :cycle-id cycle-id :version version
                  :operation :promote-artifact
                  :args [{:artifact-id "artifact/reviewed"
                          :reviewer agent-id}]})
                next-version (:version (:receipt routed-promotion))
                routed-lane
                (conductor-surface/execute-action!
                 agent-id session-id
                 {:action-id "scribe-lane-record"
                  :cycle-id cycle-id :version next-version
                  :operation :record-scribe-lanes
                  :args [{:lane "solve" :ran? true :yield ["memory/solver"]
                          :author "scribe-test"}]})]
            (is (:ok routed-promotion) (pr-str routed-promotion))
            (is (:ok routed-lane) (pr-str routed-lane))
            (let [lane-version (:version (:receipt routed-lane))
                  unknown-lane
                  (conductor-surface/execute-action!
                   agent-id session-id
                   {:action-id "scribe-lane-unknown"
                    :cycle-id cycle-id :version lane-version
                    :operation :record-scribe-lanes
                    :args [{:lane "not-a-lane" :ran? true :yield []
                            :author "scribe-test"}]})]
              (is (false? (:ok unknown-lane)))
              (is (= :tool-execution-failed
                     (get-in unknown-lane [:error :error/code])))))))
      (finally
        (binding/reset-bindings!)
        (agency/unregister-agent! agent-id)
        (doseq [path paths] (Files/deleteIfExists path))))))

(deftest conductor-action-route-owns-one-live-handle
  (let [{:keys [config paths]} (fixture)
        agent-id "claude-7"
        session-id "conductor-surface-session"
        handler (http/make-handler {})
        request!
        (fn [payload]
          (let [response
                (handler
                 {:request-method :post :uri "/api/alpha/conductor/action"
                  :body (json/generate-string
                         (merge {:agent-id agent-id :session-id session-id}
                                payload))})]
            (cond-> (assoc (json/parse-string (:body response) true)
                           :http/status (:status response))
              (string? (:error/code (json/parse-string (:body response) true)))
              (update :error/code keyword))))
        status!
        (fn [agent session]
          (let [response
                (handler
                 {:request-method :get :uri "/api/alpha/conductor/status"
                  :query-string (str "agent-id=" agent "&session-id=" session)})]
            (json/parse-string (:body response) true)))
        action!
        (fn [id operation args]
          (let [{:keys [cycle-id version]} (binding/status agent-id session-id)]
            (request! {:action-id id :operation (name operation) :args args
                       :cycle-id cycle-id :version version})))]
    (binding/reset-bindings!)
    (agency/register-agent!
     {:agent-id agent-id :type :claude
      :invoke-fn (fn [_ _] {:result "unused" :session-id session-id})
      :session-id session-id})
    (agency/register-agent!
     {:agent-id "claude-unbound" :type :claude
      :invoke-fn (fn [_ _] {:result "unused" :session-id "no-session"})
      :session-id "no-session"})
    (try
      (let [opened (conductor/open-frame!
                    (assoc config :conductor
                           {:agent agent-id :session session-id
                            :surface "problem-conductor"}))
            before (count (get-in opened [:state :steps]))
            outputs-before (get-in opened [:state :cycle/outputs])
            out-of-phase (action! "a-wrong" :dispatch-student
                                  [{:mission "M-test"} "TOP-SECRET-PACKET"])
            refused-handle @(:handle (binding/lookup agent-id session-id))
            refusal (-> refused-handle :state :cycle/action-refusals first)
            after-refusal (count (get-in refused-handle [:state :steps]))
            dispatched (action! "a-solver" :dispatch-solver
                                [{:mission "M-test"} "solver"])
            after-dispatch @(:handle (binding/lookup agent-id session-id))
            replay (let [{:keys [cycle-id]} (binding/status agent-id session-id)]
                     (request! {:action-id "a-solver" :operation "dispatch-solver"
                                :args [{:mission "M-test"} "solver"]
                                :cycle-id cycle-id
                                :version (binding/handle-version after-dispatch)}))]
        (is (:ok opened) (pr-str (:error opened)))
        (is (= :phase-tool-not-allowed (:error/code out-of-phase)))
        (is (= (inc before) after-refusal)
            "a refused action durably checkpoints exactly once")
        (is (= :problem-save (-> refused-handle :state :steps last :tool)))
        (is (= outputs-before (get-in refused-handle [:state :cycle/outputs]))
            "a refusal cannot mutate phase outputs")
        (is (= :guided-solve (get-in refused-handle [:state :current-phase])))
        (is (empty? (filter #(= :dispatch-student-fresh (:tool %))
                            (get-in refused-handle [:state :steps])))
            "the refused action itself is never recorded as successful")
        (is (= {:refusal/action-id "a-wrong"
                :refusal/tool :dispatch-student}
               (select-keys refusal
                            [:refusal/action-id :refusal/tool])))
        (is (= :phase-tool-not-allowed
               (get-in refusal [:refusal/error :error/code])))
        (is (= before (:refusal/step-index refusal)))
        (is (not (re-find #"TOP-SECRET-PACKET" (pr-str refusal)))
            "raw packet data is absent from the durable receipt")
        (is (empty? (filter #(= :promote-artifact (:tool %))
                            (get-in refused-handle [:state :steps])))
            "a refusal cannot contribute to promotion counts")
        (is (:ok dispatched))
        (is (= 1 (count (filter #(= :dispatch-solver (:tool %))
                                (get-in after-dispatch [:state :steps]))))
            "the routed action creates exactly one dispatch step")
        (is (= :conductor-action-duplicate (:error/code replay)))
        (is (= 1 (count (filter #(= :dispatch-solver (:tool %))
                                (get-in @(:handle (binding/lookup agent-id session-id))
                                        [:state :steps]))))
            "a replay cannot create a second step")
        (is (= :conductor-session-unbound
               (:error/code
                (request! {:agent-id "claude-unbound"
                           :session-id "no-session"
                           :action-id "a-unbound" :operation "close"
                           :args [] :cycle-id "none" :version 0}))))
        (is (= false (:bound? (status! "nobody" "no-session")))
            "read-only status is available without a binding")
        (let [{:keys [cycle-id version]} (binding/status agent-id session-id)]
          (is (= :conductor-operation-unknown
                 (:error/code
                  (request! {:action-id "a-unknown" :operation "eval"
                             :args [] :cycle-id cycle-id :version version}))))
          (is (= :conductor-cycle-stale
                 (:error/code
                  (request! {:action-id "a-stale-cycle" :operation "deposit"
                             :args [{}] :cycle-id "old-cycle"
                             :version version}))))
          (is (= :conductor-version-stale
                 (:error/code
                  (request! {:action-id "a-stale-version" :operation "deposit"
                             :args [{}] :cycle-id cycle-id
                             :version (dec version)})))))

        (is (:ok (action! "a-attempt" :record-solver-attempt
                          [(solver-attempt) {}])))
        (is (:ok (action! "a-deposit" :deposit
                          [{:name "deposit" :kind :feedback :hook "test"
                            :body {:lesson "surface"}
                            :subjects [{:ref/type :problem :ref/id "t94J02"}]}])))
        (is (:ok (action! "a-student" :dispatch-student
                          [{:mission "M-test"} "student"])))
        (is (:ok (action! "a-students" :record-students
                          [[(student-attempt)] []])))
        (let [used (action! "a-write-use" :write-use [])
              state (get-in @(:handle (binding/lookup agent-id session-id))
                            [:state])]
          (is (:ok used) (pr-str used))
          (is (= (mapv :offer/id
                       (get-in state [:cycle/outputs :memory-offers]))
                 (->> (:steps state)
                      (filter #(= :write-use (:tool %)))
                      (mapv #(get-in % [:result :use/offer]))))
              "the conductor dispositions every recorded offer through the surface"))
        (let [adjudicated (action! "a-adjudicate" :adjudicate
                                   [{:outcome :tier-a :residual-sorries 1
                                     :axiom-clean? false :promotion-result []}])]
          (is (:ok adjudicated) (pr-str adjudicated))
          (is (= "promote" (:phase adjudicated))))
        (let [authoritative (:handle (binding/lookup agent-id session-id))
              closed (action! "a-close" :close [])
              trace (->> (get-in @authoritative [:state :steps])
                         (filter #(= :emit-trace (:tool %)))
                         last :result :trace)]
          (is (:ok closed) (pr-str closed))
          (is (pos? (count (:memory-disposition-offer-ids trace)))
              "the emitted trace records dispositioned offer ids")
          (is (= "a-wrong" (-> trace :action-refusals first
                               :refusal/action-id))
              "the durable refusal reaches the emitted cycle trace"))
        (is (= false (:bound? (status! agent-id session-id))))
        (is (= :conductor-session-unbound
               (:error/code
                (request! {:action-id "a-after" :operation "close" :args []
                           :cycle-id (:cycle-id opened) :version 0})))
            "the sentinel removes the transport route"))
      (finally
        (binding/reset-bindings!)
        (agency/unregister-agent! agent-id)
        (agency/unregister-agent! "claude-unbound")
        (doseq [path paths] (Files/deleteIfExists path))))))

(deftest conductor-abandon-route-authenticates-and-releases-live-binding
  (let [agent-id "claude-abandon"
        session-id "abandon-session"
        cycle-id "cycle-abandon"
        handler (http/make-handler {})
        handle {:cycle-id cycle-id
                :state {:current-phase :guided-solve}
                :log []}
        post! (fn [version]
                (let [response
                      (handler {:request-method :post
                                :uri "/api/alpha/conductor/abandon"
                                :body (json/generate-string
                                       {:agent-id agent-id
                                        :session-id session-id
                                        :cycle-id cycle-id
                                        :version version})})]
                  (assoc (json/parse-string (:body response) true)
                         :http/status (:status response))))]
    (binding/reset-bindings!)
    (agency/register-agent!
     {:agent-id agent-id :type :claude
      :invoke-fn (fn [_ _] {:result "unused" :session-id session-id})
      :session-id session-id})
    (try
      (is (:ok (binding/install! agent-id session-id handle)))
      (let [stale (post! 1)]
        (is (= 409 (:http/status stale)))
        (is (= "conductor-abandonment-stale" (:error/code stale)))
        (is (some? (binding/lookup agent-id session-id))))
      (let [released (post! 0)]
        (is (= 200 (:http/status released)))
        (is (:abandoned? released))
        (is (nil? (binding/lookup agent-id session-id))))
      (let [unbound (post! 0)]
        (is (= 409 (:http/status unbound)))
        (is (= "conductor-session-unbound" (:error/code unbound))))
      (finally
        (binding/reset-bindings!)
        (agency/reset-registry!)))))

(deftest conductor-takeover-loads-the-named-version-and-preserves-parked-binding
  (let [{:keys [config paths]} (fixture)
        old-agent "claude-old"
        old-session "surface-old"
        new-agent "claude-new"
        new-session "surface-new"
        handler (http/make-handler {})
        post! (fn [uri payload]
                (let [response (handler {:request-method :post :uri uri
                                         :body (json/generate-string payload)})
                      body (json/parse-string (:body response) true)]
                  (cond-> (assoc body :http/status (:status response))
                    (string? (:error/code body)) (update :error/code keyword))))]
    (binding/reset-bindings!)
    (doseq [[agent session] [[old-agent old-session] [new-agent new-session]]]
      (agency/register-agent!
       {:agent-id agent :type :claude :session-id session
        :invoke-fn (fn [_ _] {:result "unused" :session-id session})}))
    (try
      (let [opened (conductor/open-frame!
                    (assoc config :conductor
                           {:agent old-agent :session old-session
                            :surface "problem-conductor"}))
            {:keys [cycle-id version]} (binding/status old-agent old-session)
            before @(:handle (binding/lookup old-agent old-session))
            wrong (post! "/api/alpha/conductor/takeover"
                         {:agent-id new-agent :session-id new-session
                          :cycle-id cycle-id :version (dec version)})]
        (is (:ok opened) (pr-str (:error opened)))
        (is (= :conductor-version-stale (:error/code wrong)))
        (is (= version (:version (binding/status old-agent old-session)))
            "a refused takeover leaves the old authority intact")
        (is (= :conductor-binding-exists
               (:error/code
                (post! "/api/alpha/conductor/takeover"
                       {:agent-id old-agent :session-id old-session
                        :cycle-id cycle-id :version version})))
            "a live session cannot replace its binding through takeover")

        ;; Simulate the old conductor process disappearing. The server-owned
        ;; binding remains available for an explicit versioned transfer.
        (agency/unregister-agent! old-agent)

        (let [taken (post! "/api/alpha/conductor/takeover"
                           {:agent-id new-agent :session-id new-session
                            :cycle-id cycle-id :version version})
              after-takeover (binding/status new-agent new-session)
              wake-version (:version after-takeover)
              wake (post! "/api/alpha/conductor/resume"
                          {:agent-id new-agent :session-id new-session
                           :cycle-id cycle-id :version wake-version})
              stale-wake (post! "/api/alpha/conductor/resume"
                                {:agent-id new-agent :session-id new-session
                                 :cycle-id cycle-id :version version})]
          (is (:ok taken) (pr-str taken))
          (is (= false (:bound? (binding/status old-agent old-session))))
          (is (:bound? after-takeover))
          (is (> wake-version version)
              "loading the named save is checkpointed as the next store version")
          (is (= (get-in before [:state :current-phase])
                 (:phase after-takeover)))
          (is (:ok wake))
          (is (= wake-version (:version (binding/status new-agent new-session)))
              "waking a prose continuation does not mutate the handle")
          (is (= :conductor-version-stale (:error/code stale-wake))
              "stale parked metadata is refused before an action")

          ;; Reconnect is transport state only: the server-owned cycle survives.
          (agency/unregister-agent! new-agent)
          (agency/register-agent!
           {:agent-id new-agent :type :claude :session-id new-session
            :invoke-fn (fn [_ _] {:result "unused" :session-id new-session})})
          (is (:bound? (binding/status new-agent new-session)))

          (let [routed (post! "/api/alpha/conductor/action"
                              {:agent-id new-agent :session-id new-session
                               :action-id "after-takeover"
                               :operation "dispatch-solver"
                               :args [{:mission "M-test"} "continued"]
                               :cycle-id cycle-id :version wake-version})
                authoritative @(:handle (binding/lookup new-agent new-session))]
            (is (:ok routed) (pr-str routed))
            (is (= 1 (count (filter #(= :dispatch-solver (:tool %))
                                    (get-in authoritative [:state :steps]))))
                "the taken-over cycle continues only through the typed route"))))
      (finally
        (binding/reset-bindings!)
        (agency/unregister-agent! old-agent)
        (agency/unregister-agent! new-agent)
        (doseq [path paths] (Files/deleteIfExists path))))))

(deftest cascade-attachment-window-refuses-a-full-page
  ;; The substrate caps hyperedge windows at 1000 and the end= form has no
  ;; cursor, so a full window is refused rather than silently truncated.
  (let [complete-page #'conductor/complete-page]
    (is (= [:a :b] (complete-page [:a :b] 3 {:endpoint "p"})))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"window overflow"
                          (complete-page [:a :b :c] 3 {:endpoint "p"})))))

(deftest superseded-reviewed-edges-are-not-attachments
  (let [reviewed-attachment? #'conductor/reviewed-attachment?
        current (cascade-edge "memory/one" "pattern/p" "a01A01")
        superseded (assoc-in current [:hx/props :state] :superseded)
        live-shape {:hx/type :memory/assert :prop/attachment-status :reviewed
                    :prop/state :current :prop/roles {:entry "memory/two"}}]
    (is (reviewed-attachment? current))
    (is (not (reviewed-attachment? superseded)))
    (is (reviewed-attachment? live-shape))
    (is (not (reviewed-attachment? (assoc live-shape :prop/state :superseded))))))
