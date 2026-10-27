(ns futon3c.giraffe.pass-test
  "M-giraffe stage 1 packet 1 acceptance tests (M-giraffe.md §VERIFY):
  pilot 1's scored result, pilot 2's readings A and B, the planted bad cases,
  and the RUN-001-started review scenario.  The two converters turn each
  pilot's Plane-shaped fixture into the tracker-neutral pass input; they live
  here (not in src) because the source adapter is a later packet."
  (:require [clojure.test :refer [deftest is testing]]
            [cheshire.core :as json]
            [clojure.string :as str]
            [futon3c.giraffe.pass :as gp]))

(defn- load-plane [name]
  ;; The :test alias does not put test/resources on the classpath; other
  ;; futon3c fixtures are read by repo-relative path (e.g. three-halves-square).
  (json/parse-string (slurp (str "test/resources/" name)) keyword))

(defn- issue-id-pilot1 [i]
  (-> i :external_id (str/split #"/") last (str/replace #"\.md$" "") keyword))

(def group-rank {:started 0 :unstarted 1 :backlog 2})

(defn pilot1->input
  "Pilot 1's Plane records → the pass input.  All pilot-1 edges are closure
  (prose-read); :source-order is Plane's order over open items (state group,
  then updated_at descending, then id), so the pass can be compared against
  what a Plane user sees."
  []
  (let [plane (load-plane "giraffe/pilot1-plane.json")
        state->group (into {} (map (juxt :name (comp keyword :group)))
                           (:states plane))
        issues (:issues plane)
        by-id (into {} (map (juxt issue-id-pilot1 identity)) issues)
        order (vec (sort (fn [a b]
                           (let [ga (get group-rank (state->group (:state (by-id a))) 3)
                                 gb (get group-rank (state->group (:state (by-id b))) 3)]
                             (if (not= ga gb) (compare ga gb)
                                 (let [ua (:updated_at (by-id a)) ub (:updated_at (by-id b))]
                                   (if (not= ua ub) (compare ub ua) (compare a b))))))
                         (filter (fn [id] (not (contains? #{:completed :cancelled}
                                                          (state->group (:state (by-id id))))))
                                 (map issue-id-pilot1 issues))))
        rank (into {} (map-indexed (fn [i id] [id i]) order))]
    {:as-of "2026-10-27"
     :items (vec (for [i issues]
                   (let [id (issue-id-pilot1 i)]
                     {:id id
                      :state-group (state->group (:state i))
                      :source-order (get rank id (count rank))})))
     :edges (vec (concat
                  (for [r (:relations plane)]
                    {:from (keyword (:issue r)) :to (keyword (:related_issue r))
                     :kind :item :class :closure
                     :source (keyword (get-in r [:extras :edge/source] :prose-reading))
                     :quote (get-in r [:extras :quote])
                     :file (get-in r [:extras :file]) :line (get-in r [:extras :line])})
                  (for [x (:external_needs plane)]
                    {:from (keyword (:issue x))
                     :to (keyword (str "ext:" (:issue x) ":" (:line x)))
                     :kind :external :class :closure :source :prose-reading
                     :met false
                     :quote (:quote x) :file (:file x) :line (:line x)})))}))

(defn pilot2->input
  "Pilot 2's Plane records → the pass input, with the author's edge classes.
  All created_at equal as_of, so Plane's -created_at order falls back to
  sequence_id; that is the :source-order.  :classes-fn lets a test override
  the classes (reading A forces :closure everywhere)."
  ([] (pilot2->input :author))
  ([classes]
   (let [plane (load-plane "giraffe/pilot2-plane.json")
         state->group (into {} (map (juxt :name (comp keyword :group)))
                            (:states plane))
         class-for (fn [c] (if (= classes :all-closure)
                             :closure
                             (keyword (or c "closure"))))]
     {:as-of "2026-10-27"
      :items (vec (for [i (:issues plane)]
                    {:id (keyword (:external_id i))
                     :state-group (state->group (:state i))
                     :source-order (:sequence_id i)}))
      :edges (vec (concat
                   (for [r (:relations plane)]
                     {:from (keyword (:issue r)) :to (keyword (:related_issue r))
                      :kind :item :class (class-for (get-in r [:extras :class]))
                      :source (keyword (get-in r [:extras :edge/source]
                                               :author-stated))
                      :note (get-in r [:extras :note])})
                   (for [x (:external_needs plane)]
                     {:from (keyword (:issue x)) :to (keyword (str "ext:" (:to x)))
                      :kind :external :class :closure :source :author-stated
                      :met (boolean (:state x))
                      :note (:note x)})))})))

(def pilot1-ready
  #{:T-car3-queue-routing-design-note :T-fail-activation-authority-unavailable
    :T-fail-agent-not-found :T-fail-invoke-interrupted :T-fail-operator-cancelled
    :T-zai-chat-transient-timeout})

(deftest pilot-1-reproduces-the-scored-result
  (let [out (gp/pass (pilot1->input))]
    (testing "bullet 1: 15 open (6 started, 7 unstarted, 2 backlog); the same 6 ready, 9 not ready"
      (is (= 15 (get-in out [:counts :open])))
      (is (= pilot1-ready (set (:ready-set out))))
      (is (= 9 (count (filter #(= :not-ready (:status %)) (vals (:readiness out)))))))
    (testing "bullet 2: the blocker-cancelled pair, blocked by the superseded routing ticket"
      (is (= :needs/blocker-cancelled
             (get-in out [:readiness :T-car3-phase2-impl :reason])))
      (is (= :needs/blocker-cancelled
             (get-in out [:readiness :T-mq7-unaddressable-caller :reason])))
      (is (= [:T-car3-queue-routing]
             (get-in out [:readiness :T-car3-phase2-impl :blockers])))
      (is (contains? (set (get-in out [:readiness :T-mq7-unaddressable-caller :blockers]))
                     :T-car3-queue-routing)))
    (testing "the done-with-open-needs anomaly: the tidy needs phase2 and mq7, both started"
      (is (= 1 (count (:anomalies out))))
      (let [a (first (:anomalies out))]
        (is (= :anomaly/done-with-open-needs (:anomaly a)))
        (is (= :T-durable-queue-rename-and-tidies (:item a)))
        (is (= #{:T-car3-phase2-impl :T-mq7-unaddressable-caller}
               (set (map :to (:edges a)))))))
    (testing "bullet 3: every ready item's cone is empty; nothing unblocks"
      (is (every? empty? (map (:cones out) pilot1-ready)))
      (is (every? empty? (map (:unblocks out) pilot1-ready))))
    (testing "bullet 4: no cycles"
      (is (empty? (:cycles out))))
    (testing "order: ready items first with typed reasons"
      (is (= pilot1-ready (set (take 6 (map :id (:order out))))))
      (is (every? #(= :ready (:status %)) (take 6 (:order out))))
      (is (every? #(= :not-ready (:status %)) (drop 6 (:order out)))))))

(defn cone [out id] (count (get-in out [:cones id])))

(deftest pilot-2-reading-a-reproduces-the-scored-result
  (let [out (gp/pass (pilot2->input :all-closure))]
    (testing "bullet 1: 29 open; 8 ready ignoring externals, 7 counting them"
      (is (= 29 (get-in out [:counts :open])))
      (is (= 8 (get-in out [:counts :ready-ignoring-externals])))
      (is (= 7 (get-in out [:counts :ready])))
      (is (= #{:B1-ACCEPT :R28-RETENTION :T12-DETECTOR :TRIPWIRE-PLAN
               :A1-ISSUE-PLAN :A4-PLAN :HISTORICAL-GRID :LOOP-PLAN}
             (set (:ready-ignoring-externals out))))
      (is (not (contains? (set (:ready-set out)) :A1-ISSUE-PLAN))))
    (testing "bullet 2: the cone sizes of reading A"
      (is (= {:B1-ACCEPT 21 :B1-TICKETS 14 :R28-RETENTION 14 :TRIPWIRE-PLAN 14
              :A1-ISSUE-PLAN 13 :A4-PLAN 13 :T12-DETECTOR 2 :HISTORICAL-GRID 2
              :LOOP-PLAN 1 :GATE-READY 12 :RUN-001 11}
             (into {} (map (fn [k] [k (cone out k)]))
                   [:B1-ACCEPT :B1-TICKETS :R28-RETENTION :TRIPWIRE-PLAN
                    :A1-ISSUE-PLAN :A4-PLAN :T12-DETECTOR :HISTORICAL-GRID
                    :LOOP-PLAN :GATE-READY :RUN-001]))))
    (testing "bullet 3: only closing B1-ACCEPT unblocks anything (B1-MUTATIONS)"
      (is (= [:B1-MUTATIONS] (get-in out [:unblocks :B1-ACCEPT])))
      (is (every? empty? (map (:unblocks out)
                               (remove #{:B1-ACCEPT} (:ready-set out))))))
    (testing "bullet 4/5a: no cycle; the five run-output tickets not ready in A"
      (is (empty? (:cycles out)))
      (is (every? #(= :not-ready (:status (get-in out [:readiness %])))
                  [:A4-VERIFY :B-EVIDENCE :C-EVIDENCE :D-EVIDENCE :E1-TERMINATION])))))

(deftest pilot-2-reading-b-reproduces-the-scored-result
  (let [out (gp/pass (pilot2->input :author))]
    (testing "same counts as A (no ticket has started)"
      (is (= 29 (get-in out [:counts :open])))
      (is (= 8 (get-in out [:counts :ready-ignoring-externals])))
      (is (= 7 (get-in out [:counts :ready]))))
    (testing "the cones of R28 and T12 are empty under the author's classes"
      (is (= 0 (cone out :R28-RETENTION)))
      (is (= 0 (cone out :T12-DETECTOR)))
      (is (= 14 (cone out :TRIPWIRE-PLAN)))
      (is (= 21 (cone out :B1-ACCEPT))))
    (testing "alternative edges are reported, never block"
      (is (contains? (set (map :to (get-in out [:readiness :A3-DISPOSITION
                                                :alternative-available])))
                     :R28-RETENTION)))
    (testing "only B1-ACCEPT unblocks anything"
      (is (= [:B1-MUTATIONS] (get-in out [:unblocks :B1-ACCEPT]))))
    (testing "no cycle in either reading"
      (is (empty? (:cycles out))))))

(deftest pilot-2-run-001-started-review-scenario
  (let [input (update-in (pilot2->input :author)
                         [:items]
                         (fn [items] (map (fn [i] (if (= :RUN-001 (:id i))
                                                    (assoc i :state-group :started)
                                                    i))
                                          items)))
        out (gp/pass input)]
    (testing "in B the run-output evidence tickets become ready, A4-VERIFY does not"
      (is (= :ready (:status (get-in out [:readiness :B-EVIDENCE]))))
      (is (= :ready (:status (get-in out [:readiness :C-EVIDENCE]))))
      (is (= :ready (:status (get-in out [:readiness :D-EVIDENCE]))))
      (is (= :ready (:status (get-in out [:readiness :E1-TERMINATION]))))
      (is (= :not-ready (:status (get-in out [:readiness :A4-VERIFY])))))
    (testing "in A nothing changes (RUN-001 started is still not completed)"
      (let [out-a (gp/pass (update-in (pilot2->input :all-closure)
                                      [:items]
                                      (fn [items] (map (fn [i] (if (= :RUN-001 (:id i))
                                                                 (assoc i :state-group :started)
                                                                 i))
                                                       items))))]
        (is (= :not-ready (:status (get-in out-a [:readiness :B-EVIDENCE]))))
        (is (= 7 (get-in out-a [:counts :ready])))))))

;; ---- planted bad cases: small hand-built inputs; each assertion names the
;; wrong behaviour it is there to catch (a pass with that behaviour fails).

(deftest planted-two-cycle
  (let [input {:as-of "t"
               :items [{:id :a :state-group :unstarted :source-order 1}
                       {:id :b :state-group :unstarted :source-order 2}
                       {:id :c :state-group :unstarted :source-order 3}]
               :edges [{:from :a :to :b :kind :item :class :closure :source :author-stated}
                       {:from :b :to :a :kind :item :class :closure :source :author-stated}]}
        out (gp/pass input)]
    ;; wrong: refusing the whole pool (glue-cycle refusal), or evaluating the
    ;; members as if they had no needs.
    (is (= :undetermined (get-in out [:readiness :a :status])))
    (is (= :needs/cyclic (get-in out [:readiness :a :reason])))
    (is (= :needs-breakdown (get-in out [:readiness :a :flag])))
    (is (= [:a :b] (get-in out [:readiness :a :component])))
    (is (= [[:a :b]] (:cycles out)))
    ;; the unrelated item is still evaluated and ranked; the members stay in
    ;; the order (they are open) after it, with their undetermined status.
    (is (= :ready (get-in out [:readiness :c :status])))
    (is (= :needs/none-declared (get-in out [:readiness :c :reason])))
    (is (= [:c :a :b] (map :id (:order out))))))

(deftest planted-cancelled-blocker
  (let [input {:as-of "t"
               :items [{:id :x :state-group :unstarted :source-order 1}
                       {:id :dead :state-group :cancelled :source-order 2}]
               :edges [{:from :x :to :dead :kind :item :class :closure :source :author-stated}]}
        out (gp/pass input)]
    ;; wrong: a cancelled blocker counting as met, or met-because-open.
    (is (= :not-ready (get-in out [:readiness :x :status])))
    (is (= :blocker-cancelled (get-in out [:readiness :x :flag])))
    (is (= [:dead] (get-in out [:readiness :x :blockers])))
    (is (= :needs/blocker-cancelled (get-in out [:readiness :x :reason])))))

(deftest planted-alternative-edge
  (let [input {:as-of "t"
               :items [{:id :k :state-group :unstarted :source-order 1}
                       {:id :alt :state-group :unstarted :source-order 2}]
               :edges [{:from :k :to :alt :kind :item :class :alternative
                        :source :author-stated}]}
        out (gp/pass input)]
    ;; wrong: an alternative edge blocking readiness, or vanishing silently.
    (is (= :ready (get-in out [:readiness :k :status])))
    (is (= :needs/alternative-only (get-in out [:readiness :k :reason])))
    (is (= [{:to :alt :source :author-stated}]
           (get-in out [:readiness :k :alternative-available])))))

(deftest planted-artifact-edge
  (let [started {:as-of "t"
                 :items [{:id :k :state-group :unstarted :source-order 1}
                         {:id :run :state-group :started :source-order 2}]
                 :edges [{:from :k :to :run :kind :item :class :artifact
                          :source :author-stated}]}
        unstarted (update-in started [:items 1] assoc :state-group :unstarted)]
    ;; wrong: artifact treated as closure (started not met), or always met.
    (is (= :ready (:status (get-in (gp/pass started) [:readiness :k]))))
    (is (= :not-ready (:status (get-in (gp/pass unstarted) [:readiness :k]))))
    (is (= :needs/unmet (get-in (gp/pass unstarted) [:readiness :k :reason])))))

(deftest planted-done-with-open-need
  (let [input {:as-of "t"
               :items [{:id :done :state-group :completed :source-order 1}
                       {:id :open :state-group :unstarted :source-order 2}]
               :edges [{:from :done :to :open :kind :item :class :closure
                        :source :author-stated}]}
        out (gp/pass input)]
    ;; wrong: resolving the anomaly (treating the edge as stale and dropping
    ;; it), or not reporting it.
    (is (= 1 (count (:anomalies out))))
    (is (= :anomaly/done-with-open-needs
           (:anomaly (first (:anomalies out)))))
    (is (= :done (:status (get-in out [:readiness :done]))))
    (is (= [{:from :done :to :open :kind :item :class :closure
             :source :author-stated}]
           (:edges (first (:anomalies out)))))))

(deftest order-reasons-follow-the-first-slice-rule
  ;; ready first by unblocks, then cone, then source-order; not-ready after,
  ;; by source-order, each with a typed reason.
  (let [input {:as-of "t"
               :items [{:id :r1 :state-group :unstarted :source-order 5}
                       {:id :r2 :state-group :unstarted :source-order 1}
                       {:id :n1 :state-group :unstarted :source-order 1}
                       {:id :n2 :state-group :unstarted :source-order 0}]
               :edges [{:from :n1 :to :r1 :kind :item :class :closure :source :author-stated}
                       {:from :n2 :to :r1 :kind :item :class :closure :source :author-stated}
                       {:from :n2 :to :r2 :kind :item :class :closure :source :author-stated}]}
        out (gp/pass input)]
    ;; r1 unblocks both n1 and n2? no: n2 needs r1 AND r2; only n1 flips.
    (is (= [:n1] (get-in out [:unblocks :r1])))
    (is (= [:r1 :r2 :n2 :n1] (map :id (:order out))))
    (is (= :order/ready-by-unblocks (get-in out [:order 0 :reason])))
    (is (= :order/not-ready-by-source-order (get-in out [:order 2 :reason])))))

;; Review (claude-13, 2026-10-27): three bad cases constructed at review that
;; the first version got wrong.
(deftest review-cyclic-items-are-not-ready-ignoring-externals
  ;; wrong: cycle members, absent from the glued model, defaulted to ready.
  (let [out (gp/pass {:items [{:id :a :state-group :unstarted}
                              {:id :b :state-group :unstarted}
                              {:id :c :state-group :unstarted}]
                      :edges [{:from :a :to :b} {:from :b :to :a}
                              {:from :c :to :x :kind :external}]})]
    (is (= [:c] (:ready-ignoring-externals out)))
    (is (= 1 (get-in out [:counts :ready-ignoring-externals])))))

(deftest review-closed-items-on-a-cycle-keep-their-lifecycle-status
  (let [out (gp/pass {:items [{:id :a :state-group :completed}
                              {:id :b :state-group :cancelled}]
                      :edges [{:from :a :to :b} {:from :b :to :a}]})]
    (is (= :done (get-in out [:readiness :a :status])))
    (is (= :cancelled (get-in out [:readiness :b :status])))))

(deftest review-closure-and-artifact-on-one-target-are-two-needs
  ;; wrong: both edges shared one variable and the artifact edge's value won.
  (let [out (gp/pass {:items [{:id :a :state-group :unstarted}
                              {:id :b :state-group :started}]
                      :edges [{:from :a :to :b :class :closure}
                              {:from :a :to :b :class :artifact}
                              {:from :a :to :b :class :closure}]})]
    (is (= :not-ready (get-in out [:readiness :a :status])))
    (is (= :needs/unmet (get-in out [:readiness :a :reason])))))
