(ns futon3c.diagramprover.causal.receipts
  "Computed causal receipts for the memory and Lean pipeline specifications.

  Q3 uses two explicit topology variants. Both add an exogenous
  `:M-in-store`, its isolated `:M-own-star`, and `:V12-minus-M`, whose output
  enters the existing information channel V13. The star-forest topology has
  no route from M's star to V12-minus-M. The populated topology additionally
  adds `:shared-patterns` and the path
  M-in-store -> shared-patterns -> V12-minus-M. Thus the same d-separation
  query distinguishes filter-at-dispatch from database-level withholding
  without adding comonoid equations or rewrite search."
  (:require [futon3c.diagramprover.causal.dag :as dag]
            [futon3c.diagramprover.causal.dsep :as dsep]
            [futon3c.diagramprover.causal.surgery :as surgery]))

(def memory-spec-path "docs/memory-causal-graph-spec.json")
(def lean-spec-path "docs/lean-proof-pipeline-causal-spec.json")

(defn- question [causal-dag receipt-id]
  (:question (first (filter #(= receipt-id (:id %))
                            (get-in causal-dag
                                    [:metadata :requested_receipts])))))

(defn- witnesses
  "Return one named active path, avoiding exhaustive negative enumeration."
  [causal-dag x y given]
  (if (dsep/d-connected? causal-dag x y given)
    (dsep/connecting-paths causal-dag x y given {:limit 1})
    {:paths [] :count 0 :truncated? false}))

(defn q1
  "Identification receipt for randomized recall availability."
  ([] (q1 (dag/load-spec memory-spec-path)))
  ([memory]
   (let [target :V06
         outcome :V18
         cut (surgery/cut-outgoing memory target)
         missed (witnesses cut target outcome #{})
         lane-descendant? (contains? (dag/descendants memory target) :V04)
         primary? (dsep/backdoor-adjustment? memory target outcome #{})
         lane? (dsep/backdoor-adjustment? memory target outcome #{:V04})]
     {:id "Q1"
      :question (question memory "Q1")
      :verdicts
      [{:claim :randomized-primary-total-effect
        :holds? primary? :method :backdoor :graph :base :paths []}
       {:claim :lane-subgroup-is-pre-treatment
        :holds? (not lane-descendant?) :method :descendant-check
        :graph :base :paths []}
       {:claim :lane-conditioned-backdoors-blocked
        :holds? lane? :method :backdoor :graph :base :paths []}
       {:claim :no-surviving-backdoor
        :holds? (zero? (:count missed)) :method :surgery
        :graph :cut-outgoing-V06 :paths (:paths missed)
        :paths-truncated? (:truncated? missed)}]
      :adjustment-sets (cond-> [] primary? (conj #{}))
      :subgroup-conditioning (cond-> [] lane? (conj #{:V04}))
      :refusals (cond-> []
                  lane-descendant?
                  (conj {:claim :lane-subgroup-effect
                         :reason :post-treatment-conditioning}))})))

(defn- sever-one-leak [leaked leak-id]
  (dag/validate
   (update leaked :arrows
           #(into [] (remove (fn [arrow]
                               (= leak-id (:leak/id arrow))) %)))))

(defn q2
  "Isolation and mediation receipt for I_E2."
  ([] (q2 (dag/load-spec memory-spec-path)))
  ([memory]
   (let [leaked (dag/with-leaks memory)
         severed (dag/without-leaks leaked)
         leak-verdicts
         (mapv
          (fn [{:keys [id]}]
            (let [opened (witnesses leaked id :V18 #{})
                  isolated (sever-one-leak leaked id)
                  closed? (dsep/d-separated? isolated id :V18 #{})]
              {:leak id
               :opens-path (:paths opened)
               :path-count-lower-bound (:count opened)
               :paths-truncated? (:truncated? opened)
               :severed-blocks? closed?}))
          (:leak-edges memory))
         channel-verdicts
         (mapv
          (fn [channel]
            (let [paths (witnesses severed channel :V18 #{:V07})]
              {:channel channel
               :outcome-independent-given-treatment?
               (dsep/d-separated? severed channel :V18 #{:V07})
               :paths (:paths paths)
               :paths-truncated? (:truncated? paths)}))
          [:V13 :V14])
         sensor (first (filter #(= :S05 (:id %)) (:sensors memory)))
         sensor-absent? (boolean (re-find #"(?i)planned|absent|does not exist"
                                          (str (:status sensor) " "
                                               (:missingness sensor))))]
     {:id "Q2"
      :question (question memory "Q2")
      :verdicts
      [{:claim :isolated-primary-effect
        :holds? (dsep/backdoor-adjustment? severed :V07 :V18 #{})
        :method :backdoor :graph :without-leaks :paths []}
       {:claim :all-leaks-open-outcome-routes
        :holds? (every? #(pos? (:path-count-lower-bound %)) leak-verdicts)
        :method :d-sep :graph :with-leaks
        :paths (mapv :opens-path leak-verdicts)}]
      :leaks leak-verdicts
      :mediation {:treatment :V07
                  :outcome :V18
                  :channels channel-verdicts
                  :channel-separation-given-treatment?
                  (dsep/d-separated? severed :V13 :V14 #{:V07})
                  :sensor :S05
                  :sensor-present? (not sensor-absent?)}
      :adjustment-sets [#{}]
      :refusals
      (if sensor-absent?
        [{:claim :nde-nie-by-channel
          :reason :mediator-channel-unobserved
          :sensor :S05
          :failed-independencies
          (mapv #(select-keys % [:channel
                                 :outcome-independent-given-treatment?
                                 :paths])
                (remove :outcome-independent-given-treatment?
                        channel-verdicts))}]
        [])})))

(defn q3-variants
  "Return the two validated, programmatically constructed Q3 DAGs."
  [memory]
  (let [base (-> memory
                 (assoc-in [:variables :M-in-store]
                           {:id :M-in-store :name "membership of M in store"
                            :kind :exogenous-context})
                 (assoc-in [:variables :M-own-star]
                           {:id :M-own-star :name "M's isolated attachment star"
                            :kind :mechanism})
                 (assoc-in [:variables :V12-minus-M]
                           {:id :V12-minus-M
                            :name "surfaced set excluding M" :kind :mechanism})
                 (update :arrows into
                         [{:from :M-in-store :to :M-own-star
                           :status :q3-encoding}
                          {:from :V12-minus-M :to :V13
                           :status :q3-encoding}])
                 dag/validate)
        populated (-> base
                      (assoc-in [:variables :shared-patterns]
                                {:id :shared-patterns
                                 :name "multi-attachment shared patterns"
                                 :kind :mechanism})
                      (update :arrows into
                              [{:from :M-in-store :to :shared-patterns
                                :status :q3-encoding}
                               {:from :shared-patterns :to :V12-minus-M
                                :status :q3-encoding}])
                      dag/validate)]
    {:star-forest base :populated-graph populated}))

(defn q3
  "Two-topology filter-equivalence receipt."
  ([] (q3 (dag/load-spec memory-spec-path)))
  ([memory]
   (let [variants (q3-variants memory)
         verdicts
         (mapv
          (fn [[graph-name causal-dag]]
            (let [separated? (dsep/d-separated? causal-dag
                                                  :M-in-store :V12-minus-M #{})
                  upstream (witnesses causal-dag
                                      :M-in-store :V12-minus-M #{})
                  downstream-separated?
                  (dsep/d-separated? causal-dag :M-in-store :V18
                                     #{:V12-minus-M})
                  downstream (witnesses causal-dag :M-in-store :V18
                                        #{:V12-minus-M})]
              {:graph graph-name
               :claim :filter-equivalence
               :holds? separated?
               :method :d-sep
               :paths (:paths upstream)
               :path-count-lower-bound (:count upstream)
               :paths-truncated? (:truncated? upstream)
               :v18-independent-given-realized-surfaced-set?
               downstream-separated?
               :v18-paths (:paths downstream)
               :v18-paths-truncated? (:truncated? downstream)}))
          variants)]
     {:id "Q3"
      :question (question memory "Q3")
      :encoding {:treatment :M-in-store
                 :filtered-surface :V12-minus-M
                 :realized-surface-conditioning #{:V12-minus-M}}
      :verdicts verdicts
      :adjustment-sets []
      :refusals []})))

(defn r1-selection-variant
  "Encode uncontrolled E7 selection using pre-treatment need and difficulty.

  Existing P10 is downstream of P20, so using it as a selector would create a
  cycle and be post-treatment adjustment. P10-pre is its time-indexed state at
  module-selection time."
  [lean]
  (-> lean
      (assoc-in [:variables :P10-pre]
                {:id :P10-pre :name "dependency set before extension choice"
                 :kind :exogenous-context})
      (update :arrows into
              [{:from :P01 :to :P20 :status :selection-regime
                :mechanism "problem difficulty influences module choice"}
               {:from :P10-pre :to :P20 :status :selection-regime
                :mechanism "recorded need influences module choice"}
               {:from :P10-pre :to :P10 :status :selection-regime
                :mechanism "pre-treatment need persists into dependency state"}])
      dag/validate))

(defn r1
  "Controlled and uncontrolled extension-added receipt for the Lean DAG."
  ([] (r1 (dag/load-spec lean-spec-path)))
  ([lean]
   (let [selection (r1-selection-variant lean)
         controlled? (dsep/backdoor-adjustment? lean :P20 :P16 #{})
         uncontrolled? (dsep/backdoor-adjustment? selection :P20 :P16 #{})
         selected-adjustment #{:P01 :P10-pre}
         adjusted? (dsep/backdoor-adjustment? selection :P20 :P16
                                                selected-adjustment)
         backdoors (witnesses (surgery/cut-outgoing selection :P20)
                              :P20 :P16 #{})]
     {:id "R1"
      :question (question lean "R1")
      :verdicts
      [{:claim :controlled-extension-effect
        :holds? controlled? :method :backdoor :graph :controlled
        :paths []}
       {:claim :uncontrolled-observation-identifies-effect
        :holds? uncontrolled? :method :backdoor :graph :e7-selection
        :paths (:paths backdoors)
        :paths-truncated? (:truncated? backdoors)}
       {:claim :selection-adjustment-blocks-backdoors
        :holds? adjusted? :method :backdoor :graph :e7-selection
        :paths []}]
      :confounders [{:variable :P01 :role :problem-difficulty}
                    {:variable :P10-pre :role :recorded-need
                     :derived-from :P10}]
      :adjustment-sets (cond-> [] controlled? (conj #{}) adjusted?
                         (conj selected-adjustment))
      :refusals
      (cond-> []
        (not uncontrolled?)
        (conj {:claim :causal-reading-of-four-closures
               :reason :open-selection-backdoors
               :paths (:paths backdoors)
               :paths-truncated? (:truncated? backdoors)}))})))

(defn- remove-arrow [causal-dag from to]
  (update causal-dag :arrows
          #(into [] (remove (fn [arrow]
                              (and (= from (:from arrow))
                                   (= to (:to arrow)))) %))))

(defn r2-variants
  "Encode the two provenance regimes named by R2.

  Both variants replace the aggregate P19 -> P03 arrow with a class-specific
  channel. In `:copied-class`, K2-byte-copy is an independently available copy
  feeding proof search; withholding the module (P19) has no import edge, while
  the distinct `:remove-content` decision reaches that copy. In
  `:extracted-class`, P19 reaches proof search through a real module-import
  node and there is no byte-copy node. Thus module removal and content removal
  are intentionally different surgery targets only in the copied regime."
  [lean]
  (let [without-aggregate (remove-arrow lean :P19 :P03)
        copied (-> without-aggregate
                   (assoc-in [:variables :K2-byte-copy]
                             {:id :K2-byte-copy
                              :name "independent byte-identical content copy"
                              :kind :provenance-channel})
                   (assoc-in [:variables :remove-content]
                             {:id :remove-content
                              :name "remove duplicated module content"
                              :kind :decision-intervention})
                   (update :arrows into
                           [{:from :remove-content :to :K2-byte-copy
                             :status :r2-encoding}
                            {:from :K2-byte-copy :to :P09
                             :status :r2-encoding}])
                   dag/validate)
        extracted (-> without-aggregate
                      (assoc-in [:variables :module-import]
                                {:id :module-import
                                 :name "extracted module import"
                                 :kind :provenance-channel})
                      (update :arrows into
                              [{:from :P19 :to :module-import
                                :status :r2-encoding}
                               {:from :module-import :to :P09
                                :status :r2-encoding}])
                      dag/validate)]
    {:copied-class copied :extracted-class extracted}))

(defn r2
  "Withholding-validity receipt across leak and provenance regimes."
  ([] (r2 (dag/load-spec lean-spec-path)))
  ([lean]
   (let [leaked (dag/with-leaks lean)
         leak-verdicts
         (mapv (fn [{:keys [id]}]
                 (let [opened (witnesses leaked id :P16 #{})
                       isolated (sever-one-leak leaked id)]
                   {:leak id
                    :opens-path (:paths opened)
                    :path-count-lower-bound (:count opened)
                    :paths-truncated? (:truncated? opened)
                    :severed-blocks?
                    (dsep/d-separated? isolated id :P16 #{})}))
               (:leak-edges lean))
         variants (r2-variants lean)
         copied-do (surgery/do-intervention (:copied-class variants) :P19)
         copied-survival (witnesses copied-do :K2-byte-copy :P16 #{})
         content-do (surgery/do-intervention (:copied-class variants)
                                             :remove-content)
         content-effect (witnesses content-do :remove-content :P16 #{})
         extracted-do (surgery/do-intervention (:extracted-class variants)
                                               :P19)
         extracted-effect (witnesses extracted-do :P19 :P16 #{})]
     {:id "R2"
      :question (question lean "R2")
      :leaks leak-verdicts
      :verdicts
      [{:graph :copied-class
        :claim :module-withholding-affects-consumer
        :holds? (dsep/d-connected? copied-do :P19 :P16 #{})
        :method :surgery :paths []
        :content-survives-via (:paths copied-survival)
        :content-paths-truncated? (:truncated? copied-survival)}
       {:graph :extracted-class
        :claim :module-withholding-affects-consumer
        :holds? (dsep/d-connected? extracted-do :P19 :P16 #{})
        :method :surgery :paths (:paths extracted-effect)
        :paths-truncated? (:truncated? extracted-effect)}]
      :duplication-debt
      {:module-withholding-effect?
       (dsep/d-connected? copied-do :P19 :P16 #{})
       :content-removal-effect?
       (dsep/d-connected? content-do :remove-content :P16 #{})
       :content-removal-paths (:paths content-effect)
       :paths-truncated? (:truncated? content-effect)
       :contrast? (not=
                   (dsep/d-connected? copied-do :P19 :P16 #{})
                   (dsep/d-connected? content-do :remove-content :P16 #{}))}
      :adjustment-sets []
      :refusals []})))

(defn- sensor [lean id]
  (first (filter #(= id (:id %)) (:sensors lean))))

(defn r3-variants
  "Materialize time-indexed sensor variants for R3.

  T04 follows its declared `observes P16` field: P10-at-k determines the
  contemporaneous outcome projection P16-at-k, which T04-at-k reads. CJ1's
  stated progress edge is P10-at-k -> P16-at-k+1. The hypothetical variant
  additionally materializes T05-at-k from its declared `observes P10` field;
  it remains a measurement child, never a cause of progress."
  [lean]
  (let [t04 (sensor lean :T04)
        t05 (sensor lean :T05)
        current (-> lean
                    (assoc-in [:variables :P10-at-k]
                              {:id :P10-at-k :name "dependency set at round k"
                               :kind :time-indexed-state})
                    (assoc-in [:variables :P16-at-k]
                              {:id :P16-at-k :name "outcome projection at round k"
                               :kind :time-indexed-state})
                    (assoc-in [:variables :P16-at-k+1]
                              {:id :P16-at-k+1 :name "outcome at round k+1"
                               :kind :time-indexed-outcome})
                    (assoc-in [:variables :T04-at-k]
                              {:id :T04-at-k :name (:name t04)
                               :kind :measurement
                               :sensor/observes (:observes t04)})
                    (update :arrows into
                            [{:from :P10-at-k :to :P16-at-k
                              :status :r3-time-index}
                             {:from :P16-at-k :to :T04-at-k
                              :status :r3-measurement}
                             {:from :P10-at-k :to :P16-at-k+1
                              :status :CJ1}])
                    dag/validate)
        with-t05 (-> current
                     (assoc-in [:variables :T05-at-k]
                               {:id :T05-at-k :name (:name t05)
                                :kind :measurement
                                :sensor/observes (:observes t05)
                                :sensor/status (:status t05)})
                     (update :arrows conj
                             {:from :P10-at-k :to :T05-at-k
                              :status :r3-hypothetical-measurement})
                     dag/validate)]
    {:current-sensors current :with-hypothetical-t05 with-t05}))

(defn r3
  "Hole-count and hypothetical dependency-sensor sufficiency receipt."
  ([] (r3 (dag/load-spec lean-spec-path)))
  ([lean]
   (let [variants (r3-variants lean)
         current (:current-sensors variants)
         hypothetical (:with-hypothetical-t05 variants)
         current-paths (witnesses current :P16-at-k+1 :P10-at-k
                                  #{:T04-at-k})
         t05-paths (witnesses hypothetical :P16-at-k+1 :T04-at-k
                              #{:T05-at-k})]
     {:id "R3"
      :question (question lean "R3")
      :verdicts
      [{:graph :current-sensors
        :claim :hole-count-sufficient-for-progress
        :holds? (dsep/d-separated? current :P16-at-k+1 :P10-at-k
                                   #{:T04-at-k})
        :method :d-sep :given #{:T04-at-k}
        :paths (:paths current-paths)
        :paths-truncated? (:truncated? current-paths)}
       {:graph :with-hypothetical-t05
        :claim :dependency-sensor-screens-off-hole-count
        :holds? (dsep/d-separated? hypothetical :P16-at-k+1 :T04-at-k
                                   #{:T05-at-k})
        :method :d-sep :given #{:T05-at-k}
        :paths (:paths t05-paths)
        :paths-truncated? (:truncated? t05-paths)}]
      :adjustment-sets []
      :refusals []})))

(defn all-receipts []
  (let [memory (dag/load-spec memory-spec-path)
        lean (dag/load-spec lean-spec-path)]
    [(q1 memory) (q2 memory) (q3 memory)
     (r1 lean) (r2 lean) (r3 lean)]))
