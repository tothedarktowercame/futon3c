(ns sweep
  "Convert and evaluate the frozen mfuton Book-of-Why graph corpus."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.diagramprover.causal.dag :as dag]
            [futon3c.diagramprover.causal.diagram :as diagram]
            [futon3c.diagramprover.causal.dsep :as dsep]
            [futon3c.diagramprover.causal.identify :as identify]
            [futon3c.diagramprover.causal.scm :as scm]))

(def here "holes/labs/M-diagramprover/mfuton-sweep")
(def fixture-directory (str here "/fixtures"))
(def converted-directory (str here "/converted"))

(def semantic-kind-decisions
  {"burks-nature-nurture"
   {"social" {:kind :observed
              :source-prose (str "A mediator caused by Parental Intelligence "
                                 "and also a collider receiving unmeasured causes.")
              :judgment (str "the mediator is observed; unmeasured modifies "
                             "its incoming causes, not the mediator")}
    "child" {:kind :observed
             :source-prose (str "The outcome receiving direct, mediated, and "
                                "unmeasured influences.")
             :judgment (str "the outcome is observed; unmeasured modifies "
                            "its influences, not the outcome")}}
   "deconfounding-game-1"
   {"abnormality" {:kind :latent-unobserved
                   :source-prose (str "An unobservable abnormality induced by "
                                      "smoking and capable of causing miscarriage.")
                   :judgment "the variable itself is explicitly unobservable"}}
   "deconfounding-game-2"
   {"prior_abnormality" {:kind :latent-unobserved
                         :source-prose (str "An unobservable abnormality in "
                                            "the first pregnancy.")
                         :judgment "the variable itself is explicitly unobservable"}
    "current_abnormality" {:kind :latent-unobserved
                           :source-prose (str "An unobservable abnormality in "
                                              "the second pregnancy.")
                           :judgment "the variable itself is explicitly unobservable"}}
   "generic-frontdoor"
   {"c" {:kind :latent-unobserved
         :source-prose "The unobserved confounder of X and Y."
         :judgment "the variable itself is explicitly unobserved"}}
   "jtpa-job-training"
   {"m" {:kind :latent-unobserved
         :source-prose (str "The unobserved common cause affecting "
                            "participation and earnings.")
         :judgment "the variable itself is explicitly unobserved"}}
   "wright-puppy-birth-weight"
   {"other-growth-causes"
    {:kind :latent-unobserved
     :source-prose (str "Unobserved exogenous causes of prenatal growth rate "
                        "other than litter size.")
     :judgment "the variable itself denotes unobserved exogenous causes"}
    "other-gestation-causes"
    {:kind :latent-unobserved
     :source-prose (str "Unobserved exogenous causes of gestation period "
                        "other than litter size.")
     :judgment "the variable itself denotes unobserved exogenous causes"}
    "prenatal-growth-rate"
    {:kind :latent-unobserved
     :source-prose "The unobserved rate at which the pup grows in utero."
     :judgment "the variable itself is explicitly unobserved"}}})

(def prior-review-verdicts
  {"burks-nature-nurture"
   {:pair {:treatment :parental :outcome :child}
    :identification {:method :backdoor :identifiable? true
                     :adjustment-sets [#{}]}}
   "deconfounding-game-1" {:pair nil :identification nil}
   "deconfounding-game-2" {:pair nil :identification nil}
   "generic-frontdoor"
   {:pair {:treatment :x :outcome :y}
    :identification {:method :backdoor :identifiable? true
                     :adjustment-sets [#{:c}]}}
   "jtpa-job-training"
   {:pair {:treatment :s :outcome :e}
    :identification {:method :backdoor :identifiable? true
                     :adjustment-sets [#{:m}]}}
   "wright-puppy-birth-weight"
   {:pair {:treatment :litter-size :outcome :birth-weight}
    :identification
    {:method :backdoor :identifiable? true
     :adjustment-sets [#{} #{:other-gestation-causes} #{:other-growth-causes}
                       #{:other-gestation-causes :other-growth-causes}]}}})

(def semantic-pairs
  {"airport-bag-posterior" [:bag-on-plane :bag-on-carousel]
   "algebra-for-all-mediation" [:algebra_for_all :learning]
   "alice-education-salary-counterfactual" [:education :salary]
   "berkeley-admissions-kruskal" [:gender :outcome]
   "berkeley-admissions-simple" [:gender :outcome]
   "burks-nature-nurture" [:parental :child]
   "climate-change-probabilities-of-causation" [:greenhouse :response]
   "deconfounding-game-1" [:smoking :future_miscarriage]
   "deconfounding-game-2" [:current_smoking :future_miscarriage]
   "fertilizer-improper-control" [:fertilizer :yield]
   "fertilizer-randomized" [:fertilizer :yield]
   "fertilizer-target-intervention" [:fertilizer :yield]
   "firing-squad" [:soldier-a :death]
   "freedman-child-age-fork" [:shoe-size :reading-ability]
   "jtpa-job-training" [:s :e]
   "linear-mediation-path-diagram" [:treatment :outcome]
   "lords-paradox" [:s :y]
   "lords-paradox-wainer-brown" [:d :y]
   "mendelian-randomization-hdl" [:hdl :attack]
   "nonlinear-threshold-mediation" [:education :outcome]
   "scurvy-vitamin-c-mediator" [:citrus :scurvy]
   "scurvy-wrong-acidity-mediator" [:citrus :scurvy]
   "simpsons-paradox-drug-blood-pressure" [:d :h]
   "simpsons-paradox-drug-gender" [:d :h]
   "simpsons-paradox-exercise-age-cholesterol" [:e :c]
   "smoking-gene-two-readings" [:smoking :lung_cancer]
   "smoking-tar-cancer-frontdoor" [:s :c]
   "tourniquet-selection-mediation" [:tourniquet_use :post_admission_survival]
   "vaccination" [:vaccination :death]
   "vacuuming-causal-explanation" [:vacuuming :unhappy]
   "walking-age-mortality" [:walking :mortality]
   "wright-price-to-supply-reduced-form" [:p :s]
   "wright-puppy-birth-weight" [:litter-size :birth-weight]
   "wright-supply-to-price-reduced-form" [:s :p]})

(def counterfactual-queries
  {"firing-squad"
   {:evidence {:death true}
    :intervention {:soldier-a false}
    :outcome :death
    :rob-expectation "boolean_model_test.py: conclusion is true"
    :rob-expected-value true}
   "vacuuming-causal-explanation"
   {:evidence {:vacuuming true :unhappy true}
    :intervention {:vacuuming false}
    :outcome :unhappy
    :rob-expectation "causal_explanation_test.py: alternative target value is false"
    :rob-expected-value false}})

(defn- plain [value]
  (cond
    (keyword? value) (name value)
    (map? value) (into (sorted-map)
                       (map (fn [[key item]] [(plain key) (plain item)]))
                       value)
    (set? value) (->> value (map plain) sort vec)
    (sequential? value) (mapv plain value)
    :else value))

(defn- expression-string [expression]
  (case (:kind expression)
    "variable" (:variable expression)
    "not" (when-let [input (or (:operand expression)
                                (first (:operands expression)))]
            (when (= "variable" (:kind input))
              (str "not " (:variable input))))
    ("and" "or")
    (let [operands (:operands expression)]
      (when (and (= 2 (count operands))
                 (every? #(= "variable" (:kind %)) operands))
        (str (:variable (first operands)) " " (:kind expression) " "
             (:variable (second operands)))))
    nil))

(defn- unsupported-reasons [raw]
  (let [variables (:variables raw)
        expressions (keep :structural_expression variables)
        unsupported-expressions (remove expression-string expressions)
        non-boolean-domains
        (->> variables (keep :value_domain) (remove #{"boolean"}) set sort vec)]
    (cond-> []
      (seq non-boolean-domains)
      (conj (str "non-Boolean value domains are graph-only: "
                 (str/join ", " non-boolean-domains)))
      (some :affine_equation variables)
      (conj "affine equations are outside the Boolean SCM evaluator")
      (some :coefficient (:arrows raw))
      (conj "numeric edge coefficients are graph-only")
      (some :probability_table variables)
      (conj "finite probability tables are preserved only in the frozen source")
      (:observational_distributions raw)
      (conj "numeric observational distributions are not evaluated")
      (:linear_correlations raw)
      (conj "numeric linear correlations are not evaluated")
      (some :selection variables)
      (conj "selection/regime annotations are graph-only")
      (seq unsupported-expressions)
      (conj "structural expression exceeds fixed Boolean grammar; all equations skipped"))))

(defn- structural-equations [raw]
  (let [variables (:variables raw)
        equations (into (sorted-map)
                        (keep (fn [{:keys [key structural_expression]}]
                                (when structural_expression
                                  (when-let [equation
                                             (expression-string structural_expression)]
                                    [key equation]))))
                        variables)
        endogenous (into #{}
                         (keep (fn [{:keys [key]}]
                                 (when (some #(= key (:target %)) (:arrows raw))
                                   key)))
                         variables)]
    (when (and (seq equations) (= endogenous (set (keys equations))))
      equations)))

(defn- converted-kind [raw variable]
  (or (get-in semantic-kind-decisions
              [(:example_id raw) (:key variable) :kind])
      (when (false? (:observed variable)) :latent-unobserved)
      :observed))

(defn- converted-spec [raw disposition reasons equations]
  (cond->
   {:id (:example_id raw)
    :variables
    (mapv (fn [variable]
            (cond-> {:id (:key variable)
                     :name (:name variable)
                     :kind (name (converted-kind raw variable))}
              (:value_domain variable)
              (assoc :mfuton_value_domain (:value_domain variable))
              (contains? variable :selection)
              (assoc :mfuton_selection (:selection variable))))
          (:variables raw))
    :arrows (mapv (fn [arrow]
                    {:from (:source arrow) :to (:target arrow)})
                  (:arrows raw))
    :mfuton_conversion {:disposition (name disposition)
                        :reasons reasons
                        :source-file (str (:example_id raw) ".json")}}
    equations (assoc :structural_equations equations)))

(defn- natural-pair [causal-dag example-id]
  (let [nodes (set (keys (:variables causal-dag)))
        observed? #(not= :latent-unobserved
                         (keyword (get-in causal-dag [:variables % :kind])))
        roots (filterv observed? (filter #(dag/exogenous? causal-dag %) nodes))
        sinks (filterv observed? (filter #(empty? (dag/children causal-dag %)) nodes))]
    (cond
      (contains? semantic-pairs example-id)
      {:pair (semantic-pairs example-id)
       :why "fixture title/content gives the named cause and outcome"}

      (and (nodes :x) (nodes :y) (observed? :x) (observed? :y))
      {:pair [:x :y] :why "fixture uses the conventional observed X/Y effect pair"}

      (and (nodes :treatment) (nodes :outcome))
      {:pair [:treatment :outcome]
       :why "fixture explicitly names treatment and outcome"}

      (and (<= (count nodes) 10) (= 1 (count roots)) (= 1 (count sinks)))
      {:pair [(first roots) (first sinks)]
       :why "unique observed DAG source and sink"}

      :else nil)))

(defn- summarize-identification [result]
  (cond-> {:method (:method result)
           :identifiable? (not= :refusal (:method result))}
    (:reason result) (assoc :reason (:reason result))
    (:proof-status result) (assoc :proof-status (:proof-status result))
    (:adjustment-sets result) (assoc :adjustment-sets (:adjustment-sets result))
    (:mediators result) (assoc :mediators (:mediators result))))

(defn- evaluate-one [fixture]
  (let [raw (json/parse-string (slurp fixture) true)
        reasons (unsupported-reasons raw)
        equations (when (empty? reasons) (structural-equations raw))
        expression-count (count (keep :structural_expression (:variables raw)))
        incomplete-equations? (and (pos? expression-count) (nil? equations))
        reasons (cond-> reasons
                  (and incomplete-equations? (empty? reasons))
                  (conj "structural equations are incomplete for non-exogenous nodes"))
        disposition (if (seq reasons) :graph-only :converted-fully)
        spec (converted-spec raw disposition reasons equations)
        output-file (io/file converted-directory (str (:example_id raw) ".json"))]
    (spit output-file (str (json/generate-string (plain spec) {:pretty true}) "\n"))
    (try
      (let [causal-dag (dag/load-spec (.getPath output-file))
            rendered (diagram/dag->diagram causal-dag)
            pair-record (natural-pair causal-dag (:example_id raw))
            [treatment outcome] (:pair pair-record)
            identification (when pair-record
                             (summarize-identification
                              (identify/identify causal-dag treatment outcome)))
            independencies (dsep/implied-independencies
                            causal-dag {:max-conditioning 1})
            cf-query (when equations
                       (get counterfactual-queries (:example_id raw)))
            counterfactual (when cf-query
                             (scm/counterfactual causal-dag
                                                 (dissoc cf-query
                                                         :rob-expectation
                                                         :rob-expected-value)))]
        {:example-id (:example_id raw)
         :title (:title raw)
         :source-file (.getName fixture)
         :disposition disposition
         :reason (if (seq reasons) (str/join "; " reasons)
                     (if equations
                       "graph and complete fixed-grammar Boolean SCM converted"
                       "graph schema fully represented; no evaluator payload declared"))
         :variable-count (count (:variables raw))
         :arrow-count (count (:arrows raw))
         :latent-count (count (filter #(= :latent-unobserved
                                          (converted-kind raw %))
                                      (:variables raw)))
         :dag-valid? true
         :canonical-render? (diagram/canonical? rendered)
         :round-trip? (= causal-dag (diagram/diagram->dag rendered))
         :pair (when pair-record {:treatment treatment :outcome outcome
                                  :why (:why pair-record)})
         :identification identification
         :implied-independencies independencies
         :counterfactual (when counterfactual
                           {:query (dissoc cf-query :rob-expected-value)
                            :engine counterfactual
                            :rob-expected-value (:rob-expected-value cf-query)})})
      (catch Exception exception
        {:example-id (:example_id raw)
         :title (:title raw)
         :source-file (.getName fixture)
         :disposition :skipped
         :reason (str "converted graph rejected by dag/validate: "
                      (.getMessage exception))
         :dag-valid? false
         :error-data (ex-data exception)}))))

(defn -main [& _]
  (.mkdirs (io/file converted-directory))
  (let [fixtures (->> (file-seq (io/file fixture-directory))
                      (filter #(str/ends-with? (.getName %) ".json"))
                      (sort-by #(.getName %)))
        results (mapv evaluate-one fixtures)
        output {:schema-version 1
                :fixture-count (count results)
                :semantic-kind-decisions semantic-kind-decisions
                :review-deltas
                (mapv (fn [[example-id before]]
                        (let [after (first (filter #(= example-id (:example-id %))
                                                   results))]
                          {:example-id example-id
                           :before before
                           :after {:pair (when-let [pair (:pair after)]
                                           (select-keys pair
                                                        [:treatment :outcome]))
                                   :identification (:identification after)}}))
                      (sort-by key prior-review-verdicts))
                :fixtures results}]
    (when-not (= 60 (count results))
      (throw (ex-info "Frozen mfuton fixture corpus is incomplete"
                      {:expected 60 :actual (count results)})))
    (when-not (= (count results) (count (set (map :example-id results))))
      (throw (ex-info "Frozen mfuton fixture ids are not unique" {})))
    (spit (io/file here "engine-results.json")
          (str (json/generate-string (plain output) {:pretty true}) "\n"))
    (println "mfuton engine sweep:"
             (count results) "fixtures;"
             (frequencies (map :disposition results)))))
