(ns futon3c.agents.zaif-arm-adapters
  "Declared, refusal-preserving adapters for Q_actand candidate arms."
  (:require [futon3c.agents.zaif-actand :as q-actand]
            [futon3c.aif.mission-head :as mission-head]
            [futon3c.portfolio.policy :as portfolio]))

(def arm-a-inputs
  [:gap-count :stall-count :review-age :spinoff-pressure :coverage-pct
   :mu-sens :adjacent-missions])

(defn- corpus-absence
  [field basis]
  {:source (q-actand/missing-input field)
   :basis basis})

(def arm-a-mapping
  "Declared U11c map from source records to portfolio/pragmatic-value inputs.

  Both replay corpora lack every source path. The paths name the exact shape
  a future typed record must carry; `:corpora` records the present absences
  rather than substituting similarly named channels."
  {:gap-count
   {:target [:observation :gap-count]
    :source-field [:observation :gap-count]
    :corpora
    {:calibration (corpus-absence [:observation :gap-count]
                                  :u11-reading-map/measured-0-of-114)
     :zaif-decisions (corpus-absence [:observation :gap-count]
                                     :zaif-inputs/closed-seven-key-shape)}}
   :stall-count
   {:target [:observation :stall-count]
    :source-field [:observation :stall-count]
    :corpora
    {:calibration (corpus-absence [:observation :stall-count]
                                  :u11-reading-map/measured-0-of-114)
     :zaif-decisions (corpus-absence [:observation :stall-count]
                                     :zaif-inputs/closed-seven-key-shape)}}
   :review-age
   {:target [:observation :review-age]
    :source-field [:observation :review-age]
    :corpora
    {:calibration (corpus-absence [:observation :review-age]
                                  :u11-reading-map/measured-0-of-114)
     :zaif-decisions (corpus-absence [:observation :review-age]
                                     :zaif-inputs/closed-seven-key-shape)}}
   :spinoff-pressure
   {:target [:observation :spinoff-pressure]
    :source-field [:observation :spinoff-pressure]
    :corpora
    {:calibration (corpus-absence [:observation :spinoff-pressure]
                                  :u11-reading-map/measured-0-of-114)
     :zaif-decisions (corpus-absence [:observation :spinoff-pressure]
                                     :zaif-inputs/closed-seven-key-shape)}}
   :coverage-pct
   {:target [:observation :coverage-pct]
    :source-field [:observation :coverage-pct]
    :corpora
    {:calibration (corpus-absence [:observation :coverage-pct]
                                  :u11-reading-map/measured-0-of-114)
     :zaif-decisions (corpus-absence [:observation :coverage-pct]
                                     :zaif-inputs/closed-seven-key-shape)}}
   :mu-sens
   {:target [:mu-sens]
    :source-field [:mu-sens]
    :corpora
    {:calibration (corpus-absence :mu-sens
                                  :u11-reading-map/measured-0-of-114)
     :zaif-decisions (corpus-absence :mu-sens
                                     :zaif-inputs/closed-seven-key-shape)}}
   :adjacent-missions
   {:target [:adjacent-missions]
    :source-field [:adjacent-missions]
    :corpora
    {:calibration (corpus-absence :adjacent-missions
                                  :u11-reading-map/measured-0-of-114)
   :zaif-decisions (corpus-absence :adjacent-missions
                                     :zaif-inputs/closed-seven-key-shape)}}})

(def arm-b-channels
  [:phase-progress :prediction-divergence :gate-readiness
   :obligation-satisfaction])

(def arm-b-mapping
  "Declared U11d map for mission-head channels over the tracked R2 fixtures.

  Candidate fields are recorded for a later declaration, but remain typed
  absences: similarity of names or ranges is not a replay mapping."
  {:phase-progress
   {:target [:channels :phase-progress]
    :source (q-actand/missing-input :phase-progress)
    :basis :plausible-but-undeclared
    :candidate-field [:value :mission-health]
    :candidate-corpus :u12-c-mis-falsifier/r2-fixtures}
   :prediction-divergence
   {:target [:channels :prediction-divergence]
    :source (q-actand/missing-input :prediction-divergence)
    :basis :plausible-but-undeclared
    :candidate-field [:value :loop-health]
    :candidate-corpus :u12-c-mis-falsifier/r2-fixtures}
   :gate-readiness
   {:target [:channels :gate-readiness]
    :source (q-actand/missing-input :gate-readiness)
    :basis :plausible-but-undeclared
    :candidate-field [:value :support-coverage]
    :candidate-corpus :u12-c-mis-falsifier/r2-fixtures}
   :obligation-satisfaction
   {:target [:channels :obligation-satisfaction]
    :source (q-actand/missing-input :obligation-satisfaction)
    :basis :plausible-but-undeclared
    :candidate-field [:value :attack-coverage]
    :candidate-corpus :u12-c-mis-falsifier/r2-fixtures}})

(defn- present-at?
  [record path]
  (not= ::missing (get-in record path ::missing)))

(defn- first-missing-path
  [record]
  (or (some #(let [path (get-in arm-a-mapping [% :source-field])]
               (when-not (present-at? record path) path))
            arm-a-inputs)
      (some #(when-not (present-at? record [:mu-sens %]) [:mu-sens %])
            (take 5 arm-a-inputs))))

(defn adapt-arm-a
  "Adapt and score one exact arm-A record, or refuse its first missing input.

  Successful scalar values are explicitly `:scalar-awaiting-density`; this
  adapter does not claim that portfolio pragmatic value is a density."
  [record]
  (if-let [field (first-missing-path record)]
    (q-actand/missing-input field)
    (if-not (contains? record :action)
      (q-actand/missing-input :action)
      (let [inputs {:action (:action record)
                    :observation (:observation record)
                    :mu-sens (:mu-sens record)
                    :adjacent-missions (:adjacent-missions record)}
            scalar (portfolio/pragmatic-value
                    (:action inputs) (:observation inputs)
                    (:mu-sens inputs) (:adjacent-missions inputs))]
        {:arm :A
         :inputs inputs
         :value {:type :scalar-awaiting-density
                 :pragmatic-value scalar}}))))

(defn adapt-arm-b
  "Adapt and score one exact arm-B record, or refuse its first missing input.

  `mission-head/select-mission-action` is the public evaluator. ACTION and
  TAU are therefore required inputs; neither is defaulted. The selected
  action's pragmatic term remains explicitly `:scalar-awaiting-density`."
  [record]
  (if-let [field (some #(when-not (present-at? record [:channels %]) %)
                       arm-b-channels)]
    (q-actand/missing-input field)
    (cond
      (not (contains? record :action)) (q-actand/missing-input :action)
      (not (contains? record :tau)) (q-actand/missing-input :tau)
      :else
      (let [evaluation (mission-head/select-mission-action
                        (:channels record) (:tau record))
            policy (some #(when (= (:action record) (:action %)) %)
                         (:policies evaluation))]
        (if-not policy
          (q-actand/no-typed-source :mission (:mission record) (:action record))
          {:arm :B
           :inputs {:action (:action record)
                    :channels (:channels record)
                    :tau (:tau record)}
           :value {:type :scalar-awaiting-density
                   :pragmatic-value (get-in policy [:terms :pragmatic])}})))))
