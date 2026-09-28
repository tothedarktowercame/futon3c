(ns futon3c.agency.answer-population
  "Validation and comparison for an answer's declared population.

   The declaration is closed at every structural level.  Source filters use a
   small common vocabulary because their exact values, rather than prose about
   them, are what make population substitution detectable."
  (:require [clojure.set :as set]))

(def ^:private population-keys #{:question :sources :excluded :read})
(def ^:private question-keys #{:agent :at-or-cutoff :kinds :mode})
(def ^:private source-keys
  #{:kind :filter :rows-fetched :rows-used :pages :page-limit :complete?})
(def ^:private filter-keys #{:tags :types :type :end :ids})
(def ^:private excluded-keys #{:reason :rows :scope})
(def ^:private current-read-keys
  #{:mode :system-as-of :cutoff :started-at :finished-at})
(def ^:private as-of-read-keys #{:mode :system-as-of :valid-as-of})
(defn- closed! [m allowed field]
  (when-not (map? m)
    (throw (ex-info "Answer population member must be a map"
                    {:reason :invalid-population :field field})))
  (when-let [extra (seq (set/difference (set (keys m)) allowed))]
    (throw (ex-info "Unexpected answer population key"
                    {:reason :unexpected-key :field field :keys (vec extra)})))
  m)

(defn validate!
  "Validate and return a population declaration.  The declaration has exactly
   :question, :sources, :excluded, and :read; their structural maps are closed."
  [population]
  (closed! population population-keys :population)
  (closed! (:question population) question-keys :question)
  (when-not (and (string? (get-in population [:question :agent]))
                 (string? (get-in population [:question :at-or-cutoff]))
                 (set? (get-in population [:question :kinds]))
                 (contains? #{:current :as-of} (get-in population [:question :mode])))
    (throw (ex-info "Invalid answer population question"
                    {:reason :invalid-population :field :question})))
  (when-not (vector? (:sources population))
    (throw (ex-info "Population sources must be a vector"
                    {:reason :invalid-population :field :sources})))
  (doseq [source (:sources population)]
    (closed! source source-keys :source)
    (closed! (:filter source) filter-keys :filter)
    (when-not (and (contains? #{:evidence :hyperedge} (:kind source))
                   (every? #(and (integer? %) (not (neg? %)))
                           ((juxt :rows-fetched :rows-used :pages :page-limit) source))
                   (boolean? (:complete? source)))
      (throw (ex-info "Invalid population source"
                      {:reason :invalid-population :field :sources}))))
  (when-not (vector? (:excluded population))
    (throw (ex-info "Population exclusions must be a vector"
                    {:reason :invalid-population :field :excluded})))
  (doseq [excluded (:excluded population)]
    (closed! excluded excluded-keys :excluded))
  (let [read (:read population)
        mode (:mode read)]
    (closed! read (case mode
                    :current current-read-keys
                    :as-of as-of-read-keys
                    #{})
             :read)
    (when-not (contains? #{:current :as-of} mode)
      (throw (ex-info "Invalid population read mode"
                      {:reason :invalid-population :field :read}))))
  population)

(defn- source-role [{:keys [kind filter]}]
  (cond
    (and (= :evidence kind) (= ["promise-history"] (:tags filter))) :promise-history
    (and (= :evidence kind) (= ["promise-outcome"] (:tags filter))) :promise-outcomes
    (and (= :hyperedge kind) (= :agreement/record (:type filter))) :agreements
    (and (= :hyperedge kind) (= :offer/record (:type filter))) :offers
    :else nil))

(defn- required-roles [kinds]
  (cond-> #{}
    (contains? kinds :promise) (into #{:promise-history :promise-outcomes})
    (contains? kinds :agreement) (into #{:agreements :offers})))

(defn- filter-correct? [role filter agent]
  (case role
    :promise-history (= {:tags ["promise-history"]} filter)
    :promise-outcomes (= {:tags ["promise-outcome"]
                          :types [:promise/fulfilled :promise/lapsed
                                  :promise/fulfilment-check]}
                         filter)
    :agreements (= {:type :agreement/record :end (str "agent:" agent)} filter)
    :offers (and (= :offer/record (:type filter))
                 (vector? (:ids filter))
                 (= #{:type :ids} (set (keys filter))))
    false))

(defn- read-correct? [question read]
  (let [{:keys [mode at-or-cutoff]} question]
    (case mode
      :current (and (= :current (:mode read))
                    (= :unpinned (:system-as-of read))
                    (= at-or-cutoff (:cutoff read)))
      :as-of (and (= :as-of (:mode read))
                  (= at-or-cutoff (:system-as-of read))
                  (= at-or-cutoff (:valid-as-of read)))
      false)))

(defn same-population?
  "Compare the question asked with the population actually read.  Returns all
   applicable typed mismatches; it never repairs or substitutes a declaration."
  [question population]
  (validate! population)
  (let [declared (:question population)
        by-role (into {} (keep (fn [source]
                                 (when-let [role (source-role source)]
                                   [role source])))
                      (:sources population))
        required (required-roles (:kinds question))
        found (set (keys by-role))
        rs (cond-> []
             (not= (:agent question) (:agent declared)) (conj :agent-mismatch)
             (not= (:at-or-cutoff question) (:at-or-cutoff declared)) (conj :time-mismatch)
             (not= (:kinds question) (:kinds declared)) (conj :kinds-mismatch)
             (seq (set/difference required found)) (conj :source-missing)
             (some (fn [role]
                     (when-let [source (get by-role role)]
                       (not (filter-correct? role (:filter source) (:agent question)))))
                   required)
             (conj :filter-mismatch)
             (some (comp false? :complete?) (:sources population))
             (conj :incomplete-source)
             (not (read-correct? question (:read population)))
             (conj :read-axis-mismatch))]
    (if (seq rs)
      {:status :mismatch :reasons (vec (distinct rs))}
      {:status :same})))
