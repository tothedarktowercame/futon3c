(ns ^{:clj-kondo/config '{:lint-as {clojure.core.logic/run* clojure.core/fn
                                     clojure.core.logic/fresh clojure.core/fn}}}
  futon3c.arxana.design-catalogue
  "Design declarations, bounded snapshot checks and derived enforcement status.
  No persistence or clock reads. Catalogue links are not d6 compositions."
  (:require [clojure.core.logic :as l]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.store :as store]
            [futon3c.arxana.write :as write]))

(def results #{:pass :violation :unknown :dormant :error :stale})
(defn- refuse [reason detail]
  (throw (ex-info "Invalid design catalogue input" (assoc detail :error reason))))
(defn- clusion [type content]
  (records/make-clusion {:type-revision [type 1] :endpoints [] :asserted-content content}))
(defn choice [content] (clusion :design/choice content))
(defn check-record [content] (clusion :design/check content))
(defn- typed? [record type] (= [type 1] (:type-revision record)))
(defn- external? [r]
  (and (nil? (:type-revision r)) (:ref/kind r)
       (= (:id r) (:id (records/make-external-reference r)))))
(defn- endpoint [role kind id]
  {:role role :kind kind :target {(if (= kind :clusion) :clusion/id :ref/id) id}})
(defn link
  "Build checked-by, shaped-by or constrains from actual endpoint records.
  A shaped-by target must be a ruling, not merely a design choice."
  [type from to]
  (let [[role kind valid?]
        (case type
          :design/checked-by [:check :clusion (typed? to :design/check)]
          :design/shaped-by [:ruling :clusion (and (typed? to :design/choice)
                                                  (= :ruling (get-in to [:asserted-content :kind])))]
          :design/constrains [:target :external-ref (external? to)]
          (refuse :design/unknown-link {:type type}))]
    (when-not (and (typed? from :design/choice) valid?)
      (refuse :design/invalid-link {:type type :from (:id from) :to (:id to)}))
    (records/make-clusion {:type-revision [type 1] :asserted-content {}
                          :endpoints [(endpoint :choice :clusion (:id from))
                                      (endpoint role kind (:id to))]})))

(defn check-run
  "Attest one snapshot result. AT is caller-supplied ISO time; never read a clock.
  Input pins and the check's implementation digest are part of the evidence."
  [subject check pins at {:keys [result violations] :as outcome}]
  (when-not (results result) (refuse :design/invalid-result {:result result}))
  (java.time.Instant/parse at)
  (when-not (and (typed? subject :design/choice) (typed? check :design/check))
    (refuse :design/invalid-run-subject {}))
  (records/make-attestation
   {:subject-clusion-id (:id subject) :producer {:checker (get-in check [:asserted-content :implementation-digest])}
    :basis :exact :standing :verified :coverage :partial :event-time at
    :method {:check/key (get-in check [:asserted-content :check/key]) :check/id (:id check) :input-pins pins}
    :content (assoc outcome :violations (vec violations)) :source-refs []}))

(defn- target [r role]
  (some #(when (= role (:role %)) (or (get-in % [:target :clusion/id]) (get-in % [:target :ref/id])))
        (:endpoints r)))
(defn enforcement-status
  "Relative to this record snapshot, not an unpinned live-world guarantee.
  Latest is event-time per linked check. Conflicting tied runs are unknown.
  Missing/malformed runs never pass; a latest violation takes precedence."
  [rs choice-id]
  (let [checks (distinct (for [r rs :when (and (typed? r :design/checked-by)
                                               (= choice-id (target r :choice)))] (target r :check)))
        index (into {} (map (juxt :id identity) rs))
        states (mapv
                (fn [id]
                  (if-not (typed? (index id) :design/check) :missing-check
                    (let [runs (filter #(and (= choice-id (:subject-clusion-id %))
                                             (= id (get-in % [:method :check/id]))) rs)]
                      (if (empty? runs) :no-run
                        (try
                          (let [groups (group-by #(java.time.Instant/parse (:event-time %)) runs)
                                latest (get groups (last (sort (keys groups))))
                                outcomes (set (map #(get-in % [:content :result]) latest))]
                            (cond (outcomes :violation) :violation
                                  (= 1 (count outcomes)) (let [v (first outcomes)] (if (results v) v :invalid-result))
                                  :else :conflicting-runs))
                          (catch Exception _ :invalid-run)))))) checks)]
    (cond (empty? checks) {:status :unknown :reason :no-check}
          (some #{:violation} states) {:status :violated}
          (every? #{:pass} states) {:status :enforced}
          :else {:status :unknown :reasons (vec (distinct (remove #{:pass} states)))})))

(defn- outcome [violations]
  {:result (if (seq violations) :violation :pass) :violations (vec (distinct violations))})
(defn coverage-check
  "Demo citations are feature keywords; absence is checked against this closed snapshot."
  [rs citations]
  (let [documented (vec (distinct (mapcat #(get-in % [:asserted-content :applies-to])
                                         (filter #(typed? % :design/choice) rs))))]
    (outcome (l/run* [feature] (l/membero feature (vec citations))
                    (l/nafc l/membero feature documented)))))
(defn rule-function-check
  "Every rule in the snapshot names one nonempty qualname; distinct rule ids
  cannot name the same function. Detects missing/ill-shaped names as well."
  [rs]
  (let [rules (filter #(= [:rule/content 1] (:type-revision %)) rs)
        facts (mapv #(vector (:id %) (get-in % [:asserted-content :rule/rewrite :python/qualname])) rules)
        valid (vec (filter #(and (string? %) (seq %)) (map second facts)))
        absent (l/run* [id] (l/fresh [name] (l/membero [id name] facts) (l/nafc l/membero name valid)))
        duplicates (l/run* [q] (l/fresh [a b name]
                                (l/membero [a name] facts) (l/membero [b name] facts)
                                (l/!= a b) (l/== q {:rule-a a :rule-b b :qualname name})))]
    (outcome (concat (map #(hash-map :rule % :reason :missing-qualname) absent) duplicates))))
(defn link-check
  "Relationally check catalogue endpoints in a closed record snapshot, including
  hand-made links that bypass link. No composition/cycle semantics are implied."
  [rs]
  (let [choices (mapv :id (filter #(typed? % :design/choice) rs))
        checks (mapv :id (filter #(typed? % :design/check) rs))
        rulings (mapv :id (filter #(and (typed? % :design/choice)
                                      (= :ruling (get-in % [:asserted-content :kind]))) rs))
        refs (mapv :id (filter #(try (external? %) (catch Exception _ false)) rs))
        links (filter #(#{[:design/checked-by 1] [:design/shaped-by 1] [:design/constrains 1]} (:type-revision %)) rs)]
    (outcome
     (mapcat (fn [r]
               (let [[role allowed] (case (first (:type-revision r))
                                      :design/checked-by [:check checks]
                                      :design/shaped-by [:ruling rulings]
                                      :design/constrains [:target refs])]
                 (concat
                  (l/run* [q] (l/nafc l/membero (target r :choice) choices)
                          (l/== q {:link (:id r) :role :choice :target (target r :choice)}))
                  (l/run* [q] (l/nafc l/membero (target r role) allowed)
                          (l/== q {:link (:id r) :role role :target (target r role)}))))) links))))

(defn enum-check
  "Run positive/negative manifestations through the real write validator and an
  in-memory persistence port. A negative fixture accepted by it is a violation."
  [positive negative]
  (let [memory (reify store/ArxanaGraphStore
                 (get-entity [_ _] {:ok nil})
                 (conditional-write! [_ _] {:ok {:created? true}}))
        run #(write/execute memory {:principal/id "design-catalogue" :message/id (:id %)} :clusion/assert %)
        pos (run positive) neg (run negative)]
    (outcome (concat (when-not (:ok pos) [{:fixture :positive :response pos}])
                     (when-not (= :arxana.clusion/invalid-content-enum (get-in neg [:error :error/code]))
                       [{:fixture :negative :response neg}])))))

(def source-revision "1d26769955d1821888b8b537c3e9ad17ec56ece1")
(defn- source [file section]
  {:repo "futon4" :revision source-revision :path file :section section})
(defn worked-choices
  "Five choices sourced from the reviewed documents and packet-15 round-2 ruling.
  Checks/runs are built separately so declarations never imply execution."
  []
  (let [derive "holes/labs/M-arxana-incubator/DERIVE-2-registry-and-fidelity.md"
        argue "holes/labs/M-arxana-incubator/ARGUE-revised.md"
        mission "holes/missions/M-arxana-incubator.md"
        rows [[:scope :constraint "Manifestation revision 3 scope is :role or :declaration." :manifestation-scope (source derive "§9 R6(a)")]
              [:fibres :constraint "Every view opens its evidence fibre." :evidence-fibres (source argue "§3.6 The outcome surface and fibres")]
              [:functions :ruling "A rule record identifies one function, not its introducing unit." :rule-identity (source derive "§8, 8C: Rule identity")]
              [:coverage :constraint "Every backend feature cited by Demo B has a documenting choice." :demo-coverage (source mission "§6 Shared backend, and a coverage invariant")]
              [:links :constraint "Catalogue links connect the right kinds." :catalogue-links (source derive "§9 R6(b)")]]]
    (mapv (fn [[key kind statement feature citation]]
            (choice {:choice/key key :kind kind :statement statement :sources [citation]
                     :lifecycle :accepted :applies-to [feature]
                     :rationale (if (= key :links)
                                  "Packet 15 round 2, claude-5, invoke-1794969023149-23598-0d13c585: catalogue links are not compositions; builders and a snapshot check enforce types."
                                  "Catalogue witness; execution evidence is separate.")
                     :alternatives []})) rows)))

(defn worked-catalogue
  "Build the offline witnesses from explicit pinned inputs. REFERENCE names the
  registry file/coordinate; RULES come from records-from-switch-off. No I/O here."
  [{:keys [rules reference positive negative citations implementation-digest pins at]}]
  (let [choices (worked-choices)
        by-key (into {} (map #(vector (get-in % [:asserted-content :choice/key]) %) choices))
        specs [[:scope :clojure "futon3c.arxana.design-catalogue/enum-check"]
               [:functions :core.logic "futon3c.arxana.design-catalogue/rule-function-check"]
               [:coverage :core.logic "futon3c.arxana.design-catalogue/coverage-check"]
               [:links :core.logic "futon3c.arxana.design-catalogue/link-check"]]
        checks (mapv (fn [[key runtime entry]]
                       (check-record {:check/key key :runtime runtime :entry-point entry
                                      :input-contract "Closed, caller-pinned record snapshot and fixtures."
                                      :output-contract "Result vocabulary and explicit violation vector."
                                      :implementation-digest implementation-digest
                                      :fixtures [{:namespace "futon3c.arxana.design-catalogue-test" :check key}]})) specs)
        links (vec (concat (map #(link :design/checked-by (by-key (get-in % [:asserted-content :check/key])) %) checks)
                           [(link :design/constrains (by-key :scope) reference)]))
        base (vec (concat choices checks [reference] links))
        outcomes {:scope (enum-check positive negative) :functions (rule-function-check rules)
                  :coverage (coverage-check choices citations) :links (link-check base)}
        runs (mapv (fn [c] (let [key (get-in c [:asserted-content :check/key])]
                            (check-run (by-key key) c pins at (outcomes key)))) checks)]
    {:choices choices :checks checks :links links :runs runs :records (into base runs)}))
