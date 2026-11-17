(ns futon3c.diagramprover.arxana-records
  "Pure conversion to the existing Arxana constructors; no publication.
  Arxana canonical payloads forbid sets: unordered names, kinds and observation
  sets become explicitly sorted vectors. Term/vector order is preserved."
  (:require [clojure.walk :as walk]
            [futon3c.arxana.digest :as digest]
            [futon3c.arxana.records :as records]))

(defn- refuse [reason data]
  (throw (ex-info "Cannot construct Arxana conflict record" (assoc data :error reason))))

(defn- data-only [x]
  (let [data (walk/postwalk #(if (set? %) (vec (sort-by pr-str %)) %) x)]
    (try (digest/canonicalize data)
         (catch Exception _ (refuse :arxana.conflict/non-data {})))
    data))

(defn- required [m fields]
  (doseq [k fields]
    (when (nil? (get m k)) (refuse :arxana.conflict/missing-field {:field k})))
  m)

(defn rule-record [schema system]
  (when-not (some? (:rewrite-doc schema))
    (refuse :arxana.conflict/rewrite-doc-required {:schema (:name schema)}))
  (required schema [:name :pattern])
  (records/make-clusion
   {:type-revision [:rule/content 1] :endpoints []
    :asserted-content
    (data-only {:rule/system system :rule/name (:name schema) :rule/pattern (:pattern schema)
                :rule/guards (vec (sort-by pr-str (map first (:guards schema))))
                :rule/domain-guards (vec (sort-by pr-str (:domain-guards schema)))
                :rule/rewrite (:rewrite-doc schema) :rule/priority (:priority schema 0)})}))

(defn- endpoint [role id]
  {:role role :kind :clusion :target {:clusion/id id}})

(defn- locator [pins]
  (let [loc (:host/locator pins)]
    (when-not (or (= :transcribed loc)
                  (and (map? loc) (string? (:capture/path loc))
                       (string? (:capture/sha256 loc))
                       (re-matches #"[0-9a-f]{64}" (:capture/sha256 loc))
                       (nat-int? (:hit loc))))
      (refuse :arxana.conflict/host-locator-required {:locator loc}))
    loc))

(defn finding-records
  "pins must explicitly supply :host/locator = :transcribed or
  {:capture/path string :capture/sha256 hex :hit integer}. A corpus-file hash
  is not silently substituted for a capture hash. opts may supply :event-time,
  otherwise pins' event-time is used; it is outside the content identity."
  [finding rule-ids opts]
  (let [pins (required (:finding/pins finding) [:checker-sha :schema-digest :mode :max-states])
        refs (:finding/rules finding)
        ids (mapv rule-ids refs)
        _ (when-not (and (= 2 (count ids)) (every? #(and (string? %) (seq %)) ids))
            (refuse :arxana.conflict/rule-id-required {:rules refs}))
        loc (locator pins)
        time (or (:event-time opts) (:event-time pins))
        _ (when-not time (refuse :arxana.conflict/missing-field {:field :event-time}))
        status (:finding/status finding)
        record (records/make-clusion
                {:type-revision [:conflict/finding 1]
                 :endpoints (mapv endpoint [:rule-a :rule-b] (sort ids))
                 :event-time time
                 :asserted-content
                 (data-only {:overlap (:finding/overlap finding) :status status
                             :kinds (:finding/kinds finding) :settled-by (:finding/settled-by finding)
                             :evaluation (:finding/evaluation finding)
                             :checker-sha (:checker-sha pins) :schema-digest (:schema-digest pins)
                             :mode (:mode pins) :max-states (:max-states pins) :host/locator loc})})
        attestation (records/make-attestation
                     {:subject-clusion-id (:id record) :event-time time
                      :producer {:checker (:checker-sha pins)} :basis :exact :standing :verified
                      :coverage (if (= :inconclusive status) :partial :complete)
                      :method {:checker "futon3c.diagramprover.findings"
                               :mode (:mode pins) :max-states (:max-states pins)}
                      :content (data-only {:occurrences (:finding/occurrences finding)
                                           :positions (:finding/positions finding)
                                           :witness-path (:finding/witness-path finding)
                                           :observations (:finding/observations finding)
                                           :hosts (:finding/hosts finding)})
                      :source-refs (if (= loc :transcribed) []
                                       [{:path (:capture/path loc) :sha256 (:capture/sha256 loc)}])})]
    {:finding record :evidence attestation}))

(defn residual-record [finding-id content]
  (required content [:scope :release :reason])
  (records/make-clusion {:type-revision [:residual/accepted 1]
                         :endpoints [(endpoint :finding finding-id)]
                         :asserted-content (data-only (select-keys content [:scope :release :reason]))}))

(defn acceptance-attestation [rule-id acceptance]
  (required acceptance [:reviewer :packet :merge-sha :repo])
  (records/make-attestation
   {:subject-clusion-id rule-id :producer (:reviewer acceptance)
    :basis :human :standing :verified :coverage :complete
    :method (select-keys acceptance [:packet :merge-sha :repo])
    :content {} :source-refs []}))
