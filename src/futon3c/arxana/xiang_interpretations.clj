(ns futon3c.arxana.xiang-interpretations
  "Production 象 act interpretations over the shared operator evidence reference."
  (:require [clojure.string :as str]
            [futon3c.arxana.digest :as digest]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.store :as store]
            [futon3c.arxana.store-http :as http]
            [futon3c.arxana.turn-mention-writer :as mentions]
            [futon3c.xiang.turn-store :as turns]))

(defn operator-acts
  "TURN's operator acts. Act ids live under the agent turn id (:turn_id,
  e.g. \"claude-3-turn-9-f-s1-0\"), not the turn record id."
  [turn]
  (let [base (str (:turn_id turn) "-f-")]
    (filterv #(and (= "operator" (:author %))
                   (string? (:id %)) (.startsWith ^String (:id %) base)) (:acts turn))))

(defn- target-reference [cfg turn turn-ref act]
  (if-let [[kind value] (cond (not (str/blank? (str (:to act)))) [:addressed-agent (:to act)]
                              (not (str/blank? (str (:turn act)))) [:addressed-turn (:turn act)])]
    (records/make-external-reference
     {:ref/kind kind :store/id "futon3c/turn-acts"
      :store/endpoint (get-in cfg [:turn-store :dir] "futon3c/turns")
      :record/id (str value) :schema/authority :futon3c/typed-act-target
      :valid-time (or (:created_at turn) "unknown")
      :revision/digest (digest/sha256-hex (.getBytes (str value) "UTF-8"))})
    turn-ref))

(defn plan [cfg turn]
  (if-let [{:keys [reference text]} (mentions/field-evidence cfg turn :source_text)]
    (let [acts (operator-acts turn)
          rows (mapcat
                (fn [act]
                  (let [span [(:start act) (:end act)]
                        target-ref (target-reference cfg turn reference act)
                        claim (records/make-clusion
                               {:type-revision [:interpretation 1]
                                :endpoints [{:role :source :kind :manifestation-scope
                                             :target {:artifact/revision-id (:id reference)
                                                      :locator {:codepoint-range span}}}
                                            {:role :intent :kind :entity
                                             :target {:intent/id (keyword (:kind act))}}
                                            ;; The operator evidence turn is the typed
                                            ;; fallback target; typed :to/:turn wins.
                                            {:role :target :kind :external-ref
                                             :target {:ref/id (:id target-ref)}}]
                                :asserted-content {:reading/state :candidate}})
                        att (records/make-attestation
                             {:subject-clusion-id (:id claim) :producer "象" :basis :model
                              :standing :unverified :coverage :complete
                              :method {:id "xiang/read" :version 1
                                       :parameters {:interpretation-version (:interpretation_version turn)
                                                    :vocabulary-version (:vocabulary_version turn)}}
                              :content {:act/id (:id act) :about (:about act) :to (:to act)
                                        :text-sha256 (digest/sha256-hex (.getBytes (str (:text act)) "UTF-8"))}
                              :source-refs [{:evidence/id (:evidence_id turn) :range span}]})]
                    (cond-> [{:operation :external-reference/assert :record reference}]
                      (not= (:id reference) (:id target-ref))
                      (conj {:operation :external-reference/assert :record target-ref})
                      true (conj {:operation :clusion/assert :record claim}
                                 {:operation :evidence/append :record att})))) acts)]
      {:rows (vec rows) :acts (count acts) :reference reference :text text})
    {:failed :operator-evidence-unavailable}))

(defn write! [cfg turn]
  (let [{:keys [rows acts failed] :as p} (plan cfg turn)]
    (cond failed p
          (zero? acts) {:written 0}
          :else (let [r (http/append-batch! (:graph-store cfg)
                                             (str "xiang-interpretations/" (:id turn)) rows)]
                  (if (:ok r) {:written acts} {:failed :interpretation-write-refused})))))

(defn read-acts [cfg turn]
  (if-let [{:keys [reference text]} (mentions/field-evidence cfg turn :source_text)]
    (let [q (:ok (store/bounded-query! (:graph-store cfg)
                                       {:query/type :role-reverse :edge-type [:interpretation 1]
                                        :endpoint-role :source :endpoint-id (:id reference)
                                        :limit 200 :order :asc}))
          claims (mapv #(-> (store/get-entity (:graph-store cfg) %) :ok) (:edge-ids q))]
      (if (empty? claims) {:pending :interpretations-pending}
        {:acts (->> claims
                    (map (fn [claim]
                           (let [aq (:ok (store/bounded-query! (:graph-store cfg)
                                                              {:query/type :attestations
                                                               :subject-id (:id claim) :limit 20}))
                                 att (:ok (store/get-entity (:graph-store cfg) (first (:attestation-ids aq))))
                                 [start end] (get-in claim [:endpoints 0 :target :locator :codepoint-range])]
                             {:id (get-in att [:content :act/id])
                              :kind (name (get-in claim [:endpoints 1 :target :intent/id]))
                              :author "operator" :start start :end end
                              :text (subs text (.offsetByCodePoints ^String text 0 start)
                                          (.offsetByCodePoints ^String text 0 end))})))
                    (sort-by :start) vec)}))
    {:pending :interpretations-pending}))

(defn backfill! [cfg {:keys [write? pause-ms] :or {write? false pause-ms 200}}]
  (let [cutoff (- (System/currentTimeMillis) (* 6 3600000))
        rows (->> (turns/list-records (:turn-store cfg) :limit Long/MAX_VALUE)
                  (keep (fn [{:keys [id record]}]
                          (let [at (some-> (:created_at record) str java.time.Instant/parse .toEpochMilli)]
                            (when (and at (< cutoff at) (= "operator" (:origin record))
                                       (= "analyzed" (:analysis_status record)) (seq (:acts record)))
                              (assoc record :id id))))))
        outcomes (mapv (fn [turn]
                         (if-not (:pending (read-acts cfg turn)) :already-written
                           (if write?
                             (let [r (write! cfg turn)]
                               (when (pos? pause-ms) (Thread/sleep pause-ms))
                               (cond (:failed r) (:failed r)
                                     (pos? (:written r)) :written
                                     :else :no-operator-acts))
                             (let [{:keys [rows failed]} (plan cfg turn)]
                               (cond failed failed
                                     (seq rows) :would-write
                                     :else :no-operator-acts))))) rows)]
    {:mode (if write? :write :dry-run) :turns (count rows) :outcomes (frequencies outcomes)}))
