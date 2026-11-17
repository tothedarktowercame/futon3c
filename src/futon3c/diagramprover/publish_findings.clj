(ns futon3c.diagramprover.publish-findings
  "Explicit first-publication plan. Building and validating are offline; publish!
  is an explicit call, never a namespace load effect. Persist the plan before it."
  (:require [futon3c.arxana.digest :as digest]
            [futon3c.arxana.store :as store]
            [futon3c.arxana.store-http :as http]
            [futon3c.arxana.write :as write]
            [futon3c.diagramprover.arxana-records :as ar]
            [futon3c.diagramprover.findings :as f]
            [futon3c.diagramprover.finding-view :as view]
            [futon3c.diagramprover.host-pairs :as hp]
            [futon3c.diagramprover.strategy :as strategy]
            [futon3c.diagramprover.term-schema :as ts]
            [futon3c.diagramprover.lean-import :as li]
            [futon3c.diagramprover.lean-alias :as alias]
            [futon3c.diagramprover.opaque-rules :as opaque])
  (:import [java.time Instant] [java.time.format DateTimeFormatterBuilder]))

(def fixtures "test/futon3c/diagramprover/fixtures/")
(defn instant-string [instant]
  (.format (.toFormatter (.appendInstant (DateTimeFormatterBuilder.) 3)) instant))
(defn- locator [file hit]
  {:capture/path file :capture/sha256 (digest/sha256-hex (java.nio.file.Files/readAllBytes
                                                       (java.nio.file.Path/of file (make-array String 0))))
   :hit hit})
(defn- nonjoining [report pins]
  (or (first (filter #(= :non-joining (:finding/status %)) (f/findings report pins)))
      (throw (ex-info "Expected captured conflict absent" {:error :publication/no-conflict}))))
(defn build-batch [{:keys [checker-sha event-time]}]
  (let [beta [(assoc ts/beta :rewrite-doc {:op :substitute :body '?B :argument '?A})
              (assoc li/lean-family :rewrite-doc '[:family-at [:lam ?B] ?A])]
        af (str fixtures "a02J03-borel_pi.jsonl")
        ah (nth (:hits (li/load-hits af)) 18)
        pins (fn [ss file hit bound]
               {:checker-sha checker-sha :schema-digest (f/schema-digest ss)
                :mode :unordered :max-states bound :event-time event-time
                :host/locator (locator file hit)})
        fa (nonjoining (hp/host-pairs beta (:host (li/host ah {:loader-sha checker-sha}))
                                         {:max-states 12 :settled-by? true}) (pins beta af 18 12))
        aliases (mapv #(assoc % :rewrite-doc
                              (if (= :alias-apply (:base-name %))
                                '[:fn-app SYM ?A1 ?A2]
                                {:op :reindex-first-k-then-apply-rest :k :lambda-count}))
                      (concat (alias/spine-schemas alias/alias-apply (range 1 5))
                              (alias/spine-schemas alias/family-reindex (range 1 5))))
        bf (str fixtures "m93J06-exists_global_solution.jsonl")
        bh (first (filter #(= 2 (get-in % [:alias :n_args])) (:hits (li/load-hits bf))))
        bt (:host (alias/annotate (:host (li/host bh {:loader-sha checker-sha})) bh))
        br (hp/host-pairs (alias/without-exemptions aliases) bt {:observe alias/observe})
        br (update br :pairs #(mapv (fn [p] (if (= :non-joining (:status p))
                                             (assoc p :settled-by (strategy/settled-by aliases bt p {:observe alias/observe})) p)) %))
        fb (nonjoining br (pins aliases bf (:hit bh) 100))
        cf (str fixtures "a97J08-step-table-q56-raw,q83.json")
        table (opaque/load-table cf)
        os (mapv (fn [name]
                   {:name name :pattern [:whole-sequence] :guards []
                    :rewrite-doc {:python/qualname (get (:rules table) name)
                                  :git/commit "1970149bcad565e006cc93c3340ea19dfe328387"}})
                 [:q56-raw :q83])
        fc (first (opaque/table-findings table (pins os cf 0 (:max_states table))))
        all-schemas (vec (concat beta (filter #(#{:alias-apply-2 :family-reindex-2} (:name %)) aliases) os))
        rules (mapv #(assoc (ar/rule-record % :apm-printer) :event-time event-time) all-schemas)
        ids (zipmap (map :name all-schemas) (map :id rules))
        fs [fa fb fc]
        _ (when-not (and (every? f/valid-finding? fs)
                         (= [:failing :settled :failing] (mapv :finding/evaluation fs)))
            (throw (ex-info "Unexpected findings" {:error :publication/finding-contract})))
        converted (mapv #(ar/finding-records % ids {}) fs)
        sup (view/supersedes-record (:q83 ids) (:q56-raw ids) "2026-11-17T16:16:12.000Z")
        rows (vec (concat (map #(hash-map :operation :clusion/assert :record %) rules)
                          (mapcat #(vector {:operation :clusion/assert :record (:finding %)}
                                           {:operation :evidence/append :record (:evidence %)}) converted)
                          [{:operation :clusion/assert :record sup}]))]
    {:message-id (str "diagramprover/first-write-2026-11-17/" (digest/canonical-digest rows))
     :event-time event-time :checker-sha checker-sha :rows rows
     :finding-ids (mapv #(get-in % [:finding :id]) converted) :supersedes-id (:id sup)}))

(defn validate-batch!
  "Run the real write authority against a local memory sink. No HTTP calls.
  Refuse immediately, retaining the exact authority response."
  [plan]
  (let [saved (atom {})
        memory (reify store/ArxanaGraphStore
                 (get-entity [_ id] {:ok (get @saved id)})
                 (conditional-write! [_ command]
                   (let [r (get-in command [:body :record])]
                     (swap! saved assoc (:id r) r) {:ok {:created? true}})))]
    (doseq [[i {:keys [operation record]}] (map-indexed vector (:rows plan))]
      (let [result (write/execute memory {:principal/id "arxana-service"
                                         :message/id (str (:message-id plan) "/dry/" i)} operation record)]
        (when (:error result) (throw (ex-info "Local validation refused" {:index i :response result})))
        (println (:id record))))
    {:validated (count @saved)}))

(defn publish! [graph-store plan]
  (validate-batch! plan)
  (let [send #(http/append-batch! graph-store (:message-id plan) (:rows plan))
        result (send)]
    (if (= :arxana.store/http-unavailable (get-in result [:error :error/code]))
      (send)
      result)))

(defn read-back [graph-store plan now]
  (let [reads (mapv #(let [id (get-in % [:record :id])]
                      {:id id :valid-time now :result (http/get-entity-as-of graph-store id now)}) (:rows plan))
        records (mapv #(get-in % [:result :ok]) reads)
        before (instant-string (.minusSeconds (Instant/parse (:event-time plan)) 1))]
    {:now reads
     :views (when (every? some? records)
              (mapv #(view/finding-view records % now) (:finding-ids plan)))
     :stale (when (every? some? records) (view/stale-findings records now))
     :before-finding {:valid-time before :result (http/get-entity-as-of graph-store (last (:finding-ids plan)) before)}
     :before-supersedes {:valid-time "2026-11-17T16:16:11.000Z"
                        :result (http/get-entity-as-of graph-store (:supersedes-id plan) "2026-11-17T16:16:11.000Z")}}))
