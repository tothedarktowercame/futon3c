(ns futon3c.arxana.ma-strand
  "Publish 馬's durable turn connection and cross-turn quotations as GCC edges."
  (:require [futon3c.arxana.digest :as digest]
            [futon3c.arxana.pattern-proposals :as proposals]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.store :as store]
            [futon3c.arxana.store-http :as http]
            [futon3c.arxana.turn-mention-writer :as mentions]
            [futon3c.xiang.turn-store :as turns]))

(def part-keys [:I1 :I2 :G :B])

(defn- turn-reference [cfg turn]
  (:reference (mentions/field-evidence cfg turn :source_text)))

(defn- earlier? [window current quoted]
  (let [positions (zipmap window (range))]
    (and (not= current quoted)
         (contains? positions current)
         (contains? positions quoted)
         (< (positions quoted) (positions current)))))

(defn- message-id [turn]
  (let [c (get-in turn [:pattern_verdicts :connection])]
    (str "ma-strand/" (:id turn) "/"
         (digest/canonical-digest
          (select-keys c [:status :turn/id :sha256 :reason])))))

(defn plan [cfg turn]
  (let [verdict (:pattern_verdicts turn)
        connection (:connection verdict)
        status (some-> (:status connection) keyword)
        turn-ref (turn-reference cfg turn)]
    (cond
      (nil? verdict) {:failed :ma-verdict-unavailable}
      (nil? connection) {:failed :ma-connection-unavailable}
      (nil? turn-ref) {:failed :operator-evidence-unavailable}
      (and (= :linked status)
           (nil? (turns/read-record (:turn-store cfg) (:turn/id connection))))
      {:failed :predecessor-turn-unavailable}
      (and (= :linked status)
           (nil? (turn-reference
                  cfg (assoc (turns/read-record (:turn-store cfg) (:turn/id connection))
                             :id (:turn/id connection)))))
      {:failed :predecessor-evidence-unavailable}
      :else
      (let [prev-turn (when (= :linked status)
                        (some-> (turns/read-record (:turn-store cfg) (:turn/id connection))
                                (assoc :id (:turn/id connection))))
            prev-ref (when prev-turn (turn-reference cfg prev-turn))
            strand (records/make-clusion
                    {:type-revision [:ma/strand 1]
                     :endpoints (cond-> [{:role :turn :kind :external-ref
                                          :target {:ref/id (:id turn-ref)}}]
                                  prev-ref (conj {:role :prev :kind :external-ref
                                                  :target {:ref/id (:id prev-ref)}}))
                     :asserted-content {:connection/status status
                                        :connection/sha256 (:sha256 connection)
                                        :connection/reason (:reason connection)}})
            window (vec (:window verdict))
            quotes (for [row (:patterns verdict)
                         window-key [:turn :chain]
                         part part-keys
                         :let [mapped (get-in row [window-key part])
                               quoted-id (:turn mapped)]
                         :when (and (:element mapped)
                                    (earlier? window (:id turn) quoted-id))
                         :let [quoted-turn (some-> (turns/read-record (:turn-store cfg) quoted-id)
                                                   (assoc :id quoted-id))
                               quoted-ref (when quoted-turn (turn-reference cfg quoted-turn))
                               pattern-ref (proposals/pattern-reference
                                            (or (:pattern-library cfg) proposals/default-library)
                                            (:pattern row))]
                         :when (and quoted-ref pattern-ref)]
                     {:refs [quoted-ref pattern-ref]
                      :claim (records/make-clusion
                              {:type-revision [:ma/quote 1]
                               :endpoints [{:role :turn :kind :external-ref
                                            :target {:ref/id (:id turn-ref)}}
                                           {:role :quoted-turn :kind :external-ref
                                            :target {:ref/id (:id quoted-ref)}}
                                           {:role :pattern :kind :external-ref
                                            :target {:ref/id (:id pattern-ref)}}]
                               :asserted-content {:part part :window window-key}})})
            refs (distinct (concat [turn-ref] (when prev-ref [prev-ref]) (mapcat :refs quotes)))
            claims (cons strand (map :claim quotes))
            rows (vec (concat (map #(hash-map :operation :external-reference/assert :record %) refs)
                              (map #(hash-map :operation :clusion/assert :record %) claims)))]
        {:rows rows :strand strand :quotes (count quotes)}))))

(defn write! [cfg turn]
  (let [{:keys [rows quotes failed] :as p} (plan cfg turn)]
    (if failed p
        (let [result (http/append-batch!
                      (:graph-store cfg)
                      (message-id turn)
                      rows)]
          (if (:ok result) {:written 1 :quotes quotes}
              {:failed :ma-strand-write-refused})))))

(defn read-predecessor
  "Bounded reverse walk. Linked wins over the older pending edge after repair."
  [cfg turn]
  (if-let [turn-ref (turn-reference cfg turn)]
    (let [q (:ok (store/bounded-query! (:graph-store cfg)
                                       {:query/type :role-reverse
                                        :edge-type [:ma/strand 1]
                                        :endpoint-role :turn
                                        :endpoint-id (:id turn-ref)
                                        :limit 20 :order :asc}))
          claims (keep #(-> (store/get-entity (:graph-store cfg) %) :ok)
                       (:edge-ids q))
          latest (or (some #(when (= :linked (get-in % [:asserted-content :connection/status])) %) claims)
                     (last claims))]
      {:status (get-in latest [:asserted-content :connection/status])
       :predecessor-reference-id (some #(when (= :prev (:role %))
                                          (get-in % [:target :ref/id]))
                                       (:endpoints latest))})
    {:pending :operator-evidence-unavailable}))

(defn backfill!
  [cfg {:keys [write? pause-ms] :or {write? false pause-ms 25}}]
  (let [candidates (->> (turns/list-records (:turn-store cfg) :limit Long/MAX_VALUE)
                        (keep (fn [{:keys [id record]}]
                                (when (and (= "operator" (:origin record))
                                           (get-in record [:pattern_verdicts :connection]))
                                  (assoc record :id id)))))
        outcomes (mapv (fn [turn]
                         (let [p (plan cfg turn)]
                           (if (:failed p) (:failed p)
                               (if-not write? :would-write
                                   (let [r (write! cfg turn)]
                                     (when (pos? pause-ms) (Thread/sleep pause-ms))
                                     (if (:written r) :written (:failed r)))))))
                       candidates)]
    {:mode (if write? :write :dry-run)
     :turns (count candidates)
     :outcomes (frequencies outcomes)}))
