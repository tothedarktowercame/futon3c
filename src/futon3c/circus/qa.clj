(ns futon3c.circus.qa
  "Bounded, read-only interactive QA over a pinned Giraffe run and the GCC.

  From scripts/proof-eval.sh:
    (require '[futon3c.circus.qa :as qa])
    (qa/top-items {:run-dir \"/home/joe/code/storage/giraffe/RUN\" :n 5})
    (qa/why-not {:run-dir \"/home/joe/code/storage/giraffe/RUN\"
                 :item \"M-example\"})

  Animal keys in returned maps may be constructed in ASCII-only callers with
  (keyword \"驴\"), (keyword \"馬\"), (keyword \"狗\"), and (keyword \"象\")."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon3c.arxana.store :as store]
            [futon3c.arxana.turn-mention-writer :as mentions]
            [futon3c.circus.chain-record :as chain]
            [futon3c.circus.hou :as hou]))

(def ^:private max-top-items 50)
(def ^:private max-focus-pins 1000)
(def ^:private max-focus-relations 200)
(def ^:private animals [:驴 :馬 :狗 :象 :mentions :猴])

(defn- read-edn [run-dir file fallback]
  (let [f (io/file run-dir file)]
    (if (.isFile f) (edn/read-string (slurp f)) fallback)))

(defn- item-name [x]
  (when (some? x) (if (keyword? x) (name x) (str x))))

(defn- endpoint-id [record role]
  (some (fn [endpoint]
          (when (= role (:role endpoint))
            (get-in endpoint [:target :clusion/id])))
        (:endpoints record)))

(defn- get-entity [cfg id]
  (:ok ((or (:get-entity-fn cfg) store/get-entity) (:graph-store cfg) id)))

(defn- pinned-ids [manifest]
  (->> (get-in manifest [:focus-relation-input :evidence-pin :evidence-pin])
       (keep #(when (sequential? %) (first %)))
       distinct vec))

(defn- focus-relations [cfg manifest item]
  (let [pins (pinned-ids manifest)
        scanned (set (take max-focus-pins pins))
        item-clusion (:id (hou/item-record item))
        answer ((or (:query-fn cfg) store/bounded-query!)
                (:graph-store cfg)
                {:query/type :role-reverse :edge-type [:hou/focus-relation 1]
                 :endpoint-role :item :endpoint-id item-clusion
                 :limit max-focus-relations :order :asc})
        value (:ok answer)
        _ (when-not (= :ok (:status value))
            (throw (ex-info "Focus relation query refused"
                            {:item item :answer answer})))
        matches (->> (:edge-ids value)
                     (filter scanned)
                     (keep #(get-entity cfg %))
                     (sort-by :id) vec)]
    {:relations matches
     :pin-count (count pins)
     :scanned (count scanned)
     :truncated? (or (> (count pins) max-focus-pins) (:truncated? value))}))

(defn- gcc-counts [record]
  (into {}
        (for [animal animals]
          [animal (count (get-in record [:gcc animal :clusion-ids] []))])))

(defn- explain-relation [cfg run-dir relation]
  (let [focus-id (endpoint-id relation :focus)
        header (get-entity cfg focus-id)
        turn-id (get-in header [:asserted-content :hou/turn])
        record (when turn-id
                 ((or (:chain-fn cfg) chain/chain-record)
                  (assoc cfg :turn-id turn-id :run-dir run-dir)))]
    {:clusion-id (:id relation)
     :turn (when record (select-keys (:turn record) [:id :created_at]))
     :gcc-counts (when record (gcc-counts record))}))

(defn- focus-evidence [cfg manifest run-dir item]
  (let [{:keys [relations truncated? pin-count scanned]} (focus-relations cfg manifest item)]
    {:focus-relations (mapv #(explain-relation cfg run-dir %) relations)
     :focus-pin {:count pin-count :scanned scanned :truncated? truncated?}
     :focus-status (if (seq relations) :present :absent)
     :focus-reason (when-not (seq relations) :no-pinned-focus-relation)}))

(defn top-items
  "Explain at most N (default 10, maximum 50) leading priority rows. Every
  focus scan is capped at 1000 pins and 200 indexed relations and reports
  :truncated?."
  [{:keys [run-dir n] :or {n 10} :as opts}]
  (when-not run-dir (throw (ex-info "run-dir is required" {})))
  (let [requested (max 0 (long n))
        limit (min max-top-items requested)
        cfg (merge (mentions/default-config) opts)
        manifest (read-edn run-dir "manifest.edn" {})
        priority (read-edn run-dir "priority.edn" [])
        rows (take limit priority)]
    {:status :ok
     :run-id (:run-id manifest)
     :limit limit
     :truncated? (or (> requested max-top-items) (> (count priority) limit))
     :items
     (mapv (fn [rank row]
             (let [item (item-name (:target row))]
               (merge {:item item :rank rank :pass-rank (:pass-rank row)
                       :priority-band (:priority-band row)
                       :cutoff-status (:cutoff-status row)}
                      (focus-evidence cfg manifest run-dir item))))
           (range 1 (inc (count rows))) rows)}))

(defn- find-item [rows item]
  (some #(when (= item (item-name (or (:id %) (:target %) (:from %)))) %) rows))

(defn- find-rejected-item [rows item]
  ;; rejected.edn is primarily rejected dependency edges. Their :from/:to are
  ;; not rejected work items; only an explicit item :id removes the item.
  (some #(when (= item (item-name (:id %))) %) rows))

(defn- pass-row [pass item]
  (or (some #(when (= item (item-name (:id %))) %) (:order pass))
      (when (contains? (set (map item-name (:ready-set pass))) item)
        {:id item :status :ready})))

(defn why-not
  "Walk ITEM through input, exclusion/rejection, pass, and priority. Returns at
  the first drop stage, with pinned focus evidence included at every outcome."
  [{:keys [run-dir item] :as opts}]
  (when-not (and run-dir (seq item))
    (throw (ex-info "run-dir and item are required" {})))
  (let [item (item-name item)
        cfg (merge (mentions/default-config) opts)
        manifest (read-edn run-dir "manifest.edn" {})
        input (read-edn run-dir "input.edn" {})
        excluded (read-edn run-dir "excluded.edn" [])
        rejected (read-edn run-dir "rejected.edn" [])
        pass (read-edn run-dir "pass.edn" {})
        priority (read-edn run-dir "priority.edn" [])
        input-row (find-item (:items input) item)
        excluded-row (find-item excluded item)
        rejected-row (find-rejected-item rejected item)
        pass-row* (pass-row pass item)
        priority-index (first (keep-indexed #(when (= item (item-name (:target %2))) %1)
                                            priority))
        priority-row* (when (some? priority-index) (nth priority priority-index))
        focus (focus-evidence cfg manifest run-dir item)
        outcome (cond
                  (nil? input-row) {:stage :input :reason :not-in-input :evidence {}}
                  excluded-row {:stage :excluded :reason (:reason excluded-row)
                                :evidence (select-keys excluded-row [:status-line :source])}
                  rejected-row {:stage :rejected
                                :reason (or (:reason rejected-row) (:reject rejected-row))
                                :evidence (select-keys rejected-row [:file :line :quote :to])}
                  (nil? pass-row*) {:stage :pass :reason :not-in-pass :evidence {}}
                  (nil? priority-row*) {:stage :priority :reason :not-ranked
                                       :evidence {:pass-rank (:rank pass-row*)}}
                  :else {:stage :ranked :reason :ranked
                         :evidence {:rank (inc priority-index)
                                    :pass-rank (:pass-rank priority-row*)
                                    :priority-band (:priority-band priority-row*)
                                    :cutoff-status (:cutoff-status priority-row*)}})]
    (merge {:item item :status :ok} outcome focus)))
