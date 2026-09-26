#!/usr/bin/env bb
;; held_work_ledger_land.bb — E-held-work-ledger: land the harvested :held/* ledger into
;; futon1b (:7073) as `held/item` entities + `held/on-mission` hyperedges on CANONICAL
;; <repo>-d/mission/<id> nodes (C-cascade-real standard 5 — composed). Mirrors claude-2's
;; d4_feeder_b_load.bb: additive, penholder "api", idempotent, resolve-before-edge, no JVM restart.
;;
;;   bb held_work_ledger_land.bb          # DRY RUN: resolve + honest-hole report, NO writes
;;   bb held_work_ledger_land.bb --write  # write entities + hyperedges (penholder "api")
;;
;; Held-items are keyed by a single canonical id with :id == :name (held/item/<ns>/<name>) —
;; practicing the E-futon1a-archivist discipline on this new type. (:held/item has no gate
;; descriptor yet; ratifying one with claude-2, the model owner, is a follow-on.)
(require '[babashka.http-client :as http]
         '[cheshire.core :as json]
         '[clojure.edn :as edn]
         '[clojure.string :as str])

(def LEDGER "/home/joe/code/futon3c/holes/excursions/held-work-ledger.edn")
(def BASE "http://127.0.0.1:7073/api/alpha")
(def PENHOLDER "api")

(defn encode [x]
  (java.net.URLEncoder/encode (str x) "UTF-8"))

(defn get-edn [path params]
  (let [query (str/join "&" (map (fn [[k v]] (str (name k) "=" (encode v))) params))
        r (http/get (str BASE path "?" query)
                    {:headers {"Accept" "application/edn"} :throw false})]
    (when-not (= 200 (:status r))
      (throw (ex-info "futon1b read failed"
                      {:path path :params params :status (:status r) :body (:body r)})))
    (edn/read-string (:body r))))

(defn paged [path result-key type-key type-name]
  (loop [after nil, acc [], expected nil]
    (let [page (get-edn path (cond-> [[type-key type-name] [:limit 1000]]
                               after (conj [:after after])))
          rows (result-key page)
          expected (or expected (:count page))
          acc (into acc rows)]
      (if-let [cursor (:next-cursor page)]
        (recur cursor acc expected)
        (do
          (when-not (= expected (count acc))
            (throw (ex-info "paged read did not consume advertised count"
                            {:path path :type type-name
                             :expected expected :consumed (count acc)})))
          acc)))))

(defn paged-entities [entity-type]
  (paged "/entities" :entities :type entity-type))

(defn paged-hyperedges [hx-type]
  (paged "/hyperedges" :hyperedges :type hx-type))

(defn post-json [path m]
  (http/post (str BASE path)
             {:headers {"Content-Type" "application/json" "x-penholder" PENHOLDER}
              :body (json/generate-string m) :throw false}))

(def items (:items (edn/read-string (slurp LEDGER))))

(defn held-id [i]
  (let [k (:held/id i)] (str "held/item/" (namespace k) "/" (name k))))

(defn flat-props [i]
  (into {} (for [[k v] (select-keys i [:held/reason :held/owner :held/status :held/kind
                                       :held/wake-trigger :held/evidence-condition
                                       :held/review-by :held/re-entry :held/raised-at])
                 :when (some? v)]
             [k (if (keyword? v) (name v) v)])))

(defn mission-aliases [mission]
  (remove nil? [(:entity/name mission)
                (:entity/external-id mission)
                (get-in mission [:entity/props :mission/title])]))

(defn mission-index [required-aliases]
  (let [candidates
        (reduce (fn [index mission]
                  (reduce #(update %1 %2 (fnil conj #{}) (:entity/name mission))
                          index
                          (mission-aliases mission)))
                {}
                (paged-entities "mission/doc"))]
    (into {}
          (for [[alias names] candidates
                :when (contains? required-aliases alias)]
            (let [canonical (filter #(re-find #"-d/mission/" %) names)
                  chosen (cond
                           (= 1 (count canonical)) (first canonical)
                           (= 1 (count names)) (first names)
                           :else (throw (ex-info "mission alias has no unique canonical target"
                                                 {:alias alias :candidates (sort names)})))]
              [alias chosen])))))

(defn resolved-missions [missions i]
  (keep missions (:held/missions i)))

(def required-mission-aliases
  (set (mapcat :held/missions items)))

(defn verify-layer! [intended-items intended-edges]
  (let [entities (paged-entities "held/item")
        edges (paged-hyperedges "held/on-mission")
        mission-entities (paged-entities "mission/doc")
        missions (mission-index required-mission-aliases)
        entity-names (set (keep :entity/name entities))
        mission-names (set (keep :entity/name mission-entities))
        ledger-ids (set (map held-id items))
        landed-entities (filter #(contains? ledger-ids (:entity/name %)) entities)
        expected-edge-ids (set (for [i items, m (resolved-missions missions i)]
                                 (str "hx|held-on-mission|" (held-id i) "|" m)))
        landed-edges (filter #(contains? expected-edge-ids (:hx/id %)) edges)
        unresolved (->> landed-edges
                        (mapcat :hx/ends)
                        (map :entity-id)
                        (remove #(or (contains? entity-names %)
                                     (contains? mission-names %)))
                        distinct sort vec)]
    (println (format "  read-back: held/item ledger entities=%d/%d; held/on-mission=%d/%d; unresolved endpoints=%d"
                     (count landed-entities) intended-items
                     (count landed-edges) intended-edges (count unresolved)))
    (when-not (= intended-items (count landed-entities))
      (throw (ex-info "held entity read-back count differs from intent"
                      {:intended intended-items :actual (count landed-entities)})))
    (when-not (= intended-edges (count landed-edges))
      (throw (ex-info "held edge read-back count differs from intent"
                      {:intended intended-edges :actual (count landed-edges)})))
    (when (seq unresolved)
      (throw (ex-info "held edges contain unresolved endpoints"
                      {:unresolved unresolved})))
    {:items (count landed-entities) :edges (count landed-edges)
     :unresolved unresolved}))

(defn -main [& args]
  (let [write?     (some #{"--write"} args)
        missions (mission-index required-mission-aliases)
        item-groups (group-by held-id items)
        conflicting-items (into {} (filter (fn [[_ rows]] (< 1 (count (distinct rows)))) item-groups))
        _ (when (seq conflicting-items)
            (throw (ex-info "held id has conflicting source rows"
                            {:ids (sort (keys conflicting-items))})))
        unique-items (mapv (comp first val) item-groups)
        duplicate-items (- (count items) (count unique-items))
        edges (distinct (for [i unique-items, m (resolved-missions missions i)] [i m]))
        with-edges (filter #(seq (resolved-missions missions %)) unique-items)
        edge-count (count edges)
        detached   (remove #(seq (resolved-missions missions %)) unique-items)
        unresolved-aliases (sort (remove #(contains? missions %) required-mission-aliases))]
    (println (format "held source rows=%d -> unique entities=%d (exact duplicate rows=%d)"
                     (count items) (count unique-items) duplicate-items))
    (println (format "mission-resolving: %d items -> %d held/on-mission edges | detached (no resolvable mission): %d"
                     (count with-edges) edge-count (count detached)))
    (println "by registry:" (frequencies (map #(get-in % [:held/source :registry]) items)))
    (println (format "source mission aliases unresolved: %d" (count unresolved-aliases)))
    (doseq [alias unresolved-aliases]
      (println "  UNRESOLVED SOURCE MISSION" alias))
    (when-not write?
      (println "\n-- DRY RUN — sample of what WOULD land --")
      (doseq [i (take 4 with-edges)]
        (println (format "  ENTITY %s :held/item" (held-id i)))
        (doseq [m (resolved-missions missions i)]
          (println (format "    HX held/on-mission [%s , %s]" (held-id i) m)))))
    (when write?
      (println "\n-- WRITING (penholder \"api\", idempotent) --")
      (let [er (atom {:ok 0 :err 0}) hr (atom {:ok 0 :err 0})]
        ;; Phase A: held/item entities (id == name, canonical single scheme)
        (doseq [i unique-items]
          (let [r (post-json "/entity" {:penholder PENHOLDER
                                        :id (held-id i) :name (held-id i)
                                        :type "held/item" :external-id (held-id i)
                                        :source "E-held-work-ledger"
                                        :props (flat-props i)})]
            (if (#{200 201} (:status r)) (swap! er update :ok inc)
                (do (swap! er update :err inc)
                    (when (< (:err @er) 4) (println "  ENT ERR" (:status r) (subs (str (:body r)) 0 (min 200 (count (str (:body r)))))))))))
        (println "  entities:" @er)
        ;; Phase B: held/on-mission hyperedges (stable id, idempotent)
        (doseq [[i m] edges]
          (let [r (post-json "/hyperedge"
                             {:penholder PENHOLDER
                              :hx/id (str "hx|held-on-mission|" (held-id i) "|" m)
                              :hx/type "held/on-mission"
                              :hx/endpoints [(held-id i) m]
                              :props {:held/disposition "held"
                                      :held/source-registry (name (get-in i [:held/source :registry]))
                                      :held/reason (:held/reason i)}})]
            (if (#{200 201} (:status r)) (swap! hr update :ok inc)
                (do (swap! hr update :err inc)
                    (when (< (:err @hr) 4) (println "  HX ERR" (:status r) (subs (str (:body r)) 0 (min 200 (count (str (:body r)))))))))))
        (println "  hyperedges:" @hr)
        (when (or (pos? (:err @er)) (pos? (:err @hr)))
          (throw (ex-info "held ledger write failed" {:entities @er :hyperedges @hr})))
        (verify-layer! (count unique-items) edge-count)))))

(apply -main *command-line-args*)
