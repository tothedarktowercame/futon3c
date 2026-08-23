#!/usr/bin/env bb
;; d4_feeder_b_load.bb — C-cascade-real D4 feeder-(b): land the 177 :mined-structural
;; (have→want) arrows from futon6/data/diffsub-moves-mined.edn into futon1b (:7073)
;; as code/v05/mined-move hyperedges on CANONICAL mission nodes.
;;
;;   have-side: <repo>-d/mission/<id>  (already canonical; existing nodes only)
;;   want-side: <canonical-have>-<facet>  (facet = head | <phase>); NEW :mission/head nodes
;;   hx/type:   code/v05/mined-move   props {:status :correlated :mined-structural true ...}
;;
;; Honestly tagged :correlated (no method) — NOT a proof relation (T-A4 clean).
;;
;;   bb d4_feeder_b_load.bb            # DRY RUN: parse + resolve + honest-hole report, no writes
;;   bb d4_feeder_b_load.bb --write    # write entities + hyperedges (penholder "api")
;;   bb d4_feeder_b_load.bb --write --only <have-substr>   # smoke: write only matching arrows
(require '[babashka.http-client :as http]
         '[cheshire.core :as json]
         '[clojure.edn :as edn]
         '[clojure.string :as str])

(def SRC "/home/joe/code/futon6/data/diffsub-moves-mined.edn")
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

(defn paged-entities [entity-type]
  (loop [after nil, acc [], expected nil]
    (let [page (get-edn "/entities" (cond-> [[:type entity-type] [:limit 1000]]
                                      after (conj [:after after])))
          rows (:entities page)
          expected (or expected (:count page))
          acc (into acc rows)]
      (if-let [cursor (:next-cursor page)]
        (recur cursor acc expected)
        (do
          (when-not (= expected (count acc))
            (throw (ex-info "entity paging did not consume advertised count"
                            {:type entity-type :expected expected :consumed (count acc)})))
          acc)))))

(defn paged-hyperedges [hx-type]
  (loop [after nil, acc [], expected nil]
    (let [page (get-edn "/hyperedges" (cond-> [[:type hx-type] [:limit 1000]]
                                        after (conj [:after after])))
          rows (:hyperedges page)
          expected (or expected (:count page))
          acc (into acc rows)]
      (if-let [cursor (:next-cursor page)]
        (recur cursor acc expected)
        (do
          (when-not (= expected (count acc))
            (throw (ex-info "hyperedge paging did not consume advertised count"
                            {:type hx-type :expected expected :consumed (count acc)})))
          acc)))))

(defn mission-node-names []
  (->> ["mission/doc" "mission/head" "mission/scope-target"]
       (mapcat paged-entities)
       (keep :entity/name)
       set))

(defn verify-layer! [intended-count]
  (let [edges (paged-hyperedges "code/v05/mined-move")
        mission-names (mission-node-names)
        unresolved-ends (->> edges
                             (mapcat :hx/ends)
                             (map :entity-id)
                             (remove mission-names)
                             distinct
                             sort
                             vec)]
    (println (format "  read-back: hyperedges=%d intended=%s unresolved mission ends=%d"
                     (count edges) (or intended-count "filtered smoke run") (count unresolved-ends)))
    (when (seq unresolved-ends)
      (doseq [endpoint unresolved-ends]
        (println "  UNRESOLVED END" endpoint)))
    (when (and intended-count (not= intended-count (count edges)))
      (throw (ex-info "mined-move read-back count differs from intended count"
                      {:intended intended-count :actual (count edges)})))
    (when (seq unresolved-ends)
      (throw (ex-info "mined-move contains unresolved mission ends"
                      {:unresolved unresolved-ends})))
    {:count (count edges), :unresolved-ends unresolved-ends}))

(defn post-json [path m]
  (http/post (str BASE path)
             {:headers {"Content-Type" "application/json" "x-penholder" PENHOLDER}
              :body (json/generate-string m) :throw false}))

;; --- parse the 177 mined arrows ---
(def moves
  (let [data (edn/read-string (slurp SRC))]
    (->> (:moves data)
         (filter #(= :mined-structural (:confidence %))))))

(defn canon-have [m] (:have m))                       ;; already <repo>-d/mission/<id>
(defn facet [m] (-> (:want m) (str/split #"/") last str/lower-case))  ;; head | argue | ...
(defn want-node [m] (str (canon-have m) "-" (facet m)))
(defn want-type [m] (if (= "head" (facet m)) "mission/head" "mission/scope-target"))

;; --- resolve which have-nodes exist ---
(def existing-canonical
  (->> (paged-entities "mission/doc") (keep :entity/name) set))

(defn -main [& args]
  (let [write? (some #{"--write"} args)
        only (second (drop-while #(not= "--only" %) args))
        arrows (cond->> moves only (filter #(str/includes? (canon-have %) only)))
        {res true unres false} (group-by #(contains? existing-canonical (canon-have %)) arrows)
        existing-wants (set (concat (map :entity/name (paged-entities "mission/head"))
                                    (map :entity/name (paged-entities "mission/scope-target"))))
        missing-wants (remove #(contains? existing-wants (want-node %)) res)]
    (println (format "mined arrows parsed: %d  | arrows resolved: %d  | arrows would be written: %d"
                     (count arrows) (count res) (count res)))
    (println (format "honest holes: have=%d  want-existing=%d (want nodes are created before arrows)"
                     (count unres) (count missing-wants)))
    (println (format "facets: head=%d  phase=%d"
                     (count (filter #(= "head" (facet %)) res))
                     (count (remove #(= "head" (facet %)) res))))
    (when (seq unres)
      (println "HONEST HOLES (have-side non-resolving):")
      (doseq [m (take 20 unres)] (println "  " (canon-have m))))
    (when-not write?
      (println "\n-- DRY RUN — sample of what WOULD be written --")
      (doseq [m (take 4 res)]
        (println (format "  ENTITY %s :%s" (want-node m) (want-type m)))
        (println (format "  HX code/v05/mined-move  [%s , %s]  conf=%s dg=%s class=%s"
                         (canon-have m) (want-node m) (:score m) (:delta-g m) (:move/class m)))))
    (when write?
      (println "\n-- WRITING (penholder \"api\") --")
      (let [ent-results (atom {:ok 0 :err 0})
            hx-results  (atom {:ok 0 :err 0})]
        ;; Phase A: want-side entities (idempotent ensure-entity by name)
        (doseq [m res]
          (let [r (post-json "/entity" {:penholder PENHOLDER
                                        :name (want-node m)
                                        :type (want-type m)
                                        :external-id (want-node m)
                                        :source "diffsub-mine/M-differentiable-substrate"})]
            (if (#{200 201} (:status r)) (swap! ent-results update :ok inc)
                (do (swap! ent-results update :err inc)
                    (when (< (:err @ent-results) 4) (println "  ENT ERR" (:status r) (subs (str (:body r)) 0 (min 200 (count (str (:body r)))))))))))
        (println "  entities:" @ent-results)
        (when (pos? (:err @ent-results))
          (throw (ex-info "entity writes failed" @ent-results)))
        ;; Phase B: mined-move hyperedges (idempotent stable-hyperedge-id)
        (doseq [m res]
          (let [r (post-json "/hyperedge"
                             {:penholder PENHOLDER
                              :hx/type "code/v05/mined-move"
                              :hx/endpoints [(canon-have m) (want-node m)]
                              :props {:status "correlated"
                                      :mined-structural true
                                      :confidence (:score m)
                                      :delta-g (:delta-g m)
                                      :move-class (str (:move/class m))
                                      :note (:note m)
                                      :provenance "diffsub-mine/M-differentiable-substrate"}})]
            (if (#{200 201} (:status r)) (swap! hx-results update :ok inc)
                (do (swap! hx-results update :err inc)
                    (when (< (:err @hx-results) 4) (println "  HX ERR" (:status r) (subs (str (:body r)) 0 (min 200 (count (str (:body r)))))))))))
        (println "  hyperedges:" @hx-results)
        (when (pos? (:err @hx-results))
          (throw (ex-info "hyperedge writes failed" @hx-results)))
        (verify-layer! (when-not only (count res)))))))

(apply -main *command-line-args*)
