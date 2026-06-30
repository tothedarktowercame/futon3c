#!/usr/bin/env bb
;; d4_feeder_b_load.bb — C-cascade-real D4 feeder-(b): land the 177 :mined-structural
;; (have→want) arrows from futon6/data/diffsub-moves-mined.edn into substrate-2 (:7071)
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
(def BASE "http://127.0.0.1:7071/api/alpha")
(def TOKEN (str/trim (slurp "/home/joe/code/futon3c/.admintoken")))
(def PENHOLDER "api")
(def DRAWBRIDGE "http://127.0.0.1:6768/eval")

(defn drawbridge [form]
  (let [r (http/post DRAWBRIDGE {:headers {"x-admin-token" TOKEN "Content-Type" "text/plain"}
                                 :body form :throw false})]
    (when (not= 200 (:status r)) (throw (ex-info "drawbridge failed" {:status (:status r) :body (:body r)})))
    (let [o (edn/read-string (:body r))]
      (if (:ok o) (:value o) (throw (ex-info "eval error" o))))))

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

;; --- resolve which have-nodes exist (canonical 708) ---
(def existing-canonical
  (set (drawbridge "(let [node (:node @futon3c.dev/!f1-sys) db (xtdb.api/db node)]
                      (->> (xtdb.api/q db '{:find [n] :where [[e :entity/name n] [e :entity/type :mission/doc]]})
                           (map first) (filter string?) vec))")))

(defn -main [& args]
  (let [write? (some #{"--write"} args)
        only (second (drop-while #(not= "--only" %) args))
        arrows (cond->> moves only (filter #(str/includes? (canon-have %) only)))
        {res true unres false} (group-by #(contains? existing-canonical (canon-have %)) arrows)]
    (println (format "mined arrows: %d  | have-resolves: %d  | honest-hole (have not a canonical node): %d"
                     (count arrows) (count res) (count unres)))
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
        (println "  hyperedges:" @hx-results)))))

(apply -main *command-line-args*)
