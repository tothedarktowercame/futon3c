#!/usr/bin/env bb
;; held_work_ledger_land.bb — E-held-work-ledger: land the harvested :held/* ledger into
;; substrate-2 (:7071) as `held/item` entities + `held/on-mission` hyperedges on CANONICAL
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

(def items (:items (edn/read-string (slurp LEDGER))))

(defn held-id [i]
  (let [k (:held/id i)] (str "held/item/" (namespace k) "/" (name k))))

(defn flat-props [i]
  (into {} (for [[k v] (select-keys i [:held/reason :held/owner :held/status :held/kind
                                       :held/wake-trigger :held/evidence-condition
                                       :held/review-by :held/re-entry :held/raised-at])
                 :when (some? v)]
             [k (if (keyword? v) (name v) v)])))

(defn resolvable [existing i] (filter existing (:held/missions i)))

(def existing-canonical
  (set (drawbridge "(let [node (:node @futon3c.dev/!f1-sys) db (xtdb.api/db node)]
                      (->> (xtdb.api/q db '{:find [n] :where [[e :entity/name n] [e :entity/type :mission/doc]]})
                           (map first) (filter string?) vec))")))

(defn -main [& args]
  (let [write?     (some #{"--write"} args)
        with-edges (filter #(seq (resolvable existing-canonical %)) items)
        edge-count (reduce + (map #(count (resolvable existing-canonical %)) items))
        detached   (remove #(seq (resolvable existing-canonical %)) items)]
    (println (format "held items -> entities: %d" (count items)))
    (println (format "mission-resolving: %d items -> %d held/on-mission edges | detached (no resolvable mission): %d"
                     (count with-edges) edge-count (count detached)))
    (println "by registry:" (frequencies (map #(get-in % [:held/source :registry]) items)))
    (when-not write?
      (println "\n-- DRY RUN — sample of what WOULD land --")
      (doseq [i (take 4 with-edges)]
        (println (format "  ENTITY %s :held/item" (held-id i)))
        (doseq [m (resolvable existing-canonical i)]
          (println (format "    HX held/on-mission [%s , %s]" (held-id i) m)))))
    (when write?
      (println "\n-- WRITING (penholder \"api\", idempotent) --")
      (let [er (atom {:ok 0 :err 0}) hr (atom {:ok 0 :err 0})]
        ;; Phase A: held/item entities (id == name, canonical single scheme)
        (doseq [i items]
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
        (doseq [i items, m (resolvable existing-canonical i)]
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
        (println "  hyperedges:" @hr)))))

(apply -main *command-line-args*)
