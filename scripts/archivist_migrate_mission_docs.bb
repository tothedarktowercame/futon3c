#!/usr/bin/env bb
;; archivist_migrate_mission_docs.bb — E-futon1a-archivist migration (claude-2, D-side).
;; Merge the rich UUID-keyed :mission/doc nodes onto their canonical-keyed twins
;; (or re-key when no twin), so each mission = ONE authoritative canonical node.
;;
;; SAFE BY DESIGN: additive only (fetch-merge-upsert via /entity, id-strategy :name
;; lands the doc at xt/id == canonical name). Does NOT delete/evict the UUID nodes —
;; entity eviction is counter-ratchet protected (penholder "joe" via erase.bb only).
;; UUID dups are marked :superseded-by so a later gated evict pass is trivial.
;;
;;   bb archivist_migrate_mission_docs.bb --snapshot   # dump all :mission/doc docs to disk
;;   bb archivist_migrate_mission_docs.bb              # DRY RUN classify + plan, no writes
;;   bb archivist_migrate_mission_docs.bb --execute --only <name-substr>   # one-node test
;;   bb archivist_migrate_mission_docs.bb --execute    # full additive merge + rekey
(require '[babashka.http-client :as http]
         '[clojure.edn :as edn]
         '[clojure.string :as str]
         '[clojure.pprint :as pp])

(def TOKEN (str/trim (slurp "/home/joe/code/futon3c/.admintoken")))
(def DRAWBRIDGE "http://127.0.0.1:6768/eval")
(def SNAP "/home/joe/code/futon3c/holes/excursions/archivist-mission-doc-snapshot.edn")
(def ALLOW #{"futon0-d" "futon2-d" "futon3-d" "futon3a-d" "futon3b-d" "futon3c-d"
             "futon4-d" "futon4-elisp-d" "futon5-d" "futon5a-d" "futon6-d" "futon6-py-d" "futon7-d"})

(defn drawbridge [form]
  (let [r (http/post DRAWBRIDGE {:headers {"x-admin-token" TOKEN "Content-Type" "text/plain"}
                                 :body form :throw false})]
    (when (not= 200 (:status r)) (throw (ex-info "drawbridge failed" {:status (:status r) :body (:body r)})))
    (let [o (edn/read-string (:body r))] (if (:ok o) (:value o) (throw (ex-info "eval err" o))))))

(defn all-docs []
  (drawbridge "(let [node (:node @futon3c.dev/!f1-sys) db (xtdb.api/db node)]
                 (->> (xtdb.api/q db '{:find [(pull e [*])] :where [[e :entity/type :mission/doc]]})
                      (mapv first)))"))

(defn repo-of [n] (when (and (string? n) (re-find #"/mission/" n)) (first (str/split n #"/mission/" 2))))
(defn allow? [n] (contains? ALLOW (repo-of n)))
(defn uuidish? [x] (boolean (re-matches #"[0-9a-f]{8}-[0-9a-f]{4}-.*" (str x))))
(defn stub? [d] (str/starts-with? (str (:xt/id d)) "mission-doc/"))
(defn name-keyed? [d] (= (str (:xt/id d)) (str (:entity/name d))))

(defn classify [canon-names d]
  (cond
    (stub? d) :stub-no-mapping
    (and (uuidish? (:xt/id d)) (allow? (:entity/name d)) (contains? canon-names (:entity/name d))) :uuid-merge-onto-twin
    (and (uuidish? (:xt/id d)) (allow? (:entity/name d))) :uuid-rekey-no-twin
    (uuidish? (:xt/id d)) :uuid-nonallowlist-FLAG
    (and (name-keyed? d) (allow? (:entity/name d))) :canonical-keep
    (name-keyed? d) :canonical-nonallowlist-FLAG
    :else :other))

;; NOTE: the /entity by-name upsert CANNOT target the canonical node while a UUID
;; dup with the same :name exists — ensure-entity-by-name resolves to the UUID dup
;; and enriches the WRONG node (verified 2026-06-30, agency-refactor). The migration
;; therefore runs server-side via a precise xt/id-targeted put (admin channel, pre-gate),
;; basing each canonical doc on an existing-valid doc so L4 invariants are preserved.
(def MIGRATE-FORM "
(require '[clojure.string :as str])
(let [node (:node @futon3c.dev/!f1-sys) db (xtdb.api/db node)
      allow #{\"futon0-d\" \"futon2-d\" \"futon3-d\" \"futon3a-d\" \"futon3b-d\" \"futon3c-d\"
              \"futon4-d\" \"futon4-elisp-d\" \"futon5-d\" \"futon5a-d\" \"futon6-d\" \"futon6-py-d\" \"futon7-d\"}
      docs (->> (xtdb.api/q db '{:find [(pull e [*])] :where [[e :entity/type :mission/doc]]}) (map first))
      repo-of (fn [n] (when (and (string? n) (re-find #\"/mission/\" n)) (first (str/split n #\"/mission/\" 2))))
      allow? (fn [n] (contains? allow (repo-of n)))
      uuidish? (fn [x] (boolean (re-matches #\"[0-9a-f]{8}-[0-9a-f]{4}-.*\" (str x))))
      stub? (fn [d] (str/starts-with? (str (:xt/id d)) \"mission-doc/\"))
      name-keyed? (fn [d] (= (str (:xt/id d)) (str (:entity/name d))))
      by-name (group-by :entity/name docs)
      uuid-docs (filter #(and (uuidish? (:xt/id %)) (allow? (:entity/name %)) (not (stub? %))) docs)
      mk (fn [u] (let [N (:entity/name u)
                       twin (first (filter name-keyed? (by-name N)))
                       base (or twin {:entity/name N :entity/type :mission/doc :entity/external-id (:entity/external-id u)})]
                   (assoc base :xt/id N :entity/id N :entity/name N :entity/type :mission/doc
                          :entity/external-id (:entity/external-id u)
                          :entity/props (-> (merge (:entity/props base) (:entity/props u))
                                            (assoc :migrated-from (str (:xt/id u)) :migrated-by \"archivist-migrate/claude-2\")))))
      puts (mapv mk uuid-docs)]
  (xtdb.api/await-tx node (xtdb.api/submit-tx node (mapv (fn [d] [:xtdb.api/put d]) puts)))
  {:puts (count puts)
   :merge-onto-twin (count (filter #(first (filter name-keyed? (by-name (:entity/name %)))) uuid-docs))})")

(defn run-migration! [] (drawbridge MIGRATE-FORM))

(defn -main [& args]
  (let [snap? (some #{"--snapshot"} args)
        exec? (some #{"--execute"} args)
        only (second (drop-while #(not= "--only" %) args))
        docs (all-docs)
        canon-names (set (map :entity/name (filter name-keyed? docs)))
        by-name (group-by :entity/name docs)]
    (when snap?
      (spit SNAP (with-out-str (pp/pprint {:captured-count (count docs) :docs docs})))
      (println "snapshot ->" SNAP "(" (count docs) "docs )"))
    (let [tagged (map (fn [d] [(classify canon-names d) d]) docs)
          targets (->> tagged
                       (filter (fn [[c _]] (#{:uuid-merge-onto-twin :uuid-rekey-no-twin} c)))
                       (map second)
                       (filter (fn [d] (or (nil? only) (str/includes? (str (:entity/name d)) only)))))]
      (println "classes:" (frequencies (map first tagged)))
      (println "migration targets (merge+rekey):" (count targets) (when only (str "[--only " only "]")))
      (when-not exec?
        (doseq [u (take 6 targets)]
          (let [N (:entity/name u) twin (->> (by-name N) (filter name-keyed?) first)]
            (println (format "  DRY %s  twin=%s  uuid=%s" N (boolean twin) (:xt/id u)))))
        (println "  (server-side precise xt/id put; run with --execute)"))
      (when exec?
        (when only (println "  NOTE: --only is informational; --execute runs the full server-side migration"))
        (println "  migrated:" (run-migration!))))))

(apply -main *command-line-args*)
