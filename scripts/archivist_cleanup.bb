#!/usr/bin/env bb
;; archivist_cleanup.bb — E-futon1a-archivist: evict the migrated :mission/doc
;; UUID tombstones, as penholder "joe", via the gated run-erase! pipeline.
;;
;; This is the destructive companion to archivist_migrate_mission_docs.bb. The
;; migration left each mission's rich data on a canonical <repo>-d/mission/<id>
;; node carrying :migrated-from = the old UUID node's xt/id. THIS tool evicts
;; those now-redundant UUID tombstones — but ONLY the ones it can PROVE are
;; superseded: a UUID node is eligible iff a surviving canonical node carries
;; :migrated-from == that UUID id. Anything else is refused (no data loss).
;;
;; Evict = XTDB evict (removes the doc AND its whole bitemporal history). It is
;; gated to penholder "joe" and rides the same L4->L3->L0 pipeline as erase.bb,
;; emitting an :erasure-event audit record.
;;
;;   bb scripts/archivist_cleanup.bb                       # DRY RUN: list eligible tombstones
;;   bb scripts/archivist_cleanup.bb --reason "..." --execute   # evict (penholder joe)
;;
;; DEFAULT IS DRY-RUN. --execute requires --reason (recorded in the audit).
(require '[babashka.http-client :as http]
         '[clojure.edn :as edn]
         '[clojure.string :as str])

(def TOKEN (str/trim (slurp "/home/joe/code/futon3c/.admintoken")))
(def DRAWBRIDGE "http://127.0.0.1:6768/eval")
(def PENHOLDER "joe")   ;; the only penholder run-erase! accepts

(defn eval! [form]
  (let [resp (http/post DRAWBRIDGE {:headers {"x-admin-token" TOKEN "Content-Type" "text/plain"}
                                    :body form :throw false})]
    (when (not= 200 (:status resp))
      (binding [*out* *err*] (println "Drawbridge eval failed:" (:status resp) (:body resp))) (System/exit 1))
    (let [out (edn/read-string (:body resp))]
      (if (:ok out) (:value out)
          (do (binding [*out* *err*] (println "eval error:" (pr-str out))) (System/exit 2))))))

;; Compute, server-side, the UUID tombstones PROVABLY superseded by a surviving
;; canonical node. Returns [{:eid :name :superseded-by} ...] — only safe targets.
(def ELIGIBLE-FORM "
(require '[clojure.string :as str])
(let [db (xtdb.api/db (:node @futon3c.dev/!f1-sys))
      docs (->> (xtdb.api/q db '{:find [(pull e [:xt/id :entity/name :entity/props])]
                                  :where [[e :entity/type :mission/doc]]}) (map first))
      uuidish? (fn [x] (boolean (re-matches #\"[0-9a-f]{8}-[0-9a-f]{4}-.*\" (str x))))
      name-keyed? (fn [d] (= (str (:xt/id d)) (str (:entity/name d))))
      ;; canonical successors that record where they were migrated from
      mig (->> docs (filter name-keyed?)
               (keep (fn [c] (when-let [from (get-in c [:entity/props :migrated-from])]
                               [(str from) (:entity/name c)])))
               (into {}))
      uuid-ids (set (map #(str (:xt/id %)) (filter #(uuidish? (:xt/id %)) docs)))]
  ;; eligible = UUID node that still exists AND a canonical node points back at it
  (->> mig
       (filter (fn [[from _]] (contains? uuid-ids from)))
       (mapv (fn [[from cname]] {:eid from :superseded-by cname}))))")

(defn parse-args [args]
  (loop [m {:execute? false} [a & more] args]
    (cond (nil? a) m
          (= a "--execute") (recur (assoc m :execute? true) more)
          (= a "--reason")  (recur (assoc m :reason (first more)) (rest more))
          :else (recur m more))))

(defn execute! [eids reason]
  (eval! (str "(futon1a.core.pipeline/run-erase!"
              " {:store (:store @futon3c.dev/!f1-sys) :penholder " (pr-str PENHOLDER)
              " :eids " (pr-str (vec eids)) " :reason " (pr-str reason) "})")))

(defn -main [& args]
  (let [{:keys [execute? reason]} (parse-args args)
        eligible (eval! ELIGIBLE-FORM)
        eids (mapv :eid eligible)]
    (println (format "Eligible UUID tombstones (provably superseded): %d" (count eligible)))
    (doseq [{:keys [eid superseded-by]} (take 12 eligible)]
      (println (format "  %-40s  ->  %s" eid superseded-by)))
    (when (> (count eligible) 12) (println (format "  ... (%d more)" (- (count eligible) 12))))
    (if-not execute?
      (println "\nDRY-RUN (default). Re-run with --reason \"...\" --execute to evict as penholder \"joe\".")
      (do
        (when (str/blank? (str reason))
          (binding [*out* *err*] (println "\n--execute requires --reason (audit record).")) (System/exit 3))
        (when (empty? eids)
          (println "Nothing eligible to evict.") (System/exit 0))
        (println (format "\nEVICTING %d tombstones as penholder \"%s\" — reason: %s" (count eids) PENHOLDER reason))
        (println "RESULT:" (pr-str (execute! eids reason)))))))

(apply -main *command-line-args*)
