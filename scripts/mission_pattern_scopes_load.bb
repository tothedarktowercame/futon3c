#!/usr/bin/env bb
;; mission_pattern_scopes_load.bb — C-cascade-real §7 DISSOLUTION, Checklist B:
;; land the historical mission→pattern crosslinks (the "cited patterns" back-link
;; layer, NOT PSR/PUR) from futon6/data/mission-pattern-scopes.edn into substrate-2
;; (:7071) as `cascade/mission-pattern` hyperedges on CANONICAL nodes.
;;
;;   source:    {:missions [{:mission "M-<id>" :applied [<bare-pattern> ...]
;;                           :try-candidates [{:pattern <bare> :cos <n>} ...]} ...]}
;;   mission:   M-<id>  →  <repo>-d/mission/<id>   (resolve-against-live-canonical, stem match)
;;   pattern:   <bare>  →  <ns>/<name> pattern/library node (resolve; UNIQUE only)
;;   hx/type:   cascade/mission-pattern   props {:relation "applied"|"candidate" :cos ...}
;;
;; Both endpoints must ALREADY exist (resolve, never mint islands) — non-resolving
;; missions/patterns are honest holes, reported and skipped. Honestly tagged
;; :relation, :source (a mined citation, NOT a proof relation — T-A4 clean).
;;
;;   bb mission_pattern_scopes_load.bb                 # DRY RUN: resolve + honest-hole report
;;   bb mission_pattern_scopes_load.bb --write         # write :applied edges (penholder "api")
;;   bb mission_pattern_scopes_load.bb --write --candidates   # also land :try-candidates
(require '[babashka.http-client :as http]
         '[cheshire.core :as json]
         '[clojure.edn :as edn]
         '[clojure.string :as str])

(def SRC "/home/joe/code/futon6/data/mission-pattern-scopes.edn")
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

;; --- live canonical sets (resolve-against-live-canonical, never mint islands) ---
(def canon
  (drawbridge "(let [node (:node @futon3c.dev/!f1-sys) db (xtdb.api/db node)]
     {:missions (->> (xtdb.api/q db '{:find [n] :where [[e :entity/name n] [e :entity/type :mission/doc]]})
                     (map first) (filter string?) vec)
      :patterns (->> (xtdb.api/q db '{:find [n] :where [[e :entity/name n] [e :entity/type :pattern/library]]})
                     (map first) (filter string?) vec)})"))

(def canon-missions (set (:missions canon)))
(def bare->canon    ;; bare pattern name -> #{canonical <ns>/<name> ...}
  (reduce (fn [m nm] (update m (last (str/split nm #"/")) (fnil conj #{}) nm)) {} (:patterns canon)))

(defn resolve-mission [mid]
  (let [s (str/lower-case (subs mid 2))]
    (first (filter #(str/ends-with? % (str "/mission/" s)) canon-missions))))
(defn resolve-pattern [bare]
  (let [c (bare->canon bare)] (when (= 1 (count c)) (first c))))

(defn edges-for [missions kind]  ;; kind = :applied | :candidate
  (for [m missions
        :let [mc (resolve-mission (:mission m))]
        :when mc
        p (if (= kind :applied)
            (map (fn [b] {:bare b}) (:applied m))
            (map (fn [c] {:bare (:pattern c) :cos (:cos c)}) (:try-candidates m)))
        :let [pc (resolve-pattern (:bare p))]
        :when pc]
    {:mission mc :pattern pc :relation (name kind) :cos (:cos p)}))

(defn -main [& args]
  (let [write?      (some #{"--write"} args)
        candidates? (some #{"--candidates"} args)
        data     (edn/read-string (slurp SRC))
        missions (:missions data)
        applied  (edges-for missions :applied)
        cands    (edges-for missions :candidate)
        to-write (concat applied (when candidates? cands))
        unres-m  (remove #(resolve-mission (:mission %)) missions)
        all-bare (distinct (mapcat #(concat (:applied %) (map :pattern (:try-candidates %))) missions))
        unres-p  (remove resolve-pattern all-bare)]
    (println (format "missions: %d  | resolved: %d  | honest-hole (mission): %d"
                     (count missions) (- (count missions) (count unres-m)) (count unres-m)))
    (println (format "distinct bare patterns: %d  | resolved-unique: %d  | honest-hole (pattern): %d"
                     (count all-bare) (- (count all-bare) (count unres-p)) (count unres-p)))
    (println (format "edges — :applied %d  | :try-candidates %d  | WILL WRITE %d"
                     (count applied) (count cands) (count to-write)))
    (when (seq unres-m)
      (println "HONEST HOLES (mission non-resolving):")
      (doseq [m (take 8 unres-m)] (println "  " (:mission m))))
    (when (seq unres-p)
      (println "HONEST HOLES (pattern non-resolving, first 12 — newer patterns not yet in pattern/library):")
      (doseq [p (take 12 unres-p)] (println "  " p)))
    (when-not write?
      (println "\n-- DRY RUN — sample :applied edges that WOULD be written --")
      (doseq [e (take 5 applied)]
        (println (format "  HX cascade/mission-pattern  [%s , %s]  relation=%s" (:mission e) (:pattern e) (:relation e)))))
    (when write?
      (println (format "\n-- WRITING %d edges (penholder \"api\", %s) --"
                       (count to-write) (if candidates? "applied+candidates" "applied only")))
      (let [res (atom {:ok 0 :err 0})]
        (doseq [e to-write]
          (let [r (post-json "/hyperedge"
                             {:penholder PENHOLDER
                              :hx/type "cascade/mission-pattern"
                              :hx/endpoints [(:mission e) (:pattern e)]
                              :props (cond-> {:relation (:relation e)
                                              :source "mission-pattern-scopes"
                                              :provenance "mission-pattern-scopes/M-differentiable-substrate"}
                                       (:cos e) (assoc :cos (:cos e)))})]
            (if (#{200 201} (:status r)) (swap! res update :ok inc)
                (do (swap! res update :err inc)
                    (when (< (:err @res) 4) (println "  HX ERR" (:status r) (subs (str (:body r)) 0 (min 200 (count (str (:body r)))))))))))
        (println "  hyperedges:" @res)))))

(apply -main *command-line-args*)
