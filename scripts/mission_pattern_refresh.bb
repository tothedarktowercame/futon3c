#!/usr/bin/env bb
;; mission_pattern_refresh.bb — C-cascade-real §7 Checklist B: keep the mission→pattern
;; crosslinks CURRENT by re-running the LIVE magnet (cascade_construct) for a mission,
;; instead of relying only on the Jun-8 historical snapshot (mission-pattern-scopes.edn).
;; This is the "reconstruct + keep up to date / model pattern scopes on demand" path.
;;
;;   psi (the mission's live context) → cascade_serve.py → shown[{pattern,rel,mc}]
;;     → resolve-against-live-canonical (mission spine × pattern/library)
;;     → land cascade/mission-pattern relation "on-demand" {:cos :mc :refreshed-at}
;;   RETRACT-then-PUT: the mission's prior "on-demand" edges are end-valid-timed first,
;;   so the on-demand layer always reflects the LATEST magnet run (no stale linger).
;;   :applied / :candidate (historical) edges are untouched — this only refreshes the
;;   on-demand grain, so the historical citation record is preserved alongside.
;;
;;   bb mission_pattern_refresh.bb <mission-stem> --psi "<text>"          # DRY RUN
;;   bb mission_pattern_refresh.bb <mission-stem> --psi "<text>" --write  # land
;;   (optional: --budget N (default 8)  --epsilon E (default 0.05))
(require '[babashka.http-client :as http]
         '[babashka.process :as proc]
         '[cheshire.core :as json]
         '[clojure.edn :as edn]
         '[clojure.string :as str])

(def BASE "http://127.0.0.1:7071/api/alpha")
(def TOKEN (str/trim (slurp "/home/joe/code/futon3c/.admintoken")))
(def PENHOLDER "api")
(def DRAWBRIDGE "http://127.0.0.1:6768/eval")
(def VENV "/home/joe/code/futon3a/.venv/bin/python3")
(def SERVE "/home/joe/code/futon3a/holes/labs/M-memes-arrows/cascade_serve.py")

(defn drawbridge [form]
  (let [r (http/post DRAWBRIDGE {:headers {"x-admin-token" TOKEN "Content-Type" "text/plain"}
                                 :body form :throw false})]
    (when (not= 200 (:status r)) (throw (ex-info "drawbridge failed" {:status (:status r) :body (:body r)})))
    (let [o (edn/read-string (:body r))]
      (if (:ok o) (:value o) (throw (ex-info "eval error" o))))))

(defn post-hx! [payload]
  (http/post (str BASE "/hyperedge")
             {:headers {"Content-Type" "application/json" "x-penholder" PENHOLDER}
              :body (json/generate-string payload) :throw false}))

(defn get-edges [hx-type]
  (let [r (http/get (str BASE "/hyperedges?type=" hx-type)
                    {:headers {"Accept" "application/edn"} :throw false})]
    (or (when (= 200 (:status r)) (:hyperedges (edn/read-string (:body r)))) [])))

(def canon
  (drawbridge "(let [node (:node @futon3c.dev/!f1-sys) db (xtdb.api/db node)]
     {:missions (->> (xtdb.api/q db '{:find [n] :where [[e :entity/name n] [e :entity/type :mission/doc]]})
                     (map first) (filter string?) vec)
      :patterns (->> (xtdb.api/q db '{:find [n] :where [[e :entity/name n] [e :entity/type :pattern/library]]})
                     (map first) (filter string?) set)})"))
(def canon-missions (set (:missions canon)))
(def canon-patterns (:patterns canon))

(defn resolve-mission [stem]
  (let [s (str/lower-case stem)]
    (first (filter #(str/ends-with? % (str "/mission/" s)) canon-missions))))

(defn run-magnet [psi budget epsilon]
  (let [{:keys [out exit]} (proc/sh {:out :string :err :string} VENV SERVE psi (str budget) (str epsilon))]
    (when (not= 0 exit) (throw (ex-info "cascade_serve failed" {:exit exit})))
    ;; stdout is the json.dumps line (transformers noise goes to stderr)
    (let [line (->> (str/split-lines out) (filter #(str/starts-with? (str/trim %) "{\"psi\"")) last)]
      (json/parse-string (or line (last (remove str/blank? (str/split-lines out)))) true))))

(defn -main [& args]
  (let [stem   (first (remove #(str/starts-with? % "--") args))
        write? (some #{"--write"} args)
        psi    (second (drop-while #(not= "--psi" %) args))
        budget (or (some-> (second (drop-while #(not= "--budget" %) args)) parse-long) 8)
        eps    (or (second (drop-while #(not= "--epsilon" %) args)) "0.05")
        mc     (resolve-mission stem)]
    (when-not stem (println "usage: mission_pattern_refresh.bb <mission-stem> --psi \"<text>\" [--write]") (System/exit 1))
    (when-not psi  (println "ERROR: --psi \"<mission context text>\" is required (the on-demand caller supplies live context).") (System/exit 1))
    (when-not mc   (println (format "HONEST HOLE: mission stem '%s' does not resolve to a canonical mission node." stem)) (System/exit 1))
    (println (format "mission: %s → %s" stem mc))
    (let [res    (run-magnet psi budget eps)
          shown  (:shown res)
          edges  (for [s shown :let [p (:pattern s)] :when (canon-patterns p)]
                   {:pattern p :cos (:rel s) :mc (:mc s)})
          holes  (remove #(canon-patterns (:pattern %)) shown)
          now    (str (java.time.Instant/now))
          prior  (->> (get-edges "cascade%2Fmission-pattern")
                      (filter (fn [e] (and (some #{mc} (map str (:hx/endpoints e)))
                                           (= "on-demand" (get-in e [:hx/props :relation]))))))]
      (println (format "magnet: size=%s F=%s | shown=%d resolved=%d honest-holes(pattern)=%d"
                       (:size res) (:F-free-energy res) (count shown) (count edges) (count holes)))
      (println (format "prior on-demand edges for this mission (would retract): %d" (count prior)))
      (doseq [e edges] (println (format "  %s  rel=%.3f mc=%.3f" (:pattern e) (:cos e) (:mc e))))
      (when (seq holes) (println "  honest holes (not in pattern/library):" (str/join ", " (map :pattern holes))))
      (if-not write?
        (println "\n-- DRY RUN (no writes). Re-run with --write to refresh the on-demand layer. --")
        (let [r (atom {:retract 0 :put 0 :err 0})]
          ;; retract prior on-demand edges (end-valid-time) so the layer is fresh
          (doseq [e prior]
            (let [resp (post-hx! {:hx/type "cascade/mission-pattern"
                                  :hx/endpoints (vec (map str (:hx/endpoints e)))
                                  :hx/op "retract"})]
              (if (#{200 201} (:status resp)) (swap! r update :retract inc) (swap! r update :err inc))))
          ;; put fresh on-demand edges
          (doseq [e edges]
            (let [resp (post-hx! {:hx/type "cascade/mission-pattern"
                                  :hx/endpoints [mc (:pattern e)]
                                  :hx/props {:relation "on-demand" :cos (:cos e) :mc (:mc e)
                                             :refreshed-at now :source "cascade_construct-on-demand"}})]
              (if (#{200 201} (:status resp)) (swap! r update :put inc) (swap! r update :err inc))))
          (println "\n-- REFRESHED on-demand layer:" @r "--"))))))

(apply -main *command-line-args*)
