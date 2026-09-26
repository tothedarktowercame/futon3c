;; One backlog worker JVM: walk a shard of mission ids through
;; futon3c.watcher.scope-reingest/reingest-now! IN-PROCESS. Run from
;; scripts/mission-scope-backlog-jvm.sh. Why not Drawbridge: evals into the
;; serving JVM serialize (max 1 in-flight futon1b request across 4 shell
;; workers, measured 2026-08-25), so N shards need N JVMs.
(require (quote [futon3c.watcher.scope-reingest :as sr])
         (quote [cheshire.core :as json])
         (quote [clojure.java.io :as io])
         (quote [clojure.string :as str]))
(let [shard (System/getProperty "scope.shard")
      trees "/home/joe/code/futon6/data/mission-scope-trees"
      missions (->> (line-seq (io/reader shard))
                    (map #(first (str/split % #"\t")))
                    (remove str/blank?)
                    vec)
      n (count missions)]
  (println (format "[backlog] start %s missions=%d shard=%s" (java.time.Instant/now) n shard))
  (doseq [[i mission] (map-indexed vector missions)]
    (let [tree (io/file trees (str mission ".json"))
          ;; 17 trees carry paths relative to the code root (futon7/holes/…)
          doc (some-> (when (.exists tree) (get (json/parse-string (slurp tree)) "path"))
                      (as-> p (if (str/starts-with? p "/") p (str "/home/joe/code/" p))))
          t0 (System/currentTimeMillis)]
      (if (or (nil? doc) (not (.exists (io/file doc))))
        (println (format "[backlog] %d/%d SKIP %s (doc missing: %s)" (inc i) n mission doc))
        (let [r (try (sr/reingest-now! doc)
                     (catch Throwable t {:status :failed :error (.getMessage t)}))
              ok? (= :ok (:status r))
              secs (quot (- (System/currentTimeMillis) t0) 1000)]
          (println (format "[backlog] %d/%d %s %s (%ds)%s" (inc i) n (if ok? "ok  " "FAIL") mission secs
                           (if ok? "" (str " " (pr-str (dissoc r :status))))))))
      (flush)))
  (println (format "[backlog] done %s" (java.time.Instant/now)))
  (shutdown-agents)
  (System/exit 0))
