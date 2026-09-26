(ns futon3c.apm.cascade-dry-run
  "Reproducible real-run entry point for memory-cascade expansion."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [futon3c.apm.conductor :as conductor]
            [futon3c.substrate.client :as substrate])
  (:import [java.time Instant]))

(defn snapshot-memory-ids [snapshot]
  (mapv :memory-id (:snapshot/memories snapshot)))

(defn cascade-summary [result]
  (let [expanded (remove #(= :leaf (get-in % [1 :route])) (:routes result))
        why (filter #(= :why-hop (get-in % [1 :route])) expanded)
        surfaces (vals (:pattern-surfaces result))]
    {:expanded-available (:expanded-available result)
     :expanded-count (:expanded-count result)
     :truncated? (:truncated? result)
     :seed-pattern-count (count (:seed-patterns result))
     :route-histogram (frequencies (map #(get-in % [1 :route]) expanded))
     :hop-distribution (frequencies (map #(get-in % [1 :hops]) expanded))
     :route-hop-distribution
     (frequencies (map (fn [[_ route]] [(:route route) (:hops route)]) expanded))
     :missing-dependency-why-count
     (count (filter #(= "math-strategy/missing-dependency-protocol"
                        (get-in % [1 :pattern])) why))
     :distinct-why-reachable-patterns
     (count (distinct (map #(get-in % [1 :pattern]) why)))
     :pattern-surface-count (count (:pattern-surfaces result))
     :nil-or-empty-pattern-surface?
     (boolean (some #(or (nil? %) (and (coll? %) (empty? %))) surfaces))}))

(defn- git-sha []
  (let [process (.start (ProcessBuilder. ["git" "rev-parse" "HEAD"]))
        output (slurp (.getInputStream process))]
    (when-not (zero? (.waitFor process))
      (throw (ex-info "git rev-parse HEAD failed" {})))
    (.trim output)))

(defn write-real-run!
  ([snapshot-path cap out-path]
   (write-real-run! snapshot-path cap out-path nil))
  ([snapshot-path cap out-path routes]
   (let [snapshot (edn/read-string (slurp snapshot-path))
         seeds (snapshot-memory-ids snapshot)
         substrate-url (substrate/configured-url)
         sha (git-sha)
         command (or (System/getenv "APM_CASCADE_DRY_RUN_COMMAND")
                     (str "futon3c.apm.cascade-dry-run " snapshot-path " " cap
                          " " out-path))
         started-ns (System/nanoTime)
         result (conductor/expand-memory-cascade
                 seeds
                 (cond-> (assoc (#'conductor/live-cascade-readers {}) :cap cap)
                   (some? routes) (assoc :routes routes)))
         wall-clock-ms (quot (- (System/nanoTime) started-ns) 1000000)
         timestamp (Instant/now)]
     (io/make-parents out-path)
     (with-open [writer (io/writer out-path)]
       (.write writer (str ";; REAL RUN (not counterfactual)\n"
                           ";; futon3c git sha: " sha "\n"
                           ";; substrate URL: " substrate-url "\n"
                           ";; ISO timestamp: " timestamp "\n"
                           ";; snapshot path: " snapshot-path "\n"
                           ";; seed count: " (count seeds) "\n"
                           ";; exact command: " command "\n"))
       (binding [*out* writer] (pprint/pprint result)))
     {:out-path out-path
      :wall-clock-ms wall-clock-ms
      :window-overflow? false
      :summary (cascade-summary result)})))

(defn -main [& args]
  (when-not (contains? #{3 4} (count args))
    (throw (ex-info
            "usage: cascade-dry-run SNAPSHOT-PATH CAP OUT-PATH [ROUTE,...]"
            {:args args})))
  (let [[snapshot-path cap-text out-path routes-text] args
        cap (parse-long cap-text)
        routes (when routes-text
                 (->> (str/split routes-text #",")
                      (remove str/blank?)
                      (map keyword)
                      set))]
    (when-not (and cap (pos? cap))
      (throw (ex-info "cap must be a positive integer" {:cap cap-text})))
    (prn (write-real-run! snapshot-path cap out-path routes))))
