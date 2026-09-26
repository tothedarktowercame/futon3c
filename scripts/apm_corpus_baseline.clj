(ns apm-corpus-baseline
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [futon3c.apm.toolchain-port :as toolchain])
  (:import [java.time Instant]
           [java.util.concurrent Callable Executors TimeUnit]))

(defn main [checkout output]
  (let [root (io/file checkout "problems")
        paths (->> (file-seq root)
                   (filter #(and (.isFile %)
                                 (= "Main.lean" (.getName %))
                                 (= "lean" (some-> % .getParentFile .getName))))
                   (map #(str (.relativize (.toPath (io/file checkout))
                                           (.toPath %))))
                   sort vec)
        pool (Executors/newFixedThreadPool 4)
        tasks (mapv (fn [path]
                      (.submit pool
                               ^Callable
                               (fn []
                                 [path (toolchain/elaborate
                                        {:workspace checkout} path)])))
                    paths)
        rows (->> tasks (mapv #(.get %)) (into (sorted-map)))
        _ (.shutdown pool)
        _ (.awaitTermination pool 1 TimeUnit/MINUTES)
        kinds (frequencies (mapcat (fn [[_ row]]
                                     (map :kind (:other-warnings row))) rows))
        revision (:out (shell/sh "git" "-C" checkout "rev-parse" "HEAD"))
        receipt {:baseline/type :apm-corpus-toolchain-baseline
                 :corpus/revision (.trim revision)
                 :generated-at (str (Instant/now))
                 :coverage {:expected (count paths) :observed (count rows)}
                 :diagnostic-kind-distribution kinds
                 :blocking-warning-kinds toolchain/blocking-warning-kinds
                 :acceptance-rule
                 {:exit 0 :errors 0 :sorry-warnings :positive
                  :blocking-warnings 0}
                 :observations rows}]
    (io/make-parents output)
    (spit output (str (pr-str receipt) "\n"))
    (println (pr-str (dissoc receipt :observations)))))

(apply main *command-line-args*)
(shutdown-agents)
