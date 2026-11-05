(ns futon3c.file-change.shadow-report
  "Read-only health projection over shadow demands and WM loop probes."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon3c.file-change.click-probe :as click-probe]
            [futon3c.file-change.shadow-ledger :as ledger]))

(defn build-report
  "Build a bounded score report. No demands is a typed state, not a zero score."
  [{:keys [demand-path click-log-root ambiguity-ms]
    :or {ambiguity-ms 90000}}]
  (let [demands (ledger/read-records demand-path)
        probes (click-probe/read-loop-probes click-log-root)
        verdicts {:fresh (count (filter #(true? (:fresh? %)) probes))
                  :stale (count (filter #(false? (:fresh? %)) probes))
                  :unknown (count (filter #(nil? (:fresh? %)) probes))
                  :mint-failed (count (filter :mint-failed? probes))}
        first-demand-at (when (seq demands) (apply min (map :first-at-ms demands)))
        baseline (when first-demand-at
                   (last (filter #(<= (:at-ms %) first-demand-at) probes)))
        scoring-probes (if first-demand-at
                         (vec (concat (when baseline [baseline])
                                      (filter #(> (:at-ms %) first-demand-at) probes)))
                         [])
        last-probe-at (some-> probes last :at-ms)
        pending (if last-probe-at
                  (count (filter #(> (:first-at-ms %) last-probe-at) demands))
                  (count demands))
        basis {:population :wm-loop-clicks
               :ambiguity-ms ambiguity-ms
               :demand-count (count demands)
               :probe-count (count probes)
               :scoring-probe-count (count scoring-probes)
               :awaiting-probe pending
               :probe-verdicts verdicts}]
    (if (empty? demands)
      (assoc basis :status :no-demands)
      (merge basis {:status (if (pos? pending) :awaiting-probe :scored)}
             (ledger/score-probes demands scoring-probes
                                  {:ambiguity-ms ambiguity-ms})))))

(defn write-report!
  "Write REPORT as EDN to PATH and return it."
  [path report]
  (let [file (io/file path)]
    (when-let [parent (.getParentFile file)] (.mkdirs parent))
    (spit file (str (pr-str report) "\n")))
  report)

(defn read-report [path]
  (edn/read-string (slurp path)))
