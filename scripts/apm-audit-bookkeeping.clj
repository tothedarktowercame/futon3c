#!/usr/bin/env bb
;; Read-only retained-state census. No role dispatch, database reads or writes.
(require '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[cheshire.core :as json])

(defn read-pinned [file]
  (let [text (slurp file)
        digest (.digest (java.security.MessageDigest/getInstance "SHA-256")
                        (.getBytes text "UTF-8"))]
    {:value (edn/read-string text)
     :sha256 (format "%064x" (java.math.BigInteger. 1 digest))}))

(let [[root first-frame last-frame] *command-line-args*
      lower (parse-long (or first-frame "200"))
      upper (parse-long (or last-frame "223"))
      queue (read-pinned (io/file root "queue-state.edn"))
      number-of #(some->> (:frame/id %) (re-matches #"f([0-9]+)") second parse-long)
      parks (filterv #(when-let [n (number-of %)] (<= lower n upper))
                     (:parked (:value queue)))
      frames (for [n (range lower (inc upper))
                   :let [dir (io/file root (str (.getName (io/file root)) "-f" n) "live")]]
               {:frame/id (str "f" n)
                :retained-phases
                (mapv (fn [file]
                        (let [{:keys [value sha256]} (read-pinned file)
                              prior (:last-valid-state value)]
                          (merge {:path (str file) :sha256 sha256}
                                 (select-keys value [:state/type :stage :error/code
                                                     :repair/kind :repair/attempts])
                                 (when (= :awaiting-apparatus-repair (:stage value))
                                   {:review-verdicts
                                    (frequencies (map :verdict
                                                      (get-in value [:persisted-review-result :reviews])))
                                    :predecessor-collection-present?
                                    (boolean (:terminal-collection prior))}))))
                      (sort-by str (filter #(and (.isFile %) (.endsWith (.getName %) ".edn"))
                                           (or (.listFiles dir) []))))})]
  (println
   (json/generate-string
    {:observed-at (str (java.time.Instant/now))
     :scope "Retained checkpoints and queue parks; not all historical transient errors. Files read individually, not an atomic campaign snapshot."
     :range [lower upper] :queue-sha256 (:sha256 queue)
     :active-frame (select-keys (get-in queue [:value :active :frame]) [:frame/id :problem/id])
     :park-count (count parks)
     :parks (mapv #(select-keys % [:frame/id :problem/id :state/type :phase
                                   :error/code :decision/status :promotion/state-path]) parks)
     :frames (vec frames)}
    {:pretty true})))
