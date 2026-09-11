(require '[clojure.edn :as edn] '[clojure.java.io :as io] '[clojure.string :as str])
(import '[java.security MessageDigest])
(def root "data/apm-campaigns/jit-all-open-v3")
(defn forms [f]
  (with-open [r (java.io.PushbackReader. (io/reader f))]
    (loop [xs []] (let [v (edn/read {:eof ::eof} r)]
                   (if (= v ::eof) xs (recur (conj xs v)))))))
(defn digest [f]
 (format "%064x" (BigInteger. 1 (.digest (MessageDigest/getInstance "SHA-256")
                                      (java.nio.file.Files/readAllBytes (.toPath (io/file f)))))))
(def fault-keys #{:error/code :validation/findings :terminal-repair/findings :findings :exception/message :terminal-code})
(def skip-keys #{:body :instructions :prompt :packet :failure-account :report :source :text})
(defn faults [x path]
 (cond
  (map? x) (into [] (mapcat (fn [[k v]]
    (concat (when (and (fault-keys k) (some? v) (not= [] v) (not= #{} v))
               [{:path (conj path k) :value v}])
            (when-not (skip-keys k) (faults v (conj path k))))) x))
  (sequential? x) (into [] (mapcat (fn [[i v]] (faults v (conj path i))) (map-indexed vector x)))
  :else []))
(let [q (first (forms (str root "/queue-state.edn")))
      result
      (mapv (fn [n]
       (let [id (str "f" n) dir (str root "/jit-all-open-v3-" id)
             files (concat [(io/file dir "ledger.edn") (io/file dir "problem-transitions.edn")]
                           (filter #(and (.isFile %) (str/ends-with? (.getName %) ".edn"))
                                   (file-seq (io/file dir "live"))))
             reads (mapv (fn [f]
                       (try (let [xs (forms f)]
                              {:file (str f) :sha256 (digest f) :forms (count xs)
                               :faults (faults xs [])})
                            (catch Exception e {:file (str f) :read-error (.getMessage e)}))) files)
             transitions (try (forms (str dir "/problem-transitions.edn")) (catch Exception _ []))
             ledger (forms (str dir "/ledger.edn"))
             disposition (first (for [k [:completed :parked :dispositions] x (get q k)
                                      :when (= id (:frame/id x))]
                                   {:bucket k :result (:frame/result x)
                                    :classification (:void/classification x)
                                    :decision (:decision/record x)}))]
         {:frame id :disposition disposition
          :first-at (:event/at (first ledger)) :last-at (:event/at (last ledger))
          :event-count (count ledger) :event-types (frequencies (map :event/type ledger))
          :transitions (mapv #(select-keys % [:event/observed-at :phase :operation]) transitions)
          :files reads})) (range 200 219))]
 (prn {:scope "F200-F218; ledger, transition and live EDN; no proof sources"
       :queue-sha256 (digest (str root "/queue-state.edn"))
       :coordinator {:sha256 (digest (str root "/coordinator.edn"))
                     :faults (faults (forms (str root "/coordinator.edn")) [])}
       :frames result}))
