(ns futon3c.analysis.memory-arm-e1
  "Deterministic extractor for the preregistered E1 retrieval-arm trace."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import [java.math BigInteger]
           [java.security MessageDigest]
           [java.time Duration Instant]))

(def extractor-version "3")

(defn- sha256-file [path]
  (let [digest (MessageDigest/getInstance "SHA-256")]
    (with-open [in (io/input-stream path)]
      (let [buffer (byte-array 65536)]
        (loop []
          (let [n (.read in buffer)]
            (when (pos? n)
              (.update digest buffer 0 n)
              (recur))))))
    (format "%064x" (BigInteger. 1 (.digest digest)))))

(defn- offered-bodies [document]
  (->> (:entries document)
       (filter #(= :offered (get-in % [:evidence/body :phase])))
       (sort-by :evidence/at)
       (map #(assoc (:evidence/body %) ::at (:evidence/at %)))
       vec))

(defn- span-seconds [start end]
  (.getSeconds (Duration/between (Instant/parse start) (Instant/parse end))))

(defn- surfaced-ids [body]
  (get-in body [:memory-use :memory-use/surfaced-ids]))

(defn- surfacing-via [body]
  (get-in body [:memory-use :memory-use/surfacing-via]))

(defn- unusable? [body]
  (nil? (:recall-query body)))

(defn- attributed? [body]
  (and (not (unusable? body))
       (seq (surfacing-via body))))

(defn- empty-dispatch? [body]
  (and (not (unusable? body))
       (= :recall-empty (:recall-status body))
       (empty? (surfaced-ids body))
       (empty? (surfacing-via body))))

(defn- unattributed-non-empty? [body]
  (and (not (unusable? body))
       (= :ok (:recall-status body))
       (seq (surfaced-ids body))
       (empty? (surfacing-via body))))

(defn- validate-attributed! [body]
  (let [ids (surfaced-ids body)
        vias (surfacing-via body)]
    (when-not (= :ok (:recall-status body))
      (throw (ex-info "attributed dispatch does not have :ok recall status"
                      {:job-id (:job-id body)})))
    (when-not (= ids (mapv :memory-id vias))
      (throw (ex-info "surfacing-via does not account for surfaced-ids in order"
                      {:job-id (:job-id body)})))
    (when-not (every? #{:content-match :pattern} (map :via vias))
      (throw (ex-info "unknown retrieval arm" {:job-id (:job-id body)})))))

(defn extract-trace
  "Construct the Lean `Trace` analogue and reject any unclassified receipt."
  [document]
  (let [bodies (offered-bodies document)
        attributed (filterv attributed? bodies)
        empty-dispatches (filterv empty-dispatch? bodies)
        unattributed-non-empty (filterv unattributed-non-empty? bodies)
        unusable (filterv unusable? bodies)
        earliest-attributed-index
        (or (first (keep-indexed (fn [index body]
                                   (when (attributed? body) index))
                                 bodies))
            (count bodies))
        corpus-span-seconds
        (if (seq bodies)
          (span-seconds (::at (first bodies)) (::at (last bodies)))
          0)
        attributed-span-seconds
        (if (seq attributed)
          (span-seconds (::at (first attributed)) (::at (last bodies)))
          0)
        classified-ids (concat (map :job-id attributed)
                               (map :job-id empty-dispatches)
                               (map :job-id unattributed-non-empty)
                               (map :job-id unusable))]
    (doseq [body attributed]
      (validate-attributed! body))
    (when-not (= (count bodies) (count classified-ids))
      (throw (ex-info "not every offered receipt has exactly one trace disposition"
                      {:offered (count bodies)
                       :classified (count classified-ids)})))
    (when-not (= (count classified-ids) (count (distinct classified-ids)))
      (throw (ex-info "trace dispositions overlap" {})))
    (sorted-map
     :attributed-span-seconds attributed-span-seconds
     :corpus-span-seconds corpus-span-seconds
     :empty-dispatches (mapv :job-id empty-dispatches)
     :earliest-attributed-index earliest-attributed-index
     :surfacings
     (->> attributed
          (mapcat (fn [body]
                    (map (fn [{:keys [memory-id via]}]
                           (sorted-map :dispatch-id (:job-id body)
                                       :memory-id memory-id
                                       :via-pattern (= :pattern via)))
                         (surfacing-via body))))
          vec)
     :total-dispatches (count bodies)
     :unattributed-non-empty (mapv :job-id unattributed-non-empty)
     :unusable (mapv :job-id unusable))))

(defn attribution-complete? [trace]
  (empty? (:unattributed-non-empty trace)))

(defn coverage-not-tail? [trace]
  (<= (:corpus-span-seconds trace)
      (* 2 (:attributed-span-seconds trace))))

(defn classify [trace]
  (let [surfacings (:surfacings trace)
        pattern-count (count (filter :via-pattern surfacings))]
    (cond
      (< (count surfacings) 20) :indeterminate
      (zero? pattern-count) :pattern-arm-silent
      (>= (* 4 pattern-count) (count surfacings)) :pattern-arm-substantial
      :else :pattern-arm-marginal)))

(defn analyze [input-path]
  (let [document (edn/read-string (slurp input-path))
        trace (extract-trace document)
        surfacings (:surfacings trace)
        pattern-count (count (filter :via-pattern surfacings))
        content-count (- (count surfacings) pattern-count)]
    (sorted-map
     :extractor (sorted-map :language "Clojure"
                            :language-version "1.12.0"
                            :version extractor-version)
     :frozen-input (sorted-map :path input-path
                               :sha256 (sha256-file input-path))
     :result (sorted-map
              :attribution-complete? (attribution-complete? trace)
              :attributed-dispatches
              (count (distinct (map :dispatch-id surfacings)))
              :attributed-span-seconds (:attributed-span-seconds trace)
              :classification (classify trace)
              :content-match-surfacings content-count
              :corpus-span-seconds (:corpus-span-seconds trace)
              :coverage-not-tail? (coverage-not-tail? trace)
              :empty-dispatches (count (:empty-dispatches trace))
              :earliest-attributed-index (:earliest-attributed-index trace)
              :offered-dispatches (:total-dispatches trace)
              :pattern-surfacings pattern-count
              :surfacing-denominator (count surfacings)
              :unattributed-non-empty-dispatches
              (count (:unattributed-non-empty trace))
              :unusable-dispatches (count (:unusable trace)))
     :trace trace)))

(defn -main [& [input-path output-path]]
  (when-not (and input-path output-path)
    (throw (ex-info "usage: INPUT.edn OUTPUT.edn" {})))
  (spit output-path (str (pr-str (analyze input-path)) "\n")))
