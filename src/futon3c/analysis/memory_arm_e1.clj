(ns futon3c.analysis.memory-arm-e1
  "Deterministic extractor for the preregistered E1 retrieval-arm trace."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import [java.math BigInteger]
           [java.security MessageDigest]))

(def extractor-version "1")

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
       (map :evidence/body)
       (filter #(= :offered (:phase %)))
       (sort-by :job-id)
       vec))

(defn- surfaced-ids [body]
  (get-in body [:memory-use :memory-use/surfaced-ids]))

(defn- surfacing-via [body]
  (get-in body [:memory-use :memory-use/surfacing-via]))

(defn- attributed? [body]
  (seq (surfacing-via body)))

(defn- empty-dispatch? [body]
  (and (= :recall-empty (:recall-status body))
       (empty? (surfaced-ids body))
       (empty? (surfacing-via body))))

(defn- unusable? [body]
  (and (= :ok (:recall-status body))
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
        unusable (filterv unusable? bodies)
        classified-ids (concat (map :job-id attributed)
                               (map :job-id empty-dispatches)
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
     :empty-dispatches (mapv :job-id empty-dispatches)
     :surfacings
     (->> attributed
          (mapcat (fn [body]
                    (map (fn [{:keys [memory-id via]}]
                           (sorted-map :dispatch-id (:job-id body)
                                       :memory-id memory-id
                                       :via-pattern (= :pattern via)))
                         (surfacing-via body))))
          vec)
     :unusable (mapv :job-id unusable))))

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
              :attributed-dispatches
              (count (distinct (map :dispatch-id surfacings)))
              :classification (classify trace)
              :content-match-surfacings content-count
              :empty-dispatches (count (:empty-dispatches trace))
              :offered-dispatches
              (+ (count (distinct (map :dispatch-id surfacings)))
                 (count (:empty-dispatches trace))
                 (count (:unusable trace)))
              :pattern-surfacings pattern-count
              :surfacing-denominator (count surfacings)
              :unusable-dispatches (count (:unusable trace)))
     :trace trace)))

(defn -main [& [input-path output-path]]
  (when-not (and input-path output-path)
    (throw (ex-info "usage: INPUT.edn OUTPUT.edn" {})))
  (spit output-path (str (pr-str (analyze input-path)) "\n")))

