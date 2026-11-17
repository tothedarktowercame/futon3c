(ns futon3c.diagramprover.findings
  "Pure EDN findings; no clock, git, store or function serialisation.
  A representative is the shortest witness (stable on ties). Observations and
  settlement describe that representative, not a universal claim over hosts;
  cross-host merging retains every constituent finding in :finding/host-evidence."
  (:require [clojure.edn :as edn]
            [clojure.set :as set])
  (:import [java.security MessageDigest]
           [java.time Instant]))

(defn schema-digest
  "SHA-256 of UTF-8 pr-str of the ordered vector of
  [name rule-id pattern guard-names priority] tuples (absent priority = 0).
  Functions are deliberately excluded: this is a data-shape pin, NOT a digest
  of executable guard/rewrite semantics; checker-sha must pin that code too."
  [schemas]
  (let [data (mapv (fn [s] [(:name s) (:rule-id s) (:pattern s)
                           (mapv first (:guards s)) (:priority s 0)]) schemas)
        bytes (.digest (MessageDigest/getInstance "SHA-256")
                       (.getBytes (pr-str data) "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and 255 %)) bytes))))

(defn finding-key [f]
  [(:finding/rules f) (:finding/overlap f) (:finding/status f)])

(defn- rule-ref [schema] (or (:rule-id schema) (:name schema)))
(defn- sorted-rules [rules] (vec (sort-by pr-str rules)))

(defn- evaluation [status settled]
  (case status
    :joining :met
    :inconclusive :unknown
    :non-joining (if (seq (set/intersection (or settled #{}) #{:guard :priority :traversal}))
                   :settled :failing)))

(defn- with-residual [f residual]
  (if (and residual (= :failing (:finding/evaluation f)))
    (assoc f :finding/evaluation :failing-accepted :finding/residual residual) f))

(defn findings
  "Merge pair reports by unordered rule references, overlap tree and status.
  Optional third argument {:residuals {finding-key residual-id}} accepts failures
  without discharging them. Caller supplies all pins, including optional host-id."
  ([report pins] (findings report pins {}))
  ([report pins {:keys [residuals]}]
   (let [rows (mapv (fn [pair]
                      (let [refs (mapv #(rule-ref (:schema %)) (:instances pair))
                            names (:schemas pair)
                            by-name (zipmap names refs)
                            settled (get-in pair [:settled-by :settled-by])]
                        (cond->
                         {:finding/rules (sorted-rules refs)
                          :finding/overlap (:overlap pair)
                          :finding/positions [(:position pair)]
                          :finding/kinds #{(:kind pair)}
                          :finding/status (:status pair)
                          :finding/observations
                          {:comparison (:observation-comparison pair)
                           :sides (mapv #(set (:observations %)) (:normalisations pair))}
                          :finding/occurrences 1
                          :finding/witness-path
                          (mapv (fn [step]
                                  {:rule (get by-name (:schema step) (:schema step))
                                   :position (:position step)}) (:path pair))
                          :finding/pins pins
                          :finding/evaluation (evaluation (:status pair) settled)}
                          (some? settled) (assoc :finding/settled-by settled)))) (:pairs report))]
     (mapv (fn [[key fs]]
             (let [representative (first (sort-by #(count (:finding/witness-path %)) fs))]
               (with-residual
                 (assoc representative
                        :finding/positions (vec (distinct (mapcat :finding/positions fs)))
                        :finding/kinds (apply set/union (map :finding/kinds fs))
                        :finding/occurrences (count fs))
                 (get residuals key))))
           (sort-by (comp pr-str key) (group-by finding-key rows))))))

(defn merge-hosts
  "Merge a flat sequence of findings by finding-key. Keep representative pins,
  observations, settlement and evaluation together; :finding/host-evidence retains
  ALL inputs so differing pins/dispositions are not silently overwritten.
  host-count counts distinct host ids, occurrences counts reports. Missing host-id
  is refused rather than inventing an identity."
  [fs]
  (doseq [f fs]
    (when-not (string? (get-in f [:finding/pins :host-id]))
      (throw (ex-info "A host-id is required" {:finding-key (finding-key f)}))))
  (mapv (fn [[_ group]]
          (let [representative (first (sort-by #(count (:finding/witness-path %)) group))
                hosts (vec (distinct (map #(get-in % [:finding/pins :host-id]) group)))]
            (assoc representative
                   :finding/positions (vec (distinct (mapcat :finding/positions group)))
                   :finding/kinds (apply set/union (map :finding/kinds group))
                   :finding/occurrences (reduce + (map :finding/occurrences group))
                   :finding/hosts hosts :finding/host-count (count hosts)
                   :finding/host-evidence (vec group))))
        (sort-by (comp pr-str key) (group-by finding-key fs))))

(defn- instant? [x]
  (and (string? x) (try (Instant/parse x) true (catch Exception _ false))))
(defn- edn-data? [x]
  (cond
    (or (nil? x) (boolean? x) (string? x) (keyword? x) (symbol? x) (number? x)) true
    (map? x) (every? edn-data? (mapcat identity x))
    (or (vector? x) (list? x) (set? x)) (every? edn-data? x)
    :else false))
(defn- position? [p] (and (vector? p) (every? nat-int? p)))
(defn- operator? [op]
  ;; Imported Lean nodes use a vector operator such as [:const "Classical.choose"]
  ;; (lean_import.clj); it is data, not a child term.
  (or (keyword? op)
      (and (vector? op) (keyword? (first op))
           (every? #(or (string? %) (keyword? %) (number? %)) (rest op)))))
(defn- term? [t]
  (and (vector? t) (operator? (first t))
       (if (= :bvar (first t)) (and (= 2 (count t)) (nat-int? (second t)))
           (every? term? (rest t)))))
(defn- ref? [x] (or (keyword? x) (symbol? x) (and (string? x) (seq x))))

(defn valid-finding?
  "Shape and evaluation consistency check, not verification of hashes or claims."
  [f]
  (boolean
   (and (map? f) (edn-data? f)
        (let [rules (:finding/rules f) pins (:finding/pins f)
              status (:finding/status f) ev (:finding/evaluation f)
              settled (:finding/settled-by f) obs (:finding/observations f)]
          (and (vector? rules) (= 2 (count rules)) (every? ref? rules)
               (= rules (sorted-rules rules)) (term? (:finding/overlap f))
               (vector? (:finding/positions f)) (seq (:finding/positions f))
               (every? position? (:finding/positions f))
               (= (count (:finding/positions f)) (count (distinct (:finding/positions f))))
               (set? (:finding/kinds f)) (seq (:finding/kinds f))
               (set/subset? (:finding/kinds f) #{:root :pattern-nested :variable-nested :created})
               (#{:joining :non-joining :inconclusive} status)
               (or (nil? settled) (and (set? settled)
                                       (set/subset? settled #{:guard :priority :traversal :nothing})
                                       (or (not (contains? settled :nothing)) (= settled #{:nothing}))))
               (#{:equal :different :inconclusive :not-requested} (:comparison obs))
               (vector? (:sides obs)) (= 2 (count (:sides obs))) (every? set? (:sides obs))
               (pos-int? (:finding/occurrences f))
               (vector? (:finding/witness-path f))
               (every? #(and (= #{:rule :position} (set (keys %)))
                              (ref? (:rule %)) (position? (:position %))) (:finding/witness-path f))
               (map? pins) (string? (:checker-sha pins)) (string? (:schema-digest pins))
               (= :unordered (:mode pins)) (nat-int? (:max-states pins)) (instant? (:event-time pins))
               (if (contains? f :finding/residual)
                 (and (= :failing (evaluation status settled)) (= :failing-accepted ev)
                      (string? (:finding/residual f)))
                 (= ev (evaluation status settled)))
               (if (contains? f :finding/hosts)
                 (and (vector? (:finding/hosts f)) (every? string? (:finding/hosts f))
                      (= (:finding/host-count f) (count (distinct (:finding/hosts f))))
                      (vector? (:finding/host-evidence f))
                      (every? valid-finding? (:finding/host-evidence f))) true))))))

(defn findings->edn [fs]
  (when-not (and (vector? fs) (every? valid-finding? fs))
    (throw (ex-info "Invalid finding records" {})))
  (let [text (pr-str fs)]
    (when-not (= fs (edn/read-string text))
      (throw (ex-info "Findings do not round-trip as EDN" {})))
    text))
