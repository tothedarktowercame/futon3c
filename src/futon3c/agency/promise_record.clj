(ns futon3c.agency.promise-record
  "Optional promise metadata shared by parks and followups. No fulfilment evaluator
   runs here: job-terminal-ok describes a terminal successful job; prose is explicitly
   not machine-evaluable. Dependency completion, wake delivery, fulfilment, release,
   and overdue remain distinct outcomes for future consumers."
  (:require [clojure.string :as str])
  (:import [java.time Instant DateTimeException]))

(def field-keys [:beneficiary :deadline :fulfilment-criterion])

(defn- refuse! [field message]
  (throw (ex-info message {:reason (keyword (str "invalid-" (name field)))
                           :field field :promise-record/refusal true})))

(defn- nonblank? [x] (and (string? x) (not (str/blank? x))))

(defn- criterion [value]
  (when-not (map? value)
    (refuse! :fulfilment-criterion "Criterion must be a typed map"))
  (let [v (into {} (map (fn [[k x]] [(if (string? k) (keyword k) k) x])) value)
        kind (if (string? (:kind v)) (keyword (:kind v)) (:kind v))
        [required evaluable?] (case kind
                               :job-terminal-ok [:job-id true]
                               :prose [:text false]
                               (refuse! :fulfilment-criterion "Unknown criterion kind"))]
    (when-not (and (nonblank? (get v required))
                   (every? #{:kind :machine-evaluable? required} (keys v))
                   (or (not (contains? v :machine-evaluable?))
                       (= evaluable? (:machine-evaluable? v))))
      (refuse! :fulfilment-criterion "Invalid criterion fields or evaluability claim"))
    (assoc v :kind kind :machine-evaluable? evaluable?)))

(defn fields
  "Validate and extract optional EDN/JSON fields. Deadline is an absolute ISO-8601
   instant, preserved verbatim; it is NOT the park's active :deadline-ms timeout.
   Absent fields stay absent. Invalid supplied fields yield typed ex-info refusals."
  [request]
  (reduce
   (fn [out k]
     (if-let [entry (or (find request k) (find request (name k)))]
       (let [v (val entry)]
         (assoc out k
                (case k
                  :beneficiary (if (nonblank? v) v
                                   (refuse! k "Beneficiary must be a nonblank identity"))
                  :deadline (do
                              (when-not (string? v)
                                (refuse! k "Deadline must be an absolute ISO-8601 instant"))
                              (try (Instant/parse v)
                                   (catch DateTimeException _
                                     (refuse! k "Deadline must be an absolute ISO-8601 instant")))
                              v)
                  :fulfilment-criterion (criterion v))))
       out))
   {} field-keys))
