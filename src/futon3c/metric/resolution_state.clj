(ns futon3c.metric.resolution-state
  "Metric-owned :resolution-state providers for M-substrate-metric O3.

   Providers consume substrate vertex docs when available and fall back to the
   endpoint grain. Unknown states are explicitly non-actionable."
  (:require [clojure.string :as str]
            [futon3c.metric.e1-report :as e1-report]))

(defn- prop
  [m k]
  (or (get m k)
      (get m (name k))
      (when-let [ns-part (namespace k)]
        (get m (str ns-part "/" (name k))))))

(defn- first-endpoint
  [doc]
  (first (or (:hx/endpoints doc)
             (get doc "hx/endpoints")
             (:endpoints doc))))

(defn- normalize-token
  [x]
  (cond
    (nil? x) nil
    (keyword? x) (-> x name str/lower-case)
    (string? x) (-> x str/trim (str/replace #"^:" "") str/lower-case)
    :else (-> x str str/lower-case)))

(def mission-phase->resolvedness
  {"head" 0.10
   "identify" 0.10
   "map" 0.10
   "derive" 0.10
   "argue" 0.35
   "verify" 0.35
   "instantiate" 0.65
   "document" 0.65
   "complete" 1.00
   "closed" 1.00
   "dissolved" 1.00})

(def sorry-status->resolvedness
  {"open" 0.00
   "reopened" 0.00
   "addressed" 0.65
   "closed" 1.00
   "foreclosed" 1.00
   "falsified" 1.00
   "n-a-by-design" 1.00
   "acknowledged-v1-in-force" 1.00})

(def pattern-status->resolvedness
  {"validated" 1.00
   "applied" 1.00
   "in-force" 1.00
   "candidate" 0.20
   "unvalidated" 0.20
   "proposed" 0.20})

(defn- state
  [source resolvedness raw]
  {:resolution-state/source source
   :resolution-state/resolvedness resolvedness
   :resolution-state/actionable? (and (number? resolvedness)
                                      (< (double resolvedness) 1.0))
   :resolution-state/raw raw})

(defn mission-resolution-state
  [doc]
  (let [props (:hx/props doc)
        phase (or (prop props :mission/phase)
                  (prop props :mission/status)
                  (prop props :phase))
        normalized (normalize-token phase)
        resolvedness (get mission-phase->resolvedness normalized :unknown)]
    (state :metric/resolution-mission-v0 resolvedness
           {:phase phase :normalized normalized})))

(defn sorry-resolution-state
  [doc]
  (let [props (:hx/props doc)
        status (or (prop props :sorry/status)
                   (prop props :status))
        normalized (normalize-token status)
        resolvedness (get sorry-status->resolvedness normalized :unknown)]
    (state :metric/resolution-sorry-v0 resolvedness
           {:status status :normalized normalized})))

(defn pattern-resolution-state
  [doc]
  (let [props (:hx/props doc)
        status (or (prop props :pattern/status)
                   (prop props :pattern/state)
                   (prop props :status))
        normalized (normalize-token status)
        resolvedness (get pattern-status->resolvedness normalized :unknown)]
    (state :metric/resolution-pattern-v0 resolvedness
           {:status status :normalized normalized})))

(defn file-resolution-state
  [_doc]
  {:resolution-state/source :none
   :resolution-state/resolvedness :unknown
   :resolution-state/actionable? false
   :resolution-state/raw {:reason :file-has-no-native-resolution-state}})

(defn resolution-state
  "Return the metric-owned resolution-state map for a substrate node doc.

   If the doc is only an endpoint-like map, pass :node/id or :endpoint."
  [doc]
  (let [endpoint (or (:node/id doc)
                     (:endpoint doc)
                     (first-endpoint doc))
        grain (or (:node/type doc)
                  (e1-report/node-type endpoint))]
    (case grain
      :mission (mission-resolution-state doc)
      :sorry (sorry-resolution-state doc)
      :pattern (pattern-resolution-state doc)
      :file (file-resolution-state doc)
      {:resolution-state/source :none
       :resolution-state/resolvedness :unknown
       :resolution-state/actionable? false
       :resolution-state/raw {:reason :unknown-grain
                              :endpoint endpoint
                              :grain grain}})))
