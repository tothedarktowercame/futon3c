(ns futon3c.arxana.queries
  "Bounded reserved-record queries. Return the server body unchanged on HTTP
  success (including stale-index/truncated), or the store's {:error ...}.
  A caller must inspect :status and :truncated? before treating results as
  complete. These wrappers neither page nor turn missing facts into success."
  (:require [futon3c.arxana.store :as store]))

(defn- query [graph-store type options]
  (let [response (store/bounded-query-settled! graph-store (assoc options :query/type type))]
    (if (:error response) response (:ok response))))

(defn records-by-type [graph-store options]
  (query graph-store :records-by-type options))
(defn records-by-field [graph-store options]
  (query graph-store :records-by-field options))
(defn records-by-field-range [graph-store options]
  (query graph-store :records-by-field-range options))
(defn records-by-span-overlap [graph-store options]
  (query graph-store :records-by-span-overlap (merge {:interval :half-open} options)))
(defn type-census [graph-store options]
  (query graph-store :type-census options))
(defn attestations
  "Subject history with server total/truncated metadata; never silently complete."
  [graph-store options]
  (query graph-store :attestations options))
