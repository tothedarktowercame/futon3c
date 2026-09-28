(ns futon3c.agency.obligations-reader
  "Bounded, read-only adapter from futon1b LIST responses to P9's pure input.
   Promise history is read repository-wide: evidence LIST can filter author,
   but that would omit promises owed to AGENT-ID where another author is debtor."
  (:require [futon3c.agency.agreement-record :as agreement]
            [futon3c.agency.offer-record :as offer]
            [futon3c.agency.rule-record :as store])
  (:import [java.net URLEncoder]
           [java.time Instant]))

(def page-limit 1000)
(def ^:dynamic *request!* store/request!)

(defn- encode [x] (URLEncoder/encode (str x) "UTF-8"))
(defn- keyword-string [k]
  (if-let [ns (namespace k)] (str ns "/" (name k)) (name k)))

(defn- refuse! [source response]
  (throw (ex-info "Obligations input is truncated"
                  {:reason :truncated-input :source source
                   :returned (or (:count response)
                                 (count (or (:entries response) (:hyperedges response))))})))

(defn- truncated? [response rows]
  (or (>= (count rows) page-limit)
      (true? (:truncated response)) (true? (:partial? response))
      (true? (:incomplete response)) (some? (:next-cursor response))))

(defn- evidence-page [base tag t source]
  (let [path (str "/api/alpha/evidence?tags=" (encode tag)
                  "&limit=" page-limit
                  "&system-as-of=" (encode t) "&valid-as-of=" (encode t))
        response (*request!* base "GET" path nil)
        rows (vec (:entries response))]
    (when (truncated? response rows) (refuse! source response))
    rows))

(defn- hyperedge-page [base type endpoint t source]
  (let [path (str "/api/alpha/hyperedges?type=" (encode (keyword-string type))
                  "&end=" (encode endpoint) "&limit=" page-limit
                  "&include-total=false&system-as-of=" (encode t)
                  "&valid-as-of=" (encode t))
        response (*request!* base "GET" path nil)
        rows (vec (:hyperedges response))]
    (when (truncated? response rows) (refuse! source response))
    rows))

(defn- read-offer [base id t]
  ;; Direct lookup has no temporal contract, so use the endpoint LIST route.
  (let [rows (hyperedge-page base :offer/record id t :offers)]
    (some #(when (= id (:hx/id %)) %) rows)))

(defn read-inputs
  "Read complete bounded inputs for AGENT-ID at instant T. Any source that fills
   its page is refused rather than returned partially. Mapping failures become
   :reader-incomplete diagnostics consumed by obligations-as-of."
  [base agent-id t]
  (when-not (try (Instant/parse t) true (catch Exception _ false))
    (throw (ex-info "Invalid obligations instant" {:reason :invalid-as-of})))
  (let [history (evidence-page base "promise-history" t :promise-history)
        outcomes (->> (evidence-page base "promise-outcome" t :promise-outcomes)
                      (filter #(contains? #{:promise/fulfilled :promise/lapsed}
                                          (:evidence/type %))) vec)
        endpoint (str "agent:" agent-id)
        agreement-edges (hyperedge-page base :agreement/record endpoint t :agreements)
        mapped (mapv (fn [edge]
                       (try {:record (agreement/hyperedge->record edge)}
                            (catch Exception e
                              {:issue {:obligation/id (:hx/id edge)
                                       :reason :unreadable-agreement
                                       :record-id (:hx/id edge)
                                       :message (.getMessage e)}})))
                     agreement-edges)
        agreements (vec (keep :record mapped))
        offer-ids (vec (distinct (map :agreement/offer agreements)))
        offer-mapped (mapv (fn [id]
                             (try
                               (if-let [edge (read-offer base id t)]
                                 {:record (offer/hyperedge->record edge)}
                                 {:issue {:obligation/id id :reason :unknown-offer
                                          :record-id id}})
                               (catch Exception e
                                 {:issue {:obligation/id id :reason :unreadable-offer
                                          :record-id id :message (.getMessage e)}})))
                           offer-ids)]
    {:promise-history history
     :promise-outcomes outcomes
     :agreements agreements
     :offers (vec (keep :record offer-mapped))
     :reader-incomplete (vec (concat (keep :issue mapped) (keep :issue offer-mapped)))
     :source-counts {:promise-history (count history)
                     :promise-outcomes (count outcomes)
                     :agreements (count agreement-edges)
                     :offers (count offer-ids)}}))
