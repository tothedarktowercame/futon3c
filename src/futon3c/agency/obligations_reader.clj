(ns futon3c.agency.obligations-reader
  "Complete, bounded, read-only adapter from futon1b to P9's pure input.
   Promise states select only open or checkable promises involving AGENT-ID;
   their immutable history chains are then read by promise endpoint. Every
   paged source has a hard cap that refuses rather than returns partial data."
  (:require [clojure.string :as str]
            [futon3c.agency.agreement-record :as agreement]
            [futon3c.agency.offer-record :as offer]
            [futon3c.agency.promise-log :as promise-log]
            [futon3c.agency.rule-record :as store])
  (:import [java.net URLEncoder]
           [java.time Instant]))

(def page-limit 1000)
(def max-pages 20)
(def state-log-cutover "2026-11-05T19:04:22Z")
(def ^:dynamic *request!* store/request!)

(defn- encode [x] (URLEncoder/encode (str x) "UTF-8"))
(defn- keyword-string [k]
  (if-let [ns (namespace k)] (str ns "/" (name k)) (name k)))
(defn- temporal-query [mode t]
  (if (= :as-of mode)
    (str "&system-as-of=" (encode t) "&valid-as-of=" (encode t)) ""))

(defn- timeout? [e]
  (or (= 504 (:status (ex-data e)))
      (some #(str/includes? (str/lower-case (or (.getMessage ^Throwable %) "")) "timed out")
            (take-while some? (iterate ex-cause e)))))

(defn- request! [base path source]
  (try (*request!* base "GET" path nil)
       (catch Throwable e
         (if (timeout? e)
           (throw (ex-info "Obligations store timed out"
                           {:reason :store-timeout :source source} e))
           (throw e)))))

(defn- truncated! [source pages rows]
  (throw (ex-info "Obligations input exceeded its page cap"
                  {:reason :truncated-input :source source :pages pages :rows rows})))

(defn- evidence-pages [base tag t mode source]
  (let [base-path (str "/api/alpha/evidence?tags=" (encode tag)
                       "&limit=" page-limit (temporal-query mode t))]
    (loop [path base-path pages 0 rows [] seen #{}]
      (let [response (request! base path source)
            entries (vec (:entries response))
            pages' (inc pages)
            rows' (into rows entries)
            cursor (:next-cursor response)]
        (when (or (:truncated response) (:partial? response)
                  (and (:incomplete response) (nil? cursor))
                  (and (nil? cursor) (= page-limit (count entries))))
          (truncated! source pages' (count rows')))
        (cond
          (nil? cursor) {:rows rows' :pages pages'}
          (>= pages' max-pages) (truncated! source pages' (count rows'))
          (or (not (string? (:at cursor))) (not (string? (:id cursor)))
              (contains? seen cursor))
          (truncated! source pages' (count rows'))
          :else
          (recur (str base-path "&cursor-at=" (encode (:at cursor))
                      "&cursor-id=" (encode (:id cursor)))
                 pages' rows' (conj seen cursor)))))))

(defn- hyperedge-pages [base type endpoint t mode source]
  (let [base-path (str "/api/alpha/hyperedges?type=" (encode (keyword-string type))
                       "&end=" (encode endpoint) "&limit=" page-limit
                       "&include-total=false" (temporal-query mode t))]
    (loop [path base-path pages 0 rows [] seen #{}]
      (let [response (request! base path source)
            entries (vec (:hyperedges response))
            pages' (inc pages)
            rows' (into rows entries)
            cursor (:next-cursor response)]
        (when (or (:truncated response) (:partial? response)
                  (and (:incomplete response) (nil? cursor)))
          (truncated! source pages' (count rows')))
        (cond
          (nil? cursor) {:rows rows' :pages pages'}
          (>= pages' max-pages) (truncated! source pages' (count rows'))
          (or (not (string? cursor)) (contains? seen cursor))
          (truncated! source pages' (count rows'))
          :else
          (recur (str base-path "&after=" (encode cursor))
                 pages' rows' (conj seen cursor)))))))

(defn- read-offer
  "An offer's ends are its author, seat and addressee, not its own id, so it is
   listed through the offeror's agent endpoint and picked out by id."
  [base id offeror t mode]
  (let [{:keys [rows]} (hyperedge-pages base :offer/record (str "agent:" offeror)
                                        t mode :offers)]
    (some #(when (= id (:hx/id %)) %) rows)))

(defn read-inputs
  "Read inputs for AGENT-ID. MODE is :current or :as-of. Current mode omits
   temporal store parameters and T is the caller's read-start cutoff; as-of mode
   pins both store axes to T. Returns source population/page counts in :basis."
  ([base agent-id t] (read-inputs base agent-id t :as-of))
  ([base agent-id t mode]
   (when-not (contains? #{:current :as-of} mode)
     (throw (ex-info "Invalid obligations read mode" {:reason :invalid-mode})))
   (when-not (try (Instant/parse t) true (catch Exception _ false))
     (throw (ex-info "Invalid obligations instant" {:reason :invalid-as-of})))
   (when (and (= :as-of mode)
              (.isBefore (Instant/parse t) (Instant/parse state-log-cutover)))
     (throw (ex-info "Promise state log cannot answer this historical instant"
                     {:reason :as-of-before-state-log
                      :requested-at t :state-log-cutover state-log-cutover})))
   (let [started-at (str (Instant/now))
         ;; The declaration below names these same values, so it describes the
         ;; query that ran rather than a second copy of it.
         outcome-tag "promise-outcome"
         outcome-types [:promise/fulfilled :promise/lapsed :promise/fulfilment-check]
         open-state-page (hyperedge-pages base :promise/state "promise-status:open"
                                          t mode :promise-states-open)
         ;; `obligations.clj` retains explicitly released checkable promises in
         ;; :ignored, and a plain release does not discharge a checkable debt.
         ;; This small indexed population (4 at the 2026-11-05 cutover) is the
         ;; bounded closed-state supplement; never scan all closed states.
         checkable-state-page
         (hyperedge-pages base :promise/state "promise-obligation:checkable"
                          t mode :promise-states-checkable)
         party? (fn [edge]
                  (let [props (:hx/props edge)]
                    (or (= agent-id (:debtor props))
                        (= agent-id (:creditor props)))))
         selected-states (->> (concat (:rows open-state-page)
                                      (:rows checkable-state-page))
                              (filter party?)
                              (map (juxt :hx/id identity))
                              (into {}) vals (sort-by :hx/id) vec)
         history-pages (mapv #(hyperedge-pages base :promise/history-event
                                               (get-in % [:hx/props :promise-id])
                                               t mode :promise-history)
                             selected-states)
         history (mapv promise-log/hyperedge->entry (mapcat :rows history-pages))
         outcome-page (evidence-pages base outcome-tag t mode :promise-outcomes)
         outcomes (->> (:rows outcome-page)
                       (filter #(contains? (set outcome-types) (:evidence/type %))) vec)
         endpoint (str "agent:" agent-id)
         agreement-page (hyperedge-pages base :agreement/record endpoint t mode :agreements)
         agreement-edges (:rows agreement-page)
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
         offeror-by-offer (into {} (map (juxt :agreement/offer :agreement/offeror))
                                agreements)
         offer-mapped (mapv (fn [id]
                              (try
                                (if-let [edge (read-offer base id (offeror-by-offer id)
                                                          t mode)]
                                  {:record (offer/hyperedge->record edge) :fetched? true}
                                  {:issue {:obligation/id id :reason :unknown-offer :record-id id}})
                                (catch Exception e
                                  {:fetched? true
                                   :issue {:obligation/id id :reason :unreadable-offer
                                           :record-id id :message (.getMessage e)}})))
                            offer-ids)
         pages {:promise-states-open (:pages open-state-page)
                :promise-states-checkable (:pages checkable-state-page)
                :promise-history (reduce + 0 (map :pages history-pages))
                :promise-outcomes (:pages outcome-page)
                :agreements (:pages agreement-page)
                :offers (count offer-ids)}
         rows {:promise-states-open (count (:rows open-state-page))
               :promise-states-checkable (count (:rows checkable-state-page))
               :promise-history (count history)
               :promise-outcomes (count (:rows outcome-page))
               :agreements (count agreement-edges)
               :offers (count offer-ids)}
         reader-incomplete (vec (concat (keep :issue mapped) (keep :issue offer-mapped)))
         finished-at (str (Instant/now))
         population
         {:question {:agent agent-id :at-or-cutoff t
                     :kinds #{:promise :agreement} :mode mode}
          :sources
          [{:kind :hyperedge
            :filter {:type :promise/state :end "promise-status:open"}
            :rows-fetched (count (:rows open-state-page))
            :rows-used (count (filter party? (:rows open-state-page)))
            :pages (:pages open-state-page) :page-limit page-limit :complete? true}
           {:kind :hyperedge
            :filter {:type :promise/state :end "promise-obligation:checkable"}
            :rows-fetched (count (:rows checkable-state-page))
            :rows-used (count (filter party? (:rows checkable-state-page)))
            :pages (:pages checkable-state-page) :page-limit page-limit :complete? true}
           {:kind :hyperedge
            :filter {:type :promise/history-event
                     :ids (mapv #(get-in % [:hx/props :promise-id]) selected-states)}
            :rows-fetched (count history) :rows-used (count history)
            :pages (reduce + 0 (map :pages history-pages))
            :page-limit page-limit :complete? true}
           {:kind :evidence
            :filter {:tags [outcome-tag] :types outcome-types}
            :rows-fetched (count (:rows outcome-page)) :rows-used (count outcomes)
            :pages (:pages outcome-page) :page-limit page-limit :complete? true}
           {:kind :hyperedge
            :filter {:type :agreement/record :end endpoint}
            :rows-fetched (count agreement-edges) :rows-used (count agreements)
            :pages (:pages agreement-page) :page-limit page-limit :complete? true}
           {:kind :hyperedge :filter {:type :offer/record :ids offer-ids}
            :rows-fetched (count (filter :fetched? offer-mapped))
            :rows-used (count (keep :record offer-mapped))
            :pages (count offer-ids) :page-limit page-limit :complete? true}]
          :excluded [{:reason :incomplete :rows (count reader-incomplete)
                      :scope :repository-wide}]
          :read (if (= :current mode)
                  {:mode :current :system-as-of :unpinned :cutoff t
                   :started-at started-at :finished-at finished-at}
                  {:mode :as-of :system-as-of t :valid-as-of t})}]
     {:promise-history history
      :promise-outcomes outcomes
      :agreements agreements
      :offers (vec (keep :record offer-mapped))
      :reader-incomplete reader-incomplete
      :basis {:mode mode :t t :pages pages :rows rows :population population}})))
