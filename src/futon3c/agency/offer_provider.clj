(ns futon3c.agency.offer-provider
  "Cache-only prompt segment for the newest verified exact-seat offer write."
  (:require [clojure.string :as str]
            [futon3c.agency.offer-record :as offer-record]
            [futon3c.agency.prompt-line :as prompt-line])
  (:import [java.time Instant]))

(defonce ^:private !offers (atom {}))

(defn reset-cache! [] (reset! !offers {}))

(defn publish!
  "Publish a verified offer result for its exact seat."
  [{:keys [record receipt] :as result}]
  (let [agent (get-in record [:seat :agent])
        session (get-in record [:seat :session])
        observed-at (get-in receipt [:system-as-of])]
    (when (and (map? result) (not (str/blank? (str agent)))
               (not (str/blank? (str session)))
               (not (str/blank? (str observed-at))))
      (swap! !offers assoc [(str agent) (str session)]
             {:record record :observed-at (str observed-at)})))
  result)

(defn cached-offer [agent session]
  (get @!offers [(str agent) (str session)]))

(defn clear!
  "Clear OFFER-ID from one exact seat after its verified acceptance."
  [agent session offer-id]
  (let [key [(str agent) (str session)]]
    (swap! !offers (fn [cache]
                     (if (= (str offer-id) (str (get-in cache [key :record :id])))
                       (dissoc cache key)
                       cache)))))

(defn provider
  "Return one non-pattern marker from cache only. The `!` marker means a
   structured offer is visible; the header carries its id and option count.
   An offer is hidden from its `:until` onward (half-open, as in
   `offer-record/active-offers-as-of`)."
  [{:keys [agent-id session-id render-at]}]
  (when-let [{:keys [record observed-at]}
             (get @!offers [(str agent-id) (str session-id)])]
    (let [id (str (:id record))
          until (:until record)
          expired? (and until render-at
                        (not (.isBefore (Instant/parse (str render-at))
                                        (Instant/parse (str until)))))]
      (when-not (or (str/blank? id) expired?)
        {:segment/id :offer
         :segment/marker "!"
         :segment/provider "futon3c.agency.offer-provider/provider"
         :segment/observed-at observed-at
         :segment/basis {:evidence-ref id
                         :scope {:agent-id (str agent-id)
                                 :session-id (str session-id)}}
         :segment/detail (offer-record/display-lines record)
         :segment/header (str "offer " id " (" (count (:options record)) " options)")}))))

(defn register! []
  (prompt-line/register-provider!
   {:segment/id :offer
    :provider "futon3c.agency.offer-provider/provider"
    :fn provider
    :budget-ms 25}))

(register!)
