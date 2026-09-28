(ns futon3c.agency.offer-provider
  "Cache-only prompt segment for the newest verified exact-seat offer write."
  (:require [clojure.string :as str]
            [futon3c.agency.prompt-line :as prompt-line]))

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

(defn provider
  "Return one non-pattern marker from cache only. The `!` marker means a
   structured offer is visible; the header carries its id and option count."
  [{:keys [agent-id session-id]}]
  (when-let [{:keys [record observed-at]}
             (get @!offers [(str agent-id) (str session-id)])]
    (let [id (str (:id record))]
      (when-not (str/blank? id)
        {:segment/id :offer
         :segment/marker "!"
         :segment/provider "futon3c.agency.offer-provider/provider"
         :segment/observed-at observed-at
         :segment/basis {:evidence-ref id
                         :scope {:agent-id (str agent-id)
                                 :session-id (str session-id)}}
         :segment/header (str "offer " id " (" (count (:options record)) " options)")}))))

(defn register! []
  (prompt-line/register-provider!
   {:segment/id :offer
    :provider "futon3c.agency.offer-provider/provider"
    :fn provider
    :budget-ms 25}))

(register!)
