(ns futon3c.agency.hop-events
  "Inhabitation-log emission for agency.registry hop! / hop-back!
   transitions (E-pilot-hop-trigger-wiring §(4)).

   Hops are first-class events in the agent's inhabitation history, so
   each successful hop! / hop-back! appends a :event :hop-in / :hop-out
   entry to futon5a/data/pilot-inhabitations.edn.

   Loaded lazily by agency.registry via requiring-resolve to avoid a
   compile-time dependency cycle (agency.registry → util.edn-comment-preserving
   → ... is fine but the registry shouldn't depend on substrate locations).

   Per operator-approved design choice #4: :hop-in / :hop-out entries
   are kept in :all-events but filtered out of WIP/Done counts in the
   Inhabitation Log card (filtering done in transport.http
   derive-pilot-inhabitations)."
  (:require [futon3c.util.edn-comment-preserving :as edn-comments])
  (:import [java.time Instant]))

(def ^:private pilot-inhabitations-edn-path
  "/home/joe/code/futon5a/data/pilot-inhabitations.edn")

(defn- now-iso []
  (str (Instant/now)))

(defn- name-or-str [x]
  (cond
    (keyword? x) (name x)
    (symbol? x)  (name x)
    :else        (str x)))

(defn- hop-event-id [event-kind agent-id from-peri to-peri at-iso]
  ;; EDN keywords don't allow > 1 "/" so :id is a string slug.
  (str "hop/"
       (name-or-str agent-id)
       "/"
       (case event-kind
         :hop-in  (str (name-or-str from-peri) "-to-" (name-or-str to-peri))
         :hop-out (str (name-or-str from-peri) "-back-to-" (name-or-str to-peri)))
       "/"
       at-iso))

(defn- build-hop-event
  [event-kind {:keys [agent-id from-peri to-peri cited-cg-id]}]
  (let [at (now-iso)]
    (cond-> {:id          (hop-event-id event-kind agent-id from-peri to-peri at)
             :at          at
             :event       event-kind
             :pilot-agent (name-or-str agent-id)
             :from-peripheral (some-> from-peri name-or-str)
             :to-peripheral   (some-> to-peri name-or-str)
             :driver      :pilot-hop-trigger
             :driver-note "Recorded automatically by futon3c.agency.hop-events via agency.registry hop! / hop-back! (E-pilot-hop-trigger-wiring §(4))."
             :source      :live}
      cited-cg-id (assoc :cited-consent-gate-event-id cited-cg-id))))

(defn log-hop-event!
  "Append a :event :hop-in or :hop-out entry to pilot-inhabitations.edn.

   EVENT-KIND must be :hop-in or :hop-out.  PAYLOAD: {:agent-id, :from-peri,
   :to-peri, :cited-cg-id (optional)}.

   Errors are absorbed and returned as {:ok false :error ...} so a
   substrate-write failure never breaks a registry hop transition."
  [event-kind payload]
  (try
    (let [event (build-hop-event event-kind payload)
          r (edn-comments/append-items-to-top-level-vector-preserving-comments
             pilot-inhabitations-edn-path
             :events
             [event])]
      (assoc r :event-id (:id event)))
    (catch Throwable t
      {:ok false :error (.getMessage t)})))
