(ns futon3c.agency.refusal-reset
  "A Zai seat whose conversation outgrew the model is refused on every turn
   (HTTP 400, code 1261, \"Prompt exceeds max length\"): zai-7 lost every turn
   from 2026-10-18 04:00Z until a manual reset. zai_api now compacts and
   retries (ff8ea7cc5), but that code lives in each seat's invoke closure, so
   seats built before it keep the old behaviour until rebuilt. This listener
   reaches them now: when a turn's final round is the provider's 1261 refusal,
   reset that seat's session once (per session id) and record it.
   Matches the refusal itself (the round text parses as the provider's error
   JSON), never prose that mentions it. Joe, 2026-10-19: \"it's a matter of
   restarting one session\"."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [futon3c.agency.registry :as reg]
            [futon3c.agency.turn-events :as turn-events]
            [futon3c.evidence.boundary :as boundary]
            [futon3c.evidence.store :as store]))

(defonce ^{:doc "Session ids already reset, so one bad session is reset once."}
  !reset (atom #{}))

(defonce !last (atom nil))

(defn- body-field [body k]
  (when (map? body) (or (get body k) (get body (name k)))))

(defn refusal?
  "Is TEXT the provider's prompt-too-long refusal (its error JSON)?"
  [text]
  (let [s (str/trim (str text))]
    (boolean
     (when (str/starts-with? s "{")
       (let [m (try (json/parse-string s true) (catch Exception _ nil))
             err (get-in m [:body :error])]
         (or (= "1261" (str (:code err)))
             (and (string? (:message err))
                  (str/includes? (str/lower-case (:message err)) "prompt exceeds max length"))))))))

(defn- reset!*
  [{:keys [author session-id evidence-id]}]
  (when (and author session-id (not (contains? @!reset session-id)))
    (swap! !reset conj session-id)
    (let [r (reg/reset-session! author)]
      (reset! !last {:agent author :session session-id :result (select-keys r [:ok :old-session-id])})
      (try
        (boundary/append! store/!store
                          {:subject {:ref/type :agent :ref/id author}
                           :type :coordination :claim-type :observation
                           :author "refusal-reset" :session-id session-id
                           :in-reply-to evidence-id
                           :tags [:seat-session-reset :zai-1261]
                           :body {:event "seat-session-reset" :agent author
                                  :reason "provider refused the prompt as too long (1261)"
                                  :old-session-id (:old-session-id r) :ok (boolean (:ok r))}})
        (catch Throwable _ nil))
      r)))

(defn on-turn-final
  [{:keys [kind entry] :as ev}]
  (when (= :harness-turn kind)
    (let [body (or (:evidence/body entry) (:body entry))]
      (when (refusal? (body-field body :text))
        (reset!* ev)))))

(turn-events/add-listener! ::reset :post on-turn-final)
