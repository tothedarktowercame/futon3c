(ns futon3c.agency.fed-uplink
  "Outbound WebSocket federation uplink for NAT'd Agency nodes.

   A client dials a hub over WS, announces its local roster, imports the hub's
   pushed roster, and services hub-originated invokes over the same socket."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [futon3c.agency.federation :as federation]
            [futon3c.agency.registry :as reg]
            [futon3c.transport.protocol :as proto])
  (:import [java.net URI]
           [java.net.http HttpClient WebSocket WebSocket$Listener]
           [java.time Instant]
           [java.util.concurrent Executors ExecutorService TimeUnit]))

(def ^:private default-uplink-interval-ms 30000)
(def ^:private default-max-backoff-ms 60000)

(defonce ^:private !state
  (atom {:running? false
         :site nil
         :url nil
         :connected? false
         :last-connect-at nil
         :last-announce-at nil
         :last-error nil
         :announce-count 0
         :import-results []
         :socket nil
         :runner nil
         :executor nil
         :pending (atom {})}))

(declare stop-uplink! uplink-status)

(defn- parse-long*
  [x]
  (try
    (when (some? x)
      (Long/parseLong (str/trim (str x))))
    (catch Exception _ nil)))

(defn- env-or-prop
  [k]
  (or (System/getProperty k)
      (System/getenv k)))

(defn- interval-ms
  []
  (long (or (parse-long* (env-or-prop "FUTON3C_FED_SYNC_INTERVAL_MS"))
            default-uplink-interval-ms)))

(defn- max-backoff-ms
  []
  (long (or (parse-long* (env-or-prop "FUTON3C_FED_SYNC_MAX_BACKOFF_MS"))
            default-max-backoff-ms)))

(defn- parse-uplink
  [raw]
  (when (and (string? raw) (not (str/blank? raw)))
    (if-let [[_ site url] (re-matches #"([A-Za-z][A-Za-z0-9]*)=(.+)" (str/trim raw))]
      {:site (str/lower-case site)
       :url (str/trim url)}
      {:url (str/trim raw)})))

(defn- token []
  (env-or-prop "FUTON3C_FED_TOKEN"))

(defn- invoke-error
  [code message]
  {:error/component :fed-uplink
   :error/code code
   :error/message message
   :error/at (str (Instant/now))})

(defn- send-text!
  [^WebSocket socket text]
  (when socket
    (.sendText socket text true)))

(defn- send-frame!
  [frame]
  (when-let [socket (:socket @!state)]
    (send-text! socket (json/generate-string frame))))

(defn announce! []
  (let [{:keys [site socket]} @!state
        roster (federation/export-roster {:include-proxies? false})
        tok (token)]
    (when (and socket site tok)
      (send-text! socket (proto/render-fed-announce site tok roster))
      (swap! !state
             (fn [s]
               (-> s
                   (update :announce-count (fnil inc 0))
                   (assoc :last-announce-at (str (Instant/now)))))))))

(defn- handle-fed-roster!
  [parsed]
  (let [site (:fed/site parsed)
        pending (:pending @!state)
        origin (str "ws-uplink://" site)
        results (federation/import-uplink-roster!
                 origin
                 (:fed/roster parsed)
                 {:transport :ws-uplink
                  :uplink-site site
                  :invoke-fn-fn
                  (fn [remote-id]
                    (federation/make-uplink-invoke-fn
                     (fn [frame] (send-text! (:socket @!state)
                                             (proto/render-fed-invoke frame)))
                     pending
                     remote-id))})]
    (swap! !state assoc :import-results results)))

(defn- handle-fed-invoke!
  [^ExecutorService executor parsed]
  (.submit executor
           ^Runnable
           (fn []
             (let [invoke-id (:fed/invoke-id parsed)
                   agent-id (:fed/agent-id parsed)
                   timeout-ms (long (or (:fed/timeout-ms parsed) 600000))
                   result (try
                            (reg/invoke-agent! agent-id
                                               (:fed/prompt parsed)
                                               timeout-ms)
                            (catch Throwable t
                              (invoke-error :invoke-failed (.getMessage t))))
                   ok? (not (contains? result :error/code))
                   frame (cond-> {:type "fed_invoke_result"
                                  :invoke_id invoke-id
                                  :ok ok?}
                           ok? (assoc :result (:result result)
                                      :session_id (:session-id result))
                           (not ok?) (assoc :error (or (:error/message result)
                                                       (:error result)
                                                       (str result))))]
               (send-frame! frame)))))

(defn- handle-text!
  [executor text]
  (let [parsed (proto/parse-ws-message text)]
    (when-not (contains? parsed :error/code)
      (case (:ws/type parsed)
        :fed-roster (handle-fed-roster! parsed)
        :fed-invoke (handle-fed-invoke! executor parsed)
        :fed-invoke-result (federation/resolve-uplink-invoke!
                            (:pending @!state)
                            (:fed/invoke-id parsed)
                            {:ok (:fed/ok parsed)
                             :result (:fed/result parsed)
                             :session-id (:fed/session-id parsed)
                             :error (:fed/error parsed)})
        nil))))

(defn- listener
  [executor]
  (let [buf (StringBuilder.)]
    (reify WebSocket$Listener
      (onOpen [_ ws]
        (.request ws 1)
        (swap! !state assoc
               :socket ws
               :connected? true
               :last-connect-at (str (Instant/now))
               :last-error nil)
        (announce!))
      (onText [_ ws data last?]
        (.append buf data)
        (when last?
          (let [text (str buf)]
            (.setLength buf 0)
            (handle-text! executor text)))
        (.request ws 1)
        nil)
      (onError [_ _ws error]
        (swap! !state assoc
               :connected? false
               :last-error (.getMessage ^Throwable error)))
      (onClose [_ _ws _status reason]
        (swap! !state assoc
               :connected? false
               :socket nil
               :last-error reason)
        nil))))

(defn- backoff-delay-ms
  [failure-count]
  (min (max-backoff-ms)
       (* 1000 (long (Math/pow 2 (max 0 (dec failure-count)))))))

(defn- run-loop!
  [{:keys [site url]} executor]
  (let [client (HttpClient/newHttpClient)]
    (loop [failures 0
           last-announce 0]
      (when (:running? @!state)
        (try
          (when-not (:connected? @!state)
            (-> client
                (.newWebSocketBuilder)
                (.buildAsync (URI/create url) (listener executor))
                (.join))
            (recur 0 (System/currentTimeMillis)))
          (let [now (System/currentTimeMillis)
                interval (interval-ms)]
            (when (>= (- now last-announce) interval)
              (announce!))
            (Thread/sleep (min interval 1000))
            (recur 0 now))
          (catch Throwable t
            (swap! !state assoc
                   :connected? false
                   :socket nil
                   :last-error (.getMessage t))
            (Thread/sleep (backoff-delay-ms (inc failures)))
            (recur (inc failures) last-announce)))))))

(defn start-uplink!
  "Start the outbound federation uplink from FUTON3C_FED_UPLINK.
   FUTON3C_FED_UPLINK shape: <site>=<ws-url>."
  ([] (start-uplink! {}))
  ([{:keys [uplink]}]
   (let [{:keys [site url] :as cfg} (parse-uplink (or uplink
                                                      (env-or-prop "FUTON3C_FED_UPLINK")))
         executor (Executors/newCachedThreadPool)]
     (cond
       (or (nil? cfg) (str/blank? url))
       (swap! !state assoc :running? false :last-error "FUTON3C_FED_UPLINK unset")

       (str/blank? site)
       (swap! !state assoc :running? false :last-error "FUTON3C_FED_UPLINK missing site prefix")

       (str/blank? (token))
       (swap! !state assoc :running? false :last-error "FUTON3C_FED_TOKEN unset")

       :else
       (do
         (stop-uplink!)
         (let [runner (Thread. #(run-loop! {:site site :url url} executor)
                               "futon3c-fed-uplink")]
           (.setDaemon runner true)
           (swap! !state assoc
                  :running? true
                  :site site
                  :url url
                  :executor executor
                  :runner runner
                  :pending (atom {})
                  :last-error nil)
           (.start runner)
           (uplink-status)))))))

(defn stop-uplink! []
  (let [{:keys [^WebSocket socket ^Thread runner ^ExecutorService executor]} @!state]
    (swap! !state assoc :running? false :connected? false :socket nil :runner nil :executor nil)
    (when socket
      (try (.sendClose socket WebSocket/NORMAL_CLOSURE "stop-uplink") (catch Throwable _)))
    (when runner
      (try (.interrupt runner) (catch Throwable _)))
    (when executor
      (.shutdownNow executor)
      (.awaitTermination executor 2 TimeUnit/SECONDS))
    (uplink-status)))

(defn uplink-status []
  (let [s @!state]
    (-> s
        (dissoc :socket :runner :executor :pending)
        (assoc :pending-count (count @(:pending s))))))
