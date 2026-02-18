(ns scripts.discipline-live-gate
  "Live discipline gate harness.

   Validates a transport-backed discipline round-trip with saved evidence:
   1) WS readiness handshake and action receipt for :discipline routing.
   2) Real-backend PSR->PUR->PAR execution in :discipline.
   3) Explicit hop from :discipline to :reflect with session continuity.
   4) Evidence/thread checks + artifact emission for scorecard updates."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.agency.registry :as reg]
            [futon3c.evidence.threads :as threads]
            [futon3c.peripheral.registry :as preg]
            [futon3c.peripheral.tools :as tools]
            [futon3c.runtime.agents :as runtime]
            [futon3c.social.persist :as persist]
            [futon3c.transport.http :as http]
            [futon3c.transport.ws :as ws]
            [futon3b.query.relations :as relations])
  (:import [java.net ServerSocket URI]
           [java.net.http HttpClient WebSocket WebSocket$Listener]
           [java.time Instant]
           [java.util UUID]
           [java.util.concurrent CompletableFuture]))

(defn- now-str [] (str (Instant/now)))

(defn- env [k default]
  (or (System/getenv k) default))

(defn- parse-int [s default]
  (try (Integer/parseInt (str s)) (catch Exception _ default)))

(defn- free-port []
  (with-open [ss (ServerSocket. 0)]
    (.getLocalPort ss)))

(defn- ensure! [pred message data]
  (when-not pred
    (throw (ex-info message data))))

(defn- wait-until [pred timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (cond
        (pred) true
        (> (System/currentTimeMillis) deadline) false
        :else (do (Thread/sleep 50) (recur))))))

(defn- find-frame [frames pred]
  (some (fn [f]
          (when (pred f) f))
        (map #(json/parse-string % true) @frames)))

(defn- wait-for-frame [frames pred timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (if-let [f (find-frame frames pred)]
        f
        (if (> (System/currentTimeMillis) deadline)
          nil
          (do (Thread/sleep 50) (recur)))))))

(defn- mk-listener [frames errors opened-p closed-p]
  (reify WebSocket$Listener
    (onOpen [_ web-socket]
      (deliver opened-p web-socket)
      (.request web-socket 1))
    (onText [_ web-socket data _last]
      (swap! frames conj (str data))
      (.request web-socket 1)
      (CompletableFuture/completedFuture nil))
    (onClose [_ _web-socket _status-code _reason]
      (deliver closed-p true)
      (CompletableFuture/completedFuture nil))
    (onError [_ _web-socket error]
      (swap! errors conj (str error)))))

(defn- start-ws-client! [client url]
  (let [frames (atom [])
        errors (atom [])
        opened-p (promise)
        closed-p (promise)
        ws-client (.join (.buildAsync (.newWebSocketBuilder client)
                                      (URI/create url)
                                      (mk-listener frames errors opened-p closed-p)))]
    {:ws ws-client
     :frames frames
     :errors errors
     :opened opened-p
     :closed closed-p
     :url url}))

(defn- send-json! [^WebSocket web-socket payload]
  (.join (.sendText web-socket (json/generate-string payload) true)))

(defn- close-ws! [{:keys [ws closed]}]
  (when ws
    (.join (.sendClose ws WebSocket/NORMAL_CLOSURE "done"))
    (deref closed 1500 nil)))

(defn- choose-pattern-id [search-result]
  (let [cands (get-in search-result [:result :candidates])
        with-library (first (filter :exists-in-library? cands))]
    (or (:pattern-id with-library)
        (:pattern-id (first cands)))))

(defn- artifact-file-path [artifact-dir]
  (let [stamp (-> (now-str)
                  (str/replace #":" "-")
                  (str/replace #"\." "-"))
        fname (str "discipline-live-gate-" stamp ".edn")]
    (.getPath (io/file artifact-dir fname))))

(defn- delete-tree! [^java.io.File dir]
  (when (.exists dir)
    (doseq [f (reverse (file-seq dir))]
      (try
        (.delete ^java.io.File f)
        (catch Exception _)))))

(defn- run-gate! []
  (let [host (env "FUTON3C_HOST" "127.0.0.1")
        port (if-let [p (System/getenv "FUTON3C_PORT")]
               (parse-int p 7070)
               (free-port))
        wait-ms (parse-int (env "FUTON3C_WAIT_MS" "6000") 6000)
        cwd (.getCanonicalPath (io/file (env "FUTON3C_CWD" (System/getProperty "user.dir"))))
        artifact-dir (io/file cwd "holes/qa")
        proof-dir (io/file (System/getProperty "java.io.tmpdir")
                           (str "futon3c-discipline-live-" (UUID/randomUUID)))
        codex-id "codex-1"
        claude-id "claude-1"
        ws-base (str "ws://" host ":" port)
        codex-url (str ws-base "/agency/ws?agent-id=" codex-id)
        claude-url (str ws-base "/agency/ws?agent-id=" claude-id)
        evidence-store (atom {:entries {} :order []})
        discipline-state (atom {:psr/by-pattern {}
                                :pur/history []
                                :pivot/history []
                                :par/history []})
        periph-config (runtime/make-default-peripheral-config
                       {:cwd cwd
                        :evidence-store evidence-store
                        :discipline-state discipline-state})]
    (.mkdirs artifact-dir)
    (.mkdirs proof-dir)
    (reg/reset-registry!)
    (persist/reset-sessions!)
    (runtime/register-codex!
      {:agent-id codex-id
       :invoke-fn (fn [prompt prior-session-id]
                    {:result (str "codex-ok:" prompt)
                     :session-id prior-session-id
                     :exit-code 0})})
    (runtime/register-claude!
      {:agent-id claude-id
       :invoke-fn (fn [prompt prior-session-id]
                    {:result (str "claude-ok:" prompt)
                     :session-id prior-session-id
                     :exit-code 0})})
    (let [opts {:patterns {:patterns/ids [:alleycat/discipline-live]}
                :peripheral-config periph-config}
          http-handler (runtime/make-http-handler opts)
          {:keys [handler connections]} (runtime/make-ws-handler opts)
          app (fn [req]
                (cond
                  (and (= :get (:request-method req))
                       (= "/agency/connected" (:uri req)))
                  {:status 200
                   :headers {"Content-Type" "application/json"}
                   :body (json/generate-string
                          {"connected_agents" (ws/connected-agents connections)
                           "count" (count (ws/connected-agents connections))})}
                  (= "/agency/ws" (:uri req))
                  (handler req)
                  :else
                  (http-handler req)))
          server (http/start-server! app port)
          client (HttpClient/newHttpClient)
          codex (start-ws-client! client codex-url)
          claude (start-ws-client! client claude-url)]
      (try
        (ensure! (some? (deref (:opened codex) 2000 nil))
                 "Codex WS open timeout"
                 {:url (:url codex) :errors @(:errors codex)})
        (ensure! (some? (deref (:opened claude) 2000 nil))
                 "Claude WS open timeout"
                 {:url (:url claude) :errors @(:errors claude)})
        (send-json! (:ws codex) {"type" "ready" "agent_id" codex-id})
        (send-json! (:ws claude) {"type" "ready" "agent_id" claude-id})
        (ensure! (some? (wait-for-frame (:frames codex) #(= "ready_ack" (:type %)) wait-ms))
                 "Missing codex ready_ack"
                 {:frames @(:frames codex) :errors @(:errors codex)})
        (ensure! (some? (wait-for-frame (:frames claude) #(= "ready_ack" (:type %)) wait-ms))
                 "Missing claude ready_ack"
                 {:frames @(:frames claude) :errors @(:errors claude)})
        (ensure! (wait-until #(= #{codex-id claude-id}
                                 (set (ws/connected-agents connections)))
                             wait-ms)
                 "Expected both agents connected"
                 {:connected (ws/connected-agents connections)})

        (let [msg-id (str "discipline-live-" (UUID/randomUUID))]
          (send-json! (:ws codex)
                      {"type" "message"
                       "msg_id" msg-id
                       "payload" {"peripheral" "discipline"
                                  "intent" "discipline-live-gate"}
                       "to" codex-id})
          (let [receipt (wait-for-frame
                         (:frames codex)
                         #(and (= "receipt" (:type %))
                               (= msg-id (:msg_id %)))
                         wait-ms)]
            (ensure! (some? receipt)
                     "Did not receive codex discipline receipt"
                     {:frames @(:frames codex)
                      :errors @(:errors codex)})
            (ensure! (= "peripheral/run-chain" (:route receipt))
                     "Unexpected dispatch route"
                     {:receipt receipt})
            (ensure! (= "discipline" (:peripheral_id receipt))
                     "Codex message did not route to discipline"
                     {:receipt receipt})

            (let [session-id (:session_id receipt)
                  backend (:backend periph-config)
                  peripherals (:peripherals periph-config)
                  {:keys [pattern-id expected-pattern chain]}
                  (with-redefs [relations/proof-path-dir (fn [] (.getPath proof-dir))]
                    (let [search-result (tools/execute-tool backend :psr-search
                                                            ["mandatory psr" {:top-k 8}])
                          _ (ensure! (true? (:ok search-result))
                                     "psr-search failed"
                                     {:search-result search-result})
                          pattern-id (choose-pattern-id search-result)
                          _ (ensure! (string? pattern-id)
                                     "No candidate pattern-id available from psr-search"
                                     {:search-result search-result})
                          expected-pattern (keyword pattern-id)
                          chain (preg/run-chain
                                 {:backend backend
                                  :peripherals peripherals
                                  :evidence-store evidence-store}
                                 {:session-id session-id
                                  :agent-id codex-id}
                                 [{:peripheral-id :discipline
                                   :actions [{:tool :psr-search
                                              :args ["mandatory psr" {:top-k 8}]}
                                             {:tool :psr-select
                                              :args [pattern-id
                                                     {:task-id "task-discipline-live"
                                                      :rationale "alleycat live discipline gate"}]}
                                             {:tool :pur-update
                                              :args [pattern-id
                                                     :ok
                                                     {:criteria-eval {:gate :discipline-live
                                                                      :transport :websocket}
                                                      :prediction-error {:delta 0.1}}]}
                                             {:tool :par-punctuate
                                              :args [{:session-id session-id
                                                      :what-worked "Discipline PSR/PUR round-trip completed"
                                                      :suggestions ["next-hop: reflect"]}]}]
                                   :stop-reason "par-generated"
                                   :exit-condition :par-generated}
                                  {:peripheral-id :reflect
                                   :actions []
                                   :stop-reason "par-generated"
                                   :exit-condition :par-generated}])]
                      {:search-result search-result
                       :pattern-id pattern-id
                       :expected-pattern expected-pattern
                       :chain chain}))
                  _ (ensure! (true? (:ok chain))
                             "Discipline->reflect chain failed"
                             {:chain chain})
                  _ (ensure! (= 2 (count (:fruits chain)))
                             "Expected two fruits (:discipline then :reflect)"
                             {:fruits (:fruits chain)})
                  _ (ensure! (= session-id (get-in chain [:final-context :session-id]))
                             "Session continuity mismatch after hop"
                             {:session-id session-id
                              :final-context (:final-context chain)})
                  discipline-fruit (first (:fruits chain))
                  _ (ensure! (= expected-pattern
                                (:selected-pattern discipline-fruit))
                             "Discipline fruit selected-pattern mismatch"
                             {:expected expected-pattern
                              :fruit discipline-fruit})
                  entries (->> (vals (:entries @evidence-store))
                               (filter #(= session-id (:evidence/session-id %)))
                               (sort-by :evidence/at)
                               vec)
                  has-selection? (some #(and (= :pattern-selection (:evidence/type %))
                                             (= expected-pattern (:evidence/pattern-id %)))
                                       entries)
                  has-outcome? (some #(and (= :pattern-outcome (:evidence/type %))
                                           (= expected-pattern (:evidence/pattern-id %)))
                                     entries)
                  has-reflection? (some #(= :reflection (:evidence/type %)) entries)
                  _ (ensure! has-selection?
                             "Missing pattern-selection evidence for chosen pattern"
                             {:pattern-id pattern-id})
                  _ (ensure! has-outcome?
                             "Missing pattern-outcome evidence for chosen pattern"
                             {:pattern-id pattern-id})
                  _ (ensure! has-reflection?
                             "Missing reflection evidence"
                             {:session-id session-id})
                  thread (threads/project-thread evidence-store
                                                {:ref/type :pattern
                                                 :ref/id pattern-id})
                  _ (ensure! (map? thread)
                             "Pattern thread projection missing"
                             {:pattern-id pattern-id})
                  _ (ensure! (>= (:thread/entry-count thread) 2)
                             "Pattern thread is too small"
                             {:entry-count (:thread/entry-count thread)})
                  type-counts (->> entries
                                   (map :evidence/type)
                                   frequencies
                                   (into (sorted-map)))
                  artifact {:gate/id :discipline-live
                            :status :pass
                            :ran-at (now-str)
                            :host host
                            :port port
                            :cwd cwd
                            :session-id session-id
                            :receipt receipt
                            :query "mandatory psr"
                            :pattern-id pattern-id
                            :chain {:ok (:ok chain)
                                    :fruits (count (:fruits chain))
                                    :final-context (:final-context chain)}
                            :evidence {:entry-count (count entries)
                                       :type-counts type-counts
                                       :thread-entry-count (:thread/entry-count thread)}
                            :checks {:ws-ready true
                                     :discipline-route true
                                     :psr-pur-roundtrip true
                                     :explicit-hop true
                                     :session-continuity true}}
                  artifact-path (artifact-file-path artifact-dir)]
              (spit artifact-path (pr-str artifact))
              (println "PASS: discipline live gate complete")
              (println (str "ARTIFACT_PATH=" artifact-path)))))
        (finally
          (try (close-ws! codex) (catch Exception _))
          (try (close-ws! claude) (catch Exception _))
          (delete-tree! proof-dir)
          ((:server server)))))))

(run-gate!)
