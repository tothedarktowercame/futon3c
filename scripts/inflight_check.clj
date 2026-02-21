(ns scripts.inflight-check
  "Inflight readiness checks for a running futon3c dev server.

   Probes the live system to verify all subsystems are working:
   1. HTTP API health (agents list, evidence query)
   2. Agent registration (claude-1, codex-1 present)
   3. IRC connectivity (connect, register nick, join #futon)
   4. Agent presence in IRC (virtual nicks visible)
   5. IRC relay round-trip (send message → agent responds)
   6. Evidence emission (new entries appear after relay)

   Usage:
     clojure -M scripts/inflight_check.clj
     clojure -M scripts/inflight_check.clj --skip-relay   # skip agent invoke (fast)
     clojure -M scripts/inflight_check.clj --http-only    # skip IRC entirely

   Expects dev server already running on:
     HTTP: localhost:7070 (or FUTON3C_PORT)
     IRC:  localhost:6667 (or FUTON3C_IRC_PORT)

   Non-destructive: only reads state and sends one test message."
  (:require [cheshire.core :as json]
            [clojure.string :as str])
  (:import [java.io BufferedReader BufferedWriter InputStreamReader OutputStreamWriter]
           [java.net Socket HttpURLConnection URL]
           [java.time Instant]))

;; =============================================================================
;; Configuration
;; =============================================================================

(defn- env [k default]
  (or (System/getenv k) default))

(defn- parse-int [s default]
  (try (Integer/parseInt (str s)) (catch Exception _ default)))

(def ^:dynamic *http-port* (parse-int (env "FUTON3C_PORT" "7070") 7070))
(def ^:dynamic *irc-port* (parse-int (env "FUTON3C_IRC_PORT" "6667") 6667))
(def ^:dynamic *host* "localhost")

;; =============================================================================
;; Result tracking
;; =============================================================================

(def !results (atom []))

(defn- check! [name status detail]
  (let [icon (case status :pass "PASS" :fail "FAIL" :skip "SKIP" "????")]
    (swap! !results conj {:name name :status status :detail detail})
    (println (str "  [" icon "] " name
                  (when detail (str " — " detail))))
    (flush)))

;; =============================================================================
;; HTTP helpers
;; =============================================================================

(defn- http-get [path]
  (try
    (let [url (URL. (str "http://" *host* ":" *http-port* path))
          conn (doto (.openConnection url)
                 (.setRequestMethod "GET")
                 (.setConnectTimeout 5000)
                 (.setReadTimeout 10000))
          status (.getResponseCode conn)
          body (try
                 (slurp (.getInputStream conn))
                 (catch Exception _
                   (try (slurp (.getErrorStream conn))
                        (catch Exception _ ""))))]
      {:status status :body body
       :json (try (json/parse-string body true) (catch Exception _ nil))})
    (catch Exception e
      {:status -1 :error (.getMessage e)})))

(defn- http-post [path body-map]
  (try
    (let [url (URL. (str "http://" *host* ":" *http-port* path))
          body-str (json/generate-string body-map)
          conn (doto ^HttpURLConnection (.openConnection url)
                 (.setRequestMethod "POST")
                 (.setDoOutput true)
                 (.setConnectTimeout 5000)
                 (.setReadTimeout 60000)
                 (.setRequestProperty "Content-Type" "application/json"))]
      (with-open [os (.getOutputStream conn)]
        (.write os (.getBytes body-str "UTF-8")))
      (let [status (.getResponseCode conn)
            resp-body (try
                        (slurp (.getInputStream conn))
                        (catch Exception _
                          (try (slurp (.getErrorStream conn))
                               (catch Exception _ ""))))]
        {:status status :body resp-body
         :json (try (json/parse-string resp-body true) (catch Exception _ nil))}))
    (catch Exception e
      {:status -1 :error (.getMessage e)})))

;; =============================================================================
;; IRC helpers
;; =============================================================================

(defn- irc-connect!
  "Connect to IRC, register nick, return {:socket :reader :writer :lines}."
  [nick]
  (let [sock (doto (Socket.)
               (.connect (java.net.InetSocketAddress. *host* *irc-port*) 5000)
               (.setSoTimeout 10000))
        reader (BufferedReader. (InputStreamReader. (.getInputStream sock)))
        writer (BufferedWriter. (OutputStreamWriter. (.getOutputStream sock)))
        lines (atom [])
        send! (fn [line]
                (.write writer (str line "\r\n"))
                (.flush writer))]
    (send! (str "NICK " nick))
    (send! (str "USER " nick " 0 * :Inflight check"))
    ;; Read until we get 001 (welcome) or timeout
    (loop [i 0]
      (when (< i 50)
        (let [line (try (.readLine reader) (catch Exception _ nil))]
          (when line
            (swap! lines conj line)
            (when (str/includes? line "PING")
              (send! (str "PONG " (subs line (+ 1 (.indexOf ^String line ":"))))))
            (if (str/includes? line " 001 ")
              nil ;; registered!
              (recur (inc i)))))))
    {:socket sock :reader reader :writer writer :lines lines
     :send! send!
     :read-line! (fn []
                   (try
                     (let [line (.readLine reader)]
                       (when line (swap! lines conj line))
                       line)
                     (catch Exception _ nil)))
     :close! (fn []
               (try (.close sock) (catch Exception _)))}))

(defn- irc-read-until
  "Read IRC lines until pred matches or timeout."
  [{:keys [read-line!]} pred timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (if (> (System/currentTimeMillis) deadline)
        nil
        (let [line (read-line!)]
          (cond
            (nil? line) nil
            (pred line) line
            :else (recur)))))))

;; =============================================================================
;; Check implementations
;; =============================================================================

(defn check-http-health! []
  (println "\n--- HTTP API ---")
  (let [resp (http-get "/api/alpha/agents")]
    (if (= 200 (:status resp))
      (let [data (:json resp)
            ;; API returns {:ok true :count N :agents {"claude-1" {...} ...}}
            agents-map (:agents data)
            agent-ids (cond
                        (and (map? agents-map) (not (sequential? agents-map)))
                        (vec (keys agents-map))
                        (sequential? agents-map)
                        (mapv #(or (:agent-id %) (name (key %))) agents-map)
                        :else [])]
        (check! "HTTP /api/alpha/agents responds" :pass
                (str "status=" (:status resp)))
        (let [id-strs (mapv #(if (keyword? %) (name %) (str %)) agent-ids)]
          (if (some #(= "claude-1" %) id-strs)
            (check! "claude-1 registered" :pass nil)
            (check! "claude-1 registered" :fail
                    (str "agents: " id-strs)))
          (if (some #(= "codex-1" %) id-strs)
            (check! "codex-1 registered" :pass nil)
            (check! "codex-1 registered" :skip
                    "not registered (may not be configured)"))))
      (do
        (check! "HTTP /api/alpha/agents responds" :fail
                (str "status=" (:status resp)
                     (when (:error resp) (str " error=" (:error resp)))))))))

(defn check-evidence-api! []
  (let [resp (http-get "/api/alpha/evidence?limit=3")]
    (if (= 200 (:status resp))
      (let [data (:json resp)
            entries (or (:entries data) data)
            cnt (if (sequential? entries) (count entries) 0)]
        (check! "Evidence API responds" :pass
                (str cnt " entries returned")))
      (check! "Evidence API responds" :fail
              (str "status=" (:status resp))))))

(defn check-irc-connect! []
  (println "\n--- IRC ---")
  (try
    (let [conn (irc-connect! "inflight-check")
          registered? (some #(str/includes? % " 001 ") @(:lines conn))]
      (if registered?
        (check! "IRC connect + register nick" :pass nil)
        (check! "IRC connect + register nick" :fail
                (str "lines: " (take 3 @(:lines conn)))))

      ;; JOIN #futon
      ((:send! conn) "JOIN #futon")
      (Thread/sleep 500)

      ;; Read until we get 353 (NAMES) or 366 (end of NAMES)
      (let [names-line (irc-read-until conn
                         #(or (str/includes? % " 353 ")
                              (str/includes? % " 366 "))
                         3000)
            all-lines (str/join " " @(:lines conn))]
        (if (str/includes? all-lines " 353 ")
          (do
            (check! "JOIN #futon + NAMES reply" :pass nil)
            ;; Check for agent nicks in NAMES
            (if (str/includes? all-lines "claude")
              (check! "claude nick visible in #futon" :pass nil)
              (check! "claude nick visible in #futon" :fail
                      "not in NAMES reply")))
          (check! "JOIN #futon + NAMES reply" :fail
                  (str "no 353 received"))))

      ;; Clean up
      ((:send! conn) "QUIT :inflight check done")
      ((:close! conn))
      conn)
    (catch Exception e
      (check! "IRC connect" :fail (.getMessage e))
      nil)))

(defn check-irc-relay! []
  (println "\n--- IRC Relay ---")
  (try
    (let [conn (irc-connect! "inflight-relay")
          _ ((:send! conn) "JOIN #futon")
          _ (Thread/sleep 500)
          ;; Drain join messages
          _ (irc-read-until conn #(str/includes? % " 366 ") 3000)
          ;; Send test message
          test-msg (str "@claude inflight check " (Instant/now) " — reply with OK")
          _ ((:send! conn) (str "PRIVMSG #futon :" test-msg))
          ;; Wait for response (agent PRIVMSG back to channel)
          response (irc-read-until conn
                     #(and (str/includes? % "PRIVMSG")
                           (not (str/includes? % "inflight-relay")))
                     45000)]
      (if response
        (check! "IRC relay round-trip" :pass
                (str (subs response 0 (min 80 (count response))) "..."))
        (check! "IRC relay round-trip" :fail
                "no agent response within 45s"))
      ((:send! conn) "QUIT :done")
      ((:close! conn)))
    (catch Exception e
      (check! "IRC relay round-trip" :fail (.getMessage e)))))

(defn check-evidence-after-relay! []
  (let [resp (http-get "/api/alpha/evidence?limit=5&sort=desc")]
    (if (= 200 (:status resp))
      (let [data (:json resp)
            entries (or (:entries data) data)
            recent-types (when (sequential? entries)
                           (mapv #(or (:evidence/type %) (:type %)) entries))]
        (check! "Evidence emitted after relay" :pass
                (str "recent types: " recent-types)))
      (check! "Evidence emitted after relay" :fail
              (str "status=" (:status resp))))))

;; =============================================================================
;; Main
;; =============================================================================

(defn -main [& args]
  (let [args-set (set args)
        skip-relay? (or (contains? args-set "--skip-relay")
                        (contains? args-set "--http-only"))
        http-only? (contains? args-set "--http-only")]

    (println (str "Inflight check — " (Instant/now)))
    (println (str "  HTTP: " *host* ":" *http-port*))
    (when-not http-only?
      (println (str "  IRC:  " *host* ":" *irc-port*)))

    ;; HTTP checks
    (check-http-health!)
    (check-evidence-api!)

    ;; IRC checks
    (when-not http-only?
      (check-irc-connect!)
      (when-not skip-relay?
        (check-irc-relay!)
        (check-evidence-after-relay!)))

    ;; Summary
    (println "\n--- Summary ---")
    (let [results @!results
          pass (count (filter #(= :pass (:status %)) results))
          fail (count (filter #(= :fail (:status %)) results))
          skip (count (filter #(= :skip (:status %)) results))
          total (count results)]
      (println (str "  " pass "/" total " passed"
                    (when (pos? fail) (str ", " fail " FAILED"))
                    (when (pos? skip) (str ", " skip " skipped"))))
      (when (pos? fail)
        (println "\n  Failed checks:")
        (doseq [{:keys [name detail]} (filter #(= :fail (:status %)) results)]
          (println (str "    - " name (when detail (str ": " detail))))))
      (System/exit (if (zero? fail) 0 1)))))

(apply -main *command-line-args*)
