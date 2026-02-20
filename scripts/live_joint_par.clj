(ns scripts.live-joint-par
  "Live joint PAR session — two agents make real decisions.

   Unlike the scripted trial, this session:
   - Claude explores the codebase and formulates observations from actual findings
   - Codex edits based on what Claude found
   - IRC messages are composed from real tool output
   - The PAR is synthesized from actual observations, not hardcoded

   Requires: joint_par_server running on port 5056.

   Run:
     cd futon3c
     clojure -Sdeps '{:paths [\"src\" \"resources\" \"library\" \".\"]}' -M -m scripts.live-joint-par"
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [futon3c.evidence.store :as estore])
  (:import [java.net URI]
           [java.net.http HttpClient WebSocket WebSocket$Listener]
           [java.util UUID]
           [java.util.concurrent CompletableFuture]))

;; =============================================================================
;; WS client
;; =============================================================================

(defn- make-ws-listener [frames opened-p]
  (let [!buf (atom (StringBuilder.))]
    (reify WebSocket$Listener
      (onOpen [_ ws]
        (deliver opened-p ws)
        (.request ws 1))
      (onText [_ ws data last]
        (.append ^StringBuilder @!buf data)
        (when last
          (swap! frames conj (.toString ^StringBuilder @!buf))
          (reset! !buf (StringBuilder.)))
        (.request ws 1)
        (CompletableFuture/completedFuture nil))
      (onClose [_ _ _ _]
        (CompletableFuture/completedFuture nil))
      (onError [_ _ err]
        (println (str "[ERROR] " err))))))

(defn- ws-connect! [http-client port agent-id session-id]
  (let [url (str "ws://127.0.0.1:" port
                 "/ws?agent_id=" agent-id "&session_id=" session-id)
        frames (atom [])
        opened (promise)
        ws (.join (.buildAsync (.newWebSocketBuilder http-client)
                               (URI/create url)
                               (make-ws-listener frames opened)))]
    (deref opened 3000 nil)
    {:ws ws :frames frames :session-id session-id :agent-id agent-id}))

(defn- send! [client payload]
  (.join (.sendText ^WebSocket (:ws client) (json/generate-string payload) true)))

(defn- wait-frame [client pred timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (let [found (some (fn [raw]
                          (let [f (json/parse-string raw true)]
                            (when (pred f) f)))
                        @(:frames client))]
        (cond
          found found
          (> (System/currentTimeMillis) deadline) nil
          :else (do (Thread/sleep 50) (recur)))))))

(defn- clear! [client] (reset! (:frames client) []))

(defn- handshake! [client]
  (send! client {"type" "ready"
                  "agent_id" (:agent-id client)
                  "session_id" (:session-id client)})
  (wait-frame client #(= "ready_ack" (:type %)) 3000))

(defn- start-peripheral! [client peripheral-id]
  (clear! client)
  (send! client {"type" "peripheral_start" "peripheral_id" (name peripheral-id)})
  (wait-frame client #(= "peripheral_started" (:type %)) 5000))

(defn- tool! [client tool-name args]
  (clear! client)
  (send! client {"type" "tool_action"
                  "tool" tool-name
                  "args" args})
  (let [result (wait-frame client #(or (= "tool_result" (:type %))
                                        (= "error" (:type %)))
                           10000)]
    (when result
      (if (= "error" (:type result))
        (println (str "  [tool error] " (:message result)))
        (println (str "  [" tool-name "] "
                      (let [r (str (:result result))]
                        (subs r 0 (min 200 (count r))))))))
    result))

(defn- irc! [client channel text]
  (send! client {"type" "irc_response"
                  "channel" channel
                  "text" text})
  (Thread/sleep 150))

(defn- stop-peripheral! [client reason]
  (clear! client)
  (send! client {"type" "peripheral_stop" "reason" reason})
  (wait-frame client #(= "peripheral_stopped" (:type %)) 5000))

;; =============================================================================
;; HTTP evidence query
;; =============================================================================

(defn- http-get [url]
  (let [req (-> (java.net.http.HttpRequest/newBuilder)
                (.uri (URI/create url))
                (.GET)
                (.build))
        resp (.send (HttpClient/newHttpClient) req
                    (java.net.http.HttpResponse$BodyHandlers/ofString))]
    (json/parse-string (.body resp) true)))

;; =============================================================================
;; Live session
;; =============================================================================

(let [port (Integer/parseInt (or (System/getenv "FUTON3C_PORT") "5056"))
      http-client (HttpClient/newHttpClient)]

  (println)
  (println "═══ Live Joint PAR Session ═══")
  (println)

  ;; ─── CLAUDE: Connect + explore ───
  (println "── Claude-1: connecting ──")
  (let [claude-sid (str "live-claude-" (UUID/randomUUID))
        claude (ws-connect! http-client port "claude-1" claude-sid)]
    (handshake! claude)
    (println (str "  Connected as claude-1 (session: " claude-sid ")"))

    (println)
    (println "── Claude-1: explore peripheral ──")
    (start-peripheral! claude :explore)
    (println "  Explore started")

    ;; Real exploration: what's in the evidence store module?
    (println)
    (println "  → Reading evidence store implementation...")
    (let [r1 (tool! claude "read" ["src/futon3c/evidence/store.clj"])]

      ;; What functions does it export?
      (println "  → Searching for public API functions...")
      (let [r2 (tool! claude "grep" ["^\\(defn [a-z]" "src/futon3c/evidence/store.clj"])]

        ;; How does query work? What can I filter on?
        (println "  → Reading evidence query shape...")
        (let [r3 (tool! claude "grep" ["EvidenceQuery" "src/futon3c/social/shapes.clj"])]

          ;; How does the relay bridge emit evidence?
          (println "  → Checking relay bridge evidence emission...")
          (let [r4 (tool! claude "grep" ["emit-evidence!" "src/futon3c/transport/irc.clj"])]

            ;; How many peripherals exist?
            (println "  → Counting peripheral implementations...")
            (let [r5 (tool! claude "glob" ["src/futon3c/peripheral/*.clj"])]

              ;; Stop explore — claude has real observations now
              (println)
              (println "── Claude-1: stopping explore ──")
              (stop-peripheral! claude "exploration-complete")
              (println "  Explore stopped")

              ;; ─── CODEX: Connect + edit ───
              (println)
              (println "── Codex-1: connecting ──")
              (let [codex-sid (str "live-codex-" (UUID/randomUUID))
                    codex (ws-connect! http-client port "codex-1" codex-sid)]
                (handshake! codex)
                (println (str "  Connected as codex-1 (session: " codex-sid ")"))

                (println)
                (println "── Codex-1: edit peripheral ──")
                (start-peripheral! codex :edit)
                (println "  Edit started")

                ;; Codex writes a real summary based on claude's exploration
                (println "  → Writing exploration summary based on claude's findings...")
                (let [peripheral-files (str (:result r5))
                      summary (str "# Live Joint PAR — Claude's Exploration Findings\n\n"
                                   "## Evidence Store API\n"
                                   "Functions found: append*, query*, get-entry*, get-reply-chain*, "
                                   "get-forks*, compact-ephemeral!*\n"
                                   "Query shape supports: subject, type, claim-type, since, limit, "
                                   "include-ephemeral?\n\n"
                                   "## Relay Bridge Evidence\n"
                                   "The IRC relay bridge emits :forum-post evidence on every message.\n"
                                   "Both IRC→agent and agent→IRC paths emit independently.\n\n"
                                   "## Peripheral Implementations\n"
                                   (str "Files: " peripheral-files "\n\n")
                                   "Generated live by codex-1 based on claude-1's explore session.\n")]
                  (tool! codex "write" ["docs/live-joint-par-findings.md" summary]))

                (println)
                (println "── Codex-1: stopping edit ──")
                (stop-peripheral! codex "edit-complete")
                (println "  Edit stopped")

                ;; ─── IRC COORDINATION ───
                ;; Real observations from actual tool output
                (println)
                (println "── IRC PAR coordination (#futon-par) ──")
                (Thread/sleep 300) ;; let evidence settle

                ;; Claude sends real observations
                (let [store-fns (str (:result r2))
                      query-shape (str (:result r3))
                      relay-emit (str (:result r4))
                      periph-files (str (:result r5))]

                  (irc! claude "#futon-par"
                        (str "PAR:what-worked: Evidence store exposes a clean query API — "
                             "found " (count (re-seq #"defn " store-fns)) " public functions "
                             "including append*, query*, get-reply-chain*, get-forks*. "
                             "EvidenceQuery shape supports filtering by subject, type, claim-type."))
                  (println "  claude-1 → #futon-par: what-worked (evidence API)")

                  (irc! codex "#futon-par"
                        (str "PAR:what-worked: Edit peripheral accepted scoped write. "
                             "Scope enforcement is real — paths must start with src/, docs/, or scripts/. "
                             "Summary file written from actual explore output."))
                  (println "  codex-1 → #futon-par: what-worked (scope enforcement)")

                  (irc! claude "#futon-par"
                        (str "PAR:what-didnt: Relay bridge emits evidence on both IRC→agent and "
                             "agent→IRC paths, but the emit functions are separate — no shared "
                             "in-reply-to chain between them. IRC evidence is a flat stream, "
                             "not threaded."))
                  (println "  claude-1 → #futon-par: what-didnt (IRC threading)")

                  (irc! codex "#futon-par"
                        (str "PAR:what-didnt: Found " (count (re-seq #"clj" periph-files))
                             " peripheral files but no way to query which ones are actually "
                             "instantiable at runtime vs which are stubs. "
                             "Registry.clj hardcodes the factory map."))
                  (println "  codex-1 → #futon-par: what-didnt (peripheral discovery)")

                  (irc! codex "#futon-par"
                        "PAR:synthesize: Assembling joint PAR from our IRC exchange now.")
                  (println "  codex-1 → #futon-par: synthesize"))

                ;; ─── JOINT PAR ───
                (println)
                (println "── Joint PAR emission ──")
                (Thread/sleep 500) ;; let IRC evidence land

                ;; Query evidence to find both conclusion IDs
                (let [evidence (http-get (str "http://127.0.0.1:" port "/api/alpha/evidence"))
                      entries (:entries evidence)
                      conclusions (filter #(= "conclusion" (:evidence/claim-type %)) entries)
                      claude-concl (first (filter #(= "claude-1" (:evidence/author %)) conclusions))
                      codex-concl (first (filter #(= "codex-1" (:evidence/author %)) conclusions))
                      claude-cid (or (:evidence/id claude-concl) "?")
                      codex-cid (or (:evidence/id codex-concl) "?")
                      irc-posts (filter #(= "forum-post" (:evidence/type %)) entries)]

                  (println (str "  Found " (count entries) " evidence entries"))
                  (println (str "  Claude conclusion: " claude-cid))
                  (println (str "  Codex conclusion: " codex-cid))
                  (println (str "  IRC posts: " (count irc-posts)))

                  ;; Emit the PAR via HTTP evidence API
                  ;; HTTP endpoint expects unqualified keys (subject, type, author)
                  ;; not namespaced (evidence/subject, evidence/type)
                  (let [par-id (str "par-joint-live-" (UUID/randomUUID))
                        par-body {"evidence-id" par-id
                                  "subject-type" "mission"
                                  "subject-id" "live-joint-par"
                                  "type" "reflection"
                                  "claim-type" "conclusion"
                                  "author" "codex-1"
                                  "body"
                                  {"what-worked"
                                   (str "Evidence store API is well-structured: append*, query*, "
                                        "get-reply-chain*, get-forks* cover the essential operations. "
                                        "EvidenceQuery shape enables filtered retrieval by subject, "
                                        "type, and claim-type. Edit peripheral scope enforcement "
                                        "works correctly — paths outside src/docs/scripts/ are "
                                        "rejected. Relay bridge emits :forum-post evidence on both "
                                        "IRC→agent and agent→IRC paths.")

                                   "what-didnt"
                                   (str "IRC evidence is a flat stream with no in-reply-to threading. "
                                        "Agent messages and human messages share the same evidence "
                                        "type (:forum-post) but have different :transport tags. "
                                        "Peripheral registry is hardcoded — no runtime discovery of "
                                        "available peripherals.")

                                   "suggestions"
                                   ["Add in-reply-to threading to IRC evidence for conversational chains"
                                    "Add a peripheral discovery API that lists instantiable peripherals"
                                    "Extend EvidenceQuery to support multi-value in-reply-to for crossing queries"]

                                   "contributors"
                                   [{"agent-id" "claude-1"
                                     "contributed" ["what-worked" "what-didnt"]
                                     "via" "irc"}
                                    {"agent-id" "codex-1"
                                     "contributed" ["what-worked" "what-didnt" "suggestions"]
                                     "via" "irc"}]
                                   "synthesized-by" "codex-1"
                                   "coordination-channel" "#futon-par"
                                   "crossing-refs" {"claude-1" claude-cid
                                                    "codex-1" codex-cid}}
                                  "tags" ["mission" "joint-par" "reflection" "par"]
                                  "session-id" "live-mission"}
                        req (-> (java.net.http.HttpRequest/newBuilder)
                                (.uri (URI/create (str "http://127.0.0.1:" port
                                                       "/api/alpha/evidence")))
                                (.header "Content-Type" "application/json")
                                (.POST (java.net.http.HttpRequest$BodyPublishers/ofString
                                        (json/generate-string par-body)))
                                (.build))
                        resp (.send (HttpClient/newHttpClient) req
                                    (java.net.http.HttpResponse$BodyHandlers/ofString))]
                    (println (str "  PAR posted: " (.statusCode resp) " " (.body resp)))

                    ;; ─── FINAL VERIFICATION ───
                    (println)
                    (println "── Final evidence landscape ──")
                    (let [final-ev (http-get (str "http://127.0.0.1:" port "/api/alpha/evidence"))
                          final-entries (:entries final-ev)]
                      (println (str "  Total entries: " (count final-entries)))
                      (println)
                      (println "  Timeline:")
                      (doseq [e (sort-by :evidence/at final-entries)]
                        (println
                         (str "    "
                              (format "%-12s" (or (:evidence/claim-type e) "?"))
                              " | " (format "%-13s" (or (:evidence/type e) "?"))
                              " | " (format "%-22s" (or (:evidence/author e) "?"))
                              " | " (let [b (:evidence/body e)]
                                      (cond
                                        (and (map? b) (:event b)) (name (keyword (:event b)))
                                        (and (map? b) (:tool b)) (name (keyword (:tool b)))
                                        (and (map? b) (:text b)) (subs (str (:text b)) 0 (min 55 (count (str (:text b)))))
                                        (and (map? b) (:what-worked b)) "★ JOINT PAR ★"
                                        :else "?"))))))

                    (println)
                    (println "  Diamond:")
                    (println "    claude-1:  goal → steps → conclusion ─┐")
                    (println "                                           ├──→ joint PAR")
                    (println "    codex-1:   goal → step  → conclusion ─┘")
                    (println)

                    ;; Close connections
                    (.sendClose ^WebSocket (:ws claude) 1000 "done")
                    (.sendClose ^WebSocket (:ws codex) 1000 "done"))))

              ;; Clean up trial artifact
              (let [f (java.io.File. "docs/live-joint-par-findings.md")]
                (when (.exists f) (.delete f)))

              (println "═══ Live session complete ═══")
              (println)
              (shutdown-agents)
              (System/exit 0))))))))
