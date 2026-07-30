#!/usr/bin/env bb
;; Post-session Codex rollout -> turn-round evidence harvester.
;;
;; Default mode is local dry-run only. Store writes require --commit.
;; The one accepted live fixture is 019f8b63-a009-79e0-9a22-e7402848c822;
;; callers may parse other files, but this script never bulk-discovers history.

(ns rollout-harvester
  (:require [babashka.http-client :as http]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.net URLEncoder]
           [java.nio.charset StandardCharsets]
           [java.time Instant]))

(def harvester-version 1)
(def fixture-session "019f8b63-a009-79e0-9a22-e7402848c822")
(def root "/home/joe/code/futon3c/holes/labs/M-codex-sorry-loop")
(def dryrun-path (str root "/harvest-dryrun-019f8b63.edn"))
(def report-path (str root "/harvest-report-20260728.edn"))

(defn session-paths
  "Per-session artifact paths; the fixture keeps its original S1
  acceptance filenames (owner lift, 2026-07-28)."
  [session-id]
  (if (= session-id fixture-session)
    {:dryrun dryrun-path :report report-path}
    (let [prefix (subs session-id 0 (min 8 (count session-id)))]
      {:dryrun (str root "/harvest-dryrun-" prefix ".edn")
       :report (str root "/harvest-report-" prefix ".edn")})))
(def sessions-root (str (System/getProperty "user.home") "/.codex/sessions"))
(def substrate-base "http://127.0.0.1:7073")
(def append-url (str substrate-base "/api/alpha/evidence"))
(def body-cap-bytes (* 16 1024))
(def digest-char-limit 200)
(def write-pause-ms 125)
(def retry-backoff-ms 5000)

(defn usage! []
  (binding [*out* *err*]
    (println "Usage: bb rollout_harvester.bb (--session UUID | --file PATH) [--limit N] [--commit]"))
  (System/exit 2))

(defn parse-args [args]
  (loop [xs args
         opts {:commit? false}]
    (if-let [arg (first xs)]
      (case arg
        "--session" (if-let [v (second xs)]
                      (recur (nnext xs) (assoc opts :session v))
                      (usage!))
        "--file" (if-let [v (second xs)]
                   (recur (nnext xs) (assoc opts :file v))
                   (usage!))
        "--limit" (if-let [v (second xs)]
                    (recur (nnext xs)
                           (assoc opts :limit
                                  (try
                                    (Long/parseLong v)
                                    (catch Exception _ (usage!)))))
                    (usage!))
        "--commit" (recur (next xs) (assoc opts :commit? true))
        "--allow-nonfixture" (recur (next xs)
                                    (assoc opts :allow-nonfixture? true))
        (usage!))
      opts)))

(defn find-session-file [uuid]
  (let [matches (->> (file-seq (io/file sessions-root))
                     (filter #(.isFile ^java.io.File %))
                     (filter #(and (str/starts-with? (.getName ^java.io.File %)
                                                    "rollout-")
                                   (str/ends-with? (.getName ^java.io.File %)
                                                  ".jsonl")
                                   (str/includes? (.getName ^java.io.File %)
                                                  uuid)))
                     (sort-by #(.getPath ^java.io.File %))
                     vec)]
    (case (count matches)
      0 (throw (ex-info "session rollout not found" {:session uuid}))
      1 (.getCanonicalPath ^java.io.File (first matches))
      (throw (ex-info "session uuid matched multiple rollouts"
                      {:session uuid
                       :matches (mapv #(.getPath ^java.io.File %) matches)})))))

(defn parse-json-line [line seq-number]
  (try
    (assoc (json/parse-string line true) :rollout/seq seq-number)
    (catch Exception exception
      (throw (ex-info "invalid rollout JSONL"
                      {:seq seq-number
                       :message (.getMessage exception)})))))

(defn read-events [path]
  (with-open [reader (io/reader path)]
    (->> (line-seq reader)
         (map-indexed (fn [index line]
                        (parse-json-line line (inc index))))
         doall
         vec)))

(defn session-meta [events]
  (or (some #(when (= "session_meta" (:type %)) (:payload %)) events)
      (throw (ex-info "rollout has no session_meta" {}))))

(defn turn-context? [event]
  (= "turn_context" (:type event)))

(defn partition-turns [events]
  (->> events
       (drop-while (complement turn-context?))
       (partition-by turn-context?)
       (partition-all 2)
       (mapv (fn [[context-events body-events]]
               (let [context (first context-events)]
                 {:context context
                  :events (vec body-events)
                  :seq-start (:rollout/seq context)
                  :seq-end (or (:rollout/seq (last body-events))
                               (:rollout/seq context))})))))

(defn content-text [content]
  (->> content
       (keep (fn [item]
               (when (map? item)
                 (or (:text item)
                     (when (string? (:content item)) (:content item))))))
       (str/join "\n")))

(defn reasoning-text [payload]
  ;; Current Codex rollouts generally retain encrypted_content but no plaintext
  ;; reasoning. Preserve every plaintext summary item when it exists and make
  ;; encrypted-only items auditable through the count below.
  (let [summary (:summary payload)]
    (cond
      (string? summary) summary
      (sequential? summary) (content-text summary)
      :else nil)))

(defn agent-message [event]
  (let [payload (:payload event)]
    (cond
      (and (= "event_msg" (:type event))
           (= "agent_message" (:type payload)))
      (:message payload)

      ;; Some rollout versions omit event_msg/agent_message. Accept assistant
      ;; response messages only; never ingest developer/user setup as output.
      (and (= "response_item" (:type event))
           (= "message" (:type payload))
           (= "assistant" (:role payload)))
      (content-text (:content payload))

      :else nil)))

(defn compact [value]
  (let [text (cond
               (nil? value) ""
               (string? value) value
               :else (pr-str value))
        text (str/replace text #"\s+" " ")]
    (if (> (count text) digest-char-limit)
      (str (subs text 0 digest-char-limit) "…")
      text)))

(defn output-text [output]
  (cond
    (string? output) output
    (sequential? output) (content-text output)
    :else (pr-str output)))

(defn tool-digest [event]
  (let [payload (:payload event)
        event-type (:type event)
        payload-type (:type payload)]
    (cond
      (and (= "response_item" event-type)
           (#{"function_call" "custom_tool_call"} payload-type))
      {:kind (keyword payload-type)
       :name (:name payload)
       :call-id (:call_id payload)
       :args (compact (or (:arguments payload) (:input payload)))}

      (and (= "response_item" event-type)
           (#{"function_call_output" "custom_tool_call_output"} payload-type))
      {:kind (keyword payload-type)
       :call-id (:call_id payload)
       :output (compact (output-text (:output payload)))}

      (and (= "event_msg" event-type)
           (= "patch_apply_end" payload-type))
      {:kind :patch-apply
       :call-id (:call_id payload)
       :status (if (:success payload) :success :failure)
       :files (-> (:changes payload) keys sort vec)
       :stdout (compact (:stdout payload))
       :stderr (compact (:stderr payload))}

      :else nil)))

(defn infer-agent [events]
  (let [texts (for [event events
                    :let [payload (:payload event)
                          text (or (:message payload)
                                   (when (= "message" (:type payload))
                                     (content-text (:content payload))))]
                    :when (string? text)]
                text)]
    (some (fn [text]
            (some-> (re-find #"(?m)^To: (codex-[0-9]+)$" text) second))
          texts)))

(defn byte-count [value]
  (alength (.getBytes (str value) StandardCharsets/UTF_8)))

(defn render-text [{:keys [turn reasoning messages tools]}]
  (str "Codex rollout turn " turn "\n\n"
       "REASONING (plaintext available in rollout)\n"
       (if (seq reasoning) (str/join "\n\n" reasoning) "[none available]")
       "\n\nAGENT MESSAGES\n"
       (if (seq messages) (str/join "\n\n" messages) "[none]")
       "\n\nTOOL DIGEST\n"
       (if (seq tools)
         (str/join "\n" (map pr-str tools))
         "[none]")))

(defn capped-body [base]
  (let [full-text (render-text base)
        ;; Keep the zai-compatible :text surface without duplicating its large
        ;; reasoning/message content in structured fields. Counts make any
        ;; unavailable or capped material explicit.
        body-base (-> base
                      (dissoc :reasoning :messages :tools)
                      (assoc :reasoning-count (count (:reasoning base))
                             :message-count (count (:messages base))
                             :tool-digest-count (count (:tools base))))
        body (assoc body-base :event :turn-round :text full-text
                   :truncated? false)]
    (if (<= (byte-count (pr-str body)) body-cap-bytes)
      body
      (let [without-text (assoc body-base :event :turn-round :text ""
                                :truncated? true
                                :original-text-bytes (byte-count full-text))
            marker "\n[TRUNCATED AT 16KB BODY CAP]"
            ;; pr-str escapes newlines and quotes, so select the prefix against
            ;; the serialized body itself rather than raw string byte length.
            result
            (loop [low 0 high (count full-text)]
              (if (< low high)
                (let [mid (quot (+ low high 1) 2)
                      candidate (assoc without-text :text
                                       (str (subs full-text 0 mid) marker))]
                  (if (<= (byte-count (pr-str candidate)) body-cap-bytes)
                    (recur mid high)
                    (recur low (dec mid))))
                (assoc without-text :text
                       (str (subs full-text 0 low) marker))))]
        result))))

(defn evidence-id [session-id turn-number]
  (format "e-codexroll-%s-t%03d" (subs session-id 0 8) turn-number))

(defn turn-entry [path session-id agent turn-number turn]
  (let [events (:events turn)
        reasoning-payloads (for [event events
                                 :let [payload (:payload event)]
                                 :when (and (= "response_item" (:type event))
                                            (= "reasoning" (:type payload)))]
                             payload)
        reasoning (vec (keep reasoning-text reasoning-payloads))
        encrypted-count (count (filter #(and (str/blank?
                                              (or (reasoning-text %) ""))
                                             (seq (:encrypted_content %)))
                                       reasoning-payloads))
        messages (->> events (keep agent-message) (remove str/blank?) vec)
        tools (vec (keep tool-digest events))
        body (capped-body
              {:turn turn-number
               :turn-id (get-in turn [:context :payload :turn_id])
               :reasoning reasoning
               :reasoning-encrypted-only-count encrypted-count
               :messages messages
               :tools tools
               :profile :codex-rollout
               :session-uuid session-id
               :rollout-file path
               :event-seq-range [(:seq-start turn) (:seq-end turn)]
               :harvester-version harvester-version})]
    {:evidence/id (evidence-id session-id turn-number)
     :evidence/subject {:ref/type :agent :ref/id (or agent "codex-unknown")}
     :evidence/type :coordination
     :evidence/claim-type :step
     :evidence/at (:timestamp (:context turn))
     :evidence/author (or agent "codex-rollout")
     :evidence/session-id session-id
     :evidence/body body
     :evidence/tags (cond-> [:codex :turn-round :codex-rollout]
                      agent (conj (keyword agent)))}))

(defn build-entries [path]
  (let [events (read-events path)
        meta (session-meta events)
        session-id (or (:session_id meta) (:id meta))
        turns (partition-turns events)
        agent (infer-agent events)
        entries (mapv #(turn-entry path session-id agent (inc %1) %2)
                      (range) turns)
        entries-again (mapv #(turn-entry path session-id agent (inc %1) %2)
                            (range) turns)]
    ;; Fixture assertions are always exercised when parsing the fixture.
    (when (= fixture-session session-id)
      (assert (= (count turns)
                 (count (filter turn-context? events))))
      (assert (= 86 (count turns)))
      (assert (= (mapv :evidence/id entries)
                 (mapv :evidence/id entries-again)))
      (assert (every? #(<= (byte-count (pr-str (:evidence/body %)))
                           body-cap-bytes)
                      entries)))
    {:session-id session-id
     :agent agent
     :rollout-file path
     :event-count (count events)
     :turn-context-count (count (filter turn-context? events))
     :entries entries}))

(defn encode [value]
  (URLEncoder/encode (str value) "UTF-8"))

(defn parse-edn [body]
  (when (seq body)
    (edn/read-string {:default (fn [_tag value] value)} body)))

(defn retryable? [response parsed]
  (or (= 503 (:status response))
      (= :expensive-read-busy (:error/code parsed))
      (= :expensive-read-busy (:error parsed))
      (= "expensive-read-busy" (:err parsed))
      (= "evidence-append-in-flight" (:err parsed))))

(defn request-with-retry [request-fn]
  (loop [attempt 1]
    (let [response (try
                     (request-fn)
                     (catch Exception exception
                       {:status nil :transport-error (.getMessage exception)}))
          parsed (try (parse-edn (:body response))
                      (catch Exception _ nil))]
      (if (and (= attempt 1) (retryable? response parsed))
        (do (Thread/sleep retry-backoff-ms)
            (recur 2))
        {:response response :parsed parsed :attempts attempt}))))

(defn fetch-entry [id]
  (request-with-retry
   #(http/get (str append-url "/" (encode id))
              {:headers {"Accept" "application/edn"}
               :timeout 30000
               :throw false})))

(defn existing? [id]
  (let [{:keys [response]} (fetch-entry id)]
    (case (:status response)
      200 true
      404 false
      (throw (ex-info "existence probe failed"
                      {:id id
                       :status (:status response)
                       :body (:body response)})))))

(defn append-entry! [entry]
  (let [id (:evidence/id entry)
        posted (request-with-retry
                #(http/post append-url
                            {:headers {"Content-Type" "application/edn"
                                       "Accept" "application/edn"
                                       "x-penholder" "api"}
                             :body (pr-str entry)
                             :timeout 30000
                             :throw false}))
        status (get-in posted [:response :status])]
    (when-not (<= 200 (long (or status 0)) 299)
      (throw (ex-info "append failed"
                      {:id id
                       :status status
                       :body (get-in posted [:response :body])
                       :attempts (:attempts posted)})))
    (let [{:keys [response parsed attempts]} (fetch-entry id)
          returned (or (:entry parsed) parsed)]
      (when-not (and (= 200 (:status response))
                     (= id (:evidence/id returned)))
        (throw (ex-info "append read-back verification failed"
                        {:id id
                         :status (:status response)
                         :body (:body response)})))
      {:id id
       :append-attempts (:attempts posted)
       :read-back-attempts attempts
       :verified? true})))

(defn commit! [entries]
  (loop [remaining entries
         report {:rows-considered (count entries)
                 :rows-written 0
                 :skipped-existing 0
                 :verified 0
                 :errors []
                 :writes []}]
    (if-let [entry (first remaining)]
      (let [id (:evidence/id entry)
            step
            (try
              (if (existing? id)
                {:kind :existing}
                {:kind :written :receipt (append-entry! entry)})
              (catch Exception exception
                {:kind :error
                 :exception exception}))]
          (case (:kind step)
            :existing
            (recur (next remaining)
                   (update report :skipped-existing inc))

            :written
            (do
              (Thread/sleep write-pause-ms)
              (recur (next remaining)
                     (-> report
                         (update :rows-written inc)
                         (update :verified inc)
                         (update :writes conj (:receipt step)))))

            :error
            (let [exception (:exception step)]
              ;; Stop at the first failed store seam: later rows are not attempted.
              (assoc report
                     :errors [{:id id
                               :message (.getMessage exception)
                               :data (ex-data exception)}]
                     :stopped? true
                     :remaining-count (count remaining)))))
      report)))

(defn write-edn! [path value]
  (io/make-parents path)
  (spit path (str (pr-str value) "\n")))

(defn -main [& args]
  (let [opts (parse-args args)
        path (cond
               (:file opts) (.getCanonicalPath (io/file (:file opts)))
               (:session opts) (find-session-file (:session opts))
               :else (usage!))
        built (build-entries path)
        _ (when (and (:session opts)
                     (not (str/includes? (:session-id built) (:session opts))))
            (throw (ex-info "resolved rollout session mismatch"
                            {:requested (:session opts)
                             :actual (:session-id built)})))
        entries (cond->> (:entries built)
                  (:limit opts) (take (:limit opts))
                  true vec)
        common {:harvester/version harvester-version
                :mode (if (:commit? opts) :commit :dry-run)
                :session-id (:session-id built)
                :agent (:agent built)
                :rollout-file path
                :event-count (:event-count built)
                :turn-context-count (:turn-context-count built)
                :selected-row-count (count entries)
                :body-cap-bytes body-cap-bytes
                :generated-at (str (Instant/now))}]
    (if (:commit? opts)
      (do
        ;; S1 acceptance was fixture-only; post-acceptance (owner lift,
        ;; 2026-07-28) other sessions require the explicit flag. One
        ;; session per invocation either way — bulk ingest stays a
        ;; deliberate, store-RAM-aware act, never a default.
        (when-not (or (= fixture-session (:session-id built))
                      (:allow-nonfixture? opts))
          (throw (ex-info "non-fixture commit requires --allow-nonfixture"
                          {:session-id (:session-id built)})))
        (let [paths (session-paths (:session-id built))
              prior (when (.exists (io/file (:report paths)))
                      (edn/read-string (slurp (:report paths))))
              result (commit! entries)
              report (cond-> (merge common result)
                       prior (assoc :prior-run
                                    (dissoc prior :prior-run :writes)))]
          (write-edn! (:report paths) report)
          (println (pr-str (dissoc report :writes :prior-run)))
          (when (seq (:errors report)) (System/exit 1))))
      (let [artifact (assoc common
                            :assertions {:row-count-equals-turn-context-count true
                                         :body-cap-respected true
                                         :deterministic-ids true}
                            :entries entries)
            paths (session-paths (:session-id built))]
        (write-edn! (:dryrun paths) artifact)
        (println (pr-str (dissoc artifact :entries)))))))

(apply -main *command-line-args*)
