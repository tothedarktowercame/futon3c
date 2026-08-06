(ns futon3c.agents.memory-mcp
  "Small stdio MCP server exposing trusted memory read and write seams.

   Identity and domain are process configuration supplied by the Codex
   controller, never tool arguments supplied by the model."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.evidence.futon1b-backend :as f1b]
            [futon3c.evidence.store :as store]
            [futon3c.peripheral.memory-write :as memory-write])
  (:gen-class))

(def memory-record-description
  (str "Record one deliberate assert memory. Required fields: "
       "name (non-blank string), body (memory content), and "
       "subjects [{ref/type, ref/id}] (at least one typed subject). "
       "Identity and mathematics domain are server-stamped."))

(def memory-record-schema
  {:type "object"
   :properties
   {:name {:type "string" :minLength 1
           :description "Required non-blank recall handle."}
    :body {:description "Required self-contained memory content."}
    :subjects
    {:type "array"
     :minItems 1
     :description "Required subjects [{ref/type, ref/id}]."
     :items {:type "object"
             :properties
             {"ref/type" {:type "string"}
              "ref/id" {:type "string" :minLength 1}}
             :required ["ref/type" "ref/id"]
             :additionalProperties false}}
    :hook {:type "string"}
    :kind {:type "string"}
    :why {:type "string"}
    :how_to_apply {:type "string"}
    :distills {:type "array" :items {:type "string"}}
    :facets {:type "array" :items {:type "string"}}
    :volatile {:type "boolean"}}
   :required ["name" "body" "subjects"]
   :additionalProperties false})

(def memory-search-description
  (str "Search the evidence store by subject, type, claim type, author, "
       "timestamp, or tags. Returns matching evidence entries. Read-only."))

(def memory-search-schema
  {:type "object"
   :properties
   {:subject
    {:type "object"
     :description "ArtifactRef {ref/type, ref/id} to scope the search."
     :properties
     {"ref/type" {:type "string"}
      "ref/id" {:type "string" :minLength 1}}
     :required ["ref/type" "ref/id"]
     :additionalProperties false}
    :type {:type "string" :description "EvidenceType filter."}
    :claim_type {:type "string" :description "ClaimType filter."}
    :author {:type "string"}
    :since {:type "string" :description "ISO-8601 timestamp lower bound (inclusive)."}
    :tags {:type "array" :items {:type "string"}
           :description "Tag keywords to filter by."}
    :limit {:type "integer" :description "Max items (default 20, max 100)."}
    :include_ephemeral {:type "boolean"}}
   :additionalProperties false})

(defn- clamp-limit
  [n]
  (let [n (cond
            (int? n) n
            (string? n) (try (Long/parseLong n) (catch Throwable _ 20))
            :else 20)]
    (max 1 (min 100 (int n)))))

(defn- ->keyword
  [x]
  (if (keyword? x) x (keyword x)))

(defn- search-query
  [{:keys [subject type claim_type author since tags limit include_ephemeral]}]
  (let [subject (when (and (map? subject)
                           (:ref/type subject)
                           (:ref/id subject))
                  (update subject :ref/type ->keyword))]
    (cond-> {:query/limit (clamp-limit limit)}
      subject (assoc :query/subject subject)
      type (assoc :query/type (->keyword type))
      claim_type (assoc :query/claim-type (->keyword claim_type))
      author (assoc :query/author author)
      since (assoc :query/since since)
      (seq tags) (assoc :query/tags (mapv ->keyword tags))
      (true? include_ephemeral) (assoc :query/include-ephemeral? true))))

(defn- session-id
  [session-file]
  (or (when (and session-file (.exists (io/file session-file)))
        (some-> session-file slurp str/trim not-empty))
      "codex-memory-session"))

(defn handle-request
  [{:keys [agent-id session-file domain evidence-store record-memory-fn]}
   {:keys [id method params]}]
  (let [record-memory-fn (or record-memory-fn memory-write/record-memory!)]
    (case method
      "initialize"
      {:jsonrpc "2.0" :id id
       :result {:protocolVersion "2025-03-26"
                :capabilities {:tools {}}
                :serverInfo {:name "futon-memory" :version "1"}}}

      "ping"
      {:jsonrpc "2.0" :id id :result {}}

      "tools/list"
      {:jsonrpc "2.0" :id id
       :result {:tools [{:name "memory_record"
                         :description memory-record-description
                         :inputSchema memory-record-schema}
                        {:name "memory_search"
                         :description memory-search-description
                         :inputSchema memory-search-schema}]}}

      "tools/call"
      (case (:name params)
        "memory_record"
        (let [receipt (record-memory-fn
                       {:agent-id agent-id
                        :session-id (session-id session-file)
                        :domain domain
                        :evidence-store evidence-store}
                       (or (:arguments params) {}))]
          {:jsonrpc "2.0" :id id
           :result {:content [{:type "text"
                               :text (json/generate-string receipt)}]
                    :isError (not (:ok receipt))}})

        "memory_search"
        (let [items (store/query* evidence-store
                                  (search-query (or (:arguments params) {})))]
          {:jsonrpc "2.0" :id id
           :result {:content [{:type "text"
                               :text (json/generate-string items)}]
                    :isError false}})

        {:jsonrpc "2.0" :id id
         :error {:code -32602 :message "Unknown tool"}})

      nil)))

(defn -main
  [& [agent-id session-file domain base-url]]
  (let [ctx {:agent-id agent-id
             :session-file session-file
             :domain (keyword domain)
             :evidence-store (f1b/make-futon1b-backend base-url)}
        reader (io/reader System/in)
        writer (io/writer System/out)]
    (doseq [line (line-seq reader)]
      (when-not (str/blank? line)
        (when-let [response
                   (handle-request ctx (json/parse-string line true))]
          (.write writer (str (json/generate-string response) "\n"))
          (.flush writer))))))
