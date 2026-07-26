(ns futon3c.agents.memory-mcp
  "Small stdio MCP server exposing the trusted memory_record write seam.

   Identity and domain are process configuration supplied by the Codex
   controller, never tool arguments supplied by the model."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.evidence.futon1b-backend :as f1b]
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
                         :inputSchema memory-record-schema}]}}

      "tools/call"
      (if (= "memory_record" (:name params))
        (let [receipt
              (record-memory-fn
               {:agent-id agent-id
                :session-id (session-id session-file)
                :domain domain
                :evidence-store evidence-store}
               (or (:arguments params) {}))]
          {:jsonrpc "2.0" :id id
           :result {:content [{:type "text"
                               :text (json/generate-string receipt)}]
                    :isError (not (:ok receipt))}})
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
