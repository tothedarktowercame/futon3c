(ns futon3c.evidence.http-backend
  "HTTP-based EvidenceBackend — queries a remote Agency's evidence API.

   Used by standalone agents (e.g. Tickle) that run outside the dev server
   JVM but need to read/write evidence via the HTTP API."
  (:require [futon3c.evidence.backend :as backend]
            [cheshire.core :as json]
            [clojure.string :as str]
            [org.httpkit.client :as http])
  (:import [java.net URLEncoder]
           [java.time Instant]))

(defn- api-url [base-url path]
  (str (str/replace base-url #"/$" "") path))

(defn- path-segment [s]
  (URLEncoder/encode (str s) "UTF-8"))

(defn- parse-response [resp]
  (when-let [body (:body resp)]
    (try (json/parse-string body true)
         (catch Exception _ nil))))

(defn- enum-name [x]
  (cond
    (keyword? x) (name x)
    (some? x) (str x)
    :else nil))

(defn- http-append-payload
  "Encode a validated EvidenceEntry into the public HTTP append shape.
   The server endpoint normalizes unqualified payload keys; sending raw
   :evidence/... JSON keys is rejected before the boundary can persist it."
  [entry]
  (cond-> {:evidence-id (:evidence/id entry)
           :subject (:evidence/subject entry)
           :type (enum-name (:evidence/type entry))
           :claim-type (enum-name (:evidence/claim-type entry))
           :author (:evidence/author entry)
           :body (:evidence/body entry)
           :tags (mapv enum-name (:evidence/tags entry))}
    (:evidence/pattern-id entry)
    (assoc :pattern-id (enum-name (:evidence/pattern-id entry)))

    (:evidence/session-id entry)
    (assoc :session-id (:evidence/session-id entry))

    (:evidence/in-reply-to entry)
    (assoc :in-reply-to (:evidence/in-reply-to entry))

    (:evidence/fork-of entry)
    (assoc :fork-of (:evidence/fork-of entry))

    (contains? entry :evidence/conjecture?)
    (assoc :conjecture? (:evidence/conjecture? entry))

    (contains? entry :evidence/ephemeral?)
    (assoc :ephemeral? (:evidence/ephemeral? entry))))

(defrecord HttpBackend [base-url]
  backend/EvidenceBackend

  (-append [_ entry]
    (let [resp @(http/post (api-url base-url "/api/alpha/evidence")
                           {:headers {"Content-Type" "application/json"}
                            :body (json/generate-string (http-append-payload entry))
                            :timeout 5000})
          parsed (parse-response resp)]
      (if (:ok parsed)
        {:ok true :entry (:entry parsed)}
        {:error/component :E-store
         :error/code :http-error
         :error/message (str "HTTP append failed: " (:status resp))
         :error/at (str (Instant/now))})))

  (-get [_ evidence-id]
    (let [resp @(http/get (api-url base-url (str "/api/alpha/evidence/"
                                                 (path-segment evidence-id)))
                          {:timeout 5000})
          parsed (parse-response resp)]
      (:entry parsed)))

  (-exists? [this evidence-id]
    (some? (backend/-get this evidence-id)))

  (-query [_ params]
    (let [query-params (cond-> {}
                         (:query/type params) (assoc "type" (name (:query/type params)))
                         (:query/claim-type params) (assoc "claim-type" (name (:query/claim-type params)))
                         (:query/author params) (assoc "author" (:query/author params))
                         (:query/since params) (assoc "since" (str (:query/since params)))
                         (:query/limit params) (assoc "limit" (str (:query/limit params)))
                         (seq (:query/tags params)) (assoc "tag" (str/join "," (map name (:query/tags params)))))
          qs (str/join "&" (map (fn [[k v]] (str k "=" v)) query-params))
          url (str (api-url base-url "/api/alpha/evidence")
                   (when (seq qs) (str "?" qs)))
          resp @(http/get url {:timeout 10000})
          parsed (parse-response resp)]
      (or (:entries parsed) [])))

  (-forks-of [_ _evidence-id]
    ;; Not exposed via HTTP API — return empty for now
    [])

  (-delete! [_ _ids]
    ;; Not exposed via HTTP API
    {:compacted 0})

  (-all [_]
    (let [resp @(http/get (api-url base-url "/api/alpha/evidence")
                          {:timeout 10000})
          parsed (parse-response resp)]
      (or (:entries parsed) []))))

(defn make-http-backend
  "Create an EvidenceBackend that queries a remote Agency's HTTP API.
   base-url: e.g. \"http://localhost:7070\""
  [base-url]
  (->HttpBackend base-url))
