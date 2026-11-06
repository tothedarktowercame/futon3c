(ns futon3c.arxana.envelope
  "arxana.v1 envelope validation (D3-DR1): one versioned operation envelope,
  typed domain results, unknown-field refusal in v1."
  (:require [futon3c.arxana.digest :as digest]))

(def protocol-version "arxana.v1")

(def envelope-fields
  #{:protocol/version :message/id :operation :principal/id :sent-at :body})

(def error-categories
  #{:invalid :unsupported :missing :stale :conflict :forbidden :overload :failed})

(defn error
  "A typed D3 error value. category must be one of the D3 algebra."
  [category code detail recovery]
  {:pre [(contains? error-categories category)
         (qualified-keyword? code)]}
  {:error/category category
   :error/code code
   :error/detail detail
   :error/recovery recovery})

(defn- invalid [code detail]
  (error :invalid code detail nil))

(defn validate-envelope
  "Validate an inbound arxana.v1 envelope map. Returns {:ok envelope} or
  {:error typed-error}. Unknown fields are refused, never ignored."
  [msg]
  (cond
    (not (map? msg))
    {:error (invalid :arxana.envelope/not-a-map {:received (type msg)})}

    :else
    (let [unknown (remove envelope-fields (keys msg))]
      (cond
        (seq unknown)
        {:error (invalid :arxana.envelope/unknown-field
                         {:unknown-fields (vec (sort unknown))
                          :known-fields (vec (sort envelope-fields))})}

        (not= protocol-version (:protocol/version msg))
        {:error (error :unsupported :arxana.envelope/protocol-version
                       {:expected protocol-version
                        :received (:protocol/version msg)}
                       [:capabilities/handshake])}

        (not (string? (:message/id msg)))
        {:error (invalid :arxana.envelope/message-id
                         {:required "string idempotency key"})}

        (not (qualified-keyword? (:operation msg)))
        {:error (invalid :arxana.envelope/operation
                         {:required "qualified operation keyword"})}

        (not (string? (:principal/id msg)))
        {:error (invalid :arxana.envelope/principal-id
                         {:required "string principal id"})}

        (not (map? (:body msg)))
        {:error (invalid :arxana.envelope/body {:required "map body"})}

        :else {:ok msg}))))

(defn response
  "Build a response envelope echoing version/message/operation, containing
  exactly one of :ok or :error (D3 envelope contract)."
  [request ok-or-error]
  (merge {:protocol/version protocol-version
          :message/id (:message/id request)
          :operation (:operation request)}
         (if (:error/category ok-or-error)
           {:error ok-or-error}
           {:ok ok-or-error})))

;;; Canonical projections (D5/O-D7d) ----------------------------------------

(def scope-resolve-projection-v1
  "Semantic fields digested for scope/resolve response equality. Excludes
  message/request ID, principal, timestamps, duration, transport metadata,
  trace IDs, presentation strings and operational diagnostics."
  [:protocol/semantics-version
   :operation
   :result/status
   :artifact/revision-id
   :strategy/revision-id
   [:result/candidates]
   :result/evidence-digests])

(def error-projection-v1
  "Semantic fields digested for error equivalence. Excludes localized prose
  and trace metadata."
  [:operation
   :error/category
   :error/code
   :target/id
   :expected/revision-id
   :current/revision-id
   [:error/recovery]])

(defn semantic-digest
  "Canonical SHA-256 of a response's registered semantic projection."
  [projection-fields payload]
  (digest/projected-digest projection-fields payload))

(defn error-digest
  "Canonical SHA-256 of a typed error under the error projection."
  [operation err]
  (digest/projected-digest error-projection-v1 (assoc err :operation operation)))
