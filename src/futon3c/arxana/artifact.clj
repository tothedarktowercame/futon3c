(ns futon3c.arxana.artifact
  "ArtifactProvider boundary (D2-DR3/D7 authority table): immutable canonical
  bytes addressed by whole-artifact revision identity.

  An artifact revision ID is `arxana:revision:sha256:<digest>` over the whole
  artifact's canonical bytes (D1-DR2). A client segment hash over extracted
  text is a projection-integrity check only — never an artifact revision
  identity (D7 client segment revision identity). A client holding only text
  plus a segment hash is `client-preview:*` until authority ingest returns the
  artifact revision ID."
  (:require [futon3c.arxana.digest :as digest]
            [futon3c.arxana.envelope :as env])
  (:import [java.nio.charset StandardCharsets]))

(def revision-prefix "arxana:revision:sha256:")
(def preview-prefix "client-preview:")

(defprotocol ArtifactProvider
  "D2 ArtifactProvider contract. Bytes are immutable per revision ID: a client
  may ingest new bytes as a new revision but may not override an existing ID."
  (revision-metadata [provider revision-id])
  (revision-bytes [provider revision-id]))

(defn revision-id-for-bytes
  "Content-addressed revision ID for canonical bytes."
  ^String [^bytes bs]
  (str revision-prefix (digest/sha256-hex bs)))

(defn revision-id?
  "True when x is a well-formed authority revision ID."
  [x]
  (and (string? x) (.startsWith ^String x revision-prefix)))

(defn preview-id?
  "True when x is a client-preview locator (no authority revision identity)."
  [x]
  (and (string? x) (.startsWith ^String x preview-prefix)))

(defrecord InMemoryArtifactProvider [state]
  ArtifactProvider
  (revision-metadata [_ revision-id]
    (if-let [m (get-in @state [:meta revision-id])]
      {:ok m}
      {:error (env/error :missing :arxana.artifact/revision-not-found
                         {:artifact/revision-id revision-id} nil)}))

  (revision-bytes [_ revision-id]
    (if-let [bs (get-in @state [:bytes revision-id])]
      {:ok {:artifact/revision-id revision-id
            :bytes bs
            :media-type (get-in @state [:meta revision-id :props :arxana/media-type])}}
      {:error (env/error :missing :arxana.artifact/revision-not-found
                         {:artifact/revision-id revision-id} nil)})))

(defn in-memory-provider []
  (->InMemoryArtifactProvider (atom {:bytes {} :meta {}})))

(defn ingest!
  "Ingest canonical bytes for artifact-id as a new immutable revision.
  Returns {:ok metadata} with the content-addressed revision ID. Ingest of the
  same bytes is idempotent (same revision ID); the provider never overrides
  bytes for an existing ID — with content addressing it cannot need to."
  [provider artifact-id ^bytes bs media-type]
  (let [rid (revision-id-for-bytes bs)
        state (:state provider)
        meta {:external-id rid
              :type :arxana/artifact-revision
              :source artifact-id
              :props {:arxana/revision-algorithm :sha256
                      :arxana/content-digest (digest/sha256-hex bs)
                      :arxana/media-type media-type
                      :arxana/byte-length (alength bs)}}]
    (swap! state (fn [s]
                   (-> s
                       (assoc-in [:bytes rid] bs)
                       (assoc-in [:meta rid] meta))))
    {:ok (assoc meta :artifact/revision-id rid)}))

(defn preview-segment
  "Build a client-preview locator from raw segment text. The hash covers the
  exact extracted segment bytes as a projection-integrity check; the result is
  explicitly NOT an artifact revision identity."
  [^String segment-text]
  (str preview-prefix
       (digest/sha256-hex (.getBytes segment-text StandardCharsets/UTF_8))))

(defn require-authority-revision
  "Gate artifact-dependent operations on an authority revision ID. Preview
  clients lacking one are refused with :missing — they must ingest first."
  [revision-id]
  (cond
    (revision-id? revision-id) {:ok revision-id}
    (preview-id? revision-id)
    {:error (env/error :missing :arxana.artifact/authority-revision-required
                       {:received revision-id
                        :explanation "client-preview segment hash is a projection-integrity check, not an artifact revision identity"}
                       [:revision/ingest])}
    :else
    {:error (env/error :invalid :arxana.artifact/malformed-revision-id
                       {:received revision-id}
                       [:revision/ingest])}))

(defn verify-bytes
  "D2 evaluator gate: recompute the digest of supplied canonical bytes and
  compare to the named revision. Mismatch is :stale-input territory (a D2
  result status), reported here as typed detail for the evaluator."
  [provider revision-id]
  (let [{:keys [ok error]} (revision-bytes provider revision-id)]
    (cond
      error {:error error}
      (not= revision-id (revision-id-for-bytes (:bytes ok)))
      {:error (env/error :failed :arxana.artifact/integrity-mismatch
                         {:artifact/revision-id revision-id}
                         nil)}
      :else {:ok ok})))
