(ns futon3c.agency.r9-authority
  "Read-only authority adapters. Candidate values are lookup IDs only; paths,
  byte pins, and scopes come from independently supplied host configuration."
  (:require [cheshire.core :as json] [clojure.edn :as edn]
            [clojure.string :as str] [futon3c.transport.http :as http])
  (:import (java.io PushbackReader StringReader)
           (java.nio ByteBuffer) (java.nio.charset CodingErrorAction StandardCharsets)
           (java.nio.file Files Path) (java.security MessageDigest)))

(def sha-pattern #"[0-9a-f]{64}")
(def ^:dynamic *after-authority-read* nil)
(defn- refuse! [code data cause]
  (throw (ex-info (name code) (merge {:refusal code} data) cause)))
(defn- hex [bs] (apply str (map #(format "%02x" (bit-and 0xff %)) bs)))
(defn- digest-bytes [bs]
  (hex (.digest (doto (MessageDigest/getInstance "SHA-256") (.update bs)))))
(defn- read-bytes! [path kind]
  (try (let [bs (Files/readAllBytes (Path/of path (make-array String 0)))]
         (when *after-authority-read* (*after-authority-read* path kind))
         bs)
       (catch Throwable e (refuse! :r9/authority-io-failure {:kind kind :path path} e))))
(defn- decode! [bs kind]
  (try (str (.decode (doto (.newDecoder StandardCharsets/UTF_8)
                       (.onMalformedInput CodingErrorAction/REPORT)
                       (.onUnmappableCharacter CodingErrorAction/REPORT))
                     (ByteBuffer/wrap bs)))
       (catch Throwable e (refuse! :r9/authority-utf8-invalid {:kind kind} e))))
(defn- parse-edn! [bs kind]
  (try
    (with-open [r (PushbackReader. (StringReader. (decode! bs kind)))]
      (let [eof (Object.) x (edn/read {:eof eof} r) tail (edn/read {:eof eof} r)]
        (when (or (identical? x eof) (not (identical? tail eof)))
          (refuse! :r9/authority-record-malformed {:kind kind} nil))
        x))
    (catch clojure.lang.ExceptionInfo e (throw e))
    (catch Throwable e (refuse! :r9/authority-parse-failure {:kind kind} e))))
(defn- pinned-record! [entry expected-id kind]
  (when-not (and (map? entry) (= expected-id (:id entry)) (string? (:path entry))
                 (re-matches sha-pattern (str (:metadata-sha256 entry))))
    (refuse! :r9/authority-configuration-missing {:id expected-id :kind kind} nil))
  (let [bs (read-bytes! (:path entry) kind) observed (digest-bytes bs)]
    (when-not (= observed (:metadata-sha256 entry))
      (refuse! :r9/authority-byte-mismatch {:id expected-id :kind kind :observed observed} nil))
    (let [record (parse-edn! bs kind)]
      (when-not (= expected-id (:id record))
        (refuse! :r9/authority-identity-mismatch {:id expected-id :kind kind} nil))
      record)))
(defn- validate-record! [record schema scope kind]
  (when-not (and (= :verified (:status record)) (= schema (:authority/schema record))
                 (= scope (:verification-scope record))
                 (some? (:authority-origin record)) (map? (:provenance record)))
    (refuse! :r9/authority-record-status-conflict {:kind kind :scope scope} nil))
  record)
(defn- raw-records [bs]
  (loop [start 0 i 0 out []]
    (if (= i (alength bs))
      (cond-> out (< start i) (conj (java.util.Arrays/copyOfRange bs start i)))
      (if (= 10 (bit-and 0xff (aget bs i)))
        (recur (inc i) (inc i) (conj out (java.util.Arrays/copyOfRange bs start (inc i))))
        (recur start (inc i) out)))))
(defn- parse-json-record! [bs kind]
  (try (json/parse-string (str/trim-newline (decode! bs kind)) true)
       (catch clojure.lang.ExceptionInfo e (throw e))
       (catch Throwable e (refuse! :r9/authority-parse-failure {:kind kind} e))))

(defn configured-host-event-resolver
  "Hash the exact raw JSONL record, including its actual terminator, and inspect
  that same buffer. Verification additionally requires a separately pinned
  accepted review of the exact event and explicit host-process trust model."
  [{:keys [verification-scope source session-id line record-sha256 expected trust-review]} _]
  (when-not (and (string? source) (string? session-id) (pos-int? line)
                 (re-matches sha-pattern (str record-sha256)) (map? expected))
    (refuse! :r9/host-event-configuration-missing {} nil))
  (let [file-bytes (read-bytes! source :host-session-log) records (raw-records file-bytes)
        meta-record (some-> (first records) (parse-json-record! :host-session-meta))
        event-bytes (nth records (dec line) nil)]
    (when-not event-bytes (refuse! :r9/host-event-record-missing {:line line} nil))
    (let [observed (digest-bytes event-bytes) event (parse-json-record! event-bytes :host-session-event)
          text (get-in event [:payload :content 0 :text])]
      (when-not (= record-sha256 observed)
        (refuse! :r9/host-event-byte-mismatch {:line line :observed observed} nil))
      (when-not (and (= "session_meta" (:type meta-record))
                     (= session-id (get-in meta-record [:payload :session_id]))
                     (= "response_item" (:type event)) (= "message" (get-in event [:payload :type]))
                     (= (:role expected) (get-in event [:payload :role]))
                     (every? #(str/includes? (str text) %)
                             [(str "From: " (:from expected)) (str "To: " (:to expected))
                              (str "Origin: " (:origin expected))]))
        (refuse! :r9/host-event-content-mismatch {:line line :session-id session-id} nil))
      (when-not trust-review (refuse! :r9/host-origin-review-missing {} nil))
      (let [review (pinned-record! trust-review (:id trust-review) :host-origin-review)
            subject {:source source :session-id session-id :line line :record-sha256 record-sha256
                     :role (:role expected) :from (:from expected) :to (:to expected)
                     :origin (:origin expected)}]
        (when-not (and (= :wm/host-process-origin-review-v1 (:schema review))
                       (= :accepted (:outcome review)) (= subject (:subject review))
                       (string? (:reviewer review)) (seq (:reviewer review))
                       (map? (:trust-model review)) (seq (:trust-model review))
                       (string? (:authority-root-id review)) (seq (:authority-root-id review))
                       (string? (:delegate review)) (seq (:delegate review)))
          (refuse! :r9/host-origin-review-mismatch {} nil))
        {:status :verified :authority/schema :wm/external-root-resolution-v1
         :authority-origin :reviewed-host-process-boundary :verification-scope verification-scope
         :provenance {:source source :session-id session-id :line line :record-sha256 record-sha256
                      :trust-review-path (:path trust-review)
                      :trust-review-sha256 (:metadata-sha256 trust-review)}
         :authority-root-id (:authority-root-id review) :delegate (:delegate review)}))))

(defn configured-resolvers
  [{:keys [verification-scope roots traces artifacts acceptances predecessors]}]
  (let [lookup (fn [table id schema kind]
                 (-> (get table id) (pinned-record! id kind)
                     (validate-record! schema verification-scope kind)))]
    {:root-resolver #(let [entry (get roots (:id %))]
                       (if (= :host-session-event (:kind entry))
                         (configured-host-event-resolver (assoc entry :verification-scope verification-scope) %)
                         (lookup roots (:id %) :wm/external-root-resolution-v1 :root)))
     :commission-resolver (fn [job-id]
                            {:status :verified :authority/schema :wm/invoke-commission-resolution-v1
                             :authority-origin :agency-request-commission-api
                             :verification-scope verification-scope
                             :provenance {:api "futon3c.transport.http/invoke-job-request-commission" :job-id job-id}
                             :resolved-job-id job-id :envelope (http/invoke-job-request-commission job-id)})
     :trace-resolver #(lookup traces % :wm/trace-resolution-v1 :trace)
     :artifact-resolver
     (fn [pin]
       (let [entry (get artifacts (:id pin))
             record (-> entry (pinned-record! (:id pin) :artifact-metadata)
                        (validate-record! :wm/artifact-byte-resolution-v1 verification-scope :artifact-metadata))]
         (when-not (and (string? (:artifact-path entry))
                        (re-matches sha-pattern (str (:artifact-sha256 entry))))
           (refuse! :r9/artifact-configuration-missing {:id (:id pin)} nil))
         (let [observed (digest-bytes (read-bytes! (:artifact-path entry) :artifact-content))]
           (when-not (and (= observed (:artifact-sha256 entry)) (= observed (:sha256 record)))
             (refuse! :r9/artifact-content-mismatch {:id (:id pin) :observed observed} nil))
           record)))
     :acceptance-resolver #(let [r (lookup acceptances (:id %) :wm/delegated-acceptance-resolution-v1 :acceptance)]
                             (when-not (and (= :delegated-canonical-branch-acceptance-v1 (:schema r))
                                            (= :accepted (:review-outcome r)))
                               (refuse! :r9/acceptance-record-invalid {} nil)) r)
     :predecessor-resolver #(lookup predecessors (:anchor-id %)
                                    :wm/anchored-checker-resolution-v1 :predecessor)}))
