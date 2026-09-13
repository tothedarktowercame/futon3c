(ns futon3c.agency.r9-authority
  "Read-only, independently configured authority adapters for the R9 boundary.

  Candidate values are lookup identifiers only. Paths and expected bytes come
  exclusively from the host configuration supplied when these adapters are
  constructed. No adapter turns a located record into a verified authority."
  (:require [clojure.edn :as edn] [clojure.java.io :as io]
            [futon3c.transport.http :as http])
  (:import (java.io PushbackReader) (java.nio.charset StandardCharsets)
           (java.nio.file Files Path) (java.security MessageDigest)))

(defn- hex [bs] (apply str (map #(format "%02x" (bit-and 0xff %)) bs)))
(defn- sha256 [path]
  (with-open [in (io/input-stream path)]
    (let [d (MessageDigest/getInstance "SHA-256") b (byte-array 8192)]
      (loop [] (let [n (.read in b)] (when (pos? n) (.update d b 0 n) (recur))))
      (hex (.digest d)))))
(defn- read-one [path]
  (with-open [r (PushbackReader. (io/reader path))]
    (let [eof (Object.) x (edn/read {:eof eof} r) tail (edn/read {:eof eof} r)]
      (when (or (identical? x eof) (not (identical? tail eof)))
        (throw (ex-info "authority record malformed" {:refusal :r9/authority-record-malformed})))
      x)))
(defn- sha256-text [s]
  (hex (.digest (doto (MessageDigest/getInstance "SHA-256")
                  (.update (.getBytes s StandardCharsets/UTF_8))))))

(declare configured-record)

(defn configured-host-event-resolver
  "Resolve one host event only when a separately pinned trust-review record
  accepts that exact source/line/hash under an explicit host-process model.
  Merely locating a user-role JSONL line is a typed refusal."
  [{:keys [verification-scope source line record-sha256 trust-review] :as config} _]
  (when-not (and (string? source) (pos-int? line)
                 (re-matches #"[0-9a-f]{64}" (str record-sha256)))
    (throw (ex-info "host event configuration missing"
                    {:refusal :r9/host-event-configuration-missing})))
  (let [lines (Files/readAllLines (Path/of source (make-array String 0)) StandardCharsets/UTF_8)
        text (when (<= line (count lines)) (str (nth lines (dec line)) "\n"))
        observed (when text (sha256-text text))]
    (when-not (= record-sha256 observed)
      (throw (ex-info "host event bytes changed" {:refusal :r9/host-event-byte-mismatch
                                                   :line line :observed observed})))
    (when-not trust-review
      (throw (ex-info "host origin trust has not been independently reviewed"
                      {:refusal :r9/host-origin-review-missing})))
    (let [review (configured-record trust-review (:id trust-review))]
      (when-not (and (= :wm/host-process-origin-review-v1 (:schema review))
                     (= :accepted (:outcome review))
                     (= {:source source :line line :record-sha256 record-sha256}
                        (:subject review))
                     (string? (:authority-root-id review))
                     (string? (:delegate review)))
        (throw (ex-info "host origin trust review does not accept exact event"
                        {:refusal :r9/host-origin-review-mismatch})))
      {:status :verified :authority/schema :wm/external-root-resolution-v1
       :authority-origin :reviewed-host-process-boundary
       :verification-scope verification-scope
       :provenance {:source source :line line :record-sha256 record-sha256
                    :trust-review-path (:path trust-review)
                    :trust-review-sha256 (:sha256 trust-review)}
       :authority-root-id (:authority-root-id review) :delegate (:delegate review)})))
(defn- configured-record [entry expected-id]
  (when-not (and (map? entry) (= expected-id (:id entry))
                 (string? (:path entry)) (re-matches #"[0-9a-f]{64}" (:sha256 entry)))
    (throw (ex-info "authority configuration missing" {:refusal :r9/authority-configuration-missing
                                                         :id expected-id})))
  (let [observed (sha256 (:path entry))]
    (when-not (= observed (:sha256 entry))
      (throw (ex-info "authority bytes changed" {:refusal :r9/authority-byte-mismatch
                                                  :id expected-id :observed observed})))
    (let [record (read-one (:path entry))]
      (when-not (= expected-id (:id record))
        (throw (ex-info "authority identity mismatch" {:refusal :r9/authority-identity-mismatch
                                                        :id expected-id})))
      record)))

(defn configured-resolvers
  "Construct concrete readers from a host-owned configuration map. The config
  maps candidate IDs to fixed path+sha pins; candidates cannot introduce paths.
  Commission reads use Agency's durable API and are isolated by the caller in tests."
  [{:keys [verification-scope roots traces artifacts acceptances predecessors]}]
  (let [decorate (fn [record schema origin entry]
                   (assoc record :status :verified :authority/schema schema
                          :authority-origin origin :verification-scope verification-scope
                          :provenance {:path (:path entry) :sha256 (:sha256 entry)}))
        lookup (fn [table reference schema origin]
                 (let [id (:id reference) entry (get table id)
                       record (configured-record entry id)]
                   (decorate record schema origin entry)))]
    {:root-resolver #(let [entry (get roots (:id %))]
                       (if (= :host-session-event (:kind entry))
                         (configured-host-event-resolver (assoc entry :verification-scope verification-scope) %)
                         (lookup roots % :wm/external-root-resolution-v1 :configured-host-root)))
     :commission-resolver
     (fn [job-id]
       {:status :verified :authority/schema :wm/invoke-commission-resolution-v1
        :authority-origin :agency-request-commission-api
        :verification-scope verification-scope
        :provenance {:api "futon3c.transport.http/invoke-job-request-commission"
                     :job-id job-id}
        :resolved-job-id job-id :envelope (http/invoke-job-request-commission job-id)})
     :trace-resolver #(lookup traces {:id %} :wm/trace-resolution-v1 :configured-trace-store)
     :artifact-resolver #(lookup artifacts {:id (:id %)} :wm/artifact-byte-resolution-v1
                                 :configured-artifact-store)
     :acceptance-resolver #(lookup acceptances % :wm/delegated-acceptance-resolution-v1
                                  :configured-review-store)
     :predecessor-resolver #(lookup predecessors {:id (:anchor-id %)}
                                     :wm/anchored-checker-resolution-v1
                                     :configured-anchor-store)}))
