(ns futon3c.apm.transport-conformance
  "Pure EDN boundary for APM transport/publication transition certificates.

  The vocabulary and decision rule mirror
  `ConstructionTargets.APMTransportPublicationSpec`. Runtime certificate
  emission is intentionally deferred to Park 4."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import [java.nio.charset StandardCharsets]
           [java.nio.file CopyOption Files LinkOption OpenOption Path
            StandardCopyOption StandardOpenOption]
           [java.nio.file.attribute FileAttribute]
           [java.security MessageDigest]
           [java.util UUID]))

(def operations
  #{:read :write :publication :post-publication-verification})

(def observation-outcomes
  #{:success :timeout :unavailable :malformed
    :authoritative-absence :visibility-lag})

(def evidence-validities
  #{:obtained :not-obtained :invalid})

(def park-reasons
  #{:retry-exhausted :invalid-evidence :authoritative-absence})

(def certificate-keys
  #{:identity :operation :acquired-outcome :classified-outcome :evidence
    :attempt :max-attempts :wake-at-ms :history :last-valid-state
    :last-valid-evidence :decision})

(def identity-keys #{:spec-id :source-id :loaded-runtime-id})
(def history-entry-keys #{:attempt :operation :acquired-outcome :evidence})

(defn- natural-number? [value]
  (and (integer? value) (not (neg? value))))

(defn evidence-compatible?
  "Whether EVIDENCE is compatible with the observation actually acquired."
  [outcome evidence]
  (contains? #{[:success :obtained]
               [:authoritative-absence :obtained]
               [:malformed :invalid]
               [:timeout :not-obtained]
               [:unavailable :not-obtained]
               [:visibility-lag :not-obtained]}
             [outcome evidence]))

(defn transport-failure?
  "Whether OUTCOME is one of the two transport failures in the Lean model."
  [outcome]
  (contains? #{:timeout :unavailable} outcome))

(defn authoritative-absence?
  "Whether OUTCOME is the positive, obtained observation of absence."
  [outcome]
  (= :authoritative-absence outcome))

(defn needs-retry?
  "The three acquired-without-evidence outcomes eligible for bounded retry."
  [outcome evidence]
  (contains? #{[:timeout :not-obtained]
               [:unavailable :not-obtained]
               [:visibility-lag :not-obtained]}
             [outcome evidence]))

(defn decide
  "Return the canonical typed decision for RETRY, OUTCOME, and EVIDENCE.

  A retry is represented as `[:retry wake-at-ms]`; terminal decisions are
  `[:park reason]` and `[:advance]`, matching the Lean constructors."
  [{:keys [attempt max-attempts wake-at-ms]} outcome evidence]
  (if (needs-retry? outcome evidence)
    (if (< (inc attempt) max-attempts)
      [:retry wake-at-ms]
      [:park :retry-exhausted])
    (cond
      (= [:success :obtained] [outcome evidence]) [:advance]
      (= [:authoritative-absence :obtained] [outcome evidence])
      [:park :authoritative-absence]
      :else [:park :invalid-evidence])))

(defn schedule-retry
  "Append ENTRY and advance the attempt/wake without changing last-valid data."
  [retry entry next-wake-at-ms]
  (-> retry
      (update :attempt inc)
      (assoc :wake-at-ms next-wake-at-ms)
      (update :history conj entry)))

(defn retry-state
  "Project the retry fields used by `decide` from a certificate."
  [certificate]
  (select-keys certificate
               [:attempt :max-attempts :wake-at-ms :history
                :last-valid-state :last-valid-evidence]))

(defn- finding [code field value]
  {:error/code code :field field :value value})

(defn- identity-findings [identity]
  (cond
    (not (map? identity))
    [(finding :transport-conformance-identity-not-map :identity identity)]

    :else
    (cond-> []
      (not= identity-keys (set (keys identity)))
      (conj (finding :transport-conformance-identity-keys-invalid
                     :identity (set (keys identity))))
      (not-every? string? (map identity identity-keys))
      (conj (finding :transport-conformance-identity-value-invalid
                     :identity identity))
      (and (string? (:loaded-runtime-id identity))
           (or (= "unavailable" (:loaded-runtime-id identity))
               (= "unknown" (:loaded-runtime-id identity))))
      (conj (finding :transport-conformance-loaded-runtime-id-unavailable
                     :identity identity))
      (and (string? (:source-id identity))
           (string? (:loaded-runtime-id identity))
           (not= (:source-id identity) (:loaded-runtime-id identity)))
      (conj (finding :transport-conformance-runtime-identity-mismatch
                     :identity identity)))))

(defn- history-entry-findings [index entry]
  (let [at (fn [field] [:history index field])]
    (cond
      (not (map? entry))
      [(finding :transport-conformance-history-entry-not-map
                [:history index] entry)]

      :else
      (cond-> []
        (not= history-entry-keys (set (keys entry)))
        (conj (finding :transport-conformance-history-entry-keys-invalid
                       [:history index] (set (keys entry))))
        (not (natural-number? (:attempt entry)))
        (conj (finding :transport-conformance-attempt-invalid
                       (at :attempt) (:attempt entry)))
        (not (contains? operations (:operation entry)))
        (conj (finding :transport-conformance-operation-unknown
                       (at :operation) (:operation entry)))
        (not (contains? observation-outcomes (:acquired-outcome entry)))
        (conj (finding :transport-conformance-acquired-outcome-unknown
                       (at :acquired-outcome) (:acquired-outcome entry)))
        (not (contains? evidence-validities (:evidence entry)))
        (conj (finding :transport-conformance-evidence-unknown
                       (at :evidence) (:evidence entry)))))))

(defn- decision-findings [decision]
  (cond
    (not (vector? decision))
    [(finding :transport-conformance-decision-not-vector :decision decision)]

    (= [:advance] decision) []

    (= :retry (first decision))
    (cond-> []
      (not= 2 (count decision))
      (conj (finding :transport-conformance-decision-shape-invalid
                     :decision decision))
      (not (natural-number? (second decision)))
      (conj (finding :transport-conformance-retry-wake-invalid
                     :decision decision)))

    (= :park (first decision))
    (cond-> []
      (not= 2 (count decision))
      (conj (finding :transport-conformance-decision-shape-invalid
                     :decision decision))
      (not (contains? park-reasons (second decision)))
      (conj (finding :transport-conformance-park-reason-unknown
                     :decision decision)))

    :else
    [(finding :transport-conformance-decision-constructor-unknown
              :decision decision)]))

(defn certificate-findings
  "Return precise structural and semantic findings for CERTIFICATE.

  Structural errors suppress semantic comparison so malformed wire data never
  reaches `decide` and is never guessed into a classification."
  [certificate]
  (if-not (map? certificate)
    [(finding :transport-conformance-certificate-not-map
              :certificate certificate)]
    (let [structural
          (cond-> []
            (not= certificate-keys (set (keys certificate)))
            (conj (finding :transport-conformance-certificate-keys-invalid
                           :certificate (set (keys certificate))))
            true (into (identity-findings (:identity certificate)))
            (not (contains? operations (:operation certificate)))
            (conj (finding :transport-conformance-operation-unknown
                           :operation (:operation certificate)))
            (not (contains? observation-outcomes
                            (:acquired-outcome certificate)))
            (conj (finding :transport-conformance-acquired-outcome-unknown
                           :acquired-outcome (:acquired-outcome certificate)))
            (not (contains? observation-outcomes
                            (:classified-outcome certificate)))
            (conj (finding :transport-conformance-classified-outcome-unknown
                           :classified-outcome
                           (:classified-outcome certificate)))
            (not (contains? evidence-validities (:evidence certificate)))
            (conj (finding :transport-conformance-evidence-unknown
                           :evidence (:evidence certificate)))
            (not (natural-number? (:attempt certificate)))
            (conj (finding :transport-conformance-attempt-invalid
                           :attempt (:attempt certificate)))
            (not (natural-number? (:max-attempts certificate)))
            (conj (finding :transport-conformance-max-attempts-invalid
                           :max-attempts (:max-attempts certificate)))
            (not (natural-number? (:wake-at-ms certificate)))
            (conj (finding :transport-conformance-wake-at-ms-invalid
                           :wake-at-ms (:wake-at-ms certificate)))
            (not (vector? (:history certificate)))
            (conj (finding :transport-conformance-history-not-vector
                           :history (:history certificate)))
            (vector? (:history certificate))
            (into (mapcat (fn [[index entry]]
                            (history-entry-findings index entry))
                          (map-indexed vector (:history certificate))))
            true (into (decision-findings (:decision certificate))))]
      (if (seq structural)
        structural
        (cond-> []
          (not= (:classified-outcome certificate)
                (:acquired-outcome certificate))
          (conj (finding :transport-conformance-outcome-mismatch
                         :classified-outcome
                         {:acquired (:acquired-outcome certificate)
                          :classified (:classified-outcome certificate)}))
          (not (evidence-compatible? (:acquired-outcome certificate)
                                     (:evidence certificate)))
          (conj (finding :transport-conformance-evidence-incompatible
                         :evidence
                         {:outcome (:acquired-outcome certificate)
                          :evidence (:evidence certificate)}))
          (not= (:decision certificate)
                (decide (retry-state certificate)
                        (:acquired-outcome certificate)
                        (:evidence certificate)))
          (conj (finding :transport-conformance-decision-mismatch
                         :decision
                         {:expected (decide (retry-state certificate)
                                            (:acquired-outcome certificate)
                                            (:evidence certificate))
                          :actual (:decision certificate)})))))))

(defn validate-certificate [certificate]
  (let [findings (certificate-findings certificate)]
    (if (seq findings)
      {:ok false :error/code :transport-conformance-certificate-invalid
       :findings findings}
      {:ok true :certificate certificate})))

(defn conformant? [certificate]
  (:ok (validate-certificate certificate)))

(defn retry-then-success-findings
  "Validate the two-certificate sequencing predicate used by the f64 fixture."
  [failure success]
  (let [failure-result (validate-certificate failure)
        success-result (validate-certificate success)
        expected-entry {:attempt (:attempt failure)
                        :operation (:operation failure)
                        :acquired-outcome (:acquired-outcome failure)
                        :evidence (:evidence failure)}]
    (cond-> []
      (not (:ok failure-result))
      (conj {:error/code :transport-conformance-failure-certificate-invalid
             :findings (:findings failure-result)})
      (not (:ok success-result))
      (conj {:error/code :transport-conformance-success-certificate-invalid
             :findings (:findings success-result)})
      (not= (:decision failure) [:retry (:wake-at-ms success)])
      (conj (finding :transport-conformance-retry-wake-sequence-mismatch
                     :decision (:decision failure)))
      (not= (:attempt success) (inc (:attempt failure)))
      (conj (finding :transport-conformance-attempt-sequence-mismatch
                     :attempt [(:attempt failure) (:attempt success)]))
      (not= (:history success) (conj (:history failure) expected-entry))
      (conj (finding :transport-conformance-history-sequence-mismatch
                     :history (:history success))))))

(def recovered-identity
  {:spec-id "apm-transport-publication/v1"
   :source-id "recovered-source"
   :loaded-runtime-id "recovered-source"})

(def f63-historical-certificate
  {:identity recovered-identity
   :operation :post-publication-verification
   :acquired-outcome :visibility-lag
   :classified-outcome :authoritative-absence
   :evidence :not-obtained
   :attempt 0 :max-attempts 1 :wake-at-ms 0 :history []
   :last-valid-state "last-valid-frame"
   :last-valid-evidence "persisted-review"
   :decision [:park :authoritative-absence]})

(def f64-transport-failure
  {:identity recovered-identity
   :operation :write
   :acquired-outcome :timeout
   :classified-outcome :timeout
   :evidence :not-obtained
   :attempt 0 :max-attempts 2 :wake-at-ms 120000 :history []
   :last-valid-state "last-valid-frame"
   :last-valid-evidence "independent-review"
   :decision [:retry 120000]})

(def f64-successful-visibility
  {:identity recovered-identity
   :operation :post-publication-verification
   :acquired-outcome :success
   :classified-outcome :success
   :evidence :obtained
   :attempt 1 :max-attempts 2 :wake-at-ms 120000
   :history [{:attempt 0 :operation :write
              :acquired-outcome :timeout :evidence :not-obtained}]
   :last-valid-state "published-frame"
   :last-valid-evidence "visible-review"
   :decision [:advance]})

(defn adapt-legacy-finding
  "Classify only legacy findings whose operation and acquired outcome are
  explicit. Ambiguous transport/visibility shapes remain unclassified."
  [legacy]
  (cond
    (not (map? legacy))
    {:ok false :error/code :transport-conformance-legacy-finding-malformed}

    (= :futon1b-read-timeout (:error/code legacy))
    {:ok true :operation :read :acquired-outcome :timeout
     :evidence :not-obtained}

    (or (= :transport (:error/component legacy))
        (= :memory-snapshot-review-not-visible (:error/code legacy))
        (= :snapshot-review-not-visible (:finding legacy)))
    {:ok false :error/code :transport-conformance-legacy-finding-ambiguous
     :legacy legacy}

    :else
    {:ok false :error/code :transport-conformance-legacy-finding-unclassified
     :legacy legacy}))

(defn- sha256-bytes [bytes]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256") bytes)]
    (apply str (map #(format "%02x" (bit-and (int %) 0xff)) digest))))

(defn source-resource-id
  "SHA-256 the exact classpath resource bytes, or return nil when the running
  classloader cannot identify that source. This never substitutes git HEAD."
  [resource-name]
  (when-let [resource (io/resource resource-name)]
    (with-open [stream (.openStream resource)]
      (sha256-bytes (.readAllBytes stream)))))

(defn implementation-identity
  "Build the v1 identity from a freshly read source id and a load-time id.
  Missing identities are represented explicitly and rejected by validation."
  [source-id loaded-runtime-id]
  {:spec-id "apm-transport-publication/v1"
   :source-id (or source-id "unavailable")
   :loaded-runtime-id (or loaded-runtime-id "unavailable")})

(def record-keys
  #{:record/type :record/id :emitted-at-ms :correlation :certificate
    :validation})

(def correlation-keys #{:frame-id :problem-id :phase :attempt})

(defn- atomic-create! [^Path target text]
  (let [directory (.getParent target)
        _ (Files/createDirectories directory (make-array FileAttribute 0))
        temporary (Files/createTempFile directory ".transport-certificate-"
                                        ".edn" (make-array FileAttribute 0))]
    (try
      (Files/writeString temporary text StandardCharsets/UTF_8
                         (into-array OpenOption [StandardOpenOption/WRITE
                                                 StandardOpenOption/TRUNCATE_EXISTING
                                                 StandardOpenOption/SYNC]))
      (Files/move temporary target
                  (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE]))
      {:ok true :path (str target)}
      (catch Throwable t
        {:ok false :error/code :transport-certificate-persistence-failed
         :path (str target) :error/message (.getMessage t)})
      (finally (Files/deleteIfExists temporary)))))

(defn persist-certificate!
  "Validate CERTIFICATE, retain that validation beside it, and atomically
  create a unique immutable record. Invalid certificates are evidence and are
  persisted rather than accepted as successful certificates."
  [directory correlation certificate emitted-at-ms]
  (let [validation (validate-certificate certificate)
        record-id (str emitted-at-ms "-" (:attempt correlation) "-" (UUID/randomUUID))
        record {:record/type :transport-publication-certificate
                :record/id record-id
                :emitted-at-ms emitted-at-ms
                :correlation correlation
                :certificate certificate
                :validation validation}
        target (.resolve (Path/of (str directory) (make-array String 0))
                         (str record-id ".edn"))
        written (atomic-create! target (str (pr-str record) "\n"))]
    (assoc written :record record :certificate-valid? (:ok validation))))

(defn- record-findings [record]
  (cond
    (not (map? record))
    [(finding :transport-certificate-record-not-map :record record)]

    (not= record-keys (set (keys record)))
    [(finding :transport-certificate-record-keys-invalid
              :record (set (keys record)))]

    (not= correlation-keys (set (keys (:correlation record))))
    [(finding :transport-certificate-correlation-invalid
              :correlation (:correlation record))]

    :else
    (let [correlation (:correlation record)
          validation (validate-certificate (:certificate record))]
      (cond-> []
        (or (not-every? #(and (string? %) (not-empty %))
                        ((juxt :frame-id :problem-id) correlation))
            (not (keyword? (:phase correlation)))
            (not (natural-number? (:attempt correlation)))
            (not= (:attempt correlation)
                  (get-in record [:certificate :attempt])))
        (conj (finding :transport-certificate-correlation-invalid
                       :correlation correlation))
        (not (:ok validation))
        (conj {:error/code :transport-certificate-invalid
               :record/id (:record/id record)
               :findings (:findings validation)})
        (not= validation (:validation record))
        (conj (finding :transport-certificate-validation-mismatch
                       :validation (:validation record)))))))

(defn replay-records
  "Offline validation for already parsed records. Checks each certificate and
  monotone bounded attempts/history within each frame/problem/phase stream."
  [records]
  (let [ordered (sort-by (juxt :emitted-at-ms :record/id) records)
        individual (mapcat record-findings ordered)
        groups (vals (group-by #(select-keys (:correlation %)
                                             [:frame-id :problem-id :phase])
                               ordered))
        sequencing
        (mapcat
         (fn [group]
           (mapcat
            (fn [[previous current]]
              (let [a (get-in previous [:certificate :attempt])
                    b (get-in current [:certificate :attempt])
                    max-attempts (get-in current [:certificate :max-attempts])]
                (cond-> []
                  (not= b (inc a))
                  (conj (finding :transport-certificate-attempt-sequence-mismatch
                                 :attempt [a b]))
                  (or (not (integer? b)) (not (integer? max-attempts))
                      (>= b max-attempts))
                  (conj (finding :transport-certificate-attempt-out-of-bounds
                                 :attempt [b max-attempts]))
                  (not= (get-in current [:certificate :history])
                        (conj (vec (get-in previous [:certificate :history]))
                              {:attempt a
                               :operation (get-in previous [:certificate :operation])
                               :acquired-outcome
                               (get-in previous [:certificate :acquired-outcome])
                               :evidence (get-in previous [:certificate :evidence])}))
                  (conj (finding :transport-certificate-history-sequence-mismatch
                                 :history (get-in current [:certificate :history]))))))
            (partition 2 1 group)))
         groups)
        findings (vec (concat individual sequencing))]
    (if (seq findings)
      {:ok false :error/code :transport-certificate-replay-failed
       :records (count records) :findings findings}
      {:ok true :records (count records)})))

(defn replay-directory
  "Read every *.edn certificate record in DIRECTORY and replay fail closed."
  [directory]
  (try
    (let [dir (Path/of (str directory) (make-array String 0))
          paths (if (Files/isDirectory dir (make-array LinkOption 0))
                  (with-open [stream (Files/list dir)]
                    (->> (.toList stream)
                         (filter #(and (Files/isRegularFile ^Path %
                                                           (make-array LinkOption 0))
                                       (.endsWith (str %) ".edn")))
                         (sort-by str)
                         vec))
                  [])
          records (mapv #(edn/read-string (Files/readString ^Path %)) paths)]
      (replay-records records))
    (catch Throwable t
      {:ok false :error/code :transport-certificate-replay-read-failed
       :error/message (.getMessage t)})))
