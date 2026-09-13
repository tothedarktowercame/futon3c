(ns futon3c.agency.invoke-lifecycle-snapshot
  "Isolated atomic source producer. All provider reads participate in one owned
  boundary. It emits source bytes and a coverage subject, never completeness
  approval and never restart authorization."
  (:require [clojure.edn :as edn])
  (:import (java.io PushbackReader StringReader)
           (java.nio.charset StandardCharsets)
           (java.security MessageDigest)
           (java.util.concurrent.locks ReentrantLock)))

(def source-schemas
  {:controller :agency/ingress-controller-snapshot-v1
   :hot-ledger :agency/invoke-hot-ledger-snapshot-v1
   :accepted-queue :agency/accepted-queue-snapshot-v1
   :execution :agency/execution-snapshot-v1
   :final-delivery :agency/final-delivery-snapshot-v1
   :deferred :agency/deferred-resume-snapshot-v1})

(defn- refuse! [code data]
  (throw (ex-info (name code) (assoc data :refusal code))))
(defn- sha256 [^bytes bs]
  (apply str (map #(format "%02x" (bit-and 255 %))
                  (.digest (doto (MessageDigest/getInstance "SHA-256") (.update bs))))))
(defmacro ^:private with-lock [b & body]
  `(let [^ReentrantLock lock# (:lock ~b)]
     (.lock lock#)
     (try ~@body (finally (.unlock lock#)))))

(defn boundary
  [{:keys [owner-id generation scope]}]
  (when-not (and (string? owner-id) (not-empty owner-id)
                 (integer? generation) (= :isolated-fixture scope))
    (refuse! :snapshot/boundary-config-invalid {}))
  {:owner-id owner-id :scope scope :lock (ReentrantLock.)
   :generation (atom generation) :providers (atom {})
   :operation (atom nil) :poisoned? (atom false)})

(defn- enter! [b operation]
  (when @(:poisoned? b) (refuse! :snapshot/boundary-poisoned {}))
  (when-not (compare-and-set! (:operation b) nil operation)
    (refuse! :snapshot/reentrant-operation {:active @(:operation b) :requested operation})))
(defn- leave! [b] (reset! (:operation b) nil))

(defn- edn-roundtrip! [kind record]
  (try
    (let [text (pr-str record)]
      (with-open [r (PushbackReader. (StringReader. text))]
        (let [back (edn/read {:eof ::eof} r) trailing (edn/read {:eof ::eof} r)]
          (when-not (and (= record back) (= ::eof trailing))
            (refuse! :snapshot/provider-record-unserializable {:kind kind}))))
    (catch clojure.lang.ExceptionInfo e (throw e))
    (catch Throwable e
      (throw (ex-info "provider record is not strict EDN"
                      {:refusal :snapshot/provider-record-unserializable :kind kind} e)))))

(defn register-provider!
  "Register one source whose mutation and capture are owned by BOUNDARY.
  REVISION returns an independently maintained provider revision before/after
  capture; CAPTURE returns the schema record at the boundary generation."
  [b kind {:keys [owner-id capture revision] :as provider}]
  (with-lock b
    (when-not (contains? source-schemas kind)
      (refuse! :snapshot/provider-kind-unknown {:kind kind}))
    (when-not (and (= (:owner-id b) owner-id) (fn? capture) (fn? revision))
      (refuse! :snapshot/provider-uncoordinated {:kind kind}))
    (when (contains? @(:providers b) kind)
      (refuse! :snapshot/provider-duplicate {:kind kind}))
    (swap! (:providers b) assoc kind provider)
    true))

(defn mutate!
  "Only supported provider mutation boundary. F runs while capture is excluded;
  generation advances after F returns."
  [b f]
  (with-lock b
    (enter! b :mutation)
    (try
      (let [{next-generation :generation result :result :as outcome} (f)
            current @(:generation b)]
        (when-not (and (map? outcome) (integer? next-generation)
                       (> next-generation current))
          (reset! (:poisoned? b) true)
          (refuse! :snapshot/mutation-generation-unproved
                   {:current current :outcome outcome}))
        (reset! (:generation b) next-generation)
        result)
      (catch Throwable e
        (reset! (:poisoned? b) true)
        (throw e))
      (finally (leave! b)))))

(defn capture!
  "Capture all six records under the common lock. Production refuses until its
  writers actually participate. Returned coverage is a subject for an external
  authority, not an approval."
  [b]
  (with-lock b
    (enter! b :capture)
    (try
     (let [providers @(:providers b)
          missing (set (remove (set (keys providers)) (keys source-schemas)))]
      (when (seq missing) (refuse! :snapshot/providers-incomplete {:missing missing}))
      (let [generation @(:generation b)
            before (into {} (map (fn [[k p]] [k ((:revision p))]) providers))
            records (into {} (map (fn [[kind p]] [kind ((:capture p) generation)]) providers))
            after (into {} (map (fn [[k p]] [k ((:revision p))]) providers))]
        (when-not (= before after)
          (refuse! :snapshot/concurrent-provider-change {:before before :after after}))
        (doseq [[kind record] records]
          (when-not (and (= (source-schemas kind) (:schema record))
                         (= generation (:generation record))
                         (= (:scope b) (:scope record))
                         (= (:owner-id b) (get-in record [:provenance :owner-id])))
            (refuse! :snapshot/provider-record-invalid {:kind kind})))
        (doseq [[kind record] records] (edn-roundtrip! kind record))
        (let [sources (into {}
                            (map (fn [[kind record]]
                                   (let [text (pr-str record)
                                         bytes (.getBytes text StandardCharsets/UTF_8)]
                                     [kind {:edn text :sha256 (sha256 bytes)
                                            :record record}]))) records)
              jobs (get-in records [:hot-ledger :jobs])
              universe (mapv (fn [[id job]] {:job-id id :trace-id (:trace-id job)})
                             (sort-by key jobs))]
          {:schema :agency/atomic-lifecycle-capture-v1
           :scope (:scope b) :owner-id (:owner-id b) :generation generation
           :sources sources
           :coverage-subject {:generation generation
                              :source-digests (into {} (map (fn [[k v]] [k (:sha256 v)]) sources))
                              :job-universe universe}
           :completeness-authority :absent
           :restart-authorized? false})))
     (finally (leave! b)))))

(defn capture-resolvers
  "Adapt one immutable capture to the reconciliation resolver interface."
  [capture]
  (into {} (map (fn [[kind {:keys [edn sha256]}]]
                  (let [private-bytes (.getBytes ^String edn StandardCharsets/UTF_8)]
                    [kind (fn [] {:bytes (aclone ^bytes private-bytes) :expected-sha256 sha256
                                :path (str "atomic-capture:" (name kind))})]))
                (:sources capture))))
