(ns futon3c.arxana.write
  "C2 validity boundary for immutable Formal Arxana writes."
  (:require [futon3c.arxana.envelope :as env]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.store :as store]))

(defn- refusal [category code detail]
  {:error (env/error category code detail nil)})

(defn- checked-clusion [record]
  (let [computed (records/make-clusion record)]
    (when (and (:id record) (not= (:id record) (:id computed)))
      (throw (ex-info "clusion id does not match semantic digest"
                      {:error :arxana.clusion/id-mismatch
                       :supplied (:id record) :computed (:id computed)})))
    computed))

(defn- checked-attestation [record]
  (let [computed (records/make-attestation record)]
    (when (and (:id record) (not= (:id record) (:id computed)))
      (throw (ex-info "attestation id does not match digest"
                      {:error :arxana.evidence/id-mismatch
                       :supplied (:id record) :computed (:id computed)})))
    computed))

(defn- checked-assessment [record]
  (let [computed (records/make-assessment record)]
    (when (and (:id record) (not= (:id record) (:id computed)))
      (throw (ex-info "assessment id does not match digest"
                      {:error :arxana.assessment/id-mismatch
                       :supplied (:id record) :computed (:id computed)})))
    computed))

(defn- checked-attempt [record]
  (let [computed (records/make-attempt record)]
    (when (and (:id record) (not= (:id record) (:id computed)))
      (throw (ex-info "attempt id does not match digest"
                      {:error :arxana.attempt/id-mismatch})))
    computed))

(defn- checked-external-reference [record]
  (let [computed (records/make-external-reference record)]
    (when (and (:id record) (not= (:id record) (:id computed)))
      (throw (ex-info "external reference id does not match digest"
                      {:error :arxana.external-reference/id-mismatch})))
    computed))

(defn- checked-mark-occurrence [record]
  (let [computed (records/make-mark-occurrence record)]
    (when (and (:id record) (not= (:id record) (:id computed)))
      (throw (ex-info "mark occurrence id does not match digest"
                      {:error :arxana.mark-occurrence/id-mismatch})))
    computed))

(def kernel-evidence-principals
  "Principals allowed to append basis :kernel evidence. Lean results enter
  Arxana only through the evidence adapter (arxana-wiring.edn: lean-kernel ->
  evidence-adapter -> write-ports); any other writer is refused here, at the
  validity authority."
  #{"formal-f3a/evidence-adapter"})

(defn- check-kernel-provenance! [envelope operation record]
  (when (and (= :evidence/append operation)
             (= :kernel (:basis record))
             (not (kernel-evidence-principals (:principal/id envelope))))
    (throw (ex-info "kernel evidence must come through the evidence adapter"
                    {:error :arxana.evidence/kernel-without-adapter
                     :principal/id (:principal/id envelope)}))))

(defn execute
  "Validate at the JVM authority, then submit only the validated record to
  futon1b's reserved atomic route."
  [graph-store envelope operation record]
  (try
    (check-kernel-provenance! envelope operation record)
    (let [validated (case operation
                      :clusion/assert (checked-clusion record)
                      :evidence/append (checked-attestation record)
                      :assessment/append (checked-assessment record)
                      :attempt/append (checked-attempt record)
                      :external-reference/assert (checked-external-reference record)
                      :mark-occurrence/assert (checked-mark-occurrence record))
          command {:protocol/version env/protocol-version
                   :message/id (:message/id envelope)
                   :operation operation
                   :principal/id (:principal/id envelope)
                   :body {:record validated}}]
      (store/conditional-write! graph-store command))
    (catch clojure.lang.ExceptionInfo e
      (refusal :invalid (or (:error (ex-data e)) :arxana.write/invalid-record)
               (dissoc (ex-data e) :error)))
    (catch Throwable e
      (refusal :invalid :arxana.write/invalid-record {:message (.getMessage e)}))))
