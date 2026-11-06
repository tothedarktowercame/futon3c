(ns futon3c.arxana.compile.intent
  "D6/D7 intent interpretation records and trust profiles (INSTANTIATE
  increment 7, Compile).

  An intent is a durable, provenance-bearing interpretation of an immutable
  source act (D6-DR2): it names the source act by evidence ID, the
  interpreter identity/type, the interpretation method or model+version, a
  timestamp, alternatives and evidence. Reinterpretation creates a NEW
  record; the source act is never rewritten.

  Trust (D7-DR3, O-D7e): a versioned trust profile decides which interpreter
  identities Arxana compiles directly. A provenanced-but-untrusted or merely
  client-attested intent can yield only `conditional`, never `compiled`,
  until admitted under an explicit interpretation trust policy (single-locus
  invariant 8). An unprovenanced intent is invalid input.

  This namespace only constructs and classifies records; it owns no
  interpretation policy and performs no admission or execution."
  (:require [futon3c.arxana.digest :as digest]))

;;; Intent records ---------------------------------------------------------------

(def intent-record-version
  "Digest-identified intent record shape/semantics version."
  (str "arxana:intent:sha256:"
       (digest/canonical-digest
        {:record :intent/v1
         :fields [:intent/type :intent/source-act-id :intent/target-ref
                  :intent/proposed-change :intent/interpreter
                  :intent/alternatives]})))

(def ^:private required-intent-fields
  [:intent/type :intent/source-act-id :intent/target-ref
   :intent/proposed-change :intent/interpreter])

(defn valid-interpreter?
  "An interpreter descriptor is provenance: identity, type
  (:human/:model/:consumer/...), and method or model+version."
  [m]
  (and (map? m)
       (string? (:interpreter/id m))
       (keyword? (:interpreter/type m))
       (or (string? (:interpreter/method m))
           (and (string? (:interpreter/model m))
                (string? (:interpreter/model-version m))))))

(defn provenanced?
  "An intent is provenanced when it names its immutable source act and a
  well-formed interpreter (D6). Client attestation without an interpreter
  identity is NOT provenance."
  [intent]
  (and (map? intent)
       (string? (:intent/source-act-id intent))
       (valid-interpreter? (:intent/interpreter intent))))

(defn valid-intent?
  [intent]
  (and (provenanced? intent)
       (keyword? (:intent/type intent))
       (map? (:intent/target-ref intent))
       (contains? intent :intent/proposed-change)))

(defn make-intent
  "Mint a durable intent record. The intent ID digests semantic content only
  — source act, operation type, target, proposed change, interpreter
  provenance, alternatives — never a timestamp or request identity, so a
  faithful reinterpretation of the same act by the same interpreter with the
  same result is the same intent. `:intent/interpreted-at` travels as
  evidence, not identity."
  [{type :intent/type source-act-id :intent/source-act-id
    target-ref :intent/target-ref proposed-change :intent/proposed-change
    :keys [interpreter alternatives evidence interpreted-at]}]
  (let [semantic {:intent/type type
                  :intent/source-act-id source-act-id
                  :intent/target-ref target-ref
                  :intent/proposed-change proposed-change
                  :intent/interpreter interpreter
                  :intent/alternatives (vec (or alternatives []))}
        id (str "arxana:intent:sha256:" (digest/canonical-digest semantic))]
    (merge semantic
           {:intent/id id
            :intent/record-version intent-record-version
            :intent/interpreted-at interpreted-at
            :intent/evidence (vec (or evidence []))})))

;;; Trust profiles ---------------------------------------------------------------

(defn make-trust-profile
  "A versioned trust profile (D7-DR3). `trusted-interpreters` maps
  interpreter IDs to a truthy value; `:allow-client-attested?` (default
  false) permits merely client-attested intent to compile conditionally
  rather than be invalid. Nothing here admits an ACTION — evidence
  acceptance is never action admission (D6)."
  [{id :trust-profile/id version :trust-profile/version
    :keys [trusted-interpreters allow-client-attested?]}]
  {:trust-profile/id id
   :trust-profile/version version
   :trust-profile/trusted-interpreters (or trusted-interpreters {})
   :trust-profile/allow-client-attested? (boolean allow-client-attested?)})

(defn trust-status
  "Classify an intent under a trust profile:
   :trusted       — provenanced, interpreter trusted by the profile;
   :conditional   — provenanced but untrusted, or merely client-attested
                    when the profile allows it;
   :unprovenanced — no usable provenance (never compiles to `compiled`)."
  [trust-profile intent]
  (cond
    (and (provenanced? intent)
         (get (:trust-profile/trusted-interpreters trust-profile)
              (get-in intent [:intent/interpreter :interpreter/id])))
    :trusted

    (provenanced? intent)
    :conditional

    (:trust-profile/allow-client-attested? trust-profile)
    :conditional

    :else
    :unprovenanced))
