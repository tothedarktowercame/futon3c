(ns futon3c.arxana.compile.compiler
  "D6 action compiler (INSTANTIATE increment 7, Compile).

  Compiles a grounded candidate action from a source act, a
  provenance-bearing intent, and grounding evidence (artifact revision,
  resolution assertion, correspondence assertions). Read-only: no bearer
  credential, no executor, no policy decision, no store write, no side
  effect of any kind (D6: the candidate is immutable and non-executable).

  Grounding is never stronger than the Resolve/Correspond evidence it cites
  (D6-DR3): ambiguous or unverified grounding is retained as
  alternatives/unresolved obligations or refuses with typed obligations — it
  never silently selects. Compilation outcome is exactly as strong as the
  weakest cited evidence.

  Extensions are namespaced and opaque: unknown namespaces are preserved as
  input evidence only and cannot alter core semantics, redefine core fields,
  or change the candidate ID (D6 tripwires 5/6, invariant 9).

  Result algebra (D6): :compiled | :conditional | :ambiguous |
  :stale-input | :unsupported | :invalid."
  (:require [futon3c.arxana.compile.intent :as intent]
            [futon3c.arxana.digest :as digest]))

(def compiler-version
  "Digest-identified compiler version; behaviour change is a new identity."
  (str "arxana:action:sha256:"
       (digest/canonical-digest
        {:compiler :arxana.action/v1
         :result-algebra [:compiled :conditional :ambiguous
                          :stale-input :unsupported :invalid]
         :core-fields [:candidate-action/id :operation :preconditions
                       :predicted-effects :warrants :risks :unresolved
                       :compiler :extensions]})))

;;; Extension validation -----------------------------------------------------------

(def ^:private core-namespaces
  "Namespaces an extension may not contribute keys in (invariant 9)."
  #{"arxana" "intent" "operation" "candidate-action" "compiler"
    "preconditions" "predicted-effects" "warrants" "risks" "unresolved"
    "admission" "receipt" "evidence" "artifact" "resolution" "correspondence"
    "source-act"})

(defn- core-field?
  "Any key the compiler itself emits or interprets, whether qualified or
  bare, is core and closed to extension redefinition."
  [k]
  (or (not (keyword? k))
      (nil? (namespace k))
      (contains? core-namespaces (namespace k))))

(defn validate-extensions
  "Extensions must be a map of namespaced, non-core keys to opaque values.
  Returns nil when valid, else an error detail map. An extension key that
  redefines a core field is refused, never shadowed (tripwire 6)."
  [extensions]
  (cond
    (nil? extensions) nil
    (not (map? extensions)) {:reason "extensions must be a map"}
    :else (when-let [bad (seq (filter core-field? (keys extensions)))]
            {:reason "extension keys must be namespaced and must not redefine core fields"
             :offending-keys (vec bad)})))

;;; Grounding assessment -------------------------------------------------------------

(defn- resolution-status
  "Classify a cited D2 resolution assertion. Acceptance (an attributable
  resolution-acceptance record) is required to cite ambiguous or unverified
  evidence (D2/D6); accepting evidence never admits an action."
  [a]
  (cond
    (not (map? a)) :invalid
    (:resolution/assertion-stale? a) :stale
    (and (contains? #{:ambiguous :unverified} (:evidence/outcome a))
         (not (:resolution-acceptance/id a))) :unaccepted
    :else :grounded))

(defn- correspondence-status
  "Classify a cited D4 correspondence assertion/assessment by its
  verification class (:verified/:unverified/:ambiguous/:stale/:partial)."
  [a]
  (let [v (:verification a)]
    (cond
      (not (map? a)) :invalid
      (= :stale v) :stale
      (contains? #{:ambiguous :unverified :partial} v) :weak
      (= :verified v) :grounded
      :else :invalid)))

(defn- assess-grounding
  "Returns {:status :grounded|:conditional|:ambiguous|:stale-input|:invalid
            :obligations [...] :alternatives [...]}.
  Grounding never exceeds the evidence: ambiguous resolution yields
  alternatives, never first-candidate selection (tripwire 3)."
  [grounding]
  (let [res (:resolution-assertion grounding)
        corrs (:correspondence-assertions grounding)
        res-status (resolution-status res)
        corr-statuses (mapv correspondence-status corrs)
        obligations (transient [])
        alternatives (transient [])
        add! (fn [kind detail] (conj! obligations {:type kind :detail detail}))]
    (case res-status
      :invalid (add! :resolution/invalid {:reason "resolution assertion malformed"})
      :stale (add! :resolution/stale {:assertion/id (:resolution-assertion/id res)})
      :unaccepted (add! :resolution/acceptance-required
                        {:assertion/id (:resolution-assertion/id res)
                         :outcome (:evidence/outcome res)})
      :grounded nil)
    (when (= :ambiguous (:evidence/outcome res))
      ;; ambiguity is retained as alternatives whether or not an acceptance
      ;; record permits citing it — never collapsed to a first candidate
      (doseq [alt (:evidence/alternatives res)]
        (conj! alternatives alt)))
    (doseq [[a s] (map vector corrs corr-statuses)]
      (case s
        :invalid (add! :correspondence/invalid {:reason "correspondence assertion malformed"})
        :stale (add! :correspondence/stale {:assertion/id (:correspondence-assertion/id a)})
        :weak (add! :correspondence/verification-obligation
                    {:assertion/id (:correspondence-assertion/id a)
                     :verification (:verification a)})
        :grounded nil))
    (let [obs (persistent! obligations)
          alts (persistent! alternatives)]
      {:status (cond
                 (some #(= :resolution/invalid (:type %)) obs) :invalid
                 (some #(= :correspondence/invalid (:type %)) obs) :invalid
                 (some #(#{:resolution/stale :correspondence/stale} (:type %)) obs) :stale-input
                 (and (seq alts) (empty? obs)) :ambiguous
                 (seq obs) :conditional
                 (seq alts) :ambiguous
                 :else :grounded)
       :obligations obs
       :alternatives alts})))

;;; Candidate construction -----------------------------------------------------------

(defn- semantic-candidate-payload
  "The canonical semantic inputs of a candidate (D6: ID digests canonical
  semantic inputs including compiler version, excluding request identity and
  presentation — and excluding extensions, which cannot alter core)."
  [{:keys [intent-record operation preconditions predicted-effects warrants
           risks unresolved grounding]}]
  {:intent/id (:intent/id intent-record)
   :operation operation
   :preconditions preconditions
   :predicted-effects predicted-effects
   :warrants warrants
   :risks risks
   :unresolved unresolved
   :grounding {:artifact/revision-id (:artifact/revision-id grounding)
               :resolution-assertion/id (:resolution-assertion/id
                                         (:resolution-assertion grounding))
               :correspondence-assertion/ids
               (mapv :correspondence-assertion/id
                     (:correspondence-assertions grounding))}
   :compiler/version compiler-version})

(defn- derive-operation
  "Build the candidate operation from intent + grounding. Supported intent
  types: :edit/replace → :artifact/replace-range over the grounded target
  manifestation/revision. Anything else is `unsupported` (tripwire:
  unsupported semantics fail explicitly, never no-op success)."
  [intent-record grounding]
  (case (:intent/type intent-record)
    :edit/replace
    {:type :artifact/replace-range
     :target (merge (:intent/target-ref intent-record)
                    {:artifact/revision-id (:artifact/revision-id grounding)})
     :payload (:intent/proposed-change intent-record)}
    ::unsupported))

(defn compile-action
  "Compile a D6 compiler input into a candidate-action or a typed refusal.

  Input:
    {:source-act/id evidence-id
     :intent intent-record            ; durable, provenance-bearing
     :trust-profile profile           ; versioned interpretation trust policy
     :grounding {:artifact/revision-id ...
                 :resolution-assertion {...}
                 :correspondence-assertions [...]}
     :operation/overrides {optional consumer-supplied operation core}
     :extensions {opaque namespaced extras}}

  Returns {:status <result-algebra> :candidate-action? ... :obligations ...
           :alternatives ...}. Pure and deterministic: same semantic input →
  same candidate ID (tripwire 1); no timestamp, request ID, or presentation
  enters the digest."
  [{id :source-act/id overrides :operation/overrides
    :keys [intent trust-profile grounding extensions]}]
  (cond
    (not (string? id))
    {:status :invalid :detail {:reason "source-act/id (evidence ID) is required"}}

    (not= id (:intent/source-act-id intent))
    {:status :invalid
     :detail {:reason "intent does not name the source act it interprets"
              :source-act/id id :intent/source-act-id (:intent/source-act-id intent)}}

    (not (and (map? intent)
              (keyword? (:intent/type intent))
              (map? (:intent/target-ref intent))
              (contains? intent :intent/proposed-change)))
    {:status :invalid :detail {:reason "intent is malformed"}}

    :else
    (let [ext-err (validate-extensions extensions)]
      (if ext-err
        {:status :unsupported :detail ext-err}
        (let [trust (intent/trust-status trust-profile intent)]
          (if (= trust :unprovenanced)
            {:status :invalid
             :detail {:reason "unprovenanced intent cannot compile (invariant 8)"}}
            (let [assessment (assess-grounding grounding)
                  g-status (:status assessment)]
              (if (= g-status :invalid)
                {:status :invalid :detail {:obligations (:obligations assessment)}}
                (let [operation (or overrides
                                    (derive-operation intent grounding))]
                  (if (= operation ::unsupported)
                    {:status :unsupported
                     :detail {:reason "no defined semantics for intent type"
                              :intent/type (:intent/type intent)}}
                    (let [unresolved (mapv (fn [o] {:unresolved/obligation o})
                                           (:obligations assessment))
                          warrants (cond-> [{:type :intent :id (:intent/id intent)}]
                                     (:resolution-assertion grounding)
                                     (conj {:type :resolution
                                            :id (:resolution-assertion/id
                                                 (:resolution-assertion grounding))})
                                     (seq (:correspondence-assertions grounding))
                                     (into (mapv (fn [a]
                                                   {:type :correspondence
                                                    :id (:correspondence-assertion/id a)})
                                                 (:correspondence-assertions grounding))))
                          preconditions [{:type :revision/head-equals
                                          :revision/id (:artifact/revision-id grounding)}
                                         {:type :resolution/accepted
                                          :assertion/id
                                          (:resolution-assertion/id
                                           (:resolution-assertion grounding))}]
                          predicted-effects (vec (:grounding/predicted-effects grounding))
                          risks (vec (:grounding/risks grounding))
                          payload (semantic-candidate-payload
                                   {:intent-record intent :operation operation
                                    :preconditions preconditions
                                    :predicted-effects predicted-effects
                                    :warrants warrants :risks risks
                                    :unresolved unresolved
                                    :grounding grounding})
                          cid (str "arxana:action:sha256:"
                                   (digest/canonical-digest payload))
                          candidate {:candidate-action/id cid
                                     :operation operation
                                     :preconditions preconditions
                                     :predicted-effects predicted-effects
                                     :warrants warrants
                                     :risks risks
                                     :unresolved unresolved
                                     :compiler {:version compiler-version
                                                :input-digest
                                                (str "sha256:"
                                                     (digest/canonical-digest payload))}
                                     :extensions (or extensions {})}
                          status (case g-status
                                   :stale-input :stale-input
                                   :ambiguous :ambiguous
                                   :conditional :conditional
                                   :grounded (if (= trust :trusted)
                                               :compiled
                                               :conditional))]
                      {:status status
                       :candidate-action (when-not (= status :stale-input)
                                           candidate)
                       :alternatives (:alternatives assessment)
                       :obligations (:obligations assessment)
                       :trust trust})))))))))))

;;; Non-executability -----------------------------------------------------------------

(def forbidden-candidate-keys
  "Keys that must never appear in a candidate: presence would make the
  description look like authorization or execution (D6-DR2, tripwire 4)."
  [:bearer-credential :admission-token :consent :executor :execute])

(defn non-executable?
  "A candidate is non-executable: it carries no bearer credential, admission
  token, or implicit consent, and admission/execution are separate records."
  [candidate]
  (and (map? candidate)
       (not-any? #(contains? candidate %) forbidden-candidate-keys)))
