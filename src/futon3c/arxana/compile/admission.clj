(ns futon3c.arxana.compile.admission
  "D6 admission-decision and action-receipt shapes (INSTANTIATE increment 7).

  Arxana defines the interoperable SHAPES only; it owns no institutional
  policy and no executor (D6 admission/execution boundary). Both records are
  immutable values. An admission decision is a separate record from the
  candidate — evidence acceptance never admits an action, and no candidate
  can be submitted to execution without a separate admitted decision plus a
  current expected revision (tripwire 4). A receipt is append-only and never
  retroactively rewrites the candidate's predicted effects (tripwire 7,
  invariant 8)."
  (:require [futon3c.arxana.digest :as digest]))

(def admission-statuses #{:admitted :refused :deferred :consent-required})

(defn make-admission-decision
  "Construct an admission-decision value naming the deciding policy and
  policy revision, actor, reasons, conditions and candidate ID. Pure data;
  constructing a decision here confers no authority — the external policy
  authority is the sole source of real decisions (D7 authority table)."
  [{status :decision/status policy-id :policy/id policy-rev :policy/revision
    :keys [actor reasons conditions]
    candidate-id :candidate-action/id}]
  (when-not (contains? admission-statuses status)
    (throw (ex-info "unknown admission status" {:status status})))
  (when-not (string? candidate-id)
    (throw (ex-info "admission decision must name a candidate-action ID" {})))
  (let [semantic {:decision/status status
                  :policy/id policy-id
                  :policy/revision policy-rev
                  :decision/actor actor
                  :decision/reasons (vec (or reasons []))
                  :decision/conditions (vec (or conditions []))
                  :candidate-action/id candidate-id}]
    (assoc semantic
           :admission/id
           (str "arxana:admission:sha256:" (digest/canonical-digest semantic)))))

(defn valid-decision?
  [d]
  (and (map? d)
       (string? (:admission/id d))
       (contains? admission-statuses (:decision/status d))
       (string? (:candidate-action/id d))))

(def receipt-outcomes #{:applied :refused :failed :partial})

(defn make-receipt
  "Construct an append-only action receipt naming request/operation ID,
  candidate and decision IDs, before/after revisions, actual effects,
  deviations, outcome and evidence.

  A receipt CANNOT rewrite a prediction: it carries actual effects and
  deviations, and the constructor throws if handed predicted effects
  (tripwire 7). It also cannot fabricate admission: a decision ID is
  required."
  [{rev-before :revision/before rev-after :revision/after
    :keys [actual-effects deviations outcome evidence]
    request-id :request/id operation-id :operation/id
    candidate-id :candidate-action/id admission-id :admission/id
    :as receipt}]
  (when (or (contains? receipt :predicted-effects)
            (contains? receipt :receipt/predicted-effects))
    (throw (ex-info "a receipt records actual effects and deviations; it never rewrites predicted effects"
                    {:receipt receipt})))
  (when-not (contains? receipt-outcomes outcome)
    (throw (ex-info "unknown receipt outcome" {:outcome outcome})))
  (when-not (and (string? candidate-id) (string? admission-id))
    (throw (ex-info "receipt must name candidate and admission decision IDs" {})))
  (let [semantic {:request/id request-id
                  :operation/id operation-id
                  :candidate-action/id candidate-id
                  :admission/id admission-id
                  :revision/before rev-before
                  :revision/after rev-after
                  :receipt/actual-effects (vec (or actual-effects []))
                  :receipt/deviations (vec (or deviations []))
                  :receipt/outcome outcome
                  :receipt/evidence (vec (or evidence []))}]
    (assoc semantic
           :receipt/id
           (str "arxana:receipt:sha256:" (digest/canonical-digest semantic)))))

(defn valid-receipt?
  [r]
  (and (map? r)
       (string? (:receipt/id r))
       (contains? receipt-outcomes (:receipt/outcome r))
       (string? (:candidate-action/id r))
       (string? (:admission/id r))
       (not (contains? r :predicted-effects))))
