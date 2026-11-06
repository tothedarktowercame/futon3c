(ns futon3c.arxana.resolve.promotion
  "Resolution assertion promotion (D2 promotion section, O-D2c/O-D2d, D3
  `assertion/promote`).

  Promotion happens only after the authority replays the named computation
  and the replayed semantic digest matches the submitted one (D3 evidence
  boundary: authoritative promotion requires authority-side evaluation). For
  `resolved-unverified` or `ambiguous` a separate attributable
  `arxana/resolution-acceptance` record — actor/tool, policy and policy
  revision, reason, selected candidates — is mandatory; an unattributed
  Boolean acknowledgement is invalid. The acceptance is never folded into the
  assertion: several accountable judgments may cite the same immutable
  computation (D2).

  The assertion ID is the digest of semantic inputs and outcome excluding
  request ID, timing and transport fields; repeating the same computation is
  an idempotent upsert. Non-promoted results stay response-only."
  (:require [futon3c.arxana.digest :as digest]
            [futon3c.arxana.envelope :as env]
            [futon3c.arxana.resolve.evaluator :as evaluator]
            [futon3c.arxana.store :as store]))

(def promotable-statuses #{:resolved :resolved-unverified :ambiguous})
(def acceptance-required-statuses #{:resolved-unverified :ambiguous})

(defn assertion-id
  "Digest of semantic inputs and outcome only — request ID, timing and
  transport fields are excluded (D2)."
  ^String [semantic-digest evidence-digests]
  (str "arxana:resolution-assertion:sha256:"
       (digest/canonical-digest
        {:semantic/digest semantic-digest
         :result/evidence-digests evidence-digests
         :evaluator/version evaluator/evaluator-version})))

(defn- acceptance-id
  ^String [aid acceptance]
  (str "arxana:resolution-acceptance:sha256:"
       (digest/canonical-digest
        {:assertion/id aid
         :acceptance/actor (:acceptance/actor acceptance)
         :acceptance/policy (:acceptance/policy acceptance)
         :acceptance/policy-revision (:acceptance/policy-revision acceptance)
         :acceptance/reason (:acceptance/reason acceptance)
         :acceptance/candidate-ids (:acceptance/candidate-ids acceptance)})))

(defn- validate-acceptance
  "An acceptance must name actor/tool, policy, policy revision, reason and
  selected candidates. An unattributed Boolean acknowledgement is invalid (D2)."
  [acceptance]
  (let [required [:acceptance/actor :acceptance/policy
                  :acceptance/policy-revision :acceptance/reason
                  :acceptance/candidate-ids]
        missing (remove #(some? (get acceptance %)) required)]
    (when (seq missing)
      (env/error :invalid :arxana.promotion/unattributed-acceptance
                 {:missing-fields (vec (sort missing))
                  :reason "acceptance must be attributable: actor/tool, policy + policy revision, stated reason, selected candidates"}
                 [:attribute-acceptance]))))

(defn promote
  "Authority replay + durable assertion write.

  deps: {:graph-store s :artifact-provider p :strategy-source f}
  body: {:result/digest <semantic digest claimed by caller>
         :result/evidence {:request {...} :result {...}}  ; full D2 payload
         :acceptance {...}  ; required for unverified/ambiguous
         :principal/id ...}

  Returns {:ok {:assertion/id ... :created? bool :acceptance/id ...?}} or
  {:error typed-error}. Statuses other than resolved/resolved-unverified/
  ambiguous are refused: `stale`/`missing`/`unsupported`/`invalid`/`failed`
  results are not promotable evidence."
  [deps {:keys [result/digest result/evidence acceptance principal/id]}]
  (let [{:keys [graph-store artifact-provider strategy-source]} deps
        replay-request (:request evidence)
        submitted (:result evidence)]
    (cond
      (not (and (string? digest) (map? replay-request) (map? submitted)))
      {:error (env/error :invalid :arxana.promotion/malformed-body
                         {:reason "promotion requires :result/digest plus :result/evidence carrying the original request and result"}
                         [:scope/resolve])}

      :else
      (let [replayed (evaluator/evaluate
                      {:artifact-provider artifact-provider
                       :strategy-source strategy-source}
                      replay-request)
            replay-digest (env/semantic-digest
                           env/scope-resolve-projection-v1
                           (assoc replayed :operation :scope/resolve))
            status (:result/status replayed)]
        (cond
          ;; Authority replay is the promotion gate (D3 evidence boundary).
          (not= digest replay-digest)
          {:error (env/error :conflict :arxana.promotion/replay-mismatch
                             {:claimed-digest digest
                              :replayed-digest replay-digest
                              :reason "authority replay did not reproduce the submitted semantic digest"}
                             [:scope/resolve])}

          (not (contains? promotable-statuses status))
          {:error (env/error :invalid :arxana.promotion/status-not-promotable
                             {:result/status status
                              :promotable (vec (sort promotable-statuses))}
                             [:scope/resolve])}

          :else
          (if-let [acc-err (when (contains? acceptance-required-statuses status)
                             (validate-acceptance acceptance))]
            {:error acc-err}
            (let [aid (assertion-id replay-digest
                                    (:result/evidence-digests replayed))
                  existing (store/get-entity graph-store aid)]
              (if (:ok existing)
                ;; Idempotent upsert: the same computation re-promotes to the
                ;; same assertion without mutation (D2).
                {:ok {:assertion/id aid :created? false
                      :assertion (:ok existing)}}
                (let [entity {:entity/id aid
                              :type :arxana/resolution-assertion
                              :external-id aid
                              :props
                              {:result/status status
                               :semantic/digest replay-digest
                               :artifact/revision-id (:artifact/revision-id replayed)
                               :strategy/revision-id (:strategy/revision-id replayed)
                               :subject/id (:subject/id replay-request)
                               :candidate/ids (mapv :candidate/id
                                                    (:result/candidates replayed))
                               :candidate/digests (mapv :matched-text-digest
                                                        (:result/candidates replayed))
                               :result/evidence-digests (:result/evidence-digests replayed)
                               :evaluator/version evaluator/evaluator-version
                               :arxana/semantics evaluator/semantics-version
                               ;; complete semantic result/evidence payload,
                               ;; atomically with the assertion (O-D2d)
                               :result/payload replayed}}
                      _ (store/put-entity! graph-store entity)
                      base-edges [{:hx/id (str aid ":artifact")
                                   :hx/type :arxana/resolution-evidence-for
                                   :hx/from aid
                                   :hx/to (:artifact/revision-id replayed)}
                                  {:hx/id (str aid ":strategy")
                                   :hx/type :arxana/resolution-evidence-for
                                   :hx/from aid
                                   :hx/to (:strategy/revision-id replayed)}]
                      subject-edge (when-let [sub (:subject/id replay-request)]
                                     [{:hx/id (str aid ":subject")
                                       :hx/type :arxana/resolution-evidence-for
                                       :hx/from aid
                                       :hx/to sub}])]
                  (if (contains? acceptance-required-statuses status)
                    (let [acc-id (acceptance-id aid acceptance)
                          acc-entity {:entity/id acc-id
                                      :type :arxana/resolution-acceptance
                                      :external-id acc-id
                                      :props
                                      {:assertion/id aid
                                       :acceptance/actor (:acceptance/actor acceptance)
                                       :acceptance/policy (:acceptance/policy acceptance)
                                       :acceptance/policy-revision (:acceptance/policy-revision acceptance)
                                       :acceptance/reason (:acceptance/reason acceptance)
                                       :acceptance/candidate-ids (:acceptance/candidate-ids acceptance)
                                       :acceptance/timestamp (:acceptance/timestamp acceptance)
                                       :promoted-by id}}
                          acc-edge {:hx/id (str acc-id ":accepts")
                                    :hx/type :arxana/accepts-resolution
                                    :hx/from acc-id
                                    :hx/to aid}]
                      (store/put-entity! graph-store acc-entity)
                      (store/put-hyperedges! graph-store
                                             (into (into base-edges
                                                         (or subject-edge []))
                                                   [acc-edge]))
                      {:ok {:assertion/id aid :created? true
                            :acceptance/id acc-id}})
                    (do
                      (store/put-hyperedges! graph-store
                                             (into base-edges (or subject-edge [])))
                      {:ok {:assertion/id aid :created? true}})))))))))))
