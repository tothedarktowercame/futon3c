(ns futon3c.arxana.records
  "C1 constructors and identity digests for Formal Arxana records."
  (:require [futon3c.arxana.digest :as digest]
            [futon3c.arxana.type-registry :as types]))

(def evidence-bases #{:exact :parser :model :human :kernel :composite})
(def evidence-standings #{:unverified :verified :contradicted :disputed})
(def evidence-coverages #{:none :partial :complete})

(defn- ordered-content [fields content]
  (reduce (fn [m field]
            (if (contains? content field)
              (assoc m field (get content field))
              (throw (ex-info "declared identity content field missing"
                              {:field field :content content}))))
          (array-map)
          fields))

(defn clusion-key
  "The Identity.lean key: type revision, ordered role-labelled endpoints,
  and only the asserted-content fields selected by the type declaration."
  ([record] (clusion-key types/registry-v1 record))
  ([registry {:keys [type-revision endpoints asserted-content]}]
  (let [declaration (or (get registry type-revision)
                        (throw (ex-info "unknown type revision"
                                        {:type-revision type-revision})))
        expected-roles (mapv :role (:roles declaration))
        actual-roles (mapv :role endpoints)]
    (when-not (= :clusion (:record/kind declaration))
      (throw (ex-info "type is not a clusion" {:type-revision type-revision})))
    (when-not (= expected-roles actual-roles)
      (throw (ex-info "endpoint roles/order do not match declaration"
                      {:expected expected-roles :actual actual-roles})))
    (doseq [[endpoint role-decl] (map vector endpoints (:roles declaration))]
      (when-not ((:endpoint/kinds role-decl) (:kind endpoint))
        (throw (ex-info "endpoint kind not allowed for role"
                        {:role (:role endpoint) :kind (:kind endpoint)}))))
    {:type-revision type-revision
     :endpoints endpoints
     :asserted-content (ordered-content (:identity/content-fields declaration)
                                        asserted-content)})))

(defn clusion-id
  ([clusion] (clusion-id types/registry-v1 clusion))
  ([registry clusion]
   (str "arxana:clusion:sha256:"
        (digest/canonical-digest (clusion-key registry clusion)))))

(defn make-clusion [record]
  (let [without-id (dissoc record :id)]
    (assoc without-id :id (clusion-id without-id))))

(defn attestation-id [attestation]
  (str "arxana:attestation:sha256:"
       (digest/canonical-digest (dissoc attestation :id :transport))))

(defn make-attestation [attestation]
  (when-not (evidence-bases (:basis attestation))
    (throw (ex-info "invalid evidence basis" {:basis (:basis attestation)})))
  (when-not (evidence-standings (:standing attestation))
    (throw (ex-info "invalid evidence standing" {:standing (:standing attestation)})))
  (when-not (evidence-coverages (:coverage attestation))
    (throw (ex-info "invalid evidence coverage" {:coverage (:coverage attestation)})))
  (when-not (and (string? (:subject-clusion-id attestation))
                 (:producer attestation) (map? (:method attestation)))
    (throw (ex-info "attestation subject, producer and method required"
                    {:attestation attestation})))
  (let [without-id (dissoc attestation :id)]
    (assoc without-id :id (attestation-id without-id))))

(def assessment-verdicts #{:candidate :accepted :rejected})
(declare record-id)

(defn make-assessment [assessment]
  (when-not (assessment-verdicts (:verdict assessment))
    (throw (ex-info "invalid assessment verdict" {:verdict (:verdict assessment)})))
  (when-not (and (string? (:subject-clusion-id assessment))
                 (:actor assessment) (:policy assessment) (:reason assessment)
                 (:time assessment))
    (throw (ex-info "assessment subject, attribution and reason required"
                    {:assessment assessment})))
  (let [without-id (dissoc assessment :id)]
    (assoc without-id :id
           (record-id [:assessment 1] without-id))))

(defn record-id
  "Digest a non-'clusion record against its declaration's identity fields."
  [type-revision content]
  (let [declaration (types/declaration type-revision)
        key {:type-revision type-revision
             :content (ordered-content (:identity/content-fields declaration) content)}]
    (str "arxana:record:sha256:" (digest/canonical-digest key))))
