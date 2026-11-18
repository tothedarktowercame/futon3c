(ns futon3c.arxana.type-registry
  "C1 registry for immutable, versioned Formal Arxana record declarations.

  This is schema/identity only.  It deliberately provides no persistence
  operation; C2 owns the authoritative write ports."
  (:require [clojure.walk :as walk]
            [futon3c.arxana.digest :as digest]))

(def endpoint-kinds
  #{:entity :artifact-revision :manifestation-scope :clusion :external-ref})

(def semantic-kinds
  "Closed node-kind vocabulary for the navigability schema graph.

  These names describe what an endpoint denotes, independently of how it is
  represented above.  Each is backed by a production writer: :turn and
  :pattern by pattern_proposals.clj and ma_strand.clj; :agent by
  xiang_interpretations.clj; :item by turn_mentions.clj, circus/hou.clj and
  giraffe_needs.clj; :focus by circus/hou.clj; and :memory by dog_memory.clj.
  Mission/session/file/repo/code kinds stay out until an Arxana writer gives
  them an endpoint contract rather than merely carrying their ids elsewhere."
  #{:agent :focus :item :memory :pattern :turn})

(def convergence-policies
  #{:append-only :cas :per-user-lww})

(def cycle-policies
  #{:acyclic :bounded-cyclic :fixed-point})

(defn declaration-key [{:keys [type/id type/revision]}]
  [id revision])

(defn declaration-digest [declaration]
  (str "arxana:type:sha256:"
       (digest/canonical-digest
        (walk/postwalk (fn [x] (if (set? x) (vec (sort x)) x))
                       (dissoc declaration :type/digest)))))

(defn validate-declaration
  "Return DECLARATION with its digest, or throw on an incomplete/rich schema.
  The identity projection is data in the declaration, never an implementation
  convention hidden from clients."
  [declaration]
  (let [{:keys [type/id type/revision record/kind roles arity endpoint/kinds
                identity/content-fields convergence/policy recursion/cycle-policy]}
        declaration
        role-names (mapv :role roles)]
    (when-not (and (keyword? id) (pos-int? revision))
      (throw (ex-info "type id and positive revision required" {:declaration declaration})))
    (when-not (#{:clusion :entity :attestation :assessment :receipt :event} kind)
      (throw (ex-info "unsupported record kind" {:record/kind kind})))
    (when (not= (count role-names) (count (distinct role-names)))
      (throw (ex-info "roles must be unique" {:roles role-names})))
    (when (and (= :clusion kind) (not= arity (count roles)))
      (throw (ex-info "clusion arity must equal declared roles"
                      {:arity arity :roles role-names})))
    (when-not (every? endpoint-kinds kinds)
      (throw (ex-info "unknown endpoint kind" {:endpoint/kinds kinds})))
    (doseq [{:keys [role] :as role-declaration} roles
            :when (contains? role-declaration :endpoint/semantic-kinds)]
      (let [declared-semantic-kinds (:endpoint/semantic-kinds role-declaration)]
        (when-not (and (set? declared-semantic-kinds)
                       (every? semantic-kinds declared-semantic-kinds))
        (throw (ex-info "unknown endpoint semantic kind"
                        {:error :arxana.type/unknown-semantic-kind
                         :role role
                         :endpoint/semantic-kinds declared-semantic-kinds})))))
    (when-not (vector? content-fields)
      (throw (ex-info "identity content fields must be an ordered vector"
                      {:identity/content-fields content-fields})))
    (when (contains? declaration :content/enums)
      (let [enums (:content/enums declaration)]
        (when-not (and (map? enums)
                       (every? (fn [[field allowed]]
                                 (and (keyword? field) (some #{field} content-fields)
                                      (set? allowed) (seq allowed)))
                               enums))
          (throw (ex-info "Invalid content enum declaration"
                          {:error :arxana.type/invalid-content-enums
                           :type-revision [id revision] :content/enums enums})))))
    (when-not (convergence-policies policy)
      (throw (ex-info "resolved convergence policy required" {:policy policy})))
    (when-not (cycle-policies cycle-policy)
      (throw (ex-info "resolved cycle policy required" {:cycle-policy cycle-policy})))
    (assoc declaration :type/digest (declaration-digest declaration))))

(defn register-declaration
  "Pure immutable registry update. Equal replay is idempotent; changing an
  existing [type-id revision] is refused and must mint a new revision."
  [registry declaration]
  (let [declaration (validate-declaration declaration)
        k (declaration-key declaration)]
    (if-let [existing (get registry k)]
      (if (= existing declaration)
        {:registry registry :declaration existing :created? false}
        (throw (ex-info "type declaration revision is immutable"
                        {:error :arxana.type/in-place-edit-refused
                         :type-key k
                         :existing-digest (:type/digest existing)
                         :proposed-digest (:type/digest declaration)})))
      {:registry (assoc registry k declaration)
       :declaration declaration
       :created? true})))

(defn- clusion-type [id revision roles content-fields & {:keys [cycle]
                                                          :or {cycle :bounded-cyclic}}]
  {:type/id id
   :type/revision revision
   :record/kind :clusion
   :roles roles
   :arity (count roles)
   :endpoint/kinds (set (mapcat :endpoint/kinds roles))
   :identity/content-fields content-fields
   :identity/default [:type-revision :ordered-role-labelled-endpoints
                      :declared-asserted-content]
   :convergence/policy :append-only
   :recursion/cycle-policy cycle})

(def declarations-v1
  [(assoc (clusion-type :annotation/support 1
                        [{:role :draft :endpoint/kinds #{:external-ref}}
                         {:role :source :endpoint/kinds #{:external-ref}}]
                        [:relation/kind])
          :content/enums {:relation/kind #{:lexical-correspondence :support-proposal}})
   (clusion-type :annotation/edition 1 [] [:draft/digest :scope/digest :segmentation :input/pins])
   (assoc (clusion-type :design/choice 1 []
                        [:choice/key :statement :kind :sources :lifecycle :applies-to :rationale :alternatives])
          :content/enums {:kind #{:constraint :ruling :decision}
                          :lifecycle #{:proposed :accepted :superseded}})
   (assoc (clusion-type :design/check 1 []
                        [:check/key :entry-point :runtime :input-contract :output-contract
                         :implementation-digest :fixtures])
          :content/enums {:runtime #{:core.logic :reazon :ramo :clojure}})
   ;; Catalogue links are not compositions. Port revisions document the contract;
   ;; design-catalogue builders/checks enforce it, not write/validate-composite!.
   (clusion-type :design/checked-by 1
                 [{:role :choice :endpoint/kinds #{:clusion} :endpoint/type-revisions #{[:design/choice 1]}}
                  {:role :check :endpoint/kinds #{:clusion} :endpoint/type-revisions #{[:design/check 1]}}] [])
   (clusion-type :design/shaped-by 1
                 [{:role :choice :endpoint/kinds #{:clusion} :endpoint/type-revisions #{[:design/choice 1]}}
                  {:role :ruling :endpoint/kinds #{:clusion} :endpoint/type-revisions #{[:design/choice 1]}}] [])
   (clusion-type :design/constrains 1
                 [{:role :choice :endpoint/kinds #{:clusion} :endpoint/type-revisions #{[:design/choice 1]}}
                  {:role :target :endpoint/kinds #{:external-ref}}] [])
   (clusion-type :rule/content 1 []
                 [:rule/system :rule/name :rule/pattern :rule/guards
                  :rule/domain-guards :rule/rewrite :rule/priority])
   (clusion-type :conflict/finding 1
                 [{:role :rule-a :endpoint/kinds #{:clusion}}
                  {:role :rule-b :endpoint/kinds #{:clusion}}]
                 [:overlap :status :kinds :settled-by :evaluation :checker-sha
                  :schema-digest :mode :max-states :host/locator])
   (clusion-type :sentence/role 1 []
                 [:module :declaration :corpus-sha256 :split-expression-key :role])
   ;; Six live revision-1 records are all role-scoped; preserve their identity.
   (clusion-type :sentence/manifestation 1 [{:role :role :endpoint/kinds #{:clusion}}]
                 [:renderer-commit :corpus-sha256 :wordings :count])
   (clusion-type :sentence/manifestation 2 [{:role :role :endpoint/kinds #{:clusion}}]
                 [:renderer-commit :renderer-commit-time :corpus-sha256 :wordings :count :scope])
   (assoc (clusion-type :sentence/manifestation 3 [{:role :role :endpoint/kinds #{:clusion}}]
                        [:renderer-commit :renderer-commit-time :corpus-sha256 :wordings :count :scope])
          :content/enums {:scope #{:role :declaration}})
   (clusion-type :rule/application 1
                 [{:role :sentence :endpoint/kinds #{:clusion}} {:role :rule :endpoint/kinds #{:clusion}}]
                 [:method :renderer-commit :corpus-sha256 :tested-rules :interventions])
   (clusion-type :rule-set/diff 1
                 [{:role :supersession :endpoint/kinds #{:clusion}}]
                 [:host/locator :version-a :version-b :comparison
                  :observation-comparison :checker-sha :rule-form])
   (clusion-type :residual/accepted 1
                 [{:role :finding :endpoint/kinds #{:clusion}}]
                 [:scope :release :reason])
   (clusion-type :workplan/item 1 [] [:item/id])
   ;; The rule's stable subject is the entity.  Correctable forms and switches
   ;; live in attestations, deliberately outside the clusion identity.
   (clusion-type :workplan/entity-match-rule 1
                 [{:role :entity :endpoint/kinds #{:clusion}}]
                 [:rule/id])
   (clusion-type :workplan/entity-rule-member 1
                 [{:role :catalog :endpoint/kinds #{:clusion}}
                  {:role :rule :endpoint/kinds #{:clusion}}] [])
   (clusion-type :workplan/introduction 1
                 [{:role :entity :endpoint/kinds #{:clusion}}
                  {:role :source :endpoint/kinds #{:external-ref}}] [])
   (clusion-type :workplan/referent 1
                 [{:role :entity :endpoint/kinds #{:clusion}}
                  {:role :target :endpoint/kinds #{:clusion :external-ref}}]
                 [:referent/role])
   (clusion-type :workplan/catalog 1 [] [:catalog/id])
   (clusion-type :lexicon/entity 1 [] [:entity/slug])
   (clusion-type :benefit-line 1 [] [:benefit/line])
   (clusion-type :workplan/names 1
                 [{:role :item :endpoint/kinds #{:clusion}}
                  {:role :entity :endpoint/kinds #{:clusion}}] [])
   (clusion-type :workplan/serves 1
                 [{:role :item :endpoint/kinds #{:clusion}}
                  {:role :benefit-line :endpoint/kinds #{:clusion}}] [])
   ;; Substrate items are document entities, never workplan/item mirrors.
   (clusion-type :workplan/serves 2
                 [{:role :item :endpoint/kinds #{:entity}}
                  {:role :benefit-line :endpoint/kinds #{:clusion}}] [])
   ;; Historical imports have no document digest or trustworthy reading time.
   ;; Each source act stays independent; these are not current reading facts.
   (clusion-type :giraffe/legacy-reading 1
                 [{:role :item :endpoint/kinds #{:entity}}]
                 [:legacy/source-record-id])
   (clusion-type :workplan/closure-claim 1
                 [{:role :item :endpoint/kinds #{:entity}}]
                 [:legacy/source-record-id])
   (clusion-type :hou/focus 1
                 [{:role :evidence :endpoint/kinds #{:external-ref}
                   :endpoint/semantic-kinds #{:turn}}]
                 [:hou/turn :hou/at :hou/rule :hou/relations :hou/associated-radius
                  :hou/hop-bound :hou/evidence-pin :hou/sources])
   (clusion-type :hou/focus-relation 1
                 [{:role :focus :endpoint/kinds #{:clusion}
                   :endpoint/semantic-kinds #{:focus}}
                  {:role :item :endpoint/kinds #{:clusion}
                   :endpoint/semantic-kinds #{:item}}]
                 [:relation])
   (clusion-type :workplan/mention 1
                 [{:role :item :endpoint/kinds #{:clusion}
                   :endpoint/semantic-kinds #{:item}}
                  {:role :evidence :endpoint/kinds #{:external-ref}
                   :endpoint/semantic-kinds #{:turn}}]
                 [:mention/role])
   (clusion-type :workplan/mention 2
                 [{:role :item :endpoint/kinds #{:clusion}
                   :endpoint/semantic-kinds #{:item}}
                  {:role :evidence :endpoint/kinds #{:external-ref}
                   :endpoint/semantic-kinds #{:turn}}]
                 ;; :mention/quote is not identity content: it is stored
                 ;; only when the text at the range differs from the item id
                 ;; (Joe 2026-11-16: no ("Arxana" "Arxana") per mention).
                 [:mention/role :mention/range])
   (clusion-type :code-definition 1 []
                 [:artifact/revision-id :scope/kind :symbol/name :definition/form])
   (clusion-type :doc-entry 1 []
                 [:artifact/revision-id :entry/id :symbol/name])
   (clusion-type :lean-declaration 1 []
                 [:artifact/revision-id :declaration/fqn])
   (clusion-type :scope-in-revision 1
                 [{:role :revision :endpoint/kinds #{:artifact-revision}}
                  {:role :scope :endpoint/kinds #{:clusion}}]
                 [:scope/kind])
   (clusion-type :interpretation 1
                 [{:role :source :endpoint/kinds #{:manifestation-scope}}
                  {:role :intent :endpoint/kinds #{:entity}}
                  {:role :target :endpoint/kinds #{:entity :external-ref}
                   :endpoint/semantic-kinds #{:agent :turn}}]
                 [:reading/state])
   (clusion-type :resolution-observation 1
                 [{:role :subject :endpoint/kinds #{:clusion :manifestation-scope}}]
                 [:resolution/state])
   (clusion-type :supersedes 1
                 [{:role :successor :endpoint/kinds #{:clusion}}
                  {:role :predecessor :endpoint/kinds #{:clusion}}]
                 [] :cycle :acyclic)
   (clusion-type :pattern/proposal 1
                 [{:role :turn :endpoint/kinds #{:external-ref}
                   :endpoint/semantic-kinds #{:turn}}
                  {:role :pattern :endpoint/kinds #{:external-ref}
                   :endpoint/semantic-kinds #{:pattern}}]
                 [:intent])
   (clusion-type :pattern/occurrence 1
                 [{:role :pattern :endpoint/kinds #{:external-ref}
                   :endpoint/semantic-kinds #{:pattern}}
                  {:role :turn :endpoint/kinds #{:external-ref}
                   :endpoint/semantic-kinds #{:turn}}]
                 [:window :proposal])
   (clusion-type :ma/strand 1
                 [{:role :turn :endpoint/kinds #{:external-ref}
                   :endpoint/semantic-kinds #{:turn}}
                  {:role :prev :endpoint/kinds #{:external-ref} :required? false
                   :endpoint/semantic-kinds #{:turn}}]
                 [:connection/status :connection/sha256 :connection/reason])
   (clusion-type :ma/quote 1
                 [{:role :turn :endpoint/kinds #{:external-ref}
                   :endpoint/semantic-kinds #{:turn}}
                  {:role :quoted-turn :endpoint/kinds #{:external-ref}
                   :endpoint/semantic-kinds #{:turn}}
                  {:role :pattern :endpoint/kinds #{:external-ref}
                   :endpoint/semantic-kinds #{:pattern}}]
                 [:part :window])
   (clusion-type :needs 1
                 [{:role :needing :endpoint/kinds #{:external-ref}
                   :endpoint/semantic-kinds #{:item}}
                  {:role :supplier :endpoint/kinds #{:external-ref}
                   :endpoint/semantic-kinds #{:item}}]
                 [:class])
   (clusion-type :memory/recall 1
                 [{:role :turn :endpoint/kinds #{:external-ref}
                   :endpoint/semantic-kinds #{:turn}}
                  {:role :memory :endpoint/kinds #{:external-ref}
                   :endpoint/semantic-kinds #{:memory}}]
                 [:arm :delivered])
   (clusion-type :memory/use 1
                 [{:role :turn :endpoint/kinds #{:external-ref}
                   :endpoint/semantic-kinds #{:turn}}
                  {:role :memory :endpoint/kinds #{:external-ref}
                   :endpoint/semantic-kinds #{:memory}}]
                 [])
   (clusion-type :memory/burial 1
                 [{:role :turn :endpoint/kinds #{:external-ref}
                   :endpoint/semantic-kinds #{:turn}}
                  {:role :memory :endpoint/kinds #{:external-ref}
                   :endpoint/semantic-kinds #{:memory}}]
                 [])
   (clusion-type :memory/repetition 1
                 [{:role :turn :endpoint/kinds #{:external-ref}
                   :endpoint/semantic-kinds #{:turn}}
                  {:role :memory :endpoint/kinds #{:external-ref}
                   :endpoint/semantic-kinds #{:memory}}]
                 [])
   (clusion-type :kernel-builds 1
                 [{:role :artifact :endpoint/kinds #{:artifact-revision}}]
                 [:language])
   (clusion-type :declaration-location 1
                 [{:role :declaration :endpoint/kinds #{:entity}}
                  {:role :artifact :endpoint/kinds #{:artifact-revision}}]
                 [])
   (clusion-type :correspondence 1
                 [{:role :source :endpoint/kinds #{:manifestation-scope}}
                  {:role :target :endpoint/kinds #{:entity}}]
                 [:direction :relation/kind])
   (clusion-type :generation-run 1
                 [{:role :input :endpoint/kinds #{:artifact-revision}}
                  {:role :output :endpoint/kinds #{:artifact-revision}}]
                 [:model :prompt :configuration])
   (clusion-type :synthetic-mutation 1
                 [{:role :source :endpoint/kinds #{:artifact-revision}}
                  {:role :mutation :endpoint/kinds #{:artifact-revision}}]
                 [:label :diff])
   (clusion-type :formalization-coverage 1
                 [{:role :declaration :endpoint/kinds #{:entity}}]
                 [:coverage/state :reason])
   (clusion-type :geometry/entity 1 [] [:entity/id :entity/kind :theory/version])
   (clusion-type :geometry/map 1
                 [{:role :domain :endpoint/kinds #{:clusion}}
                  {:role :codomain :endpoint/kinds #{:clusion}}]
                 [:map/id :map/definition :theory/version])
   (clusion-type :geometry/symmetry 1
                 [{:role :carrier :endpoint/kinds #{:clusion}}
                  {:role :map :endpoint/kinds #{:clusion}}]
                 [:symmetry/id :theory/version])
   (clusion-type :geometry/projection 1
                 [{:role :subject :endpoint/kinds #{:clusion}}]
                 [:projection/id :theory/version])
   (clusion-type :geometry/preserves 1
                 [{:role :map :endpoint/kinds #{:clusion}}
                  {:role :invariant :endpoint/kinds #{:clusion}}]
                 [:theory/version])
   (clusion-type :geometry/satisfies 1
                 [{:role :subject :endpoint/kinds #{:clusion}}
                  {:role :invariant :endpoint/kinds #{:clusion}}]
                 [:theory/version])
   (clusion-type :geometry/fibre-of 1
                 [{:role :fibre :endpoint/kinds #{:clusion}}
                  {:role :map :endpoint/kinds #{:clusion}}
                  {:role :value :endpoint/kinds #{:clusion}}]
                 [:theory/version :kernel/attestation-id])
   (clusion-type :geometry/fibre-of 2
                 [{:role :fibre :endpoint/kinds #{:clusion}}
                  {:role :map :endpoint/kinds #{:clusion}}
                  {:role :value :endpoint/kinds #{:clusion}}]
                 [:theory/version :kernel/attestation-id :lean/declaration :fibre/cardinality])
   (clusion-type :geometry/member 1
                 [{:role :fibre :endpoint/kinds #{:clusion}}
                  {:role :member :endpoint/kinds #{:clusion}}]
                 [:theory/version])
   (clusion-type :geometry/value 1
                 [{:role :point :endpoint/kinds #{:clusion}}]
                 [:turn/numerator :turn/denominator :theory/version])
   (clusion-type :geometry/placement 1
                 [{:role :panel :endpoint/kinds #{:clusion}}
                  {:role :subject :endpoint/kinds #{:clusion}}]
                 [:offset/x :offset/y :placed-at :theory/version])
   (clusion-type :geometry/named 1
                 [{:role :scene :endpoint/kinds #{:clusion}}
                  {:role :object :endpoint/kinds #{:clusion}}]
                 [:name :object/kind :theory/version])
   (clusion-type :geometry/selection 1
                 [{:role :scene :endpoint/kinds #{:clusion}}
                  {:role :map :endpoint/kinds #{:clusion}}]
                 [:selected-at :theory/version])
   (clusion-type :f6/item 1 [] [:item/id :item/kind :content :record/ids])
   (clusion-type :f6/contains 1
                 [{:role :argument :endpoint/kinds #{:clusion}}
                  {:role :item :endpoint/kinds #{:clusion}}] [])
   (clusion-type :f6/supports 1
                 [{:role :supported-claim :endpoint/kinds #{:clusion}}
                  {:role :supporting-content :endpoint/kinds #{:clusion}}]
                 [:method :evidence :actor :asserted-at :reason])
   (clusion-type :f6/re-supports 1
                 [{:role :successor :endpoint/kinds #{:clusion}}
                  {:role :predecessor :endpoint/kinds #{:clusion}}]
                 [:actor :changed-at :reason] :cycle :acyclic)
   (clusion-type :f6/placement 1
                 [{:role :argument :endpoint/kinds #{:clusion}}
                  {:role :item :endpoint/kinds #{:clusion}}]
                 [:zone :order :offset/x :offset/y :placed-at :actor])
   (clusion-type :text/patch 1
                 [{:role :base :endpoint/kinds #{:artifact-revision}}]
                 [:artifact/id :patch/digest :author :operation/id :hunks])
   (clusion-type :text/conflict 1
                 [{:role :left :endpoint/kinds #{:clusion}}
                  {:role :right :endpoint/kinds #{:clusion}}]
                 [:artifact/id :base/revision-id :computed-against/head-revision-id
                  :overlap/class])
   (clusion-type :text/conflict 2
                 [{:role :left :endpoint/kinds #{:clusion}}
                  {:role :right :endpoint/kinds #{:clusion}}]
                 [:artifact/id :base/revision-id :computed-against/head-revision-id
                  :overlap/class :conflicting/change-ids])
   (clusion-type :text/application 1
                 [{:role :change :endpoint/kinds #{:clusion}}
                  {:role :from :endpoint/kinds #{:artifact-revision}}
                  {:role :to :endpoint/kinds #{:artifact-revision}}]
                 [:artifact/id :event-time :patch])
   (clusion-type :text/alternative 1
                 [{:role :conflict :endpoint/kinds #{:clusion}}
                  {:role :keeps :endpoint/kinds #{:clusion}}]
                 [:result/digest :result/text])
   (clusion-type :text/choice 1
                 [{:role :conflict :endpoint/kinds #{:clusion}}
                  {:role :alternative :endpoint/kinds #{:clusion}}]
                 [:actor :chosen-at :reason])
   (clusion-type :d6/intent-stage 1
                 [{:role :source :endpoint/kinds #{:clusion}}]
                 [:intent/id :intent/record])
   (clusion-type :d6/candidate-stage 1
                 [{:role :intent :endpoint/kinds #{:clusion}}]
                 [:candidate/id :candidate/record :candidate/non-executable?])
   (clusion-type :d6/decision-stage 1
                 [{:role :candidate :endpoint/kinds #{:clusion}}]
                 [:decision/id :decision/record])
   (clusion-type :d6/intends 1
                 [{:role :source-act :endpoint/kinds #{:clusion}}
                  {:role :intent :endpoint/kinds #{:clusion}}]
                 [])
   (clusion-type :d6/compiles-to 1
                 [{:role :intent :endpoint/kinds #{:clusion}}
                  {:role :candidate :endpoint/kinds #{:clusion}}]
                 [])
   (clusion-type :d6/decides 1
                 [{:role :candidate :endpoint/kinds #{:clusion}}
                  {:role :decision :endpoint/kinds #{:clusion}}]
                 [])
   (clusion-type :d6/capability 1
                 [{:role :candidate :endpoint/kinds #{:clusion}
                   :endpoint/type-revisions #{[:d6/candidate-stage 1]}}]
                 [:principal/id :dynamics :granted-by :granted-at]
                 :cycle :acyclic)
   (clusion-type :d6/receipt-stage 1
                 [{:role :decision :endpoint/kinds #{:clusion}
                   :endpoint/type-revisions #{[:d6/decision-stage 1]}}]
                 [:attempt/id :action/receipt]
                 :cycle :acyclic)
   (assoc
    (clusion-type :d6/composite 1
                  [{:role :source-act :endpoint/kinds #{:clusion}
                    :endpoint/type-revisions #{[:interpretation 1]}}
                   {:role :intent :endpoint/kinds #{:clusion}
                    :endpoint/type-revisions #{[:d6/intent-stage 1]}}
                   {:role :candidate :endpoint/kinds #{:clusion}
                    :endpoint/type-revisions #{[:d6/candidate-stage 1]}}
                   {:role :decision :endpoint/kinds #{:clusion}
                    :endpoint/type-revisions #{[:d6/decision-stage 1]}}]
                  [:dynamics :topology/evidence] :cycle :acyclic)
    :composition/typed-ports? true)
   (assoc
    (clusion-type :d6/composite-group 1
                  [{:role :child :endpoint/kinds #{:clusion}
                    :endpoint/type-revisions #{[:d6/composite 1]
                                               [:d6/composite-group 1]}}]
                  [:dynamics :topology/evidence] :cycle :acyclic)
    :composition/typed-ports? true)
   (assoc
    (clusion-type :d6/composite 2
                  [{:role :predecessor :endpoint/kinds #{:clusion}
                    :endpoint/type-revisions #{[:d6/composite 1]}}
                   {:role :source-act :endpoint/kinds #{:clusion}
                    :endpoint/type-revisions #{[:interpretation 1]}}
                   {:role :intent :endpoint/kinds #{:clusion}
                    :endpoint/type-revisions #{[:d6/intent-stage 1]}}
                   {:role :candidate :endpoint/kinds #{:clusion}
                    :endpoint/type-revisions #{[:d6/candidate-stage 1]}}
                   {:role :decision :endpoint/kinds #{:clusion}
                    :endpoint/type-revisions #{[:d6/decision-stage 1]}}
                   {:role :receipt :endpoint/kinds #{:clusion}
                    :endpoint/type-revisions #{[:d6/receipt-stage 1]}}]
                  [:dynamics :topology/evidence] :cycle :acyclic)
    :composition/typed-ports? true)
   {:type/id :mark-occurrence :type/revision 1 :record/kind :entity
    :roles [] :arity 0 :endpoint/kinds #{}
    :identity/content-fields [:glyph :form :artifact/revision-id :locator]
    :identity/default [:type-revision :declared-asserted-content]
    :convergence/policy :append-only :recursion/cycle-policy :acyclic}
   {:type/id :evidence-attestation :type/revision 1 :record/kind :attestation
    :roles [] :arity 0 :endpoint/kinds #{}
    :identity/content-fields [:subject-clusion-id :producer :basis :standing
                              :coverage :method :content :source-refs]
    :identity/default [:type-revision :declared-asserted-content]
    :convergence/policy :append-only :recursion/cycle-policy :acyclic}
   {:type/id :assessment :type/revision 1 :record/kind :assessment
    :roles [] :arity 0 :endpoint/kinds #{}
    :identity/content-fields [:subject-clusion-id :verdict :actor :policy :reason :time]
    :identity/default [:type-revision :declared-asserted-content]
    :convergence/policy :append-only :recursion/cycle-policy :acyclic}
   {:type/id :operation-attempt :type/revision 1 :record/kind :receipt
    :roles [] :arity 0 :endpoint/kinds #{}
    :identity/content-fields [:dynamics :inputs :outcome :reason :actor :time]
    :identity/default [:type-revision :declared-asserted-content]
    :convergence/policy :append-only :recursion/cycle-policy :acyclic}
   {:type/id :external-reference :type/revision 1 :record/kind :entity
    :roles [] :arity 0 :endpoint/kinds #{}
    :identity/content-fields [:ref/kind :store/id :store/endpoint :record/id
                              :schema/authority :valid-time :revision/digest]
    :identity/default [:type-revision :declared-asserted-content]
    :convergence/policy :append-only :recursion/cycle-policy :acyclic}
   {:type/id :proposal/shown :type/revision 1 :record/kind :event
    :roles [] :arity 0 :endpoint/kinds #{}
    :identity/content-fields [:proposal-id :attestation-id :surface :actor :time]
    :identity/default [:type-revision :declared-asserted-content]
    :convergence/policy :append-only :recursion/cycle-policy :acyclic}])

(def registry-v1
  (:registry
   (reduce (fn [{:keys [registry]} declaration]
             (register-declaration registry declaration))
           {:registry {}}
           declarations-v1)))

(defn schema-edges
  "The declared semantic schema edges in REGISTRY (registry-v1 by default).

  Every ordered pair of distinct, semantically declared roles contributes
  an edge for the Cartesian product of their kinds.  Ordered role pairs make
  reverse-query navigation explicit.  Roles without semantic kinds are
  deliberately invisible: this function never guesses from a role name."
  ([] (schema-edges registry-v1))
  ([registry]
   (->> registry
        (mapcat
         (fn [[type-revision declaration]]
           (for [from-role (:roles declaration)
                 to-role (:roles declaration)
                 :when (and (not= (:role from-role) (:role to-role))
                            (seq (:endpoint/semantic-kinds from-role))
                            (seq (:endpoint/semantic-kinds to-role)))
                 from (:endpoint/semantic-kinds from-role)
                 to (:endpoint/semantic-kinds to-role)]
             {:from from :to to
              :type type-revision
              :from-role (:role from-role)
              :to-role (:role to-role)})))
        (sort-by pr-str)
        vec)))

(defn schema-kind-graph
  "Sorted map [from-kind to-kind] -> the declarations that realise that move."
  ([] (schema-kind-graph registry-v1))
  ([registry]
   (into (sorted-map)
         (map (fn [[pair edges]]
                [pair (->> edges
                           (map #(select-keys % [:type :from-role :to-role]))
                           (sort-by pr-str)
                           vec)]))
         (group-by (juxt :from :to) (schema-edges registry)))))

(defn declaration [type-revision]
  (or (get registry-v1 type-revision)
      (throw (ex-info "unknown type revision"
                      {:error :arxana.type/unknown-revision
                       :type-revision type-revision}))))

(def dynamics-v1
  "Executable method revisions admitted by the C3b receipt boundary. New
  implementations add a version; they never reinterpret an old receipt."
  #{["hou/focus" 1]
    ["馬/occurrence" 1]
    ["xiang/read" 1]
    ["embedding" "all-MiniLM-L6-v2"]
    ["formal-arxana/d6-compose" 1]
    ["formal-arxana/bounded-read" 1]})

(defn known-dynamics? [{:keys [id version]}]
  (contains? dynamics-v1 [(str id) version]))
