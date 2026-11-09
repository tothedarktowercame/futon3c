(ns futon3c.arxana.type-registry
  "C1 registry for immutable, versioned Formal Arxana record declarations.

  This is schema/identity only.  It deliberately provides no persistence
  operation; C2 owns the authoritative write ports."
  (:require [clojure.walk :as walk]
            [futon3c.arxana.digest :as digest]))

(def endpoint-kinds
  #{:entity :artifact-revision :manifestation-scope :clusion :external-ref})

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
    (when-not (vector? content-fields)
      (throw (ex-info "identity content fields must be an ordered vector"
                      {:identity/content-fields content-fields})))
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
  [(clusion-type :code-definition 1 []
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
                  {:role :target :endpoint/kinds #{:entity :external-ref}}]
                 [:reading/state])
   (clusion-type :resolution-observation 1
                 [{:role :subject :endpoint/kinds #{:clusion :manifestation-scope}}]
                 [:resolution/state])
   (clusion-type :supersedes 1
                 [{:role :successor :endpoint/kinds #{:clusion}}
                  {:role :predecessor :endpoint/kinds #{:clusion}}]
                 [] :cycle :acyclic)
   (clusion-type :pattern/proposal 1
                 [{:role :turn :endpoint/kinds #{:external-ref}}
                  {:role :pattern :endpoint/kinds #{:external-ref}}]
                 [:intent])
   (clusion-type :pattern/occurrence 1
                 [{:role :pattern :endpoint/kinds #{:external-ref}}
                  {:role :turn :endpoint/kinds #{:external-ref}}]
                 [:window :proposal])
   (clusion-type :needs 1
                 [{:role :needing :endpoint/kinds #{:external-ref}}
                  {:role :supplier :endpoint/kinds #{:external-ref}}]
                 [:class])
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

(defn declaration [type-revision]
  (or (get registry-v1 type-revision)
      (throw (ex-info "unknown type revision"
                      {:error :arxana.type/unknown-revision
                       :type-revision type-revision}))))

(def dynamics-v1
  "Executable method revisions admitted by the C3b receipt boundary. New
  implementations add a version; they never reinterpret an old receipt."
  #{["馬/occurrence" 1]
    ["xiang/read" 1]
    ["embedding" "all-MiniLM-L6-v2"]
    ["formal-arxana/d6-compose" 1]
    ["formal-arxana/bounded-read" 1]})

(defn known-dynamics? [{:keys [id version]}]
  (contains? dynamics-v1 [(str id) version]))
