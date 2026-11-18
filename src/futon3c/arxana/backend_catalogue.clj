(ns futon3c.arxana.backend-catalogue
  "Backend choices and bounded, read-only fixture probes. Results are evidence
  about caller-pinned fixtures, never a universal correctness certificate."
  (:require [futon3c.arxana.catalogue-view :as view]
            [futon3c.arxana.dependence :as dependence]
            [futon3c.arxana.design-catalogue :as catalogue]
            [futon3c.arxana.join-registry :as joins]
            [futon3c.arxana.store :as store]))

(def features
  [{:feature :records-by-type :statement "Type revision queries return id-ordered records at valid time, bounded with explicit truncation."
    :repo "futon1b" :revision "b8b3fbdf133b0d06b64264bdd744445239ad0c86" :path "futon1b_arxana.clj" :section "records-by-type"}
   {:feature :records-by-field :statement "All six reserved record kinds use uniform field projection: clusions expose asserted-content; other kinds expose top-level fields and one map level. Scalar vectors expand; nested values are canonical EDN, not element-searchable."
    :repo "futon1b" :revision "90aef613f3adae88623b0ac3a7153bf691aae2fb" :path "futon1b_arxana.clj" :section "record-fields, record-field-rows, record-kind-tables"}
   {:feature :records-by-field-range :statement "Range queries compare numeric fields only, with explicit inclusive or exclusive bounds and valid time."
    :repo "futon1b" :revision "90aef613f3adae88623b0ac3a7153bf691aae2fb" :path "futon1b_arxana.clj" :section "records-by-field-range"}
   {:feature :records-by-span-overlap :statement "One bounded query joins both numeric span fields at the same valid time. Half-open touching spans, empty spans and missing fields are excluded."
    :repo "futon1b" :revision "2cde1f92f24d3c6c0b73804280747c5b2558e13f" :path "futon1b_arxana.clj" :section "records-by-span-overlap"}
   {:feature :type-census :statement "A bounded temporal census counts clusion revisions and other reserved record kinds, reporting truncation."
    :repo "futon1b" :revision "90aef613f3adae88623b0ac3a7153bf691aae2fb" :path "futon1b_arxana.clj" :section "type-census"}
   {:feature :attestations :statement "Subject attestation history reports total and truncation while preserving its existing order."
    :repo "futon1b" :revision "b8b3fbdf133b0d06b64264bdd744445239ad0c86" :path "futon1b_arxana_index.clj" :section "attestations"}
   {:feature :edge-join :statement "Edge joins admit registered role sets from a data snapshot. A drift check compares the snapshot to the registry; legacy role disagreement refuses loading."
    :repo "futon1b" :revision "abf2234fc70b08926ce2b58d3c63a5a77ec035ab" :path "futon1b_arxana.clj" :section "checked-join-shapes, edge-join"}
   {:feature :dependence :statement "Fibre and staleness readers request 200 reverse candidates and read each as-of. Stale or truncated indexes yield inconclusive, never partial success."
    :repo "futon3c" :revision "6ef3d2349baf758ae9c740f6d6c06ff616ee0142" :path "src/futon3c/arxana/dependence.clj" :section "reverse-query, fibre, staleness"}
   {:feature :catalogue/view :statement "The catalogue view is query-backed and complete-or-inconclusive; any refused or truncated acquisition prevents a partial catalogue."
    :repo "futon3c" :revision "ee7b00631303e7f0e1ad2a90b78a736dd95fdc29" :path "src/futon3c/arxana/catalogue_view.clj" :section "read-catalogue"}
   {:feature :catalogue/read-only-endpoint :kind :constraint :statement "The private Emacs catalogue endpoint admits only catalogue/view and refuses write operations before service dispatch."
    :repo "futon4a" :revision "779d215961d5535528544092c7b46cd27b0043d5" :path "scripts/catalogue_demo.clj" :section "handler, read-only-store"}])

(def query-features
  #{:records-by-type :records-by-field :records-by-field-range
    :records-by-span-overlap :type-census :attestations :edge-join})

(defn- execute-case [feature {:keys [store endpoint]} {:keys [request operation args at]}]
  (cond
    (query-features feature)
    (let [r (store/bounded-query! store (assoc request :query/type feature))]
      (if (:error r) r (:ok r)))
    (= feature :dependence)
    (case operation
      :fibre (apply dependence/fibre store args)
      :staleness (apply dependence/staleness store args)
      (throw (ex-info "Unknown dependence probe" {:error :catalogue/invalid-probe :operation operation})))
    (= feature :catalogue/view) (view/read-catalogue store at)
    (= feature :catalogue/read-only-endpoint)
    ;; No writable record is supplied, even to a misconfigured endpoint. The
    ;; endpoint port takes a decoded envelope and returns a decoded response.
    (endpoint {:protocol/version "arxana.v1" :message/id "catalogue-read-only-probe"
               :principal/id "catalogue-probe" :operation :clusion/assert :body {}})
    :else (throw (ex-info "Unknown backend feature" {:error :catalogue/unknown-feature :feature feature}))))

(defn probe
  "Run finite cases against explicit ports. Each case has :expected, a vector of
  {:path [...], :value ...} assertions. Missing fields differ from nil. Cases must
  have a known answer; fixture provisioning is outside this function. :snapshot
  on an edge-join fixture additionally checks registry drift. Exceptions are
  :error, not passing evidence. No commands or writes are issued to STORE."
  [feature ports {:keys [cases snapshot]}]
  (when-not (and (seq cases) (every? #(seq (:expected %)) cases))
    (throw (ex-info "A probe needs nonempty known answers" {:error :catalogue/invalid-probe})))
  (try
    (let [violations
          (vec (concat
                (mapcat (fn [{:keys [name expected] :as c}]
                          (let [actual (execute-case feature ports c)]
                            (for [{:keys [path value]} expected
                                  :let [observed (get-in actual path ::missing)]
                                  :when (not= value observed)]
                              {:case name :path path :expected value :actual observed}))) cases)
                (when (and (= feature :edge-join) snapshot (not= snapshot (mapv (fn [[revision roles]] {:type-revision revision :roles (vec roles)}) (joins/join-shapes))))
                  [{:reason :registry-snapshot-drift}])))]
      {:result (if (seq violations) :violation :pass) :violations violations})
    (catch Exception e {:result :error :violations [{:reason :probe-exception :message (.getMessage e)}]})))

(defn build
  "Build ten new choices, checks, links and partial-coverage run attestations.
  FIXTURES is keyed by feature; PINS and implementation digest are caller supplied.
  No existing worked choice is rebuilt or changed, and no coverage run is made."
  [{:keys [ports fixtures pins at implementation-digest]}]
  (let [groups
        (mapv
         (fn [{:keys [feature statement kind] :as f}]
           (let [fixture (get fixtures feature)
                 choice (catalogue/choice
                         {:choice/key (keyword "backend" (str (when-let [n (namespace feature)] (str n ".")) (name feature)))
                          :statement statement :kind (or kind :decision)
                          :sources (cond-> [(select-keys f [:repo :revision :path :section])]
                                     (= feature :edge-join)
                                     (conj {:repo "futon3c" :revision "fd5c6c3ec33b14f86607e61088536c5c6c926114"
                                            :path "test/futon3c/arxana/join_registry_test.clj"
                                            :section "committed-snapshot-matches-registry"}))
                          :lifecycle :accepted :applies-to [feature]
                          :rationale "Expose backend contracts and bounded evidence before Demo B cites them."
                          :alternatives []})
                 check (catalogue/check-record
                        {:check/key (keyword "backend-probe" (name feature))
                         :entry-point "futon3c.arxana.backend-catalogue/probe" :runtime :clojure
                         :input-contract {:feature feature :ports [:store :endpoint] :fixture :known-answers}
                         :output-contract {:result [:pass :violation :error] :coverage :partial}
                         :implementation-digest implementation-digest :fixtures [fixture]})
                 link (catalogue/link :design/checked-by choice check)
                 run (catalogue/check-run choice check pins at (probe feature ports fixture))]
             {:choice choice :check check :link link :run run})) features)]
    {:choices (mapv :choice groups) :checks (mapv :check groups)
     :runs (mapv :run groups) :records (vec (mapcat #(map % [:choice :check :link :run]) groups))}))
