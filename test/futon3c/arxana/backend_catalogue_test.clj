(ns futon3c.arxana.backend-catalogue-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.arxana.backend-catalogue :as b]
            [futon3c.arxana.design-catalogue :as d]
            [futon3c.arxana.design-catalogue-test :as dt]
            [futon3c.arxana.join-registry :as joins]
            [futon3c.arxana.store :as store]))

(defn expected [m] (mapv (fn [[k v]] {:path [k] :value v}) m))
(defn query-case [name request response]
  {:name name :request request :expected (expected response)})
(def empty-page {:status :ok :records [] :truncated? false})
(def fixtures
  {:records-by-type
   {:cases [(query-case :bounded {:type-revision [:design/choice 1] :limit 1}
                        {:status :ok :records [{:id "a"}] :truncated? true})
            (query-case :complete {:type-revision [:design/choice 1] :limit 2}
                        {:status :ok :records [{:id "a"} {:id "b"}] :truncated? false})]}
   :records-by-field
   {:cases (mapv (fn [kind]
                   (query-case kind (cond-> {:record-kind kind :field :label :value :example :limit 200}
                                      (= kind :clusion) (assoc :type-revision [:design/choice 1]))
                               {:status :ok :records [{:id (name kind)}] :truncated? false}))
                 [:clusion :external-reference :attestation :assessment :attempt :mark-occurrence])}
   :records-by-field-range
   {:cases [(query-case :numeric {:field :start :type-revision [:design/choice 1]
                                  :lower {:value 0 :inclusive? true} :upper {:value 5 :inclusive? false}}
                        {:status :ok :records [{:id "numeric"}] :truncated? false})]}
   :records-by-span-overlap
   {:cases [(query-case :half-open {:type-revision [:design/choice 1] :start-field :start :end-field :end
                                    :start 3 :end 5 :interval :half-open}
                        {:status :ok :records [{:id "overlap"}] :truncated? false})]}
   :type-census
   {:cases [(query-case :counts {:limit 200}
                        {:status :ok :counts [{:record-kind :clusion :type-revision [:design/choice 1] :count 2}]
                         :truncated? false})]}
   :attestations
   {:cases [(query-case :bounded {:subject-clusion-id "a" :limit 1}
                        {:status :ok :attestation-ids ["run-b"] :total 2 :truncated? true})
            (query-case :complete {:subject-clusion-id "a" :limit 2}
                        {:status :ok :attestation-ids ["run-b" "run-a"] :total 2 :truncated? false})]}
   :edge-join
   {:snapshot (mapv (fn [[revision roles]] {:type-revision revision :roles (vec roles)}) (joins/join-shapes))
    :cases [(query-case :linked {:patterns [{:edge-type [:design/checked-by 1] :bind {:choice "c" :check "k"}}
                                           {:edge-type [:design/checked-by 1] :bind {:choice "c" :check "k"}}]
                                :limit 2}
                        {:status :ok :rows [{:c "a" :k "check" :edges ["link" "link"]}]
                         :truncated? false})]}
   :dependence
   {:cases [{:name :fibre :operation :fibre :args [[:design/checked-by 1] :check :choice "check" dt/at]
             :expected (expected {:status :ok :ids ["a"]})}
            {:name :staleness :operation :staleness :args [[:design/checked-by 1] :choice :check "a" dt/at]
             :expected (expected {:status :ok :stale? true :stale-because ["sup"]})}]}
   :catalogue/view
   {:cases [{:name :empty-complete :at dt/at
             :expected (expected {:status :ok :choices []})}]}
   :catalogue/read-only-endpoint
   {:cases [{:name :write-refusal
             :expected [{:path [:error :error/code] :value :futon4a.catalogue/read-only}]}]}})

(def records
  {"a" {:id "a"} "check" {:id "check"}
   "link" {:id "link" :endpoints [{:role :choice :target {:clusion/id "a"}}
                                   {:role :check :target {:clusion/id "check"}}]}
   "sup" {:id "sup" :event-time dt/at}})

(defn fixture-store
  "Finite query transcript plus real dependence/view execution. This port tests
  the probes, not XTDB's query implementation (covered by futon1b's tests)."
  [mutate calls]
  (let [responses (into {} (for [[feature fixture] fixtures :when (b/query-features feature)
                                 c (:cases fixture)]
                            [(assoc (:request c) :query/type feature)
                             (into {} (map (fn [{:keys [path value]}] [(first path) value]) (:expected c)))]))]
    (reify store/ArxanaGraphStore
      (bounded-query! [_ q]
        (swap! calls conj q)
        {:ok (mutate q
                     (or (get responses q)
                         (case (:query/type q)
                           :record-by-id {:status :ok :record (get records (:record-id q))}
                           :role-reverse {:status :ok :total 1 :truncated? false :watermark "fixture"
                                          :edge-ids [(if (= [:supersedes 1] (:edge-type q)) "sup" "link")]}
                           :records-by-type empty-page
                           (throw (ex-info "Unexpected fixture query" {:query q})))) )})
      (conditional-write! [_ _] (throw (ex-info "Probe attempted a write" {}))))))
(defn endpoint [envelope]
  (is (= :clusion/assert (:operation envelope)))
  (is (= {} (:body envelope)))
  {:error {:error/code :futon4a.catalogue/read-only}})
(defn ports [] {:store (fixture-store (fn [_ r] r) (atom [])) :endpoint endpoint})

(deftest all-probes-and-record-validation
  (let [original (d/worked-choices)
        built (b/build {:ports (ports) :fixtures fixtures :pins [{:fixture :invented-finite-query-transcript}]
                        :at dt/at :implementation-digest "invented-probe-digest"})]
    (is (= (count b/features) (count (:choices built))))
    (is (every? #(= :pass (get-in % [:content :result])) (:runs built)))
    (is (every? :ok (dt/validate (:records built))))
    (is (= original (d/worked-choices)))
    (is (not-any? #(= :coverage (get-in % [:asserted-content :choice/key])) (:choices built)))
    (doseq [c (:choices built)]
      (is (= {:status :enforced} (d/enforcement-status (:records built) (:id c)))))
    (is (every? #(= :partial (:coverage %)) (:runs built)))))

(deftest planted-bad-probes
  (doseq [[feature mutation]
          [[:records-by-type (fn [_ r] (dissoc r :truncated?))]
           [:records-by-span-overlap (fn [_ r] (update r :records conj {:id "touching"}))]
           [:attestations (fn [_ r] (assoc r :total 0))]]]
    (testing (str feature)
      (is (= :violation (:result (b/probe feature
                                         {:store (fixture-store mutation (atom []))}
                                         (fixtures feature)))))))
  (is (= :violation (:result (b/probe :catalogue/read-only-endpoint {:endpoint (constantly {:ok {}})}
                                     (fixtures :catalogue/read-only-endpoint)))))
  (is (= :violation (:result (b/probe :edge-join (ports) (assoc (fixtures :edge-join) :snapshot {})))))
  (doseq [feature [:dependence :catalogue/view]]
    (is (= :violation
           (:result (b/probe feature {:store (fixture-store (fn [_ r] (assoc r :truncated? true)) (atom []))}
                             (fixtures feature)))))))

(deftest incomplete-fixture-and-errors-never-pass
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"known answers" (b/probe :type-census (ports) {:cases []})))
  (is (= :error (:result (b/probe :type-census
                                 {:store (fixture-store (fn [_ _] (throw (ex-info "offline" {}))) (atom []))}
                                 (fixtures :type-census)))))
  (let [calls (atom [])]
    (b/probe :dependence {:store (fixture-store (fn [_ r] r) calls)} (fixtures :dependence))
    (is (every? #(= 200 (:limit %)) (filter #(= :role-reverse (:query/type %)) @calls)))))

(deftest ^:slow probes-on-private-xtdb-node
  ;; These writes provision only the isolated test node, before probes run. The
  ;; supplied probe store implements no write port. No HTTP server is started.
  (let [start-node (requiring-resolve 'xtdb.node/start-node)
        command! (requiring-resolve 'futon1b-arxana/conditional-command!)
        open-index (requiring-resolve 'futon1b-arxana-index/open-index)
        rebuild! (requiring-resolve 'futon1b-arxana-index/rebuild!)
        att-query (requiring-resolve 'futon1b-arxana-index/attestations)
        file (java.io.File/createTempFile "backend-probe" ".db")
        index (open-index (.getPath file))
        clusion (fn [id start end] {:id (str "arxana:clusion:sha256:" id)
                                    :type-revision [:probe/span 1] :endpoints []
                                    :asserted-content {:start start :end end :label [:example :other]}})
        a (clusion "a" 2 4) touch (clusion "touch" 5 7) empty-span (clusion "empty" 4 4)
        kinds {:external-reference :external-reference/assert :attestation :evidence/append
               :assessment :assessment/append :attempt :attempt/append :mark-occurrence :mark-occurrence/assert}
        other (into {} (for [[kind _] kinds]
                         [kind (merge {:id (str "arxana:" (if (= kind :attestation) "attestation" "record") ":sha256:" (name kind)) :label :example}
                                      (case kind
                                        :attestation {:subject-clusion-id (:id a) :method {:check/id "probe"}}
                                        :assessment {:subject-clusion-id (:id a)}
                                        :external-reference {:record/id "outside" :store/id "fixture"}
                                        :attempt {:subject/id (:id a) :outcome :ran-with-output}
                                        :mark-occurrence {:locator {:start 2 :end 4}}))]))
        link {:id "arxana:clusion:sha256:link" :type-revision [:design/checked-by 1]
              :asserted-content {} :endpoints [{:role :choice :kind :clusion :target {:clusion/id (:id a)}}
                                               {:role :check :kind :clusion :target {:clusion/id "check"}}]}]
    (try
      (with-open [node (start-node)]
        (doseq [[op r] (concat (map #(vector :clusion/assert %) [a touch empty-span link])
                               (for [[kind r] other] [(kinds kind) r]))]
          (let [r (command! node {:protocol/version "arxana.v1" :message/id (:id r)
                                  :operation op :body {:record r}})]
            (is (not (:error r)))))
        (rebuild! index node)
        (let [s (reify store/ArxanaGraphStore
                  (bounded-query! [_ q]
                    {:ok (if (= :attestations (:query/type q))
                           (att-query index node (:subject-clusion-id q) (:limit q))
                           ((requiring-resolve (symbol "futon1b-arxana" (name (:query/type q)))) node q))}))
              run (fn [feature cases]
                    (let [r (b/probe feature {:store s} {:cases cases})]
                      (is (= :pass (:result r)) (pr-str r))))]
          (run :records-by-type
               [(query-case :limited {:type-revision [:probe/span 1] :limit 1}
                            {:status :ok :records [a] :truncated? true})
                (query-case :all {:type-revision [:probe/span 1] :limit 3}
                            {:status :ok :records [a empty-span touch] :truncated? false})])
          (run :records-by-field
               (into [(query-case :vector-element {:type-revision [:probe/span 1] :field :label :value :example}
                                  {:status :ok :records [a empty-span touch] :truncated? false})]
                     (for [[kind r] other]
                       (query-case kind {:record-kind kind :field :label :value :example}
                                   {:status :ok :records [r] :truncated? false}))))
          (run :records-by-field-range
               [(query-case :numbers {:type-revision [:probe/span 1] :field :start
                                      :lower {:value 0 :inclusive? true} :upper {:value 3 :inclusive? false}}
                            {:status :ok :records [a] :truncated? false})])
          (run :records-by-span-overlap
               [(query-case :exclude-touch-and-empty {:type-revision [:probe/span 1] :start-field :start :end-field :end
                                                       :start 3 :end 5 :interval :half-open}
                            {:status :ok :records [a] :truncated? false})])
          (run :attestations
               [(query-case :known-count {:subject-clusion-id (:id a) :limit 1}
                            {:status :ok :attestation-ids [(:id (:attestation other))] :total 1 :truncated? false})])
          (run :type-census
               [(query-case :known-counts {:limit 200}
                            {:status :ok :truncated? false
                             :counts (vec (sort-by (juxt :record-kind (comp pr-str :type-revision))
                                                   (concat [{:record-kind :clusion :type-revision [:probe/span 1] :count 3}
                                                            {:record-kind :clusion :type-revision [:design/checked-by 1] :count 1}]
                                                           (map #(hash-map :record-kind % :count 1) (keys other)))))})])
          (run :edge-join
               [(query-case :linked {:patterns [{:edge-type [:design/checked-by 1] :bind {:choice "c" :check "k"}}
                                                {:edge-type [:design/checked-by 1] :bind {:choice "c" :check "k"}}] :limit 2}
                            {:status :ok :truncated? false :rows [{:c (:id a) :k "check" :edges [(:id link) (:id link)]}]})])))
      (finally (.delete file)))))
