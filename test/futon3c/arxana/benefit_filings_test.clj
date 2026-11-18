(ns futon3c.arxana.benefit-filings-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.arxana.benefit-filings :as f]
            [futon3c.arxana.benefit-filings-catalogue :as c]
            [futon3c.arxana.design-catalogue-test :as fixtures]
            [futon3c.arxana.service :as service]
            [futon3c.arxana.store :as store]))
(def at fixtures/at)
(defn record [id type content] {:id id :type-revision type :asserted-content content :endpoints []})
(defn edge [id type a b]
  (assoc (record id type {}) :endpoints
         (mapv (fn [[role kind target]] {:role role :kind kind :target {(if (= kind :entity) :entity/id :clusion/id) target}}) [a b])))
(def rs
  [(record "b" [:benefit-line 1] {:benefit/line "invented-H"})
   (record "i" [:workplan/item 1] {:item/id "M-invented"})
   (record "unnamed" [:workplan/item 1] {:item/id "M-unnamed"})
   (record "e" [:lexicon/entity 1] {:entity/slug "invented"})
   (edge "s" [:workplan/serves 1] [:item :clusion "i"] [:benefit-line :clusion "b"])
   (edge "s2" [:workplan/serves 1] [:item :clusion "unnamed"] [:benefit-line :clusion "b"])
   (edge "sub" [:workplan/serves 2] [:item :entity "substrate"] [:benefit-line :clusion "b"])
   (edge "n" [:workplan/names 1] [:item :clusion "i"] [:entity :clusion "e"])
   {:id "a" :subject-clusion-id "s" :basis :model :standing :unverified :coverage :partial
    :source-refs [{:path "invented-source"}] :content {}}])
(defn fake [calls override]
  (reify store/ArxanaGraphStore
    (bounded-query! [_ q]
      (swap! calls conj q)
      (or (override q)
          {:ok (case (:query/type q)
                 :record-by-id (if-let [r (first (filter #(= (:record-id q) (:id %)) rs))]
                                 {:status :ok :record r} {:status :missing})
                 :records-by-type {:status :ok :truncated? false :records (filterv #(= (:type-revision q) (:type-revision %)) rs)}
                 :attestations (let [ids (mapv :id (filter #(= (:subject-clusion-id q) (:subject-clusion-id %)) rs))]
                                 {:status :ok :total (count ids) :truncated? false :attestation-ids ids})
                 :role-reverse (let [ids (mapv :id (filter #(and (= (:edge-type q) (:type-revision %))
                                                                                (some (fn [ep] (and (= (:endpoint-role q) (:role ep))
                                                                                                    (= (:endpoint-id q) (val (first (:target ep)))))) (:endpoints %))) rs))]
                                 {:status :ok :total (count ids) :truncated? false :edge-ids ids}))}))
    (conditional-write! [_ _] (throw (ex-info "No writes" {})))))
(def request {:benefit-id "b" :valid-time at})
(deftest parents-evidence-and-lanes
  (let [calls (atom []) s (fake calls (constantly nil)) v (f/read-filings s request)]
    (is (= :ok (:status v)))
    (is (= ["i" "unnamed"] (mapv #(get-in % [:item :record :id]) (:items v))))
    (is (= [] (:names (second (:items v)))))
    (is (= :model (get-in v [:items 0 :serves 0 :evidence 0 :basis])))
    (is (= ["substrate"] (mapv :entity/id (:substrate v))))
    (is (not-any? #(= "substrate" (:endpoint-id %)) @calls))
    (is (= {:serves-v1 2 :serves-v2 1 :items 2 :naming-rows 1 :named-items 1 :entities 1} (:counts v)))
    (is (= 2 (get-in (f/read-filings s {:valid-time at}) [:benefits 0 :serves-v1])))
    (is (= :pass (:result (c/probe s request [[[:status] :ok] [[:counts :items] 2]]))))))
(deftest incomplete-not-a-partial-page
  (doseq [type [:role-reverse :attestations :records-by-type]]
    (let [s (fake (atom []) #(when (= type (:query/type %)) {:ok {:status :ok :truncated? true}}))
          request (if (= type :records-by-type) {:valid-time at} request)
          v (f/read-filings s request)]
      (is (= :inconclusive (:status v)))
      (is (not (contains? v :items)))
      (is (= :pass (:result (c/probe s request [[[:status] :inconclusive] [[:reason] :truncated]]))))))
  (is (= :inconclusive (:status (f/read-filings (fake (atom []) (constantly {:error {:error/code :offline}})) request)))))
(deftest registry-and-citations
  (let [records (c/records {:revision "invented-sha" :implementation-digest "invented-digest"
                            :fixtures [{:name :unnamed-parent} {:name :truncated-reverse}]})]
    (is (every? :ok (fixtures/validate records)))
    (is (= :cascade/benefit-filings (:view (c/manifest))))))
(deftest dispatch-schema
  (let [cfg {:graph-store (fake (atom []) (constantly nil))}
        envelope {:protocol/version "arxana.v1" :message/id "invented" :principal/id "reader"
                  :operation :cascade/benefit-filings :body request}]
    (is (= :ok (get-in (service/dispatch cfg envelope) [:ok :status])))
    (is (:error (service/dispatch cfg (assoc-in envelope [:body :unknown] true))))))

(deftest coverage-query-and-probe-cannot-pass-vacuously
  (let [documented (mapv (fn [read] {:type-revision [:design/choice 1]
                                    :asserted-content {:applies-to [(:feature read)]}})
                         (remove #(= :cascade/benefit-filings (:feature %)) (:reads (c/manifest))))
        s (fake (atom []) #(when (= :records-by-type (:query/type %))
                            {:ok {:status :ok :truncated? false :records documented}}))]
    (is (= {:status :ok :result :violation :violations [:cascade/benefit-filings]} (c/coverage s at)))
    (is (thrown? clojure.lang.ExceptionInfo (c/probe s request [])))))

(deftest h1-count-shaped-invented-fixture
  (let [parents (mapv #(record (str "i" %) [:workplan/item 1] {:item/id (str "M-invented-" %)}) (range 44))
        entities (mapv #(record (str "e" %) [:lexicon/entity 1] {:entity/slug (str "invented-" %)}) (range 32))
        serving (mapcat (fn [n] [(edge (str "s" n) [:workplan/serves 1] [:item :clusion (str "i" n)] [:benefit-line :clusion "b"])
                                 (edge (str "sub" n) [:workplan/serves 2] [:item :entity (str "substrate-" n)] [:benefit-line :clusion "b"])]) (range 44))
        naming (concat (map #(edge (str "n" %) [:workplan/names 1] [:item :clusion (str "i" %)] [:entity :clusion (str "e" %)]) (range 27))
                       (map #(edge (str "extra" %) [:workplan/names 1] [:item :clusion (str "i" %)] [:entity :clusion (str "e" (+ 27 (mod % 5)))]) (range 24)))]
    (with-redefs [rs (vec (concat [(record "b" [:benefit-line 1] {:benefit/line "invented-H1"})] parents entities serving naming))]
      (let [s (fake (atom []) (constantly nil))]
        (is (= :pass (:result (c/probe s request c/h1-answers))))
        (is (= 44 (get-in (f/read-filings s request) [:counts :items])))))))
