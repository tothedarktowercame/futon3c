(ns futon3c.scripts.mission-scope-ingest-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.scripts.mission-scope-ingest :as ingest]))

(deftest removal-prefers-live-batch-contract
  (let [calls (atom [])
        documents [{:table :hyperedges :id "hx-1"}
                   {:table :entities :id "scope-1"}]]
    (with-redefs [ingest/http-edn
                  (fn [_ method url & [body]]
                    (swap! calls conj [method url body])
                    {:status 200 :body {:ok true :count 2}})]
      (is (= {:deleted-count 2
              :retained-entity-count 0
              :retraction-mode :batch}
             (#'ingest/delete-docs! :client "http://substrate" "api" documents)))
      (is (= 1 (count @calls)))
      (is (= [:post "http://substrate/api/alpha/documents/retract"]
             (subvec (first @calls) 0 2)))
      (is (= documents (get-in @calls [0 2 :documents]))))))

(deftest expensive-read-admission-is-retried
  (let [attempts (atom 0)
        sleeps (atom [])]
    (with-redefs [ingest/http-edn
                  (fn [& _]
                    (if (< (swap! attempts inc) 3)
                      {:status 503 :body {:error :expensive-read-busy
                                          :retry-after-seconds 2}}
                      {:status 200 :body {:ok true}}))]
      (binding [ingest/*retry-sleep!* #(swap! sleeps conj %)]
        (is (= {:status 200 :body {:ok true}}
               (#'ingest/http-edn-read :client "http://substrate/read")))
        (is (= 3 @attempts))
        (is (= [2000 2000] @sleeps))))))

(deftest hyperedges-by-type-walks-cursor-pages-in-order
  (let [urls (atom [])
        pages [{:count 5
                :hyperedges [{:hx/id "hx-1"} {:hx/id "hx-2"}]
                :next-cursor "hx|2"}
               {:hyperedges [{:hx/id "hx-3"} {:hx/id "hx-4"}]
                :next-cursor "hx|4"}
               {:hyperedges [{:hx/id "hx-5"}]}]]
    (#'ingest/reset-run-caches!)
    (with-redefs [ingest/http-edn
                  (fn [_ method url & [_body]]
                    (is (= :get method))
                    (let [page (nth pages (count @urls))]
                      (swap! urls conj url)
                      {:status 200 :body page}))]
      (is (= ["hx-1" "hx-2" "hx-3" "hx-4" "hx-5"]
             (mapv :hx/id
                   (#'ingest/hyperedges-by-type
                    :client "http://substrate" "mission-scope/test"))))
      (is (= ["http://substrate/api/alpha/hyperedges?type=mission-scope%2Ftest&limit=5000"
              (str "http://substrate/api/alpha/hyperedges?type=mission-scope%2Ftest"
                   "&limit=5000&after=hx%7C2&include-total=false")
              (str "http://substrate/api/alpha/hyperedges?type=mission-scope%2Ftest"
                   "&limit=5000&after=hx%7C4&include-total=false")]
             @urls)))))

(deftest hyperedges-by-type-fails-closed-after-short-cursor-walk
  (#'ingest/reset-run-caches!)
  (with-redefs [ingest/http-edn
                (fn [& _]
                  {:status 200
                   :body {:count 2 :hyperedges [{:hx/id "hx-1"}]}})]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"futon1b hyperedge result truncated"
         (#'ingest/hyperedges-by-type
          :client "http://substrate" "mission-scope/truncated")))))

(deftest hyperedges-by-type-single-page-issues-one-request
  (let [urls (atom [])]
    (#'ingest/reset-run-caches!)
    (with-redefs [ingest/http-edn
                  (fn [_ _method url & [_body]]
                    (swap! urls conj url)
                    {:status 200
                     :body {:count 2
                            :hyperedges [{:hx/id "hx-1"} {:hx/id "hx-2"}]}})]
      (is (= ["hx-1" "hx-2"]
             (mapv :hx/id
                   (#'ingest/hyperedges-by-type
                    :client "http://substrate" "mission-scope/single"))))
      (is (= 1 (count @urls))))))

(deftest removal-falls-back-only-for-an-unported-route
  (let [calls (atom [])
        documents [{:table :hyperedges :id "hx-1"}
                   {:table :entities :id "scope-1"}]]
    (with-redefs [ingest/http-edn
                  (fn [_ method url & [body]]
                    (swap! calls conj [method url body])
                    (cond
                      (= url "http://substrate/api/alpha/documents/retract")
                      {:status 404 :body "No context found for request"}

                      (= method :get)
                      {:status 200 :body {:hx/id "hx-1" :hx/type :mission-scope/phase
                                          :hx/endpoints ["mission" "scope-1"]}}

                      :else {:status 200 :body {:ok true :retracted? true}}))]
      (is (= {:deleted-count 1
              :retained-entity-count 1
              :retraction-mode :per-hyperedge-archival}
             (#'ingest/delete-docs! :client "http://substrate" "api" documents)))
      (is (= 3 (count @calls)))
      (is (= [:post "http://substrate/api/alpha/hyperedge"]
             (subvec (last @calls) 0 2)))
      (is (= {:hx/id "hx-1" :hx/type :mission-scope/phase
              :hx/endpoints ["mission" "scope-1"] :hx/op "retract"
              :penholder "api"}
             (last (last @calls)))))))

(deftest removal-does-not-mask-a-genuine-rejection
  (let [calls (atom 0)]
    (with-redefs [ingest/http-edn
                  (fn [& _]
                    (swap! calls inc)
                    {:status 403 :body {:error {:layer 3 :reason :forbidden}}})]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"request failed"
                            (#'ingest/delete-docs!
                             :client "http://substrate" "api"
                             [{:table :hyperedges :id "hx-1"}])))
      (is (= 1 @calls)))))

(deftest operator-gates-are-archival-not-removal-candidates
  (is (#'ingest/archival-binder? "operator-gate"))
  (is (#'ingest/archival-binder? :mission-scope/operator-gate))
  (is (not (#'ingest/archival-binder? "eightfold-phase"))))

(deftest operator-gates-become-stable-typed-substrate-records
  (let [raw-scope {:scope-id "M-learning-loop:scope-999"
                   :binder-type "operator-gate"
                   :parent nil
                   :ends [{:role "entity" :ident "M-learning-loop"}
                          {:role "environment"
                           :name "operator gate: operator-acceptance"
                           :phase "head"}
                          {:role "operator-gate"
                           :kind "operator-acceptance"
                           :text "inspect the capability graph"
                           :source-line 4}]
                   :hx/content {:position 80 :end 150}
                   :gate-kind "operator-acceptance"
                   :gate-text "inspect the capability graph"
                   :source-line 4}
        [scope] (#'ingest/stable-scopes-for-binder
                 "M-learning-loop" "/missing/M-learning-loop.md"
                 [raw-scope] "operator-gate")
        entity (#'ingest/scope-entity-spec
                "M-learning-loop" "/missing/M-learning-loop.md" scope)
        hyperedge (#'ingest/scope-hyperedge
                   {:id "mission-doc/learning-loop"
                    :external-id "M-learning-loop"}
                   {:id (:id entity) :name (:name entity)} [] scope)]
    (is (some #{"operator-gate"}
              (var-get #'ingest/structural-binders)))
    (is (= "learning-loop/operator-gate/operator-acceptance/inspect-the-capability-graph"
           (:scope-id scope)))
    (is (= "scope/operator-gate" (:type entity)))
    (is (= {:operator-gate/kind "operator-acceptance"
            :operator-gate/text "inspect the capability graph"
            :operator-gate/source-line 4}
           (select-keys (:props entity)
                        [:operator-gate/kind :operator-gate/text
                         :operator-gate/source-line])))
    (is (= "mission-scope/operator-gate" (:hx/type hyperedge)))
    (is (= {:operator-gate/kind "operator-acceptance"
            :operator-gate/text "inspect the capability graph"
            :operator-gate/source-line 4}
           (select-keys (:props hyperedge)
                        [:operator-gate/kind :operator-gate/text
                         :operator-gate/source-line])))))
