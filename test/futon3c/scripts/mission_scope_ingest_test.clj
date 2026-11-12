(ns futon3c.scripts.mission-scope-ingest-test
  (:require [clojure.test :refer [deftest is testing]]
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

(deftest mission-scope-reads-push-the-mission-filter-down
  (let [urls (atom [])]
    (#'ingest/reset-run-caches!)
    (with-redefs [ingest/http-edn
                  (fn [_ _method url & [_body]]
                    (swap! urls conj url)
                    {:status 200
                     :body {:hyperedges [{:hx/id "hx-1"
                                          :hx/props {:mission "M-x"
                                                     :scope/binder-type "loose-section"}}]}})]
      (is (= ["hx-1"]
             (mapv :hx/id
                   (#'ingest/mission-scope-hyperedges
                    :client "http://substrate" "loose-section" "M-x"))))
      (is (= [(str "http://substrate/api/alpha/hyperedges?type=mission-scope%2Floose-section"
                   "&mission=M-x&limit=250&include-total=false")]
             @urls)))))

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

(deftest mission-doc-id-follows-the-watcher-label
  (testing "the label comes from the watcher root covering the path, not basename+\"-d\""
    (is (= "futon4-elisp-d/mission/formal-arxana"
           (#'ingest/mission-doc-id "M-formal-arxana"
                                    "/home/joe/code/futon4/holes/missions/M-formal-arxana.md")))
    (is (= "futon6-py-d/mission/differentiable-math"
           (#'ingest/mission-doc-id "M-differentiable-math"
                                    "/home/joe/code/futon6/holes/missions/M-differentiable-math.md")))
    (is (= "futon3c-d/mission/repl-circus"
           (#'ingest/mission-doc-id "M-repl-circus"
                                    "/home/joe/code/futon3c/holes/missions/M-repl-circus.md")))
    (is (= "futon5-d2/mission/categorical-code"
           (#'ingest/mission-doc-id "M-categorical-code"
                                    "/home/joe/code/futon5/holes/missions/M-categorical-code.md")))))

(deftest mission-doc-id-falls-back-to-basename-label-outside-watch-roots
  (testing "a path under no watch root keeps the old basename+\"-d\" behaviour"
    (is (= "futon9x-d/mission/side-quest"
           (#'ingest/mission-doc-id "M-side-quest"
                                    "/home/joe/code/futon9x/holes/missions/M-side-quest.md")))))

(deftest scope-lane-writes-no-mission-doc-entity-for-excursion-stems
  (let [ensured (atom [])]
    (binding [ingest/*dry-run?* true]
      (with-redefs [ingest/ensure-entity!
                    (fn [_ _ _ spec]
                      (swap! ensured conj spec)
                      spec)]
        (ingest/ingest-scope-tree! {:client nil
                                    :base-url "http://unused"
                                    :penholder "test"
                                    :path "test/fixtures/scope-trees/E-side-quest.json"})
        (is (empty? (filter #(= "mission/doc" (:type %)) @ensured))
            "E- stems belong to the watcher's excursion/doc entities")))))

(deftest scope-lane-still-writes-mission-doc-entity-for-mission-stems
  (let [ensured (atom [])]
    (binding [ingest/*dry-run?* true]
      (with-redefs [ingest/ensure-entity!
                    (fn [_ _ _ spec]
                      (swap! ensured conj spec)
                      spec)]
        (ingest/ingest-scope-tree! {:client nil
                                    :base-url "http://unused"
                                    :penholder "test"
                                    :path "test/fixtures/scope-trees/M-side-quest.json"})
        (is (= ["futon3c-d/mission/side-quest"]
               (map :id (filter #(= "mission/doc" (:type %)) @ensured))))))))
