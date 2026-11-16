(ns futon3c.arxana.workplan-service-test
  (:require [clojure.test :refer [deftest is]]
            [futon1b-gates :as gates]
            [futon1b-server :as f1]
            [futon3c.arxana.json :as json]
            [futon3c.arxana.service :as service]
            [futon3c.arxana.sessionista-introductions :as introductions]
            [futon3c.arxana.store :as store]
            [futon3c.arxana.store-http :as store-http]
            [futon3c.arxana.workplan :as workplan]
            [futon3c.arxana.workplan-backlinks :as backlinks]
            [futon3c.arxana.workplan-service :as subject]
            [futon3c.arxana.workplan-transfer :as transfer]
            [xtdb.node :as xtn])
  (:import [java.io File]
           [java.net ServerSocket URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers]))

;; Entirely invented item, entity, evidence and text.
(def fixture-slice
  {:slice "M-fixture-lantern"
   :records [{:record :clusion :local-id :item :type-revision [:workplan/item 1]
              :identity {:item/id "M-fixture-lantern"} :endpoints []}
             {:record :attestation :subject :item :producer "fixture-author"
              :basis :exact :standing :verified :coverage :complete
              :method {:id "fixture" :version 1} :content {:title "Invented lantern"}}]})

(defn- free-port [] (with-open [s (ServerSocket. 0)] (.getLocalPort s)))

(deftest lifecycle-and-client-operations-through-http
  (gates/seed-mission-contract!)
  (let [idx (File/createTempFile "fold-test-" ".sqlite")
        backlinks-file (File/createTempFile "fold-backlinks-" ".edn")
        _ (.delete idx)
        _ (spit backlinks-file (pr-str {:backlinks [] :stats {} :index/as-of "fixture"}))]
    (try
      (with-open [node (xtn/start-node)]
        (let [server (f1/start-server! {:node node :port 0 :bind-host "127.0.0.1" :arxana-index-path (.getPath idx)})
              options {:port (free-port) :store-url (str "http://127.0.0.1:" (.getPort (.getAddress server)))
                       :backlinks-index (.getPath backlinks-file)}]
          (try
            (let [h (subject/start! options) cfg (:cfg h)
                  dispatch #(service/dispatch cfg %)
                  client (HttpClient/newHttpClient)
                  call (fn [op body]
                         (let [start (System/nanoTime)
                               req (-> (HttpRequest/newBuilder (URI/create (str "http://127.0.0.1:" (:port options) "/arxana/v1")))
                                       (.header "Content-Type" "application/json")
                                       (.POST (HttpRequest$BodyPublishers/ofString
                                               (json/encode {:protocol/version "arxana.v1" :message/id (str (random-uuid))
                                                             :operation op :principal/id "fixture-reviewer" :body body}))) .build)
                               response (.send client req (HttpResponse$BodyHandlers/ofString))
                               result (json/decode (.body response))]
                           (println "FOLD_OP_MS" op (/ (- (System/nanoTime) start) 1e6))
                           (is (= 200 (.statusCode response)))
                           (is (:ok result) (pr-str result))
                           (:ok result)))
                  loaded (workplan/materialise! dispatch fixture-slice)
                  rules {:rules [{:entity/slug "fixture-lantern" :forms [{:form/text "fixture lantern" :enabled? true}]}]}]
              (is (identical? h (subject/start! options)))
              (is (thrown? clojure.lang.ExceptionInfo (subject/start! (assoc options :port (free-port)))))
              (is (empty? (:failures loaded)))
              (backlinks/materialise-rules! dispatch rules)
              (is (= 1 (:result/total (call :workplan/items {:query/limit 1}))))
              (is (= "M-fixture-lantern" (get-in (call :workplan/item-view {:item/id "M-fixture-lantern"}) [:item :asserted-content :item/id])))
              (is (some #{"fixture-lantern"} (:entities (call :workplan/entity-backlinks {}))))
              (with-redefs [introductions/resolve-slug introductions/slugify]
                (let [intro (call :workplan/introduce {:evidence/id "invented-turn" :text "[[fixture comet | fixture lantern]]"
                                                      :actor "fixture-reviewer" :time "2026-01-01T00:00:00Z" :candidate/slugs []})
                      row (first (:introductions intro)) ref-id (first (:referent/ids row))]
                  (is ref-id)
                  (call :workplan/assess-referent {:subject/id ref-id :verdict :accepted :actor "fixture-reviewer"
                                                 :reason "invented review" :time "2026-01-02T00:00:00Z"})
                  (is (= :accepted (get-in (call :workplan/entity-backlinks {:entity/slug "fixture-comet"}) [:referents 0 :review/status])))
                  (is (= 1 (count (:result/edge-ids (call :graph/query {:query/type :role-reverse :edge/type [:workplan/introduction 1]
                                                                    :endpoint/role :entity :endpoint/id (:entity/id row) :time/order :asc})))))))
              ;; Transfer dry run, append and exact replay through the isolated store.
              (let [entry {:operation :clusion/assert
                           :record (get-in (workplan/plan (assoc-in fixture-slice [:records 0 :identity :item/id] "M-fixture-extra")) [:records 0])}
                    gs (:graph-store cfg) p (transfer/plan gs [entry])]
                (is (= 1 (count (:new p))))
                (is (= :arxana.store/record-not-found (get-in (store/get-entity gs (get-in entry [:record :id])) [:error :error/code])))
                (transfer/execute! gs p "fixture-transfer" 0)
                (is (= 1 (count (:existing (transfer/plan gs [entry])))))
                (is (empty? (:new (transfer/plan gs [entry]))))
                (let [bad (assoc-in entry [:record :asserted-content :title] "conflicting content")
                      conflict (transfer/plan gs [bad])]
                  (is (= 1 (count (:conflicts conflict))))
                  (is (thrown? clojure.lang.ExceptionInfo (transfer/execute! gs conflict "must-not-write" 0)))))
              (is (true? (subject/stop!)))
              (is (true? (subject/stop!)))
              (is (= :stopped (:status (subject/health))))
              (with-open [occupied (ServerSocket. 0)]
                (is (thrown? clojure.lang.ExceptionInfo (subject/start! (assoc options :port (.getLocalPort occupied)))))))
            (finally (subject/stop!) (f1/stop-server! server)))))
      (finally (.delete idx) (.delete backlinks-file)))))

(deftest export-dependency-order-and-cycle-refusal
  (let [parent {:operation :clusion/assert :record {:id "a"}}
        child {:operation :clusion/assert :record {:id "b" :endpoints [{:target {:clusion/id "a"}}]}}
        evidence {:operation :evidence/append :record {:id "c" :subject-clusion-id "b"}}]
    (is (= [parent child evidence] (transfer/dependency-order [evidence child parent])))
    (is (thrown? clojure.lang.ExceptionInfo
                 (transfer/dependency-order [(assoc-in parent [:record :subject-clusion-id] "b") child])))))

(deftest transfer-batches-are-bounded-and-read-back
  (let [entries (mapv (fn [n] {:operation :clusion/assert :record {:id (str "fixture-" n)}}) (range 1001))
        written (atom {}) sizes (atom [])]
    (with-redefs [store-http/append-batch! (fn [_ _ batch]
                                           (swap! sizes conj (count batch))
                                           (swap! written into (map #(vector (get-in % [:record :id]) (:record %)) batch))
                                           {:ok {}})
                  store/get-entity (fn [_ id] {:ok (get @written id)})]
      (transfer/execute! nil {:new entries :conflicts []} "fixture-batches" 0)
      (is (= [500 500 1] @sizes))
      (is (= 1001 (count @written))))))
