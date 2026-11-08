(ns futon3c.arxana.formal-c3b-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon1b-arxana :as f1]
            [futon1b-gates :as gates]
            [futon1b-server :as f1-server]
            [futon3c.arxana.digest :as digest]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.service :as service]
            [futon3c.arxana.store :as store]
            [futon3c.arxana.store-http :as store-http]
            [xtdb.node :as xtn])
  (:import [com.sun.net.httpserver HttpServer]
           [java.io File]
           [java.net InetSocketAddress]
           [java.nio.charset StandardCharsets]))

(defn envelope [mid op body]
  {:protocol/version "arxana.v1" :message/id mid :operation op
   :principal/id "c3b" :body body})

(defn proposal []
  (records/make-clusion
   {:type-revision [:pattern/proposal 1]
    :endpoints [{:role :turn :kind :external-ref :target {:ref/id "turn-1"}}
                {:role :pattern :kind :external-ref :target {:ref/id "pattern-1"}}]
    :asserted-content {:intent :shown}}))

(defn occurrence []
  (records/make-clusion
   {:type-revision [:pattern/occurrence 1]
    :endpoints [{:role :pattern :kind :external-ref :target {:ref/id "pattern-1"}}
                {:role :turn :kind :external-ref :target {:ref/id "turn-1"}}]
    :asserted-content {:window ["turn-1"] :proposal "proposal-1"}}))

(defn attempt [subject outcome & [envelope? dynamics]]
  (cond-> {:subject/id subject
           :dynamics (or dynamics {:id "馬/occurrence" :version 1})
           :inputs {:turn {:ref/id "turn-1"}
                    :pattern {:ref/id "pattern-1"}}
           :outcome outcome :reason (name outcome)
           :actor {:tool "test"} :time (str "2026-11-08T00:00:0" (rand-int 9) "Z")}
    envelope? (assoc-in [:inputs :envelope] {:window ["turn-1"] :roles [:I1]})))

(defn dispatch [cfg mid op body] (service/dispatch cfg (envelope mid op body)))

(defn stand-in [bytes schema]
  (let [server (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)]
    (.createContext server "/record"
                    (reify com.sun.net.httpserver.HttpHandler
                      (handle [_ ex]
                        (let [schema' (if (.contains (str (.getQuery (.getRequestURI ex))) "minus40")
                                        "futon1b/v1" schema)]
                          (.add (.getResponseHeaders ex) "x-schema-authority" schema'))
                        (.sendResponseHeaders ex 200 (count bytes))
                        (with-open [out (.getResponseBody ex)] (.write out bytes)))))
    (.start server)
    server))

(deftest c3b-real-store-receipts-references-and-aggregate
  (gates/seed-mission-contract!)
  (let [db (File/createTempFile "formal-c3b-" ".db") _ (.delete db)
        bytes (.getBytes "turn evidence" StandardCharsets/UTF_8)
        authority (stand-in bytes "xiang/v1")
        endpoint (str "http://127.0.0.1:" (.getPort (.getAddress authority)) "/record")]
    (try
      (with-open [node (xtn/start-node)]
        (let [server (f1-server/start-server! {:node node :port 0 :bind-host "127.0.0.1"
                                               :arxana-index-path (.getPath db)})]
          (try
            (let [cfg {:graph-store (store-http/http-store
                                     (str "http://127.0.0.1:" (.getPort (.getAddress server)))
                                     f1/service-principal)}
                  p (proposal) o (occurrence)]
              (is (:ok (dispatch cfg "proposal" :clusion/assert {:record p})))
              (is (:ok (dispatch cfg "occurrence" :clusion/assert {:record o})))

              (testing "all attempt outcomes persist; absence needs an envelope"
                (doseq [outcome [:not-run :refused :failed :ran-with-output]]
                  (is (:ok (dispatch cfg (str "attempt-" (name outcome)) :attempt/append
                                     {:record (attempt (:id p) outcome)}))))
                (is (= :arxana.attempt/search-envelope-required
                       (get-in (dispatch cfg "bad-empty" :attempt/append
                                         {:record (attempt (:id p) :ran-no-output)})
                               [:error :error/code])))
                (is (:ok (dispatch cfg "empty" :attempt/append
                                   {:record (attempt (:id p) :ran-no-output true)})))
                (is (= :arxana.dynamics/unknown-revision
                       (get-in (dispatch cfg "unknown-dynamics" :attempt/append
                                         {:record (attempt (:id p) :not-run false
                                                           {:id "馬/occurrence" :version 999})})
                               [:error :error/code])))
                (let [q (dispatch cfg "attempt-query" :graph/query
                                  {:query/type :attempts :subject/id (:id p)
                                   :interpreter/id "馬/occurrence" :query/limit 20})]
                  (is (= 5 (count (get-in q [:ok :result/receipts]))))
                  (is (= #{"not-run" "refused" "failed" "ran-with-output" "ran-no-output"}
                         (set (map :outcome (get-in q [:ok :result/receipts])))))))

              (testing "typed external references never substitute for authority"
                (let [base {:ref/kind :http-record :store/id :xiang-turn-store
                            :store/endpoint endpoint :record/id "turn-1"
                            :schema/authority :xiang/v1 :valid-time "2026-11-08"
                            :revision/digest (digest/sha256-hex bytes)}
                      ref (records/make-external-reference base)]
                  (is (:ok (dispatch cfg "ref-write" :external-reference/assert {:record ref})))
                  (is (= :ok (get-in (dispatch cfg "ref-ok" :external-reference/resolve
                                                {:reference ref}) [:ok :status])))
                  (is (= :ok
                         (get-in (dispatch cfg "ref-minus40" :external-reference/resolve
                                           {:reference (records/make-external-reference
                                                       (assoc base :store/id :minus40
                                                              :record/id "minus40"
                                                              :schema/authority :futon1b/v1))})
                                 [:ok :status])))
                  (is (= :digest-mismatch
                         (get-in (dispatch cfg "ref-digest" :external-reference/resolve
                                           {:reference (assoc ref :revision/digest (apply str (repeat 64 "0")))})
                                 [:ok :status])))
                  (is (= :schema-drift
                         (get-in (dispatch cfg "ref-schema" :external-reference/resolve
                                           {:reference (assoc ref :schema/authority :minus40/v1)})
                                 [:ok :status])))
                  (is (= :unavailable
                         (get-in (dispatch cfg "ref-down" :external-reference/resolve
                                           {:reference (assoc ref :store/endpoint "http://127.0.0.1:1/record")})
                                 [:ok :status])))
                  (let [file (File/createTempFile "pattern-library-" ".md")]
                    (try
                      (spit file "pattern bytes")
                      (let [fb (.getBytes "pattern bytes" StandardCharsets/UTF_8)
                            library (records/make-external-reference
                                     {:ref/kind :file :store/id :pattern-library
                                      :store/endpoint (.getPath file)
                                      :record/id "library/pattern.md"
                                      :schema/authority :file/sha256
                                      :valid-time "2026-11-08"
                                      :revision/digest (digest/sha256-hex fb)})]
                        (is (= :ok (get-in (dispatch cfg "ref-file" :external-reference/resolve
                                                     {:reference library}) [:ok :status]))))
                      (finally (.delete file))))))

              (testing "aggregate is materialised, rebuildable, and not-run has zero weight"
                (let [att (records/make-attestation
                           {:subject-clusion-id (:id o) :producer "馬"
                            :basis :model :standing :unverified :coverage :complete
                            :method {:id "馬/occurrence" :version 1}
                            :content {} :source-refs []})]
                  (is (:ok (dispatch cfg "att" :evidence/append {:record att})))
                  ;; +1 occurrence and -0.5 ran-no-output. Four other outcomes,
                  ;; including not-run, contribute nothing.
                  (is (= 0.5 (get-in (dispatch cfg "score" :aggregate/query
                                               {:pattern/id "pattern-1" :window ["turn-1"]})
                                     [:ok :score])))
                  (let [before (get-in (dispatch cfg "score-before" :aggregate/query
                                                 {:pattern/id "pattern-1" :window ["turn-1"]})
                                       [:ok :score])]
                    (is (:ok (dispatch cfg "not-run-2" :attempt/append
                                       {:record (attempt (:id p) :not-run)})))
                    (is (= before (get-in (dispatch cfg "score-after" :aggregate/query
                                                    {:pattern/id "pattern-1" :window ["turn-1"]})
                                          [:ok :score]))))
                  (let [rebuilt (store-http/http-store
                                 (str "http://127.0.0.1:" (.getPort (.getAddress server)))
                                 f1/service-principal)]
                    (is (= :ok (get-in (store/bounded-query!
                                        rebuilt {:query/type :rebuild}) [:ok :status])))
                    ;; The deterministic full rebuild must reproduce the
                    ;; incrementally maintained aggregate exactly.
                    (is (= 0.5 (get-in (dispatch (assoc cfg :graph-store rebuilt)
                                                 "score-repeat" :aggregate/query
                                                 {:pattern/id "pattern-1" :window ["turn-1"]})
                                         [:ok :score])))))
                ;; Bypass the server catch-up: authoritative receipt advances,
                ;; aggregate watermark does not, so the public read says stale.
                (let [r (records/make-attempt (attempt (:id p) :not-run))]
                  (f1/conditional-command! node
                                           {:protocol/version "arxana.v1" :message/id "behind-c3b"
                                            :operation :attempt/append :body {:record r}})
                  (is (= :stale (get-in (dispatch cfg "stale-score" :aggregate/query
                                                   {:pattern/id "pattern-1" :window ["turn-1"]})
                                         [:error :error/category])))))
              )
            (finally (f1-server/stop-server! server)))
          )
        )
      (finally
        (.stop authority 0)
        (.delete db)))
    ))
