(ns futon3c.arxana.formal-c3a-test
  (:require [clojure.test :refer [deftest is]]
            [futon1b-arxana :as f1]
            [futon1b-gates :as gates]
            [futon1b-server :as f1-server]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.service :as service]
            [futon3c.arxana.store-http :as store-http]
            [xtdb.node :as xtn])
  (:import [java.io File]))

(defn envelope [mid op body]
  {:protocol/version "arxana.v1" :message/id mid :operation op
   :principal/id "c3a" :body body})
(defn occurrence [n endpoint]
  (records/make-clusion
   {:type-revision [:pattern/occurrence 1]
    :endpoints [{:role :pattern :kind :external-ref :target {:ref/id endpoint}}
                {:role :turn :kind :external-ref :target {:ref/id (str "turn-" n)}}]
    :asserted-content {:window [] :proposal "p"} :event-time (format "%08d" n)}))
(defn occurrence-in-turn [n pattern turn]
  (records/make-clusion
   {:type-revision [:pattern/occurrence 1]
    :endpoints [{:role :pattern :kind :external-ref :target {:ref/id pattern}}
                {:role :turn :kind :external-ref :target {:ref/id turn}}]
    :asserted-content {:window [] :proposal "p"} :event-time (format "%08d" n)}))
(defn role-reverse [cfg mid role endpoint limit]
  (service/dispatch cfg (envelope mid :graph/query
                                  {:query/type :role-reverse
                                   :edge/type [:pattern/occurrence 1]
                                   :endpoint/role role :endpoint/id endpoint
                                   :time/order :desc :query/limit limit})))

(deftest bounded-watermarked-graph-query-over-http
  (gates/seed-mission-contract!)
  (let [db (File/createTempFile "formal-c3a-" ".db") _ (.delete db)]
    (try
      (with-open [node (xtn/start-node)]
        (let [server (f1-server/start-server! {:node node :port 0 :bind-host "127.0.0.1"
                                               :arxana-index-path (.getPath db)})]
          (try
            (let [cfg {:graph-store (store-http/http-store
                                     (str "http://127.0.0.1:" (.getPort (.getAddress server)))
                                     f1/service-principal)}]
              (doseq [n (range 55)]
                (is (:ok (service/dispatch cfg (envelope (str "c-" n) :clusion/assert
                                                          {:record (occurrence n "hot")})))))
              (let [r (service/dispatch cfg (envelope "q1" :graph/query
                                                      {:query/type :role-reverse
                                                       :edge/type [:pattern/occurrence 1]
                                                       :endpoint/role :pattern
                                                       :endpoint/id "hot"
                                                       :time/order :desc :query/limit 50}))]
                ;; added in review (claude-13): 55 matches under LIMIT 50 is a
                ;; truncated answer from the real store, with its total
                (is (= :truncated (get-in r [:ok :result/status])))
                (is (= :limit (get-in r [:ok :result/truncation])))
                (is (= 55 (get-in r [:ok :result/total])))
                (is (= 50 (count (get-in r [:ok :result/edge-ids])))))
              ;; added in review (claude-13): the index key includes the role.
              ;; "dual" is a turn here, never a pattern.
              (is (:ok (service/dispatch cfg (envelope "c-dual" :clusion/assert
                                                        {:record (occurrence-in-turn 60 "other" "dual")}))))
              (let [as-pattern (role-reverse cfg "q-dual-p" :pattern "dual" 50)
                    as-turn (role-reverse cfg "q-dual-t" :turn "dual" 50)]
                (is (= [] (get-in as-pattern [:ok :result/edge-ids])))
                (is (= :complete (get-in as-pattern [:ok :result/status])))
                (is (= 1 (count (get-in as-turn [:ok :result/edge-ids]))))
                (is (= :complete (get-in as-turn [:ok :result/status]))))
              ;; Bypass the server hook deliberately: authoritative rows move,
              ;; sidecar watermark does not. The read must say stale.
              (f1/conditional-command! node
                                       {:protocol/version "arxana.v1" :message/id "behind"
                                        :operation :clusion/assert
                                        :body {:record (occurrence 99 "hot")}})
              (let [r (service/dispatch cfg (envelope "q2" :graph/query
                                                      {:query/type :role-reverse
                                                       :edge/type [:pattern/occurrence 1]
                                                       :endpoint/role :pattern
                                                       :endpoint/id "hot" :query/limit 50}))]
                (is (= :stale (get-in r [:error :error/category])))
                (is (= :arxana.graph/index-behind (get-in r [:error :error/code])))))
            (finally (f1-server/stop-server! server)))))
      (finally (.delete db)))))
