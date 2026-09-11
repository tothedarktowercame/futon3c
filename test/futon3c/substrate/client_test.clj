(ns futon3c.substrate.client-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.substrate.client :as sut])
  (:import [java.util.concurrent CompletableFuture]))

(deftest substrate-get-has-a-wall-clock-body-timeout
  (let [get-edn-var (ns-resolve 'futon3c.substrate.client 'get-edn!)
        http-get-var (ns-resolve 'babashka.http-client 'get)
        pending (CompletableFuture.)
        trace (atom nil)]
    (with-redefs-fn
      {http-get-var (fn [_url options]
                      (is (:async options))
                      (reset! trace (get-in options [:headers "X-Trace-Id"]))
                      pending)}
      #(let [error (try (get-edn-var "http://substrate.test/stalled" 10)
                        nil
                        (catch clojure.lang.ExceptionInfo error error))]
         (is (= "authoritative substrate read timed out"
                (some-> error .getMessage)))
         (is (= 10 (:timeout-ms (ex-data error))))
         (is (= @trace (:trace-id (ex-data error))))
         (is (str/starts-with? @trace "substrate-read:"))
         (is (<= 0 (:elapsed-ms (ex-data error))))
         (is (.isCancelled pending))))))

(deftest hyperedge-read-follows-server-cursor
  (let [urls (atom [])
        get-edn-var (ns-resolve 'futon3c.substrate.client 'get-edn!)]
    (with-redefs-fn
      {#'sut/configured-url (constantly "http://substrate.test")
       get-edn-var
       (fn [url _timeout-ms]
         (swap! urls conj url)
         (if (str/includes? url "after=edge-1")
           {:hyperedges [{:hx/id "edge-2"}]}
           {:hyperedges [{:hx/id "edge-1"}]
            :next-cursor "edge-1"}))}
      #(let [rows (sut/hyperedges-by-type :test/edge {:limit 10})]
         (is (= ["edge-1" "edge-2"] (mapv :hx/id rows)))
         (is (= 2 (count @urls)))
         (is (every? (fn [url] (re-find #"limit=(?:10|9)" url)) @urls))
         (is (false? (sut/partial-result? rows)))))))

(deftest hyperedge-budget-exhaustion-is-marked-partial
  (let [get-edn-var (ns-resolve 'futon3c.substrate.client 'get-edn!)]
    (with-redefs-fn
      {#'sut/configured-url (constantly "http://substrate.test")
       get-edn-var
       (fn [_url _timeout-ms]
         {:hyperedges [{:hx/id "edge-1"}]
          :next-cursor "edge-1"})}
      #(let [rows (sut/hyperedges-by-type
                   :test/edge {:limit 10 :request-budget 1})]
         (is (= ["edge-1"] (mapv :hx/id rows)))
         (is (sut/partial-result? rows))
         (is (= "edge-1" (:next-cursor (meta rows))))
         (is (= 1 (:request-budget (meta rows))))))))

(def projection-edge
  {:hx/id "attachment" :hx/type :memory/assert :hx/endpoints ["memory"]
   :hx/props {:state :current :attachment-status :reviewed}})

(def projection-response
  {:ok true :endpoints ["memory"]
   :temporal-basis {:mode :current :projection-revision 1 :projection-generation 1}
   :groups [{:endpoint "memory" :components [{:edge projection-edge}]}]})

(deftest memory-attachment-projection-is-fresh-and-fails-closed
  (let [response (atom projection-response) calls (atom [])]
    (with-redefs [sut/memory-projection
                  (fn [endpoints options]
                    (swap! calls conj [endpoints options]) @response)]
      (is (= [projection-edge] (sut/memory-assertions-by-end "memory")))
      ;; A subsequent projection revision must be consulted, including removal.
      (swap! response assoc :groups [{:endpoint "memory" :components []}])
      (is (= [] (sut/memory-assertions-by-end "memory")))
      (is (= 2 (count @calls)))
      (is (= [["memory"] {:limit 100}] (first @calls)))
      (doseq [bad [(assoc projection-response :ok false)
                   (assoc projection-response :endpoints ["other"])
                   (assoc-in projection-response [:temporal-basis :mode] :as-of)
                   (update projection-response :temporal-basis dissoc :projection-generation)
                   (assoc-in projection-response [:groups 0 :endpoint] "other")
                   (assoc-in projection-response [:groups 0 :components] nil)
                   (assoc-in projection-response [:groups 0 :components]
                             (vec (repeat 100 {:edge projection-edge})))
                   (assoc-in projection-response [:groups 0 :components 0 :edge :hx/endpoints]
                             ["other"])]]
        (reset! response bad)
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"invalid or incomplete"
                             (sut/memory-assertions-by-end "memory")))))))

(deftest projection-post-has-a-whole-response-deadline
  (let [pending (CompletableFuture.)]
    (with-redefs-fn
      {(ns-resolve 'babashka.http-client 'post)
       (fn [_ options] (is (:async options)) pending)}
      #(is (thrown-with-msg? clojure.lang.ExceptionInfo #"timed out"
                            (sut/memory-projection ["memory"] {:timeout-ms 10}))))
    (is (.isCancelled pending))))
