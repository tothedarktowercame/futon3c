(ns futon3c.evidence.spool-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.evidence.backend :as backend]
            [futon3c.evidence.futon1b-backend :as sut]
            [futon3c.evidence.spool :as spool]
            [org.httpkit.server :as server])
  (:import [java.net ServerSocket]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def ^:dynamic *path* nil)
(use-fixtures :each
  (fn [f]
    (let [dir (Files/createTempDirectory "evidence-spool-test" (make-array FileAttribute 0))
          path (str dir "/spool.db")]
      (binding [*path* path spool/*db-path* path]
        (with-redefs [sut/append-retry-ms 45 sut/append-retry-max-backoff-ms 20]
          (try (f) (finally (spool/stop! path))))))))

(defn- port [] (with-open [s (ServerSocket. 0)] (.getLocalPort s)))
(defn- entry [id & [parent]]
  (cond-> {:evidence/id id :evidence/type :coordination :evidence/claim-type :step
           :evidence/author "invented" :evidence/at "2026-11-18T00:00:00Z"
           :evidence/body {:fixture true} :evidence/tags []}
    parent (assoc :evidence/in-reply-to parent)))
(defn- backend-at [p] (sut/make-futon1b-backend (str "http://127.0.0.1:" p)))
(defn- until [pred]
  (loop [n 200]
    (cond (pred) true (zero? n) false
          :else (do (Thread/sleep 10) (recur (dec n))))))
(defn- stub! [p seen requests reject]
  (server/run-server
   (fn [req]
     (let [raw (slurp (:body req)) e (edn/read-string raw)
           id (:evidence/id e) parent (:evidence/in-reply-to e)
           _ (swap! requests conj {:id id :body raw})
           [status body] (cond
                           (contains? reject id) [400 {:error :invalid}]
                           (and parent (not (contains? @seen parent))) [409 {:error :reply-not-found}]
                           (contains? @seen id) [409 {:error :duplicate-id}]
                           :else (do (swap! seen conj id) [201 {:entry e}]))]
       {:status status :headers {"content-type" "application/edn"} :body (pr-str body)}))
   {:ip "127.0.0.1" :port p}))

(deftest refused-parent-and-reply-drain-fifo
  (let [p (port) b (backend-at p) seen (atom #{}) requests (atom [])
        parent (entry "P") reply (entry "R" "P")]
    (doseq [e [parent reply]]
      (let [r (backend/-append b e)]
        (is (= :store-unreachable (:error/code r))) (is (:spooled r))))
    (is (= 2 (:pending (spool/spool-status))))
    (let [stop (stub! p seen requests #{})]
      (try
        (is (until #(zero? (:pending (spool/spool-status)))))
        (is (= ["P" "R"] (mapv :id @requests)))
        (is (= #{"P" "R"} @seen))
        (is (= [(pr-str parent) (pr-str reply)] (mapv :body @requests)))
        (is (zero? (:dead-letter (spool/spool-status))))
        (finally (stop))))))

(deftest restart-recovers-write-ahead
  (let [p (port) b (backend-at p) seen (atom #{}) requests (atom [])]
    (is (:spooled (backend/-append b (entry "crash"))))
    (spool/stop! *path*)
    (let [stop (stub! p seen requests #{})]
      (try
        (backend-at p)
        (is (until #(contains? @seen "crash")))
        (is (until #(zero? (:pending (spool/spool-status)))))
        (finally (stop))))))

(deftest empty-queue-fast-path-needs-no-replay
  (let [p (port) seen (atom #{}) requests (atom []) stop (stub! p seen requests #{})
        deliveries (atom []) original @#'sut/send-spooled!]
    (try
      (with-redefs-fn {#'sut/send-spooled! (fn [row replay?]
                                           (swap! deliveries conj replay?)
                                           (original row replay?))}
        #(let [r (backend/-append (backend-at p) (entry "fast"))]
           (is (:ok r)) (is (not (:spooled r)))
           (is (= [false] @deliveries))
           (is (= 1 (count @requests)))
           (is (zero? (:pending (spool/spool-status))))))
      (finally (stop)))))

(deftest permanent-replay-rejection-does-not-block-next-row
  (let [p (port) b (backend-at p) seen (atom #{}) requests (atom [])]
    (doseq [id ["bad" "good"]] (is (:spooled (backend/-append b (entry id)))))
    (let [stop (stub! p seen requests #{"bad"})]
      (try
        (is (until #(zero? (:pending (spool/spool-status)))))
        (is (= 1 (:dead-letter (spool/spool-status))))
        (is (= #{"good"} @seen))
        (is (= ["bad" "good"] (mapv :id @requests)))
        (finally (stop))))))

(deftest own-id-409-on-replay-settles-the-row
  (let [p (port) b (backend-at p) seen (atom #{"own"}) requests (atom [])]
    (is (:spooled (backend/-append b (entry "own"))))
    (let [stop (stub! p seen requests #{})]
      (try
        (is (until #(zero? (:pending (spool/spool-status)))))
        (is (= ["own"] (mapv :id @requests)))
        (is (zero? (:dead-letter (spool/spool-status))))
        (finally (stop))))))

(deftest busy-response-keeps-wal-for-later-delivery
  (let [p (port) busy (atom true) seen (atom [])
        stop (server/run-server
              (fn [req]
                (let [e (edn/read-string (slurp (:body req)))]
                  (if @busy
                    {:status 504 :body "{}"}
                    (do (swap! seen conj (:evidence/id e))
                        {:status 201 :body (pr-str {:entry e})}))))
              {:ip "127.0.0.1" :port p})]
    (try
      (let [r (backend/-append (backend-at p) (entry "busy"))]
        (is (= :store-unavailable (:error/code r)))
        (is (:spooled r))
        (is (= 1 (:pending (spool/spool-status))))
        (reset! busy false)
        (is (until #(zero? (:pending (spool/spool-status)))))
        (is (= ["busy"] @seen))
        (is (zero? (:dead-letter (spool/spool-status)))))
      (finally (stop)))))

(deftest unrelated-fast-writes-do-not-wait-for-inflight-parent
  (let [p (port) entered (promise) release (promise) requests (atom [])
        stop (server/run-server
              (fn [req]
                (let [e (edn/read-string (slurp (:body req))) id (:evidence/id e)]
                  (swap! requests conj id)
                  (when (= "slow" id) (deliver entered true) (deref release 2000 nil))
                  {:status 201 :body (pr-str {:entry e})}))
              {:ip "127.0.0.1" :port p})
        b (backend-at p)]
    (try
      (let [first-result (future (backend/-append b (entry "slow")))]
        (is (deref entered 1000 false))
        (is (:ok (backend/-append b (entry "unrelated"))))
        ;; A child is queued, rather than overtaking its in-flight parent.
        (is (:spooled (backend/-append b (entry "child" "slow"))))
        (is (= ["slow" "unrelated"] @requests))
        (deliver release true)
        (is (:ok (deref first-result 1000 nil)))
        (is (until #(zero? (:pending (spool/spool-status)))))
        (is (= ["slow" "unrelated" "child"] @requests)))
      (finally (deliver release true) (stop)))))
