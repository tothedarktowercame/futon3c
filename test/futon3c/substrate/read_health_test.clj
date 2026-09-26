(ns futon3c.substrate.read-health-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.substrate.read-health :as sut]
            [futon3c.substrate.client]
            [futon3c.evidence.futon1b-backend])
  (:import [java.util.concurrent CompletableFuture]))

(defn clock [elapsed]
  (let [ticks (atom [-1 0])]
    (fn [] (let [[n _] (swap! ticks (fn [[n _]] [(inc n) 0]))]
             (* n elapsed 1000000)))))

(deftest soft-warning-preserves-value-and-exception
  (doseq [elapsed [4999 5000 5600 30001]]
    (let [warnings (atom [])]
      (binding [sut/*nano-time* (clock elapsed)
                sut/*context* {:frame/id "f1"}
                sut/*record-warning!* #(swap! warnings conj %)]
        (is (= :checked (sut/observe! {:trace-id "read1"} (constantly :checked)))))
      (is (= (if (> elapsed 5000) 1 0) (count @warnings)))
      (when (seq @warnings)
        (is (= elapsed (:elapsed-ms (first @warnings))))
        (is (= :returned (:read/outcome (first @warnings)))))))
  (let [failure (ex-info "private body" {}) warnings (atom [])]
    (binding [sut/*nano-time* (clock 5600)
              sut/*record-warning!* #(swap! warnings conj %)]
      (is (identical? failure
                     (try (sut/observe! {} #(throw failure))
                          (catch Exception e e)))))
    (is (= :failed (:read/outcome (first @warnings))))
    (is (not (.contains (pr-str @warnings) "private body")))))

(deftest frame-context-selects-hard-limit-without-changing-other-clients
  (is (= 123 (sut/timeout-ms 123)))
  (binding [sut/*context* {:frame/id "f1"}]
    (is (= 30000 (sut/timeout-ms 5000)))
    (is (= 30000 @(future (sut/timeout-ms 5000))))))

(deftest both-real-client-boundaries-record-slow-returned-reads
  (doseq [[namespace get-name http-namespace response expected]
          [['futon3c.substrate.client 'get-edn! 'babashka.http-client
            {:status 200 :body "{:checked true}"} {:checked true}]
           ['futon3c.evidence.futon1b-backend 'get-edn 'org.httpkit.client
            {:status 200 :body "{:checked true}"}
            {:status 200 :body {:checked true}}]]]
    (testing (str namespace)
      (let [warnings (atom []) options (atom nil)]
        (with-redefs-fn
          {(ns-resolve http-namespace 'get)
           (fn [_ opts]
             (reset! options opts)
             (if (= http-namespace 'babashka.http-client)
               (CompletableFuture/completedFuture response)
               (delay response)))}
          #(binding [sut/*context* {:frame/id "f1" :problem/id "p1"}
                     sut/*nano-time* (clock 5600)
                     sut/*record-warning!* (fn [w] (swap! warnings conj w))]
             (is (= expected ((ns-resolve namespace get-name) "http://test/read" 5000)))))
        (is (= 30000 (:timeout @options)))
        (is (= 1 (count @warnings)))
        (is (= "f1" (:frame/id (first @warnings))))
        (is (= :returned (:read/outcome (first @warnings))))
        (is (string? (:trace-id (first @warnings))))))))

(deftest hard-http-deadline-still-cancels-in-a-frame
  (let [pending (CompletableFuture.)]
    (with-redefs-fn
      {(ns-resolve 'babashka.http-client 'get) (fn [_ _] pending)
       #'sut/hard-limit-ms 10}
      #(binding [sut/*context* {:frame/id "f1"}]
         (is (thrown? clojure.lang.ExceptionInfo
                      ((ns-resolve 'futon3c.substrate.client 'get-edn!)
                       "http://test/stalled" 5000)))
         (is (.isCancelled pending))))))

(deftest structured-projection-read-preserves-warning-and-hard-bound
  (doseq [status [200 503]]
    (let [warnings (atom []) options (atom nil)]
      (with-redefs-fn
        {(ns-resolve 'babashka.http-client 'post)
         (fn [_ opts]
           (reset! options opts)
           (CompletableFuture/completedFuture
            {:status status :body "{:observed true}"}))}
        #(binding [sut/*context* {:frame/id "f1" :problem/id "p1"}
                   sut/*nano-time* (clock 5600)
                   sut/*record-warning!* (fn [w] (swap! warnings conj w))]
           (if (= 200 status)
             (is (= {:observed true}
                    (futon3c.substrate.client/memory-projection ["memory"])))
             (is (thrown? clojure.lang.ExceptionInfo
                          (futon3c.substrate.client/memory-projection ["memory"]))))))
      (is (= 30000 (:timeout @options)))
      (is (:async @options))
      (is (= 1 (count @warnings)))
      (is (= 5600 (:elapsed-ms (first @warnings))))
      (is (= (if (= 200 status) :returned :failed)
             (:read/outcome (first @warnings))))
      (is (= (get-in @options [:headers "X-Trace-Id"])
             (:trace-id (first @warnings)))))))
