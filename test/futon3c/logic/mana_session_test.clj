(ns futon3c.logic.mana-session-test
  "Tests for the read-side binding to nonstarter mana
   (`futon3c.logic.mana-session`). Uses with-redefs to stub the
   underlying HTTP layer so tests are hermetic and don't require
   nonstarter to be running.

   Mission: M-bounded-in-flight-state INSTANTIATE Block 5."
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.logic.mana-session :as mana]
            [babashka.http-client :as http]))

(defn- stub-resp
  "Build a fake babashka.http-client response."
  ([status] (stub-resp status nil))
  ([status body] {:status status :body body}))

(deftest mana-summary-happy-path
  (testing "Happy path: nonstarter returns 200 with valid JSON; we get a parsed map."
    (with-redefs [http/get (fn [url _opts]
                             (is (= "http://localhost:7072/api/mana" url))
                             (stub-resp 200
                                        "{\"session-id\":\"sess-A\",\"earned\":12.5,\"spent\":3.0,\"balance\":9.5}"))]
      (let [r (mana/mana-summary "sess-A")]
        (is (= "sess-A" (:session-id r)))
        (is (= 12.5 (:earned r)))
        (is (= 3.0 (:spent r)))
        (is (= 9.5 (:balance r)))))))

(deftest mana-summary-nil-session-id
  (testing "nil or blank session-id short-circuits to nil without an HTTP call."
    (let [calls (atom 0)]
      (with-redefs [http/get (fn [_ _] (swap! calls inc) (stub-resp 200 "{}"))]
        (is (nil? (mana/mana-summary nil)))
        (is (nil? (mana/mana-summary "")))
        (is (nil? (mana/mana-summary "   ")))
        (is (zero? @calls) "no HTTP calls made for blank session-ids")))))

(deftest mana-summary-non-200-returns-nil
  (testing "Non-200 response returns nil (not throw)."
    (with-redefs [http/get (fn [_ _] (stub-resp 404 "{\"error\":\"not found\"}"))]
      (is (nil? (mana/mana-summary "sess-A"))))
    (with-redefs [http/get (fn [_ _] (stub-resp 500 "boom"))]
      (is (nil? (mana/mana-summary "sess-A"))))))

(deftest mana-summary-malformed-json-returns-nil
  (testing "200 with malformed body returns nil (not throw)."
    (with-redefs [http/get (fn [_ _] (stub-resp 200 "{not json"))]
      (is (nil? (mana/mana-summary "sess-A"))))))

(deftest mana-summary-network-exception-returns-nil
  (testing "HTTP throw → caught → nil. Robust to nonstarter being down."
    (with-redefs [http/get (fn [_ _]
                             (throw (java.net.ConnectException. "refused")))]
      (is (nil? (mana/mana-summary "sess-A"))))))

(deftest pool-stats-happy-path
  (testing "GET /api/pool returns parsed stats."
    (with-redefs [http/get (fn [url _opts]
                             (is (= "http://localhost:7072/api/pool" url))
                             (stub-resp 200
                                        "{\"balance\":42.0,\"total-donated\":100.0,\"total-funded\":58.0}"))]
      (let [r (mana/pool-stats)]
        (is (= 42.0 (:balance r)))
        (is (= 100.0 (:total-donated r)))
        (is (= 58.0 (:total-funded r)))))))

(deftest pool-stats-down
  (testing "Pool unavailable → nil."
    (with-redefs [http/get (fn [_ _]
                             (throw (java.net.ConnectException. "refused")))]
      (is (nil? (mana/pool-stats))))))

(deftest reachable-mirrors-pool
  (testing "reachable? is true iff pool-stats returns non-nil."
    (with-redefs [http/get (fn [_ _] (stub-resp 200 "{\"balance\":0}"))]
      (is (true? (mana/reachable?))))
    (with-redefs [http/get (fn [_ _] (stub-resp 503 ""))]
      (is (false? (mana/reachable?))))))

(deftest balance-for-session-extracts
  (testing "balance-for-session returns just the :balance number."
    (with-redefs [http/get (fn [_ _]
                             (stub-resp 200
                                        "{\"session-id\":\"S\",\"earned\":5,\"spent\":2,\"balance\":3}"))]
      (is (= 3 (mana/balance-for-session "S"))))
    (with-redefs [http/get (fn [_ _] (stub-resp 404 ""))]
      (is (nil? (mana/balance-for-session "S"))))))

(deftest base-url-override
  (testing "Caller can override the base-url (for non-default nonstarter
            instances; useful for federation later)."
    (let [observed (atom nil)]
      (with-redefs [http/get (fn [url _opts]
                               (reset! observed url)
                               (stub-resp 200 "{\"balance\":1}"))]
        (mana/mana-summary "S" {:base-url "http://other-host:9999"})
        (is (= "http://other-host:9999/api/mana" @observed))))))
