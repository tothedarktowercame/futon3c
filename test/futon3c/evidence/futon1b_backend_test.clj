(ns futon3c.evidence.futon1b-backend-test
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.evidence.backend :as backend]
            [futon3c.evidence.futon1b-backend :as sut]
            [org.httpkit.client :as http]))

(deftest post-filters-retain-the-bounded-server-window
  (testing "the server fills an exact bounded window and the client rechecks membership"
    (let [seen-url (atom nil)
          store (sut/make-futon1b-backend "http://store.test")]
      (with-redefs [http/get (fn [url _]
                               (reset! seen-url url)
                               (delay {:status 200
                                       :body "{:entries []}"}))]
        (is (= []
               (backend/-query store
                               {:query/tags [:open :pipeline-tracer]
                                :query/subject {:ref/type :thread :ref/id "t 1"}
                                :query/pattern-id :agent/pause
                                :query/limit 5})))
        (is (str/includes? @seen-url "tags=open%2Cpipeline-tracer"))
        (is (str/includes? @seen-url "subject-type=thread"))
        (is (str/includes? @seen-url "subject-id=t+1"))
        (is (str/includes? @seen-url "pattern-id=pause"))
        (is (str/includes? @seen-url "limit=5"))
        (is (str/includes? @seen-url "include-ephemeral=false"))))))

(deftest post-filtered-count-uses-projected-count-route
  (testing "counting tags never hydrates an unbounded evidence response"
    (let [seen-url (atom nil)
          store (sut/make-futon1b-backend "http://store.test")]
      (with-redefs [http/get (fn [url _]
                               (reset! seen-url url)
                               (delay {:status 200 :body "{:count 7}"}))]
        (is (= 7 (backend/-count store {:query/tags [:open]})))
        (is (str/includes? @seen-url "/api/alpha/evidence/count?"))
        (is (str/includes? @seen-url "tags=open"))))))

(deftest canonical-problem-query-includes-legacy-subject-types
  (let [seen-urls (atom [])
        store (sut/make-futon1b-backend "http://legacy-subject-store.test")]
    (with-redefs [http/get
                  (fn [url _]
                    (swap! seen-urls conj url)
                    (delay
                      {:status 200
                       :body (cond
                               (str/includes? url "subject-type=apm-problem")
                               "{:entries [{:evidence/id \"legacy\", :evidence/subject {:ref/type :apm-problem, :ref/id \"a01\"}, :evidence/at \"2026-08-01T00:00:00Z\"}]}"
                               :else "{:entries []}")}))]
      (is (= ["legacy"]
             (mapv :evidence/id
                   (backend/-query
                    store {:query/subject {:ref/type :problem :ref/id "a01"}}))))
      (is (= #{"problem" "apm-problem" "bpm-problem"}
             (set (map #(second (re-find #"subject-type=([^&]+)" %))
                       @seen-urls)))))))

(deftest repeated-bounded-query-is-cached-and-write-invalidates
  (let [gets (atom 0)
        posts (atom 0)
        store (sut/make-futon1b-backend "http://cache-store.test")
        params {:query/type :coordination :query/limit 100}
        entry {:evidence/id "e-cache-invalidation"
               :evidence/type :coordination
               :evidence/claim-type :step
               :evidence/author "test"
               :evidence/at "2026-08-02T00:00:00Z"
               :evidence/body {}
               :evidence/tags []}]
    (with-redefs [http/get (fn [_ _]
                             (swap! gets inc)
                             (delay {:status 200
                                     :body "{:entries [{:evidence/id \"cached\", :evidence/type :coordination, :evidence/at \"2026-08-01T00:00:00Z\"}]}"}))
                  http/post (fn [_ _]
                              (swap! posts inc)
                              (delay {:status 201 :body (pr-str {:entry entry})}))]
      (is (= ["cached"] (mapv :evidence/id (backend/-query store params))))
      (is (= ["cached"] (mapv :evidence/id (backend/-query store params))))
      (is (= 1 @gets) "identical reads within the TTL share one store scan")
      (is (:ok (backend/-append store entry)))
      (is (= 1 @posts))
      (is (= ["cached"] (mapv :evidence/id (backend/-query store params))))
      (is (= 2 @gets) "a successful write invalidates cached reads"))))

(deftest expired-query-cache-is-refreshed
  (let [gets (atom 0)
        store (sut/make-futon1b-backend "http://expiry-store.test")]
    (with-redefs [sut/query-cache-ttl-ms 1
                  http/get (fn [_ _]
                             (swap! gets inc)
                             (delay {:status 200 :body "{:entries []}"}))]
      (backend/-query store {:query/limit 10})
      (Thread/sleep 5)
      (backend/-query store {:query/limit 10})
      (is (= 2 @gets) "external writes become visible after bounded staleness"))))

(deftest unbounded-protocol-query-pages-through-server-cursors
  (testing "-all never asks the store JVM for an unbounded response"
    (let [seen-urls (atom [])
          store (sut/make-futon1b-backend "http://store.test")]
      (with-redefs [http/get
                    (fn [url _]
                      (swap! seen-urls conj url)
                      (delay
                        {:status 200
                         :body (if (str/includes? url "cursor-at=")
                                 "{:entries [{:evidence/id \"e1\", :evidence/at \"2026-01-01T00:00:00Z\"}]}"
                                 "{:entries [{:evidence/id \"e2\", :evidence/at \"2026-01-02T00:00:00Z\"}], :next-cursor {:at \"2026-01-02T00:00:00Z\", :id \"e2\"}}" )}))]
        (is (= ["e2" "e1"]
               (mapv :evidence/id (backend/-all store))))
        (is (= 2 (count @seen-urls)))
        (is (every? #(str/includes? % "limit=1000") @seen-urls))
        (is (str/includes? (second @seen-urls) "cursor-id=e2"))))))

(deftest incomplete-evidence-page-forces-continuation
  (let [seen-urls (atom [])
        store (sut/make-futon1b-backend "http://incomplete-store.test")]
    (with-redefs [http/get
                  (fn [url _]
                    (swap! seen-urls conj url)
                    (delay
                      {:status 200
                       :body (if (str/includes? url "cursor-id=e2")
                               "{:entries [{:evidence/id \"e1\", :evidence/at \"2026-01-01T00:00:00Z\"}]}"
                               "{:entries [], :count 0, :scanned 20000, :incomplete true, :next-cursor {:at \"2026-01-02T00:00:00Z\", :id \"e2\"}}") }))]
      (is (= ["e1"]
             (mapv :evidence/id (backend/-query store {:query/limit 10}))))
      (is (= 2 (count @seen-urls)))
      (is (str/includes? (second @seen-urls) "cursor-id=e2")))))

(deftest evidence-budget-exhaustion-is-marked-partial
  (let [store (sut/make-futon1b-backend "http://partial-store.test")]
    (with-redefs [http/get
                  (fn [_url _]
                    (delay {:status 200
                            :body "{:entries [], :incomplete true, :next-cursor {:at \"2026-01-02T00:00:00Z\", :id \"e2\"}}"}))]
      (let [entries (backend/-query
                     store {:query/limit 10 :query/request-budget 1})]
        (is (empty? entries))
        (is (sut/partial-result? entries))
        (is (= 1 (-> entries meta :partial-pages first :request-budget)))
        (is (= {:at "2026-01-02T00:00:00Z" :id "e2"}
               (-> entries meta :partial-pages first :next-cursor)))))))

(deftest append-classifies-timeout-separately-from-unreachable
  (let [entry {:evidence/id "e-timeout"
               :evidence/type :coordination
               :evidence/claim-type :step
               :evidence/author "test"
               :evidence/at "2026-07-22T00:00:00Z"
               :evidence/body {}
               :evidence/tags []}
        store (sut/make-futon1b-backend "http://store.test")]
    (with-redefs [sut/append-retry-ms 0
                  http/get (fn [_ _] (delay {:status 404 :body "{}"}))
                  http/post (fn [_ _]
                              (delay {:error (java.net.SocketTimeoutException.
                                               "read timed out")}))]
      (is (= :store-timeout
             (:error/code (backend/-append store entry)))))
    (with-redefs [http/get (fn [_ _] (delay {:status 404 :body "{}"}))
                  http/post (fn [_ _]
                              (delay {:error (java.net.ConnectException.
                                               "connection refused")}))]
      (is (= :store-unreachable
             (:error/code (backend/-append store entry)))))))

(deftest append-retries-connection-refusal-then-succeeds
  (let [attempts (atom 0)
        entry {:evidence/id "e-retry-success"
               :evidence/type :coordination
               :evidence/claim-type :step
               :evidence/author "test"
               :evidence/at "2026-08-19T00:00:00Z"
               :evidence/body {}
               :evidence/tags []}
        store (sut/make-futon1b-backend "http://store.test")]
    (with-redefs [sut/append-retry-ms 1000
                  http/post (fn [_ _]
                              (let [attempt (swap! attempts inc)]
                                (delay
                                  (if (<= attempt 2)
                                    {:error (java.net.ConnectException.
                                              "connection refused")}
                                    {:status 201
                                     :body (pr-str {:entry entry})}))))]
      (let [result (backend/-append store entry)]
        (is (:ok result))
        (is (= entry (:entry result)))
        (is (= 3 @attempts) "two refused connections plus one confirmed write")))))

(deftest append-timeout-is-never-retried
  (let [attempts (atom 0)
        entry {:evidence/id "e-no-retry-timeout"
               :evidence/type :coordination
               :evidence/claim-type :step
               :evidence/author "test"
               :evidence/at "2026-08-19T00:00:00Z"
               :evidence/body {}
               :evidence/tags []}
        store (sut/make-futon1b-backend "http://store.test")]
    (with-redefs [sut/append-retry-ms 1000
                  http/post (fn [_ _]
                              (swap! attempts inc)
                              (delay {:error (java.net.SocketTimeoutException.
                                               "read timed out")}))]
      (is (= :store-timeout (:error/code (backend/-append store entry))))
      (is (= 1 @attempts)))))

(deftest append-duplicate-id-is-never-retried
  (let [attempts (atom 0)
        entry {:evidence/id "e-no-retry-duplicate"
               :evidence/type :coordination
               :evidence/claim-type :step
               :evidence/author "test"
               :evidence/at "2026-08-19T00:00:00Z"
               :evidence/body {}
               :evidence/tags []}
        store (sut/make-futon1b-backend "http://store.test")]
    (with-redefs [sut/append-retry-ms 1000
                  http/post (fn [_ _]
                              (swap! attempts inc)
                              (delay {:status 409 :body "{:error :duplicate-id}"}))]
      (is (= :duplicate-id (:error/code (backend/-append store entry))))
      (is (= 1 @attempts)))))

(deftest append-unreachable-exhaustion-carries-receipt
  (let [attempts (atom 0)
        retry-window 120
        entry {:evidence/id "e-retry-exhausted"
               :evidence/type :coordination
               :evidence/claim-type :step
               :evidence/author "test"
               :evidence/at "2026-08-19T00:00:00Z"
               :evidence/body {}
               :evidence/tags []}
        store (sut/make-futon1b-backend "http://store.test")]
    (with-redefs [sut/append-retry-ms retry-window
                  http/post (fn [_ _]
                              (swap! attempts inc)
                              (delay {:error (java.net.ConnectException.
                                               "connection refused")}))]
      (let [result (backend/-append store entry)]
        (is (= :store-unreachable (:error/code result)))
        (is (= @attempts (get-in result [:error/context :attempts])))
        ;; 3, not 2: the loop now makes one final probe AT the deadline
        ;; instead of sleeping to the window edge and giving up unprobed.
        ;; The old shape could sleep straight through the store returning.
        (is (= 3 @attempts))
        (is (<= retry-window (get-in result [:error/context :elapsed-ms])))))))

(deftest append-retry-sleep-is-capped-at-window
  (let [attempts (atom 0)
        retry-window 25
        entry {:evidence/id "e-retry-window"
               :evidence/type :coordination
               :evidence/claim-type :step
               :evidence/author "test"
               :evidence/at "2026-08-19T00:00:00Z"
               :evidence/body {}
               :evidence/tags []}
        store (sut/make-futon1b-backend "http://store.test")
        started (System/nanoTime)]
    (with-redefs [sut/append-retry-ms retry-window
                  http/post (fn [_ _]
                              (swap! attempts inc)
                              (delay {:error (java.net.ConnectException.
                                               "connection refused")}))]
      (let [result (backend/-append store entry)
            wall-ms (quot (- (System/nanoTime) started) 1000000)]
        (is (= :store-unreachable (:error/code result)))
        ;; The invariant that matters is the wall-clock bound below, not
        ;; the attempt count. One probe AT the deadline is required: a
        ;; restart completing during the final sleep must still be caught.
        (is (= 2 @attempts) "one final probe at the deadline, then stop")
        (is (<= wall-ms (+ retry-window 100))
            "the retry loop stays within the configured window plus scheduler tolerance")))))

(deftest append-delegates-reference-validation-to-the-store
  (let [gets (atom 0)
        store (sut/make-futon1b-backend "http://store.test")
        entry {:evidence/id "e-child"
               :evidence/type :coordination
               :evidence/claim-type :step
               :evidence/author "test"
               :evidence/at "2026-07-22T00:00:00Z"
               :evidence/body {}
               :evidence/tags []
               :evidence/in-reply-to "missing-parent"}]
    (with-redefs [http/get (fn [& _] (swap! gets inc) (delay {:status 404 :body "{}"}))
                  http/post (fn [_ _]
                              (delay {:status 409
                                      :body "{:error :reply-not-found}"}))]
      (is (= :reply-not-found (:error/code (backend/-append store entry))))
      (is (zero? @gets) "the client does not add a preflight point read"))))

(deftest append-propagates-a-stable-trace-id
  (let [request (atom nil)
        store (sut/make-futon1b-backend "http://store.test")
        entry {:evidence/id "e-traced"
               :evidence/type :coordination
               :evidence/claim-type :step
               :evidence/author "producer-test"
               :evidence/at "2026-07-23T00:00:00Z"
               :evidence/body {:event "trace-test"}
               :evidence/tags []}]
    (with-redefs [http/post
                  (fn [url options]
                    (reset! request {:url url :options options})
                    (delay {:status 201 :body (pr-str {:entry entry})}))]
      (let [result (backend/-append store entry)]
        (is (:ok result))
        (is (= "evidence-append:e-traced" (:trace-id result)))
        (is (= "evidence-append:e-traced"
               (get-in @request [:options :headers "x-trace-id"])))
        (is (= entry
               (edn/read-string (get-in @request [:options :body]))))))))

(deftest malformed-wire-edn-is-rejected-before-http
  (let [posts (atom 0)
        store (sut/make-futon1b-backend "http://store.test")
        malformed (keyword ":")
        entry {:evidence/id "e-malformed"
               :evidence/type :coordination
               :evidence/claim-type :step
               :evidence/author "producer-test"
               :evidence/at "2026-07-23T00:00:00Z"
               :evidence/body {:event "malformed-test"
                               :producer-value malformed}
               :evidence/tags []}]
    (with-redefs [http/post (fn [& _]
                              (swap! posts inc)
                              (delay {:status 201 :body "{}"}))]
      (let [result (backend/-append store entry)]
        (is (= :store-serialization (:error/code result)))
        (is (= "evidence-append:e-malformed"
               (get-in result [:error/context :trace-id])))
        (is (= [{:path [:evidence/body :producer-value]
                 :value-type "clojure.lang.Keyword"
                 :token "::"}]
               (get-in result [:error/context :invalid-edn])))
        (is (zero? @posts)
            "an unreadable payload never opens a store connection")))))

(deftest append-retry-backoff-is-capped-so-the-store-keeps-being-probed
  (testing "uncapped 100*2^n leaves the store unprobed for tens of seconds at
            the end of the window; a restart finishing in that gap would still
            lose the write"
    (let [attempts (atom 0)
          entry {:evidence/id "e-retry-capped"
                 :evidence/type :coordination
                 :evidence/claim-type :step
                 :evidence/author "test"
                 :evidence/at "2026-08-19T00:00:00Z"
                 :evidence/body {}
                 :evidence/tags []}
          store (sut/make-futon1b-backend "http://store.test")]
      (with-redefs [sut/append-retry-ms 2000
                    sut/append-retry-max-backoff-ms 50
                    http/post (fn [_ _]
                                (swap! attempts inc)
                                (delay {:error (java.net.ConnectException.
                                                 "connection refused")}))]
        (backend/-append store entry)
        ;; uncapped doubling reaches 2s in ~5 attempts; capped at 50ms the
        ;; window admits far more, i.e. the store is actually being polled
        (is (> @attempts 10)
            "capped backoff must keep probing rather than sleeping in blocks")))))

(deftest append-duplicate-after-a-retry-is-our-own-write-landing
  (testing "evidence ids are client-minted, so a 409 on a RETRY means our
            earlier attempt succeeded; reporting failure would mask it"
    (let [attempts (atom 0)
          entry {:evidence/id "e-retry-recovered-duplicate"
                 :evidence/type :coordination
                 :evidence/claim-type :step
                 :evidence/author "test"
                 :evidence/at "2026-08-19T00:00:00Z"
                 :evidence/body {}
                 :evidence/tags []}
          store (sut/make-futon1b-backend "http://store.test")]
      (with-redefs [sut/append-retry-ms 2000
                    sut/append-retry-max-backoff-ms 10
                    http/post (fn [_ _]
                                (let [n (swap! attempts inc)]
                                  (delay
                                    (if (= 1 n)
                                      {:error (java.net.ConnectException. "connection refused")}
                                      {:status 409 :body "{:error :duplicate-id}"}))))]
        (let [result (backend/-append store entry)]
          (is (:ok result) "a duplicate after a retry is a success, not a failure")
          (is (:recovered-after-retry result))
          (is (= 2 @attempts)))))))

(deftest append-duplicate-on-the-FIRST-attempt-is-still-an-error
  (testing "only a duplicate seen after a retry is attributable to us"
    (let [entry {:evidence/id "e-first-attempt-duplicate"
                 :evidence/type :coordination
                 :evidence/claim-type :step
                 :evidence/author "test"
                 :evidence/at "2026-08-19T00:00:00Z"
                 :evidence/body {}
                 :evidence/tags []}
          store (sut/make-futon1b-backend "http://store.test")]
      (with-redefs [sut/append-retry-ms 2000
                    http/post (fn [_ _]
                                (delay {:status 409 :body "{:error :duplicate-id}"}))]
        (is (= :duplicate-id (:error/code (backend/-append store entry))))))))
