(ns futon3c.test-registry.sqlite-backend-test
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.test :refer [deftest is testing]]
            [futon3c.evidence.backend :as evidence]
            [futon3c.test-registry :as registry]
            [futon3c.test-registry.sqlite-backend :as sqlite])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.sql DriverManager]
           [java.time Instant]))

(defn- temp-dir []
  (.toFile (Files/createTempDirectory "registry-sqlite-" (make-array FileAttribute 0))))

(defn- cleanup! [dir]
  (doseq [f (reverse (file-seq dir))] (io/delete-file f true)))

(defn- entry
  ([id at] (entry id at {}))
  ([id at extra]
   (merge {:evidence/id id
           :evidence/subject {:ref/type :session :ref/id "sqlite-test"}
           :evidence/type :coordination
           :evidence/claim-type :observation
           :evidence/author "sqlite-test"
           :evidence/at at
           :evidence/tags [:test-registry]
           :evidence/body {:payload-edn "{:kind :fixture}\n" :sha256 "fixture"}}
          extra)))

(defn- run-entry [id namespace command ran finished warrant?]
  (let [payload (cond-> {:schema "test-registry/v1" :kind :run :author "sqlite-test"
                         :run/id id :repo/root "/fixture" :command command
                         :ran-at ran :finished-at finished :warrant? warrant?
                         :git-head id}
                  namespace (assoc :namespace namespace))]
    (entry id finished {:evidence/body {:payload-edn (pr-str payload) :sha256 id}})))

(defn- median-ms [f]
  (dotimes [_ 10] (f))
  (let [samples (sort (repeatedly 100
                        #(let [start (System/nanoTime)] (f)
                           (/ (double (- (System/nanoTime) start)) 1000000.0))))]
    (nth samples 50)))

(deftest protocol-roundtrip-query-and-append-only-semantics
  (let [dir (temp-dir) db (io/file dir "registry.sqlite") b (sqlite/sqlite-backend db)
        text "{:message \"象 snowman ☃\"}\n"
        first-entry (entry "entry-1" "2026-09-27T00:00:00Z"
                           {:evidence/tags [:test-registry :alpha]
                            :evidence/body {:payload-edn text :sha256 "one"}})]
    (try
      (testing "exact envelope and payload bytes survive a round trip"
        (is (:ok (evidence/-append b first-entry)))
        (is (= first-entry (evidence/-get b "entry-1")))
        (is (= text (get-in (evidence/-get b "entry-1") [:evidence/body :payload-edn]))))
      (testing "duplicate and missing-parent checks preserve the first row"
        (is (= :duplicate-id (:error/code (evidence/-append b (assoc first-entry :evidence/author "changed")))))
        (is (= first-entry (evidence/-get b "entry-1")))
        (is (= :reply-not-found
               (:error/code (evidence/-append b
                              (entry "orphan" "2026-09-27T00:00:01Z"
                                     {:evidence/in-reply-to "missing"}))))))
      (testing "fork checks and append-only deletion"
        (is (= :fork-not-found
               (:error/code (evidence/-append b
                              (entry "bad-fork" "2026-09-27T00:00:01Z"
                                     {:evidence/fork-of "missing"})))))
        (is (= :append-only (:error/code (evidence/-delete! b ["entry-1"]))))
        (is (= first-entry (evidence/-get b "entry-1"))))
      (testing "ALL-of tags, newest-first and limit"
        (doseq [e [(entry "both-old" "2026-09-27T00:00:02Z" {:evidence/tags [:alpha :beta]})
                   (entry "alpha-only" "2026-09-27T00:00:03Z" {:evidence/tags [:alpha]})
                   (entry "both-new" "2026-09-27T00:00:04Z" {:evidence/tags [:alpha :beta]})]]
          (is (:ok (evidence/-append b e))))
        (is (= ["both-new"]
               (mapv :evidence/id (evidence/-query b {:query/tags [:alpha :beta]
                                                       :query/limit 1}))))
        (is (= ["both-new" "alpha-only"]
               (mapv :evidence/id (evidence/-query b {:query/tags [:alpha]
                                                       :query/since "2026-09-27T00:00:03Z"
                                                       :query/before "2026-09-27T00:00:05Z"})))))
      (finally (cleanup! dir)))))

(deftest registry-chain-and-latest-run-semantics
  (let [dir (temp-dir) b (sqlite/sqlite-backend (io/file dir "registry.sqlite"))]
    (try
      (testing "registry canonical chain verifies through the non-HTTP backend"
        (let [intent (registry/append-record! b {:kind :intent :author "sqlite-test" :run/id "chain"} nil)
              result (registry/append-record! b {:kind :run :author "sqlite-test" :run/id "chain"}
                                              (:evidence/id intent))
              review (registry/append-record! b {:kind :review :author "sqlite-test" :run/id "chain"}
                                              (:evidence/id result))]
          (is (= [:intent :run :review]
                 (mapv #(get-in % [:payload :kind])
                       (registry/read-chain! b (:evidence/id review)))))))
      (testing "a later failed run supersedes an earlier pass"
        (doseq [e [(run-entry "pass" "demo-test" ["clojure" "-n" "demo-test"]
                              "2026-09-27T01:00:00Z" "2026-09-27T01:00:01Z" true)
                   (run-entry "fail" "demo-test" ["clojure" "-n" "demo-test"]
                              "2026-09-27T02:00:00Z" "2026-09-27T02:00:01Z" false)]]
          (is (:ok (evidence/-append b e))))
        (is (= "fail" (:evidence/id (sqlite/latest-run-for-namespace b "demo-test")))))
      (testing "command-only runs are indexed by their exact command"
        (let [command ["lake" "build" "DarkTower.Module"]]
          (is (:ok (evidence/-append b
                    (run-entry "command-only" nil command
                               "2026-09-27T03:00:00Z" "2026-09-27T03:00:01Z" true))))
          (is (= "command-only" (:evidence/id (sqlite/latest-run-for-command b command))))))
      (finally (cleanup! dir)))))

(deftest storage-failure-is-not-absence
  (let [dir (temp-dir) garbage (io/file dir "garbage.sqlite")]
    (try
      (spit garbage "not a sqlite database")
      (is (thrown? java.sql.SQLException (sqlite/sqlite-backend dir)))
      (is (thrown? java.sql.SQLException (sqlite/sqlite-backend garbage)))
      (finally (cleanup! dir)))))

(defn -main [db prefix n]
  (let [b (sqlite/sqlite-backend db)
        base (.getEpochSecond (Instant/parse "2026-09-27T04:00:00Z"))]
    (when-not (every? :ok
                      (sqlite/append-batch!
                       b (mapv (fn [i]
                                 (entry (str prefix i) (str (Instant/ofEpochSecond (+ base i)))))
                               (range (parse-long n)))))
      (System/exit 1))))

(deftest two-process-contention-retains-all-entries
  (let [dir (temp-dir) db (str (io/file dir "registry.sqlite"))
        _ (sqlite/sqlite-backend db)
        command (fn [prefix]
                  ["clojure" "-Sdeps" "{:paths [\"src\" \"test\" \"resources\" \"library\"]}"
                   "-M" "-m"
                   "futon3c.test-registry.sqlite-backend-test" db prefix "50"])
        start (fn [prefix] (.start (ProcessBuilder. ^java.util.List (command prefix))))
        finish (fn [process]
                 (let [exit (.waitFor process)]
                   {:exit exit :out (slurp (.getInputStream process))
                    :err (slurp (.getErrorStream process))}))]
    (try
      (let [a (start "process-a-") b (start "process-b-")
            a-result (finish a) b-result (finish b)]
        (is (zero? (:exit a-result)) (pr-str a-result))
        (is (zero? (:exit b-result)) (pr-str b-result))
        (is (= 100 (evidence/-count (sqlite/sqlite-backend db) {}))))
      (finally (cleanup! dir)))))

(deftest existing-index-tables-coexist
  (let [dir (temp-dir) db (str (io/file dir "registry.sqlite"))
        ledger (io/file dir "empty-ledger.edn")]
    (try
      (spit ledger "")
      (let [{:keys [exit err]} (shell/sh "python3" "scripts/warrant_index.py"
                                         "--db" db "--ledger" (str ledger)
                                         "check" "--prefix" "no-such-namespace")]
        (is (zero? exit) err))
      (let [b (sqlite/sqlite-backend db)]
        (is (:ok (evidence/-append b (entry "coexist" "2026-09-27T05:00:00Z")))))
      (let [{:keys [exit err]} (shell/sh "python3" "scripts/warrant_index.py"
                                         "--db" db "--ledger" (str ledger)
                                         "check" "--prefix" "no-such-namespace")]
        (is (zero? exit) err))
      (with-open [c (DriverManager/getConnection (str "jdbc:sqlite:" db))
                  s (.createStatement c)]
        (is (some? (with-open [r (.executeQuery s "SELECT count(*) FROM warrants")]
                     (when (.next r) (.getLong r 1)))))
        (is (some? (with-open [r (.executeQuery s "SELECT count(*) FROM files")]
                     (when (.next r) (.getLong r 1))))))
      (finally (cleanup! dir)))))

(deftest rerun-request-lifecycle
  (let [dir (temp-dir) b (sqlite/sqlite-backend (io/file dir "registry.sqlite"))]
    (try
      (testing "only stale and absent lookups license a request"
        (let [absent (sqlite/request-rerun! b {:namespace "absent-test"
                                               :repo "futon3c" :reason :absent})
              stale (sqlite/request-rerun! b {:namespace "stale-test"
                                              :repo "futon2" :reason :stale})]
          (is (true? (:created? absent)))
          (is (= :queued (:state absent)))
          (is (true? (:created? stale)))
          (is (= :invalid-rerun-request
                 (:error/code (sqlite/request-rerun!
                               b {:namespace "failed-test" :repo "futon2"
                                  :reason :not-passing}))))))
      (testing "queued and running requests deduplicate by namespace"
        (let [first-request (sqlite/request-rerun!
                             b {:namespace "same-test" :repo "futon3c" :reason :absent})
              queued-again (sqlite/request-rerun!
                            b {:namespace "same-test" :repo "futon3c" :reason :stale})]
          (is (= (:request-id first-request) (:request-id queued-again)))
          (is (false? (:created? queued-again)))
          (let [claimed (sqlite/claim-reruns! b 3)
                running (first (filter #(= "same-test" (:namespace %)) claimed))
                running-again (sqlite/request-rerun!
                               b {:namespace "same-test" :repo "futon3c" :reason :stale})]
            (is (= :running (:state running)))
            (is (= (:request-id first-request) (:request-id running-again)))
            (is (false? (:created? running-again)))
            (is (= 1 (count (sqlite/rerun-requests b {:namespace "same-test"})))))))
      (testing "only running rows finish, and history remains append-only"
        (let [queued (sqlite/request-rerun!
                      b {:namespace "finish-test" :repo "futon3c" :reason :absent})]
          (is (= :rerun-not-running
                 (:error/code (sqlite/finish-rerun!
                               b (:request-id queued)
                               {:state :done :entry-id "test-registry-pass"}))))
          (let [running (first (sqlite/claim-reruns! b 1))]
            (is (= (:request-id queued) (:request-id running)))
            (is (= :done (:state (sqlite/finish-rerun!
                                  b (:request-id running)
                                  {:state :done :entry-id "test-registry-pass"}))))
            (let [next-request (sqlite/request-rerun!
                                b {:namespace "finish-test" :repo "futon3c"
                                   :reason :stale})
                  history (sqlite/rerun-requests b {:namespace "finish-test"})]
              (is (true? (:created? next-request)))
              (is (not= (:request-id queued) (:request-id next-request)))
              (is (= [:done :queued] (mapv :state history))))))
        (let [queued (first (sqlite/rerun-requests b {:state :queued}))
              running (first (sqlite/claim-reruns! b 1))
              failed (sqlite/finish-rerun!
                      b (:request-id running)
                      {:state :failed :detail "test command exited 1"})]
          (is (= (:request-id queued) (:request-id running)))
          (is (= :failed (:state failed)))
          (is (= "test command exited 1" (:detail failed)))))
      (finally (cleanup! dir)))))

(deftest rerun-claims-are-oldest-first
  (let [dir (temp-dir) b (sqlite/sqlite-backend (io/file dir "registry.sqlite"))]
    (try
      (doseq [namespace ["first-test" "second-test" "third-test"]]
        (sqlite/request-rerun! b {:namespace namespace :repo "futon3c" :reason :absent}))
      (is (= ["first-test" "second-test"]
             (mapv :namespace (sqlite/claim-reruns! b 2))))
      (is (= ["third-test"]
             (mapv :namespace (sqlite/rerun-requests b {:state :queued}))))
      (finally (cleanup! dir)))))

(deftest concurrent-rerun-requests-have-one-identity
  (let [dir (temp-dir) db (io/file dir "registry.sqlite")
        b (sqlite/sqlite-backend db)
        ready (promise)
        calls (mapv (fn [_]
                      (future
                        @ready
                        (sqlite/request-rerun!
                         b {:namespace "concurrent-test" :repo "futon3c"
                            :reason :absent})))
                    (range 20))]
    (try
      (deliver ready true)
      (let [results (mapv deref calls)]
        (is (= 1 (count (set (map :request-id results)))))
        (is (= 1 (count (filter :created? results))))
        (is (= 1 (count (sqlite/rerun-requests b {:namespace "concurrent-test"})))))
      (finally (cleanup! dir)))))

(deftest rerun-request-time-bar
  (let [dir (temp-dir) b (sqlite/sqlite-backend (io/file dir "registry.sqlite"))
        base (.getEpochSecond (Instant/parse "2026-09-27T07:00:00Z"))
        entries (mapv (fn [i]
                        (entry (str "perf-entry-" i)
                               (str (Instant/ofEpochSecond (+ base i)))))
                      (range 200))]
    (try
      (is (every? :ok (sqlite/append-batch! b entries)))
      (let [samples (mapv (fn [i]
                            (let [start (System/nanoTime)]
                              (sqlite/request-rerun!
                               b {:namespace (str "request-perf-" i)
                                  :repo "futon3c" :reason :absent})
                              (/ (double (- (System/nanoTime) start)) 1000000.0)))
                          (range 200))
            ordered (sort samples)
            median (nth ordered 100)
            maximum (last ordered)]
        (println "RERUN REQUEST TIME BAR"
                 {:registry-entries 200 :requests 200
                  :median-ms median :maximum-ms maximum})
        (is (< maximum 50.0)))
      (finally (cleanup! dir)))))

(deftest five-thousand-entry-time-bar
  (let [dir (temp-dir) b (sqlite/sqlite-backend (io/file dir "registry.sqlite"))
        base (.getEpochSecond (Instant/parse "2026-09-27T06:00:00Z"))
        entries (mapv (fn [i]
                        (if (< i 2000)
                          (run-entry (str "run-" i) (str "perf-" (mod i 20))
                                     ["runner" (str (mod i 20))]
                                     (str (Instant/ofEpochSecond (+ base i)))
                                     (str (Instant/ofEpochSecond (+ base i 1))) true)
                          (entry (str "entry-" i) (str (Instant/ofEpochSecond (+ base i))))))
                      (range 5000))]
    (try
      (is (every? :ok (sqlite/append-batch! b entries)))
      (let [get-ms (median-ms #(evidence/-get b "entry-4999"))
            latest-ms (median-ms #(sqlite/latest-run-for-namespace b "perf-19"))]
        (println "SQLITE TIME BAR" {:entries 5000 :runs 2000
                                     :get-median-ms get-ms :latest-median-ms latest-ms})
        (is (< get-ms 10.0))
        (is (< latest-ms 10.0)))
      (finally (cleanup! dir)))))
