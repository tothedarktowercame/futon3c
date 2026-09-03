(ns futon3c.transport.invoke-ledger-atomicity-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.agency.parked-on :as parked-on]
            [futon3c.transport.http :as http])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-dir []
  (.toFile (Files/createTempDirectory
            "invoke-ledger-atomicity-"
            (make-array FileAttribute 0))))

(defn- delete-tree! [file]
  (when (.exists file)
    (doseq [child (reverse (file-seq file))]
      (Files/deleteIfExists (.toPath child)))))

(deftest interrupted-replacement-preserves-previous-ledger
  (let [dir (temp-dir)
        target (io/file dir "invoke-jobs.edn")
        old {:version 1 :next-seq 1 :job-order ["old"]
             :trace->job {} :jobs {"old" {:state "done"}}}
        new (assoc old :next-seq 2 :job-order ["old" "new"]
                   :jobs {"old" {:state "done"}
                          "new" {:state "queued"}})]
    (try
      (with-redefs-fn {#'http/invoke-jobs-store-path
                       (constantly (.getAbsolutePath target))}
        (fn []
          (#'http/persist-invoke-jobs-ledger! old)
          (is (thrown-with-msg?
               clojure.lang.ExceptionInfo
               #"persistence failed"
               (with-bindings {#'http/*invoke-jobs-persist-stage-hook*
                               (fn [stage _]
                                 (when (= :temp-forced stage)
                                   (throw (ex-info "simulated interruption" {}))))}
                 (#'http/persist-invoke-jobs-ledger! new))))
          (is (= old (edn/read-string (slurp target))))
          (is (empty? (filter #(re-find #"^\.invoke-jobs-" (.getName %))
                              (.listFiles dir))))))
      (finally (delete-tree! dir)))))

(deftest truncated-ledger-is-loud
  (let [dir (temp-dir)
        target (io/file dir "invoke-jobs.edn")]
    (try
      (spit target "{:version 1 :jobs {")
      (with-redefs-fn {#'http/invoke-jobs-store-path
                       (constantly (.getAbsolutePath target))}
        (fn []
          (is (thrown-with-msg?
               clojure.lang.ExceptionInfo
               #"refusing empty fallback"
               (#'http/load-invoke-jobs-ledger)))))
      (finally (delete-tree! dir)))))

(deftest schema-incomplete-ledgers-are-loud-and-unchanged
  (doseq [incomplete [{} {:version 1} {:jobs {}}
                      {:version 1 :next-seq 0 :job-order []
                       :trace->job {} :jobs {}}]]
    (let [dir (temp-dir)
          target (io/file dir "invoke-jobs.edn")
          original (pr-str incomplete)]
      (try
        (spit target original)
        (with-redefs-fn {#'http/invoke-jobs-store-path
                         (constantly (.getAbsolutePath target))}
          (fn []
            (is (thrown-with-msg?
                 clojure.lang.ExceptionInfo
                 #"refusing empty fallback"
                 (#'http/load-invoke-jobs-ledger)))
            (is (= original (slurp target)))))
        (finally (delete-tree! dir))))))

(deftest absent-ledger-is-a-fresh-install-not-an-empty-authority
  (let [dir (temp-dir)
        target (io/file dir "invoke-jobs.edn")
        ledger-atom (var-get #'http/!invoke-jobs-ledger)
        index-atom (var-get #'http/!active-invoke-job-index)
        before-ledger @ledger-atom
        before-index @index-atom]
    (try
      (reset! ledger-atom nil)
      (reset! index-atom nil)
      (with-redefs-fn {#'http/invoke-jobs-store-path
                       (constantly (.getAbsolutePath target))}
        (fn []
          (is (= {} (:jobs (#'http/ensure-invoke-jobs-ledger!))))
          (is (false? (.exists target)))))
      (finally
        (reset! ledger-atom before-ledger)
        (reset! index-atom before-index)
        (delete-tree! dir)))))

(deftest persistence-error-reaches-mutation-caller-and-rolls-back-memory
  (let [dir (temp-dir)
        ledger-atom (var-get #'http/!invoke-jobs-ledger)
        index-atom (var-get #'http/!active-invoke-job-index)
        before-ledger @ledger-atom
        before-index @index-atom
        old {:version 1 :next-seq 0 :job-order [] :trace->job {} :jobs {}}]
    (try
      (reset! ledger-atom old)
      (reset! index-atom nil)
      ;; A directory cannot be replaced by the temporary ledger file.
      (with-redefs-fn {#'http/invoke-jobs-store-path
                       (constantly (.getAbsolutePath dir))}
        (fn []
          (testing "the update does not report success or retain unpersisted state"
            (is (thrown-with-msg?
                 clojure.lang.ExceptionInfo
                 #"persistence failed"
                 (#'http/update-invoke-jobs-ledger!
                  #(assoc % :next-seq 1))))
            (is (= old @ledger-atom)))))
      (finally
        (reset! ledger-atom before-ledger)
        (reset! index-atom before-index)
        (delete-tree! dir)))))

(deftest post-rename-force-failure-keeps-memory-equal-to-disk
  (let [dir (temp-dir)
        target (io/file dir "invoke-jobs.edn")
        ledger-atom (var-get #'http/!invoke-jobs-ledger)
        index-atom (var-get #'http/!active-invoke-job-index)
        before-ledger @ledger-atom
        before-index @index-atom
        old {:version 1 :next-seq 1 :job-order ["old"]
             :trace->job {} :jobs {"old" {:state "done"}}}]
    (try
      (reset! ledger-atom old)
      (reset! index-atom nil)
      (with-redefs-fn {#'http/invoke-jobs-store-path
                       (constantly (.getAbsolutePath target))}
        (fn []
          (#'http/persist-invoke-jobs-ledger! old)
          (let [failure (try
                          (with-bindings
                            {#'http/*invoke-jobs-persist-stage-hook*
                             (fn [stage _]
                               (when (= :renamed stage)
                                 (throw (ex-info "directory force failed" {}))))}
                            (#'http/update-invoke-jobs-ledger!
                             #(-> %
                                  (assoc :next-seq 2)
                                  (assoc-in [:jobs "new"] {:state "queued"})
                                  (update :job-order conj "new"))))
                          nil
                          (catch clojure.lang.ExceptionInfo e e))
                disk (edn/read-string (slurp target))]
            (is (some? failure))
            (is (true? (:committed? (ex-data failure))))
            (is (= :unconfirmed (:durability (ex-data failure))))
            (is (= disk @ledger-atom))
            (is (= 2 (:next-seq disk))))))
      (finally
        (reset! ledger-atom before-ledger)
        (reset! index-atom before-index)
        (delete-tree! dir)))))

(deftest heap-stream-persist-round-trips-large-ledger
  ;; D15 retains D13's ~3MB round-trip regression while replacing the payload
  ;; FileChannel with heap-backed streams. FileChannel.write of heap buffers
  ;; populated per-thread direct-buffer caches during the 2026-09-02 outage.
  (testing "a ~3MB ledger persists and reads back equal via heap streams"
    (let [dir (temp-dir)
          path (str (io/file dir "jobs.edn"))
          filler (apply str (repeat 1500 "x"))
          ledger {:version 1
                  :next-seq 2048
                  :job-order (vec (map #(str "job-" %) (range 2048)))
                  :trace->job {}
                  :jobs (into {} (map (fn [i]
                                        [(str "job-" i)
                                         {:job-id (str "job-" i)
                                          :state "done"
                                          :events [{:type "prompt" :text filler}]}])
                                      (range 2048)))}]
      (try
        (with-redefs-fn {#'http/invoke-jobs-store-path (constantly path)}
          (fn []
            (#'http/persist-invoke-jobs-ledger! ledger)
            (is (> (.length (io/file path)) (* 2 1024 1024))
                "fixture exercises a production-sized write")
            (is (= ledger (edn/read-string (slurp path)))
                "heap-stream write round-trips byte-exactly")))
        (finally (delete-tree! dir))))))

(deftest rolling-expiry-compacts-terminal-detail-but-retains-live-and-parked-jobs
  ;; Live ledger pin, /tmp/futon3c-invoke-jobs.edn, job
  ;; invoke-1787750995295-1507-0f434f39 (captured 2026-09-02): these values are
  ;; verbatim -- state "failed", created-at 2026-08-26T13:29:55.295079374Z,
  ;; finished-at 2026-08-26T13:29:56.668288653Z, event-seq 5.
  (let [old-id "invoke-1787750995295-1507-0f434f39"
        active-id "active-job"
        parked-id "parked-terminal-job"
        bulky (apply str (repeat 10000 "recorded packet text "))
        old {:job-id old-id :state "failed"
             :created-at "2026-08-26T13:29:55.295079374Z"
             :finished-at "2026-08-26T13:29:56.668288653Z"
             :event-seq 5 :result-text bulky :result bulky
             :events [{:seq 1 :type "accepted" :at "2026-08-26T13:29:55.295079374Z"
                       :text bulky}
                      {:seq 5 :type "failed" :at "2026-08-26T13:29:56.668288653Z"
                       :message "ended"}]}
        active (assoc old :job-id active-id :state "running" :finished-at nil)
        parked (assoc old :job-id parked-id)
        ledger {:version 1 :next-seq 3
                :job-order [old-id active-id parked-id]
                :trace->job {} :jobs {old-id old active-id active parked-id parked}}
        ;; D14: this clock must stay INSIDE the seven-day tombstone horizon
        ;; (finished-at + 7d = 2026-09-02T13:29Z). Past it, old-id would be
        ;; dropped rather than compacted and this test would be asserting the
        ;; wrong horizon. It is >24h after finished-at, so detail expiry fires.
        compacted (with-redefs [http/*invoke-ledger-now*
                                (constantly (java.time.Instant/parse
                                             "2026-08-28T20:00:00Z"))
                                parked-on/snapshot
                                (constantly {:index {parked-id #{"park-1"}}})]
                    (#'http/compact-invoke-jobs-ledger ledger))]
    (is (= {old-id "failed" active-id "running" parked-id "failed"}
           (into {} (map (fn [[id job]] [id (:state job)])) (:jobs compacted)))
        "expiry retains every id and lifecycle state")
    (is (= :d13/rolling-expiry (get-in compacted [:jobs old-id :events-trimmed])))
    (is (nil? (get-in compacted [:jobs old-id :result-text])))
    (is (< (count (pr-str (get-in compacted [:jobs old-id]))) 2000)
        "expired terminal payload has a fixed small bound")
    (is (= bulky (get-in compacted [:jobs active-id :result-text]))
        "active job remains complete")
    (is (= bulky (get-in compacted [:jobs parked-id :result-text]))
        "park-dependent terminal job remains complete")
    (is (= :d13/rolling-expiry
           (:events-trimmed (#'http/invoke-job-public-view
                             (get-in compacted [:jobs old-id]))))
        "the public job view reports the expiry, so :result nil is not
         mistaken for a job that produced nothing")))

(deftest rolling-expiry-drops-aged-tombstones-without-dangling-indexes
  ;; Live ledger pin, /tmp/futon3c-invoke-jobs.edn, job
  ;; invoke-1787750995295-1507-0f434f39 (captured 2026-09-02): the id, state
  ;; "failed", and finished-at 2026-08-26T13:29:56.668288653Z are verbatim.
  (let [live-id "invoke-1787750995295-1507-0f434f39"
        active-id "active-job"
        parked-id "parked-terminal-job"
        aged-job (fn [id]
                   {:job-id id :state "failed"
                    :finished-at "2026-08-26T13:29:56.668288653Z"
                    :events-trimmed :d13/rolling-expiry})
        aged-ids (into [live-id] (map #(str "aged-job-" %) (range 5000)))
        jobs (into {active-id {:job-id active-id :state "running"}
                    parked-id (aged-job parked-id)}
                   (map (fn [id] [id (aged-job id)]))
                   aged-ids)
        order (into aged-ids [active-id parked-id])
        ledger {:version 1 :next-seq 5003 :job-order order
                :trace->job {"expired-trace" live-id
                             "active-trace" active-id
                             "parked-trace" parked-id}
                :jobs jobs}
        compacted (with-redefs [http/*invoke-ledger-now*
                                (constantly (java.time.Instant/parse
                                             "2026-09-03T14:00:00Z"))
                                parked-on/snapshot
                                (constantly {:index {parked-id #{"park-1"}}})]
                    (#'http/compact-invoke-jobs-ledger ledger))]
    (is (= #{active-id parked-id} (set (keys (:jobs compacted))))
        "an arbitrarily large aged population leaves only protected jobs")
    (is (= [active-id parked-id] (:job-order compacted))
        ":job-order preserves retained ordering and contains no dropped ids")
    (is (= {"active-trace" active-id "parked-trace" parked-id}
           (:trace->job compacted))
        ":trace->job contains no dropped ids")
    (is (= "running" (get-in compacted [:jobs active-id :state])))
    (is (= "failed" (get-in compacted [:jobs parked-id :state])))
    (let [only-aged (assoc ledger
                           :job-order aged-ids
                           :trace->job {"expired-trace" live-id}
                           :jobs (select-keys jobs aged-ids))
          one-sentinel (with-redefs [http/*invoke-ledger-now*
                                     (constantly (java.time.Instant/parse
                                                  "2026-09-03T14:00:00Z"))
                                     parked-on/snapshot
                                     (constantly {:index {}})]
                         (#'http/compact-invoke-jobs-ledger only-aged))]
      (is (= [(last aged-ids)] (:job-order one-sentinel))
          "the corruption-detection invariant retains only the newest tombstone")
      (is (= {} (:trace->job one-sentinel)))
      ;; Discriminating check: the fixture already carries :events-trimmed, so
      ;; asserting that key alone passes whether or not compaction ran. The
      ;; fixture has no :events key at all, and compact-terminal-job is the
      ;; only thing that can add one -- so this proves the retained sentinel
      ;; is a compacted tombstone rather than the untouched input record.
      (is (= [] (get-in one-sentinel [:jobs (last aged-ids) :events]))
          "the retained sentinel is compacted, not passed through")
      (is (= :d13/rolling-expiry
             (get-in one-sentinel [:jobs (last aged-ids) :events-trimmed]))))))
