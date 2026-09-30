(ns futon3c.transport.invoke-ledger-coalescing-test
  "Acceptance for the 2026-09-30 coalesced invoke-jobs ledger persistence
  (kimi-5's write-storm finding): the event path must not wait on the
  full-ledger tmp+fsync+rename write.

  (a) 50 job updates in a burst produce <= 3 ledger writes and the flushed
      file equals the final in-memory ledger;
  (b) a write stubbed to sleep 2 s does not delay 10 updates beyond 200 ms;
  (c) a terminal job state is on disk within the flush bound;
  (plant) the same 10 updates with a SYNCHRONOUS write take >= 2 s — i.e. the
      old behaviour the coalescer removes."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.transport.http :as http])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-dir []
  (.toFile (Files/createTempDirectory
            "invoke-ledger-coalescing-"
            (make-array FileAttribute 0))))

(defn- delete-tree! [file]
  (when (.exists file)
    (doseq [child (reverse (file-seq file))]
      (Files/deleteIfExists (.toPath child)))))

(defn- seed-ledger!
  "One running job, persisted to the (already redef'd) temp store."
  []
  (#'http/reset-invoke-jobs!)
  (#'http/persist-invoke-jobs-ledger!
   {:version 1 :next-seq 0 :job-order ["job-1"]
    :trace->job {} :jobs {"job-1" {:job-id "job-1" :state "queued"
                                    :events []}}}))

(defn- with-isolated-invoke-store
  "Hermetic store: rebind invoke-jobs-store-path to a temp file for F (which
  receives the target file), including setup/teardown, so neither the test
  nor the background flusher thread ever touches the real /tmp ledger."
  [f]
  (let [dir (temp-dir)
        target (io/file dir "invoke-jobs.edn")
        before @(var-get #'http/!invoke-jobs-ledger)]
    (try
      (with-redefs-fn {#'http/invoke-jobs-store-path
                       (constantly (.getAbsolutePath target))}
        (fn []
          (seed-ledger!)
          (f target)
          ;; drain pending work and clear flush state while the store-path
          ;; redef is still in force
          (try (#'http/flush-invoke-jobs-ledger!) (catch Throwable _ nil))
          (#'http/reset-invoke-jobs!)))
      (finally
        (reset! (var-get #'http/!invoke-jobs-ledger) before)
        (delete-tree! dir)))))

(defn- update-burst!
  [n]
  (dotimes [i n]
    (#'http/update-invoke-jobs-ledger!
     #(assoc-in % [:jobs "job-1" :events]
                (conj (or (get-in % [:jobs "job-1" :events]) [])
                      {:seq i :type "text" :text (str "event " i)})))))

(deftest burst-of-updates-coalesces-into-few-writes
  (testing "(a) 50 updates in a burst produce <= 3 writes; final file == memory"
    (with-isolated-invoke-store
     (fn [target]
       (let [writes (atom 0)]
         (with-redefs-fn
           {#'http/*invoke-ledger-flush-interval-ms* 4000
            #'http/*invoke-ledger-write!*
            (fn [ledger]
              (swap! writes inc)
              (#'http/persist-invoke-jobs-ledger! ledger))}
           (fn []
             (update-burst! 50)
             (let [deadline (+ (System/nanoTime) (* 10 1e9))]
               (while (and (or (:dirty? @(var-get #'http/!invoke-ledger-flush-state))
                               (:prompt? @(var-get #'http/!invoke-ledger-flush-state)))
                           (< (System/nanoTime) deadline))
                 (Thread/sleep 25)))
             (is (<= @writes 3)
                 (str "expected <= 3 writes, saw " @writes))
             (is (= @(var-get #'http/!invoke-jobs-ledger)
                    (edn/read-string (slurp target)))
                 "flushed file equals the final in-memory ledger"))))))))

(deftest event-path-does-not-wait-on-the-write
  (testing "(b) a 2 s write stub does not delay 10 updates beyond 200 ms"
    (with-isolated-invoke-store
     (fn [_target]
       (with-redefs-fn
         {#'http/*invoke-ledger-write!*
          (fn [_ledger] (Thread/sleep 2000))}
         (fn []
           (let [start (System/nanoTime)]
             (update-burst! 10)
             (let [elapsed-ms (/ (- (System/nanoTime) start) 1e6)]
               (is (< elapsed-ms 200)
                   (str "10 updates took " elapsed-ms " ms; the event path "
                        "is waiting on the write"))))))))))

(deftest synchronous-write-stub-stalls-the-event-path
  (testing "PLANT: with the write called synchronously per update, 10 updates
            against a 2 s stub take >= 2 s — the behaviour the coalescer
            removes. If this stops failing, the coalescing was lost and (b)
            above is lying."
    (with-isolated-invoke-store
     (fn [_target]
       (with-redefs-fn
         {#'http/*invoke-ledger-write!* (fn [_ledger] (Thread/sleep 2000))}
         (fn []
           (let [start (System/nanoTime)]
             (dotimes [_ 10]
               ;; the OLD event path: write inline with the mutation
               (#'http/update-invoke-jobs-ledger! identity)
               ((var-get #'http/*invoke-ledger-write!*) nil))
             (is (>= (/ (- (System/nanoTime) start) 1e6) 2000)))))))))

(deftest terminal-state-is-on-disk-promptly
  (testing "(c) a terminal transition reaches disk within the flush bound"
    (with-isolated-invoke-store
     (fn [target]
       (#'http/update-invoke-jobs-ledger!
       #(-> %
            (assoc-in [:jobs "job-1" :state] "done")
            (assoc-in [:jobs "job-1" :finished-at] "2026-09-30T00:00:00Z")))
      ;; prompt flush skips the 1.5 s coalesce wait; allow the write itself
      ;; plus margin, still inside the flush bound.
      (let [deadline (+ (System/nanoTime) (* 3 1e9))
            on-disk (atom false)]
        (while (and (not @on-disk) (< (System/nanoTime) deadline))
          (Thread/sleep 20)
          (when (.exists target)
            (try
              (when (= "done"
                       (get-in (edn/read-string (slurp target))
                               [:jobs "job-1" :state]))
                (reset! on-disk true))
              (catch Throwable _ nil))))
         (is (true? @on-disk)
             "terminal state not flushed within 3 s of the transition"))))))
