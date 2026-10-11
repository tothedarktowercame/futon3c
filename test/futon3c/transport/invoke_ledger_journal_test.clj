(ns futon3c.transport.invoke-ledger-journal-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3c.transport.http :as http])
  (:import [java.io RandomAccessFile]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-dir []
  (.toFile (Files/createTempDirectory
            "invoke-ledger-journal-" (make-array FileAttribute 0))))

(defn- delete-tree! [file]
  (when (.exists file)
    (doseq [child (reverse (file-seq file))]
      (Files/deleteIfExists (.toPath child)))))

(defn- ledger [jobs order trace next-seq]
  {:version 1 :jobs jobs :job-order order :trace->job trace
   :next-seq next-seq})

(defn- with-store [f]
  (let [dir (temp-dir)
        target (io/file dir "invoke-jobs.edn")]
    (try
      (with-redefs-fn {#'http/invoke-jobs-store-path
                       (constantly (.getAbsolutePath target))}
        #(f target (io/file dir "invoke-jobs.edn.journal")))
      (finally (delete-tree! dir)))))

(deftest journal-round-trip-reproduces-all-ledger-indexes
  (with-store
    (fn [_ _]
      (let [initial (ledger {"a" {:job-id "a" :state "queued"}}
                            ["a"] {"ta" "a"} 1)]
        (#'http/invoke-ledger-write! initial)
        (let [states (reductions
                      (fn [l i]
                        (case (mod i 4)
                          0 (let [id (str "j" i)]
                              (-> l
                                  (assoc-in [:jobs id]
                                            {:job-id id :state "queued" :events []})
                                  (update :job-order conj id)
                                  (assoc-in [:trace->job (str "t" i)] id)
                                  (update :next-seq inc)))
                          1 (update-in l [:jobs (last (:job-order l)) :events]
                                       (fnil conj []) {:type "text" :n i})
                          2 (assoc-in l [:jobs (last (:job-order l)) :state] "done")
                          3 (if (> (count (:job-order l)) 2)
                              (let [id (second (:job-order l))
                                    traces (into {} (remove (fn [[_ jid]] (= jid id)))
                                                 (:trace->job l))]
                                (-> l (update :jobs dissoc id)
                                    (update :job-order #(into [] (remove #{id}) %))
                                    (assoc :trace->job traces)))
                              l)))
                      initial (range 40))]
          (doseq [state (rest states)] (#'http/invoke-ledger-write! state))
          (is (= (last states) (#'http/load-invoke-jobs-ledger)))
          (is (= (:job-order (last states))
                 (:job-order (#'http/load-invoke-jobs-ledger))))
          (is (= (:trace->job (last states))
                 (:trace->job (#'http/load-invoke-jobs-ledger)))))))))

(deftest changed-job-write-volume-is-bounded
  (with-store
    (fn [snapshot journal]
      (let [payload (apply str (repeat 19000 "x"))
            jobs (into {} (map (fn [i]
                                 (let [id (str "job-" i)]
                                   [id {:job-id id :state "queued" :payload payload}]))
                               (range 5000)))
            initial (ledger jobs (vec (keys jobs)) {} 5000)]
        (#'http/invoke-ledger-write! initial)
        (loop [i 0 state initial]
          (when (< i 200)
            (let [id (str "job-" (mod (* i 17) 5000))
                  next (assoc-in state [:jobs id :tick] i)]
              (#'http/invoke-ledger-write! next)
              (recur (inc i) next))))
        (let [snapshot-size (.length snapshot)
              journal-size (.length journal)
              ratio (/ (double journal-size) (* 200.0 snapshot-size))]
          (println "invoke ledger journal write-volume ratio:" ratio)
          (is (< ratio 0.02) (str "measured ratio " ratio)))))))

(deftest torn-tail-is-ignored-but-corrupt-middle-is-loud
  (with-store
    (fn [_ journal]
      (let [a (ledger {"a" {:job-id "a" :state "queued"}} ["a"] {} 1)
            b (assoc-in a [:jobs "a" :state] "running")
            c (assoc-in b [:jobs "a" :state] "done")]
        (#'http/invoke-ledger-write! a)
        (swap! (var-get #'http/!invoke-ledger-journal-state)
               update-vals #(assoc % :snapshot-size Long/MAX_VALUE))
        (#'http/invoke-ledger-write! b)
        (#'http/invoke-ledger-write! c)
        (with-open [raf (RandomAccessFile. journal "rw")]
          (.setLength raf (- (.length raf) 12)))
        (is (= b (#'http/load-invoke-jobs-ledger)))
        (let [lines (vec (line-seq (io/reader journal)))]
          (spit journal (str (first lines) "\n{:broken\n" (last lines) "\n"))
          (is (thrown-with-msg? clojure.lang.ExceptionInfo
                                #"refusing empty fallback"
                                (#'http/load-invoke-jobs-ledger))))))))

(deftest every-snapshot-rotation-crash-point-is-loadable
  (doseq [stage [:temp-forced :renamed :journal-temp-forced :journal-renamed]]
    (with-store
      (fn [_ _]
        (let [before (ledger {"a" {:job-id "a" :state "queued"}} ["a"] {} 1)
              after (assoc-in before [:jobs "a" :state] "done")
              state-atom (var-get #'http/!invoke-ledger-journal-state)
              store (str (.toAbsolutePath
                          (java.nio.file.Path/of (#'http/invoke-jobs-store-path)
                                                (make-array String 0))))]
          (#'http/invoke-ledger-write! before)
          (swap! state-atom assoc-in [store :snapshot-size] -1)
          (try
            (binding [http/*invoke-jobs-persist-stage-hook*
                      (fn [seen _]
                        (when (= seen stage) (throw (ex-info "crash" {}))))]
              (#'http/invoke-ledger-write! after))
            (catch clojure.lang.ExceptionInfo _))
          (is (contains? #{before after} (#'http/load-invoke-jobs-ledger))
              (str "loadable at " stage)))))))

(deftest old-format-snapshot-loads-without-a-journal
  (with-store
    (fn [snapshot _]
      (let [old (ledger {"old" {:job-id "old" :state "done"}}
                        ["old"] {} 1)]
        (spit snapshot (pr-str old))
        (is (= old (#'http/load-invoke-jobs-ledger)))))))

(deftest inflight-recovery-runs-after-journal-replay
  (with-store
    (fn [_ _]
      (let [queued (ledger {"a" {:job-id "a" :state "queued" :events []}}
                           ["a"] {} 1)
            running (assoc-in queued [:jobs "a" :state] "running")]
        (#'http/invoke-ledger-write! queued)
        (#'http/invoke-ledger-write! running)
        (let [recovered (#'http/recover-inflight-jobs
                         (#'http/load-invoke-jobs-ledger))]
          (is (= "failed" (get-in recovered [:jobs "a" :state])))
          (is (= "worker-lost-on-restart"
                 (get-in recovered [:jobs "a" :terminal-code]))))))))

(deftest request-commission-sees-latest-journal-state
  (with-store
    (fn [_ _]
      (let [commission {:agent-id "codex-1" :prompt "do it"
                        :caller "claude-15" :surface "bell"}
            base-job {:job-id "a" :agent-id "codex-1" :prompt "do it"
                      :caller "claude-15" :surface "bell" :mode "work"
                      :request-digest (#'http/invoke-job-request-digest
                                       "codex-1" "do it" "claude-15" "bell" nil)
                      :request-commission commission
                      :state "queued" :events []}
            before (ledger {"a" base-job} ["a"] {} 1)
            after (-> before
                      (assoc-in [:jobs "a" :state] "done")
                      (assoc-in [:jobs "a" :trace-id] "trace-a"))
            ledger-atom (var-get #'http/!invoke-jobs-ledger)
            saved @ledger-atom]
        (try
          (#'http/invoke-ledger-write! before)
          (#'http/invoke-ledger-write! after)
          (reset! ledger-atom (#'http/load-invoke-jobs-ledger))
          (let [commission (http/invoke-job-request-commission "a")]
            (is (= "do it" (get-in commission [:commission :prompt])))
            (is (= "trace-a" (get-in commission [:job-join :trace-id]))))
          (finally (reset! ledger-atom saved)))))))
