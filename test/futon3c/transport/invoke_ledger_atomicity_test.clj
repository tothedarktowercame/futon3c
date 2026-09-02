(ns futon3c.transport.invoke-ledger-atomicity-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
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

(deftest multi-chunk-persist-round-trips
  ;; D13 regression (claude-2): the persist writes in 1MB chunks so no
  ;; handler thread ever caches a ledger-sized direct buffer (the 2026-09-02
  ;; outage: a 170MB single write exhausted MaxDirectMemory). A >2-chunk
  ;; ledger must round-trip byte-exactly through the chunked path.
  (testing "a ~3MB ledger persists and reads back equal via chunked writes"
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
                "fixture is large enough to exercise multiple chunks")
            (is (= ledger (edn/read-string (slurp path)))
                "chunked write round-trips byte-exactly")))
        (finally (delete-tree! dir))))))
