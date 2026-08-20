(ns futon3c.apm.campaign-observe-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.campaign-observe :as observe])
  (:import [java.nio.charset StandardCharsets]
           [java.nio.file Files OpenOption Path StandardOpenOption]
           [java.nio.file.attribute FileAttribute]
           [java.time Instant]))

(def now (Instant/parse "2026-08-20T12:00:00Z"))

(defn temp-dir []
  (Files/createTempDirectory "campaign-observe-"
                             (make-array FileAttribute 0)))

(defn write-edn! [^Path path value]
  (Files/writeString path (pr-str value) StandardCharsets/UTF_8
                     (into-array OpenOption [StandardOpenOption/CREATE_NEW
                                             StandardOpenOption/WRITE]))
  path)

(defn delete-tree! [^Path dir]
  (with-open [paths (Files/walk dir (make-array java.nio.file.FileVisitOption 0))]
    (doseq [path (reverse (sort-by #(.getNameCount ^Path %)
                                  (iterator-seq (.iterator paths))))]
      (Files/deleteIfExists path))))

(deftest registration-is-normalized-from-real-schema-and-byte-hashed
  (let [dir (temp-dir)]
    (try
      (let [path (write-edn! (.resolve dir "registration.edn")
                             {:reg/frame-id "f17"
                              :reg/harness-revision "harness"
                              :reg/solver-seat "f17-solver"
                              :reg/guide-seat "f17-guide"
                              :problem {:problem-id "m97A06"}})
            result (observe/observe-registration path now)]
        (is (:ok result))
        (is (= "f17" (get-in result [:fact :frame-id])))
        (is (= "m97A06" (get-in result [:fact :problem-id])))
        (is (= "f17-guide" (get-in result [:fact :seats :guide])))
        (is (= 64 (count (get-in result [:fact :registration-hash])))))
      (finally (delete-tree! dir)))))

(deftest frame-arm-records-are-aggregated-with-independent-digests
  (let [dir (temp-dir)]
    (try
      (let [solver (write-edn! (.resolve dir "solver.edn")
                               {:frame/status :open :arm :solver
                                :frame/id "frame-17-m97A06-solver"
                                :problem "m97A06" :batch "frame-17"
                                :seat "f17-solver" :base-revision "base"})
            student (write-edn! (.resolve dir "student.edn")
                                {:frame/status :open :arm :student
                                 :frame/id "frame-17-m97A06-student"
                                 :problem "m97A06" :batch "frame-17"
                                 :seat "f17-student" :base-revision "base"})
            result (observe/observe-frame-records [solver student] "f17" now)]
        (is (:ok result))
        (is (= :open (get-in result [:fact :status])))
        (is (= 2 (count (get-in result [:fact :records]))))
        (is (every? #(= 64 (count (:digest %)))
                    (get-in result [:fact :records]))))
      (finally (delete-tree! dir)))))

(deftest disagreement-between-frame-records-is-exposed
  (let [dir (temp-dir)]
    (try
      (let [a (write-edn! (.resolve dir "a.edn")
                          {:frame/status :open :frame/id "frame-17-p-solver"
                           :problem "p" :batch "frame-17"})
            b (write-edn! (.resolve dir "b.edn")
                          {:frame/status :closed :frame/id "frame-17-q-student"
                           :problem "q" :batch "frame-17"})
            result (observe/observe-frame-records [a b] "f17" now)]
        (is (:ok result))
        (is (= :conflict (get-in result [:fact :status])))
        (is (nil? (get-in result [:fact :problem-id]))))
      (finally (delete-tree! dir)))))

(deftest binding-and-jobs-normalize-string-keyed-http-shapes
  (let [binding (observe/normalize-binding
                 {"ok" true "bound?" true "agent-id" "f17-guide"
                  "problem-id" "m97A06" "version" 27
                  "phase" "guided-solve"} now)
        jobs (observe/normalize-jobs
              {"ok" true
               "jobs" [{"job-id" "j1" "agent-id" "f17-solver"
                         "state" "running"}]} now)]
    (is (:ok binding))
    (is (= "f17" (get-in binding [:fact :frame-id])))
    (is (= 27 (get-in binding [:fact :binding-version])))
    (is (:ok jobs))
    (is (= {:job-id "j1" :agent-id "f17-solver" :frame-id "f17"
            :role :solver :state :running :created-at nil :started-at nil
            :finished-at nil}
           (first (get-in jobs [:fact :items]))))))

(deftest receipt-files-use-content-digest-not-self-declaration
  (let [dir (temp-dir)]
    (try
      (let [path (write-edn! (.resolve dir "soundness.edn")
                             {:receipt/kind :soundness :receipt/frame-id "f17"
                              :receipt/digest "declared"})
            result (observe/observe-receipts [path] now)
            receipt (first (get-in result [:fact :items]))]
        (is (:ok result))
        (is (= "declared" (:declared-digest receipt)))
        (is (= 64 (count (:digest receipt))))
        (is (not= (:declared-digest receipt) (:digest receipt))))
      (finally (delete-tree! dir)))))

(deftest aggregate-observation-refuses-partial-facts
  (let [result (observe/observe-facts
                {:registration-path "/does/not/exist"
                 :frame-record-paths [] :receipt-paths []
                 :binding-response {:ok true :bound? false}
                 :jobs-response {:ok true :jobs []}
                 :expected-frame-id "f17" :now now})]
    (is (false? (:ok result)))
    (is (= :campaign-observation-failed (:error/code result)))
    (is (contains? (:failures result) :registration))
    (is (contains? (:failures result) :frame-record))))
