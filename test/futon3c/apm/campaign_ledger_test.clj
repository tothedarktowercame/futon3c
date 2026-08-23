(ns futon3c.apm.campaign-ledger-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.campaign-ledger :as ledger]
            [futon3c.apm.campaign-machine :as machine])
  (:import [java.nio.charset StandardCharsets]
           [java.nio.file Files OpenOption Path StandardOpenOption]
           [java.nio.file.attribute FileAttribute]))

(def phases [:probe :freeze :solve :verify :close])

(defn event [seq type body]
  {:event/id (str "event-" seq) :event/seq seq :event/type type
   :event/campaign-id "apm-200" :event/actor "regulator"
   :event/at (str "2026-08-20T00:00:0" seq "Z")
   :event/expected-version seq :event/body body})

(def empty-digest (:ledger/digest (machine/projection [])))

(defn temp-ledger []
  (let [dir (Files/createTempDirectory "campaign-ledger-"
                                       (make-array FileAttribute 0))]
    [dir (.resolve dir "events.edn")]))

(defn delete-tree! [^Path dir]
  (when (Files/exists dir (make-array java.nio.file.LinkOption 0))
    (with-open [paths (Files/walk dir (make-array java.nio.file.FileVisitOption 0))]
      (doseq [path (reverse (sort-by #(.getNameCount ^Path %) (iterator-seq (.iterator paths))))]
        (Files/deleteIfExists path)))))

(deftest compare-and-append-produces-durable-receipts
  (let [[dir path] (temp-ledger)]
    (try
      (let [first-receipt
            (ledger/compare-and-append!
             path 0 empty-digest
             (event 0 :campaign/registered
                    {:series :apm :manifest-hash "manifest"
                     :phase-order phases}))
            second-receipt
            (ledger/compare-and-append!
             path 1 (get-in first-receipt [:after :digest])
             (event 1 :block/opened {:block-id "block-1" :ordinal 1}))
            loaded (ledger/read-ledger path)]
        (is (:ok first-receipt))
        (is (:durable? first-receipt))
        (is (= 0 (get-in first-receipt [:write :byte-offset])))
        (is (:ok second-receipt))
        (is (= 2 (count (:events loaded))))
        (is (= (get-in second-receipt [:after :digest])
               (get-in loaded [:projection :ledger/digest]))))
      (finally (delete-tree! dir)))))

(deftest stale-writers-cannot-append
  (let [[dir path] (temp-ledger)]
    (try
      (let [registered (ledger/compare-and-append!
                        path 0 empty-digest
                        (event 0 :campaign/registered
                               {:phase-order phases}))
            digest (get-in registered [:after :digest])
            winner (ledger/compare-and-append!
                    path 1 digest
                    (event 1 :block/opened {:block-id "winner"}))
            loser (ledger/compare-and-append!
                   path 1 digest
                   (event 1 :block/opened {:block-id "loser"}))]
        (is (:ok winner))
        (is (= :campaign-ledger-version-mismatch (:error/code loser)))
        (is (= 2 (count (:events (ledger/read-ledger path))))))
      (finally (delete-tree! dir)))))

(deftest concurrent-writers-have-one-winner
  (let [[dir path] (temp-ledger)]
    (try
      (let [registered (ledger/compare-and-append!
                        path 0 empty-digest
                        (event 0 :campaign/registered {:phase-order phases}))
            digest (get-in registered [:after :digest])
            start (promise)
            writer (fn [block-id]
                     (future
                       @start
                       (loop [attempts 0]
                         (let [result (ledger/compare-and-append!
                                      path 1 digest
                                      (event 1 :block/opened {:block-id block-id}))]
                           (if (and (= :campaign-ledger-lock-busy (:error/code result))
                                    (< attempts 100))
                             (recur (inc attempts))
                             result)))))
            a (writer "a")
            b (writer "b")]
        (deliver start true)
        (let [results [@a @b]]
          (is (= 1 (count (filter :ok results))))
          (is (= 1 (count (remove :ok results))))
          (is (= 2 (count (:events (ledger/read-ledger path)))))))
      (finally (delete-tree! dir)))))

(deftest corrupt-or-invalid-history-is-never-repaired
  (let [[dir path] (temp-ledger)]
    (try
      (Files/writeString path "{:partial " StandardCharsets/UTF_8
                         (into-array OpenOption [StandardOpenOption/CREATE
                                                 StandardOpenOption/WRITE]))
      (is (= :campaign-ledger-corrupt
             (:error/code (ledger/read-ledger path))))
      (is (= :campaign-ledger-corrupt
             (:error/code
              (ledger/compare-and-append!
               path 0 empty-digest
               (event 0 :campaign/registered {:phase-order phases})))))
      (is (= "{:partial " (Files/readString path StandardCharsets/UTF_8)))
      (finally (delete-tree! dir)))))

(deftest event-is-validated-before-any-write
  (let [[dir path] (temp-ledger)]
    (try
      (let [result (ledger/compare-and-append!
                    path 0 empty-digest
                    (event 0 :frame/opened
                           {:frame-id "f1" :block-id "b1" :problem-id "p"}))]
        (is (= :campaign-event-refused (:error/code result)))
        (is (zero? (Files/size path))))
      (finally (delete-tree! dir)))))
