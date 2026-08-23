(ns futon3c.apm.campaign-snapshot-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.campaign-ledger :as ledger]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.campaign-snapshot :as snapshot])
  (:import [java.nio.charset StandardCharsets]
           [java.nio.file Files OpenOption Path StandardOpenOption]
           [java.nio.file.attribute FileAttribute]
           [java.time Instant]))

(def now (Instant/parse "2026-08-20T12:00:00Z"))
(def observed-at "2026-08-20T11:59:50Z")
(def phases [:probe :close])

(defn event [seq type body]
  {:event/id (str "e" seq) :event/seq seq :event/type type
   :event/campaign-id "apm-200" :event/actor "regulator"
   :event/at observed-at :event/expected-version seq :event/body body})

(defn temp-dir []
  (Files/createTempDirectory "campaign-snapshot-"
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

(defn fixture []
  (let [dir (temp-dir)
        ledger-path (.resolve dir "ledger.edn")
        registration-path
        (write-edn! (.resolve dir "registration.edn")
                    {:reg/frame-id "f1" :reg/harness-revision "harness"
                     :problem {:problem-id "p"}})
        registration-hash
        (let [bytes (Files/readAllBytes registration-path)
              digest (.digest (java.security.MessageDigest/getInstance "SHA-256") bytes)]
          (apply str (map #(format "%02x" (bit-and (int %) 0xff)) digest)))
        frame-path (write-edn! (.resolve dir "frame.edn")
                               {:frame/status :open :frame/id "frame-1-p-solver"
                                :problem "p" :batch "b1" :arm :solver})
        events [(event 0 :campaign/registered
                       {:series :apm :phase-order phases})
                (event 1 :block/opened {:block-id "b1"})
                (event 2 :frame/opened
                       {:frame-id "f1" :block-id "b1" :problem-id "p"
                        :registration-hash registration-hash
                        :harness-hash "harness"})]
        _ (reduce (fn [receipt e]
                    (ledger/compare-and-append!
                     ledger-path
                     (get-in receipt [:after :version] 0)
                     (get-in receipt [:after :digest]
                             (:ledger/digest (machine/projection []))) e))
                  {} events)
        projection (:projection (ledger/read-ledger ledger-path))]
    {:dir dir :ledger-path ledger-path :registration-path registration-path
     :frame-path frame-path :projection projection}))

(defn observation [{:keys [registration-path frame-path projection]}]
  {:registration-path registration-path
   :frame-record-paths [frame-path]
   :receipt-paths []
   :binding-response {:ok true :bound? true :agent-id "f1-guide"
                      :ledger-digest (:ledger/digest projection)}
   :jobs-response {:ok true :jobs []}})

(deftest snapshot-binds-ledger-facts-and-reconciliation
  (let [f (fixture)]
    (try
      (let [result (snapshot/snapshot
                    {:ledger-path (:ledger-path f) :observation (observation f)
                     :now now})
            certificate (:certificate result)]
        (is (:ok result))
        (is (= :valid (:snapshot/status certificate)))
        (is (= (:ledger/digest (:projection f)) (:ledger/digest certificate)))
        (is (= 64 (count (:facts/digest certificate))))
        (is (= 64 (count (:certificate/id certificate)))))
      (finally (delete-tree! (:dir f))))))

(deftest observation-failure-still-emits-stale-certificate
  (let [f (fixture)]
    (try
      (let [result (snapshot/snapshot
                    {:ledger-path (:ledger-path f)
                     :observation (assoc (observation f)
                                         :registration-path "/missing")
                     :now now})
            certificate (:certificate result)]
        (is (:ok result))
        (is (= :stale (:snapshot/status certificate)))
        (is (= :campaign-observation-failed
               (:observation/error certificate))))
      (finally (delete-tree! (:dir f))))))

(deftest contradictory-observation-emits-conflict-certificate
  (let [f (fixture)]
    (try
      (let [result (snapshot/snapshot
                    {:ledger-path (:ledger-path f)
                     :observation (assoc-in (observation f)
                                            [:binding-response :agent-id]
                                            "f2-guide")
                     :now now})]
        (is (= :conflict (get-in result [:certificate :snapshot/status]))))
      (finally (delete-tree! (:dir f))))))

(deftest certificate-persistence-is-create-only-and-idempotent
  (let [f (fixture)]
    (try
      (let [certificate (:certificate
                         (snapshot/snapshot
                          {:ledger-path (:ledger-path f)
                           :observation (observation f) :now now}))
            directory (.resolve ^Path (:dir f) "certificates")
            first-write (snapshot/persist! directory certificate)
            second-write (snapshot/persist! directory certificate)
            loaded (snapshot/read-certificate (:path first-write))]
        (is (:created? first-write))
        (is (false? (:created? second-write)))
        (is (= certificate (:certificate loaded))))
      (finally (delete-tree! (:dir f))))))

(deftest forged-certificate-id-is-refused-before-write
  (let [dir (temp-dir)]
    (try
      (let [result (snapshot/persist!
                    dir {:certificate/id (apply str (repeat 64 "0"))
                         :certificate/type :campaign-projection})]
        (is (= :campaign-certificate-content-mismatch (:error/code result)))
        (is (empty? (with-open [paths (Files/list dir)]
                      (vec (iterator-seq (.iterator paths)))))))
      (finally (delete-tree! dir)))))
