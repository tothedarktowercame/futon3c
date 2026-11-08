(ns futon3c.agency.clock-outbox-test
  "A futon1b that is down must not cost a turn its admission or a finished job
  its completion (2026-11-08: futon1b was down 08:09-15:18; a finished Codex
  job was marked failed on its clock activity decision, and the park wake that
  should have reported it was refused at admission)."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.agency.clock-decision :as decision]
            [futon3c.agency.clock-store :as clock]
            [futon3c.agency.registry :as reg]
            [futon3c.blackboard :as bb]
            [futon3c.evidence.boundary :as boundary]
            [futon3c.evidence.store :as store])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def agent-id "outbox-worker")
(def session-id "outbox-session")

(def held-decision
  {:agent-id agent-id :session-id session-id :status :clocked :source 1
   :decision-id "clock-decision-held" :decided-at "2026-11-08T00:00:00Z"
   :clock {:mission-id "M-formal-arxana"}})

(defn- refused-connection []
  (ex-info "futon1b unreachable"
           {:error/code :futon1b-unreachable
            :transport/acquired-outcome :unavailable
            :transport/evidence :not-obtained}))

(defn- read-timeout []
  (ex-info "futon1b unreachable"
           {:error/code :futon1b-unreachable
            :transport/acquired-outcome :timeout
            :transport/evidence :not-obtained}))

(def ^:dynamic *backend* nil)

(use-fixtures :each
  (fn [f]
    (clock/reset-store!)
    (reg/reset-registry!)
    (let [dir (str (Files/createTempDirectory "clock-outbox" (into-array FileAttribute [])))]
      (with-redefs [bb/project-agents! (constantly nil)
                    reg/ring-bell-file! (constantly nil)]
        (try
          (binding [*backend* (atom {:entries {} :order []})
                    decision/*outbox-dir* dir
                    decision/*repo-roots* {}
                    decision/*store-busy-waits-ms* []]
            (binding [decision/*test-store* *backend*]
              (f)))
          (finally
            (clock/reset-store!)
            (reg/reset-registry!)
            (doseq [x (reverse (file-seq (io/file dir)))] (io/delete-file x true))))))))

(defn- context [turn-id]
  {:agent-id agent-id :session-id session-id :turn-id turn-id
   :surface "parked-resume" :phase :accepted :text "codex-21 F1c-1 ... REVIEW"})

(defn- hold-clock! [] (clock/set-decision! agent-id session-id held-decision))

(defn- thrown [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e e)))

(defn- stored-ids []
  (set (map :evidence/id (store/query* *backend* {:query/tags [:clock-decision]}))))

(deftest down-store-with-a-held-clock-admits-the-turn-through-the-outbox
  (hold-clock!)
  (with-redefs [store/get-entry* (fn [& _] (throw (refused-connection)))]
    ;; A long schedule proves a down store is not waited out.
    (binding [decision/*store-busy-waits-ms* [60000 60000]]
      (let [started (System/nanoTime)
            d (decision/record! (context "resume-1"))
            elapsed-ms (quot (- (System/nanoTime) started) 1000000)]
        (is (< elapsed-ms 5000) "no busy-schedule wait for a refused connection")
        (is (= :outbox (:persisted-to d)))
        (is (= {:mission-id "M-formal-arxana"} (select-keys (:clock d) [:mission-id]))
            "decided from the clock this JVM holds")
        (is (= (:decision-id d)
               (get-in (clock/current-state agent-id session-id) [:decision :decision-id])))
        (is (= [(:decision-id d)] (map :evidence/id (decision/outbox-entries))))
        (is (empty? (stored-ids)) "nothing reached futon1b while it was down")))))

(deftest down-store-write-is-also-spooled
  (hold-clock!)
  (with-redefs [boundary/append! (fn [& _] {:ok false :error/code :store-unreachable
                                            :error/message "connection refused"})]
    (let [d (decision/record! (context "resume-write"))]
      (is (= :outbox (:persisted-to d))))))

(deftest down-store-without-a-held-clock-still-refuses
  ;; Nothing in RAM to decide from: guessing could unclock a real session.
  (with-redefs [store/get-entry* (fn [& _] (throw (refused-connection)))
                store/count* (fn [& _] (throw (refused-connection)))]
    (let [e (thrown #(decision/record! (context "cold")))]
      (is (= :clock/store-busy (:error/code (ex-data e))))
      (is (empty? (decision/outbox-entries))))))

(deftest slow-store-keeps-the-busy-schedule
  ;; A timeout is futon1b answering slowly, not down: wait, then refuse as before.
  (hold-clock!)
  (let [reads (atom 0)]
    (with-redefs [store/get-entry* (fn [& _] (swap! reads inc) (throw (read-timeout)))]
      (binding [decision/*store-busy-waits-ms* [0 0]]
        (let [e (thrown #(decision/record! (context "slow")))]
          (is (= :clock/store-busy (:error/code (ex-data e))))
          (is (= 3 @reads) "first try plus the two scheduled retries")
          (is (empty? (decision/outbox-entries))))))))

(deftest outbox-replays-when-futon1b-answers-again
  (hold-clock!)
  (let [spooled (with-redefs [store/get-entry* (fn [& _] (throw (refused-connection)))]
                  (decision/record! (context "while-down")))]
    (is (= 1 (count (decision/outbox-entries))))
    (testing "the next decision with futon1b up replays the outbox first"
      (let [next-d (decision/record! (context "after-recovery"))]
        (is (nil? (:persisted-to next-d)))
        (is (contains? (stored-ids) (:decision-id spooled)))
        (is (contains? (stored-ids) (:decision-id next-d)))
        (is (empty? (decision/outbox-entries)))))
    (testing "replaying an already-stored decision is a no-op, not a second copy"
      (with-redefs [store/get-entry* (fn [& _] (throw (refused-connection)))]
        (decision/record! (context "while-down")))
      ;; Same stable id as the first spool: the file is rewritten, then the
      ;; replay meets a duplicate id and clears it.
      (is (= {:replayed 1 :remaining 0} (decision/replay-outbox! *backend*)))
      (is (= 1 (count (filter #{(:decision-id spooled)} (map :evidence/id
                                                            (store/query* *backend* {:query/tags [:clock-decision]})))))))))

(deftest finished-turn-is-not-failed-by-a-down-store
  ;; The F1c-1 case: admitted while futon1b was up, edited files, then
  ;; futon1b went down before the activity decision could be written.
  (let [context (decision/start! agent-id session-id "work" {:turn-id "job-1"
                                                            :surface "bell"})]
    (binding [decision/*turn* context]
      (with-redefs [store/get-entry* (fn [& _] (throw (refused-connection)))
                    boundary/append! (fn [& _] {:ok false :error/code :store-unreachable})]
        (decision/record-tool-use! agent-id session-id
                                   {:name "Edit" :id "tool-1"
                                    :input {:file_path "/home/joe/code/futon3c/README.md"}})
        (is (nil? (thrown #(decision/finish! session-id)))
            "finish! does not turn a down store into a failed job")
        (is (some #(= :activity (get-in % [:evidence/body :phase]))
                  (decision/outbox-entries)))))))
