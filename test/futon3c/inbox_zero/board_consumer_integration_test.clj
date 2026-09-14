(ns futon3c.inbox-zero.board-consumer-integration-test
  "Real Git regression in a disposable repository; no live watcher writes."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3.inbox-zero.watcher :as watcher]
            [futon3c.inbox-zero.board-consumer :as consumer])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.time Instant]
           [java.util Date]))

(defn git! [root & args]
  (let [r (apply shell/sh (concat ["git" "-C" root] args))]
    (when-not (zero? (:exit r)) (throw (ex-info "Test Git failed" r)))
    (:out r)))

(deftest ^:slow real-git-cycle-closes-and-cites-proposal
  (let [dir (.toFile (Files/createTempDirectory "inbox-zero-consumer-test-"
                                                (make-array FileAttribute 0)))
        repo (io/file dir "repo")
        root (.getPath repo)
        state-path (str dir "/state.edn")
        ledger-path (str dir "/cycle.edn")]
    (try
      (.mkdir repo)
      (git! root "init" "-q")
      (git! root "config" "user.name" "Inbox zero test")
      (git! root "config" "user.email" "inbox-zero-test@example.invalid")
      (spit (io/file repo "README.md") "base\n")
      (git! root "add" "README.md")
      (git! root "commit" "-qm" "fixture baseline")
      (spit (io/file repo "README.md") "base\ncompleted fixture work\n")
      (let [old (Date/from (.minusSeconds (Instant/now) (* 48 3600)))
            observations (watcher/observe-repo {:records {}} {:path root :label "repo"} old)
            claim {:record/type :inbox-zero/session-file-claim :claim/id "fixture-claim"
                   :seat/id "seat:fixture:session" :repo/id "repo"
                   :worktree/id (:worktree/id (first observations))
                   :path "README.md" :state :active :last-observed-at old}
            state {:schema/version 0
                   :records (assoc (into {} (map (juxt :observation/id identity)) observations)
                                   "fixture-claim" claim)}]
        (spit state-path (pr-str state))
        (let [result (consumer/run-cycle! {:state-path state-path :ledger-path ledger-path
                                           :gate-specs [{:gate/name :whitespace
                                                         :cmd ["git" "diff" "--check"]}]})
              records (mapcat :records (:consumed result))
              witness (first (filter #(= :inbox-zero/commit-witness (:record/type %)) records))
              message (git! root "log" "-1" "--format=%B")]
          (is (= [1 0] [(:flags/before result) (:flags/after result)]))
          (is (= 1 (count (filter #(= :inbox-zero/commit-witness (:record/type %)) records))))
          (is (str/blank? (git! root "status" "--porcelain")))
          (is (= (str/trim (git! root "rev-parse" "HEAD")) (get-in witness [:result :commit/sha])))
          (is (str/includes? message (get-in witness [:certificate :board/digest])))
          (is (str/includes? message (:verbs/digest witness)))
          (is (consumer/verified-proposal? (:before result)))
          (is (consumer/verified-proposal? (:after result)))
          (is (= (pr-str state) (slurp state-path)) "consumer never writes the watcher snapshot")))
      (finally
        (doseq [file (reverse (file-seq dir))] (io/delete-file file))))))
