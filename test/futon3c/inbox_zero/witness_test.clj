(ns futon3c.inbox-zero.witness-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.test :refer [deftest is testing]]
            [futon3c.inbox-zero.witness :as witness])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.util Date]))

(defn- temp-dir []
  (.toFile (Files/createTempDirectory "inbox-zero-witness-"
                                      (make-array FileAttribute 0))))

(deftest successful-edit-publishes-exact-seat-and-claim
  (let [repo (temp-dir)
        intake (temp-dir)
        file (io/file repo "src/a.clj")
        at (Date. 1234)]
    (.mkdirs (.getParentFile file))
    (spit file "(+ 1 2)\n")
    (shell/sh "git" "-C" (.getPath repo) "init" "-q")
    (let [result (witness/publish-successful-edit!
                  {:witness-dir (.getPath intake)
                   :agent-id "claude-13" :session-id "session-exact"
                   :tool-detail {:id "tool-1" :name "Edit"
                                 :input {:file_path (.getPath file)}}
                   :observed-at at :host-id "test-host"})
          records (map #(edn/read-string (slurp %)) (.listFiles intake))]
      (is (= 2 (count records)))
      (is (= "seat:claude-13:session-exact" (get-in result [:claim :seat/id])))
      (is (= "src/a.clj" (get-in result [:claim :path])))
      (is (= :tool-edit (get-in result [:claim :witness/type])))
      ;; Replaying the same successful result is idempotent.
      (witness/publish-successful-edit!
       {:witness-dir (.getPath intake)
        :agent-id "claude-13" :session-id "session-exact"
        :tool-detail {:id "tool-1" :name "Edit"
                      :input {:file_path (.getPath file)}}
        :observed-at at :host-id "test-host"})
      (is (= 2 (count (.listFiles intake)))))))

(deftest non-edit-and-non-git-inputs-do-not-forge-claims
  (let [intake (temp-dir)
        plain (io/file (temp-dir) "a.txt")]
    (spit plain "x")
    (testing "read tools are not edit witnesses"
      (is (nil? (witness/publish-successful-edit!
                 {:witness-dir (.getPath intake) :agent-id "a" :session-id "s"
                  :tool-detail {:id "1" :name "Read"
                                :input {:file_path (.getPath plain)}}}))))
    (testing "a named file outside Git cannot join watcher observations"
      (is (nil? (witness/publish-successful-edit!
                 {:witness-dir (.getPath intake) :agent-id "a" :session-id "s"
                  :tool-detail {:id "2" :name "Write"
                                :input {:file_path (.getPath plain)}}}))))
    (is (empty? (.listFiles intake)))))
