(ns futon3c.inbox-zero.compensating-batch-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3.inbox-zero.watcher :as watcher]
            [futon3c.inbox-zero.batch-dispatch :as batch]
            [futon3c.inbox-zero.compensating-commit :as comp]
            [futon3c.inbox-zero.compensating-commit-test :refer [git!]])
  (:import [java.nio.file Files] [java.nio.file.attribute FileAttribute]
           [java.time Instant] [java.util Date]))

(deftest ^:slow dispatched-reviewed-batch-commits-and-pushes
  (let [dir (.toFile (Files/createTempDirectory "inbox-zero-batch-test-" (make-array FileAttribute 0)))
        root (str dir "/demo") remote (str dir "/remote.git")
        state-path (str dir "/state.edn") ledger-path (str dir "/ledger.edn")
        paths (mapv #(str "notes/" % ".md") (range 10))]
    (try
      (.mkdir (io/file root))
      (.mkdir (io/file remote))
      (git! root "init" "-q" "-b" "main")
      (git! remote "init" "--bare" "-q")
      (git! root "config" "user.name" "Batch fixture")
      (git! root "config" "user.email" "batch@example.invalid")
      (doseq [path paths] (io/make-parents root path) (spit (io/file root path) "base\n"))
      (git! root "add" "notes")
      (git! root "commit" "-qm" "base")
      (git! root "remote" "add" "origin" remote)
      (git! root "push" "-qu" "origin" "main")
      (doseq [path paths] (spit (io/file root path) "reviewed fixture work\n"))
      (let [old (Date/from (.minusSeconds (Instant/now) (* 48 3600)))
            observations (watcher/observe-repo {:records {}} {:path root :label "demo"} old)
            state {:schema/version 0 :records (into {} (map (juxt :observation/id identity)) observations)}
            _ (spit state-path (pr-str state))
            r (batch/execute-batch!
               {:safety :compensating :state-path state-path :ledger-path ledger-path
                :dispatch {:id "fixture-dispatch" :operator "fixture" :message "package and push fixture"
                           :source "test:operator-cue" :action :package-commit-and-push}
                :reviews {[root paths] {:message "Document reviewed fixture notes" :rationale "Test fixture review"
                                        :blobs (into {} (map #(vector % (comp/blob-id root %))) paths)
                                        :gates [{:gate/name :whitespace :cmd ["git" "diff" "--check"]}]}}})]
        (is (= [1 0] [(:flags/before r) (:flags/after r)]) (pr-str r))
        (is (= 1 (count (:committed r))))
        (is (= 1 (count (:pushed r))))
        (is (empty? (:held r)))
        (is (= (git! root "rev-parse" "HEAD") (git! remote "rev-parse" "refs/heads/main")))
        (is (= "" (git! root "status" "--porcelain")))
        (is (= 5000 (get-in r [:assumptions :detection/response-budget-ms])))
        (is (= (pr-str state) (slurp state-path))))
      (finally (doseq [p (reverse (file-seq dir))] (io/delete-file p))))))
