(ns futon3c.apm.apparatus-pin-test
  (:require [clojure.java.shell :as shell]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.apparatus-pin :as pin])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn temp-repository []
  (str (Files/createTempDirectory "apparatus-pin-"
                                  (make-array FileAttribute 0))))

(defn git [repository & args]
  (apply shell/sh (concat ["git" "-C" repository] args)))

(defn write-file! [repository path content]
  (let [file (java.io.File. repository path)]
    (.mkdirs (.getParentFile file))
    (spit file content)))

(defn fixture []
  (let [repository (temp-repository)]
    (git repository "init" "-q" "-b" "apparatus/v1")
    (git repository "config" "user.email" "apparatus@example.test")
    (git repository "config" "user.name" "Apparatus Test")
    (write-file! repository "cards/student.md" "frozen student\n")
    (git repository "add" "cards/student.md")
    (git repository "commit" "-q" "-m" "freeze apparatus")
    (let [revision (str/trim
                    (:out (git repository "rev-parse" "HEAD")))
          blob (str/trim
                (:out (git repository "rev-parse"
                           (str revision ":cards/student.md"))))]
      {:repository repository
       :apparatus {:repository repository :branch "apparatus/v1"
                   :revision revision
                   :role-cards {:student {:path "cards/student.md"
                                          :blob blob}}}})))

(deftest advancing-and-dirtying-the-developer-worktree-does-not-change-the-pin
  (let [{:keys [repository apparatus]} (fixture)
        before (pin/validate apparatus)]
    (write-file! repository "developer-note.txt" "uncommitted and irrelevant\n")
    (write-file! repository "cards/student.md" "later card generation\n")
    (git repository "add" "cards/student.md")
    (git repository "commit" "-q" "-m" "advance apparatus branch")
    (let [after (pin/validate apparatus)]
      (is (:valid? before) (pr-str before))
      (is (:valid? after) (pr-str after))
      (is (false? (:worktree-consulted? after)))
      (is (not= (:revision apparatus) (:branch-head after)))
      (is (= (get-in before [:artifacts 0 :observed-blob])
             (get-in after [:artifacts 0 :observed-blob]))))))

(deftest blob-mismatch-and-unrelated-branch-fail-closed
  (let [{:keys [repository apparatus]} (fixture)
        wrong-blob (assoc-in apparatus [:role-cards :student :blob]
                             "0000000000000000000000000000000000000000")]
    (is (= [:apparatus-artifact-blob-mismatch]
           (:findings (pin/validate wrong-blob))))
    (git repository "checkout" "-q" "--orphan" "unrelated")
    (doseq [file (reverse (file-seq (java.io.File. repository "cards")))]
      (when (.isFile file) (.delete file)))
    (write-file! repository "other.txt" "other root\n")
    (git repository "add" "-A")
    (git repository "commit" "-q" "-m" "unrelated root")
    (is (= [:apparatus-commit-not-on-branch]
           (:findings (pin/validate (assoc apparatus :branch "unrelated")))))))

(deftest frame-18-apparatus-pin-resolves-from-git-objects
  (let [control (edn/read-string
                 (slurp "holes/labs/M-apm-demonstration/frame-18-control.edn"))
        result (pin/validate (:frame/apparatus control))]
    (is (:valid? result) (pr-str result))
    (is (= 6 (count (:artifacts result))))
    (is (every? :matches? (:artifacts result)))))
