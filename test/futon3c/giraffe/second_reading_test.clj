(ns futon3c.giraffe.second-reading-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.giraffe.corpus-generation :as generation]
            [futon3c.giraffe.second-reading :as second])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- fixture []
  (let [root (.toFile (Files/createTempDirectory "second-reading-" (make-array FileAttribute 0)))
        generations (doto (io/file root "generations") .mkdirs)
        base (doto (io/file generations "base") .mkdirs)
        pointer (io/file root "corpus") source (io/file root "T-one.md")]
    (spit source "depends on M-real\ncriterion not met\n")
    (let [pin {:path (.getAbsolutePath source) :sha256 (generation/file-sha source)}
          criterion {:raw_target "criterion not met" :kind "depends-on" :basis "inferred"
                     :reason "own completion criterion"
                     :evidence (assoc pin :line 2 :quote "criterion not met")}
          genuine {:raw_target "M-real" :kind "depends-on" :basis "explicit"
                   :reason "genuine prerequisite"
                   :evidence (assoc pin :line 1 :quote "depends on M-real")}
          record {:id "T-one" :source pin :status_as_read "open" :purpose "test"
                  :edges [] :unresolved [criterion genuine]}]
      (spit (io/file base "members.json")
            (json/generate-string [(assoc pin :id "T-one")]))
      (spit (io/file base "batch-00.jsonl") (str (json/generate-string record) "\n"))
      (spit (io/file base "check_edges.py")
            (str "#!/usr/bin/env python3\n# EDGE = {kind, target, basis, evidence=EVIDENCE}\n"
                 "# UNRESOLVED = {raw_target, kind, basis, evidence=EVIDENCE, reason}\n"
                 "# EVIDENCE = {path, sha256, line, quote}; quote occurs in line..line+5}\n"
                 "KINDS = {\"depends-on\"}\nBASES = {\"explicit\", \"inferred\"}\nSTATUS = {\"open\"}\n"
                 "import sys; sys.exit(0)\n"))
      (Files/createSymbolicLink (.toPath pointer) (.toPath (.getAbsoluteFile base))
                                (make-array FileAttribute 0))
      {:root root :generations generations :pointer pointer :pin pin
       :record record :criterion criterion :genuine genuine})))

(deftest prompt-explains-defect-and-preserves-real-links
  (let [{:keys [pointer]} (fixture)
        {:keys [member record checker]} (second/current-member-and-record pointer "T-one")
        prompt (second/repair-prompt "T-one" member record checker)]
    (is (str/includes? prompt "own unmet completion criteria"))
    (is (str/includes? prompt (json/generate-string record)))
    (is (str/includes? prompt "do not drop real prerequisites"))))

(deftest repair-publishes-and-logs-one-removed-one-kept
  (let [{:keys [root pointer generations record criterion genuine]} (fixture)
        outcomes (io/file root "outcomes.jsonl")
        corrected (assoc record :unresolved [genuine])
        contact (fn [_seat _request _prompt] {:ok true :record corrected})
        result (second/process-one! {:pointer pointer :generations-dir generations
                                     :contact-fn contact :outcomes-file outcomes}
                                    "kimi-2" "T-one")
        logged (json/parse-string (str/trim (slurp outcomes)) true)
        current (:record (second/current-member-and-record pointer "T-one"))]
    (is (= "published" (:publication_status result)))
    (is (= 1 (:after_unresolved result)))
    (is (= [{:raw_target (:raw_target criterion) :reason (:reason criterion)}]
           (:removed result)))
    (is (= [genuine] (:unresolved current)))
    (is (= (:removed result) (:removed logged)))))
