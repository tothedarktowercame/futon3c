(ns futon3c.apm.library-lane-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.library-lane :as lane]
            [futon3c.apm.library-lane-runner :as runner])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-dir [prefix]
  (str (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(defn- delete-tree! [root]
  (doseq [file (reverse (file-seq (io/file root)))] (.delete file)))

(defn- write! [root path content]
  (let [file (io/file root path)]
    (.mkdirs (.getParentFile file))
    (spit file content)))

(defn- problem! [root id status lean?]
  (write! root (str "problems/" id "/status.json")
          (json/generate-string status))
  (when lean?
    (write! root (str "problems/" id "/lean/Main.lean")
            "theorem fixture : True := by trivial\n")))

(deftest lanes-are-live-status-derived-and-ordered
  (let [root (temp-dir "library-lane-corpus-")]
    (try
      (problem! root "lib-z" {:classification "partial"
                               :sorry_count_total 2
                               :note "infrastructure missing from Mathlib"} true)
      (problem! root "lib-a" {:classification "partial"
                               :sorry_count_total 1
                               :note "library infrastructure absent from the repo"} true)
      (problem! root "done" {:classification "closed"
                              :sorry_count_total 0
                              :note "infrastructure missing"} true)
      (problem! root "repair" {:classification "invalid-statement"} true)
      (problem! root "formalize" {:classification "partial"} false)
      ;; A deliberately contradictory snapshot proves it is not consulted.
      (write! root "Reports/apm-corpus-lanes-v1.edn"
              "{\"done\" :library, \"lib-a\" :done}")
      (is (= {"done" :done "formalize" :formalize "lib-a" :library
              "lib-z" :library "repair" :repair}
             (lane/lanes root)))
      (is (= ["lib-a" "lib-z"] (lane/queue root :library)))
      (is (not-any? #{"done"} (lane/queue root :library)))
      (finally (delete-tree! root)))))

(deftest targets-come-from-elaborated-sorry-locations
  (let [root (temp-dir "library-target-corpus-")
        path "problems/p/lean/Main.lean"]
    (try
      (write! root path
              (str "theorem ordinary : True := by\n  sorry\n\n"
                   "theorem p_bridge_1 : True := by\n  sorry\n"))
      (let [run-fn (fn [_ _]
                     {:exit 0 :out ""
                      :err (str path ":2:3: warning: declaration uses `sorry`\n"
                                path ":5:3: warning: declaration uses `sorry`\n")})]
        (is (= {:ok true :targets ["ordinary" "p_bridge_1"] :warning-count 2}
               (runner/elaborate-targets
                {:corpus-root root :problem-id "p" :run-fn run-fn}))))
      (finally (delete-tree! root)))))

(defn- git [repo & args]
  (apply shell/sh (concat ["git" "-C" repo] args)))

(defn- runner-fixture []
  (let [root (temp-dir "library-runner-corpus-")]
    (problem! root "fixture" {:classification "partial" :sorry_count_total 1
                               :note "infrastructure absent from Mathlib"} true)
    (git root "init" "-q" "-b" "trunk")
    (git root "config" "user.email" "runner@example.test")
    (git root "config" "user.name" "Runner Test")
    (git root "add" "-A")
    (git root "commit" "-q" "-m" "fixture")
    root))

(defn- head [repo]
  (str/trim (:out (git repo "rev-parse" "trunk"))))

(defn- phase-inputs [{:keys [kind]}] {:ok true :kind kind})

(defn- successful-phase [{:keys [kind]}]
  {:ok true :certificate {:receipt/id (str "receipt-" (name kind))
                          :receipt/final-head (apply str (repeat 40 "a"))}})

(deftest run-one-closes-and-is-rerunnable
  (let [root (runner-fixture)
        calls (atom [])
        options {:corpus-root root :trunk-branch "trunk" :contract {}
                 :seat {} :phase-inputs-fn phase-inputs
                 :target-fn (constantly {:ok true :targets ["fixture"]})
                 :phase-run-fn (fn [input] (swap! calls conj (:kind input))
                                 (successful-phase input))
                 :bank-request-fn (constantly {:ok true :ruling :closed})
                 :bank-fn (fn [_] {:ok true
                                   :receipt {:receipt/ruling :closed}})}]
    (try
      (is (= :closed (:ruling (runner/run-one! options))))
      (is (= :closed (:ruling (runner/run-one! options))))
      (is (= [:preflight :solve :verify :preflight :solve :verify] @calls))
      (finally (delete-tree! root)))))

(deftest verify-failure-is-blocked-and-trunk-unmoved
  (let [root (runner-fixture)
        before (head root)
        result (runner/run-one!
                {:corpus-root root :trunk-branch "trunk" :contract {} :seat {}
                 :phase-inputs-fn phase-inputs
                 :target-fn (constantly {:ok true :targets ["fixture"]})
                 :phase-run-fn (fn [{:keys [kind] :as input}]
                                 (if (= :verify kind)
                                   {:ok false :error/code :fixture-verify-failed}
                                   (successful-phase input)))
                 :bank-request-fn (constantly {:ok true})
                 :bank-fn (fn [_] (throw (ex-info "bank must not run" {})))})]
    (try
      (is (= :blocked (:ruling result)))
      (is (= :verify (:seam result)))
      (is (= :fixture-verify-failed (get-in result [:finding :error/code])))
      (is (= before (head root)))
      (finally (delete-tree! root)))))

(deftest missing-runtime-adapters-refuse-exactly
  (let [root (runner-fixture)]
    (try
      (is (= {:ok false :ruling :blocked :problem-id "fixture"
              :seam :configuration :finding :phase-inputs-fn-missing}
             (runner/run-one! {:corpus-root root})))
      (finally (delete-tree! root)))))

(deftest real-corpus-lanes-conform-read-only
  (let [root (io/file "/home/joe/code/apm-lean")]
    (if-not (.isDirectory root)
      (is true "real APM corpus absent; read-only conformance skipped")
      (let [before (.lastModified root)
            first-lanes (lane/lanes root)
            second-lanes (lane/lanes root)
            first-queue (lane/queue root :library)
            second-queue (lane/queue root :library)]
        (is (seq first-queue))
        (is (every? #{:done :repair :formalize :library :standard}
                    (vals first-lanes)))
        (is (= first-lanes second-lanes))
        (is (= first-queue second-queue))
        (is (= before (.lastModified root))
            "lane derivation did not mutate the corpus root")))))
