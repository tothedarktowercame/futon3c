(ns futon3c.apm.library-lane-launch-test
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.test :refer [deftest is testing]]
            [futon3c.apm.library-lane-adapters :as adapters]
            [futon3c.apm.library-lane-launch :as sut]
            [futon3c.apm.library-lane-runner :as runner]
            [futon3c.apm.workspace-lifecycle :as workspace])
  (:import [java.nio.file Files LinkOption]
           [java.nio.file.attribute FileAttribute]))

(def problem-id "fixture01")

(defn- temp-dir [prefix]
  (str (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(defn- sh [dir & argv]
  (apply shell/sh (concat (map str argv) [:dir (str dir)])))

(defn- git [repo & argv]
  (apply sh repo "git" "-C" repo argv))

(defn- write! [root path content]
  (let [file (io/file root path)]
    (.mkdirs (.getParentFile file))
    (spit file content)))

(defn- delete-tree! [root]
  (doseq [file (reverse (file-seq (io/file root)))] (.delete file)))

(defn- fixture []
  (let [root (temp-dir "lane-launch-fixture-")
        repo (str (io/file root "corpus"))
        workspace-root (str (io/file root "workspaces"))
        substrate (str (io/file root "substrate" ".lake"))
        state-root (str (io/file root "state"))]
    (.mkdirs (io/file repo))
    (.mkdirs (io/file workspace-root))
    (.mkdirs (io/file substrate))
    (write! root "substrate/lake-manifest.json" "{}\n")
    (write! repo (str "problems/" problem-id "/status.json")
            (json/generate-string {:schema "apm-problem-bundle.v1"
                                   :problem_id problem-id
                                   :classification "partial"}))
    (write! repo (str "problems/" problem-id "/lean/Main.lean")
            "theorem fixture_target : True := by\n  sorry\n")
    (write! repo ".gitignore" ".lake\n")
    (git repo "init" "-q" "-b" "trunk")
    (git repo "config" "user.email" "lane-launch@example.test")
    (git repo "config" "user.name" "Lane Launch Test")
    (git repo "add" "-A")
    (git repo "commit" "-q" "-m" "fixture corpus")
    {:root root :repo repo :workspace-root workspace-root
     :substrate substrate :state-root state-root}))

(defn- effects [{:keys [workspace-root substrate]}]
  (let [leases (atom {})
        seats (atom {})
        provisions (atom 0)]
    {:leases leases :seats seats :provisions provisions
     :options
     {:observe-problem-fn sut/observe-problem
      :leases-fn (fn [_] @leases)
      :workspace-exists? (fn [_ role] (contains? @leases role))
      :provision-fn
      (fn [unit role]
        (swap! provisions inc)
        (let [result (workspace/provision!
                      {:unit unit :role role :workspace-root workspace-root
                       :substrate-path substrate
                       :now "2026-08-22T12:00:00Z"})]
          (when (:ok result) (swap! leases assoc role (:lease result)))
          result))
      :validate-workspace-fn
      (fn [lease] (workspace/validate lease {:probe-fn (constantly {:exit 0})}))
      :mint-fn
      (fn [frame-id seat-types timeouts solver-assignment-id]
        (reset! seats
                (into {}
                      (map (fn [[role type]]
                             [role {:agent-id (if (= :solver role)
                                                solver-assignment-id
                                                (str frame-id "-" (name role)))
                                    :type type :frame-id frame-id
                                    :invoke-ready? true
                                    :effective-timeouts timeouts}]))
                      seat-types))
        {:ok true})
      :roster-fn (fn [_ _] @seats)
      :outcome-fn (constantly {:verified-proof? true
                               :remaining-sorries 0})}}))

(defn- launch-options [fixture effects]
  (merge (:options effects)
         {:corpus-root (:repo fixture) :problem-id problem-id
          :control-root (System/getProperty "user.dir")
          :trunk-branch "trunk" :keying-target "fixture_target"
          :state-root (:state-root fixture) :agency-base "http://agency.test"
          :occupied-frame-ids #{}}))

(def contract
  (edn/read-string
   (slurp "holes/labs/M-apm-demonstration/frame-cycle-contract-codex-only-v1.edn")))

(deftest solver-assignments-are-stable-per-problem-and-isolated-between-problems
  (is (= (sut/problem-solver-assignment-id "t00J02")
         (sut/problem-solver-assignment-id "t00J02")))
  (is (not= (sut/problem-solver-assignment-id "t00J02")
            (sut/problem-solver-assignment-id "m94A03"))))

(deftest launch-composes-authority-accepted-by-all-live-phases
  (let [fixture (fixture)
        effects (effects fixture)]
    (try
      (let [first-launch (sut/launch! (launch-options fixture effects))
            config (:config first-launch)
            phase-inputs (adapters/make-phase-inputs-fn config)
            solve-receipt {:receipt/id "solve-receipt"
                           :receipt/final-head (apply str (repeat 40 "a"))}
            verify-receipt {:receipt/id (apply str (repeat 64 "b"))
                            :receipt/final-head (apply str (repeat 40 "a"))}]
        (is (:ok first-launch) (pr-str first-launch))
        (testing "the persisted config rehydrates tagged Path authority"
          (let [resumed (sut/resume-config
                         {:state-root (:state-root fixture)
                          :problem-id problem-id
                          :revision (get-in config [:unit :problem :revision])
                          :outcome-fn (:outcome-fn config)})]
            (is (= (dissoc config :outcome-fn)
                   (dissoc resumed :outcome-fn)))
            (is (every? #(instance? java.nio.file.Path %)
                        (vals (:state-paths resumed))))))
        (doseq [kind [:preflight :solve :verify]]
          (let [result (phase-inputs
                        {:kind kind :problem-id problem-id
                         :role-card runner/library-card :contract contract
                         :checkpoint-role-card (when (= :solve kind)
                                                 runner/solver-restrategize-card)
                         :receipts (if (= :verify kind)
                                     {:solve solve-receipt} {})})]
            (is (:ok result) (str (name kind) ": " (pr-str result)))
            (is (= kind (get-in result [:request :phase])))))
        (let [bank-request ((adapters/make-bank-request-fn config)
                            {:problem-id problem-id
                             :receipts {:solve solve-receipt
                                        :verify verify-receipt}})]
          (is (:ok bank-request) (pr-str bank-request))
          (is (= (:repo fixture) (:repository bank-request)))
          (is (= "trunk" (:trunk-branch bank-request))))
        (testing "a repeated launch reuses the lease and frame"
          (let [second-launch (sut/launch! (launch-options fixture effects))]
            (is (:ok second-launch))
            (is (= (get-in first-launch [:config :unit :frame/id])
                   (get-in second-launch [:config :unit :frame/id])))
            (is (= (get-in first-launch [:config :workspace :workspace/id])
                   (get-in second-launch [:config :workspace :workspace/id])))
            (is (= 1 @(:provisions effects)))))
        (testing "an id occupied by our OWN prior lease is reuse, not collision"
          ;; The frame id is content-addressed on [problem-id revision], so a
          ;; retry always finds its own previous seats in the occupied set.
          ;; Refusing that would make launch! single-shot in reality while
          ;; passing every fixture.
          (let [frame-id (get-in first-launch [:config :unit :frame/id])
                reuse (sut/launch!
                       (assoc (launch-options fixture effects)
                              :occupied-frame-ids #{frame-id}))]
            (is (:ok reuse) (pr-str reuse))
            (is (= frame-id (get-in reuse [:config :unit :frame/id])))))
        (testing "an id occupied with NO lease of ours is a foreign collision"
          (let [frame-id (get-in first-launch [:config :unit :frame/id])
                collision (sut/launch!
                           (assoc (launch-options fixture effects)
                                  :leases-fn (constantly {})
                                  :occupied-frame-ids #{frame-id}))]
            (is (= :library-lane-frame-id-refused (:error/code collision)))
            (is (= :codex-frame-id-collision
                   (get-in collision [:findings 0 :error/code])))))
        (is (= #{:unit :ledger :workspace :seats :actions :state-paths :control-root
                 :agency-base :trunk-branch :keying-target :outcome-fn
                 :solver-assignment-id}
               (set (keys config))))
        (is (= (str "library-" problem-id "-solver")
               (:solver-assignment-id config)
               (get-in config [:seats :solver :agent-id]))))
      (finally
        (doseq [[_ lease] @(:leases effects)]
          (when (Files/exists (java.nio.file.Path/of (:workspace/path lease)
                                                     (make-array String 0))
                              (make-array LinkOption 0))
            (git (:repo fixture) "worktree" "remove" "--force"
                 (:workspace/path lease))))
        (delete-tree! (:root fixture))))))

(deftest launch-refuses-a-missing-problem-bundle-with-a-typed-finding
  (let [fixture (fixture)
        effects (effects fixture)]
    (try
      (let [result (sut/launch!
                    (assoc (launch-options fixture effects)
                           :problem-id "missing"))]
        (is (= :library-lane-problem-observation-failed (:error/code result)))
        (is (= :problem-bundle-missing
               (get-in result [:findings 0 :error/code])))
        (is (= :status-json-missing
               (get-in result [:findings 0 :findings 0 :finding]))))
      (finally (delete-tree! (:root fixture))))))
