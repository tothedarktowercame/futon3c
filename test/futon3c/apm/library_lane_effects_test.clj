(ns futon3c.apm.library-lane-effects-test
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.library-lane-effects :as sut])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-dir [prefix]
  (str (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(defn- delete-tree! [root]
  (doseq [file (reverse (file-seq (io/file root)))] (.delete file)))

(defn- git [repo & args]
  (apply shell/sh (concat ["git" "-C" repo] args)))

(defn- fixture []
  (let [root (temp-dir "library-effects-test-")
        corpus (str (io/file root "corpus"))
        frames (str (io/file root "frames"))]
    (.mkdirs (io/file corpus ".lake"))
    (.mkdirs (io/file frames))
    (spit (io/file corpus "lake-manifest.json") "{}\n")
    (spit (io/file corpus "Main.lean") "theorem fixture : True := by trivial\n")
    (git corpus "init" "-q" "-b" "trunk")
    (git corpus "config" "user.email" "effects@example.test")
    (git corpus "config" "user.name" "Effects Test")
    (git corpus "add" "-A")
    (git corpus "commit" "-q" "-m" "fixture")
    {:root root :corpus corpus :frames frames}))

(defn- agency-stub []
  (let [agents (atom {}) calls (atom [])
        http-fn
        (fn [method url payload]
          (swap! calls conj [method url payload])
          (cond
            (and (= "GET" method) (str/ends-with? url "/api/alpha/agents"))
            {:ok true :http/status 200 :agents @agents}

            (and (= "POST" method) (str/ends-with? url "/api/alpha/agents/restore"))
            (let [id (:agent-id payload)]
              (swap! agents assoc id
                     {:type :codex :invoke-ready? true
                      :metadata (:metadata payload)})
              {:ok true :http/status 201 :agent-id id})

            :else {:ok false :http/status 404}))]
    {:agents agents :calls calls :http-fn http-fn}))

(defn- effects [fixture agency & [run-fn]]
  (sut/live-effects
   (cond-> {:agency-base "http://agency.test"
            :corpus-root (:corpus fixture) :frames-root (:frames fixture)
            :http-fn (:http-fn agency)}
     run-fn (assoc :run-fn run-fn))))

(deftest mint-restores-only-the-two-exact-codex-seats
  (let [fixture (fixture) agency (agency-stub)]
    (try
      (let [live (effects fixture agency)
            mint (:mint-fn live)
            timeouts {:turn-timeout-ms 3600000}
            result (mint "f9001" {:solver :codex :proctor :codex} timeouts)
            restores (filter #(str/ends-with? (second %) "/agents/restore")
                             @(:calls agency))]
        (is (= #{:observe-problem-fn :provision-fn :validate-workspace-fn
                 :workspace-exists? :leases-fn :roster-fn :mint-fn
                 :occupied-frame-ids :outcome-fn}
               (set (keys live))))
        (is (:ok result))
        (is (= #{"f9001-solver" "f9001-proctor"}
               (set (map #(get-in % [2 :agent-id]) restores))))
        (is (every? #(= "codex" (get-in % [2 :type])) restores))
        (is (= :seat-mint-shape-refused
               (:error/code (mint "f9001" {:solver :codex} timeouts))))
        (is (= 2 (count restores)) "refused shape issued no restore"))
      (finally (delete-tree! (:root fixture))))))

(deftest roster-is-frame-narrowed-and-fail-closed
  (let [fixture (fixture) agency (agency-stub)]
    (try
      (let [live (effects fixture agency)
            mint (:mint-fn live)
            roster (:roster-fn live)
            timeouts {:turn-timeout-ms 3600000}]
        (mint "f9001" {:solver :codex :proctor :codex} timeouts)
        (swap! (:agents agency) assoc "other-solver"
               {:type :codex :invoke-ready? true :metadata {}})
        (is (= #{:solver :proctor} (set (keys (roster "f9001")))))
        (swap! (:agents agency) dissoc "f9001-proctor")
        (is (= :frame-roster-refused (:error/code (roster "f9001"))))
        (swap! (:agents agency) assoc "f9001-proctor"
               {:type :codex :invoke-ready? false
                :metadata {:effective-timeouts timeouts}})
        (is (= :frame-roster-refused (:error/code (roster "f9001")))))
      (finally (delete-tree! (:root fixture))))))

(deftest occupied-frame-ids-unions-roster-and-disk
  (let [fixture (fixture) agency (agency-stub)]
    (try
      (.mkdirs (io/file (:frames fixture) "f9123"))
      (swap! (:agents agency) assoc "f9456-solver"
             {:type :codex :invoke-ready? true})
      (swap! (:agents agency) assoc "f9789-student"
             {:type :zai :invoke-ready? true})
      (is (= #{"f9123" "f9456" "f9789"}
             (:occupied-frame-ids (effects fixture agency))))
      (is (= :frames-root-unobservable
             (:error/code
              (sut/live-effects {:agency-base "http://agency.test"
                                 :corpus-root (:corpus fixture)
                                 :frames-root (str (io/file (:root fixture) "absent"))
                                 :http-fn (:http-fn agency)}))))
      (let [failed-http (fn [& _] (throw (ex-info "offline" {})))]
        (is (= :occupied-frame-ids-unobservable
               (:error/code
                (sut/live-effects {:agency-base "http://agency.test"
                                   :corpus-root (:corpus fixture)
                                   :frames-root (:frames fixture)
                                   :http-fn failed-http})))))
      (finally (delete-tree! (:root fixture))))))

(deftest occupied-frame-ids-extracts-from-keyword-roster-keys
  ;; The live agency parses JSON to KEYWORD map keys, so the roster arrives as
  ;; {:f21-solver {...}}. A str-based extraction yields ":f21-solver", whose
  ;; leading colon defeats the ^f[0-9]+ anchor, silently producing an empty
  ;; occupied set -- a collision guard that never fires and never complains.
  ;; The other fixtures seed string keys, which is why they did not catch it.
  (let [fixture (fixture) agency (agency-stub)]
    (try
      (swap! (:agents agency) assoc :f21-solver {:type :codex :invoke-ready? true})
      (swap! (:agents agency) assoc :f13-proctor {:type :codex :invoke-ready? true})
      (is (= #{"f21" "f13"} (:occupied-frame-ids (effects fixture agency))))
      (finally (delete-tree! (:root fixture))))))

(deftest provisioned-lease-is-persisted-and-reobserved
  (let [fixture (fixture) agency (agency-stub)]
    (try
      (let [live (effects fixture agency)
            revision (str/trim (:out (git (:corpus fixture) "rev-parse" "HEAD")))
            blob (str/trim (:out (git (:corpus fixture) "rev-parse" "HEAD:Main.lean")))
            unit {:frame/id "f9001" :problem/id "fixture"
                  :problem {:repository (:corpus fixture) :revision revision
                            :blob blob :path "Main.lean"}}
            provisioned ((:provision-fn live) unit :solver)
            lease (:lease provisioned)]
        (is (:ok provisioned) (pr-str provisioned))
        (is (true? ((:workspace-exists? live) unit :solver)))
        (is (= lease (get ((:leases-fn live) unit) :solver)))
        (is (= (:workspace/id lease)
               (:workspace/id (get ((:leases-fn (effects fixture agency)) unit)
                                   :solver))))
        (git (:corpus fixture) "worktree" "remove" "--force"
             (:workspace/path lease)))
      (finally (delete-tree! (:root fixture))))))

(deftest outcome-requires-clean-verify-and-elaboration
  (let [fixture (fixture) agency (agency-stub)
        verify {:receipt/id (apply str (repeat 64 "a"))
                :receipt/final-head (apply str (repeat 40 "b"))
                :receipt/frame-id "f9001" :receipt/problem-id "p"
                :receipt/type :frame-verify
                :receipt/mathematical-sound? true}
        solve {:receipt/final-head (:receipt/final-head verify)}
        frame-dir (io/file (:frames fixture) "f9001")
        _ (.mkdirs frame-dir)
        _ (spit (io/file frame-dir "workspace-solver.edn")
                (pr-str {:frame/id "f9001" :problem/id "p" :role :solver
                         :workspace/path (:corpus fixture)
                         :base-revision (apply str (repeat 40 "c"))}))
        responses (atom [])
        run-fn (fn [_ _] (let [response (first @responses)]
                           (swap! responses subvec 1) response))
        outcome (:outcome-fn (effects fixture agency run-fn))]
    (try
      (is (= :outcome-verify-evidence-missing
             (:error/code (outcome {:problem-id "p" :receipts {}}))))
      (reset! responses [{:exit 0 :out "" :err ""}
                         {:exit 0 :out "" :err ""}
                         {:exit 0 :out "" :err ""}])
      (is (= {:verified-proof? true :remaining-sorries 0}
             (outcome {:problem-id "p" :receipts {:solve solve :verify verify}})))
      (reset! responses [{:exit 0 :out "ConstructionTargets/New.lean\n" :err ""}
                         {:exit 0 :out "" :err ""}
                         {:exit 0 :out ""
                          :err "warning: declaration uses `sorry`\n"}])
      (is (= {:verified-library? true :library-sorry-warnings 0
              :problem-open? true :remaining-sorries 1
              :boundary "problem remains open after verified sorry-free library work"}
             (outcome {:problem-id "p" :receipts {:solve solve :verify verify}})))
      (reset! responses [{:exit 0 :out "ConstructionTargets/New.lean\n" :err ""}
                         {:exit 0 :out ""
                          :err "warning: declaration uses `sorry`\n"}
                         {:exit 0 :out "" :err ""}])
      (is (= :outcome-library-carries-sorry
             (:error/code
              (outcome {:problem-id "p" :receipts {:solve solve
                                                    :verify verify}}))))
      (finally (delete-tree! (:root fixture))))))
