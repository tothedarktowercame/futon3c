(ns futon3c.apm.library-lane-adapters-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.apm.library-lane-adapters :as sut]))

(def sha40 (apply str (repeat 40 "a")))
(def sha64 (apply str (repeat 64 "b")))
(def problem-id "a00J01")
(def unit
  {:frame/id "f9001" :problem/id problem-id
   :problem {:repository "/tmp/fixture-corpus" :branch "trunk"
             :revision sha40 :path "problems/a00J01/lean/Main.lean"
             :blob sha40}})
(def workspace
  {:workspace/path "/tmp/fixture-workspace" :branch "exp/library-a00J01"
   :base-revision sha40 :problem/blob sha40})
(def role-card
  {:path "holes/labs/M-apm-demonstration/role-cards/codex-solver-library-v1.md"
   :blob "a03d58e9fb261fb78b1ee90d9e497d395e4f1dd2"})
(def ledger {:version 5 :digest sha64 :phase :preflight :claim nil})
(def seats
  {:solver {:agent-id "f9001-solver" :type :codex :frame-id "f9001"
            :invoke-ready? true
            :effective-timeouts {:turn-timeout-ms 3600000}}
   :proctor {:agent-id "f9001-proctor" :type :codex :frame-id "f9001"
             :invoke-ready? true
             :effective-timeouts {:turn-timeout-ms 3600000}}})
(def actions
  {:preflight {:frame-id "f9001" :problem-id problem-id
               :timeouts {:request-ms 300000 :turn-ms 3600000}}
   :solve {:frame-id "f9001" :problem-id problem-id
           :timeouts {:turn-ms 3600000}}
   :verify {:frame-id "f9001" :problem-id problem-id
            :timeouts {:turn-ms 3600000}}})

(deftest codex-preparation-binds-only-the-honest-cast
  (let [minted (atom nil)
        lease {:workspace/id "workspace-solver" :frame/id "f9001"
               :problem/id problem-id :role :solver}
        result (sut/prepare-codex-only!
                {:unit unit :ledger ledger
                 :role-cards {:solver role-card :proctor role-card}
                 :leases {}
                 :workspace-exists? (constantly false)
                 :provision-fn (fn [_ role]
                                 (is (= :solver role))
                                 {:ok true :lease lease})
                 :validate-workspace-fn (constantly {:valid? true :findings []})
                 :mint-fn (fn [_ seat-types _]
                            (reset! minted seat-types) {:ok true})
                 :roster-fn (constantly seats)})]
    (is (:ok result))
    (is (= {:solver :codex :proctor :codex} @minted))
    (is (= #{:solver} (set (keys (get-in result [:receipt :workspace/ids])))))
    (is (= #{:solver :proctor}
           (set (keys (get-in result [:receipt :seat/ids])))))))

(defn phase-adapter
  ([] (phase-adapter {}))
  ([overrides]
   (sut/make-phase-inputs-fn
    (merge {:unit unit :ledger ledger :workspace workspace :seats seats
            :actions actions
            :state-paths {:preflight "/tmp/preflight.edn"
                          :solve "/tmp/solve.edn" :verify "/tmp/verify.edn"}
            :agency-base "http://agency.test"}
           overrides))))

(deftest phase-adapter-builds-every-live-input-with-threaded-authority
  (let [adapter (phase-adapter)
        solve-receipt {:receipt/id "solve-id" :receipt/final-head sha40}]
    (doseq [kind [:preflight :solve :verify]]
      (let [result (adapter {:kind kind :problem-id problem-id
                             :role-card role-card :contract {:contract/id :fixture}
                             :receipts (if (= :verify kind)
                                         {:solve solve-receipt} {})})]
        (is (:ok result) (name kind))
        (is (= kind (:kind result)))
        (is (= "http://agency.test" (:agency-base result)))
        (is (= (str "/tmp/" (name kind) ".edn") (:state-path result)))
        (is (= (if (= :solve kind) "f9001-solver" "f9001-proctor")
               (get-in result [:request :agent-id])))))))

(deftest phase-adapter-does-not-repair-missing-authority
  (let [result ((phase-adapter {:seats (dissoc seats :proctor)})
                {:kind :verify :problem-id problem-id :role-card role-card
                 :contract {} :receipts {:solve {:receipt/id "solve"}}})]
    (is (= :library-phase-authority-invalid (:error/code result)))
    (is (some #{:registered-seat-missing} (:findings result)))))

(defn bank-adapter [outcome]
  (sut/make-bank-request-fn
   {:unit unit :workspace workspace :trunk-branch "trunk"
    :keying-target "apm_a00j01" :outcome-fn (constantly outcome)}))

(def receipts
  {:solve {:receipt/final-head sha40}
   :verify {:receipt/id sha64 :receipt/final-head sha40}})

(deftest bank-adapter-selects-ruling-from-terminal-evidence
  (testing "a verified closed proof"
    (let [result ((bank-adapter {:verified-proof? true :remaining-sorries 0})
                  {:problem-id problem-id :receipts receipts})]
      (is (:ok result))
      (is (= :closed (:ruling result)))
      (is (= ["lake" "build" "ConstructionTargets"] (:rollup-command result)))
      (is (= ["lake" "env" "lean" "problems/a00J01/lean/Main.lean"]
             (:status-command result)))
      (is (re-find #"#print axioms apm_a00j01"
                   (last (:axiom-command result))))))
  (testing "verified reusable library with the problem still open"
    (let [result ((bank-adapter {:verified-library? true
                                 :library-sorry-warnings 0
                                 :problem-open? true
                                 :boundary "the final consumer seam remains"})
                  {:problem-id problem-id :receipts receipts})]
      (is (:ok result))
      (is (= :partial-banked (:ruling result)))
      (is (= "the final consumer seam remains" (:receipt/boundary result)))))
  (testing "optimism is not evidence"
    (is (= :bank-ruling-evidence-insufficient
           (:error/code
            ((bank-adapter {:verified-proof? true :remaining-sorries 1})
             {:problem-id problem-id :receipts receipts}))))))

(deftest library-axiom-script-builds-only-lean-modules
  (let [script (sut/library-axiom-script
                ["ConstructionTargets/DiskBoundaryHomology.lean"
                 "ConstructionTargets/DiskBoundaryHomology.md"])]
    (is (str/includes? script "ConstructionTargets.DiskBoundaryHomology"))
    (is (not (str/includes? script "ConstructionTargets.DiskBoundaryHomology.md")))))

(deftest content-addressed-frame-ids-refuse-observed-collisions
  (let [first-id (sut/codex-frame-id problem-id sha40 #{})]
    (is (:ok first-id))
    (is (re-matches (re-pattern (str "f9[0-9]{" sut/frame-id-digits "}"))
                     (:frame-id first-id)))
    (is (= :occupied-frame-id-set-required
           (:error/code (sut/codex-frame-id problem-id sha40 nil))))
    (is (= :codex-frame-id-collision
           (:error/code (sut/codex-frame-id problem-id sha40
                                             #{(:frame-id first-id)}))))))
