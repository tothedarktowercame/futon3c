(ns futon3c.xiang.bell-turns-test
  "E-agency-work-orders W1: a work bell between registered agents as a 象
   turn — one :ask-action act opens at the bell, the recipient's marked
   reply closes it.

   Fixture: the claude-4 -> codex-23 c1-capture bell
   (invoke-1791152410965-32287-c9db2bca, 2026-10-04T22:20Z) and its
   completion, copied unmodified from /tmp/futon3c-invoke-jobs.edn into
   bell_fixtures/c1_capture.edn. Note what the ledger actually holds:
   codex-23's immediate result (\"Waiting on `bg-...`\") carries no
   proforma mark, so the port honestly stays open at the first
   completion. The marked ㊢ text in the fixture's :completion-bell is NOT
   codex-23's: it is claude-4's turn on receiving the auto-bellback. It is
   used below only as a stand-in for a marked recipient reply, to test the
   adapter; live, the port closes only if the recipient's own result is
   marked. Codex agents do not write proforma marks, so their bells stay
   open (reported to Joe 2026-10-05)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.logic.xiang :as lx]
            [futon3c.xiang.turn-acts :as ta]
            [futon3c.xiang.turn-service :as svc]
            [futon3c.xiang.turn-store :as ts]))

(def ^:private fixture
  (edn/read-string (slurp (io/file "test/futon3c/xiang/bell_fixtures/c1_capture.edn"))))

(defn- temp-store []
  (ts/store (str (java.nio.file.Files/createTempDirectory
                  "xiang-bells-" (make-array java.nio.file.attribute.FileAttribute 0)))))

(defn- harness [& [overrides]]
  (let [clock (atom 1791152410000)
        s (svc/service (merge {:store (temp-store)
                               :now-ms (fn [] @clock)
                               :schedule! (fn [_ thunk] (thunk))
                               :evidence-async! (fn [thunk] (thunk))
                               :draft-async! (fn [thunk] (thunk))}
                              overrides))]
    {:svc s :clock clock}))

(def ^:private bell-opts
  {:dispatch :none
   :origin "agent"
   :agent-id (:agent-id fixture)
   :session-id "pending"
   :turn-id (:job-id fixture)
   :text (:prompt fixture)
   :caller (:caller fixture)})

(def ^:private bell-id (str (:job-id fixture) "-b-0"))

(defn- store-of [h] (get-in (:svc h) [:config :store]))

(defn- acts-of [record reply]
  (ta/session-acts [{:record record :reading {} :reply (or reply "") :commits []}]))

(defn- open-ids [acts iso]
  (lx/open-ports (lx/db acts) iso))

(deftest a-work-bell-records-one-open-port
  (let [h (harness)
        {:keys [record dispatch]} (svc/record-turn! (:svc h) bell-opts)]
    (is (= :declared dispatch) "no reading, no LLM")
    (is (= "declared" (:analysis_status record)))
    (is (= "claude-4" (:caller record)) "the caller rides on the record")
    (is (= (:job-id fixture) (:turn_id record)))
    (is (= "pending" (:session_id record))
        "placeholder until the recipient's session is known at finalize")
    (is (= "agent" (:origin record)))
    (let [acts (acts-of record nil)
          bell (first (filter #(= bell-id (:id %)) acts))]
      (is (some? bell) "one act for the bell as a whole")
      (is (= :ask-action (:kind bell)))
      (is (= "claude-4" (:author bell)) "author is the caller, not the operator")
      (is (= "codex-23" (:to bell)) "the debtor: the recipient owes the answer")
      (is (= 300 (count (:text bell))) "the prompt's first 300 chars")
      (is (str/starts-with? (:prompt fixture) (:text bell)))
      (is (contains? (open-ids acts (:created_at record)) bell-id)
          "after the bell the port is open"))))

(deftest unmarked-first-reply-keeps-the-port-open
  (let [h (harness)
        {:keys [id record]} (svc/record-turn! (:svc h) bell-opts)
        r (svc/attach-happened! (:svc h) id {:reply (:first-result fixture) :commits []}
                                {:agent-session (fn [_] (:session-id fixture))})]
    (is (= :declared (:reason r)))
    (let [stored (ts/read-record (store-of h) id)]
      (is (= (:session-id fixture) (:session_id stored))
          "the placeholder session is replaced by the recipient's")
      (is (= (:first-result fixture) (:reply_text stored)))
      (let [open (set (map :act (get-in stored [:ports :still_open])))]
        (is (contains? open bell-id)
            "ports computed for a declared agent turn; the bell is still open"))
      (let [acts (acts-of stored (:first-result fixture))]
        (is (contains? (open-ids acts (:created_at record)) bell-id)
            "codex-23's unmarked reply declares no act; nothing closes")))))

(deftest a-marked-report-closes-the-port
  (let [h (harness)
        {:keys [id record]} (svc/record-turn! (:svc h) bell-opts)
        report (get-in fixture [:completion-bell :report])
        _ (svc/attach-happened! (:svc h) id {:reply report :commits []}
                                {:agent-session (fn [_] (:session-id fixture))})
        stored (ts/read-record (store-of h) id)
        acts (acts-of stored report)
        report-act (first (filter #(= (str (:job-id fixture) "-r-0") (:id %)) acts))]
    (is (= :report (:kind report-act)) "㊢ declares a report")
    (is (= bell-id (:target report-act)) "the adapter targets the bell it answers")
    (is (not (contains? (open-ids acts (:created_at record)) bell-id))
        "closed: adjacency [:ask-action :report] -> :closes, via the explicit target")
    (is (empty? (get-in stored [:ports :still_open]))
        "the HUD reads the closure off the record")))

(deftest a-verify-reply-closes-the-port-too
  (let [record {:turn_id "t-bell-1" :origin "agent" :caller "claude-4"
                :agent_id "codex-23" :session_id "sess-1"
                :created_at "2026-10-04T22:20:10Z"
                :source_text "Please run the acceptance probe."}
        acts (ta/turn->acts record {} "㊬ probe output identical with and without capture." [])
        verify-act (first (filter #(= "t-bell-1-r-0" (:id %)) acts))]
    (is (= :verify (:kind verify-act)))
    (is (= "t-bell-1-b-0" (:target verify-act)))
    (is (not (contains? (open-ids acts "2026-10-04T22:20:10Z") "t-bell-1-b-0"))
        (str "no [:ask-action :verify] adjacency entry: the explicit target "
             "closes by answerso's default (the verify itself opens its own "
             "port, awaiting the operator's report — kernel adjacency)"))))

(deftest a-reply-with-no-closing-act-leaves-the-port-open
  (let [record {:turn_id "t-bell-2" :origin "agent" :caller "claude-4"
                :agent_id "codex-23" :session_id "sess-1"
                :created_at "2026-10-04T22:20:10Z"
                :source_text "Please run the acceptance probe."}
        acts (ta/turn->acts record {} "㊥ noted, running it now." [])]
    (is (= 1 (count acts)) "gist is annotator-only: no reply act")
    (is (contains? (open-ids acts "2026-10-04T22:20:10Z") "t-bell-2-b-0"))))

(deftest no-completion-means-no-closure
  (let [h (harness)
        {:keys [record]} (svc/record-turn! (:svc h) bell-opts)
        acts (acts-of record nil)]
    (is (contains? (open-ids acts (:created_at record)) bell-id)
        "a failed/cancelled/timed-out job attaches nothing; the port stays open")))

(deftest operator-turns-have-no-bell-act
  (let [record {:turn_id "t-op-1" :agent_id "claude-17" :session_id "sess-1"
                :created_at "2026-10-04T22:20:10Z" :origin "operator"
                :source_text "Please continue."}
        reading {:sentences [{:id "s1" :fragments [{:intent "ask-action" :text "Please continue."}]}]}
        acts (ta/turn->acts record reading "" [])]
    (is (nil? (ta/bell-act record)))
    (is (= ["t-op-1-f-s1-0"] (map :id acts)) "fragments, exactly as before")))
