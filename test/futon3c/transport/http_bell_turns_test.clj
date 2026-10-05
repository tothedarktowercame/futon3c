(ns futon3c.transport.http-bell-turns-test
  "The W1 hook points in transport.http: create-invoke-job-ledger! records a
   work bell between registered agents as a 象 turn (off the request path),
   finalize-invoke-job! attaches the result on done only. Ledger, registry
   and parks isolated after auto-bellback-test's pattern; the 象 service is
   real, over a temp store."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.agency.parked-on :as parked-on]
            [futon3c.agency.registry :as reg]
            [futon3c.social.mesh-test-fixtures :as mesh-fixtures]
            [futon3c.transport.http :as http]
            [futon3c.xiang.turn-service :as xiang]
            [futon3c.xiang.turn-store :as ts]))

(def ^:dynamic *ledger-file* nil)

(defn- temp-store []
  (ts/store (str (java.nio.file.Files/createTempDirectory
                  "xiang-bell-hooks-" (make-array java.nio.file.attribute.FileAttribute 0)))))

(defn- xiang-service [store]
  (xiang/service {:store store
                  :schedule! (fn [_ thunk] (thunk))
                  :evidence-async! (fn [thunk] (thunk))
                  :draft-async! (fn [thunk] (thunk))}))

(defn- register-agent! [agent-id type]
  (reg/register-agent!
   {:agent-id {:id/value agent-id :id/type :continuity}
    :type type
    :invoke-fn (fn [_prompt _session-id] {:result "ok" :session-id nil})
    :capabilities [:invoke]}))

(defn- create-job! [{:keys [job-id] :as m}]
  (#'http/create-invoke-job! (assoc m :requested-job-id job-id)))

(defn- finalize! [job-id terminal-state result]
  (#'http/finalize-invoke-job! job-id terminal-state nil nil result "sess-recipient-1"))

(use-fixtures
  :each mesh-fixtures/with-store
  (fn [f]
    (let [tmp (java.io.File/createTempFile "bell-turns" ".edn")]
      (.delete tmp)
      (binding [*ledger-file* (.getAbsolutePath tmp)]
        (with-redefs-fn {#'http/invoke-jobs-store-path (fn [] *ledger-file*)}
          (fn []
            (reg/reset-registry!)
            (parked-on/clear!)
            (http/reset-invoke-jobs!)
            (reset! @#'http/!bell-turn-ids {})
            (try
              (f)
              (finally
                (reg/reset-registry!)
                (parked-on/clear!)
                (http/reset-invoke-jobs!)
                (reset! @#'http/!bell-turn-ids {})
                (io/delete-file tmp true)))))))))

(deftest a-work-bell-between-registered-agents-is-recorded
  (register-agent! "claude-4" :claude)
  (register-agent! "codex-23" :codex)
  (let [recorded (atom [])]
    (with-redefs-fn {#'http/*record-bell-turn!* (fn [request job-id]
                                                  (swap! recorded conj [request job-id]))}
      (fn []
        (create-job! {:job-id "job-w1" :agent-id "codex-23" :caller "claude-4"
                      :prompt "capture the elaborated terms" :mode "work" :surface "bell"})
        (is (= 1 (count @recorded)))
        (let [[request job-id] (first @recorded)]
          (is (= "job-w1" job-id))
          (is (= "claude-4" (:caller request)))
          (is (= "codex-23" (str (:agent-id request))))
          (is (= "capture the elaborated terms" (:prompt request))))
        (testing "a deduped reuse of the same job id records nothing twice"
          (create-job! {:job-id "job-w1" :agent-id "codex-23" :caller "claude-4"
                        :prompt "capture the elaborated terms" :mode "work" :surface "bell"})
          (is (= 1 (count @recorded))))))))

(deftest brief-auto-bellback-and-unregistered-record-nothing
  (register-agent! "claude-4" :claude)
  (register-agent! "codex-23" :codex)
  (let [recorded (atom [])]
    (with-redefs-fn {#'http/*record-bell-turn!* (fn [request job-id]
                                                  (swap! recorded conj [request job-id]))}
      (fn []
        (create-job! {:job-id "job-brief" :agent-id "codex-23" :caller "claude-4"
                      :prompt "quick question" :mode "brief"})
        (create-job! {:job-id "job-ab" :agent-id "claude-4" :caller "auto-bellback"
                      :prompt "RE: your bell" :mode "work"})
        (create-job! {:job-id "job-tc" :agent-id "codex-23" :caller "turn-capture"
                      :prompt "mode: task interpret this turn" :mode "work"})
        (create-job! {:job-id "job-ghost" :agent-id "ghost-9" :caller "claude-4"
                      :prompt "mode: task do work" :mode "work"})
        (is (empty? @recorded))))))

(deftest done-finalize-attaches-the-result-and-closes-the-port
  (register-agent! "claude-4" :claude)
  (register-agent! "codex-23" :codex)
  (let [store (temp-store)
        svc (xiang-service store)
        {:keys [id]} (xiang/record-turn!
                      svc {:dispatch :none :origin "agent"
                           :agent-id "codex-23" :session-id "pending"
                           :turn-id "job-c1" :text "capture the elaborated terms"
                           :caller "claude-4"})
        _ (swap! @#'http/!bell-turn-ids assoc "job-c1" id)
        report "㊢ capture complete, probe identical with and without."]
    (with-redefs-fn {#'http/xiang-bell-service (fn [] svc)
                     ;; the turn is already recorded above (id in
                     ;; !bell-turn-ids); skip the create-side hook so its
                     ;; future cannot race the assoc.
                     #'http/*record-bell-turn!* (fn [_ _] nil)
                     #'http/*enqueue-auto-bellback!* (fn [_] nil)}
      (fn []
        (create-job! {:job-id "job-c1" :agent-id "codex-23" :caller "claude-4"
                      :prompt "capture the elaborated terms" :mode "work" :surface "bell"})
        (finalize! "job-c1" "done" {:ok true :result report})))
    (let [stored (ts/read-record store id)]
      (is (= report (:reply_text stored)) "the result text is the happened reply")
      (is (= "sess-recipient-1" (:session_id stored))
          "the recipient's session replaces the placeholder")
      (is (empty? (get-in stored [:ports :still_open]))
          "the ㊢ report closed the bell's port"))))

(deftest failed-finalize-attaches-nothing
  (register-agent! "claude-4" :claude)
  (register-agent! "codex-23" :codex)
  (let [closed (atom [])]
    (with-redefs-fn {#'http/*close-bell-turn!* (fn [job] (swap! closed conj job))
                     #'http/*enqueue-auto-bellback!* (fn [_] nil)}
      (fn []
        (create-job! {:job-id "job-f1" :agent-id "codex-23" :caller "claude-4"
                      :prompt "mode: task do work" :mode "work"})
        (finalize! "job-f1" "failed" {:ok false :result "boom"})
        (is (empty? @closed) "a failed job attaches nothing; the port stays open")))))

(deftest done-finalize-of-an-unrecorded-job-is-a-no-op
  (register-agent! "claude-4" :claude)
  (register-agent! "codex-23" :codex)
  (let [store (temp-store)
        svc (xiang-service store)]
    (with-redefs-fn {#'http/xiang-bell-service (fn [] svc)
                     #'http/*enqueue-auto-bellback!* (fn [_] nil)}
      (fn []
        (create-job! {:job-id "job-brief-2" :agent-id "codex-23" :caller "claude-4"
                      :prompt "quick question" :mode "brief"})
        (finalize! "job-brief-2" "done" {:ok true :result "sure"})))
    (is (empty? (ts/list-records store)) "a brief bell recorded nothing, closed nothing")))
