(ns futon3c.peripheral.reflect-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.evidence.store :as store]
            [futon3c.peripheral.reflect :as reflect]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]
            [futon3c.social.shapes :as shapes]
            [futon3c.social.test-fixtures :as fix]))

(deftest reflect-start-uses-reflection-evidence-type
  (let [p (reflect/make-reflect (tools/make-mock-backend))
        start (runner/start p {:session-id "sess-reflect-1" :agent-id "codex-1"})]
    (is (:ok start))
    (is (= :goal (get-in start [:evidence :evidence/claim-type])))
    (is (= :reflection (get-in start [:evidence :evidence/type])))))

(deftest reflect-step-rejects-non-reflect-tools
  (let [p (reflect/make-reflect (tools/make-mock-backend))
        start (runner/start p {:session-id "sess-reflect-2"})
        err (runner/step p (:state start) {:tool :edit :args ["src/a.clj"]})]
    (fix/assert-valid! shapes/SocialError err)
    (is (= :tool-not-allowed (:error/code err)))))

(deftest reflect-stop-produces-par-evidence-entry-fruit
  (let [backend (tools/make-mock-backend {:musn-log {:lines 80}})
        p (reflect/make-reflect backend)
        start (runner/start p {:session-id "sess-reflect-3"})
        step (runner/step p (:state start) {:tool :musn-log :args ["session.log"]})
        stop (runner/stop p (:state step) "session close")
        par (get-in stop [:fruit :par])]
    (is (:ok stop))
    (fix/assert-valid! shapes/EvidenceEntry par)
    (is (= :reflection (:evidence/type par)))
    (is (= :observation (:evidence/claim-type par)))
    (is (= :conclusion (get-in stop [:evidence :evidence/claim-type])))))

(deftest reflect-appends-par-and-stop-when-store-provided
  (let [evidence-store (atom {:entries {} :order []})
        backend (tools/make-mock-backend {:read {:content "log"}})
        p (reflect/make-reflect backend)
        start (runner/start p {:session-id "sess-reflect-4" :evidence-store evidence-store})
        step (runner/step p (:state start) {:tool :read :args ["session.log"]})
        stop (runner/stop p (:state step) "done")
        entries (store/query* evidence-store {})]
    (is (:ok stop))
    ;; start + step + par + stop
    (is (= 4 (count entries)))
    (is (some #(= :observation (:evidence/claim-type %)) entries))
    (is (every? #(= :reflection (:evidence/type %)) entries))))
