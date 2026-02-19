(ns futon3c.transport.ws-peripheral-test
  "Seam 4: WS session lifecycle → peripheral lifecycle tests.

   Tests the three new frame types (peripheral_start, tool_action, peripheral_stop)
   and the on-close auto-stop behavior. Uses mock send-fn/close-fn like ws_test.clj
   but with a real peripheral config (RealBackend + evidence store)."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [futon3c.agency.registry :as reg]
            [futon3c.peripheral.real-backend :as rb]
            [futon3c.peripheral.registry :as preg]
            [futon3c.social.persist :as persist]
            [futon3c.social.test-fixtures :as fix]
            [futon3c.transport.ws :as ws])
  (:import [java.io File]))

(use-fixtures
  :each
  (fn [f]
    (reg/reset-registry!)
    (persist/reset-sessions!)
    (f)))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- register-mock-agent! [agent-id-str type]
  (reg/register-agent!
   {:agent-id {:id/value agent-id-str :id/type :continuity}
    :type type
    :invoke-fn (fn [_ _] {:result "ok" :session-id nil})
    :capabilities [:explore :edit :mission-control]}))

(defn- seed-temp-dir! []
  (let [dir (fix/temp-dir!)
        hello (io/file dir "hello.txt")]
    (spit hello "seam-4 test content")
    {:dir dir :hello-path (.getAbsolutePath hello)}))

(defn- cleanup-temp-dir! [dir]
  (doseq [f (reverse (file-seq (io/file dir)))]
    (.delete ^File f)))

(defn- make-real-peripheral-config [cwd evidence-store]
  {:backend (rb/make-real-backend {:cwd cwd :evidence-store evidence-store :timeout-ms 10000})
   :peripherals (preg/load-peripherals)
   :evidence-store evidence-store})

(defn- make-ws-with-peripherals
  "Create WS callbacks with real peripheral config for testing."
  [cwd evidence-store & {:keys [on-disconnect]}]
  (let [sent (atom [])
        closed (atom [])
        registry (fix/mock-registry
                  {:peripheral-config (make-real-peripheral-config cwd evidence-store)})
        config {:registry registry
                :patterns (fix/mock-patterns)
                :send-fn (fn [ch data] (swap! sent conj {:ch ch :data data}))
                :close-fn (fn [ch] (swap! closed conj ch))
                :on-disconnect on-disconnect}
        callbacks (ws/make-ws-callbacks config)]
    (assoc callbacks :sent sent :closed closed)))

(defn- mock-request []
  {:request-method :get :uri "/ws" :request-uri "/ws"})

(defn- last-sent [sent-atom]
  (when-let [entry (last @sent-atom)]
    (json/parse-string (:data entry) true)))

(defn- ready-frame [agent-id]
  (json/generate-string {"type" "ready" "agent_id" agent-id}))

(defn- peripheral-start-frame
  ([] (json/generate-string {"type" "peripheral_start"}))
  ([pid] (json/generate-string {"type" "peripheral_start" "peripheral_id" pid})))

(defn- tool-action-frame [tool args]
  (json/generate-string {"type" "tool_action" "tool" tool "args" args}))

(defn- peripheral-stop-frame [reason]
  (json/generate-string {"type" "peripheral_stop" "reason" reason}))

(defn- connect-agent!
  "Open connection and complete handshake. Returns channel keyword."
  [{:keys [on-open on-receive sent]} ch agent-id]
  (on-open ch (mock-request))
  (on-receive ch (ready-frame agent-id))
  (reset! sent [])
  ch)

;; =============================================================================
;; 1. Start peripheral session
;; =============================================================================

(deftest ws-peripheral-start-default
  (testing "peripheral_start without peripheral_id → uses agent type default (claude → :explore)"
    (let [{:keys [dir]} (seed-temp-dir!)
          evidence-store (atom {:entries {} :order []})]
      (try
        (register-mock-agent! "claude-1" :claude)
        (let [ws (make-ws-with-peripherals dir evidence-store)
              ch (connect-agent! ws :ch-1 "claude-1")]
          ((:on-receive ws) ch (peripheral-start-frame))
          (let [frame (last-sent (:sent ws))]
            (is (= "peripheral_started" (:type frame)))
            (is (= "explore" (:peripheral_id frame)))
            (is (string? (:session_id frame)))))
        (finally (cleanup-temp-dir! dir))))))

(deftest ws-peripheral-start-explicit-id
  (testing "peripheral_start with explicit peripheral_id"
    (let [{:keys [dir]} (seed-temp-dir!)
          evidence-store (atom {:entries {} :order []})]
      (try
        (register-mock-agent! "claude-1" :claude)
        (let [ws (make-ws-with-peripherals dir evidence-store)
              ch (connect-agent! ws :ch-2 "claude-1")]
          ((:on-receive ws) ch (peripheral-start-frame "explore"))
          (let [frame (last-sent (:sent ws))]
            (is (= "peripheral_started" (:type frame)))
            (is (= "explore" (:peripheral_id frame)))))
        (finally (cleanup-temp-dir! dir))))))

(deftest ws-peripheral-start-before-handshake
  (testing "peripheral_start before readiness handshake → error"
    (let [{:keys [dir]} (seed-temp-dir!)
          evidence-store (atom {:entries {} :order []})]
      (try
        (let [ws (make-ws-with-peripherals dir evidence-store)]
          ((:on-open ws) :ch-3 (mock-request))
          ((:on-receive ws) :ch-3 (peripheral-start-frame))
          (let [frame (last-sent (:sent ws))]
            (is (= "error" (:type frame)))
            (is (= "not-ready" (:code frame)))))
        (finally (cleanup-temp-dir! dir))))))

(deftest ws-peripheral-start-already-active
  (testing "peripheral_start when already active → error"
    (let [{:keys [dir]} (seed-temp-dir!)
          evidence-store (atom {:entries {} :order []})]
      (try
        (register-mock-agent! "claude-1" :claude)
        (let [ws (make-ws-with-peripherals dir evidence-store)
              ch (connect-agent! ws :ch-4 "claude-1")]
          ;; Start first peripheral
          ((:on-receive ws) ch (peripheral-start-frame))
          (reset! (:sent ws) [])
          ;; Try to start another
          ((:on-receive ws) ch (peripheral-start-frame))
          (let [frame (last-sent (:sent ws))]
            (is (= "error" (:type frame)))
            (is (= "peripheral-already-active" (:code frame)))))
        (finally (cleanup-temp-dir! dir))))))

;; =============================================================================
;; 2. Tool action through peripheral
;; =============================================================================

(deftest ws-tool-action-reads-real-file
  (testing "tool_action :read through active explore peripheral → real file content"
    (let [{:keys [dir hello-path]} (seed-temp-dir!)
          evidence-store (atom {:entries {} :order []})]
      (try
        (register-mock-agent! "claude-1" :claude)
        (let [ws (make-ws-with-peripherals dir evidence-store)
              ch (connect-agent! ws :ch-5 "claude-1")]
          ;; Start peripheral
          ((:on-receive ws) ch (peripheral-start-frame))
          (reset! (:sent ws) [])
          ;; Execute tool action
          ((:on-receive ws) ch (tool-action-frame "read" [hello-path]))
          (let [frame (last-sent (:sent ws))]
            (is (= "tool_result" (:type frame)))
            (is (= "read" (:tool frame)))
            (is (true? (:ok frame)))
            (is (string? (:result frame)))
            (is (.contains ^String (:result frame) "seam-4 test content"))))
        (finally (cleanup-temp-dir! dir))))))

(deftest ws-tool-action-without-peripheral
  (testing "tool_action without active peripheral → error"
    (let [{:keys [dir]} (seed-temp-dir!)
          evidence-store (atom {:entries {} :order []})]
      (try
        (register-mock-agent! "claude-1" :claude)
        (let [ws (make-ws-with-peripherals dir evidence-store)
              ch (connect-agent! ws :ch-6 "claude-1")]
          ((:on-receive ws) ch (tool-action-frame "read" ["/some/file"]))
          (let [frame (last-sent (:sent ws))]
            (is (= "error" (:type frame)))
            (is (= "no-active-peripheral" (:code frame)))))
        (finally (cleanup-temp-dir! dir))))))

(deftest ws-multiple-tool-actions
  (testing "multiple tool actions accumulate state in the peripheral"
    (let [{:keys [dir hello-path]} (seed-temp-dir!)
          evidence-store (atom {:entries {} :order []})]
      (try
        (register-mock-agent! "claude-1" :claude)
        (let [ws (make-ws-with-peripherals dir evidence-store)
              ch (connect-agent! ws :ch-7 "claude-1")]
          ;; Start peripheral
          ((:on-receive ws) ch (peripheral-start-frame))
          (reset! (:sent ws) [])
          ;; Two tool actions
          ((:on-receive ws) ch (tool-action-frame "read" [hello-path]))
          (let [f1 (last-sent (:sent ws))]
            (is (= "tool_result" (:type f1))))
          ((:on-receive ws) ch (tool-action-frame "glob" ["*.txt"]))
          (let [f2 (last-sent (:sent ws))]
            (is (= "tool_result" (:type f2))))
          ;; Evidence store has entries from both steps
          (let [entries (vals (:entries @evidence-store))
                steps (filter #(= :step (:evidence/claim-type %)) entries)]
            (is (>= (count steps) 2)
                (str "Expected >= 2 step entries, got " (count steps)))))
        (finally (cleanup-temp-dir! dir))))))

;; =============================================================================
;; 3. Stop peripheral session
;; =============================================================================

(deftest ws-peripheral-stop-returns-fruit
  (testing "peripheral_stop → fruit returned in response"
    (let [{:keys [dir hello-path]} (seed-temp-dir!)
          evidence-store (atom {:entries {} :order []})]
      (try
        (register-mock-agent! "claude-1" :claude)
        (let [ws (make-ws-with-peripherals dir evidence-store)
              ch (connect-agent! ws :ch-8 "claude-1")]
          ;; Start, do one action, then stop
          ((:on-receive ws) ch (peripheral-start-frame))
          ((:on-receive ws) ch (tool-action-frame "read" [hello-path]))
          (reset! (:sent ws) [])
          ((:on-receive ws) ch (peripheral-stop-frame "done"))
          (let [frame (last-sent (:sent ws))]
            (is (= "peripheral_stopped" (:type frame)))
            (is (= "explore" (:peripheral_id frame)))
            (is (= "done" (:reason frame)))
            ;; Fruit present
            (is (some? (:fruit frame)))))
        (finally (cleanup-temp-dir! dir))))))

(deftest ws-peripheral-stop-without-active
  (testing "peripheral_stop without active peripheral → error"
    (let [{:keys [dir]} (seed-temp-dir!)
          evidence-store (atom {:entries {} :order []})]
      (try
        (register-mock-agent! "claude-1" :claude)
        (let [ws (make-ws-with-peripherals dir evidence-store)
              ch (connect-agent! ws :ch-9 "claude-1")]
          ((:on-receive ws) ch (peripheral-stop-frame "done"))
          (let [frame (last-sent (:sent ws))]
            (is (= "error" (:type frame)))
            (is (= "no-active-peripheral" (:code frame)))))
        (finally (cleanup-temp-dir! dir))))))

;; =============================================================================
;; 4. On-close auto-stops peripheral
;; =============================================================================

(deftest ws-close-stops-active-peripheral
  (testing "closing connection with active peripheral auto-stops it"
    (let [{:keys [dir hello-path]} (seed-temp-dir!)
          evidence-store (atom {:entries {} :order []})
          disconnected (atom [])]
      (try
        (register-mock-agent! "claude-1" :claude)
        (let [ws (make-ws-with-peripherals dir evidence-store
                                           :on-disconnect #(swap! disconnected conj %))
              ch :ch-10]
          ;; Connect, start peripheral, do an action
          ((:on-open ws) ch (mock-request))
          ((:on-receive ws) ch (ready-frame "claude-1"))
          ((:on-receive ws) ch (peripheral-start-frame))
          ((:on-receive ws) ch (tool-action-frame "read" [hello-path]))
          ;; Close connection — should auto-stop peripheral
          ((:on-close ws) ch :normal)
          ;; Disconnect hook fired
          (is (= ["claude-1"] @disconnected))
          ;; Connection removed
          (is (nil? (get @(:connections ws) ch)))
          ;; Evidence includes stop entry from auto-stop
          (let [entries (vals (:entries @evidence-store))
                conclusions (filter #(= :conclusion (:evidence/claim-type %)) entries)]
            (is (>= (count conclusions) 1)
                "peripheral stop should have emitted conclusion evidence")))
        (finally (cleanup-temp-dir! dir))))))

;; =============================================================================
;; 5. Full session lifecycle: start → actions → stop
;; =============================================================================

(deftest ws-full-peripheral-session-lifecycle
  (testing "complete lifecycle: connect → handshake → start → actions → stop → disconnect"
    (let [{:keys [dir hello-path]} (seed-temp-dir!)
          evidence-store (atom {:entries {} :order []})]
      (try
        (register-mock-agent! "claude-1" :claude)
        (let [ws (make-ws-with-peripherals dir evidence-store)
              ch :ch-11]
          ;; 1. Connect
          ((:on-open ws) ch (mock-request))
          (is (false? (:connected? (get @(:connections ws) ch))))

          ;; 2. Handshake
          ((:on-receive ws) ch (ready-frame "claude-1"))
          (is (true? (:connected? (get @(:connections ws) ch))))

          ;; 3. Start peripheral
          ((:on-receive ws) ch (peripheral-start-frame))
          (let [conn (get @(:connections ws) ch)]
            (is (some? (:peripheral conn)) "peripheral instance stored")
            (is (some? (:peripheral-state conn)) "peripheral state atom stored")
            (is (= :explore (:peripheral-id conn))))

          ;; 4. Tool actions
          ((:on-receive ws) ch (tool-action-frame "read" [hello-path]))
          ((:on-receive ws) ch (tool-action-frame "glob" ["*.txt"]))

          ;; 5. Stop peripheral
          (reset! (:sent ws) [])
          ((:on-receive ws) ch (peripheral-stop-frame "session-complete"))
          (let [frame (last-sent (:sent ws))
                conn (get @(:connections ws) ch)]
            (is (= "peripheral_stopped" (:type frame)))
            ;; Peripheral cleared from connection state
            (is (nil? (:peripheral conn)))
            (is (nil? (:peripheral-state conn)))
            (is (nil? (:peripheral-id conn))))

          ;; 6. Evidence chain is complete
          (let [entries (vals (:entries @evidence-store))
                by-claim (group-by :evidence/claim-type entries)]
            (is (= 1 (count (:goal by-claim))) "one start/goal entry")
            (is (= 2 (count (:step by-claim))) "two step entries")
            (is (= 1 (count (:conclusion by-claim))) "one stop/conclusion entry"))

          ;; 7. Disconnect
          ((:on-close ws) ch :normal)
          (is (nil? (get @(:connections ws) ch))))
        (finally (cleanup-temp-dir! dir))))))

;; =============================================================================
;; 6. Regression tests for Codex review findings
;; =============================================================================

(deftest ws-peripheral-start-unknown-id-returns-error
  (testing "peripheral_start with unknown peripheral ID → typed error, not exception (Codex #2)"
    (let [{:keys [dir]} (seed-temp-dir!)
          evidence-store (atom {:entries {} :order []})]
      (try
        (register-mock-agent! "claude-1" :claude)
        (let [ws (make-ws-with-peripherals dir evidence-store)
              ch (connect-agent! ws :ch-unknown "claude-1")]
          ((:on-receive ws) ch (peripheral-start-frame "nonexistent"))
          (let [frame (last-sent (:sent ws))]
            (is (= "error" (:type frame)))
            (is (= "unknown-peripheral" (:code frame)))))
        (finally (cleanup-temp-dir! dir))))))

(deftest ws-concurrent-tool-actions-no-lost-updates
  (testing "concurrent tool_action frames don't lose state updates (Codex #3)"
    (let [{:keys [dir hello-path]} (seed-temp-dir!)
          evidence-store (atom {:entries {} :order []})]
      (try
        (register-mock-agent! "claude-1" :claude)
        (let [ws (make-ws-with-peripherals dir evidence-store)
              ch (connect-agent! ws :ch-race "claude-1")]
          ((:on-receive ws) ch (peripheral-start-frame))
          (reset! (:sent ws) [])
          ;; Fire two tool actions from parallel threads
          (let [f1 (future ((:on-receive ws) ch (tool-action-frame "read" [hello-path])))
                f2 (future ((:on-receive ws) ch (tool-action-frame "glob" ["*.txt"])))]
            @f1 @f2)
          ;; Both should have succeeded (no error frames)
          (let [frames (mapv #(json/parse-string (:data %) true) @(:sent ws))]
            (is (= 2 (count frames)))
            (is (every? #(= "tool_result" (:type %)) frames)
                "Both concurrent actions should produce tool_result, not error"))
          ;; Evidence should have 2 step entries (not 1)
          (let [entries (vals (:entries @evidence-store))
                steps (filter #(= :step (:evidence/claim-type %)) entries)]
            (is (>= (count steps) 2)
                "Both actions should be recorded — no lost updates")))
        (finally (cleanup-temp-dir! dir))))))

(deftest ws-stop-idempotent-under-close-race
  (testing "stop-peripheral! is idempotent — double stop produces only one conclusion (Codex #4)"
    (let [{:keys [dir hello-path]} (seed-temp-dir!)
          evidence-store (atom {:entries {} :order []})]
      (try
        (register-mock-agent! "claude-1" :claude)
        (let [ws (make-ws-with-peripherals dir evidence-store)
              ch :ch-idempotent]
          ;; Connect, start, do an action
          ((:on-open ws) ch (mock-request))
          ((:on-receive ws) ch (ready-frame "claude-1"))
          ((:on-receive ws) ch (peripheral-start-frame))
          ((:on-receive ws) ch (tool-action-frame "read" [hello-path]))
          ;; Race: explicit stop and on-close concurrently
          (let [f1 (future ((:on-receive ws) ch (peripheral-stop-frame "explicit")))
                f2 (future ((:on-close ws) ch :normal))]
            @f1 @f2)
          ;; Only one conclusion entry in evidence
          (let [entries (vals (:entries @evidence-store))
                conclusions (filter #(= :conclusion (:evidence/claim-type %)) entries)]
            (is (= 1 (count conclusions))
                (str "Expected exactly 1 conclusion, got " (count conclusions)))))
        (finally (cleanup-temp-dir! dir))))))
