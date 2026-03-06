(ns futon3c.cyder-test
  "CYDER process registry tests — lifecycle, inspection, and jack-in."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.java.io :as io]
            [futon3c.cyder :as cyder]))

(use-fixtures
  :each
  (fn [f]
    (reset! cyder/!processes {})
    (f)))

;; =============================================================================
;; Registration
;; =============================================================================

(deftest register-basic
  (testing "register a process and find it in the list"
    (let [stopped? (atom false)
          result (cyder/register!
                  {:id "test-server"
                   :type :server
                   :stop-fn #(reset! stopped? true)})]
      (is (= "test-server" (:process/id result)))
      (is (= :server (:process/type result)))
      (is (= :infra (:process/layer result)))
      (is (= 1 (count (cyder/list-processes)))))))

(deftest register-duplicate-rejected
  (testing "duplicate registration returns error"
    (cyder/register! {:id "dup" :type :server :stop-fn (fn [])})
    (let [result (cyder/register! {:id "dup" :type :server :stop-fn (fn [])})]
      (is (false? (:ok result)))
      (is (re-find #"already registered" (:error result))))))

(deftest register-repl-layer-inferred
  (testing "providing step-fn implies :repl layer"
    (let [result (cyder/register!
                  {:id "conductor"
                   :type :daemon
                   :stop-fn (fn [])
                   :step-fn (fn [] :stepped)})]
      (is (= :repl (:process/layer result))))))

;; =============================================================================
;; Deregistration
;; =============================================================================

(deftest deregister-removes-process
  (testing "deregister removes without calling stop-fn"
    (let [stopped? (atom false)]
      (cyder/register! {:id "temp" :type :bridge :stop-fn #(reset! stopped? true)})
      (is (= 1 (count (cyder/list-processes))))
      (let [result (cyder/deregister! "temp")]
        (is (true? (:ok result)))
        (is (= 0 (count (cyder/list-processes))))
        (is (false? @stopped?))))))

(deftest deregister-unknown-returns-error
  (let [result (cyder/deregister! "nope")]
    (is (false? (:ok result)))))

;; =============================================================================
;; Inspection
;; =============================================================================

(deftest inspect-with-state-fn
  (testing "inspect returns live state from state-fn"
    (let [counter (atom 0)]
      (cyder/register! {:id "stateful"
                        :type :daemon
                        :stop-fn (fn [])
                        :state-fn #(do {:count @counter})})
      (swap! counter inc)
      (swap! counter inc)
      (let [result (cyder/inspect "stateful")]
        (is (true? (:ok result)))
        (is (= {:count 2} (get-in result [:process :process/state])))))))

(deftest inspect-unknown-returns-error
  (let [result (cyder/inspect "ghost")]
    (is (false? (:ok result)))))

(deftest list-processes-omits-functions
  (testing "list-processes returns no function values (JSON-safe)"
    (cyder/register! {:id "srv" :type :server :stop-fn (fn [])
                      :state-fn (fn [] {:ok true})})
    (let [procs (cyder/list-processes)
          p (first procs)]
      (is (every? (complement fn?) (vals p))))))

;; =============================================================================
;; Lifecycle — stop
;; =============================================================================

(deftest stop-calls-stop-fn-and-deregisters
  (let [stopped? (atom false)]
    (cyder/register! {:id "mortal" :type :daemon :stop-fn #(reset! stopped? true)})
    (let [result (cyder/stop! "mortal")]
      (is (true? (:ok result)))
      (is (true? @stopped?))
      (is (= 0 (count (cyder/list-processes)))))))

(deftest stop-unknown-returns-error
  (let [result (cyder/stop! "void")]
    (is (false? (:ok result)))))

(deftest stop-all-stops-everything
  (let [log (atom [])]
    (cyder/register! {:id "a" :type :server :stop-fn #(swap! log conj :a)})
    (cyder/register! {:id "b" :type :bridge :stop-fn #(swap! log conj :b)})
    (let [n (cyder/stop-all!)]
      (is (= 2 n))
      (is (= 0 (count (cyder/list-processes))))
      (is (= 2 (count @log))))))

;; =============================================================================
;; Jack-in — stepping
;; =============================================================================

(deftest step-steppable-process
  (let [counter (atom 0)]
    (cyder/register! {:id "stepper"
                      :type :daemon
                      :stop-fn (fn [])
                      :step-fn #(swap! counter inc)})
    (let [result (cyder/step! "stepper")]
      (is (true? (:ok result)))
      (is (= 1 (:result result)))
      (is (= 1 @counter)))))

(deftest step-non-steppable-returns-error
  (cyder/register! {:id "infra" :type :server :stop-fn (fn [])})
  (let [result (cyder/step! "infra")]
    (is (false? (:ok result)))
    (is (re-find #"not steppable" (:error result)))))

;; =============================================================================
;; Aggregate status
;; =============================================================================

(deftest registry-status-shape
  (cyder/register! {:id "x" :type :server :stop-fn (fn [])})
  (cyder/register! {:id "y" :type :daemon :stop-fn (fn []) :step-fn (fn [] :ok)})
  (let [status (cyder/registry-status)]
    (is (= 2 (:count status)))
    (is (contains? (:processes status) "x"))
    (is (contains? (:processes status) "y"))))

;; =============================================================================
;; Mission scanner
;; =============================================================================

(deftest scan-missions-reads-real-docs
  (testing "scan-missions finds M-*.md files with status lines"
    (let [missions (cyder/scan-missions "holes/missions")]
      (is (pos? (count missions)))
      (is (every? :name missions))
      (is (every? :phase missions))
      (is (some #(= "M-cyder" (:name %)) missions)))))

(deftest scan-missions-empty-dir
  (testing "scan-missions returns empty for nonexistent dir"
    (is (nil? (cyder/scan-missions "/tmp/no-such-dir-cyder-test")))))

(deftest register-missions-registers-active
  (testing "register-missions! registers active missions as processes"
    (let [n (cyder/register-missions! "holes/missions")]
      (is (pos? n))
      ;; M-cyder is at DERIVE — should be registered
      (let [result (cyder/inspect "M-cyder")]
        (is (true? (:ok result)))
        (is (= :state-machine (get-in result [:process :process/type])))
        (is (= :repl (get-in result [:process :process/layer])))
        ;; state-fn returns live phase
        (is (string? (get-in result [:process :process/state :phase])))))))
