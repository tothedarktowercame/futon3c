(ns futon3c.peripheral.drive-test
  "The drive proof: a Cyder-registered peripheral driven through multiple
   cycles, inspectably and boundedly — each arc-2 autorunner failure mode
   answered by an assertion."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.cyder :as cyder]
            [futon3c.peripheral.drive :as drive]))

(defn- cleanup! [& ids]
  (doseq [id ids]
    (cyder/deregister! id)
    (swap! drive/!drives dissoc (str "drive/" id))))

(deftest drives-a-peripheral-through-all-cycles-to-done
  (let [pid "toy-five-test"]
    (try
      (drive/register-toy-cycles! pid 5)
      (is (some? (get @cyder/!processes pid)) "target visible in Cyder")
      (let [drive-id (drive/drive! pid :max-steps 10)
            final (drive/await-drive drive-id 5000)]
        (is (= :done (:status final)) "stopped because the TARGET reported done — state-aware, not clock-driven")
        (is (= 5 (count (:steps final))) "exactly the work that remained: five steps, no more")
        (let [state ((get-in @cyder/!processes [pid :process/state-fn]))]
          (is (= 5 (:cycle state)))
          (is (true? (:done? state)))
          (is (= 5 (count (:work state))) "each cycle did real journaled work"))
        (is (nil? (get @cyder/!processes drive-id))
            "conductor deregistered itself when finished — no residue"))
      (finally (cleanup! pid)))))

(deftest stop-control-halts-mid-drive
  (let [pid "toy-slow-test"]
    (try
      (drive/register-toy-cycles! pid 100)
      (let [drive-id (drive/drive! pid :max-steps 100 :step-delay-ms 40)]
        (Thread/sleep 100)
        (is (= :running (:status (drive/drive-status drive-id))) "conductor visibly running")
        (is (some? (get @cyder/!processes drive-id)) "conductor itself Cyder-registered while running")
        (drive/stop-drive! drive-id)
        (let [final (drive/await-drive drive-id 3000)]
          (is (= :stopped (:status final)) "visible stop control works mid-drive")
          (is (< 0 (count (:steps final)) 100) "some cycles ran, not all")))
      (finally (cleanup! pid)))))

(deftest max-steps-bounds-a-runaway
  (let [pid "toy-endless-test"]
    (try
      (drive/register-toy-cycles! pid 1000000)
      (let [drive-id (drive/drive! pid :max-steps 7)
            final (drive/await-drive drive-id 5000)]
        (is (= :max-steps (:status final)) "bounded: a drive is a budget, not a blank check")
        (is (= 7 (count (:steps final)))))
      (finally (cleanup! pid)))))

(deftest guard-fn-is-the-state-aware-hook
  (let [pid "toy-guarded-test"]
    (try
      (drive/register-toy-cycles! pid 100)
      ;; guard: refuse to step past cycle 3 — e.g. a wall-report condition
      (let [drive-id (drive/drive! pid :max-steps 100
                                   :guard-fn (fn [state] (< (:cycle state 0) 3)))
            final (drive/await-drive drive-id 5000)]
        (is (= :halted (:status final)) "guard halts the drive on a state condition")
        (is (= 3 (count (:steps final))) "stepped exactly while the guard allowed"))
      (finally (cleanup! pid)))))

(deftest deregistered-target-halts-not-spins
  (let [pid "toy-vanish-test"]
    (try
      (drive/register-toy-cycles! pid 100)
      (let [drive-id (drive/drive! pid :max-steps 100 :step-delay-ms 40)]
        (Thread/sleep 90)
        (cyder/deregister! pid)
        (let [final (drive/await-drive drive-id 3000)]
          (is (= :halted (:status final)) "a vanished target halts the conductor loudly")))
      (finally (cleanup! pid)))))
