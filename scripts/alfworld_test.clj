#!/usr/bin/env bb
(ns scripts.alfworld-test
  "Test script for ALFWorld peripheral.

   Prerequisites:
   - ALFWorld server running: .venv-alfworld/bin/python3 scripts/alfworld-server.py

   Usage: clojure -M scripts/alfworld_test.clj"
  (:require [futon3c.peripheral.alfworld :as alfworld]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]))

(println "=== ALFWorld Peripheral Test ===")
(println)

(def backend (tools/make-mock-backend))
(def spec {:alfworld-server "http://localhost:3456"})
(def peripheral (alfworld/make-alfworld spec backend))

(println "1. Starting peripheral (resets ALFWorld to new task)...")
(def start-result (runner/start peripheral {:session-id "test-alf-001"}))
(if (:ok start-result)
  (do
    (println "   ✓ Started successfully")
    (let [state (:state start-result)
          alf-state (:alfworld-state state)]
      (println "   Task:" (:task alf-state))
      (println "   Task type:" (:task_type alf-state))
      (println "   Initial observation:")
      (println "   " (:observation alf-state))
      (println)))
  (do
    (println "   ✗ Start failed:")
    (println "   " start-result)
    (System/exit 1)))

(def state (:state start-result))

(println "2. Taking ALFWorld actions...")
(def actions ["look" "inventory" "go to cabinet 1"])
(loop [s state
       remaining actions
       step-num 1]
  (if (empty? remaining)
    (do
      (println "   ✓ All actions completed")
      (def final-state s))
    (let [action (first remaining)
          _ (println (format "   Step %d: %s" step-num action))
          step-result (runner/step peripheral s action)]
      (if (:ok step-result)
        (let [result (:result step-result)
              new-state (:state step-result)]
          (println (format "     → %s" (first (clojure.string/split (:observation result) #"\n"))))
          (recur new-state (rest remaining) (inc step-num)))
        (do
          (println "   ✗ Step failed:")
          (println "   " step-result)
          (System/exit 1))))))

(println)
(println "3. Testing coordination: bell to Joe...")
(def bell-result (runner/step peripheral final-state
                               {:coord-type :bell
                                :target "joe"
                                :message "I'm exploring the kitchen, any tips?"}))
(if (:ok bell-result)
  (do
    (println "   ✓ Bell sent successfully")
    (def bell-state (:state bell-result)))
  (do
    (println "   ✗ Bell failed:")
    (println "   " bell-result)
    (System/exit 1)))

(println)
(println "4. Testing coordination: whistle to Codex...")
(def whistle-result (runner/step peripheral bell-state
                                  {:coord-type :whistle
                                   :target "codex"
                                   :message "Can you generate code to parse the admissible commands?"}))
(if (:ok whistle-result)
  (do
    (println "   ✓ Whistle sent successfully (simulated)")
    (def whistle-state (:state whistle-result)))
  (do
    (println "   ✗ Whistle failed:")
    (println "   " whistle-result)
    (System/exit 1)))

(println)
(println "5. Stopping peripheral...")
(def stop-result (runner/stop peripheral whistle-state "test complete"))
(if (:ok stop-result)
  (do
    (println "   ✓ Stopped successfully")
    (let [fruit (:fruit stop-result)
          exit-ctx (:context stop-result)]
      (println "   Fruit:")
      (println "     Won:" (:won fruit))
      (println "     Score:" (:score fruit))
      (println "     Steps taken:" (:step-count fruit))
      (println "     Bells sent:" (count (get-in fruit [:coordination-events :bells-sent])))
      (println "     Whistles sent:" (count (get-in fruit [:coordination-events :whistles-sent])))
      (println "   Exit context includes:")
      (println "     Session ID:" (:session-id exit-ctx))
      (println "     ALFWorld state preserved:" (boolean (:alfworld-state exit-ctx)))
      (println "     Bells in context:" (count (:bells-sent exit-ctx)))))
  (do
    (println "   ✗ Stop failed:")
    (println "   " stop-result)
    (System/exit 1)))

(println)
(println "=== Test Complete ===")
(println "The peripheral can:")
(println "  - Start fresh ALFWorld tasks")
(println "  - Execute ALFWorld actions (go, take, look, etc.)")
(println "  - Send bells (async notifications)")
(println "  - Send whistles (blocking requests)")
(println "  - Preserve state in exit context for hop resumption")
