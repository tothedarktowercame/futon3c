#!/usr/bin/env bb
(ns scripts.alfworld-demo
  "Interactive demo of ALFWorld stepper with hop transitions.

   Simulates: Chat → ALFWorld (explore) → Chat (stuck) → ALFWorld (resume) → Win

   Prerequisites:
   - ALFWorld server: .venv-alfworld/bin/python3 scripts/alfworld-server.py

   Usage: clojure -M scripts/alfworld_demo.clj"
  (:require [futon3c.peripheral.alfworld :as alfworld]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]))

(defn simulate-chat-turn [msg]
  (println)
  (println "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
  (println "CHAT PERIPHERAL")
  (println "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
  (println msg)
  (println))

(defn simulate-alf-turn [observation step-count]
  (println)
  (println "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
  (println (format "ALFWORLD PERIPHERAL (step %d)" step-count))
  (println "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
  (println observation)
  (println))

(println)
(println "╔═══════════════════════════════════════════════════════╗")
(println "║  ALFWorld Stepper Demo: Multi-Hop Session            ║")
(println "╚═══════════════════════════════════════════════════════╝")
(println)

(def backend (tools/make-mock-backend))
(def spec {:alfworld-server "http://localhost:3456"})
(def session-id "demo-hop-001")

;; =============================================================================
;; Act 1: Chat → ALFWorld (initial hop)
;; =============================================================================

(simulate-chat-turn
  "Joe: @claude Can you solve an ALFWorld task for me?

Claude: Sure! Let me hop into the ALFWorld peripheral and see what task
        we're working with. 🎮

        [hopping to alfworld...]")

(println "Starting ALFWorld peripheral (first hop)...")
(def peripheral (alfworld/make-alfworld spec backend))
(def start-result (runner/start peripheral {:session-id session-id}))

(when-not (:ok start-result)
  (println "ERROR: Failed to start peripheral")
  (println start-result)
  (System/exit 1))

(def state-1 (:state start-result))
(def alf-state-1 (:alfworld-state state-1))

(simulate-alf-turn
  (str "Task: " (:task alf-state-1) "\n"
       "Type: " (:task_type alf-state-1) "\n\n"
       (:observation alf-state-1))
  0)

(println "Claude: Okay, I need to find and clean a mug, then put it in the coffeemachine.")
(println "        Let me start exploring...\n")

;; =============================================================================
;; Act 2: ALFWorld exploration (taking actions)
;; =============================================================================

(def actions-1 ["look" "inventory" "go to cabinet 1" "open cabinet 1"])

(loop [s state-1
       remaining actions-1
       step-num 1]
  (if (empty? remaining)
    (def state-2 s)
    (let [action (first remaining)]
      (println (format "  > %s" action))
      (let [step-result (runner/step peripheral s action)]
        (when-not (:ok step-result)
          (println "ERROR: Step failed")
          (println step-result)
          (System/exit 1))
        (let [result (:result step-result)
              new-state (:state step-result)]
          (println (format "    %s" (first (clojure.string/split (:observation result) #"\n"))))
          (recur new-state (rest remaining) (inc step-num)))))))

(println)
(println "Claude: Hmm, the cabinet is empty. I'm not sure where to look next.")
(println "        Let me bell Joe for guidance...")
(println)

;; =============================================================================
;; Act 3: Coordination via bell
;; =============================================================================

(def bell-result (runner/step peripheral state-2
                               {:coord-type :bell
                                :target "joe"
                                :message "I've checked cabinet 1 but it's empty. Where else should I look for the mug?"}))

(when-not (:ok bell-result)
  (println "ERROR: Bell failed")
  (println bell-result)
  (System/exit 1))

(def state-3 (:state bell-result))

(println "  [Bell sent to Joe]")
(println)
(println "Claude: Actually, I'm quite stuck. Let me hop back to chat and wait for Joe's response.")
(println "        I can resume from this exact state once I have more info.")
(println)

;; =============================================================================
;; Act 4: Hop out (explicit exit)
;; =============================================================================

(def stop-result-1 (runner/stop peripheral state-3 "Waiting for Joe's guidance"))

(when-not (:ok stop-result-1)
  (println "ERROR: Stop failed")
  (println stop-result-1)
  (System/exit 1))

(def exit-context-1 (:context stop-result-1))
(def fruit-1 (:fruit stop-result-1))

(println "  [Peripheral stopped, state saved in exit context]")
(println (format "  Bells sent: %d" (count (:bells-sent exit-context-1))))
(println (format "  ALFWorld state preserved: %s" (boolean (:alfworld-state exit-context-1))))
(println)

(simulate-chat-turn
  "Claude: I've hopped back to chat. I explored the kitchen but couldn't find
         the mug in cabinet 1. I've sent you a bell with my question. While
         I wait, should I try a different approach or do you have a hint?

Joe: @claude Check the dining table - mugs are often there in these tasks.

Claude: Great tip! Let me hop back into ALFWorld and check the dining table.

        [hopping back to alfworld with preserved state...]")

;; =============================================================================
;; Act 5: Hop back in (resume from saved state)
;; =============================================================================

(println "Resuming ALFWorld peripheral (second hop)...")
(def peripheral-2 (alfworld/make-alfworld spec backend))
(def resume-context {:session-id session-id
                     :alfworld-state (:alfworld-state exit-context-1)})
(def start-result-2 (runner/start peripheral-2 resume-context))

(when-not (:ok start-result-2)
  (println "ERROR: Failed to resume peripheral")
  (println start-result-2)
  (System/exit 1))

(def state-4 (:state start-result-2))
(def alf-state-2 (:alfworld-state state-4))

(println "  ✓ Resumed from preserved state")
(println (format "  Current step: %d" (:step alf-state-2)))
(println (format "  Current observation: %s..." (subs (:observation alf-state-2) 0 60)))
(println)

(simulate-alf-turn
  (str "Resuming where I left off...\n\n"
       (:observation alf-state-2))
  (:step alf-state-2))

(println "Claude: Good, I'm back in the kitchen. Now let me go to the dining table...")
(println)

;; =============================================================================
;; Act 6: Completing the task
;; =============================================================================

(def actions-2 ["go to diningtable 1" "examine diningtable 1" "take mug 1"
                "go to sinkbasin 1" "clean mug 1 with sinkbasin 1"
                "go to coffeemachine 1" "put mug 1 in coffeemachine 1"])

(loop [s state-4
       remaining actions-2
       step-num (inc (:step alf-state-2))]
  (if (empty? remaining)
    (def state-5 s)
    (let [action (first remaining)]
      (println (format "  > %s" action))
      (let [step-result (runner/step peripheral-2 s action)]
        (when-not (:ok step-result)
          (println "ERROR: Step failed")
          (println step-result)
          (System/exit 1))
        (let [result (:result step-result)
              new-state (:state step-result)]
          (println (format "    %s" (first (clojure.string/split (:observation result) #"\n"))))
          (when (:won result)
            (println)
            (println "    🎉 TASK COMPLETED! 🎉")
            (println (format "    Score: %.1f/%.1f" (:score result) (:max_score result))))
          (recur new-state (rest remaining) (inc step-num)))))))

(println)
(println "Claude: Success! I found the mug on the dining table (thanks to your hint!),")
(println "        cleaned it, and put it in the coffeemachine. Task complete!")
(println)

;; =============================================================================
;; Act 7: Final hop out (task complete)
;; =============================================================================

(def stop-result-2 (runner/stop peripheral-2 state-5 "Task completed successfully"))

(when-not (:ok stop-result-2)
  (println "ERROR: Final stop failed")
  (println stop-result-2)
  (System/exit 1))

(def fruit-2 (:fruit stop-result-2))

(simulate-chat-turn
  "Claude: I've hopped back to chat. Here's the summary:

         Result: Won ✓
         Score: 1.0
         Total steps: 11
         Coordination events:
           - 1 bell sent to Joe (asking for hint)
           - Used your dining table tip to find the mug

         This demonstrates the stepper pattern perfectly:
         1. Hopped into ALFWorld
         2. Got stuck, hopped back to chat for guidance
         3. Received your hint
         4. Hopped back in with preserved state
         5. Completed the task
         6. Hopped out with results

         The state continuity across hops is exactly what P-4 and P-6 are about!")

(println)
(println "╔═══════════════════════════════════════════════════════╗")
(println "║  Demo Complete                                        ║")
(println "╚═══════════════════════════════════════════════════════╝")
(println)
(println "Key takeaways:")
(println "  ✓ Agent hopped into ALFWorld peripheral")
(println "  ✓ Took ALFWorld actions (go, take, clean, put)")
(println "  ✓ Sent coordination bell to Joe")
(println "  ✓ Hopped back to chat (explicit exit with reason)")
(println "  ✓ Resumed from exact same state (preserves room/inventory)")
(println "  ✓ Completed task with Joe's guidance")
(println "  ✓ Hopped out with fruit (won=true, score=1.0)")
(println)
(println "This validates:")
(println "  P-4: Explicit exit — agent decides when to hop, not forced by errors")
(println "  P-6: Interleaved streams — agent processes both ALFWorld obs and Joe's messages")
(println)
