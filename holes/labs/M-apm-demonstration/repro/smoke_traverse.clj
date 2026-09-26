;; SMOKE TEST, 2026-08-15. Kept OUT of test/ deliberately: it demonstrates a gap
;; rather than guarding a behaviour. Copy into test/futon3c/smoke/ to run.
;;
;; RESULT
;;   BEGIN -> :register
;;       all six :register tools execute ok, :assign-checkouts included
;;     :register  *** NO-OP: advance returned :ok and DID NOT MOVE ***
;;        payload supplied : (:environment-checkouts :environment-revision
;;                            :harness-revision :registration :store-snapshot
;;                            :stratum-frozen-at)
;;        outputs recorded : ()
;;
;; FINDING: :advance-problem-phase is not implemented. The engine takes the next
;; phase from the advance tool's result (cycle.clj:367, (:cycle/phase result));
;; the mock answers any unknown tool with {:ok true :result nil}, so new-phase is
;; nil. Both the transition AND the :cycle/outputs merge are gated on new-phase
;; (cycle.clj:379-381), so the payload is discarded too -- silently, with :ok.
;;
;; :advance-problem-phase occurs exactly ONCE in src/: the keyword definition at
;; problem.clj:28. The sibling peripheral has a real one --
;; proof_backend.clj:332 tool-cycle-advance, which reads the current phase and
;; returns the next.
;;
;; So the question "are the remaining seven phases wired?" cannot be answered
;; yet: the phase machine has no advance, so no phase after :register has ever
;; been entered by the machine. Every test that exercises a later phase gets
;; there by assoc'ing :current-phase onto a state by hand.

(ns futon3c.smoke.traverse-test
  "SMOKE TEST: walk the problem peripheral register -> close -> sentinel through
   the runner. Calls every phase tool, then advances with that phase's required
   outputs. Injected provisioner, injected dispatch, injected harness
   measurement: nothing is dispatched and no worktree is created.

   Reports every stop. A stop is a GATE (the machine correctly refusing bad or
   absent input) or a GAP (nothing implemented behind the tool)."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [futon3c.peripheral.problem :as problem]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]))

(def ENVREV "a92ffb6c9cda32a33df0d259df552b1dbc611daf")
(def HARNESS "7c743f777ccdd2b023149149f375e17bf1b1f949")
(def registration
  (edn/read-string (slurp "holes/labs/M-apm-demonstration/round1-registration.edn")))

(defn- mk []
  (problem/make-problem
   (tools/make-mock-backend)
   (fn [& _] {:ok true :job-id "smoke" :environment-checkout
              {:checkout "/tmp/smoke/student" :base-revision ENVREV}})
   "/tmp/smoke-state"
   (fn [{:keys [arm]}] {:checkout (str "/tmp/smoke/" (name (or arm :solver)))
                        :base-revision ENVREV})
   (fn [_] {:harness-revision HARNESS :harness-tree-dirty? false})))

;; What each phase must produce to advance. Environment fields are stamped by
;; the engine from the recorded :assign-checkouts, but must be PRESENT.
(def payloads
  {:register {:registration registration
              :store-snapshot {:snap/id "snap-1" :snap/memory-ids []}
              :stratum-frozen-at 1
              :environment-revision ENVREV
              :harness-revision HARNESS
              :environment-checkouts {}}
   :frame {:frame {:frame/id "frame-1" :frame/scaffold-hash "s" :frame/closing-hash "c"}
           :containment-probe {:cprobe/id "cp-1" :cprobe/frame "frame-1"
                               :cprobe/claimed? true :cprobe/recorded? true}}
   :guided-solve {:solver-attempt {:attempt/id "a-solver" :attempt/seq 0
                                   :cycle/regime "r" :cycle/store-revision "s"
                                   :cycle/runner-freshness :cold}
                  :ground-control-events []
                  :memory-offers []}
   :intervene {:intervention {:kind :store-write}}
   :student-attempts {:student-attempts [{:attempt/id "a-student" :attempt/seq 1
                                          :cycle/regime "r" :cycle/store-revision "s"
                                          :cycle/runner-freshness :cold}]
                      :memory-uses []}
   :adjudicate {:disposition [{:disp/id "d-1" :disp/cycle "c"}]}
   :promote {:promotion-result []}
   :close {}})

(def phase-tools
  (into {} (for [[ph ts] problem/base-phase-tools]
             [ph (vec (remove #{problem/advance} ts))])))

(deftest smoke-traverse
  (let [p (mk)
        ctx {:session-id "smoke" :problem-id "t94J02" :cycle/mode :store-mode
             :harness-repo "/home/joe/code/futon3c"
             :lean-repo "/home/joe/code/mathlib4"
             :agency-endpoint "http://localhost:7070/api/alpha/invoke/jobs?limit=200"
             :authorization-revision (apply str (repeat 40 "a"))
             :authorization-output "/tmp/smoke-auth.edn"}
        begun (runner/step p (:state (runner/start p ctx))
                           {:tool :begin-problem-cycle :args ["M" "C"]})]
    (println "\nBEGIN ->" (get-in begun [:state :current-phase]))
    (loop [state (:state begun), n 0]
      (let [phase (:current-phase state)]
        (cond
          (nil? phase) (println "\n*** REACHED THE SENTINEL after" n "advances ***")
          (> n 12) (println "\n*** gave up at" phase)
          :else
          (let [;; 1. call every tool in the phase
                tool-args {:assign-checkouts
                           [{:problem "t94J02" :batch "frame-1" :base-rev ENVREV
                             :solver-seat "codex-4" :student-seat "zai-1"
                             :recall-system "futon1b"}]}
                state' (reduce (fn [st tool]
                                 (let [r (runner/step p st {:tool tool
                                                            :args (get tool-args tool [])})]
                                   (if (:ok r)
                                     (do (println (format "    tool %-26s ok" tool))
                                         (:state r))
                                     (do (println (format "    tool %-26s STOP %s %s"
                                                          tool (:error/code r)
                                                          (or (:error/message r) "")))
                                         st))))
                               state (get phase-tools phase))
                ;; 2. advance with the phase's required outputs
                r (runner/step p state' {:tool problem/advance
                                         :args ["M" "C" (get payloads phase {})]})]
            (cond
              (and (:ok r) (= phase (get-in r [:state :current-phase])))
              (do (println (format "  %-17s *** NO-OP: advance returned :ok and DID NOT MOVE ***" phase))
                  (println "       payload supplied :" (sort (keys (get payloads phase))))
                  (println "       outputs recorded :" (sort (keys (get-in r [:state :cycle/outputs]))))
                  (println "       => the advance payload is DISCARDED as well as the transition"))

              (:ok r)
              (do (println (format "  %-17s ADVANCED -> %s" phase
                                   (or (get-in r [:state :current-phase]) "(cleared)")))
                  (recur (:state r) (inc n)))
              :else
              (do (println (format "  %-17s STOPS: %s" phase (:error/code r)))
                  (println "      " (pr-str (select-keys (:error/context r)
                                                         [:missing :invariant :failure
                                                          :phase :tool])))))))))
    (is true)))
