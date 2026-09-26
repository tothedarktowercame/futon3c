;; REPRODUCTION, 2026-08-15. Kept OUT of test/ deliberately: it currently
;; demonstrates a gap rather than guarding a behaviour. Copy into
;; test/futon3c/repro/ to run.
;;
;; FINDING: :begin-problem-cycle returns {:ok true} and opens NO CYCLE.
;; The engine opens a cycle only when the tool result carries :cycle/id
;; (cycle.clj:396). Nothing in the problem peripheral produces one: the mock
;; backend answers any unknown tool with {:ok true :result nil}, and none of the
;; three wrappers (provisioning, ground-control, state-IO) implement the tool.
;; The sibling peripherals DO have one -- proof_backend.clj:250 tool-cycle-begin
;; -- there is simply no problem_backend.clj.
;;
;; Undetected because no test advances through the phases: all 126 jump in by
;; assoc'ing :current-phase and :current-cycle-id by hand (9 occurrences).

(ns futon3c.repro.dry-traverse-test
  "DRY TRAVERSE: drive the problem peripheral register -> close through the
   runner, with the mock backend, an injected provisioner and an injected
   harness measurement. Dispatches nothing, creates no worktrees. The point is
   that no existing test advances through the phases at all -- every close-phase
   test jumps in by assoc'ing :current-phase."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.peripheral.problem :as problem]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]))

(def HARNESS "7c743f777ccdd2b023149149f375e17bf1b1f949")
(def ENVREV  "a92ffb6c9cda32a33df0d259df552b1dbc611daf")

(defn- provisioner [{:keys [arm]}]
  {:checkout (str "/tmp/dry/" (name (or arm :solver))) :base-revision ENVREV})

(defn- mk []
  (problem/make-problem (tools/make-mock-backend)
                        (fn [& _] {:ok true :job-id "dry"})
                        "/tmp/dry-state"
                        provisioner
                        (fn [_] {:harness-revision HARNESS
                                 :harness-tree-dirty? false})))

(deftest dry-traverse
  (let [p (mk)
        s0 (:state (runner/start p {:session-id "dry" :problem-id "t94J02"
                                    :cycle/mode :store-mode
                                    :harness-repo "/home/joe/code/futon3c"
                                    :lean-repo "/home/joe/code/mathlib4"
                                    :agency-endpoint "http://localhost:7070/api/alpha/invoke/jobs?limit=200"
                                    :authorization-revision (apply str (repeat 40 "a"))
                                    :authorization-output "/tmp/dry-auth.edn"}))
        r0 (runner/step p s0 {:tool :begin-problem-cycle :args ["M" "C"]})]
    (println "\n=== begin ok:" (:ok r0) " err:" (:error/code r0))
    (println "=== result keys:" (sort (keys (:result r0))))
    (println "=== state keys :" (sort (keys (:state r0))))
    (println "=== phase      :" (get-in r0 [:state :current-phase])
             " cycle-id:" (get-in r0 [:state :current-cycle-id]))
    (loop [state (:state r0), n 0]
      (let [phase (:current-phase state)]
        (cond
          (nil? phase) (println "=== cycle CLEARED after" n "advances (reached the sentinel)")
          (> n 12) (println "=== gave up after 12 advances at" phase)
          :else
          (let [r (runner/step p state {:tool problem/advance :args ["M" "C" {}]})]
            (if (:ok r)
              (do (println (format "  %-18s -> %s" phase (get-in r [:state :current-phase])))
                  (recur (:state r) (inc n)))
              (do (println (format "  %-18s STOPS: %s" phase (:error/code r)))
                  (println "    missing:" (or (get-in r [:error/context :missing])
                                              (get-in r [:error/context :invariant])
                                              (dissoc (:error/context r) :state)))))))))
    (is true)))
