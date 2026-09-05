(ns futon3c.apm.library-lane-smoke-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.library-lane-smoke :as sut]))

(defn- drive-phases
  "Drive the PRODUCTION lane adapter through the smoke step, returning the
  status and phase-update sequence it produces."
  []
  (let [adapter (sut/adapter-constructor {:problem-id "smoke-t00J02"})
        reconcile (:reconcile-fn adapter)]
    (loop [phase :preflight, strategy? false, out []]
      (let [intent {:dispatch/parameters {:phase phase
                                          :strategy-required? strategy?}}
            state {:library/phase phase :library/strategy-required? strategy?}
            r (reconcile intent state)
            next-phase (get-in r [:regulator/state-updates :library/phase])
            row {:phase phase :status (:status r) :ok (:ok r)}]
        (cond
          (not (:ok r)) (conj out (assoc row :error (:error/code r)))
          (nil? next-phase) (conj out row)
          (> (count out) 8) (conj out (assoc row :error :did-not-terminate))
          :else (recur next-phase
                       (boolean (get-in r [:regulator/state-updates
                                           :library/strategy-required?]))
                       (conj out row)))))))

(deftest smoke-walks-the-lane-to-completion-through-the-production-adapter
  ;; C-square's lesson: assert the harness COMPLETES, not that its parts
  ;; typecheck. C-square's tests stubbed process-one! and stayed green for
  ;; eight days while the harness could not run at all.
  (let [rows (drive-phases)]
    (is (every? :ok rows) (str "no step may fail: " (pr-str rows)))
    (is (= sut/expected-phases (mapv :phase rows))
        (str "the production adapter must visit the declared phase order; got "
             (pr-str (mapv :phase rows))))
    (is (= :frame-complete (:status (last rows)))
        "the lane must reach :frame-complete, not merely stop advancing")
    (is (= [:library-phase-certified :library-phase-certified
            :library-phase-certified :frame-complete]
           (mapv :status rows))
        "ruling interpretation is production and must map as declared")))

(deftest smoke-step-is-injected-not-config-borne
  ;; The step must arrive via the adapter constructor. Coordinator config is
  ;; persisted durably, and a function in it serialises as #object[...] which
  ;; no EDN reader can read back -- the registry then poisons every later
  ;; read of that coordinator.
  (let [adapter (sut/adapter-constructor {:problem-id "p"})]
    (is (fn? (:reconcile-fn adapter)))
    (is (fn? (:decide-fn adapter))))
  (is (= :apm/library-lane-smoke sut/adapter-key)
      "the smoke registers its own adapter key rather than reusing the lane's"))

(deftest lean-trace-acceptance-is-reported-absent-not-assumed
  ;; The library lane never calls issue-combined-trace-receipt!, so unlike
  ;; C-square there is no Lean acceptance to assert. Reporting :absent keeps
  ;; that visible instead of implying a check that never ran.
  (is (= :absent (:lean-trace-acceptance (sut/result)))))
