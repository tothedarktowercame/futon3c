(ns futon3c.wm.clean-click-admission-test
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.load-identity :as identity]
            [futon3c.wm.runner-service :as service]))

(defn admission-error [opts]
  (try (service/click! opts) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(deftest production-configuration-does-not-load-a-missing-runner
  (let [loads (atom 0) find-original find-ns
        before @identity/registry]
    (with-redefs-fn {(ns-resolve 'futon3c.wm.machinery-execution-cohort 'apply-binding) identity
                    (ns-resolve 'futon3c.wm.runner-service 'configure-self-heal-ports) identity
                    #'clojure.core/find-ns (fn [n] (when-not (= n 'futon2.aif.full-loop-runner)
                                                   (find-original n)))
                    #'clojure.core/requiring-resolve (fn [_] (swap! loads inc) identity)}
      (fn []
        (binding [service/*resolve-var* requiring-resolve]
          (is (= {:fixture true}
                 ((ns-resolve 'futon3c.wm.runner-service 'configured-runner-opts) {:fixture true})))
          (is (zero? @loads))
          (is (= before @identity/registry)))))))

(deftest stale-dependency-cannot-allocate-or-charge
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       (java.nio.file.Paths/get "/home/joe/notes" (make-array String 0))
                       "clean-admission-" (make-array java.nio.file.attribute.FileAttribute 0)))
        charges (atom 0) workers (atom 0)
        saved @service/!status
        config (ns-resolve 'futon3c.wm.runner-service 'configured-runner-opts)
        worker (ns-resolve 'futon3c.wm.runner-service 'run-click!)
        scope ['futon2.aif.full-loop-runtime 'futon2.report.war-machine
               'futon2.aif.meta-live-outer-selector 'futon2.aif.meta-pipeline-selector
               'futon2.aif.wm.family-selection 'futon2.aif.wm.family-canonical-g]]
    (try
      (with-redefs-fn {config identity
                      #'service/meta-self-heal-preflight! identity
                      worker (fn [& _] (swap! workers inc))}
        (fn []
          (binding [service/*click-acceptance-dir* (.getPath root)]
            (doseq [bad [:stale :unregistered :unavailable]]
              (reset! service/!status service/initial-status)
              (let [before @service/!status]
                (with-redefs [identity/report (fn [& _]
                                               (assoc (zipmap scope (repeat {:status :current}))
                                                      'futon2.aif.wm.family-canonical-g {:status bad}))]
                  (is (= :wm-click-launch-unavailable
                         (:error (admission-error {:ordinary-click/issue! (fn [& _] (swap! charges inc))}))))
                  (is (= before @service/!status))
                  (is (empty? (seq (.listFiles root)))))))
            (is (zero? @charges))
            (is (zero? @workers)))))
      (finally (reset! service/!status saved)))))

(deftest launch-replay-conflict-and-running-order
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       (java.nio.file.Paths/get "/home/joe/notes" (make-array String 0))
                       "clean-replay-" (make-array java.nio.file.attribute.FileAttribute 0)))
        state (atom :current) checks (atom 0) charges (atom 0)
        worker-ran (promise) saved @service/!status
        opts {:idempotency-key "same" :acceptance-payload {:x 1}
              :ordinary-click/issue! (fn [& _] (swap! charges inc))}]
    (try
      (reset! service/!status service/initial-status)
      (with-redefs-fn {(ns-resolve 'futon3c.wm.runner-service 'configured-runner-opts) identity
                      #'service/meta-self-heal-preflight! identity
                      (ns-resolve 'futon3c.wm.runner-service 'run-click!)
                      (fn [& _] (deliver worker-ran true))}
        (fn []
          (binding [service/*click-acceptance-dir* (.getPath root)
                    service/*source-readiness* #(do (swap! checks inc) {:status @state})]
            (let [accepted (service/click! opts)]
              (is (= :queued (:status accepted)))
              (is (= true (deref worker-ran 1000 false)))
              (is (= 1 @charges))
              (reset! state :stale)
              (is (= (:click-id accepted) (:click-id (service/click! opts))))
              (is (= :wm-click-idempotency-conflict
                     (:error (admission-error (assoc opts :acceptance-payload {:x 2})))))
              (is (= :already-running (:rejected (service/click! {}))))
              (is (= 1 @checks) "replay/conflict/running never consult readiness")
              (is (= :wm-click-run-id-malformed (:error (admission-error {:run-id ""}))))
              (is (= 1 @charges))))))
      (finally (reset! service/!status saved)))))

(deftest cas-conflict-rechecks-readiness-before-launch
  (let [saved @service/!status checks (atom 0) charges (atom 0)
        cas compare-and-set!]
    (try
      (reset! service/!status service/initial-status)
      (with-redefs-fn {(ns-resolve 'futon3c.wm.runner-service 'configured-runner-opts) identity
                      #'service/meta-self-heal-preflight! identity
                      #'clojure.core/compare-and-set!
                      (fn [a old new] (if (identical? a service/!status) false (cas a old new)))}
        (fn []
          (binding [service/*source-readiness* #(if (= 1 (swap! checks inc))
                                                {:status :current} {:status :unavailable})]
            (is (= :wm-click-launch-unavailable
                   (:error (admission-error {:ordinary-click/issue! (fn [& _] (swap! charges inc))}))))
            (is (= 2 @checks))
            (is (zero? @charges))
            (is (= service/initial-status @service/!status)))))
      (finally (reset! service/!status saved)))))
