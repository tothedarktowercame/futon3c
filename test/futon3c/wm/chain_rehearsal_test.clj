(ns futon3c.wm.chain-rehearsal-test
  "Throwaway reload -> HTTP click -> operational certificate rehearsal.

   This crosses the real in-process boundaries without contacting the serving
   JVM or production HTTP listener. The runner body and selection are fixtures."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.agency.registry :as reg]
            [futon3c.social.test-fixtures :as fix]
            [futon3c.transport.http :as http]
            [futon3c.wm.code-identity :as identity]
            [futon3c.wm.runner-service :as service]))

(def fixture-run
  "/home/joe/code/futon2/holes/labs/wm-contract/tick-run-record-2026-08-31.edn")
(def certificate-source
  "/home/joe/code/futon2/checks/wm_operational_certificate.clj")
(def observer-source
  "/home/joe/code/futon2/checks/wm_click_resource_observer.clj")

(defn- git! [root & args]
  (let [{:keys [exit err]} (apply shell/sh "git" "-C" root args)]
    (when-not (zero? exit)
      (throw (ex-info "fixture git failed" {:args args :err err})))))

(defn- handler []
  (http/make-handler {:registry (fix/mock-registry)
                      :patterns (fix/mock-patterns)}))

(defn- register-apparatus! []
  (reg/reset-registry!)
  (reg/register-agent!
   {:agent-id {:id/value "war-machine" :id/type :apparatus}
    :type :wm :invoke-fn nil :capabilities [] :metadata {:apparatus? true}}))

(defn- fixture-repo [run-out]
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "wm-chain-rehearsal-"
                       (make-array java.nio.file.attribute.FileAttribute 0)))
        source (io/file root identity/production-runner)
        body (str "(ns futon2.aif.full-loop-runner (:require [clojure.edn :as edn]))\n"
                  "(defn config [x] x)\n"
                  "(defn run-opportunity! [_]\n"
                  "  (let [run (assoc (edn/read-string (slurp " (pr-str fixture-run) "))\n"
                  "                   :startedAt (str (java.time.Instant/now))\n"
                  "                   :click/id (:click-id _))]\n"
                  "    (spit " (pr-str run-out) " (pr-str run))\n"
                  "    {:attempt-id \"rehearsal-attempt\" :outcome :grounded-no-change\n"
                  "     :run/id (:run/id run) :run-record-status :present\n"
                  "     :run-record " (pr-str run-out) "}))\n")]
    (io/make-parents source)
    (spit source body)
    (git! (.getPath root) "init" "-q")
    (git! (.getPath root) "config" "user.email" "test@example.invalid")
    (git! (.getPath root) "config" "user.name" "Test")
    (git! (.getPath root) "add" identity/production-runner)
    (git! (.getPath root) "commit" "-qm" "fixture runner")
    {:root root :source source}))

(defn- resolver [sym]
  (case sym
    futon2.aif.full-loop-runner/config
    (resolve 'futon2.aif.full-loop-runner/config)
    futon2.aif.full-loop-runner/run-opportunity!
    (resolve 'futon2.aif.full-loop-runner/run-opportunity!)
    futon3c.wm.code-identity/status identity/status
    futon3c.peripheral.live-wm-selection/validated-selection
    (fn [_] {:selected-policy-id "rehearsal-policy"})
    futon3c.wm.scheduler/ensure-war-machine-agent! (fn [] nil)
    nil))

(deftest ^:slow reload-click-certificate-chain-and-mismatch-control
  (let [out-root (.toFile (java.nio.file.Files/createTempDirectory
                           "wm-chain-output-"
                           (make-array java.nio.file.attribute.FileAttribute 0)))
        run-out (.getPath (io/file out-root "tick-run-record-rehearsal.edn"))
        cert-out (.getPath (io/file out-root "certificate.edn"))
        bad-cert-out (.getPath (io/file out-root "certificate-mismatch.edn"))
        good-resource (.getPath (io/file out-root "resource.edn"))
        bad-resource (.getPath (io/file out-root "resource-mismatch.edn"))
        binding-dir (.getPath (io/file out-root "bindings"))
        observed-receipt (atom nil)
        {:keys [root source]} (fixture-repo run-out)]
    (identity/reset-for-test!)
    (reset! service/!status service/initial-status)
    (register-apparatus!)
    (binding [identity/*futon2-root* (.getPath root)]
      (identity/load-file-recorded! (.getPath source)))
    (let [loaded (:identity (identity/status))
          head (str/trim (:out (shell/sh "git" "-C" (.getPath root) "rev-parse" "HEAD")))]
      (is (= :available (:availability (identity/status))))
      (is (= head (:git-head loaded)))
      (is (false? (:dirty? loaded)))
      (is (true? (:stable? loaded))))
    (load-file observer-source)
    (binding [service/*resolve-var* resolver
              service/*click-run-binding-dir* binding-dir]
      (let [h (handler)
            observe! (resolve 'checks.wm-click-resource-observer/observe!)]
        (reset! observed-receipt
                (observe! {:payload {:reviewer "rehearsal-reviewer"}
                           :post-click #(json/parse-string
                                         (:body (h {:request-method :post
                                                    :uri "/api/alpha/wm/click"
                                                    :body (json/generate-string %)})) true)
                           :status service/status
                           :cgroup-sample (constantly {:cgroup "/fixture-serving"
                                                       :pids-events-max 0
                                                       :pids-current 12})
                           :journal-sample (constantly {:readable? true
                                                        :native-thread-markers []})
                           :sleep-ms (fn [_] (Thread/sleep 5))})))
      (is (= "clean" (:resource-status @observed-receipt))))
    (is (.isFile (io/file run-out)))
    (is (= :grounded-no-change
           (get-in (service/status) [:last-result :outcome])))
    (is (= :verified (get-in (service/status) [:last-result :binding-status])))
    (is (= :present (get-in (service/status) [:last-result :run-record-status])))
    (load-file certificate-source)
    (let [run (edn/read-string (slurp run-out))
          run-id (get-in (service/status)
                         [:last-result :run-id-observation :value])
          binding-record (edn/read-string
                          (slurp (get-in (service/status)
                                         [:last-result :run-binding])))
          certificate-main (resolve 'checks.wm-operational-certificate/main)
          normalize (resolve 'checks.wm-click-resource-observer/certificate-resource)
          fixture-head (get-in @observed-receipt
                               [:serving-runner-code :identity :git-head])
          normalized (assoc (normalize run-id "rehearsal-observer"
                                       @observed-receipt)
                            ;; The rehearsal's committed throwaway repository
                            ;; is exercised by this test rather than by the
                            ;; production bounded-job producer. Keep that
                            ;; fixture scope explicit; production certification
                            ;; still requires a producer-bound tested job.
                            :tested-commit fixture-head
                            :tested-job-id "chain-rehearsal-fixture"
                            :tested-attempt "throwaway-jvm")]
      ;; The join now crosses the production service port. The fixture record
      ;; and durable binding must independently resolve the same run id.
      (is (= run-id
             (:run/id run)
             (get-in binding-record [:run-id-observation :value])))
      (is (= (:click/id binding-record) (:click-id (service/status))))
      (is (= (:click/id run) (:click/id binding-record)))
      (spit good-resource
            (str (pr-str normalized) "\n"))
      (is (= 0 (certificate-main ["--run" run-out "--resource" good-resource
                                  "--certificate" cert-out])))
      (let [certificate (edn/read-string (slurp cert-out))]
        (is (= :pass (:verdict certificate)))
        (is (= :match (get-in certificate [:program-identity-status :status])))
        (is (= fixture-head
               (get-in certificate [:program-identity-status :tested-commit]))))
      ;; Broken seam: a resource receipt for a different run cannot certify.
      (spit bad-resource
            (str (pr-str (assoc normalized :run/id "different-run")) "\n"))
      (is (= 1 (certificate-main ["--run" run-out "--resource" bad-resource
                                  "--certificate" bad-cert-out])))
      (is (= :fail (:verdict (edn/read-string (slurp bad-cert-out))))))))
