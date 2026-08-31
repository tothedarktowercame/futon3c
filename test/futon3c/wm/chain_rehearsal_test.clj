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

(defn- git! [root & args]
  (let [{:keys [exit err]} (apply shell/sh "git" "-C" root args)]
    (when-not (zero? exit)
      (throw (ex-info "fixture git failed" {:args args :err err})))))

(defn- wait-until [pred]
  (let [deadline (+ (System/currentTimeMillis) 5000)]
    (loop []
      (cond (pred) true
            (< (System/currentTimeMillis) deadline) (do (Thread/sleep 5) (recur))
            :else false))))

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
        body (str "(ns futon2.aif.full-loop-runner)\n"
                  "(defn config [x] x)\n"
                  "(defn run-opportunity! [_]\n"
                  "  (spit " (pr-str run-out) " (slurp " (pr-str fixture-run) "))\n"
                  "  {:attempt-id \"rehearsal-attempt\" :outcome :rehearsed})\n")]
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
    futon3c.peripheral.live-wm-selection/validated-selection
    (fn [_] {:selected-policy-id "rehearsal-policy"})
    futon3c.wm.scheduler/ensure-war-machine-agent! (fn [] nil)
    nil))

(defn- resource [run-id]
  {:schema 1 :run/id run-id :source-schema :futon-bounded-test-v1
   :status :clean :reason nil :command-exit 0 :wrapper-exit 0
   :pids-events-max-delta 0 :native-thread-exhaustion false
   :tasks-peak 1 :source-receipt "rehearsal"})

(deftest ^:slow reload-click-certificate-chain-and-mismatch-control
  (let [out-root (.toFile (java.nio.file.Files/createTempDirectory
                           "wm-chain-output-"
                           (make-array java.nio.file.attribute.FileAttribute 0)))
        run-out (.getPath (io/file out-root "tick-run-record-rehearsal.edn"))
        cert-out (.getPath (io/file out-root "certificate.edn"))
        bad-cert-out (.getPath (io/file out-root "certificate-mismatch.edn"))
        good-resource (.getPath (io/file out-root "resource.edn"))
        bad-resource (.getPath (io/file out-root "resource-mismatch.edn"))
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
    (binding [service/*resolve-var* resolver]
      (let [response ((handler) {:request-method :post
                                 :uri "/api/alpha/wm/click"
                                 :body (json/generate-string
                                        {:reviewer "rehearsal-reviewer"})})]
        (is (= 200 (:status response)))
        (is (wait-until #(false? (:running? (service/status)))))))
    (is (.isFile (io/file run-out)))
    (is (= :rehearsed (get-in (service/status) [:last-result :outcome])))
    (load-file certificate-source)
    (let [run (edn/read-string (slurp run-out))
          run-id (:run/id run)
          certificate-main (resolve 'checks.wm-operational-certificate/main)]
      (spit good-resource (str (pr-str (resource run-id)) "\n"))
      (is (= 0 (certificate-main ["--run" run-out "--resource" good-resource
                                  "--certificate" cert-out])))
      (is (= :pass (:verdict (edn/read-string (slurp cert-out)))))
      ;; Broken seam: a resource receipt for a different run cannot certify.
      (spit bad-resource (str (pr-str (resource "different-run")) "\n"))
      (is (= 1 (certificate-main ["--run" run-out "--resource" bad-resource
                                  "--certificate" bad-cert-out])))
      (is (= :fail (:verdict (edn/read-string (slurp bad-cert-out))))))))
