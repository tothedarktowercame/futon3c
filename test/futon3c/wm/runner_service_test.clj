(ns futon3c.wm.runner-service-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.agency.registry :as reg]
            [futon3c.social.test-fixtures :as fix]
            [futon3c.transport.http :as http]
            [futon3c.wm.runner-service :as service])
  (:import [java.time Instant]))

(def scratch-agent-id "war-machine")

(defn- reset-service!
  [f]
  (reg/reset-registry!)
  (reset! service/!status service/initial-status)
  (reg/register-agent!
   {:agent-id {:id/value scratch-agent-id :id/type :apparatus}
    :type :wm
    :invoke-fn nil
    :capabilities []
    :metadata {:apparatus? true}})
  (f))

(use-fixtures :each reset-service!)

(defn- handler []
  (http/make-handler
   {:registry (fix/mock-registry)
    :patterns (fix/mock-patterns)}))

(defn- response-body [response]
  (json/parse-string (:body response) true))

(defn- wait-until
  [predicate timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (cond
        (predicate) true
        (< (System/currentTimeMillis) deadline)
        (do (Thread/sleep 5) (recur))
        :else false))))

(defn- resolver
  [run! select]
  (fn [sym]
    (case sym
      futon2.aif.full-loop-runner/config identity
      futon2.aif.full-loop-runner/run-opportunity! run!
      futon3c.peripheral.live-wm-selection/validated-selection select
      futon3c.wm.scheduler/ensure-war-machine-agent! (fn [] nil)
      nil)))

(deftest single-flight-routes-and-registry-status
  (let [release (promise)
        phase-seen (promise)
        selection-request (atom nil)
        runner-opts-seen (atom nil)
        runner-thread-seen (atom nil)
        runner-daemon-seen (atom nil)
        run!
        (fn [opts]
          (reset! runner-opts-seen opts)
          (reset! runner-thread-seen (.getName (Thread/currentThread)))
          (reset! runner-daemon-seen (.isDaemon (Thread/currentThread)))
          (let [selection ((:strategic-selection-invoke-fn opts)
                           {:scheduler-habit-ranking ["M-test"]
                            :trace-id "trace-test"})]
            (is (= {:ok true :selection {:selected-policy-id "pi-test"}}
                   selection)))
          ((:phase-log-fn opts)
           {:phase :author-wait
            :transition :start
            :attempt-id "attempt-test"})
          (deliver phase-seen true)
          @release
          ((:phase-log-fn opts)
           {:phase :opportunity
            :transition :end
            :attempt-id "attempt-test"
            :outcome :grounded-change})
          {:attempt-id "attempt-test" :outcome :grounded-change})
        select (fn [request]
                 (reset! selection-request request)
                 {:selected-policy-id "pi-test"})
        h (handler)]
    (binding [service/*resolve-var* (resolver run! select)]
      (let [first-response
            (h {:request-method :post
                :uri "/api/alpha/wm/click"
                :body (json/generate-string
                       {:author "zai-2"
                        :reviewer "codex-2"
                        :repair-reviewer "codex-1"})})
            first-body (response-body first-response)
            _ (is (= true (deref phase-seen 1000 false)))
            second-response
            (h {:request-method :post
                :uri "/api/alpha/wm/click"
                :body "{}"})
            second-body (response-body second-response)
            get-response
            (h {:request-method :get :uri "/api/alpha/wm/click"})
            get-body (response-body get-response)
            invoking-agent (reg/get-agent scratch-agent-id)
            active-at (:agent/last-active invoking-agent)]
        (is (= 200 (:status first-response)))
        (is (string? (:click-id first-body)))
        (is (string? (:started-at first-body)))
        (is (= 409 (:status second-response)))
        (is (= "already-running" (:rejected second-body)))
        (is (= (:click-id first-body) (:click-id second-body)))
        (is (= 200 (:status get-response)))
        (is (true? (:running? get-body)))
        (is (= "author-wait" (:phase get-body)))
        (is (= "attempt-test" (:attempt-id get-body)))
        (is (= :invoking (:agent/status invoking-agent)))
        (is (= "author-wait attempt-test"
               (:agent/invoke-activity invoking-agent)))
        (is (instance? Instant active-at))
        (is (= {:scheduler-habit-ranking ["M-test"]
                :trace-id "trace-test"}
               @selection-request))
        (is (= "wm-runner-click" @runner-thread-seen))
        (is (true? @runner-daemon-seen))
        (is (= "zai-2" (:author @runner-opts-seen)))
        (is (= "codex-2" (:reviewer @runner-opts-seen)))
        (is (= "codex-1" (:repair-reviewer @runner-opts-seen)))
        (deliver release true)
        (is (wait-until #(false? (:running? (service/status))) 5000))
        (let [closed (service/status)
              idle-agent (reg/get-agent scratch-agent-id)]
          (is (= :idle (:agent/status idle-agent)))
          (is (nil? (:agent/invoke-activity idle-agent)))
          (is (not (.isBefore ^Instant (:agent/last-active idle-agent)
                              ^Instant active-at)))
          (is (= {:attempt-id "attempt-test"
                  :outcome :grounded-change}
                 (:last-result closed))))))))

(deftest service-source-has-no-process-spawn-surface
  (let [source (slurp
                (io/file "src/futon3c/wm/runner_service.clj"))]
    (testing "the in-process service does not import or invoke process APIs"
      (is (not (str/includes? source "clojure.java.shell")))
      (is (not (str/includes? source "ProcessBuilder")))
      (is (not (re-find #"\(\s*sh/" source))))))

(deftest runner-resolution-failure-releases-single-flight
  (binding [service/*resolve-var*
            (fn [sym]
              (case sym
                futon2.aif.full-loop-runner/config identity
                (throw (ex-info "runner unavailable" {:symbol sym}))))]
    (let [started (service/click! {:wm-agent-id scratch-agent-id})]
      (is (string? (:click-id started)))
      (is (wait-until #(false? (:running? (service/status))) 5000))
      (is (= :service-failed
             (get-in (service/status) [:last-result :outcome])))
      (is (= :idle
             (:agent/status (reg/get-agent scratch-agent-id)))))))
