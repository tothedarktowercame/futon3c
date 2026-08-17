(ns futon3c.apm.conductor-open-test
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.agency.registry :as agency]
            [futon3c.apm.conductor-binding :as binding]
            [futon3c.apm.conductor-open :as conductor-open]
            [futon3c.peripheral.problem :as problem]
            [futon3c.peripheral.tools :as tools]
            [futon3c.transport.http :as http])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute FileTime]))

(def ^:private f7-registration
  (edn/read-string
   (slurp "holes/labs/M-apm-demonstration/frame-7-registration.edn")))

(use-fixtures :each
  (fn [test-fn]
    (agency/reset-registry!)
    (binding/reset-bindings!)
    (try (test-fn)
         (finally
           (binding/reset-bindings!)
           (agency/reset-registry!)))))

(defn- register-guide! [session-id]
  (agency/register-agent!
   {:agent-id "f7-guide" :type :claude
    :invoke-fn (fn [_ _] {:result "unused" :session-id session-id})})
  (agency/update-agent! "f7-guide" :agent/session-id session-id))

(defn- fixture []
  (let [root (Files/createTempDirectory "conductor-open-state-"
                                        (make-array FileAttribute 0))
        registration (Files/createTempFile "conductor-open-registration-" ".edn"
                                           (make-array FileAttribute 0))
        scaffold (Files/createTempFile "conductor-open-scaffold-" ".md"
                                       (make-array FileAttribute 0))
        closing (Files/createTempFile "conductor-open-closing-" ".md"
                                      (make-array FileAttribute 0))
        witness (Files/createTempFile "conductor-open-witness-" ".md"
                                      (make-array FileAttribute 0))
        env-revision (:reg/environment-revision f7-registration)
        harness-revision (:reg/harness-revision f7-registration)
        peripheral
        (problem/make-problem
         (tools/make-mock-backend)
         (fn [& _] {:ok true :job-id "unused"})
         (str root)
         (fn [{:keys [arm branch batch]}]
           {:checkout (str "/tmp/f7/" arm)
            :base-revision env-revision :branch branch
            :frame/id (str batch "-" arm) :batch batch})
         (fn [_] {:harness-revision harness-revision
                  :harness-tree-dirty? false})
         (constantly [])
         (constantly 1))]
    (spit (.toFile registration) (pr-str f7-registration))
    (spit (.toFile scaffold) "scaffold\n")
    (spit (.toFile closing) "closing\n")
    (spit (.toFile witness) "contained\n")
    (Files/setLastModifiedTime scaffold (FileTime/fromMillis 1000))
    (Files/setLastModifiedTime closing (FileTime/fromMillis 2000))
    {:payload {:registration-path (str registration)
               :problem-id "a98A01" :mode :store-mode :batch "frame-7"
               :frame {:scaffold (str scaffold) :closing (str closing)
                       :witness (str witness)}}
     :options {:peripheral peripheral
               :harness-measurer
               (fn [_] {:harness-revision harness-revision
                        :harness-tree-dirty? false})
               :evidence-store (atom {:entries {} :order []})}
     :registration registration
     :paths [registration scaffold closing witness]}))

(deftest production-open-route-binds-the-registered-guide
  (let [{:keys [payload options paths]} (fixture)
        session-id "fresh-f7-guide-session"
        handler (http/make-handler {:conductor-open-options options})]
    (register-guide! session-id)
    (try
      (let [response (handler {:request-method :post
                               :uri "/api/alpha/conductor/open"
                               :body (json/generate-string payload)})
            body (json/parse-string (:body response) true)]
        (is (= 200 (:status response)) (pr-str body))
        (is (:ok body))
        (is (string? (:cycle-id body)))
        (is (integer? (:version body)))
        (is (= :guided-solve (keyword (:phase body))))
        (is (some? (binding/lookup "f7-guide" session-id)))
        (is (nil? (binding/lookup "http-caller" session-id)))
        (is (= "f7-solver" (get-in body [:seats :reg/solver-seat]))))
      (finally
        (doseq [path paths] (Files/deleteIfExists path))))))

(deftest unstaffed-registration-surfaces-the-p5-finding-verbatim
  (let [{:keys [payload options registration paths]} (fixture)
        unstaffed (-> f7-registration
                      (assoc :reg/role-cards
                             {:guide (apply str (repeat 40 "a"))})
                      (dissoc :reg/guide-seat))]
    (spit (.toFile registration) (pr-str unstaffed))
    (try
      (let [result (conductor-open/open! payload options)]
        (is (false? (:ok result)))
        (is (= :registration-shape-invalid (:error/code result)))
        (is (some #(= {:finding :unstaffed-carded-seat
                       :role :guide :seat-key :reg/guide-seat}
                     %)
                  (:findings result))))
      (finally
        (doseq [path paths] (Files/deleteIfExists path))))))

(deftest second-open-refuses-before-running-the-machine-again
  (let [{:keys [payload options paths]} (fixture)
        calls (atom 0)
        session-id "f7-guide-session"
        wrapped-options
        (assoc options :open-frame-fn
               (fn [config]
                 (swap! calls inc)
                 ((requiring-resolve 'futon3c.apm.conductor/open-frame!) config)))]
    (register-guide! session-id)
    (try
      (let [first-result (conductor-open/open! payload wrapped-options)
            second-result (conductor-open/open! payload wrapped-options)]
        (is (:ok first-result))
        (is (false? (:ok second-result)))
        (is (= :conductor-binding-exists (:error/code second-result)))
        (is (= 1 @calls)))
      (finally
        (doseq [path paths] (Files/deleteIfExists path))))))

(deftest stale-harness-pin-refuses-before-opening-or-provisioning
  (let [{:keys [payload options paths]} (fixture)
        opens (atom 0)
        session-id "stale-pin-guide-session"
        measured (apply str (repeat 40 "a"))]
    (register-guide! session-id)
    (try
      (let [result (conductor-open/open!
                    payload
                    (assoc options
                           :harness-measurer
                           (fn [_] {:harness-revision measured
                                    :harness-tree-dirty? false})
                           :open-frame-fn
                           (fn [_]
                             (swap! opens inc)
                             {:ok false :error/code :must-not-open})))]
        (is (false? (:ok result)))
        (is (= :harness-pin-stale (:error/code result)))
        (is (= (:reg/harness-revision f7-registration) (:pinned result)))
        (is (= measured (:measured result)))
        (is (zero? @opens))
        (is (nil? (binding/lookup "f7-guide" session-id))))
      (finally
        (doseq [path paths] (Files/deleteIfExists path))))))

(deftest production-open-threads-the-optional-memory-cascade-flag
  (let [{:keys [payload options paths]} (fixture)
        captured (atom nil)
        session-id "cascade-flag-guide-session"]
    (register-guide! session-id)
    (try
      (let [result (conductor-open/open!
                    (assoc payload :memory-cascade-enabled? true)
                    (assoc options :open-frame-fn
                           (fn [config]
                             (reset! captured config)
                             {:ok true :cycle-id "cycle/test"
                              :state {:current-phase :guided-solve}})))]
        (is (:ok result))
        (is (true? (:memory-cascade-enabled? @captured))))
      (finally
        (doseq [path paths] (Files/deleteIfExists path))))))
