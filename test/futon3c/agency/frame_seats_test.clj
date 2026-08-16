(ns futon3c.agency.frame-seats-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.agency.frame-seats :as frame-seats]
            [futon3c.agency.registry :as registry]
            [futon3c.transport.http :as http]
            [jsonista.core :as json]))

(use-fixtures :each
  (fn [f]
    (registry/reset-registry!)
    (try (f) (finally (registry/reset-registry!)))))

(defn- ready-seat [{:keys [agent-id agent-type]}]
  {:invoke-fn (fn [_prompt _session-id]
                {:result (str "fresh " agent-id) :session-id nil})
   :metadata {:fixture-type agent-type}})

(deftest mint-registers-five-fresh-invoke-ready-seats
  (let [calls (atom [])
        result (frame-seats/mint-seats!
                {:prepare-seat-fn (fn [seat]
                                    (swap! calls conj seat)
                                    (ready-seat seat))}
                "frame-17")
        expected {:reg/solver-seat "frame-17-solver"
                  :reg/student-seat "frame-17-student"
                  :reg/guide-seat "frame-17-guide"
                  :reg/proctor-seat "frame-17-proctor"
                  :reg/scribe-seat "frame-17-scribe"}]
    (is (:ok result))
    (is (= expected (:seats result)))
    (is (= 5 (count @calls)))
    (doseq [[seat-key agent-id] expected]
      (let [agent (registry/get-agent agent-id)
            roster (get-in (registry/registry-status) [:agents agent-id])]
        (is (some? agent) (name seat-key))
        (is (nil? (:agent/session-id agent)) (name seat-key))
        (is (true? (:invoke-ready? roster)) (name seat-key))
        (is (true? (get-in agent [:agent/metadata :fresh-session?])) (name seat-key))))))

(deftest mint-is-idempotent-per-frame
  (let [calls (atom 0)
        opts {:prepare-seat-fn (fn [seat]
                                (swap! calls inc)
                                (ready-seat seat))}
        first-result (frame-seats/mint-seats! opts "same-frame")
        second-result (frame-seats/mint-seats! opts "same-frame")]
    (is (= first-result second-result))
    (is (= 5 @calls))
    (is (= 5 (count (registry/registered-agents))))))

(deftest non-invocable-seat-is-a-structured-finding
  (let [result
        (frame-seats/mint-seats!
         {:prepare-seat-fn
          (fn [{:keys [agent-type] :as seat}]
            (if (= :zai agent-type)
              {:invoke-fn nil :reason :adapter-unavailable}
              (ready-seat seat)))}
         "broken-frame")]
    (is (false? (:ok result)))
    (is (= :seat-mint-incomplete (:error result)))
    (is (some #(and (= :seat-not-invoke-ready (:finding %))
                    (= :reg/student-seat (:seat %)))
              (:findings result)))
    (is (nil? (:seats result)))))

(deftest mint-seats-http-route
  (let [handler (http/make-handler {:frame-seat-prepare-fn ready-seat})
        response (handler {:request-method :post
                           :uri "/api/alpha/frames/mint-seats"
                           :body (java.io.ByteArrayInputStream.
                                  (.getBytes (json/write-value-as-string
                                              {:frame-id "http-frame"})
                                             "UTF-8"))})
        body (json/read-value (:body response) json/keyword-keys-object-mapper)]
    (is (= 200 (:status response)))
    (is (true? (:ok body)))
    (is (= "http-frame-solver" (get-in body [:seats :reg/solver-seat])))
    (is (= 5 (count (:seats body))))))
