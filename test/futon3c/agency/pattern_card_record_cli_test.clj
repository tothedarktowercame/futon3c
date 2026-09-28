(ns futon3c.agency.pattern-card-record-cli-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.agency.rule-record :as store]
            [futon3c.agency.pattern-card-record-cli :as cli]))

(def harness {:kind :none :basis :producer-context :source-ref "test:p10-2a-3"})
(def selection-record
  {:kind :pattern-card/selection :author "claude-17" :agent "claude-17"
   :session "session-1" :at "2026-09-28T11:00:00Z" :pattern-id "pattern/a"})
(def selection-request {:record selection-record :idempotency-key "p10-selection-1"})
(def target
  {:hx/id "act:card-a" :hx/type :pattern-card/selection
   :hx/valid-time "2026-09-28T11:00:00Z"
   :hx/endpoints ["agent:claude-17" "session:session-1" "pattern:pattern/a"]
   :hx/props (-> selection-record
                 (dissoc :kind)
                 (assoc :act/harness harness :pattern-card/schema 1))})
(def withdrawal-record
  {:kind :act/withdrawal :author "claude-17" :at "2026-09-28T11:30:00Z"
   :target "act:card-a" :status :effective :basis {:kind :self}})
(def withdrawal-request {:record withdrawal-record :idempotency-key "p10-withdraw-1"})

(defn reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest selection-payload-is-minted-and-stamped
  (let [payload (cli/selection-payload selection-request harness)]
    (is (nil? (:hx/id payload)))
    (is (true? (:hx/mint-id payload)))
    (is (= "p10-selection-1" (:hx/idempotency-key payload)))
    (is (= 1 (get-in payload [:hx/props :pattern-card/schema])))
    (is (= (:at selection-record) (get-in payload [:hx/props :at])))
    (is (= harness (get-in payload [:hx/props :act/harness])))))

(deftest withdrawal-payload-validates-the-stored-target
  (let [payload (cli/withdrawal-payload withdrawal-request target harness)]
    (is (true? (:hx/mint-id payload)))
    (is (= :act/withdrawal (:hx/type payload)))
    (is (= "p10-withdraw-1" (:hx/idempotency-key payload)))
    (is (= 1 (get-in payload [:hx/props :pattern-card/schema])))
    (is (= harness (get-in payload [:hx/props :act/harness])))))

(deftest target-refusals-are-typed
  (is (= :target-absent
         (reason #(cli/withdrawal-payload withdrawal-request nil harness))))
  (is (= :target-wrong-type
         (reason #(cli/withdrawal-payload
                   withdrawal-request (assoc target :hx/type :rule/record) harness))))
  (is (= :not-author
         (reason #(cli/withdrawal-payload
                   (assoc-in withdrawal-request [:record :author] "agent-b")
                   target harness)))))

(deftest live-target-uses-http-and-translates-absence
  (testing "stored target"
    (with-redefs [store/request! (fn [base method path body]
                                   (is (= "http://store" base))
                                   (is (= "GET" method))
                                   (is (= "/api/alpha/hyperedge/act%3Acard-a" path))
                                   (is (nil? body))
                                   target)]
      (is (= target (cli/live-target! "http://store" "act:card-a")))))
  (testing "404"
    (with-redefs [store/request! (fn [& _]
                                   (throw (ex-info "missing" {:status 404})))]
      (is (= :target-absent
             (reason #(cli/live-target! "http://store" "act:missing")))))))

(deftest harness-is-required-and-caller-storage-fields-are-refused
  (is (= :invalid-harness-map
         (reason #(cli/selection-payload selection-request nil))))
  (is (= :caller-assigned-storage-field
         (reason #(cli/selection-payload
                   (assoc-in selection-request [:record :id] "act:caller") harness))))
  (is (= :caller-assigned-storage-field
         (reason #(cli/selection-payload
                   (assoc-in selection-request [:record :act/harness] harness) harness)))))
