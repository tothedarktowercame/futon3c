(ns futon3c.agency.prompt-line-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.agency.prompt-line :as prompt-line]))

(def now "2026-09-27T20:00:00Z")

(defn registration
  ([id provider f] (registration id provider f 100))
  ([id provider f budget]
   {:segment/id id :provider provider :fn f :budget-ms budget}))

(defn segment [id provider extra]
  (merge {:segment/id id :segment/provider provider
          :segment/observed-at now
          :segment/basis {:evidence-ref "e-test" :scope {}}}
         extra))

(use-fixtures :each (fn [f] (prompt-line/reset-registry!) (f)))

(deftest pure-render-composes-pattern-and-ordered-markers
  (is (= "> " (:prompt (prompt-line/render {:render-at now} []))))
  (let [providers [(registration :pattern "pattern"
                                 (fn [_] (segment :pattern "pattern"
                                                  {:segment/value "~象/诺必践"})))
                   (registration :first "first"
                                 (fn [_] (segment :first "first"
                                                  {:segment/marker "*"})))
                   (registration :inbox-zero "inbox"
                                 (fn [_] (segment :inbox-zero "inbox"
                                                  {:segment/marker "?"
                                                   :segment/header "unattributed"})))]
        result (prompt-line/render {:render-at now} providers)]
    (is (= "$~象/诺必践*?> " (:prompt result)))
    (is (= [:pattern :first :inbox-zero] (mapv :segment/id (:segments result))))))

(deftest provider-omissions-are-typed-and-bounded
  (let [providers [(registration :slow "slow" (fn [_] (Thread/sleep 2000)) 25)
                   (registration :throwing "throwing" (fn [_] (throw (ex-info "no" {}))))
                   (registration :nothing "nothing" (constantly nil))
                   (registration :invalid "invalid" (constantly {:segment/id :invalid}))]
        started (System/nanoTime)
        result (prompt-line/render {:render-at now} providers)
        elapsed-ms (/ (- (System/nanoTime) started) 1000000.0)]
    (is (< elapsed-ms 250.0) (str "render took " elapsed-ms " ms"))
    (is (= [{:segment/id :slow :reason :timeout}
            {:segment/id :throwing :reason :error}
            {:segment/id :nothing :reason :nil}
            {:segment/id :invalid :reason :invalid}]
           (:omitted result)))
    (is (= "> " (:prompt result)))))

(deftest duplicate-provider-is-refused
  (prompt-line/register-provider! (registration :one "a" (constantly nil)))
  (is (= "a" (:provider (prompt-line/register-provider!
                          (registration :one "a" (constantly nil))))))
  (is (= :duplicate-segment-provider
         (:error/code (ex-data
                       (try
                         (prompt-line/register-provider!
                          (registration :one "b" (constantly nil)))
                         (catch Exception e e)))))))

(deftest render-bang-remembers-exact-seat
  (prompt-line/register-provider!
   (registration :inbox-zero "inbox"
                 (fn [_] (segment :inbox-zero "inbox" {:segment/marker "?"}))))
  (let [result (prompt-line/render! {:agent-id "claude-17" :session-id "s1"
                                     :render-at now})]
    (is (= result (prompt-line/last-render "claude-17" "s1")))
    (is (nil? (prompt-line/last-render "claude-17" "s2")))))
