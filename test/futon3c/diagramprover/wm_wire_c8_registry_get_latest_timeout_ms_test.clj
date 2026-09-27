(ns futon3c.diagramprover.wm-wire-c8-registry-get-latest-timeout-ms-test
  "The real registry-get catches an HTTP transport timeout and the real
  reader records timeout-ms in refusal data. Transport is replaced; no
  sockets or live registry. The first-layer classification is unchanged."
  (:require [futon3c.diagramprover.wm-wire-c8-products-support :as products]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-c8-registry-get-entry-message-test :as msg-wire]))

(defn observe
  "Real registry-get catches a timeout from the replaced HTTP transport.
   First-layer controls retain their historical post-reader intervention."
  ([] (observe identity))
  ([tamper]
   (let [o (products/observe :latest identity)]
     {:writer (get-in o [:written :timeout-ms])
      :refusal (:result o)
      :reader (get-in (tamper (:result o)) [:data :timeout-ms] {:absent :field-not-carried})})))

(defn check [] (observe))

(def live-records-read (:live-records-read msg-wire/wire))

(def wire
  {:second-layer {:test `diagnostic-is-recorded-without-changing-refusal
                  :kind :record :product [:data :timeout-ms] :intervention :before-reader}
   :wire [:c8-registry-get :c8-latest :timeout-ms]
   :kind :witnessed-hermetically
   :test `the-timeout-reaches-the-latest-refusal
   :check check
   :live-records-read live-records-read})

(deftest the-timeout-reaches-the-latest-refusal
  (let [o (check)]
    (is (= :registry-unreadable (get-in o [:refusal :kind])))
    (is (= :unreachable (get-in o [:refusal :data :status])))
    (is (= 50 (:writer o)) "the writer recorded the bound timeout")
    (is (w/received? o))))

(deftest a-typed-absence-at-the-reader-fails-the-wire
  (let [o (observe #(update % :data dissoc :timeout-ms))]
    (is (= {:absent :field-not-carried} (:reader o)))
    (is (not (w/received? o)))))

(deftest a-different-timeout-at-the-reader-fails-the-wire
  (let [o (observe #(assoc-in % [:data :timeout-ms] 5000))]
    (is (some? (:reader o)))
    (is (not= (:writer o) (:reader o)))
    (is (not (w/received? o)))))

(deftest diagnostic-is-recorded-without-changing-refusal
  ;; observation_checks.clj:233-236,262-269 select and merge diagnostics;
  ;; status determines the refusal, not the message or timeout value.
  (products/assert-record :latest :timeout-ms))
