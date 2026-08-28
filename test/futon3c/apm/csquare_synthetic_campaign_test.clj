(ns futon3c.apm.csquare-synthetic-campaign-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.csquare-synthetic-campaign :as sut]))

(deftest campaign-shape-is-ten-sequential-real-gated-frames
  (is (= 10 sut/problem-count))
  (is (= [:solve :verify :close] sut/phases))
  (let [events (#'sut/append-frame-events
                (:events (#'sut/initial-campaign)) "c01"
                {:trace/combined {"schemaVersion" 1}
                 :trace/digest "not-a-real-receipt"})]
    ;; The production model, not C□, rejects an unbound closure receipt.
    (is (= :frame-close-combined-trace-required
           (:error/code
            (machine/projection events))))))

(deftest adapter-reports-only-real-postconditions
  (with-redefs [sut/process-one! (constantly {:ok true :status :phase-advanced})]
    (is (= :queue-tick-complete
           (:status ((:reconcile-fn (sut/adapter-constructor {})) nil nil)))))
  (with-redefs [sut/process-one! (constantly {:ok true :status :batch-complete})]
    (is (= :frame-complete
           (:status ((:reconcile-fn (sut/adapter-constructor {})) nil nil))))))

(deftest absent-campaign-state-starts-at-frame-one
  (with-redefs [clojure.core/slurp
                (fn [path & _]
                  (throw (AssertionError.
                          (str "absent state must not be read: " path))))
                io/file
                (fn [& _]
                  (proxy [java.io.File] ["/definitely/absent"]
                    (exists [] false)))]
    (is (= 1 (:next-frame (#'sut/read-campaign-state))))))
