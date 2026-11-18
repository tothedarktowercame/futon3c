(ns futon3c.arxana.demo-b-tracker-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.arxana.demo-b-tracker :as tracker]
            [futon3c.arxana.catalogue-view-test :as fixtures]
            [futon3c.arxana.service :as service]))
(deftest planned-undocumented-and-inconclusive
  (let [manifest {:reads [{:read :future :feature :missing :use :planned}]}
        v (tracker/tracker-view manifest {:status :ok :choices []})]
    (is (= :planned (get-in v [:reads 0 :use])))
    (is (= {:status :unknown :reason :undocumented} (get-in v [:reads 0 :enforcement-status])))
    (is (= false (get-in v [:provenance :manifest :record?]))))
  (is (= {:status :inconclusive :reason :truncated}
         (tracker/tracker-view {} {:status :inconclusive :reason :truncated}))))
(deftest store-join-and-closed-dispatch
  (let [calls (atom []) s (fixtures/fake-store (fixtures/fixture-records) calls (constantly nil))
        envelope {:protocol/version "arxana.v1" :message/id "invented" :principal/id "reader"
                  :operation :demo-b/tracker :body {:valid-time fixtures/at}}
        v (:ok (service/dispatch {:graph-store s} envelope))]
    (is (= :ok (:status v)))
    (is (= 9 (count (:reads v))))
    (is (= :no-run (first (get-in v [:coverage 0 :enforcement-status :reasons]))))
    (is (= :not-built (get-in v [:mojira :status])))
    (is (:error (service/dispatch {:graph-store s} (assoc-in envelope [:body :unknown] true))))
    (is (not-any? #(= :role-reverse (:query/type %)) @calls))))
