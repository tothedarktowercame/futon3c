(ns futon3c.evidence.harness-test
  (:require [clojure.edn :as edn]
            [cheshire.core :as json]
            [futon3c.evidence.backend :as backend]
            [futon3c.evidence.http-backend :as legacy]
            [clojure.test :refer [deftest is]]
            [futon3c.evidence.boundary :as boundary]
            [futon3c.evidence.store :as store]
            [futon3c.evidence.futon1b-backend :as f1b]
            [futon3c.transport.http :as ingress]
            [org.httpkit.client :as http]))

(def origin {:kind :agent :actor "codex-5" :writer "harness-test"
             :attributed-author "codex-5" :authorization :unknown
             :recorded-at "2026-09-27T00:00:00Z" :basis :write-time})
(def base {:subject {:ref/type :agent :ref/id "codex-5"}
           :type :coordination :claim-type :step :author "codex-5"
           :body {:event "harness-pass-through"} :origin origin})
(def harness {:kind :none :basis :producer-context :source-ref "session:test"})

(deftest args-and-complete-entries-reach-real-serializer
  ;; Only the HTTP transport is substituted: boundary, store, schema, backend
  ;; and EDN serialization all run, including the old field-dropping call shape.
  (let [seen (atom nil) db (f1b/make-futon1b-backend "http://harness.test")]
    (with-redefs [http/post (fn [_ opts]
                             (let [row (edn/read-string (:body opts))]
                               (reset! seen row)
                               (delay {:status 201 :body (pr-str {:entry row})})))
                  http/get (fn [_ _] (delay {:status 200 :body (pr-str @seen)}))]
      (doseq [append [boundary/append! store/append*]
              input [base (assoc base :harness harness)
                     (assoc base :evidence/harness harness)
                     (assoc base :harness {"kind" "none" "basis" "producer-context"})
                     (assoc base :harness nil)]]
        (let [present? (or (contains? input :harness) (contains? input :evidence/harness))
              expected (if (contains? input :evidence/harness) (:evidence/harness input) (:harness input))
              r (append db input)]
          (is (:ok r))
          (is (= present? (contains? @seen :evidence/harness)))
          (is (= expected (:evidence/harness @seen)))
          (is (= origin (:evidence/origin @seen)))))
      (let [entry (assoc @seen :evidence/id "complete-harness" :evidence/harness harness)]
        (is (:ok (boundary/append! db entry)))
        (is (= entry @seen))))))

(def wire-base (json/parse-string (json/generate-string base) true))

(deftest ingress-and-atom-preservation
  (doseq [key [:harness "harness" :evidence/harness "evidence/harness"]
          value [harness nil {"kind" "none" "basis" "producer-context"}]]
    (let [db (atom {:entries {} :order []})
          input (#'ingress/normalize-evidence-payload (assoc wire-base key value))
          receipt (boundary/append! db input)
          row (store/get-entry* db (get-in receipt [:entry :evidence/id]))]
      (is (:ok receipt))
      (is (contains? row :evidence/harness))
      (is (= value (:evidence/harness row)))
      (is (= origin (:evidence/origin row)))))
  (let [db (atom {:entries {} :order []})
        r (boundary/append! db (#'ingress/normalize-evidence-payload wire-base))]
    (is (:ok r))
    (is (not (contains? (:entry r) :evidence/harness))))
  (is (nil? (:harness (#'ingress/normalize-evidence-payload
                       (assoc wire-base :harness harness :evidence/harness nil))))))

(deftest legacy-http-payload-preserves-context
  (let [db (legacy/make-http-backend "http://legacy.test")
        seen (atom nil)]
    (with-redefs [http/post (fn [_ opts]
                             (reset! seen (json/parse-string (:body opts) true))
                             (delay {:status 201 :body "{\"ok\":true}"}))]
      (doseq [entry [{:evidence/id "absent" :evidence/origin origin}
                     {:evidence/id "present" :evidence/origin origin :evidence/harness harness}
                     {:evidence/id "null" :evidence/origin origin :evidence/harness nil}]]
        (backend/-append db entry)
        (is (= (contains? entry :evidence/harness) (contains? @seen :harness)))
        (is (= (json/parse-string (json/generate-string (:evidence/harness entry)) true)
               (:harness @seen)))
        (is (= (:origin wire-base) (:origin @seen)))))))
