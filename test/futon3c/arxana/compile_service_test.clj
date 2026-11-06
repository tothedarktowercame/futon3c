(ns futon3c.arxana.compile-service-test
  "Public-envelope acceptance for increment 7 Compile."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.arxana.artifact :as artifact]
            [futon3c.arxana.compile.intent :as intent]
            [futon3c.arxana.envelope :as env]
            [futon3c.arxana.service :as service]
            [futon3c.arxana.store :as store]))

(defn- request [body message-id]
  {:protocol/version env/protocol-version
   :message/id message-id
   :operation :action/compile
   :principal/id "compile-service-test"
   :sent-at "2026-11-07T00:00:00Z"
   :body body})

(defn- compile-body []
  (let [interpreter {:interpreter/id "interp/test-human-1"
                     :interpreter/type :human
                     :interpreter/method "manual-reading"}]
    {:source-act/id "evidence/act-1"
     :intent (intent/make-intent
              {:intent/type :edit/replace
               :intent/source-act-id "evidence/act-1"
               :intent/target-ref {:manifestation/id "mft/tex-section-1"}
               :intent/proposed-change {:replacement "Objects and Rules"}
               :interpreter interpreter
               :interpreted-at "2026-11-07T00:00:00Z"})
     :trust-profile
     (intent/make-trust-profile
      {:trust-profile/id "trust/test" :trust-profile/version "v1"
       :trusted-interpreters {"interp/test-human-1" true}})
     :grounding
     {:artifact/revision-id "rev-tex-1"
      :resolution-assertion {:resolution-assertion/id "res-assert-1"
                             :evidence/outcome :unique
                             :artifact/revision-id "rev-tex-1"}
      :correspondence-assertions [{:correspondence-assertion/id "corr-assert-1"
                                   :verification :verified}]
      :grounding/predicted-effects [{:type :revision/creates
                                     :artifact/id "art-1"}]
      :grounding/risks []}}))

(deftest compile-is-public-deterministic-and-non-executable
  (let [cfg {:graph-store (store/mock-store)
             :artifact-provider (artifact/in-memory-provider)}
        r1 (service/dispatch cfg (request (compile-body) "compile-1"))
        r2 (service/dispatch cfg (request (compile-body) "compile-2"))
        c1 (get-in r1 [:ok :candidate-action])]
    (is (= :compiled (get-in r1 [:ok :status])))
    (is (some? (get-in r1 [:ok :semantic/digest])))
    (is (= (get-in r1 [:ok :semantic/digest])
           (get-in r2 [:ok :semantic/digest]))
        "transport identity does not alter semantic equality")
    (is (= (:candidate-action/id c1)
           (get-in r2 [:ok :candidate-action :candidate-action/id])))
    (is (every? #(not (contains? c1 %))
                [:credential :admission/token :consent :executor]))))

(deftest compile-public-schema-refuses-unknown-fields
  (let [cfg {:graph-store (store/mock-store)
             :artifact-provider (artifact/in-memory-provider)}
        response (service/dispatch cfg
                                   (request (assoc (compile-body) :execute? true)
                                            "compile-invalid"))]
    (is (= :invalid (get-in response [:error :error/category])))
    (is (= :arxana.schema/unknown-body-field
           (get-in response [:error :error/code])))))
