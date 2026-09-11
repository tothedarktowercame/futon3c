(ns futon3c.wm.run4-boot-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.wm.run4-boot :as sut]))

(def token (apply str (repeat 64 "a")))

(deftest disabled-is-an-exact-no-op
  (let [called (atom [])]
    (is (= {} (sut/materialize false
                               {:read-template #(swap! called conj :template)
                                :read-secret #(swap! called conj :secret)
                                :resolve-mission #(swap! called conj :mission)
                                :admissible? #(swap! called conj :admissible)
                                :attest! #(swap! called conj :attest)})))
    (is (empty? @called))))

(deftest enabled-uses-only-server-owned-ports
  (let [template (slurp sut/template-path)
        mission {:id "M-u88-contextual-preferences" :status-class :draft
                 :open-hole-count 0}
        result (sut/materialize
                true
                {:read-template (constantly template)
                 :read-secret (constantly token)
                 :resolve-mission #(when (= (:id mission) %) mission)
                 :admissible? (fn [_ _] false)
                 :attest! (constantly true)})]
    (is (true? (get-in result [:run4 :enabled?])))
    (is (= "Joe" (get-in result [:run4 :operator])))
    (is (= token (get-in result [:run4 :bearer-token])))
    (is (false? ((get-in result [:run4 :action-admissible?])
                 mission {:type :advance-mission :target (:id mission)})))))

(deftest malformed-activation-and-secret-refuse
  (is (= :activation-not-boolean
         (try (sut/materialize :yes) nil
              (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))
  (is (= :credential-unprovisioned
         (try (sut/materialize true
                               {:read-template #(slurp sut/template-path)
                                :read-secret (constantly "not-a-secret")
                                :resolve-mission (constantly nil)
                                :admissible? (constantly false)
                                :attest! (constantly true)})
              nil
              (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))

(deftest production-mission-and-guardrail-ports-accept-only-exact-activated-action
  ;; The only substituted boot dependencies are the secret read and current
  ;; environment attestation. Mission parsing and admissibility are production.
  (let [cfg (sut/materialize true {:read-secret (constantly token)
                                   :attest! (constantly true)})
        resolve-mission (get-in cfg [:run4 :resolve-mission])
        admissible? (get-in cfg [:run4 :action-admissible?])
        mission (resolve-mission "M-u88-contextual-preferences")]
    (is (= :open (:status-class mission)))
    (is (true? (admissible? mission
                            {:type :advance-mission
                             :target "M-u88-contextual-preferences"})))
    (is (false? (admissible? mission
                             {:type :advance-mission
                              :target "futon2-d/mission/u88-contextual-preferences"})))))
