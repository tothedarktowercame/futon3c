(ns futon3c.wm.medic-authority-provider-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is use-fixtures]]
            [futon2.aif.meta-injury-ledger :as ledger]
            [futon2.aif.meta-medic-authority :as authority]
            [futon2.aif.meta-medic-authority-test :as authority-fixture]
            [futon2.aif.meta-self-heal-production :as self-heal]
            [futon3c.wm.machinery-execution-cohort :as machinery-cohort]
            [futon3c.wm.runner-service :as service]))

(def pin {:path "synthetic.edn" :sha256 (apply str (repeat 64 "a"))})
(def context
  {:intent {:schema :wm/meta-medic-intent-v1}
   :artifacts {"synthetic.edn" "synthetic"}
   :evaluation-pin pin :review-pin pin :authorization-pin pin
   :trust (constantly true)})

(use-fixtures :each
  (fn [test-fn]
    (service/install-medic-authority-provider! nil)
    (try (test-fn)
         (finally (service/install-medic-authority-provider! nil)))))

(deftest provider-resolution-precedes-production-readiness
  (let [seen (atom nil)]
    (service/install-medic-authority-provider! (constantly context))
    (let [resolved (#'service/acquire-medic-authority
                    {:meta-medic-authority-context {:trust (constantly false)}
                     :meta-medic-authority-verifier (constantly true)})]
      (with-redefs [self-heal/production-readiness
                    (fn [opts]
                      (reset! seen opts)
                      {:status :ready})]
        (is (some? (:meta-medic-authority-resolution
                    (service/meta-self-heal-preflight! resolved))))
        (is (ifn? (:meta-medic-authority-verifier @seen)))
        (is (some? (:meta-medic-authority-resolution @seen)))
        (is (nil? (:meta-medic-authority-context @seen)))))))

(deftest actual-preflight-uses-provider-for-medic-bearing-ledger
  (let [bundle (authority-fixture/fixture)
        context (select-keys bundle [:intent :artifacts :evaluation-pin
                                     :review-pin :authorization-pin :trust])
        verifier (authority/authority-verifier context)
        medic (ledger/append-medic-repaired
               (:ledger bundle)
               {:path "synthetic/medic.edn" :sha256 (apply str (repeat 64 "8"))}
               (:operation bundle) verifier)
        unrelated (ledger/append-transition
                   (:ledger medic)
                   {:injury-id "injury/unrelated" :state :open :attempt 1
                    :cause-record-pin {:path "synthetic/cause.edn"
                                       :sha256 (apply str (repeat 64 "9"))}
                    :failure-stage :construction :failure-kind :synthetic
                    :detected-click "click/synthetic"}
                   verifier)
        dir (io/file "target/medic-authority-provider")
        _ (.mkdirs dir)
        ledger-path (str (io/file dir "ledger.edn"))
        tx-path (str (io/file dir "transaction.edn"))
        _ (spit ledger-path (str (pr-str (:ledger unrelated)) "\n"))]
    (service/install-medic-authority-provider! (constantly context))
    (let [resolved (#'service/acquire-medic-authority
                    {:meta-injury-ledger-path ledger-path
                     :meta-repair-transaction-path tx-path})
          preflight (service/meta-self-heal-preflight! resolved)
          diagnosis (self-heal/diagnosis-context resolved)]
      (is (= :appended (:status medic)))
      (is (= :appended (:status unrelated)))
      (is (some? (:meta-medic-authority-resolution preflight)))
      (is (= :absent (get-in preflight [:meta-self-heal-readiness :status])))
      (is (= #{"injury/unrelated"}
             (set (keys (get-in diagnosis [:active-injuries :injuries]))))))))

(deftest serialization-retains-only-stable-authority-identity
  (service/install-medic-authority-provider! (constantly context))
  (let [resolved (#'service/acquire-medic-authority {})
        payload (#'service/serializable-click-opts resolved)
        roundtrip (read-string (pr-str payload))]
    (is (= (:meta-medic-authority-identity resolved)
           (:meta-medic-authority-identity roundtrip)))
    (is (= (:meta-medic-authority-identity resolved)
           (:meta-medic-authority-identity
            (#'service/acceptance-payload
             (assoc resolved :acceptance-payload {:custom true})))))
    (is (nil? (:meta-medic-authority-resolution payload)))
    (is (nil? (:meta-medic-authority-verifier payload)))
    (is (not-any? fn? (tree-seq coll? seq payload)))
    (is (= :wm-click-authority-not-serializable
           (:error (try
                     (#'service/serializable-click-opts
                      {:nested {:trust (constantly true)}})
                     nil
                     (catch clojure.lang.ExceptionInfo e (ex-data e))))))))

(deftest worker-reacquires-same-scope-and-refuses-drift
  (service/install-medic-authority-provider! (constantly context))
  (let [admitted (#'service/acquire-medic-authority {})
        identity (:meta-medic-authority-identity admitted)]
    (is (= identity (:meta-medic-authority-identity
                     (#'service/acquire-medic-authority admitted identity))))
    (service/install-medic-authority-provider! nil)
    (let [failure (try
                    (#'service/acquire-medic-authority admitted identity)
                    nil
                    (catch clojure.lang.ExceptionInfo e (ex-data e)))]
      (is (= :meta-medic-authority-context-drift (:error failure))))))

(deftest absent-refused-and-provider-transitions-are-explicit
  (let [absent (#'service/acquire-medic-authority {})
        absent-id (:meta-medic-authority-identity absent)]
    (is (= {:status :absent} (get-in absent-id [:authority])))
    (is (= absent-id (:meta-medic-authority-identity
                      (#'service/acquire-medic-authority absent absent-id))))
    (service/install-medic-authority-provider! (constantly context))
    (is (= :meta-medic-authority-context-drift
           (:error (try
                     (#'service/acquire-medic-authority absent absent-id)
                     nil
                     (catch clojure.lang.ExceptionInfo e (ex-data e)))))))
  (service/install-medic-authority-provider! (constantly nil))
  (is (= :meta-self-heal/medic-authority-unavailable
         (:error (try
                   (#'service/acquire-medic-authority {})
                   nil
                   (catch clojure.lang.ExceptionInfo e (ex-data e)))))))

(deftest provider-change-before-admission-lock-spends-no-ration
  (let [spent (atom 0)]
    (service/install-medic-authority-provider! nil)
    (with-redefs [machinery-cohort/apply-binding identity]
      (binding [service/*resolve-var* (constantly nil)
                service/*before-medic-admission-lock-hook*
                #(swap! (deref (ns-resolve 'futon3c.wm.runner-service
                                           '!medic-authority-provider))
                         update :generation inc)]
        (let [failure (try
                        (service/click! {:ordinary-click/issue! #(swap! spent inc)
                                         :acceptance-payload {:race :synthetic}})
                        nil
                        (catch clojure.lang.ExceptionInfo e (ex-data e)))]
          (is (= :meta-medic-authority-context-drift (:error failure)))
          (is (true? (:before-ration-spend? failure)))
          (is (zero? @spent)))))))

(deftest default-provider-denies-and-request-cannot-install-trust
  ;; Contract after the preflight-standing-verifier fix: with no provider,
  ;; admission installs NO verifier at all (nil) — the standing resolver is
  ;; composed downstream by futon2's medic-authority-verifier (P1) and is
  ;; itself fail-closed. A deny-all placeholder here used to poison the
  ;; pre-ration preflight ledger read (run 2026-10-31-bc036e43).
  (let [resolved (#'service/acquire-medic-authority
                  {:meta-medic-authority-context context
                   :meta-medic-authority-verifier (constantly true)})]
    (is (= {:status :absent}
           (get-in resolved [:meta-medic-authority-identity :authority])))
    (is (nil? (:meta-medic-authority-resolution resolved)))
    (is (nil? (:meta-medic-authority-verifier resolved))
        "no-provider admission must not install any verifier — deny-all or otherwise")
    (is (nil? (:meta-medic-authority-context resolved)))))
