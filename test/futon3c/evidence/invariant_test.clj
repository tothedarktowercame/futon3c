(ns futon3c.evidence.invariant-test
  "I-evidence-per-turn tests against the authoritative Futon1b HTTP backend."
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.evidence.backend :as backend]
            [futon3c.evidence.futon1b-backend :as f1b]
            [futon3c.evidence.invariant :as inv]
            [futon3c.evidence.store :as store]
            [futon3c.social.test-fixtures :as fix]
            [org.httpkit.client :as http]))

(defn- remote-store [] (f1b/make-futon1b-backend "http://store.test"))

(deftest check-store-backing-passes-for-live-futon1b
  (testing "Futon1bBackend satisfies I-evidence-per-turn when its cheap health route responds"
    (with-redefs [http/get (fn [_ _]
                             (delay {:status 200 :body "{:ok true :deep false}"}))]
      (let [r (inv/check-store-backing (remote-store))]
        (is (true? (:ok r)))
        (is (= :futon1b (:kind r)))
        (is (string? (:invariant r)))))))

(deftest check-store-backing-fails-for-unreachable-futon1b
  (with-redefs [http/get (fn [_ _]
                           (delay {:error (ex-info "down" {})}))]
    (let [r (inv/check-store-backing (remote-store))]
      (is (false? (:ok r)))
      (is (= :futon1b (:kind r)))
      (is (re-find #"unreachable" (:reason r))))))

(deftest check-store-backing-fails-for-atom-and-unknown
  (let [ab (backend/->AtomBackend (atom {:entries {} :order []}))]
    (is (= :atom-backend (:kind (inv/check-store-backing ab))))
    (is (= :raw-atom (:kind (inv/check-store-backing (atom {})))))
    (is (= :unknown (:kind (inv/check-store-backing "not-a-store"))))))

(deftest verify-persisted-reads-back-through-futon1b
  (let [entry (fix/make-evidence-entry {:evidence/id "e-inv-live"})]
    (with-redefs [http/get (fn [_ _]
                             (delay {:status 200 :body (pr-str entry)}))]
      (let [r (inv/verify-persisted (remote-store) "e-inv-live")]
        (is (:ok r))
        (is (= :futon1b (:kind r)))
        (is (= "e-inv-live" (:evidence/id r)))))))

(deftest verify-persisted-detects-missing-entry
  (with-redefs [http/get (fn [_ _] (delay {:status 404 :body "{:error :missing}"}))]
    (let [r (inv/verify-persisted (remote-store) "missing")]
      (is (false? (:ok r)))
      (is (= :futon1b (:kind r)))
      (is (re-find #"not readable" (:reason r))))))

(deftest invariant-holds-end-to-end-on-futon1b
  (let [entry (fix/make-evidence-entry {:evidence/id "e-inv-e2e"})]
    (with-redefs [http/post (fn [_ _]
                              (delay {:status 201
                                      :body (pr-str {:ok true :entry entry})}))
                  http/get (fn [_ _]
                             (delay {:status 200 :body (pr-str entry)}))]
      (let [remote (remote-store)
            appended (store/append* remote entry)
            verified (inv/verify-persisted remote "e-inv-e2e")]
        (is (:ok appended))
        (is (:ok verified))
        (is (= :futon1b (:kind verified)))))))
