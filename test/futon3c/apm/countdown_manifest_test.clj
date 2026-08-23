(ns futon3c.apm.countdown-manifest-test
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.countdown-manifest :as sut]))

(def manifest-path
  "holes/labs/M-apm-demonstration/countdown-10-manifest-v1.edn")

(defn load-manifest [] (edn/read-string (slurp manifest-path)))

(defn load-manifest-v2 []
  (edn/read-string
   (slurp "holes/labs/M-apm-demonstration/countdown-10-manifest-v2.edn")))

(deftest qualification-checkout-is-revision-addressed-beside-repository
  (is (= "/home/joe/code/apm-frames/qualification/rev-123"
         (sut/qualification-checkout-path "/home/joe/code/apm-lean"
                                          "rev-123"))))

(deftest committed-countdown-manifest-resolves-from-immutable-git-objects
  (let [result (sut/validate (load-manifest))]
    (is (:valid? result) (pr-str (:findings result)))
    (is (= 10 (count (:problem-observations result))))
    (is (every? :valid? (:problem-observations result)))
    (is (every? :valid? (:apparatus-observations result)))
    (is (false? (:worktree-head-consulted? result)))))

(deftest manifest-refuses-identity-and-classification-drift
  (let [manifest (load-manifest)
        readdress #(assoc % :manifest/id
                          (machine/ledger-digest [(dissoc % :manifest/id)]))]
    (testing "duplicate problem"
      (let [changed (readdress
                     (assoc-in manifest [:units 1 :problem/id]
                               (get-in manifest [:units 0 :problem/id])))]
        (is (some #{:countdown-manifest-problem-duplicate}
                  (:findings (sut/validate changed))))))
    (testing "topology classification"
      (let [changed (readdress
                     (assoc-in manifest [:units 1 :classification/value] :topology))]
        (is (some #{:countdown-manifest-classification-invalid}
                  (:findings (sut/validate changed))))))
    (testing "mutable or wrong blob"
      (let [changed (readdress
                     (assoc-in manifest [:units 1 :problem :blob]
                               "0000000000000000000000000000000000000000"))]
        (is (some #{:countdown-manifest-problem-pin-invalid}
                  (:findings (sut/validate changed))))))
    (testing "unaddressed edit"
      (is (some #{:countdown-manifest-content-address-invalid}
                (:findings (sut/validate (assoc manifest :series :other))))))))

(deftest rebuilt-manifest-executes-every-pinned-eligibility-baseline
  (let [result (sut/validate (load-manifest-v2))]
    (is (:valid? result) (pr-str (:findings result)))
    (is (= 10 (count (:eligibility-observations result))))
    (is (every? :valid? (:eligibility-observations result)))))

(deftest eligibility-runs-in-revision-addressed-checkout
  (let [manifest (load-manifest-v2)
        result (sut/validate manifest)
        observations (:eligibility-observations result)]
    (is (:valid? result) (pr-str (:findings result)))
    (is (every? #(string? (:qualification-checkout %)) observations))
    (is (= (mapv #(get-in % [:problem :revision]) (:units manifest))
           (mapv :qualification-revision observations)))
    (is (every? #(str/includes? (:qualification-checkout %)
                                "/apm-frames/qualification/")
                observations))))

(deftest solved-problem-cannot-be-filed-as-an-eligible-unit
  (let [manifest (load-manifest-v2)
        solved (get-in (load-manifest) [:units 1])
        changed (-> manifest
                    (assoc-in [:units 1 :problem/id] (:problem/id solved))
                    (assoc-in [:units 1 :problem] (:problem solved))
                    (assoc :manifest/id "temporarily-unaddressed"))
        changed (assoc changed :manifest/id
                       (machine/ledger-digest [(dissoc changed :manifest/id)]))
        result (sut/validate changed)]
    (is (some #{:countdown-manifest-eligibility-observation-invalid}
              (:findings result)))
    (is (= 0 (get-in result [:eligibility-observations 1
                             :observation :sorry-warnings])))))

(deftest one-off-scope-requires-exactly-one-ordinal-one-unit
  (let [source (load-manifest)
        one-off (-> source
                    (assoc :manifest/scope :one-off
                           :campaign/id "apm-f20-one-off-v1"
                           :block/id "f20-one-off-v1"
                           :units [(assoc (nth (:units source) 2) :ordinal 1)])
                    (assoc :manifest/id "pending"))
        one-off (assoc one-off :manifest/id
                       (machine/ledger-digest [(dissoc one-off :manifest/id)]))]
    (is (:valid? (sut/validate one-off)))
    (let [invalid (-> one-off
                      (assoc-in [:units 0 :ordinal] 2)
                      (assoc :manifest/id "pending"))
          invalid (assoc invalid :manifest/id
                         (machine/ledger-digest [(dissoc invalid :manifest/id)]))]
      (is (some #{:countdown-manifest-frame-order-invalid}
                (:findings (sut/validate invalid)))))))

(deftest eligibility-baseline-is-semantic-not-style-warning-exact
  (let [baseline {:exit 0 :warnings 20 :sorry-warnings 2 :errors 0}
        observed {:exit 0 :warnings 38 :sorry-warnings 2 :errors 0
                  :blocking-warnings 0}]
    (is (sut/eligibility-observation-valid? baseline observed))
    (is (not (sut/eligibility-observation-valid?
              baseline (assoc observed :sorry-warnings 1))))
    (is (not (sut/eligibility-observation-valid?
              baseline (assoc observed :blocking-warnings 1))))))
