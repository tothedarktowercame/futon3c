(ns futon3c.aif.stack-generator-test
  "Tests for the live AIF+ stack projection — in particular the
   E-wm-live-recommendation surface (`:reading :next-move-live`).

   H3 (SPEC-flat-removal-and-cascade-decision, 2026-09-17): the snapshot's
   judgement carries a cascade decision built with the REAL
   futon2.aif.policy/select-action-cascades over receipted candidates (it
   passes futon2.aif.decision-gate/emit!), or a typed abstention. The flat
   :ranked-actions grain is gone and is not read."
  (:require [clojure.test :refer [deftest is testing]]
            [futon2.aif.decision-gate :as gate]
            [futon2.aif.policy :as policy]
            [futon3c.aif.stack-generator :as sg]))

(defn- cascade-action
  [cascade-id precedence]
  {:kind :cascade-candidate
   :cascade-id cascade-id
   :precedence (vec precedence)
   :construction-receipt {:cascade-id cascade-id :moves (count precedence)}
   :interpretation-receipts (mapv (fn [p] {:pattern p :admitted true})
                                  precedence)})

(defn- cascade-decision
  []
  (gate/emit!
   (policy/select-action-cascades
    [{:action (cascade-action :C1-test-first
                              [{:type :advance-mission :target "M-foo"}
                               {:type :address-sorry :target "sorry/x"}])
      :controller-score 11.434408130577516}
     {:action (cascade-action :C2-fix-first
                              [{:type :advance-mission :target "M-bar"}])
      :controller-score 10.9}
     {:action (cascade-action :C3-fix-only [{:type :no-op}])
      :controller-score 12.101074797244184}]
    {:beta 0.25})))

(def ^:private cascade-judgement
  {:mode :base-case
   :decision (cascade-decision)
   :priorities
   [{:type :missing-head :id "h1" :summary "no head h1"}
    {:type :channel-gap :id "g1" :summary "gap g1"}]})

(def ^:private abstain-judgement
  {:mode :base-case
   :decision {:status :abstained
              :refusals [{:target "M-foo" :kind :want-not-declared}
                         {:target "M-bar" :kind :beta-not-declared}]}})

(defn- snapshot
  [judgement]
  ;; keyword-keyed payload (the generator tolerates the JSON-stringified
  ;; cache form too; the candidate-map posterior keys cannot survive JSON
  ;; round-trips, so the keyword form is the faithful in-memory mirror)
  {:days 14
   :as-of (java.time.Instant/now)
   :body-bytes 1234
   :duration-ms 9
   :payload {:judgement judgement}})

(deftest derive-next-move-live-projects-the-cascade-decision
  (testing "target, enacted first step, posterior mass and β; alternatives from the posterior"
    (let [live (sg/derive-next-move-live (snapshot cascade-judgement))]
      (is (some? live))
      (is (= :recommendation-issued (:status live)))
      (is (= :judgement.decision (:source live)))
      (is (= :base-case (:mode live)))
      (is (contains? #{:advance-mission :no-op} (-> live :action :type)))
      (is (contains? #{:C1-test-first :C2-fix-first :C3-fix-only} (:target live)))
      (is (= {:value 0.25 :status :declared} (:beta live)))
      (is (pos? (:posterior-mass live)))
      (is (string? (:specifically live)))
      (is (false? (get-in live [:selection-boundary :recomputed?])))
      (is (= 300 (:scheduler-period-seconds live)))
      (is (false? (:stale? live)) "fresh snapshot is not stale")
      (testing "alternatives carry posterior masses, not flat G"
        (let [alts (:alternatives-considered live)]
          (is (seq alts))
          (doseq [[_ s] alts]
            (is (re-find #"\(p=0\.\d{3}\)" s)))))
      (testing "the tile's tied bucket holds only the enacted step's marginal"
        (is (= 1 (:tied-count live)))
        (is (pos? (get-in live [:tied-actions 0 :posterior-mass]))))
      (testing "priorities are carried through (top 5)"
        (is (= 2 (count (:priorities live))))))))

(deftest derive-next-move-live-renders-abstention-as-readiness
  (testing "a typed abstention is a readiness state with refusals grouped by kind"
    (let [live (sg/derive-next-move-live (snapshot abstain-judgement))]
      (is (some? live))
      (is (= :abstained-readiness (:status live)))
      (is (nil? (:action live)))
      (is (= {:want-not-declared 1 :beta-not-declared 1}
             (into {} (map (fn [[k v]] [k (count v)]))
                   (:refusals-by-kind live))))
      (is (= 0 (:tied-count live)))
      (is (false? (get-in live [:actuation :authorized?]))))))

(deftest derive-next-move-live-handles-nil
  (testing "Returns nil when there is no snapshot"
    (is (nil? (sg/derive-next-move-live nil)))))

(deftest derive-next-move-live-marks-stale
  (testing "An hours-old snapshot is marked stale (>2× period)"
    (let [old-as-of (.minusSeconds (java.time.Instant/now) (long 1200))
          live (sg/derive-next-move-live
                (assoc (snapshot cascade-judgement) :as-of old-as-of))]
      (is (true? (:stale? live))))))
