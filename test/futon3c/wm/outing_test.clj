(ns futon3c.wm.outing-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.wm.outing :as outing]))

(defn- step
  ([] (step {}))
  ([overrides]
   (merge {:step 0
           :p "pre-tick"
           :dT-snapshot [{:action {:type :address-sorry :target "sorry/foo"}
                          :g-total -1.0
                          :rank 1}]
           :v {:type :address-sorry :target "sorry/foo"}
           :v-attribution :pilot-autonomous
           :predicted-discharge -1.0
           :cg-id "cg-test"
           :artefact {:kind :test}
           :delta-∇? false
           :p' "post-tick"
           :realised-discharge -1.0
           :prediction-error 0.0}
          overrides)))

(defn- frame
  ([] (frame [(step)]))
  ([trace] {:run-id "run-test" :mode :supervised-proposal :trace trace}))

(defn- ok-sorry-check []
  {:outcome :ok :detail {:checked-sorries 3}})

(defn- regression-sorry-check []
  {:outcome :violation
   :detail {:count 1
            :regressions [{:id :sorry/foo
                           :resolved-at "2026-06-01T00:00:00Z"}]}})

(defn- inactive-sorry-check []
  {:outcome :inactive :detail {:reason "no sorrys.edn found"}})

(defn- run
  ([cycle-result] (run cycle-result {}))
  ([cycle-result opts]
   (outing/run-cycle-gates!
    (merge {:frame (frame)
            :top-shift? true
            :claimed-discharge? false
            :run-id "run-test"}
           cycle-result)
    (merge {:sorry-check-fn ok-sorry-check} opts))))

(deftest pass-verdict-when-all-gates-pass-test
  (let [result (run {})]
    (is (= :pass (:verdict result)))
    (is (= [] (:reasons result)))
    (is (true? (get-in result [:gates :g1 :pass?])))
    (is (true? (get-in result [:gates :g2 :pass?])))
    (is (true? (get-in result [:gates :g3 :pass?])))))

(deftest g1-nonconformance-quarantines-test
  (let [bad-frame (frame [(step {:dT-snapshot []})])
        result (run {:frame bad-frame})]
    (is (= :quarantine (:verdict result)))
    (is (= [:g1-non-conformant] (:reasons result)))
    (is (false? (get-in result [:gates :g1 :pass?])))))

(deftest g2-regression-quarantines-test
  (let [result (run {} {:sorry-check-fn regression-sorry-check})]
    (is (= :quarantine (:verdict result)))
    (is (= [:g2-regression] (:reasons result)))
    (is (= :violation (get-in result [:gates :g2 :outcome])))))

(deftest g2-inactive-registry-does-not-fail-test
  ;; :inactive = no registry found = absence-of-data, not a regression; G2 passes.
  (let [result (run {} {:sorry-check-fn inactive-sorry-check})]
    (is (= :pass (:verdict result)))
    (is (true? (get-in result [:gates :g2 :pass?])))
    (is (= :inactive (get-in result [:gates :g2 :outcome])))))

(deftest gate-error-quarantines-test
  (let [result (run {} {:verify-fn (fn [& _] (throw (ex-info "boom" {})))})]
    (is (= :quarantine (:verdict result)))
    (is (= [:g1-error] (:reasons result)))
    (is (true? (get-in result [:gates :g1 :error?])))))

(deftest fake-finish-hard-halts-test
  (let [result (run {:claimed-discharge? true
                     :top-shift? false})]
    (is (= :hard-halt (:verdict result)))
    (is (= [:g3-fake-finish] (:reasons result)))
    (is (true? (get-in result [:gates :g3 :fake-finish?])))))
