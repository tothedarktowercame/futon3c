(ns futon3c.wm.retry-identity-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.wm.runner-service :as service]
            [futon3c.wm.runner-service-test :as fixture]
            [futon3c.wm.meta-self-heal-adapters :as adapter]
            [futon3c.wm.machinery-execution-cohort :as cohort]))

(use-fixtures :each #'fixture/reset-service!
  (fn [f] (with-redefs [cohort/apply-binding identity] (f))))

(def repair-id "repair/d9ca0b41fe22ddc094e3b30816a1e085b6e16481")
(def patches ["dafa05dbd9e22f305a4b8e8ddf72cea3678eee50"
              "7ca70e10ea413d94303f4592755fb04433c31021"])

(defn retry-request [ordinal patch]
  {:repair-id repair-id :attempt 1 :patch-sha patch
   :retry-click (adapter/retry-click-id repair-id 1 ordinal patch)})

(deftest different-patches-admit-distinct-children-and-replays-do-not-run
  (doseq [ordinal [0 1]]
    (let [runs (atom [])
          run! (fn [opts] (swap! runs conj (:meta-repair-retry opts))
                 {:outcome :grounded-change})
          child (fn [patch] (#'service/retry-child-opts
                             {:wm-agent-id fixture/scratch-agent-id}
                             (retry-request ordinal patch)))]
      (binding [service/*resolve-var* (#'fixture/resolver run! (constantly {}))]
        (let [first-opts (child (first patches))
              first-result (service/click! first-opts)
              _ (service/await-click! (:click-id first-result))
              second-result (try (service/click! (child (second patches)))
                                 (catch clojure.lang.ExceptionInfo e (ex-data e)))]
          (is (= :queued (:status second-result)))
          (is (string? (:click-id second-result)))
          (is (not= (:click-id first-result) (:click-id second-result)))
          (when (= :queued (:status second-result))
            (service/await-click! (:click-id second-result)))
          (is (= (:click-id first-result) (:click-id (service/click! first-opts))))
          (is (= 2 (count @runs))))))))

(deftest malformed-patch-is-refused-before-minting
  (doseq [patch [nil "" "abc" (apply str (repeat 40 "z"))]]
    (is (thrown? AssertionError (adapter/retry-click-id repair-id 1 0 patch)))))

(deftest child-does-not-inherit-parent-run-id
  ;; run001 click 3: the requeue child ran under the run id wm_click.sh had
  ;; reserved for the admitted parent, so (run, parent-click) never existed.
  (let [parent-run "2026-10-27-c9d132d8-a2be-42b3-8369-fd23e9b9924f"
        child (#'service/retry-child-opts
               {:wm-agent-id fixture/scratch-agent-id :run-id parent-run}
               (retry-request 1 (first patches))
               "wm-click-5b04d4f2-0b15-4a71-97dc-6d00eed8587b")]
    (is (not (contains? child :run-id)))
    (is (= "wm-click-5b04d4f2-0b15-4a71-97dc-6d00eed8587b" (:parent-click-id child)))))
