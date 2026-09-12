(ns futon3c.wm.run4-codex-fold-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon2.aif.c-fold-config :as digest]
            [futon2.aif.full-loop-runner :as runner]
            [futon3c.wm.run4-codex-fold :as sut]))

(def enriched
  {:wiring {:boxes []}
   :coverage-score-delta -1
   :policy-holes []})

(deftest authenticated-codex-job-is-the-only-output-authority
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                      "codex-fold" (make-array java.nio.file.attribute.FileAttribute 0)))
        plan "{:schema :wm/codex-fold-plan-v1 :prompt-prefix \"Fold this construction.\"}\n"
        _ (spit (java.io.File. dir "plan.edn") plan)
        authority {:schema :wm/codex-fold-authority-v1 :root (.getPath dir)
                   :plan-ref "plan.edn" :plan-sha256 (digest/sha256 plan)
                   :seat "codex-12" :agency-base "http://agency" :caller "run4"}
        seen (atom nil)
        port (sut/make-port
              authority
              {:announce-fn (fn [_ request]
                              {:ok true :job-id (:job-id request)})
               :activate-fn (fn [_ request]
                              (reset! seen request)
                              {:ok true :accepted? true :job-id (:job-id request)})
               :await-fn (fn [_ dispatch]
                           {:ok true :dispatch-observation
                            {:terminal {:job-id (:job-id dispatch) :agent-id "codex-12"
                                        :state :done :report enriched}}})})
        result (port {:shown []})]
    (is (= "codex-12" (:agent-id @seen)))
    (is (= enriched (dissoc result :fold/execution)))
    (is (= :wired (:status (runner/construction-wiring-result
                            {:shown []} port))))
    (testing "wrong actor and typed refusal never become silent nil"
      (let [bad (sut/make-port
                 authority
                 {:announce-fn (fn [_ r] {:ok true :job-id (:job-id r)})
                  :activate-fn (fn [_ r] {:ok true :accepted? true :job-id (:job-id r)})
                  :await-fn (fn [_ d]
                              {:ok true :dispatch-observation
                               {:terminal {:job-id (:job-id d) :agent-id "zai-5"
                                           :state :done :report enriched}}})})]
        (is (= :refused (:status (runner/construction-wiring-result
                                  {:shown []} bad))))))))
