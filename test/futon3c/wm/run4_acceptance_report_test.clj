(ns futon3c.wm.run4-acceptance-report-test
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.c-fold-config :as digest]
            [futon3c.wm.run4-acceptance-report :as sut]
            [futon3c.wm.run4-terminal-evidence :as terminal]))

(def sha (apply str (repeat 64 "a")))
(def visibility {:schema "wm/run-visibility-v1" :run_id "RUN4-x"
                 :stage "complete" :result "passed"
                 :trials [{:trial_id ":t1" :stage "complete" :result "passed"}]})
(def control-text (pr-str {:edges [{:from :R20 :to :R12}]
                            :route-measured-drawn [] :decisions {}}))
(def bundle {:schema :wm/run4-terminal-evidence-bundle-v1
             :identity {:series-id "RUN4-x" :trial-id :t1
                        :casting {:author "zai-2" :reviewer "codex-17"}}
             :attempt-id "attempt-1"
             :projection-digest sha :run-record-digest sha
             :terminal-projection
             {:run/id "run-1" :attempt/id "internal-1"
              :run4/task-pin {:mission-id "M-run4"}
              :outcome :grounded-change :checkpoints {}
              :failure {:kind nil :stage nil} :evidence {}}
             :classification {:task-result :succeeded :infrastructure :safe
                              :evidence-id sha}
             :run-record {:run/id "run-1"
                          :route [{:fromNode "R20" :toNode "R12" :via "scan"}]}})

(defn- report [input]
  (with-redefs [terminal/read-terminal-evidence-bundle (fn [_ _ _] bundle)]
    (sut/report-durable
     (merge {:roots {} :trials [{:admission-request {} :started {}}]
             :control-map-text control-text
             :expected-control-map-sha256 (digest/sha256 control-text)} input))))

(deftest current-durable-results-do-not-invent-preregistration-acceptance
  (let [input {:visibility visibility :expected-series-sha256 sha
               :expected-series-id "RUN4-x"
               :observed-series-sha256 sha}
        a (report input) b (report input)]
    (is (= a b))
    (is (= :operator-decision-required (:decision a)))
    (is (false? (:accepted? a)))
    (is (= :operator-reserved (:acceptance-authority a)))))

(deftest drift-and-unknown-states-refuse
  (is (= :refused-source-drift
         (:decision (report {:visibility visibility
                                 :expected-series-id "RUN4-x"
                                 :expected-series-sha256 sha
                                 :observed-series-sha256 (apply str (repeat 64 "b"))}))))
  (is (= :unsupported-or-incomplete-terminal-state
         (:decision (report {:visibility (assoc visibility :stage "working")
                                 :expected-series-id "RUN4-x"
                                 :expected-series-sha256 sha :observed-series-sha256 sha}))))
  (is (false? (:accepted? (report {:visibility nil}))))
  (let [forged (report {:visibility visibility
                            :expected-series-id "RUN4-x"
                            :expected-series-sha256 sha :observed-series-sha256 sha
                            :route-evidence {:schema :wm/run4-route-conformance-v1
                                             :routes ["invented"]}
                            :recording-evidence
                            {:schema :wm/run4-recording-completeness-v1
                             :complete? true :battery-ref "/missing"}})]
    ;; Forged request fields are ignored; the server-read bundle determines it.
    (is (= true (get-in forged [:checks :route-conformance])))
    (is (= true (get-in forged [:checks :recording-completeness])))
    (is (= :operator-decision-required (:decision forged)))
    (is (false? (:accepted? forged)))))

(deftest incomplete-or-foreign-visibility-cannot-green-terminal-check
  (doseq [v [(dissoc visibility :run_id)
             (assoc visibility :run_id "")
             (assoc visibility :run_id "FOREIGN")
             (assoc visibility :trials [{:trial_id "t1" :stage "working"
                                         :result "pending"}])]]
    (is (false? (get-in (report {:visibility v
                                     :expected-series-id "RUN4-x"
                                     :expected-series-sha256 sha
                                     :observed-series-sha256 sha})
                        [:checks :terminal-task-results])))))

(deftest exact-bundle-route-can-green-route-but-not-acceptance
  (let [r (report {:visibility visibility :expected-series-id "RUN4-x"
                       :expected-series-sha256 sha :observed-series-sha256 sha
                       :terminal-bundles [bundle] :control-map-text control-text
                       :expected-control-map-sha256
                       (digest/sha256 control-text)})]
    (is (true? (get-in r [:checks :route-conformance])))
    (is (= :operator-decision-required (:decision r)))
    (is (= 4 (count (get-in r [:battery :rows]))))
    (is (false? (:accepted? r))))
  (with-redefs [terminal/read-terminal-evidence-bundle
                (fn [_ _ _] (assoc bundle :classification nil))]
    (let [r (sut/report-durable
             {:roots {} :trials [{:admission-request {} :started {}}]
              :visibility visibility :expected-series-id "RUN4-x"
              :expected-series-sha256 sha :observed-series-sha256 sha
              :control-map-text control-text
              :expected-control-map-sha256 (digest/sha256 control-text)})]
      (is (false? (get-in r [:checks :route-conformance]))))))

(deftest visible-and-durable-trials-require-an-exact-bijection
  (let [t2 (assoc-in bundle [:identity :trial-id] :t2)
        duplicated (assoc visibility :trials [(first (:trials visibility))
                                               (first (:trials visibility))])]
    (with-redefs [terminal/read-terminal-evidence-bundle
                  (fn [_ request _] (if (= :second (:which request)) t2 bundle))]
      (let [r (sut/report-durable
               {:roots {} :trials [{:admission-request {:which :first} :started {}}
                                    {:admission-request {:which :second} :started {}}]
                :visibility duplicated :expected-series-id "RUN4-x"
                :expected-series-sha256 sha :observed-series-sha256 sha
                :control-map-text control-text
                :expected-control-map-sha256 (digest/sha256 control-text)})]
        (is (false? (get-in r [:checks :route-conformance]))))))
  (let [string-id (assoc-in bundle [:identity :trial-id] "t1")
        both (assoc visibility :trials [{:trial_id ":t1" :stage "complete" :result "passed"}
                                        {:trial_id "t1" :stage "complete" :result "passed"}])]
    (with-redefs [terminal/read-terminal-evidence-bundle
                  (fn [_ request _] (if (= :string (:which request)) string-id bundle))]
      (let [r (sut/report-durable
               {:roots {} :trials [{:admission-request {:which :keyword} :started {}}
                                    {:admission-request {:which :string} :started {}}]
                :visibility both :expected-series-id "RUN4-x"
                :expected-series-sha256 sha :observed-series-sha256 sha
                :control-map-text control-text
                :expected-control-map-sha256 (digest/sha256 control-text)})]
        (is (true? (get-in r [:checks :route-conformance])))))))
