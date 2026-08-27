(ns futon3c.apm.incident-regression-fixtures-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [futon3c.agency.inbox :as agency-inbox]
            [futon3c.agency.parked-on :as parked-on]
            [futon3c.agency.registry :as registry]
            [futon3c.apm.durable-coordinator :as coordinator]
            [futon3c.apm.live-job-driver :as job-driver]
            [futon3c.apm.role-memory-search :as memory-search]
            [futon3c.apm.semantic-progress-watchdog :as watchdog]
            [futon3c.transport.http :as http]))

(def incidents
  (-> "test/resources/apm-regressions/incidents-2026-08-27.edn"
      slurp edn/read-string))

(defn holdout-valid?
  [{:keys [served-ids used-ids withheld-ids problem-id depositor-problem-id]}]
  (and (empty? (set/intersection
                (set withheld-ids) (set (concat served-ids used-ids))))
       ;; The intended M2 policy is depositor truth, including store-only
       ;; memories that never entered the shelf-derived withheld set.
       (not (and (some? depositor-problem-id)
                 (= problem-id depositor-problem-id)
                 (seq served-ids)))))

(defn predecessor-valid? [state]
  (let [predecessor (first (:superseded-terminals state))]
    (boolean
     (and (seq (get-in predecessor [:job :report :memory-use :used-ids]))
          (get-in predecessor [:terminal-collection :evidence :collection/id])
          (true? (get-in predecessor
                         [:trace/successor-observation
                          :predecessor-persisted?]))))))

(defn progress-valid? [progress enable-history]
  (and (= true (:first-violation-recorded? progress))
       (= true (:coordinator-disabled? progress))
       (= false (:enabled/new (last enable-history)))))

(defn delivery-valid? [delivery observation]
  (and (= "delivered" (:status delivery))
       (or (:inbox-file-created? observation)
           (:registered-push-performed? observation))))

(defn search-result [ids]
  {:index-as-of "incident-fixture"
   :content-matches (mapv (fn [id] {:memory/id id :text "fixture"}) ids)
   :candidates []})

(deftest real-holdout-incidents-have-mechanism-derived-deltas
  (doseq [incident (take 2 (:holdout incidents))]
    (testing (name (:id incident))
      (let [historical incident
            authority {:shelf/holdout :same-problem
                       :shelf/withheld-ids (:withheld-ids incident)}
            withheld (memory-search/withheld-for-authority authority)
            enforced (memory-search/enforce-holdout
                      (search-result (:served-ids incident)) withheld)
            upgraded (assoc incident
                            :served-ids (mapv :memory/id
                                              (get-in enforced
                                                      [:result :content-matches]))
                            :used-ids [])]
        (is (false? (holdout-valid? historical)) "historical REJECTED")
        (is (true? (holdout-valid? upgraded)) "upgraded ACCEPTED")
        (is (= (vec (sort (:withheld-ids incident))) (:excluded enforced))
            "delta is enforce-holdout output, using withheld-for-authority")))))

(deftest store-only-f47-pair-stays-red-until-m2-exists
  (let [incident (last (:holdout incidents))
        authority {:shelf/holdout :same-problem
                   :shelf/withheld-ids (:withheld-ids incident)}
        enforced (memory-search/enforce-holdout
                  (search-result (:served-ids incident))
                  (memory-search/withheld-for-authority authority))
        upgraded (assoc incident
                        :served-ids (mapv :memory/id
                                          (get-in enforced
                                                  [:result :content-matches])))]
    (is (false? (holdout-valid? incident)) "historical REJECTED")
    (is (false? (holdout-valid? upgraded))
        "upgraded remains REJECTED: M2 depositor-truth mechanism is absent")
    (is (= [] (:excluded enforced))
        "the actual shelf-derived mechanism produces no delta")
    (is (= :reject-missing-m2 (:expected-upgrade incident)))))

(defn run-repair [incident]
  (let [calls (atom [])
        persisted (atom [])
        request {:dispatch/id "historical-dispatch" :agent-id "student"
                 :frame-id (name (:id incident)) :problem-id "historical"
                 :phase :student-attempt-1}
        terminal {:job-id (:predecessor-id incident) :state :done
                  :report {:memory-use {:used-ids (:used-ids incident)}}}
        effects {:request request
                 :announce-fn (fn [r]
                                (swap! calls conj
                                       [:announce (:dispatch/id r)
                                        (boolean (:superseded-terminals
                                                  (last @persisted)))])
                                {:ok true :job-id
                                 (if (= "repair-dispatch" (:dispatch/id r))
                                   "successor-job" (:predecessor-id incident))})
                 :activate-fn (fn [& _] {:ok true})
                 :job-fn (constantly terminal)
                 :persist-fn (fn [state]
                               (swap! persisted conj state)
                               {:ok true})
                 :terminal-validator
                 (constantly {:ok false
                              :findings [:student-memory-used-despite-holdout]})
                 :receipt-provider (constantly {:ok true})
                 :terminal-repair-request-fn
                 (fn [r & _]
                   {:ok true :request (assoc r :dispatch/id "repair-dispatch")})}
        dispatched (:state (job-driver/drive! effects))
        collection {:evidence {:collection/id (:collection-id incident)}}
        repaired (job-driver/drive!
                  (assoc effects :state (assoc dispatched
                                               :terminal-collection collection)))]
    {:result repaired :calls @calls :persisted @persisted}))

(deftest real-overwrite-incidents-have-append-before-announce-deltas
  (doseq [incident (:durability incidents)]
    (testing (name (:id incident))
      (let [historical {:terminal-collection nil
                        :used-ids (:surviving-used-ids incident)}
            {:keys [result calls]} (run-repair incident)
            upgraded (:state result)
            repair-announcement (last (filter #(= :announce (first %)) calls))]
        (is (false? (predecessor-valid? historical)) "historical REJECTED")
        (is (true? (predecessor-valid? upgraded)) "upgraded ACCEPTED")
        (is (= [:announce "repair-dispatch" true] repair-announcement)
            "live-job-driver persisted :superseded-terminals before announce")
        (is (= (:used-ids incident)
               (get-in upgraded
                       [:superseded-terminals 0 :job :report
                        :memory-use :used-ids])))))))

(defn write-edn! [file value]
  (spit file (str (pr-str value) "\n")))

(deftest f49-failure-gains-watchdog-and-enable-transition
  (let [incident (:progress incidents)
        directory (.toFile (java.nio.file.Files/createTempDirectory
                            "f49-progress-fixture"
                            (make-array java.nio.file.attribute.FileAttribute 0)))
        state-file (io/file directory "state.edn")
        registry-file (io/file directory "registry.edn")
        coordinator-id "fixture:f49"]
    (try
      (write-edn! state-file
                  {:state/type :live-regulator :regulator/id coordinator-id
                   :regulator/status :failed :regulator/ticks 1
                   :regulator/last-result {:ok false
                                           :error/code (:failure-code incident)}})
      (binding [coordinator/*enabled-transition-now-fn* (constantly 1787840000000)]
        (is (:ok (coordinator/register!
                  {:registry-path registry-file :coordinator-id coordinator-id
                   :adapter :fixture/f49 :config {} :state-path state-file
                   :period-ms 2000})))
        (let [historical-progress nil
              historical-history (:coordinator/enabled-history
                                  (get-in (coordinator/read-registry registry-file)
                                          [:entries coordinator-id]))
              stopped (atom nil)
              result (watchdog/check!
                      {:watch-state nil
                       :observation {:cursor {:frame-id "f49"}
                                     :coordinator-enabled? true
                                     :regulator {:regulator/status :failed}
                                     :supervisor/status :ready}
                       :now-ms 1787840000000
                       :registry-path registry-file
                       :coordinator-id coordinator-id
                       :stop-fn (fn [path id]
                                  (reset! stopped (coordinator/stop! path id))
                                  @stopped)
                       :persist-fn (fn [_] {:ok true})})
              progress (get-in result [:state :watchdog/trace-observation])
              upgraded-history (:coordinator/enabled-history
                                (get-in (coordinator/read-registry registry-file)
                                        [:entries coordinator-id]))]
          (is (not (progress-valid? historical-progress historical-history))
              "historical REJECTED")
          (is (progress-valid? progress upgraded-history) "upgraded ACCEPTED")
          (is (= :regulator-failed (get-in result [:reason :code]))
              "watchdog produced the progress delta")
          (is (:durably-disabled? @stopped))
          (is (= (inc (count historical-history))
                 (count upgraded-history))
              "durable-coordinator/stop! appended the enable delta")))
      (finally
        (doseq [file (reverse (file-seq directory))]
          (io/delete-file file true))))))

(deftest delivery-pair-is-produced-by-pull-only-completion-path
  (let [historical (first (get-in incidents [:delivery :historical]))
        directory (.toFile (java.nio.file.Files/createTempDirectory
                            "delivery-regression"
                            (make-array java.nio.file.attribute.FileAttribute 0)))
        ledger-file (io/file directory "jobs.edn")
        caller "fixture-pull-only"
        job-id (:job-id historical)]
    (try
      (with-redefs-fn
        {#'http/invoke-jobs-store-path (constantly (.getPath ledger-file))
         #'agency-inbox/inbox-root (constantly directory)}
        (fn []
          (registry/reset-registry!)
          (parked-on/clear!)
          (http/reset-invoke-jobs!)
          (registry/register-agent!
           {:agent-id caller :type :mock :delivery-mode :inbox
            :invoke-fn (fn [& _] {:result "unused"}) :capabilities [:invoke]})
          (#'http/create-invoke-job!
           {:requested-job-id job-id :agent-id "unregistered-worker"
            :prompt "incident regression" :caller caller :surface "bell"})
          (#'http/finalize-invoke-job! job-id "done" nil nil
                                      {:ok true :result "done"} "session")
          (#'http/record-bell-completion-delivery! job-id caller {:ok true})
          (let [job (#'http/get-invoke-job job-id)
                upgraded (:delivery job)
                observation (:trace/delivery-observation job)]
            (is (false? (delivery-valid? historical
                                         {:inbox-file-created? false
                                          :registered-push-performed? false}))
                "historical REJECTED")
            (is (delivery-valid? upgraded observation) "upgraded ACCEPTED")
            (is (= "inbox" (:surface upgraded)))
            (is (true? (:inbox-file-created? observation))
                "ebea1f84 completion path produced the delta"))))
      (finally
        (registry/reset-registry!)
        (parked-on/clear!)
        (http/reset-invoke-jobs!)
        (doseq [file (reverse (file-seq directory))]
          (io/delete-file file true))))))
