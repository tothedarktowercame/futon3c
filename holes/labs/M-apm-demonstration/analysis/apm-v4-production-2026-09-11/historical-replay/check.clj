;; Run ONLY in an isolated test JVM. The runner supplies historical source files.
(require '[futon3c.apm.pattern-revision-review-test :as fixture]
         '[futon3c.apm.pattern-revision-review :as review]
         '[futon3c.apm.typed-role-submission :as submission])
(let [old-root (System/getenv "APM_V4_REPLAY_OLD_ROOT")]
  (assert old-root "Use the isolated historical-replay runner")
  (load-file (str old-root "/typed.clj"))
  (load-file (str old-root "/review.clj")))
(fixture/with-fixture
 (fn [{:keys [options jobs calls]}]
   (let [request (:request (review/prepare! options))
         opts (assoc options :request request)
         id (:submission/job-id request)]
     (assert (:ok (review/dispatch! opts)))
     (fixture/submit-review! request jobs {})
     (assert (:ok (review/collect! opts)))
     (let [original (submission/submitted id)]
       (load-file "src/futon3c/apm/typed_role_submission.clj")
       (load-file "src/futon3c/apm/pattern_revision_review.clj")
       (let [before @calls
             dispatch (review/dispatch! opts)
             collect (review/collect! opts)]
         (doseq [result [dispatch collect]]
           (assert (= :review-request-retirement-required (:status result)))
           (assert (= :pattern-review-legacy-pin-schema (:error/code result)))
           (assert (= id (:job-id result))))
         (assert (= before @calls))
         (assert (= original (submission/submitted id)))
         (println (pr-str {:historical-source "af3b3b26"
                          :original-dispatch-submit-collect :passed
                          :repaired-dispatch :retirement-required
                          :repaired-collect :retirement-required
                          :authority-and-completion-unchanged true
                          :new-agency-effects 0})))))))
(shutdown-agents)
