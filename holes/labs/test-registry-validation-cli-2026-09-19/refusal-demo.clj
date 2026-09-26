(require '[clojure.edn :as edn]
         '[futon3c.test-registry.validation :as v])
(def packet "holes/labs/test-registry-validation-cli-2026-09-19/")
(def opts {:index-file (str packet "demo-subjects.ednlog")
           :queue-file (str packet "demo-queue.ednlog")})
(def warrant "test-registry-13f52fc77768ff4453d68593598d04d99d4ac98fcf252ab60af1bdf6283192a7")
(def at (str (java.time.Instant/now)))
(v/bind-subject! opts "acceptance/refusal-demo" warrant "codex-2" at)
(try (v/bind-subject! opts "acceptance/refusal-demo" "" "codex-2" at)
     (catch clojure.lang.ExceptionInfo e (prn :refused-bind (ex-data e))))
(def incident (v/enqueue-revalidation!
               opts {:subject-id "acceptance/refusal-demo"
                     :incident {:kind :error :source "isolated-acceptance-demo"
                                :detail "Synthetic incident after existing warrant; not a live incident"
                                :at at}}))
(try (v/close-revalidation! opts (:entry/id incident) warrant "codex-2" at)
     (catch clojure.lang.ExceptionInfo e (prn :refused-close (ex-data e))))
(prn :subjects-count (count (v/subjects opts)) :open-incidents (count (v/revalidation-queue opts)))
(v/bind-subject! (assoc opts :index-file (str packet "adversarial-subjects.ednlog"))
                "test-registry/validation-cli" warrant "codex-2" at)
(v/bind-subject! (assoc opts :index-file (str packet "adversarial-subjects.ednlog"))
                "acceptance/fabricated" "test-registry-fabricated-validation-cli-repair" "codex-2" at)
(spit (str packet "adversarial-report.edn")
      (pr-str {:index-file (str packet "adversarial-subjects.ednlog")
               :queue-file (str packet "empty-queue.ednlog")}))
(shutdown-agents)
