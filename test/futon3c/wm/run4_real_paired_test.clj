(ns futon3c.wm.run4-real-paired-test
  "Uses relocated historical evidence and frozen successor packet. Only ordinary task
  execution/environment are fixtures; durable readers and resolver are actual."
  (:require [clojure.test :refer [deftest is use-fixtures]] [clojure.edn :as edn]
            [clojure.java.io :as io] [clojure.string :as str]
            [cheshire.core :as json]
            [futon2.aif.c-fold-config :as digest]
            [futon2.aif.hermetic-repair-fixture :as hermetic]
            [futon2.aif.full-loop-cohort :as cohort]
            [futon2.aif.full-loop-runner :as full-runner]
            [futon2.aif.full-loop-runner-test :as ft]
            [futon2.aif.repair-obligation :as repair]
            [futon3c.wm.run4-historical-projection :as historical]
            [futon3c.wm.run4-historical-roundtrip-test :as fresh]
            [futon3c.wm.run4-deployment-config :as deployment]
            [futon3c.wm.run4-series-service :as service]
            [futon3c.wm.run4-historical-successor :as successor]
            [futon3c.wm.runner-service :as runner]
            [futon3c.wm.run4-u88-roundtrip-test :as u]
            [futon3c.agency.registry :as registry]))
(use-fixtures :once hermetic/with-hermetic-stores)
(def base "holes/labs/wm-contract/runs/RUN4-U88-production-successor-2026-09-11-v2/")
(def casting {:author "zai-2" :reviewer "codex-12" :repair-reviewer "codex-12"})
(defn- tmp [] (.toFile (java.nio.file.Files/createTempDirectory "paired-history-" (make-array java.nio.file.attribute.FileAttribute 0))))
(defn- delete! [root] (doseq [f (reverse (file-seq root))] (io/delete-file f true)))
(defn- read-edn [p] (edn/read-string (slurp p)))
(defn- copy-history! [root]
  (let [receipt (json/parse-string
                 (slurp "holes/labs/wm-contract/runs/RUN4-repair057-admission-2026-09-11/LIVE-HISTORICAL-READBACK.json") true)
        link-text (slurp (str base "historical-successor-link.edn"))
        link (edn/read-string link-text)
        source "/home/joe/code/futon3c/holes/labs/wm-contract/runs/RUN4-U88-successor-deployment-2026-09-11/repair-057-revalidation-20260911-v1.verification.edn"
        copy "/home/joe/code/futon2/data/wm-repair-obligations/verification-evidence/repair-057-revalidation-20260911-v1.edn"
        prereg (get-in link [:verification-cohort :preregistration])
        files (distinct (concat (map :path (:sources receipt)) [source copy prereg
            "/home/joe/code/futon2/data/wm-repair-obligations/findings/repair-attempt-057-untyped-failure.edn"
            (str (get-in link [:historical-evidence :roots :admission]) "/"
                 (get-in link [:historical-evidence :admission-request :attempt-id]) "/reservation.edn")
            (str (get-in link [:historical-evidence :roots :admission]) "/"
                 (get-in link [:historical-evidence :admission-request :attempt-id]) "/click-result.edn")]))
        relocate #(str/replace % "/home/joe/" (str (.getPath root) "/"))
        original (into {} (map (fn [p] [p (slurp p)]) files))]
    (doseq [{:keys [path sha256]} (:sources receipt)]
      (is (= sha256 (digest/sha256 (get original path)))))
    ;; Rebase path-bearing evidence, then propagate dependency digests to fixed point.
    ;; These are explicitly transformed test copies, never production receipts.
    (loop [texts (into {} (map (fn [[p text]] [p (pr-str (edn/read-string (relocate text)))]) original))
           hashes (into {} (map (fn [[p text]]
                                 [p (digest/sha256
                                     (if (= :wm/run4-historical-admission-projection-v1
                                            (:schema (edn/read-string text)))
                                       (pr-str (edn/read-string text)) text))]) original))
           link-text (relocate link-text) n 0]
      (assert (< n 20) "digest graph must converge")
      (let [next-hashes (into {} (map (fn [[p text]] [p (digest/sha256 text)]) texts))
            replacements (for [[p hash] hashes :when (not= hash (next-hashes p))]
                           [hash (next-hashes p)])
            replace-hashes (fn [text] (reduce (fn [s [a b]] (str/replace s a b)) text replacements))]
        (if (empty? replacements)
          (do (doseq [[p text] texts] (io/make-parents (relocate p)) (spit (relocate p) text))
              (edn/read-string link-text))
          (recur (into {} (map (fn [[p text]] [p (pr-str (edn/read-string (replace-hashes text)))]) texts))
                 next-hashes (replace-hashes link-text) (inc n)))))))
(defn- close-fixture-cohort! [{:keys [preregistration data-root]}]
  (let [binding {:preregistration preregistration :data-root data-root
                 :cohort-id (:cohort/id (read-edn preregistration))
                 :sha256 (digest/sha256 (slurp preregistration))}
        authority (cohort/execution-authority binding)
        term #(hash-map :judgment % :ground {:kind :test-witness})
        s (cohort/start-attempt! preregistration data-root
           (term {:opportunity-id "successor-fixture/1" :trigger :duree-click-on-demand
                  :machine-state {} :agent-roster [] :semantic-epoch :test
                  :code-state {:git-sha "fixture" :git-dirty? false :resolved-mode-flags {}
                               :configuration-digest "fixture"}
                  :execution-authority authority}))]
    (is (= "attempt-001" (:attempt/id s)))
    (doseq [k [:selection :construction :dispatch :build :adjudication]]
      (cohort/append-checkpoint! preregistration data-root "attempt-001" k
                                {:sorry {:kind :task-core-fixture}}))
    (cohort/close-attempt! preregistration data-root "attempt-001"
       (term {:outcome :grounded-change :grounded? true :artifact-only? false
              :duration-ms 1 :resource-use {:agent-turns 0}
              :witness {:before "a" :after "b" :resolved? true :dial-moved? true}}))
    (cohort/closed-execution binding "attempt-001")))
(def fixture-core
(fn [opts]
              (let [execution (close-fixture-cohort! (:execution-cohort opts))
                    action {:type :advance-mission :target "M-u88-contextual-preferences"}
                    judgment {:decision {:action {:type :no-op}}
                              :ranked-actions [{:rank 1 :action action}]
                              :admissible-actions [{:rank 1 :action action}]}
                    selected (full-runner/resolve-pinned-selection
                              opts judgment (select-keys opts (keys casting)))
                    identity (:identity selected)]
                {:attempt-id "attempt-001"
                 :execution-identity (select-keys execution [:kind :id])
                 :execution-provenance (dissoc execution :outcome)
                 :outcome :grounded-change
                 :checkpoints
                 {:selection {:judgment {:outcome :ok}
                              :ground {:kind :wm-judgement :run4/task-pin identity
                                       :run4/operator-selection
                                       (:provenance selected)}}
                  :construction {:judgment {:run4/task-pin identity}
                                 :ground {:kind :decision-pinned-construction
                                          :run4/task-pin identity}}
                  :dispatch {:judgment {:agent "zai-2" :availability :invoke-ready
                                        :job-id "author-job-1"}
                             :ground {:kind :agency-dispatch}}
                  :build {:judgment {:commits ["commit-1"]
                                     :validation
                                     {:approved? true :review-job "review-job-1"
                                      :review-gate {:required? true :executed? true
                                                    :tool-events 2 :passed? true}}}
                          :ground {:kind :git-commit-and-independent-review}}
                  :adjudication
                  {:judgment {:build-match {:commit "commit-1"
                                            :review-approved? true}
                              :dial {:moved? true :implementation-id "impl-1"}}
                   :ground {:kind :authoritative-substrate-discharge}}}
                 :data {:commit "commit-1"
                        :author-job {:job-id "author-job-1"}
                        :review-job {:job-id "review-job-1"}
                        :witness {:resolved? true :dial-moved? true
                                  :implementation-id "impl-1"}}
                 :wm/route [{:node :R20 :via "scan" :at "2026-09-10T12:00:00Z"}
                            {:node :R12 :via "select" :at "2026-09-10T12:00:01Z"}] })))
(deftest ^:slow actual-paired-readers-resolve-and-replay
  (let [root (tmp)]
    (try
      (let [link (copy-history! root)
            hist (:historical-evidence link)
            bundle (historical/read-bundle! (:roots hist) (:admission-request hist) (:started hist))
            store (get-in hist [:roots :repair-root])
            materialize deployment/materialize
            captured (atom nil)]
        (is (= :awaiting-validation (get-in bundle [:classification :repair-status])))
        (with-redefs [u/template-path (str base "server-config.disabled.edn")
                      deployment/materialize
                      (fn [text deps]
                        (let [t (edn/read-string text)
                              ref (first (filter #(str/ends-with? % "/cohort.edn") (:source-allowlist t)))
                              prereg (.getCanonicalPath (io/file (:authority-root t) ref))
                              data (.getPath (doto (io/file (:authority-root t) "successor-cohort") .mkdir))
                              cb {:preregistration prereg :data-root data
                                  :cohort-id (:cohort/id (read-edn prereg))
                                  :sha256 (digest/sha256 (slurp prereg))}
                              _ (cohort/activate! prereg data)]
                          (reset! captured cb)
                          (materialize text (assoc deps :execution-cohort cb
                                                   :cohort-preflight! cohort/execution-preflight
                                                   :historical-successor link))))]
          (#'u/with-service
           (fn [service-root cfg]
             (reset! runner/!status runner/initial-status)
             (registry/reset-registry!)
             (registry/register-agent! {:agent-id {:id/value "war-machine" :id/type :apparatus}
                                       :type :wm :invoke-fn nil :capabilities [] :metadata {:apparatus? true}})
             (binding [full-runner/*wm-status-reporting?* false]
               (with-redefs-fn {#'full-runner/run-opportunity-core! fixture-core}
                 (fn []
                   (let [req {:run4-series-ref (get-in cfg [:run4 :series :manifest-ref])}
                         start (service/step! cfg u/auth req)]
                     (is (= :trial-started (:status start)))
                     (is (= :completed (:status (runner/await-click! (:click-id start)))))
                     ;; Inject failure before resolution, after the controller has persisted terminal.
                     ;; Recovery below calls the real resolver and both real readers.
                     (with-redefs [successor/resolve-from-durable!
                                   (fn [& _] (throw (ex-info "injected post-terminal failure" {})))]
                       (is (thrown-with-msg? clojure.lang.ExceptionInfo
                                             #"injected post-terminal failure"
                                             (service/step! cfg u/auth req))))
                     (is (.isFile (io/file service-root "store-controller-and-admission/001-terminal.edn")))
                     (is (= 1 (count (repair/open-obligations store))))
                     (is (= :series-terminal (:status (service/step! cfg u/auth req))))
                     (is (empty? (repair/open-obligations store)))
                     (is (= :series-terminal (:status (service/step! cfg u/auth req))))
                     (is (= 1 (:attempt-count (cohort/ledger (:preregistration @captured) (:data-root @captured)))))
                     (let [resolution (io/file store "resolutions/repair-attempt-057-untyped-failure.edn")
                           original (slurp resolution)]
                       (is (= :resolved (:repair/status (edn/read-string original))))
                       (spit resolution (pr-str (assoc (edn/read-string original) :run-id "foreign-run")))
                       (let [conflict (slurp resolution)]
                         (is (thrown-with-msg? clojure.lang.ExceptionInfo
                                               #"Historical resolution conflicts"
                                               (service/step! cfg u/auth req)))
                         (is (= conflict (slurp resolution))))
                       (spit resolution original))
                     (is (.isFile (io/file service-root "store-controller-and-admission/001-terminal.edn")))))))))))
      (finally (delete! root)))))

(deftest ^:slow fresh-authority-historical-admission-resolves-through-fresh-successor
  (fresh/with-authority
   (fn [historical-deps]
     (let [materialize deployment/materialize
           actual full-runner/run-opportunity!
           historical-link (atom nil)
           historical-service-root (atom nil)]
       (with-redefs [deployment/materialize
                     (fn [text dependencies]
                       (materialize text (merge dependencies historical-deps)))
                     full-runner/run-opportunity!
                     (fn [opts]
                       (actual (merge opts (ft/isolated-runner-opts)
                                      {:cohort? true
                                       :roster-fn (fn [_] {:zai-2 {:status "idle" :invoke-ready? true}
                                                           :codex-12 {:status "idle" :invoke-ready? true}
                                                           :codex-17 {:status "idle" :invoke-ready? true}})
                                       :repair-open-fn
                                       #(repair/open-obligations
                                         (get-in historical-deps
                                                 [:historical-action :repair-root]))})))]
         (with-redefs-fn
          {(ns-resolve 'futon3c.wm.run4-u88-roundtrip-test 'delete-tree!)
           (fn [_] nil)}
          (fn []
           (#'u/with-service
            (fn [root cfg]
            (reset! historical-service-root root)
            (reset! runner/!status runner/initial-status)
            (registry/reset-registry!)
            (registry/register-agent! {:agent-id {:id/value "war-machine" :id/type :apparatus}
                                       :type :wm :invoke-fn nil :capabilities []
                                       :metadata {:apparatus? true}})
            (binding [full-runner/*wm-status-reporting?* false]
              (let [req {:run4-series-ref (get-in cfg [:run4 :series :manifest-ref])}
                    started-result (service/step! cfg u/auth req)
                    _ (is (= :completed
                             (:status (runner/await-click! (:click-id started-result)))))
                    started (read-edn (io/file root "store-controller-and-admission/001-started.edn"))
                    reservation (read-edn
                                 (io/file root "store-controller-and-admission"
                                          (:attempt-id started) "reservation.edn"))
                    roots {:admission (get-in cfg [:run4 :admission-root])
                           :bindings (get-in cfg [:run4 :series :binding-root])
                           :projections (get-in cfg [:run4 :series :projection-root])
                           :run-records (get-in cfg [:run4 :series :run-record-root])
                           :repair-root (get-in historical-deps [:historical-action :repair-root])
                           :cohort-preregistration
                           (get-in historical-deps [:execution-cohort :preregistration])
                           :cohort-data-root
                           (get-in historical-deps [:execution-cohort :data-root])}
                    bundle (historical/read-bundle!
                            roots {:attempt-id (:attempt-id started)
                                   :identity (:identity reservation)} started)
                    frozen (read-edn (str base "historical-successor-link.edn"))]
                (is (= 1 (get-in bundle [:closed-execution :identity-version])))
                (reset! historical-link
                        {:repair-id (get-in bundle [:projection :repair :id])
                         :verification-id (get-in bundle [:projection :repair :verification-id])
                         :verification-attempt (get-in bundle [:projection :execution-attempt])
                         :verification-cohort (:execution-cohort historical-deps)
                         :historical-evidence {:roots roots
                                               :admission-request
                                               {:attempt-id (:attempt-id started)
                                                :identity (:identity reservation)}
                                               :started started}
                         :successor (:successor frozen)}))))))))
       (let [captured (atom nil)]
         (with-redefs [u/template-path (str base "server-config.disabled.edn")
                       deployment/materialize
                       (fn [text deps]
                         (let [t (edn/read-string text)
                               ref (first (filter #(str/ends-with? % "/cohort.edn")
                                                  (:source-allowlist t)))
                               prereg (.getCanonicalPath (io/file (:authority-root t) ref))
                               data (.getPath (doto (io/file (:authority-root t)
                                                            "fresh-successor-cohort") .mkdir))
                               cb {:preregistration prereg :data-root data
                                   :cohort-id (:cohort/id (read-edn prereg))
                                   :sha256 (digest/sha256 (slurp prereg))}]
                           (cohort/activate! prereg data)
                           (reset! captured cb)
                           (materialize text (assoc deps :execution-cohort cb
                                                    :cohort-preflight! cohort/execution-preflight
                                                    :historical-successor @historical-link))))]
           (#'u/with-service
            (fn [_ cfg]
              (reset! runner/!status runner/initial-status)
              (registry/reset-registry!)
              (registry/register-agent! {:agent-id {:id/value "war-machine" :id/type :apparatus}
                                         :type :wm :invoke-fn nil :capabilities []
                                         :metadata {:apparatus? true}})
              (binding [full-runner/*wm-status-reporting?* false]
                (with-redefs [full-runner/run-opportunity-core! fixture-core]
                  (let [req {:run4-series-ref (get-in cfg [:run4 :series :manifest-ref])}
                        start (service/step! cfg u/auth req)]
                    (is (= :completed (:status (runner/await-click! (:click-id start)))))
                    (is (= :trial-terminal (:status (service/step! cfg u/auth req))))
                    (is (empty? (repair/open-obligations
                                 (get-in @historical-link
                                         [:historical-evidence :roots :repair-root]))))
                    (is (= :series-terminal (:status (service/step! cfg u/auth req))))
                    (is (= 1 (:attempt-count
                              (cohort/ledger (:preregistration @captured)
                                             (:data-root @captured))))))))))))
         (delete! @historical-service-root)))))
