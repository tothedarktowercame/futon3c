(ns futon3c.wm.run4-trusted-entry-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon2.aif.c-fold-config :as digest]
            [futon2.aif.full-loop-runner :as full-runner]
            [futon3c.wm.run4-effective-environment :as effective]
            [futon3c.wm.run4-historical-action :as historical-action]
            [futon3c.wm.run4-trusted-entry :as sut]))

(def token (apply str (repeat 64 "a")))
(def casting {:author "codex-10" :reviewer "codex-17"
              :repair-reviewer "codex-1"})
(def mission {:id "M-run4" :status-class :active})
(def source-text "bounded task\n")
(def config-text
  (str (pr-str {:schema :wm/run4-pinned-run-config-v1
                :runner-options {:accumulate-strategic-habit? false}
                :c-fold {:enabled? false}
                :serving-declaration
                {:required-environment
                 {"FUTON_WM_FPI_DARK" "1" "FUTON_WM_BETA_DARK" "1"
                  "FUTON_WM_TRACE_POLICY_DETAILS" "1"}
                 :hierarchy {:model :single-level :scope :RUN4}
                 :recording-requirement
                 {:contract :wm/realized-recording-v1
                  :environment {"FUTON_WM_RECORDING_CONTRACT" "1"}}}}) "\n"))

(defn pin [overrides]
  (merge {:schema :wm/run4-task-pin-v1
          :series-id "RUN4-2026-09-10"
          :trial-id :outer-loop-successor
          :series-order :as-declared
          :candidate-task-ids [:outer-loop-successor :math-probe
                               :caption-probe :feedback-monitor]
          :selected-task-id :outer-loop-successor
          :sources [{:path "source.md" :sha256 (digest/sha256 source-text)}]
          :casting casting
          :operator-selection {:mode :operator-selected :operator "Joe"
                               :authority-ref "SERIES.edn selection"}
          :config {:path "config.edn" :sha256 (digest/sha256 config-text)}
          :mapping {:mission-id "M-run4"
                    :action {:type :advance-mission :target "M-run4"}}}
         overrides))

(defn delete-tree! [root]
  (doseq [f (reverse (file-seq root))] (io/delete-file f true)))

(defn with-fixture [f]
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "run4-auth" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (try
      (spit (io/file root "source.md") source-text)
      (spit (io/file root "config.edn") config-text)
      (spit (io/file root "cohort.edn")
            "{:cohort/id :run4-test :target 2}\n")
      (spit (io/file root "seed.edn")
            (slurp (io/file ".." "futon2" "resources" "run4" "seeded-c.edn")))
      (spit (io/file root "kernel.edn")
            (slurp (io/file ".." "futon2" "resources" "run4"
                            "checkpoint-kernel.edn")))
      (spit (io/file root "pin.edn") (pr-str (pin {})))
      (binding [sut/*attest-effective-environment*
                (fn [declaration]
                  {:schema :wm/run4-effective-environment-attestation-v1
                   :hierarchy (:hierarchy declaration)
                   :flags (mapv (fn [[flag consumer]]
                                  {:flag flag :required "1" :observed "1"
                                   :effective true :consumer consumer})
                                (sort-by key effective/flag-spec))
                   :recording
                   {:status :not-attested-by-this-component
                    :consumer "holes/labs/wm-contract/wm_step_observe.bb"}})]
        (let [cohort-file (io/file root "cohort.edn")
              cohort {:preregistration (.getCanonicalPath cohort-file)
                      :data-root (.getCanonicalPath root)
                      :cohort-id :run4-test
                      :sha256 (digest/sha256 (slurp cohort-file))}]
        (f {:root root
          :config {:run4 {:enabled? true :bearer-token token :operator "Joe"
                          :casting casting
                          :admission-root (.getPath root)
                          :pin-root (.getPath root) :pin-allowlist #{"pin.edn"}
                          :source-root (.getPath root)
                          :source-allowlist #{"source.md" "config.edn"
                                              "seed.edn" "kernel.edn"}
                          :resolve-mission #(when (= "M-run4" %) mission)
                          :execution-cohort cohort
                          :cohort-preflight!
                          (fn [requested]
                            {:cohort-id (:cohort-id requested)
                             :target 2 :remaining 2 :snapshot ::internal})
                          :action-admissible?
                          #(and (= mission %1)
                                (= {:type :advance-mission :target "M-run4"} %2))}}})))
      (finally (delete-tree! root)))))

(def auth {"authorization" (str "Bearer " token)})
(def request {:run4-pin-ref "pin.edn" :run4-attempt-id "attempt-1"})

(deftest prepared-options-retain-requested-pin-without-enacting-it
  (with-fixture
    (fn [{:keys [root config]}]
      (let [configured (assoc-in config [:run4 :historical-action]
                                 {:repair-root (.getPath root)
                                  :verification-root (.getPath root)
                                  :verification-path (.getPath (io/file root "verification.edn"))
                                  :verification-sha256 (apply str (repeat 64 "b"))})
            prepared (with-redefs [historical-action/validate-applicable!
                                   (constantly {:repair-id "repair-test"})]
                       (sut/prepare configured auth request))]
        (is (:ok prepared))
        (is (fn? (get-in prepared [:opts :historical-verification-candidate-fn])))
        (is (= :authenticated-not-enacted
               (get-in prepared [:opts :run4/requested-pin :status])))
        (is (nil? (get-in prepared [:opts :run4/enacted-action])))))))

(deftest inapplicable-historical-action-refuses-before-cohort-preflight
  (with-fixture
    (fn [{:keys [root config]}]
      (let [cohort-called (atom 0)
            configured (-> config
                           (assoc-in [:run4 :historical-action]
                                     {:repair-root (.getPath root)
                                      :verification-root (.getPath root)
                                      :verification-path (.getPath (io/file root "verification.edn"))
                                      :verification-sha256 (apply str (repeat 64 "b"))})
                           (assoc-in [:run4 :cohort-preflight!]
                                     (fn [_] (swap! cohort-called inc))))]
        (with-redefs [historical-action/validate-applicable!
                      (fn [_] (throw (ex-info "wrong stop-line" {})))]
          (is (= :run4-historical-action-not-applicable
                 (:error (sut/prepare configured auth request))))
          (is (zero? @cohort-called)))))))

(def action {:type :advance-mission :target "M-run4"})
(def judgment {:decision {:action {:type :no-op}}
               :ranked-actions [{:rank 1 :action action}]
               :admissible-actions [{:rank 1 :action action}]})

(deftest authenticates-validates-and-mints-one-use-digest-context
  (with-fixture
    (fn [{:keys [config]}]
      (let [result (sut/prepare config auth request)
            opts (:opts result)
            trust (:run4-trusted-boundary-fn opts)
            sha (digest/sha256 (:run4-task-pin-text opts))]
        (is (:ok result))
        (is (= casting (select-keys opts (keys casting))))
        (is (not (contains? opts :cohort?)))
        (is (= :run4-test (get-in opts [:execution-cohort :cohort-id])))
        (is (false? (:accumulate-strategic-habit? opts)))
        (is (false? (:ruled-outcome-c-enabled? opts)))
        (is (= {:path "config.edn" :sha256 (digest/sha256 config-text)}
               (:run4/config-pin opts)))
        (is (= source-text ((get-in opts [:run4-task-pin-ports :read-text])
                            "source.md")))
        (is (thrown? clojure.lang.ExceptionInfo
                     (trust {:pin-digest "wrong"
                             :operator-selection {:operator "Joe"}})))
        (is (thrown? clojure.lang.ExceptionInfo
                     (trust {:pin-digest sha
                             :operator-selection {:operator "Mallory"}})))
        (let [attestation (trust {:pin-digest sha
                                  :operator-selection {:operator "Joe"}})]
          (is (= sha (:pin-sha256 attestation)))
          (is (= :wm/run4-effective-environment-attestation-v1
                 (get-in attestation [:effective-environment :schema]))))
        (is (thrown? clojure.lang.ExceptionInfo
                     (trust {:pin-digest sha
                             :operator-selection {:operator "Joe"}})))))))

(deftest credential-configuration-and-header-are-strict
  (with-fixture
    (fn [{:keys [config]}]
      (doseq [bad [(assoc-in config [:run4 :enabled?] :yes)
                   (assoc-in config [:run4 :bearer-token] (apply str (repeat 64 " ")))
                   (assoc-in config [:run4 :bearer-token] "change-me")
                   (assoc-in config [:run4 :bearer-token] (apply str (repeat 64 "A")))]]
        (is (= :run4-credential-configuration-invalid
               (:error (sut/prepare bad auth request)))))
      (doseq [headers [{} {"authorization" token}
                       {"authorization" (str "bearer " token)}
                       {"authorization" 42}]]
        (is (= :run4-authentication-failed
               (:error (sut/prepare config headers request))))))))

(deftest refuses-untrusted-request-and-invalid-server-authority
  (with-fixture
    (fn [{:keys [config]}]
      (is (= :run4-request-key-forbidden
             (:error (sut/prepare config auth
                                  (assoc request :authenticated true)))))
      (is (= :run4-pin-reference-refused
             (:error (sut/prepare config auth (assoc request :run4-pin-ref "../pin.edn")))))
      (is (= :run4-casting-mismatch
             (:error (sut/prepare config auth
                                  (assoc request :author "forged")))))
      (is (= :run4-disabled
             (:error (sut/prepare {} auth request))))
      (is (= :run4-port-configuration-invalid
             (:error (sut/prepare (assoc-in config [:run4 :resolve-mission] nil)
                                  auth request))))
      (is (= :run4-file-authority-invalid
             (:error (sut/prepare (assoc-in config [:run4 :source-allowlist] ["source.md"])
                                  auth request))))
      (is (= :run4-attempt-identity-invalid
             (:error (sut/prepare config auth (dissoc request :run4-attempt-id))))))))

(deftest refuses-invalid-pin-before-returning-runner-options
  (with-fixture
    (fn [{:keys [root config]}]
      (testing "stale source"
        (spit (io/file root "pin.edn")
              (pr-str (pin {:sources [{:path "source.md"
                                       :sha256 (apply str (repeat 64 "0"))}]})))
        (is (= {:error :run4-pin-invalid :reason :stale-source}
               (select-keys (sut/prepare config auth request)
                            [:error :reason]))))
      (testing "forged operator"
        (spit (io/file root "pin.edn")
              (pr-str (pin {:operator-selection
                            {:mode :operator-selected :operator "Mallory"
                             :authority-ref "forged"}})))
        (is (= :run4-operator-mismatch
               (:error (sut/prepare config auth request)))))
      (testing "casting differs from server identity"
        (spit (io/file root "pin.edn")
              (pr-str (pin {:casting (assoc casting :author "zai-2")})))
        (is (= :run4-casting-mismatch
               (:error (sut/prepare config auth request)))))
      (testing "unsupported but freshly repinned run config"
        (let [bad "{:schema :invented :runner-options {} :c-fold {:enabled? false}}\n"]
          (spit (io/file root "config.edn") bad)
          (spit (io/file root "pin.edn")
                (pr-str (pin {:config {:path "config.edn"
                                      :sha256 (digest/sha256 bad)}})))
          (is (= :run4-pinned-config-invalid
                 (:error (sut/prepare config auth request)))))))))

(deftest runner-boundary-rereads-source-and-config-after-preparation
  (with-fixture
    (fn [{:keys [root config]}]
      (doseq [[path changed]
              [["source.md" "changed task\n"]
               ["config.edn"
                "{:schema :wm/run4-pinned-run-config-v1 :runner-options {:cohort? true} :c-fold {:enabled? false}}\n"]]]
        ;; Each case gets a fresh preparation and then changes authoritative
        ;; bytes before the actual selection boundary consumes the options.
        (spit (io/file root "source.md") source-text)
        (spit (io/file root "config.edn") config-text)
        (let [prepared (sut/prepare config auth request)
              opts (:opts prepared)]
          (is (:ok prepared))
          (spit (io/file root path) changed)
          (is (= changed ((get-in opts [:run4-task-pin-ports :read-text]) path)))
          (is (= :invalid-or-stale-task-pin
                 (:failure-detail
                  (try
                    (full-runner/resolve-pinned-selection opts judgment casting)
                    nil
                    (catch clojure.lang.ExceptionInfo e (ex-data e)))))))))))

(deftest trusted-attestation-rereads-c-fold-artifacts
  (with-fixture
    (fn [{:keys [root config]}]
      (let [seed (slurp (io/file root "seed.edn"))
            kernel (slurp (io/file root "kernel.edn"))
            fold-config
            (str (pr-str
                  {:schema :wm/run4-pinned-run-config-v1
                   :runner-options {}
                   :c-fold {:enabled? true
                            :seed {:id :ruled-outcome-c-v1 :path "seed.edn"
                                   :sha256 (digest/sha256 seed)}
                            :kernel {:adapter :constant-checkpoint-kernel/v1
                                     :path "kernel.edn"
                                     :sha256 (digest/sha256 kernel)}}}) "\n")]
        (spit (io/file root "config.edn") fold-config)
        (spit (io/file root "pin.edn")
              (pr-str (pin {:config {:path "config.edn"
                                    :sha256 (digest/sha256 fold-config)}})))
        (let [prepared (sut/prepare config auth request)]
          (is (:ok prepared))
          (is (true? (get-in prepared [:opts :ruled-outcome-c-enabled?])))
          (spit (io/file root "kernel.edn") "{:schema :changed}\n")
          (is (= :captured-source-changed
                 (:reason
                  (try
                    (full-runner/resolve-pinned-selection
                     (:opts prepared) judgment casting)
                    nil
                    (catch clojure.lang.ExceptionInfo e (ex-data e)))))))))))
