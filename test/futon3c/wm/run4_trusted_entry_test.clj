(ns futon3c.wm.run4-trusted-entry-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon2.aif.c-fold-config :as digest]
            [futon3c.wm.run4-trusted-entry :as sut]))

(def token (apply str (repeat 64 "a")))
(def casting {:author "codex-10" :reviewer "codex-17"
              :repair-reviewer "codex-1"})
(def mission {:id "M-run4" :status-class :active})
(def source-text "bounded task\n")
(def config-text
  (str (pr-str {:schema :wm/run4-pinned-run-config-v1
                :runner-options {:cohort? false
                                 :accumulate-strategic-habit? false}
                :c-fold {:enabled? false}}) "\n"))

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
      (spit (io/file root "pin.edn") (pr-str (pin {})))
      (f {:root root
          :config {:run4 {:enabled? true :bearer-token token :operator "Joe"
                          :casting casting
                          :admission-root (.getPath root)
                          :pin-root (.getPath root) :pin-allowlist #{"pin.edn"}
                          :source-root (.getPath root)
                          :source-allowlist #{"source.md" "config.edn"}
                          :resolve-mission #(when (= "M-run4" %) mission)
                          :action-admissible?
                          #(and (= mission %1)
                                (= {:type :advance-mission :target "M-run4"} %2))}}})
      (finally (delete-tree! root)))))

(def auth {"authorization" (str "Bearer " token)})
(def request {:run4-pin-ref "pin.edn" :run4-attempt-id "attempt-1"})

(deftest authenticates-validates-and-mints-one-use-digest-context
  (with-fixture
    (fn [{:keys [config]}]
      (let [result (sut/prepare config auth request)
            opts (:opts result)
            trust (:run4-trusted-boundary-fn opts)
            sha (digest/sha256 (:run4-task-pin-text opts))]
        (is (:ok result))
        (is (= casting (select-keys opts (keys casting))))
        (is (false? (:cohort? opts)))
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
        (is (= sha (:pin-sha256
                    (trust {:pin-digest sha
                            :operator-selection {:operator "Joe"}}))))
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
