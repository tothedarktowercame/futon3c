(ns futon3c.wm.run4-pinned-run-config-test
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.c-fold-config :as digest]
            [futon2.aif.efe :as efe]
            [futon2.report.war-machine :as wm]
            [futon3c.wm.run4-pinned-run-config :as sut]))

(def state {:belief {:x 0.5} :observation {:mission-health 0.5}})
(def action {:type :no-op})

(defn- load-sheet [sheet]
  (let [text (str (pr-str sheet) "\n")]
    (sut/load! {:path "pins/run-config.edn" :sha256 (digest/sha256 text)}
               #(if (= % "pins/run-config.edn") text
                    (throw (ex-info "not authorized" {}))))))

(deftest explicit-false-reaches-house-scorer-without-coercing-absence
  (let [opts (load-sheet
              {:schema :wm/run4-pinned-run-config-v1
               :runner-options {:accumulate-strategic-habit? false}
               :c-fold {:enabled? false}})
        scored-opts (#'wm/configured-fold-efe-opts {} opts)
        legacy (efe/compute-efe state action {})
        scored (efe/compute-efe state action scored-opts)]
    (is (not (contains? opts :cohort?)))
    (is (false? (:accumulate-strategic-habit? opts)))
    (is (false? (:ruled-outcome-c-enabled? opts)))
    (is (not (contains? opts :beta-habit-in-both?)))
    (is (= (pr-str legacy) (pr-str scored)))
    (is (= {:path "pins/run-config.edn"
            :sha256 (get-in opts [:run4/config-pin :sha256])}
           (:run4/config-pin opts)))))

(deftest unknown-keys-bad-types-and-source-drift-refuse
  (doseq [sheet [{:schema :wm/run4-pinned-run-config-v1
                  :runner-options {:dispatch-fn identity}
                  :c-fold {:enabled? false}}
                 {:schema :wm/run4-pinned-run-config-v1
                  :runner-options {:run-record-dir "/tmp/client-selected"}
                  :c-fold {:enabled? false}}
                 {:schema :wm/run4-pinned-run-config-v1
                  :runner-options {:cohort? nil}
                  :c-fold {:enabled? false}}
                 {:schema :wm/run4-pinned-run-config-v1
                  :runner-options {}
                  :c-fold {:enabled? false :seed {}}}]]
    (is (thrown? clojure.lang.ExceptionInfo (load-sheet sheet))))
  (is (= :config-source-drift
         (:reason
          (try
            (sut/load! {:path "config.edn" :sha256 (apply str (repeat 64 "0"))}
                       (constantly "{}"))
            nil
            (catch clojure.lang.ExceptionInfo e (ex-data e)))))))

(def serving-declaration
  {:required-environment {"FUTON_WM_FPI_DARK" "1"
                          "FUTON_WM_BETA_DARK" "1"
                          "FUTON_WM_TRACE_POLICY_DETAILS" "1"}
   :hierarchy {:model :single-level :scope :RUN4}
   :recording-requirement
   {:contract :wm/realized-recording-v1
    :environment {"FUTON_WM_RECORDING_CONTRACT" "1"}}})

(deftest serving-declaration-is-strict-data-not-attestation
  (let [opts (load-sheet {:schema :wm/run4-pinned-run-config-v1
                          :runner-options {}
                          :c-fold {:enabled? false}
                          :serving-declaration serving-declaration})]
    (is (= serving-declaration (:run4/serving-declaration opts)))
    (is (nil? (:run4/effective-environment-attestation opts))))
  (doseq [bad [(assoc serving-declaration :required-environment "1")
               (assoc serving-declaration :required-environment [true])
               (dissoc serving-declaration :hierarchy)
               (assoc-in serving-declaration
                         [:required-environment "FUTON_WM_FPI_DARK"] "0")
               (assoc serving-declaration :hierarchy
                      {:model :hierarchical :scope :RUN4})]]
    (is (thrown? clojure.lang.ExceptionInfo
                 (load-sheet {:schema :wm/run4-pinned-run-config-v1
                              :runner-options {}
                              :c-fold {:enabled? false}
                              :serving-declaration bad})))))
