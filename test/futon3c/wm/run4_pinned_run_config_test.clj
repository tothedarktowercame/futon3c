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
               :runner-options {:cohort? false
                                :accumulate-strategic-habit? false}
               :c-fold {:enabled? false}})
        scored-opts (#'wm/configured-fold-efe-opts {} opts)
        legacy (efe/compute-efe state action {})
        scored (efe/compute-efe state action scored-opts)]
    (is (false? (:cohort? opts)))
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
