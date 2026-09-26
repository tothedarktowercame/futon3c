(ns futon3c.wm.u88-trial-packet-test
  "Activated U88 packet validation. The production mission resolver accepts the
   exact pinned action; C-fold references resolve once. No dispatch or
   credential access occurs."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon2.aif.c-fold-config :as c-fold]
            [futon2.aif.mission-registry :as missions]
            [futon2.aif.run4-task-pin :as task-pin]
            [futon3c.wm.run4-pinned-run-config :as pinned-config]
            [futon3c.wm.run4-series-controller :as series]))


(def f2 "../futon2")
(def packet-dir
  (str f2 "/holes/labs/wm-contract/runs/RUN4-preparation-2026-09-10/u88-trial-1"))
(def run-config-path (str packet-dir "/run-config.edn"))
(def task-pin-path (str packet-dir "/task-pin.edn"))
(def series-pin-path (str packet-dir "/series-pin.edn"))
(def mission-path (str f2 "/holes/missions/M-u88-contextual-preferences.md"))
(def mission-id "M-u88-contextual-preferences")

(defn- read-text
  [path]
  ;; Fixture-scope stub: the packet's source pins are futon2-root-relative;
  ;; try the path as given, futon2-prefixed, and textually normalized forms,
  ;; without touching any tree.
  (let [candidates [path
                    (str f2 "/" path)
                    (str (.normalize (.toPath (io/file path))))
                    (str (.normalize (.toPath (io/file (str f2 "/" path)))))]
        found (some #(when (.isFile (io/file %)) %) candidates)]
    (if found (slurp found) (throw (ex-info "stub miss" {:path path})))))

(deftest ruled-flags-materialize-through-the-underlying-consumer
  ;; Single-resolution path: resolve-opts resolves seed/kernel refs relative
  ;; to the config path's parent, which is the documented contract. The ruled
  ;; flags all materialize from the actual committed bytes.
  (let [sheet (edn/read-string (read-text run-config-path))
        opts (c-fold/resolve-opts (:runner-options sheet)
                                  run-config-path read-text)]
    (is (true? (:ruled-outcome-c-enabled? opts)))
    (is (:seeded-c opts))
    (is (:disposition-kernel opts))
    (is (:c-fold-provenance opts))))

(deftest strict-loader-enabled-c-fold-resolves-once
  (let [opts (pinned-config/load!
               {:path run-config-path
                :sha256 (c-fold/sha256 (read-text run-config-path))}
               read-text)]
    (is (true? (:ruled-outcome-c-enabled? opts)))
    (is (:seeded-c opts))
    (is (fn? (:disposition-kernel opts)))
    (is (:c-fold-provenance opts))))

(deftest pinned-config-refuses-drift
  (let [drift (fn [path]
                (if (= path run-config-path)
                  (str (read-text path) " ")
                  (read-text path)))
        reason (try (pinned-config/load!
                     {:path run-config-path
                      :sha256 (c-fold/sha256 (read-text run-config-path))}
                     drift)
                    nil
                    (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))]
    (is (= :config-source-drift reason))))

(deftest frozen-series-passes-real-preflight-with-production-open-identity
  (let [open (into {} (map (juxt :id identity) (missions/open-missions)))
        mission (get open mission-id)
        pin-ports {:read-text read-text
                   :resolve-mission (fn [id] (get open id))
                   :action-admissible? (fn [m action]
                                         (and (= mission-id (:id m))
                                              (= {:type :advance-mission
                                                  :target mission-id}
                                                 action)))}
        prepare-trial (fn [trial]
                        (let [value (task-pin/validate
                                     (read-text (get-in trial [:packet :path]))
                                     pin-ports)]
                          {:ok (:valid? value)
                           :admission-request
                           {:identity {:series-id "run4-inner-loop-u88-2026-09-10"
                                       :trial-id (:trial-id trial)
                                       :pin-sha256 (:pin-sha256 trial)
                                       :casting {:author "zai-2"
                                                 :reviewer "codex-12"
                                                 :repair-reviewer "codex-17"}}
                            :attempt-id (:attempt-id trial)}}))
        preflighted (series/preflight (read-text series-pin-path)
                                      {:read-text read-text
                                       :prepare-trial prepare-trial})]
    (is (= :open (:status-class mission)))
    (is (= :wm/run4-series-pin-v1 (:schema (:manifest preflighted))))
    (is (= "9cf34ffcff3a78bbe2b60e5c8cbfa674887ff3c34de5a41c49ea57315b63b717"
           (get-in (first (:prepared preflighted))
                   [:admission-request :identity :pin-sha256])))
    (is (every? :ok (:prepared preflighted)))))
