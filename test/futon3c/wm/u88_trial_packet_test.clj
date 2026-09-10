(ns futon3c.wm.u88-trial-packet-test
  "Fixture-scoped U88 packet validation; C-fold references resolve once.
   No live-valid pin, dispatch, credentials or activation is claimed."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon2.aif.c-fold-config :as c-fold]
            [futon2.aif.mission-registry :as missions]
            [futon2.aif.run4-task-pin :as task-pin]
            [futon3c.wm.run4-pinned-run-config :as pinned-config]
            [futon3c.wm.run4-series-controller :as series])
  (:import (java.io File)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(def f2 "../futon2")
(def packet-dir
  (str f2 "/holes/labs/wm-contract/runs/RUN4-preparation-2026-09-10/u88-trial-1"))
(def run-config-path (str packet-dir "/run-config.edn"))
(def task-pin-path (str packet-dir "/task-pin.edn"))
(def series-pin-path (str packet-dir "/series-pin.edn"))
(def mission-path
  (str f2 "/holes/labs/wm-contract/runs/RUN4-preparation-2026-09-10/draft-missions/M-u88-contextual-preferences.md"))
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

(defn- with-temp-code-root [f]
  (let [root (Files/createTempDirectory "u88-f3c-packet"
                                        (into-array FileAttribute []))]
    (try
      (f (str root))
      (finally
        (doseq [^File child (reverse (file-seq (io/file (str root))))]
          (.delete child))))))

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

(deftest frozen-series-passes-real-preflight-with-open-copy-identity
  (with-temp-code-root
    (fn [root]
      (let [target (io/file root "futon2/holes/missions"
                            (str mission-id ".md"))]
        (io/make-parents target)
        (spit target (str/replace
                      (read-text mission-path)
                      "DRAFT — NON-LIVE; independent review and explicit activation required"
                      "OPEN — fixture episode milestone pending"))
        (let [open (into {} (map (juxt :id identity)
                                 (missions/open-missions
                                  {:missions (:missions (missions/load-missions root))})))
              pin-ports {:read-text read-text
                         :resolve-mission (fn [id] (get open id))
                         ;; DELIBERATE FIXTURE STUB, not the serving rule.
                         :action-admissible? (fn [mission action]
                                               (and (= :open (:status-class mission))
                                                    (= :advance-mission (:type action))))}
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
          (is (= :wm/run4-series-pin-v1
                 (:schema (:manifest preflighted))))
          (is (= "fd203522990a819fe95e04fb7f81fcae98f5f7c4a18d1b1365188ac8a68d13d4"
                 (get-in (first (:prepared preflighted))
                         [:admission-request :identity :pin-sha256])))
          (is (every? :ok (:prepared preflighted))))))))
