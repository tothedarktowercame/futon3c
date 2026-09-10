(ns futon3c.wm.run4-series-service-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon2.aif.c-fold-config :as digest]
            [futon3c.transport.http :as http]
            [futon3c.wm.runner-service :as runner]))

(def token (apply str (repeat 64 "c")))
(def casting {:author "codex-10" :reviewer "codex-17"
              :repair-reviewer "codex-1"})
(def mission {:id "M-outer-loop-successor" :status-class :active})

(defn- delete-tree! [root]
  (doseq [f (reverse (file-seq root))] (io/delete-file f true)))

(defn- write! [root path text]
  (spit (io/file root path) text))

(defn- request [payload headers]
  {:request-method :post :uri "/api/alpha/wm/run4/series/step"
   :headers headers
   :body (java.io.ByteArrayInputStream.
          (.getBytes (json/generate-string payload) "UTF-8"))})

(def auth {"authorization" (str "Bearer " token)})

(defn- with-service [f]
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "run4-series-service"
                       (make-array java.nio.file.attribute.FileAttribute 0)))
        source-text "eligible outer-loop issue\n"
        config-text "{:mode :fixture-scoped}\n"
        pin {:schema :wm/run4-task-pin-v1
             :series-id "RUN4-eligible-issues" :trial-id :outer-loop-successor
             :series-order :as-declared
             :candidate-task-ids [:outer-loop-successor]
             :selected-task-id :outer-loop-successor
             :sources [{:path "source.md" :sha256 (digest/sha256 source-text)}]
             :casting casting
             :operator-selection {:mode :operator-selected :operator "Joe"
                                  :authority-ref "reviewed eligible-task series"}
             :config {:path "config.edn" :sha256 (digest/sha256 config-text)}
             :mapping {:mission-id "M-outer-loop-successor"
                       :action {:type :advance-mission
                                :target "M-outer-loop-successor"}}}
        pin-text (pr-str pin)
        manifest {:schema :wm/run4-series-pin-v1
                  :series-id "RUN4-eligible-issues" :status :frozen
                  :order :ordinal
                  :stop-rule :attempt-each-once-even-after-fail-or-block
                  :casting casting
                  :source-pins [{:path "source.md"
                                 :sha256 (digest/sha256 source-text)}]
                  :trials [{:ordinal 1 :trial-id :outer-loop-successor
                            :attempt-id "eligible-attempt-1"
                            :pin-sha256 (digest/sha256 pin-text)
                            :packet {:path "pin.edn"
                                     :sha256 (digest/sha256 pin-text)}}]}
        controller-root (io/file root "controller")
        admission-root controller-root
        binding-root (io/file root "bindings")
        projection-root (io/file root "projections")
        run-record-root (io/file root "run-records")
        cfg {:run4 {:enabled? true :bearer-token token :operator "Joe"
                    :casting casting
                    :admission-root (.getPath admission-root)
                    :pin-root (.getPath root) :pin-allowlist #{"pin.edn"}
                    :source-root (.getPath root)
                    :source-allowlist #{"source.md" "config.edn"}
                    :resolve-mission #(when (= "M-outer-loop-successor" %) mission)
                    :action-admissible? (fn [m action]
                                          (and (= mission m)
                                               (= {:type :advance-mission
                                                   :target "M-outer-loop-successor"}
                                                  action)))
                    :series {:enabled? true
                             :manifest-root (.getPath root)
                             :manifest-ref "series.edn"
                             :manifest-allowlist #{"series.edn"}
                             :controller-root (.getPath controller-root)
                             :binding-root (.getPath binding-root)
                             :projection-root (.getPath projection-root)
                             :run-record-root (.getPath run-record-root)}}}]
    (try
      (doseq [dir [controller-root binding-root projection-root
                   run-record-root]]
        (.mkdir dir))
      (write! root "source.md" source-text)
      (write! root "config.edn" config-text)
      (write! root "pin.edn" pin-text)
      (write! root "series.edn" (pr-str manifest))
      (f root cfg)
      (finally (delete-tree! root)))))

(deftest serving-route-starts-one-explicit-transition-and-resumes-idempotently
  (with-service
    (fn [root cfg]
      (let [handler (http/make-handler cfg)
            clicks (atom [])
            payload {:run4-series-ref "series.edn"}]
        (with-redefs [runner/click! (fn [opts]
                                      (swap! clicks conj opts)
                                      {:click-id "click-series-1"
                                       :started-at "2026-09-10T12:00:00Z"})]
          (let [started (handler (request payload auth))
                waiting (handler (request payload auth))]
            (is (= 200 (:status started)))
            (is (= "trial-started"
                   (:status (json/parse-string (:body started) true))))
            (is (= 200 (:status waiting)))
            (is (= "awaiting-terminal-evidence"
                   (:status (json/parse-string (:body waiting) true))))
            (is (= 1 (count @clicks)))
            (is (= casting (select-keys (first @clicks) (keys casting))))
            (is (.isFile (io/file root "controller" "001-started.edn")))
            (is (not (.exists (io/file root "controller" "001-terminal.edn"))))))))))

(deftest disabled-auth-shape-and-source-drift-refuse-before-click
  (with-service
    (fn [root cfg]
      (let [clicks (atom 0)
            payload {:run4-series-ref "series.edn"}]
        (with-redefs [runner/click! (fn [_] (swap! clicks inc))]
          (is (= 403 (:status ((http/make-handler (assoc-in cfg [:run4 :series :enabled?]
                                                            false))
                              (request payload auth)))))
          (is (= 403 (:status ((http/make-handler cfg) (request payload {})))))
          (is (= 403 (:status ((http/make-handler cfg)
                              (request (assoc payload :ports {}) auth)))))
          (write! root "source.md" "drifted\n")
          (is (= 500 (:status ((http/make-handler cfg) (request payload auth)))))
          (is (zero? @clicks))
          (is (not (.exists (io/file root "controller" "eligible-attempt-1")))))))))

(deftest declared-corrupt-terminal-chain-stops-resume-without-redispatch
  (with-service
    (fn [root cfg]
      (let [handler (http/make-handler cfg)
            payload {:run4-series-ref "series.edn"}
            clicks (atom 0)]
        (with-redefs [runner/click! (fn [_]
                                      (swap! clicks inc)
                                      {:click-id "click-corrupt-chain"
                                       :started-at "2026-09-10T12:00:00Z"})]
          (is (= 200 (:status (handler (request payload auth)))))
          (write! (io/file root "bindings")
                  "click-run-binding-click-corrupt-chain.edn" "nil\n")
          (let [response (handler (request payload auth))
                body (json/parse-string (:body response) true)]
            (is (= 500 (:status response)))
            (is (= "run4-terminal-evidence-refused" (:error body)))
            (is (= 1 @clicks))
            (is (not (.exists (io/file root "controller" "001-terminal.edn"))))))))))
