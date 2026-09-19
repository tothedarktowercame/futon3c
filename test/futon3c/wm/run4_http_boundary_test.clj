(ns futon3c.wm.run4-http-boundary-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon2.aif.c-fold-config :as digest]
            [futon3c.transport.http :as http]
            [futon3c.wm.run4-attempt-admission :as admission]
            [futon3c.wm.run4-effective-environment :as effective]
            [futon3c.wm.run4-trusted-entry :as entry]
            [futon3c.wm.runner-service :as service]))

(def token (apply str (repeat 64 "b")))
(def casting {:author "codex-10" :reviewer "codex-17"
              :repair-reviewer "codex-1"})
(def mission {:id "M-run4" :status-class :active})

(defn delete-tree! [root]
  (doseq [f (reverse (file-seq root))] (io/delete-file f true)))

(defn with-handler [f]
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "run4-http" (make-array java.nio.file.attribute.FileAttribute 0)))
        source "task\n"
        config-text (str (pr-str {:schema :wm/run4-pinned-run-config-v1
                                  :runner-options {}
                                  :c-fold {:enabled? false}
                                  :serving-declaration
                                  {:required-environment
                                   {"FUTON_WM_FPI_DARK" "1"
                                    "FUTON_WM_BETA_DARK" "1"
                                    "FUTON_WM_TRACE_POLICY_DETAILS" "1"}
                                   :hierarchy {:model :single-level :scope :RUN4}
                                   :recording-requirement
                                   {:contract :wm/realized-recording-v1
                                    :environment {"FUTON_WM_RECORDING_CONTRACT" "1"}}}}) "\n")
        pin {:schema :wm/run4-task-pin-v1
             :series-id "RUN4-2026-09-10" :trial-id :outer-loop-successor
             :series-order :as-declared
             :candidate-task-ids [:outer-loop-successor :math-probe
                                  :caption-probe :feedback-monitor]
             :selected-task-id :outer-loop-successor
             :sources [{:path "source.md" :sha256 (digest/sha256 source)}]
             :casting casting
             :operator-selection {:mode :operator-selected :operator "Joe"
                                  :authority-ref "SERIES.edn selection"}
             :config {:path "config.edn" :sha256 (digest/sha256 config-text)}
             :mapping {:mission-id "M-run4"
                       :action {:type :advance-mission :target "M-run4"}}}
        cfg {:run4 {:enabled? true :bearer-token token :operator "Joe"
                    :casting casting
                    :admission-root (.getPath root)
                    :pin-root (.getPath root) :pin-allowlist #{"pin.edn"}
                    :source-root (.getPath root)
                    :source-allowlist #{"source.md" "config.edn"}
                    :resolve-mission #(when (= "M-run4" %) mission)
                    :action-admissible? (fn [m a]
                                          (and (= mission m)
                                               (= :advance-mission (:type a))))}}]
    (try
      (spit (io/file root "source.md") source)
      (spit (io/file root "config.edn") config-text)
      (spit (io/file root "pin.edn") (pr-str pin))
      (f (http/make-handler cfg) cfg root pin)
      (finally (delete-tree! root)))))

;; Production reads the live process environment through
;; run4-effective-environment/attest; the boundary under test is the HTTP
;; contract, not the environment, so the attest seam is stubbed exactly as
;; run4_trusted_entry_test does.
(defn- with-attest-stub [f]
  (binding [entry/*attest-effective-environment*
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
    (f)))

(defn request [payload headers]
  {:request-method :post :uri "/api/alpha/wm/click"
   :headers headers
   :body (java.io.ByteArrayInputStream.
          (.getBytes (json/generate-string payload) "UTF-8"))})

(def auth {"authorization" (str "Bearer " token)})

(deftest run4-refusals-occur-before-click-creation
  (with-attest-stub
   (fn [] (with-handler
     (fn [handler cfg root pin]
      (let [calls (atom [])
            base {:run4-pin-ref "pin.edn" :run4-attempt-id "attempt-http"}
            cases [[base {}]
                   [(assoc base :authenticated true) auth]
                   [(assoc base :author "Mallory") auth]
                   [(assoc base :run4-pin-ref "../pin.edn") auth]]]
        (with-redefs [service/click! #(swap! calls conj %)]
          (doseq [[payload headers] cases]
            (is (= 403 (:status (handler (request payload headers))))))
          (is (false? (.exists (io/file root "attempt-http"))))
          (spit (io/file root "pin.edn")
                (pr-str (assoc-in pin [:operator-selection :operator] "Mallory")))
          (is (= 403 (:status (handler
                               (request base auth)))))
          (spit (io/file root "pin.edn")
                (pr-str (assoc pin :sources
                               [{:path "not-authorized.md"
                                 :sha256 (digest/sha256 "missing")}])))
          (is (= 403 (:status (handler
                               (request base auth)))))
          (spit (io/file root "pin.edn") "{:pin 1}")
          (is (= 403 (:status (handler
                               (request base auth)))))
          (is (empty? @calls))
          (let [disabled (http/make-handler (dissoc cfg :run4))]
            (is (= 403 (:status (disabled
                                 (request base auth)))))
            (is (empty? @calls)))))))))
   )

(deftest valid-run4-propagates-exact-server-derived-options
  (with-attest-stub
   (fn [] (with-handler
     (fn [handler _ _ _]
      (let [seen (atom [])]
        (with-redefs [service/click! (fn [opts] (swap! seen conj opts)
                                      {:click-id "click-valid"
                                       :started-at "2026-09-10T00:00:00Z"})]
          (is (= 200 (:status (handler (request {:run4-pin-ref "pin.edn"
                                                 :run4-attempt-id "attempt-valid"}
                                                auth)))))
          (is (= 200 (:status (handler (request {:run4-pin-ref "pin.edn"
                                                 :run4-attempt-id "attempt-valid"}
                                                auth)))))
          (is (= 1 (count @seen)))
          (is (not (contains? (first @seen) :ordinary-click/issue!)))
          (is (= casting (select-keys (first @seen) (keys casting))))
          (is (string? (:run4-task-pin-text (first @seen))))
          (is (fn? (:run4-trusted-boundary-fn (first @seen))))
          (is (= #{:read-text :resolve-mission :action-admissible?}
                 (set (keys (:run4-task-pin-ports (first @seen))))))))
   )))
   ))

(deftest concurrent-duplicates-corruption-and-write-failure-never-double-click
  (with-attest-stub
   (fn [] (with-handler
     (fn [handler _ root _]
      (let [clicks (atom 0)
            payload {:run4-pin-ref "pin.edn"
                     :run4-attempt-id "attempt-concurrent"}
            start (promise)]
        (with-redefs [service/click! (fn [_] (swap! clicks inc)
                                      {:click-id "click-one"
                                       :started-at "2026-09-10T00:00:00Z"})]
          (let [requests (doall (repeatedly 10
                                            #(future @start
                                                     (handler (request payload auth)))))]
            (deliver start true)
            (is (every? #{200} (map (comp :status deref) requests)))
            (is (= 1 @clicks))))
        (let [before @clicks]
          (.mkdir (io/file root "attempt-corrupt"))
          (spit (io/file root "attempt-corrupt" "reservation.edn") "")
          (is (= 409 (:status
                      (handler (request (assoc payload :run4-attempt-id
                                              "attempt-corrupt") auth)))))
          (is (= before @clicks))
          (binding [admission/*atomic-write!*
                    (fn [& _] (throw (ex-info "disk failure" {:committed? false})))]
            (is (= 500 (:status
                        (handler (request (assoc payload :run4-attempt-id
                                                "attempt-write-failure") auth))))))
          (is (= before @clicks)))))))
   ))

(deftest legacy-and-single-flight-responses-are-preserved
  (let [handler (http/make-handler {})
        calls (atom [])]
    (with-redefs [service/click! (fn [opts]
                                   (swap! calls conj opts)
                                   {:started true})]
      (is (= 200 (:status (handler (request {:author "legacy"} {})))))
      (is (= [{:author "legacy"
                :issuer-provenance {:status :present :identity :caller-unknown
                                    :source :wm-click-http-boundary}}] (mapv #(dissoc % :ordinary-click/issue!) @calls)))
      (is (fn? (:ordinary-click/issue! (first @calls)))))
    (with-redefs [service/click! (constantly {:rejected :already-running})]
      (is (= 409 (:status (handler (request {} {}))))))))
