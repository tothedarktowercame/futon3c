(ns futon3c.wm.run4-series-controller-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon2.aif.c-fold-config :as digest]
            [futon3c.wm.run4-attempt-admission :as admission]
            [futon3c.wm.run4-series-controller :as sut]))

(def casting {:author "zai-2" :reviewer "codex-17" :repair-reviewer "codex-1"})
(def trial-ids [:memory-assisted-mathematics :caption-review-and-admission
                :feedback-obligation-prototype :outer-loop-aif-replacement])
(def files (into {} (map-indexed (fn [i id] [(str (name id) ".md") (str "packet-" i "\n")]) trial-ids)))

(def manifest
  {:schema :wm/run4-series-pin-v1 :series-id "RUN4-2026-09-10"
   :status :frozen :order :ordinal
   :stop-rule :attempt-each-once-even-after-fail-or-block
   :casting casting
   :source-pins [{:path "series-source.edn" :sha256 (digest/sha256 "series\n")}]
   :trials (mapv (fn [ordinal id]
                   (let [path (str (name id) ".md")]
                     {:ordinal ordinal :trial-id id
                      :attempt-id (str "attempt-" ordinal)
                      :pin-sha256 (digest/sha256 (str "pin-" ordinal))
                      :packet {:path path :sha256 (digest/sha256 (files path))}}))
                 (range 1 5) trial-ids)})

(def manifest-text (pr-str manifest))

(defn delete-tree! [root]
  (doseq [f (reverse (file-seq root))] (io/delete-file f true)))

(defn with-controller [f]
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "run4-series" (make-array java.nio.file.attribute.FileAttribute 0)))
        all-files (assoc files "series-source.edn" "series\n")
        clicks (atom [])
        terminals (atom {})
        prepared (fn [{:keys [trial-id attempt-id pin-sha256]}]
                   {:ok true :opts {:trial-id trial-id}
                    :admission-request
                    {:attempt-id attempt-id
                     :identity {:series-id (:series-id manifest) :trial-id trial-id
                                :pin-sha256 pin-sha256 :casting casting}}})
        ports {:read-text #(or (all-files %) (throw (ex-info "missing" {})))
               :prepare-trial prepared
               :click! (fn [opts]
                         (swap! clicks conj opts)
                         {:click-id (str "click-" (count @clicks))
                          :started-at "2026-09-10T00:00:00Z"})
               :terminal-evidence #(get @terminals (:ordinal %))}]
    (try (f (.getPath root) ports clicks terminals)
         (finally (delete-tree! root)))))

(deftest ordered-four-trial-series-advances-only-on-explicit-terminal-evidence
  (with-controller
    (fn [root ports clicks terminals]
      (is (= {:status :trial-started :ordinal 1 :click-id "click-1"}
             (sut/step! root manifest-text ports)))
      (is (= :awaiting-terminal-evidence (:status (sut/step! root manifest-text ports))))
      (is (= 1 (count @clicks)))
      (doseq [[ordinal result] [[1 :failed] [2 :blocked] [3 :succeeded] [4 :succeeded]]]
        (swap! terminals assoc ordinal
               {:task-result result :infrastructure :safe
                :evidence-id (str "evidence-" ordinal)})
        (is (= :trial-terminal (:status (sut/step! root manifest-text ports))))
        (when (< ordinal 4)
          (is (= (inc ordinal) (:ordinal (sut/step! root manifest-text ports))))))
      (is (= :series-terminal (:status (sut/step! root manifest-text ports))))
      (is (= trial-ids (mapv :trial-id @clicks))))))

(deftest unsafe-infrastructure-stops-and-marks-remainder-not-attempted
  (with-controller
    (fn [root ports clicks terminals]
      (sut/step! root manifest-text ports)
      (swap! terminals assoc 1 {:task-result :blocked :infrastructure :unsafe
                                :evidence-id "infra-stop"})
      (is (= :infrastructure-stopped (:status (sut/step! root manifest-text ports))))
      (is (= 1 (count @clicks)))
      (is (= :infrastructure-stopped (:status (sut/step! root manifest-text ports))))
      (doseq [ordinal (range 2 5)]
        (is (= :not-attempted
               (:task-result (read-string
                              (slurp (io/file root (format "%03d-terminal.edn" ordinal)))))))))))

(deftest source-drift-and-prepared-identity-mismatch-refuse-before-click
  (with-controller
    (fn [root ports clicks _]
      (is (= :source-drift
             (:reason (try (sut/step! root manifest-text
                                      (assoc ports :read-text (constantly "drift"))) nil
                           (catch clojure.lang.ExceptionInfo e (ex-data e))))))
      (is (= :prepared-trial-identity-mismatch
             (:reason (try (sut/step! root manifest-text
                                      (assoc ports :prepare-trial
                                             (fn [trial]
                                               (assoc-in ((:prepare-trial ports) trial)
                                                         [:admission-request :identity :trial-id]
                                                         :wrong)))) nil
                           (catch clojure.lang.ExceptionInfo e (ex-data e))))))
      (is (empty? @clicks)))))

(deftest preparation-manifest-and-indeterminate-reservation-never-launch
  (with-controller
    (fn [root ports clicks _]
      (is (= :production-manifest-preparation-only
             (:reason (try (sut/step! root
                                      (pr-str {:schema :wm/run4-series-preparation-v1})
                                      ports) nil
                           (catch clojure.lang.ExceptionInfo e (ex-data e))))))
      (let [trial (first (:trials manifest))
            request (:admission-request ((:prepare-trial ports) trial))]
        (is (:new? (admission/reserve! root request)))
        (is (= :reconciliation-required
               (:status (sut/step! root manifest-text ports))))
        (is (empty? @clicks))))))

(deftest durable-click-replay-reconstructs-start-without-redispatch
  (with-controller
    (fn [root ports clicks _]
      (is (= :trial-started (:status (sut/step! root manifest-text ports))))
      (io/delete-file (io/file root "001-started.edn"))
      (is (= :trial-started (:status (sut/step! root manifest-text ports))))
      (is (= 1 (count @clicks))))))

(deftest corrupt-or-wrong-identity-series-event-never-redispatches
  (with-controller
    (fn [root ports clicks _]
      (spit (io/file root "001-started.edn") "nil")
      (is (= :corrupt-series-event
             (:reason (try (sut/step! root manifest-text ports) nil
                           (catch clojure.lang.ExceptionInfo e (ex-data e))))))
      (io/delete-file (io/file root "001-started.edn"))
      (spit (io/file root "001-started.edn")
            (pr-str {:schema :wm/run4-series-started-v1 :ordinal 1
                     :trial-id :wrong :attempt-id "attempt-1"}))
      (is (= :persisted-event-identity-mismatch
             (:reason (try (sut/step! root manifest-text ports) nil
                           (catch clojure.lang.ExceptionInfo e (ex-data e))))))
      (is (empty? @clicks)))))

(deftest unsafe-terminal-recovers-partial-remainder-marking-without-dispatch
  (with-controller
    (fn [root ports clicks terminals]
      (sut/step! root manifest-text ports)
      (swap! terminals assoc 1 {:task-result :blocked :infrastructure :unsafe
                                :evidence-id "unsafe-one"})
      (let [write! sut/*atomic-write!*
            failed? (atom false)]
        (is (thrown? clojure.lang.ExceptionInfo
                     (binding [sut/*atomic-write!*
                               (fn [file value]
                                 (if (and (not @failed?)
                                          (= "002-terminal.edn" (.getName file)))
                                   (do (reset! failed? true)
                                       (throw (ex-info "injected write failure" {})))
                                   (write! file value)))]
                       (sut/step! root manifest-text ports)))))
      (is (= :infrastructure-stopped
             (:status (sut/step! root manifest-text ports))))
      (is (= 1 (count @clicks)))
      (doseq [ordinal (range 2 5)]
        (is (= :not-attempted
               (:task-result (read-string
                              (slurp (io/file root (format "%03d-terminal.edn" ordinal)))))))))))

(deftest incomplete-terminal-and-overwriting-evidence-refuse-before-dispatch
  (with-controller
    (fn [root ports clicks terminals]
      (spit (io/file root "001-terminal.edn")
            (pr-str {:schema :wm/run4-series-terminal-v1 :ordinal 1
                     :trial-id (first trial-ids) :attempt-id "attempt-1"}))
      (is (= :invalid-persisted-terminal
             (:reason (try (sut/step! root manifest-text ports) nil
                           (catch clojure.lang.ExceptionInfo e (ex-data e))))))
      (io/delete-file (io/file root "001-terminal.edn"))
      (sut/step! root manifest-text ports)
      (swap! terminals assoc 1 {:task-result :succeeded :infrastructure :safe
                                :evidence-id "one" :trial-id :forged})
      (is (= :invalid-terminal-evidence
             (:reason (try (sut/step! root manifest-text ports) nil
                           (catch clojure.lang.ExceptionInfo e (ex-data e))))))
      (is (= 1 (count @clicks))))))

(deftest full-looking-terminal-records-require-permitted-state-and-lifecycle
  (with-controller
    (fn [root ports clicks _]
      (let [trial (first (:trials manifest))
            base {:schema :wm/run4-series-terminal-v1
                  :series-id (:series-id manifest)
                  :manifest-sha256 (digest/sha256 manifest-text)
                  :ordinal 1 :trial-id (:trial-id trial)
                  :attempt-id (:attempt-id trial) :pin-sha256 (:pin-sha256 trial)}
            terminal-file (io/file root "001-terminal.edn")]
        (spit terminal-file
              (pr-str (merge base {:task-result :not-attempted
                                   :infrastructure :safe :reason :invented})))
        (is (= :invalid-persisted-terminal
               (:reason (try (sut/step! root manifest-text ports) nil
                             (catch clojure.lang.ExceptionInfo e (ex-data e))))))
        (io/delete-file terminal-file)
        (spit terminal-file
              (pr-str (merge base {:task-result :succeeded :infrastructure :safe
                                   :evidence-id "unbound"})))
        (is (= :terminal-without-start
               (:reason (try (sut/step! root manifest-text ports) nil
                             (catch clojure.lang.ExceptionInfo e (ex-data e))))))
        (is (empty? @clicks))))))

(deftest persisted-start-must-join-the-exact-durable-admission-click
  (with-controller
    (fn [root ports clicks _]
      (sut/step! root manifest-text ports)
      (let [started-file (io/file root "001-started.edn")
            started (read-string (slurp started-file))]
        (spit started-file (pr-str (assoc started :click-id "forged-click")))
        (is (= :started-admission-mismatch
               (:reason (try (sut/step! root manifest-text ports) nil
                             (catch clojure.lang.ExceptionInfo e (ex-data e))))))
        (is (= 1 (count @clicks)))))))

(deftest busy-admission-is-infrastructure-stop-not-success
  (with-controller
    (fn [root ports clicks _]
      (let [busy-ports (assoc ports :click!
                              (fn [opts] (swap! clicks conj opts)
                                {:rejected :already-running
                                 :click-id "unrelated-click"}))]
        (is (= :busy-admission-rejected
               (:reason (sut/step! root manifest-text busy-ports))))
        (is (= 1 (count @clicks)))
        (is (= :not-attempted
               (:task-result (read-string
                              (slurp (io/file root "001-terminal.edn"))))))))))
