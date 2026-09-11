(ns futon3c.wm.run4-infrastructure-reconciliation-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon2.aif.c-fold-config :as digest]
            [futon3c.wm.run4-infrastructure-reconciliation :as subject]))

(defn- tmp [] (.toFile (java.nio.file.Files/createTempDirectory
                         "run4-reconciliation" (make-array java.nio.file.attribute.FileAttribute 0))))
(defn- put! [root name value]
  (let [f (io/file root name)] (io/make-parents f) (spit f (str (pr-str value) "\n")) (.getPath f)))

(deftest captures-positive-six-cell-prefix-and-refuses-corruption
  (let [root (tmp) series (doto (io/file root "series") .mkdir)
        cohort (doto (io/file root "cohort") .mkdir)
        bindings (doto (io/file root "bindings") .mkdir)
        repairs (doto (io/file root "repairs") .mkdir)
        out (doto (io/file root "out") .mkdir)
        identity {:series-id "series" :trial-id "trial"
                  :controller-attempt-id "controller-1" :click-id "click-1"
                  :cohort-id :cohort-a :cohort-attempt-id "attempt-001"
                  :wrapper-attempt-id "initialization-1" :repair-id "repair-initialization-1"
                  :pin-sha256 (apply str (repeat 64 "a"))
                  :manifest-sha256 (apply str (repeat 64 "b"))}
        started (put! series "001-started.edn" {:schema :wm/run4-series-started-v1
                                                 :series-id "series" :trial-id "trial"
                                                 :attempt-id "controller-1" :click-id "click-1"
                                                 :pin-sha256 (:pin-sha256 identity)
                                                 :manifest-sha256 (:manifest-sha256 identity)})
        admission-identity {:series-id "series" :trial-id "trial"
                            :pin-sha256 (:pin-sha256 identity)
                            :casting {:author "zai-2" :reviewer "codex-12"
                                      :repair-reviewer "codex-17"}}
        reservation (put! series "controller-1/reservation.edn"
                          {:schema :wm/run4-attempt-reservation-v1
                           :attempt-id "controller-1"
                           :identity admission-identity
                           :content-sha256
                           (digest/sha256
                            (pr-str ["series" "trial" (:pin-sha256 identity)
                                     (:casting admission-identity)]))})
        click (put! series "controller-1/click-result.edn"
                    {:schema :wm/run4-attempt-click-result-v1
                     :attempt-id "controller-1"
                     :click {:click-id "click-1" :started-at "2026-09-11T02:13:32Z"}})
        binding (put! bindings "binding.edn" {:schema :wm-click-run-binding-v1
                                               :click/id "click-1" :attempt/id "initialization-1"
                                               :outcome :incomplete :binding-status :unavailable
                                               :run-record-status :absent})
        repair (put! repairs "repair.edn" {:repair/schema-version 3
                                            :repair/status :open
                                            :repair/class :machine-failure
                                            :repair/id "repair-initialization-1"
                                            :attempt-id "initialization-1"
                                            :failure-stage :initialization
                                            :failure-outcome :incomplete
                                            :failure-kind :initialization-failed})
        checkpoints
        (mapv (fn [n cp]
                (put! cohort (str n ".edn")
                      {:event/schema-version 1 :attempt/ordinal 1
                       :event/sequence n :checkpoint/type cp :cohort/id :cohort-a
                       :attempt/id "attempt-001"
                       :payload (if (<= n 2) {:judgment {}}
                                    {:sorry {:outcome :agent-unavailable
                                             :kind (keyword (str "not-reached-" (name cp)))}})}))
              (range 1 7) [:time-step :selection :construction :dispatch :build :adjudication])
        roots {:series-root (.getPath series) :cohort-root (.getPath cohort)
               :binding-root (.getPath bindings) :repair-root (.getPath repairs)}
        paths {:started started :reservation reservation :click-result click
               :binding binding :repair repair :checkpoint-prefix checkpoints}
        record (subject/construct! roots identity paths)]
    (is (= [:reconciliation-required :unknown false]
           ((juxt :state :task-verdict :redispatch-permitted?) record)))
    (is (= :unknown (:cross-store-association record)))
    (is (= record (subject/publish! (.getPath out) roots identity paths)))
    (is (= record (subject/publish! (.getPath out) roots identity paths)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (subject/publish! (.getPath out) roots
                                   (assoc identity :controller-attempt-id "../escaped")
                                   paths)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (subject/construct! roots (assoc identity :click-id "foreign") paths)))
    (let [external (put! root "external.edn" {:attempt-id "controller-1"})]
      (is (thrown? clojure.lang.ExceptionInfo
                   (subject/construct! roots identity (assoc paths :reservation external)))))
    ;; A still-valid source mutation is recaptured and conflicts with the
    ;; immutable published evidence rather than being acknowledged as replay.
    (let [original (slurp repair)]
      (spit repair (str (pr-str (assoc (read-string original) :review-note "changed")) "\n"))
      (is (thrown? clojure.lang.ExceptionInfo
                   (subject/publish! (.getPath out) roots identity paths)))
      (spit repair original))
    (spit (last checkpoints) "nil\n")
    (is (thrown? clojure.lang.ExceptionInfo
                 (subject/construct! roots identity paths)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (subject/publish! (.getPath out) roots identity paths)))))
