(ns futon3c.wm.run4-infrastructure-reconciliation-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
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
        started (put! series "001-started.edn" {:series-id "series" :trial-id "trial"
                                                 :attempt-id "controller-1" :click-id "click-1"
                                                 :pin-sha256 (:pin-sha256 identity)
                                                 :manifest-sha256 (:manifest-sha256 identity)})
        reservation (put! series "controller-1/reservation.edn"
                          {:attempt-id "controller-1"
                           :identity {:series-id "series" :trial-id "trial"
                                      :pin-sha256 (:pin-sha256 identity)}})
        click (put! series "controller-1/click-result.edn"
                    {:click {:click-id "click-1"}})
        binding (put! bindings "binding.edn" {:click/id "click-1" :attempt/id "initialization-1"
                                               :outcome :incomplete :binding-status :unavailable
                                               :run-record-status :absent})
        repair (put! repairs "repair.edn" {:repair/id "repair-initialization-1"
                                            :attempt-id "initialization-1"
                                            :failure-kind :initialization-failed})
        checkpoints
        (mapv (fn [n cp]
                (put! cohort (str n ".edn")
                      {:event/sequence n :checkpoint/type cp :cohort/id :cohort-a
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
    (is (= record (subject/publish! (.getPath out) record)))
    (is (= record (subject/publish! (.getPath out) record)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (subject/publish! (.getPath out)
                                   (assoc record :task-verdict :succeeded))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (subject/construct! roots (assoc identity :click-id "foreign") paths)))
    (let [external (put! root "external.edn" {:attempt-id "controller-1"})]
      (is (thrown? clojure.lang.ExceptionInfo
                   (subject/construct! roots identity (assoc paths :reservation external)))))
    (spit (last checkpoints) "nil\n")
    (is (thrown? clojure.lang.ExceptionInfo
                 (subject/construct! roots identity paths)))))
