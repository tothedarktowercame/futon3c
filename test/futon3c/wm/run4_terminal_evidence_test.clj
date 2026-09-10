(ns futon3c.wm.run4-terminal-evidence-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon2.aif.c-fold-config :as digest]
            [futon3c.wm.run4-terminal-evidence :as sut]))

(def pin {:sha256 (apply str (repeat 64 "a")) :series-id "RUN4"
          :trial-id :outer-loop :mission-id "M-run4" :action {:type :mission}})
(def casting {:author "zai-2" :reviewer "codex-17" :repair-reviewer "codex-1"})
(def request {:attempt-id "outer-attempt"
              :identity {:series-id "RUN4" :trial-id :outer-loop
                         :pin-sha256 (:sha256 pin) :casting casting}})
(def started {:ordinal 1 :click-id "click-1" :started-at "2026-09-10T00:00:00Z"})

(defn- write! [file value] (io/make-parents file) (spit file (str (pr-str value) "\n")))
(defn- delete-tree! [root]
  (doseq [f (reverse (file-seq root))] (io/delete-file f true)))

(defn- grounded-projection [run-path]
  {:schema :wm-run4-terminal-projection-v1 :click/id "click-1" :run/id "run-1"
   :attempt/id "internal-1" :run4/task-pin pin :outcome :grounded-change
   :checkpoints
   {:selection {:status :present :judgment {}
                :ground {:kind :wm-judgement :run4/task-pin pin}}
    :construction {:status :present :judgment {:run4/task-pin pin}
                   :ground {:kind :decision-pinned-construction :run4/task-pin pin}}
    :dispatch {:status :present :judgment {} :ground {:kind :agency-dispatch}}
    :build {:status :present
            :judgment {:commits ["abc"]
                       :validation {:approved? true :review-job "review-1"
                                    :review-gate {:required? true :executed? true
                                                  :tool-events 2 :passed? true}}}
            :ground {:kind :git-commit-and-independent-review}}
    :adjudication {:status :present
                   :judgment {:build-match {:commit "abc" :review-approved? true}
                              :dial {:moved? true :implementation-id "impl-1"}}
                   :ground {:kind :authoritative-substrate-discharge}}}
   :failure {:kind nil :stage nil}
   :evidence {:commit "abc" :author-job-id "author-1" :reviewer-job-id "review-1"
              :grounding-witness {:resolved? true :dial-moved? true
                                  :implementation-id "impl-1"}}
   :source {:run-record run-path :run-record-sha256 nil}})

(defn- fixture [f]
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "run4-terminal-reader"
                       (make-array java.nio.file.attribute.FileAttribute 0)))
        roots (into {} (for [k [:admission :bindings :projections :run-records]]
                         [k (.getPath (doto (io/file root (name k)) .mkdirs))]))
        reservation {:schema :wm/run4-attempt-reservation-v1
                     :attempt-id (:attempt-id request) :identity (:identity request)
                     :content-sha256
                     (digest/sha256 (pr-str ["RUN4" :outer-loop (:sha256 pin) casting]))}
        click {:schema :wm/run4-attempt-click-result-v1 :attempt-id "outer-attempt"
               :click (select-keys started [:click-id :started-at])}
        run-file (io/file (:run-records roots) "run.edn")
        run-record {:run/id "run-1" :click/id "click-1" :startedAt (:started-at started)
                    :selectorSeam "live:validated-selection" :traceWritten true
                    :route [{:fromNode "R20" :toNode "R12" :via "observe" :at_ (:started-at started)}]
                    :run4/task-pin pin}
        _ (write! (io/file (:admission roots) "outer-attempt/reservation.edn") reservation)
        _ (write! (io/file (:admission roots) "outer-attempt/click-result.edn") click)
        _ (write! run-file run-record)
        run-text (slurp run-file)
        projection (assoc-in (grounded-projection (.getCanonicalPath run-file))
                             [:source :run-record-sha256] (digest/sha256 run-text))
        projection-file (io/file (:projections roots) "run4-terminal-projection-click-1.edn")
        _ (write! projection-file projection)
        projection-ref {:path (.getAbsolutePath projection-file)
                        :sha256 (digest/sha256 (pr-str projection))
                        :source-sha256 (digest/sha256 run-text)}
        binding {:schema :wm-click-run-binding-v1 :click/id "click-1"
                 :attempt/id "internal-1" :outcome :grounded-change
                 :binding-status :verified
                 :run-id-observation {:status :present :source :runner-return :value "run-1"}
                 :run-record-status :present :recorded-at "2026-09-10T00:00:01Z"
                 :run-record (.getCanonicalPath run-file)
                 :run4/terminal-projection projection-ref}
        binding-file (io/file (:bindings roots) "click-run-binding-click-1.edn")]
    (write! binding-file binding)
    (try (f {:roots roots :projection-file projection-file :binding-file binding-file
             :run-file run-file :projection projection :binding binding})
         (finally (delete-tree! root)))))

(deftest strict-valid-chain-classifies-grounded-success
  (fixture (fn [{:keys [roots projection]}]
             (let [value (sut/read-terminal-evidence roots request started)]
               (is (= [:succeeded :safe]
                      ((juxt :task-result :infrastructure) value)))
               (is (= (digest/sha256 (pr-str projection))
                      (:evidence-id value)))))))

(deftest read-only-port-is-the-series-controller-boundary
  (fixture
   (fn [{:keys [roots]}]
     (let [port (sut/terminal-evidence-port
                 roots {1 {:admission-request request}})]
       (is (= :succeeded (:task-result (port started))))
       (is (= :missing-prepared-trial
              (:reason (try (port (assoc started :ordinal 2)) nil
                            (catch clojure.lang.ExceptionInfo e (ex-data e))))))))))

(deftest supported-safe-build-failure-and-unsafe-infrastructure
  (fixture
   (fn [{:keys [roots projection-file binding-file projection binding]}]
     (letfn [(install! [p]
               (write! projection-file p)
               (write! binding-file
                       (assoc binding :outcome (:outcome p)
                              :run4/terminal-projection
                              (assoc (:run4/terminal-projection binding)
                                     :sha256 (digest/sha256 (pr-str p))))))]
       (let [failed (-> projection
                        (assoc :outcome :build-failed)
                        (assoc :failure {:kind :build-failed :stage :reviewer-wait})
                        (assoc-in [:checkpoints :build :judgment :validation :approved?] false)
                        (assoc-in [:checkpoints :build :judgment :validation
                                   :review-gate :passed?] false))]
         (install! failed)
         (is (= [:failed :safe]
                ((juxt :task-result :infrastructure)
                 (sut/read-terminal-evidence roots request started)))))
       (let [unsafe (-> projection
                        (assoc :outcome :incomplete)
                        (assoc :failure {:kind :transport-timeout :stage :dispatch}))]
         (install! unsafe)
         (is (= [:blocked :unsafe]
                ((juxt :task-result :infrastructure)
                 (sut/read-terminal-evidence roots request started)))))))))

(deftest grounded-success-requires-every-semantic-ladder-join
  (fixture
   (fn [{:keys [roots projection-file binding-file projection binding]}]
     (doseq [[label mutate]
             [[:selection-absent
               #(assoc-in % [:checkpoints :selection]
                          {:status :absent :reason :checkpoint-not-returned})]
              [:selection-pin #(assoc-in % [:checkpoints :selection :ground :run4/task-pin]
                                          (assoc pin :sha256 (apply str (repeat 64 "b"))))]
              [:construction-pin #(assoc-in % [:checkpoints :construction :judgment
                                               :run4/task-pin]
                                             (assoc pin :trial-id :other))]
              [:dispatch-absent #(assoc-in % [:checkpoints :dispatch]
                                           {:status :absent :reason :checkpoint-not-returned})]
              [:review-job #(assoc-in % [:checkpoints :build :judgment :validation
                                         :review-job] "unrelated-review")]
              [:build-commit #(assoc-in % [:checkpoints :build :judgment :commits]
                                         ["unrelated-commit"])]
              [:adjudicated-commit #(assoc-in % [:checkpoints :adjudication :judgment
                                                 :build-match :commit]
                                               "unrelated-commit")]
              [:implementation-id #(assoc-in % [:checkpoints :adjudication :judgment
                                                :dial :implementation-id]
                                              "other-implementation")]
              [:contradictory-failure #(assoc % :failure
                                              {:kind :build-failed :stage :reviewer-wait})]]]
       (testing (name label)
         (let [modified (mutate projection)]
           (write! projection-file modified)
           (write! binding-file
                   (assoc binding :run4/terminal-projection
                          (assoc (:run4/terminal-projection binding)
                                 :sha256 (digest/sha256 (pr-str modified)))))
           (is (nil? (sut/read-terminal-evidence roots request started)))))))))

(deftest safe-build-failure-rejects-contradictory-success-checkpoint
  (fixture
   (fn [{:keys [roots projection-file binding-file projection binding]}]
     (let [contradictory (-> projection
                             (assoc :outcome :build-failed)
                             (assoc :failure {:kind :build-failed :stage :reviewer-wait}))]
       (write! projection-file contradictory)
       (write! binding-file
               (assoc binding :outcome :build-failed
                      :run4/terminal-projection
                      (assoc (:run4/terminal-projection binding)
                             :sha256 (digest/sha256 (pr-str contradictory)))))
       (is (nil? (sut/read-terminal-evidence roots request started)))))))

(deftest missing-is-indeterminate-and-unknown-does-not-advance
  (fixture
   (fn [{:keys [roots binding-file projection-file projection binding]}]
     (io/delete-file binding-file)
     (is (nil? (sut/read-terminal-evidence roots request started)))
     (write! binding-file binding)
     (let [unknown (assoc projection :outcome :artifact-only)]
       (write! projection-file unknown)
       (write! binding-file
               (assoc binding :outcome :artifact-only
                      :run4/terminal-projection
                      (assoc (:run4/terminal-projection binding)
                             :sha256 (digest/sha256 (pr-str unknown)))))
       (is (nil? (sut/read-terminal-evidence roots request started)))))))

(deftest every-identity-and-source-join-is-fail-closed
  (fixture
   (fn [{:keys [roots projection-file binding-file run-file projection binding]}]
     (doseq [[label mutate]
             [[:wrong-pin #(write! projection-file
                                   (assoc-in projection [:run4/task-pin :sha256]
                                             (apply str (repeat 64 "b"))))]
              [:truncated #(spit projection-file "{")]
              [:wrong-click #(write! binding-file (assoc binding :click/id "other"))]
              [:source-drift #(write! run-file (assoc (read-string (slurp run-file))
                                                      :traceWritten false))]]]
       (testing (name label)
         ;; Restore the exact fixture before applying one mutation.
         (write! projection-file projection)
         (write! binding-file binding)
         (write! run-file {:run/id "run-1" :click/id "click-1"
                           :startedAt (:started-at started)
                           :selectorSeam "live:validated-selection" :traceWritten true
                           :route [{:fromNode "R20" :toNode "R12" :via "observe"
                                    :at_ (:started-at started)}]
                           :run4/task-pin pin})
         (mutate)
         (is (= :run4-terminal-evidence-refused
                (:error (try (sut/read-terminal-evidence roots request started) nil
                             (catch clojure.lang.ExceptionInfo e (ex-data e)))))))))))
