(ns futon3c.wm.run4-terminal-projection-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon2.aif.c-fold-config :as digest]
            [futon2.aif.full-loop-runner :as full-runner]
            [futon3c.wm.run4-terminal-projection :as sut]
            [futon3c.wm.runner-service :as service]))

(def pin {:sha256 (apply str (repeat 64 "a")) :series-id "RUN4"
          :trial-id :outer-loop :mission-id "M-run4" :action {:type :mission}})
(def checkpoints
  {:selection {:judgment {:outcome :ok}
               :ground {:kind :policy-selection :run4/task-pin pin}}
   :construction {:judgment {:run4/task-pin pin} :ground {:kind :construction}}
   :dispatch {:judgment {:agent "zai-2" :availability :invoke-ready
                         :job-id "author-1"}
              :ground {:kind :agency-dispatch}}
   :build {:judgment {:commits ["abc"]
                      :validation {:approved? true :review-job "reviewer-1"
                                   :review-gate {:required? true :executed? true
                                                 :tool-events 2 :passed? true}}}
           :ground {:kind :build}}
   :adjudication {:judgment {:build-match {:review-approved? true}
                             :dial {:moved? true}}
                  :ground {:kind :authoritative-substrate-discharge}}})

(defn delete-tree! [root]
  (doseq [f (reverse (file-seq root))] (io/delete-file f true)))

(defn fixture [f]
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "run4-terminal" (make-array java.nio.file.attribute.FileAttribute 0)))
        projections (io/file root "projections")
        bindings (io/file root "bindings")
        run-record (io/file root "run.edn")]
    (.mkdirs projections) (.mkdirs bindings)
    (spit run-record (str (pr-str {:run/id "run-1" :click/id "click-1"
                                   :run4/task-pin pin}) "\n"))
    (try
      (f {:root root :projections (.getPath projections) :bindings (.getPath bindings)
          :run-record (.getPath run-record)
          :result {:run/id "run-1" :attempt-id "internal-1"
                   :outcome :grounded-change :checkpoints checkpoints
                   :data {:commit "abc" :author-job {:job-id "author-1"}
                          :review-job {:job-id "reviewer-1"}
                          :witness {:resolved? true :dial-moved? true}}
                   :run-record (.getPath run-record)}})
      (finally (delete-tree! root)))))

(deftest persists-versioned-source-bound-projection-and-digest
  (fixture
   (fn [{:keys [projections result]}]
     (let [{:keys [path sha256]} (sut/persist! projections "click-1" result)
           value (read-string (slurp path))]
       (is (= :wm-run4-terminal-projection-v1 (:schema value)))
       (is (= ["click-1" "run-1" "internal-1"]
              ((juxt :click/id :run/id :attempt/id) value)))
       (is (= pin (:run4/task-pin value)))
       (is (= :grounded-change (:outcome value)))
       (is (= #{:selection :construction :dispatch :build :adjudication}
              (set (keys (:checkpoints value)))))
       (is (= sha256 (digest/sha256 (pr-str value))))
       (is (= {:kind nil :stage nil} (:failure value)))
       (is (= true (get-in value [:checkpoints :build :judgment
                                  :validation :approved?])))
       (is (= {:resolved? true :dial-moved? true}
              (select-keys (get-in value [:evidence :grounding-witness])
                           [:resolved? :dial-moved?])))))))

(deftest legacy-opt-out-is-byte-and-write-free
  (fixture
   (fn [{:keys [projections result]}]
     (let [legacy (assoc result :checkpoints {})]
       (is (nil? (sut/persist! projections "click-1" legacy)))
       (is (empty? (seq (.listFiles (io/file projections)))))))))

(deftest present-malformed-run4-pin-is-not-a-legacy-opt-out
  (fixture
   (fn [{:keys [projections result]}]
     (doseq [bad [nil false {:sha256 "bad"}]]
       (let [bad-result (assoc-in result
                                  [:checkpoints :selection :ground :run4/task-pin]
                                  bad)]
         (is (= :malformed-returned-evidence
                (:reason (try (sut/persist! projections "click-1" bad-result) nil
                              (catch clojure.lang.ExceptionInfo e (ex-data e)))))))))))

(deftest validated-and-hashed-run-record-is-one-byte-snapshot
  (fixture
   (fn [{:keys [result run-record]}]
     (let [actual (slurp run-record)
           value (with-redefs [clojure.core/slurp
                               (fn [& _] "{:run/id \"different-second-read\"}")]
                   (sut/projection "click-1" result))]
       (is (= "run-1" (:run/id value)))
       (is (= (digest/sha256 actual)
              (get-in value [:source :run-record-sha256])))))))

(deftest malformed-mismatched-truncated-and-conflicting-evidence-refuses
  (fixture
   (fn [{:keys [projections result run-record]}]
     (testing "missing returned identity"
       (is (= :malformed-returned-evidence
              (:reason (try (sut/persist! projections "click-1"
                                           (dissoc result :attempt-id)) nil
                            (catch clojure.lang.ExceptionInfo e (ex-data e)))))))
     (testing "run binding mismatch"
       (is (= :run-record-binding-mismatch
              (:reason (try (sut/persist! projections "wrong" result) nil
                            (catch clojure.lang.ExceptionInfo e (ex-data e)))))))
     (testing "truncated source"
       (spit run-record "{")
       (is (= :unreadable-source
              (:reason (try (sut/persist! projections "click-1" result) nil
                            (catch clojure.lang.ExceptionInfo e (ex-data e))))))))))

(deftest failed-write-and-replay-conflict-never-return-a-projection-reference
  (fixture
   (fn [{:keys [projections result]}]
     (is (thrown? clojure.lang.ExceptionInfo
                  (binding [sut/*atomic-write!* (fn [& _]
                                                  (throw (ex-info "write failed" {})))]
                    (sut/persist! projections "click-1" result))))
     (let [{:keys [path]} (sut/persist! projections "click-1" result)]
       (spit path (pr-str (assoc (read-string (slurp path)) :outcome :other)))
       (is (= :projection-replay-conflict
              (:reason (try (sut/persist! projections "click-1" result) nil
                            (catch clojure.lang.ExceptionInfo e (ex-data e))))))))))

(deftest runner-binding-references-projection-only-for-run4
  (fixture
   (fn [{:keys [projections bindings result]}]
     (binding [service/*run4-terminal-projection-dir* projections
               service/*click-run-binding-dir* bindings]
       (let [binding (#'service/persist-click-run-binding! "click-1" result)
             stored (read-string (slurp (:path binding)))]
         (is (= (:run4/terminal-projection stored)
                (:run4/terminal-projection binding)))
         (is (= #{:path :sha256 :source-sha256}
                (set (keys (:run4/terminal-projection stored))))))))))

(deftest consumes-actual-run-opportunity-wrapper-result-with-stubbed-core
  (fixture
   (fn [{:keys [root projections]}]
     (let [record-dir (io/file root "actual-run-records")]
       (.mkdirs record-dir)
       (binding [full-runner/*wm-status-reporting?* false]
         (with-redefs-fn
           {#'full-runner/run-opportunity-core!
            (fn [_] {:attempt-id "actual-internal-attempt"
                     :outcome :grounded-change :checkpoints checkpoints
                     :data {:commit "actual-commit" :witness {:resolved? true}}
                     :wm/route [{:node :R20 :via "scan" :at "2026-09-10T00:00:00Z"}
                                {:node :R12 :via "observe" :at "2026-09-10T00:00:01Z"}]})}
           (fn []
             (let [result (full-runner/run-opportunity!
                           {:run-id "actual-run" :click-id "actual-click"
                            :run-record-dir (.getPath record-dir)})
                   ref (sut/persist! projections "actual-click" result)
                   value (read-string (slurp (:path ref)))]
               (is (= "actual-run" (:run/id value)))
               (is (= "actual-internal-attempt" (:attempt/id value)))
               (is (= :grounded-change (:outcome value)))))))))))
