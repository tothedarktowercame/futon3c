(ns futon3c.wm.run4-historical-projection-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3c.wm.run4-historical-projection :as sut]))

(defn- fixture []
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "historical-projection"
                       (make-array java.nio.file.attribute.FileAttribute 0)))
        requested {:status :authenticated-not-enacted
                   :identity {:series-id "series" :trial-id :trial
                              :pin-sha256 (apply str (repeat 64 "a"))
                              :casting {:author "zai-2" :reviewer "codex-12"
                                        :repair-reviewer "codex-10"}}}
        enacted {:type :revalidate-historical-repair :repair-id "repair-057"}
        checkpoints {:selection {:ground {:run4/requested-pin requested
                                          :run4/enacted-action enacted}}
                     :adjudication {:judgment {:repair-resolved? false}}}
        execution {:kind :runner-execution :id "qualified-attempt-2"}
        provenance (merge execution {:identity-version 1 :cohort-id :cohort
                                     :cohort-sha256 (apply str (repeat 64 "d"))
                                     :data-root-sha256 (apply str (repeat 64 "e"))
                                     :authority-id (apply str (repeat 64 "f"))
                                     :attempt-id "attempt-2"})
        transition {:schema :wm/historical-repair-admission-v1
                    :repair/id "repair-057" :repair/status :awaiting-validation
                    :verification-id "verification-057"
                    :verification-attempt execution
                    :verification-source {:path "/evidence/source"
                                          :sha256 (apply str (repeat 64 "b"))}
                    :verification-artifact {:path "/evidence/store"
                                            :sha256 (apply str (repeat 64 "c"))}}
        record {:click/id "click-1" :run/id "run-1"
                :runner-attempt/id "attempt-2"
                :runner-execution/identity execution
                :runner-execution/provenance provenance
                :run4/controller-attempt-id "controller-attempt-2"
                :run4/requested-pin requested :run4/enacted-action enacted
                :historical-verification transition
                :execution-cohort {:cohort-id :cohort :sha256 (apply str (repeat 64 "d"))}}
        path (io/file root "run.edn")
        _ (spit path (str (pr-str record) "\n"))
        result {:run/id "run-1" :attempt-id "attempt-2"
                :execution-identity execution :execution-provenance provenance
                :outcome :historical-verification-awaiting-validation
                :run-record (.getPath path) :checkpoints checkpoints
                :data {:repair-obligation transition}}]
    {:root root :result result}))

(deftest persists-nonresolution-historical-projection
  (let [{:keys [root result]} (fixture)
        ref (sut/persist! (.getPath root) "click-1" result)
        value (read-string (slurp (:path ref)))]
    (is (= :wm/run4-historical-admission-projection-v2 (:schema value)))
    (is (= :awaiting-validation (get-in value [:repair :status])))
    (is (false? (get-in value [:repair :resolved?])))
    (is (true? (get-in value [:repair :production-successor-required?])))
    (is (= :authenticated-not-enacted (get-in value [:requested-pin :status])))))

(deftest refuses-foreign-run-record-and-success-label
  (let [{:keys [result]} (fixture)]
    (is (= :historical-run-record-binding-mismatch
           (:reason (try (sut/projection "foreign-click" result) nil
                         (catch Exception e (ex-data e))))))
    (is (= :malformed-historical-result
           (:reason (try (sut/projection "click-1" (assoc result :outcome :grounded-change)) nil
                         (catch Exception e (ex-data e))))))))
