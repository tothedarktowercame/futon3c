(ns futon3c.wm.run4-realized-recording-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon2.aif.realized-recording :as recording]
            [futon3c.wm.run4-realized-recording :as sut]))

(def sha (apply str (repeat 64 "a")))

(def bundle
  {:schema :wm/run4-terminal-evidence-bundle-v1
   :identity {:series-id "RUN4" :trial-id :t1
              :casting {:author "zai-2" :reviewer "codex-17"}}
   :attempt-id "attempt-1" :projection-digest sha :run-record-digest sha
   :run-record {:run/id "run-1"}
   :terminal-projection {:run/id "run-1" :attempt/id "internal-1"
                         :run4/task-pin {:mission-id "M-run4"}
                         :outcome :grounded-change :checkpoints {}
                         :failure {:kind nil :stage nil} :evidence {}}
   :classification {:task-result :succeeded :infrastructure :safe}})

(deftest durable-terminal-bundle-becomes-a-valid-realized-recording
  (let [r (sut/from-terminal-bundle bundle)]
    (is (= r (recording/validate! r)))
    (is (= :grounded-change (:outcome r)))
    (is (= :unknown (get-in r [:observations :channels :pre :status])))
    (is (= :prior-accepted-step-not-recorded
           (get-in r [:observations :channels :pre :reason])))))

(deftest absent-terminal-classification-remains-explicitly-unknown
  (let [r (sut/from-terminal-bundle
           (assoc-in bundle [:terminal-projection :outcome] nil))]
    (is (= r (recording/validate! r)))
    (is (nil? (:outcome r)))
    (is (= :unknown (get-in r [:classification :status])))))

(deftest persisted-record-is-strictly-bound-to-current-bundle
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "run4-recording"
                       (make-array java.nio.file.attribute.FileAttribute 0)))]
    (try
      (sut/persist-bundle! (.getPath root) bundle)
      (is (= :wm/realized-recording-v1
             (:recording-contract
              (sut/read-bundle-recording! (.getPath root) bundle))))
      (is (thrown? clojure.lang.ExceptionInfo
                   (sut/read-bundle-recording!
                    (.getPath root) (assoc bundle :projection-digest
                                           (apply str (repeat 64 "b"))))))
      (finally
        (doseq [f (reverse (file-seq root))] (io/delete-file f true))))))
