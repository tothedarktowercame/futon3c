(ns futon3c.arxana.formal-f2-demo-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.arxana.formal-f2-demo :as demo]
            [futon3c.arxana.service :as service]))

(deftest private-demo-prepares-editable-f1-head
  (let [h (demo/start! 0)]
    (try
      (let [prepared (:prepared h)
            id (:artifact/id prepared)
            revision-id (:head/revision-id prepared)
            manifest-revision-id (:manifest/revision-id prepared)
            head (service/dispatch (:cfg h) (demo/request "head" :revision/head
                                                          {:artifact/id id}))
            revision (service/dispatch (:cfg h) (demo/request "rev" :revision/get
                                                              {:artifact/revision-id revision-id}))]
        (is (= "formal-arxana:f1-conversation" id))
        (is (= revision-id (get-in head [:ok :head/revision-id])))
        (is (string? manifest-revision-id))
        (is (re-find #"turn-4qzbnC" (get-in revision [:ok :revision/text]))))
      (finally (demo/stop! h)))))
