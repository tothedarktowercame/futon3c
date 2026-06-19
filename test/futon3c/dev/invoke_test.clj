(ns futon3c.dev.invoke-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.dev.invoke :as invoke]
            [futon3c.evidence.store :as estore]))

(deftest emit-invoke-evidence-normalizes-string-tags
  (testing "string tags are converted to keywords before append"
    (let [store (atom {:entries {} :order []})
          result (invoke/emit-invoke-evidence!
                  store
                  "codex-8"
                  "invoke-complete"
                  {"ok" true}
                  :session-id "sid-1"
                  :tags ["invoke-complete"])
          eid (get-in result [:entry :evidence/id])
          persisted (estore/get-entry* store eid)]
      (is (:ok result))
      (is (= [:invoke :dev :codex-8 :invoke-complete]
             (get-in result [:entry :evidence/tags])))
      (is (= [:invoke :dev :codex-8 :invoke-complete]
             (:evidence/tags persisted))))))
