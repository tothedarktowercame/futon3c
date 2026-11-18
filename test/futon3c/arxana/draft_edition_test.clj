(ns futon3c.arxana.draft-edition-test
  (:require [clojure.test :refer [deftest is]] [clojure.edn :as edn] [clojure.java.io :as io]
            [cheshire.core :as json] [futon3c.arxana.draft-edition :as d]
            [futon3c.arxana.draft-edition-catalogue :as dc] [futon3c.arxana.design-catalogue :as c]
            [futon3c.arxana.store :as store] [futon3c.arxana.write :as write]
            [futon3c.arxana.service :as service]))
(defn fixture [f]
  (let [draft (java.io.File/createTempFile "invented-draft" ".txt") source (java.io.File/createTempFile "invented-source" ".json")
        text "😀 Same. Same." st "😀 proof one; proof two"]
    (try
      (spit draft text) (spit source (json/generate-string {:id "invented-source" :text st}))
      (f {:draft-path (.getPath draft) :draft-digest (d/sha text) :scope-digest (d/sha text)
          :at "2026-11-01T00:00:00.000Z" :segmentation :invented/split :pins {}
          :anchors [{:index 1 :sentence "Same." :draft-start 2 :draft-end 7 :class :none :benefit? true}
                    {:index 2 :sentence "Same." :draft-start 8 :draft-end 13 :class :none}]
          :candidates {:clips (mapv (fn [[a b]] {:segment 1 :span [a b] :text (d/slice st a b)
                                                :source {:file (.getPath source) :id "invented-source" :text-field "text" :text-sha256 (d/sha st)}
                                                :query {:strategy "keyword"} :accepted true :accepted-by "zai-13-test"}) [[2 11] [13 22]])}}
         source)
      (finally (.delete draft) (.delete source)))))
(deftest pins-offsets-review-and-projection
  (fixture (fn [input source]
             (let [b (d/build input) v (d/edition-view b) link (get-in b [:items 0 :link :id])]
               (is (= 2 (count (get-in v [:segments 0 :proposals]))))
               (is (not= (get-in b [:target-refs 0 :id]) (get-in b [:target-refs 1 :id])))
               (is (= :none (get-in v [:segments 0 :proposals 0 :review :joe])))
               (let [review (d/review link "joe" :accepted "2026-11-01T01:00:00.000Z" :annotation)
                     view (d/edition-view (assoc b :reviews [review]))]
                 (is (= :accepted (get-in view [:segments 0 :proposals 0 :review :joe])))
                 (is (= :projected (get-in view [:segments 0 :benefit-status]))))
               (is (= :none (:joe (d/review-state [(d/review link "zai-13-test" :accepted "2026-11-01T01:00:00.000Z" :annotation)] link))))
               (is (= :inconclusive (:status (d/edition-view (assoc b :truncated? true)))))
               (is (thrown? clojure.lang.ExceptionInfo (d/build (assoc input :draft-digest "wrong"))))
               (spit source (json/generate-string {:id "invented-source" :text "changed"}))
               (is (= :digest-mismatch (try (d/build input) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))) ))
(deftest actual-write-validation-and-coverage
  (fixture (fn [input _]
             (let [b (d/build input) docs (dc/records {:revision "invented" :implementation-digest "invented"})
                   saved (atom {}) s (reify store/ArxanaGraphStore
                                        (get-entity [_ id] {:ok (get @saved id)})
                                        (conditional-write! [_ cmd] (let [r (get-in cmd [:body :record])] (swap! saved assoc (:id r) r) {:ok {:created? true}})))
                   citations (edn/read-string (slurp (io/resource "arxana/demo-c-citations.edn")))]
               (is (= :pass (:result (dc/probe b))))
               (is (= :pass (:result (c/coverage-check docs (mapv :feature (:reads citations))))))
               (doseq [r (concat (:records b) docs)]
                 (is (nil? (:error (write/execute s {:principal/id "arxana-service" :message/id (:id r)}
                                                 (cond (:ref/kind r) :external-reference/assert (:type-revision r) :clusion/assert :else :evidence/append) r)))))))))
(deftest envelope-refuses-extra-body
  (is (:error (service/dispatch {} {:protocol/version "arxana.v1" :message/id "invented" :principal/id "invented"
                                    :operation :draft/edition :body {:unexpected true}}))))

(deftest envelope-returns-edition
  (with-redefs [d/read-edition (fn [_] {:status :ok :authority :pinned-external-files :stored? false :segments []})]
    (is (= :ok (get-in (service/dispatch {} {:protocol/version "arxana.v1" :message/id "edition"
                                            :principal/id "invented" :operation :draft/edition :body {}})
                       [:ok :status])))))
