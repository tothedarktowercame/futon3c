(ns futon3c.evidence.futon1b-backend-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.evidence.backend :as backend]
            [futon3c.evidence.futon1b-backend :as sut]
            [org.httpkit.client :as http]))

(deftest client-only-filters-are-sent-as-narrowing-hints
  (testing "the server narrows candidates while the client retains exact membership"
    (let [seen-url (atom nil)
          store (sut/make-futon1b-backend "http://store.test")]
      (with-redefs [http/get (fn [url _]
                               (reset! seen-url url)
                               (delay {:status 200
                                       :body "{:entries []}"}))]
        (is (= []
               (backend/-query store
                               {:query/tags [:open :pipeline-tracer]
                                :query/subject {:ref/type :thread :ref/id "t 1"}
                                :query/pattern-id :agent/pause
                                :query/limit 5})))
        (is (str/includes? @seen-url "tags=open%2Cpipeline-tracer"))
        (is (str/includes? @seen-url "subject-type=thread"))
        (is (str/includes? @seen-url "subject-id=t+1"))
        (is (str/includes? @seen-url "pattern-id=pause"))
        (is (not (str/includes? @seen-url "limit=")))
        (is (str/includes? @seen-url "include-ephemeral=true"))))))
