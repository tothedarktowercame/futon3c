(ns futon3c.transport.morning-brief-http-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is testing]]
            [futon3c.transport.http :as http]))

(def ^:private review-uri "/api/alpha/morning-brief/review")
(def ^:private pending-uri "/api/alpha/morning-brief/pending")

(defn- response-body [response]
  (json/parse-string (:body response) true))

(defn- post-review [payload]
  ((http/make-handler {})
   {:request-method :post
    :uri review-uri
    :body (json/generate-string payload)}))

(defn- get-pending []
  ((http/make-handler {}) {:request-method :get :uri pending-uri}))

(defn- resolver [implementations]
  (fn [sym] (get implementations sym)))

(deftest review-route-resolves-and-invokes-canonical-store-api
  (let [called (atom nil)
        review {:morning-brief/review-id "mbqa-1"
                :attempt-id "attempt-1"
                :objective :feature-verdict
                :answer :accept-feature}]
    (with-redefs [clojure.core/requiring-resolve
                  (resolver
                   {'futon2.aif.morning-brief/review!
                    (fn [& args] (reset! called args) review)})]
      (let [response (post-review {:attempt-id "attempt-1"
                                   :objective "feature-verdict"
                                   :answer "accept-feature"
                                   :note "Ready to use"
                                   :reviewer "joe"})]
        (is (= 200 (:status response)))
        (is (= ["attempt-1" :feature-verdict :accept-feature
                "Ready to use" "joe"]
               @called))
        (is (= "mbqa-1" (get-in (response-body response)
                                 [:review :morning-brief/review-id])))))))

(deftest review-route-maps-validation-and-conflict-errors
  (testing "invalid request fields are rejected before store invocation"
    (with-redefs [clojure.core/requiring-resolve
                  (resolver {'futon2.aif.morning-brief/review! (fn [& _])})]
      (is (= 400 (:status (post-review {:attempt-id "attempt-1"
                                        :objective "feature-verdict"
                                        :answer "accept-feature"
                                        :note " "
                                        :reviewer "joe"}))))))
  (testing "store validation errors become 400"
    (with-redefs [clojure.core/requiring-resolve
                  (resolver
                   {'futon2.aif.morning-brief/review!
                    (fn [& _] (throw (ex-info "Unknown Morning Brief answer" {})))})]
      (is (= 400 (:status (post-review {:attempt-id "attempt-1"
                                        :objective "feature-verdict"
                                        :answer "bogus"
                                        :note "No"
                                        :reviewer "joe"}))))))
  (testing "the store's per-objective duplicate refusal becomes 409"
    (with-redefs [clojure.core/requiring-resolve
                  (resolver
                   {'futon2.aif.morning-brief/review!
                    (fn [& _]
                      (throw (ex-info "Morning Brief objective was already reviewed"
                                      {:review-id "mbqa-existing"})))})]
      (is (= 409 (:status (post-review {:attempt-id "attempt-1"
                                        :objective "feature-verdict"
                                        :answer "accept-feature"
                                        :note "Again"
                                        :reviewer "joe"})))))))

(deftest pending-route-returns-applicable-and-answered-objectives
  (with-redefs [clojure.core/requiring-resolve
                (resolver
                 {'futon2.aif.morning-brief/items
                  (fn [] [{:attempt-id "attempt-1" :commit "abc"}
                          {:attempt-id "attempt-2"}])
                  'futon2.aif.morning-brief/reviews
                  (fn [] [{:attempt-id "attempt-1"
                           :objective :feature-verdict}])
                  'futon2.aif.morning-brief/item-objectives
                  (fn [item]
                    (if (:commit item)
                      [:feature-verdict :selection-quality]
                      [:selection-quality]))})]
    (let [response (get-pending)
          body (response-body response)]
      (is (= 200 (:status response)))
      (is (= 2 (:count body)))
      (is (= ["feature-verdict" "selection-quality"]
             (get-in body [:items 0 :applicable-objectives])))
      (is (= ["feature-verdict"]
             (get-in body [:items 0 :answered-objectives])))
      (is (= [] (get-in body [:items 1 :answered-objectives]))))))

(deftest morning-brief-routes-report-an-unavailable-futon2-api
  (with-redefs [clojure.core/requiring-resolve (constantly nil)]
    (doseq [response [(post-review {:attempt-id "attempt-1"
                                    :objective "feature-verdict"
                                    :answer "accept-feature"
                                    :note "Ready"
                                    :reviewer "joe"})
                      (get-pending)]]
      (is (= 501 (:status response)))
      (is (= "morning-brief-unavailable" (:err (response-body response)))))))
