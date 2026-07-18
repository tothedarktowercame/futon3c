(ns futon3c.transport.morning-brief-http-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is testing]]
            [futon3c.transport.http :as http]))

(def ^:private review-uri "/api/alpha/morning-brief/review")
(def ^:private addendum-uri "/api/alpha/morning-brief/addendum")
(def ^:private pending-uri "/api/alpha/morning-brief/pending")

(defn- response-body [response]
  (json/parse-string (:body response) true))

(defn- post-review [payload]
  ((http/make-handler {})
   {:request-method :post
    :uri review-uri
    :body (json/generate-string payload)}))

(defn- post-addendum [payload]
  ((http/make-handler {})
   {:request-method :post
    :uri addendum-uri
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

(deftest addendum-route-resolves-and-validates-the-canonical-store-api
  (let [called (atom nil)
        addendum {:morning-brief/addendum-id "mba-1"
                  :attempt-id "attempt-1"
                  :kind :repro}]
    (with-redefs [clojure.core/requiring-resolve
                  (resolver
                   {'futon2.aif.morning-brief/addendum!
                    (fn [& args] (reset! called args) addendum)})]
      (let [response (post-addendum {:attempt-id "attempt-1"
                                     :kind "repro"
                                     :title "Run it"
                                     :body "M-x field-desk -> notebook appears"
                                     :author "joe"})]
        (is (= 200 (:status response)))
        (is (= ["attempt-1" :repro "Run it"
                "M-x field-desk -> notebook appears" "joe"]
               @called))
        (is (= "mba-1" (get-in (response-body response)
                                [:addendum :morning-brief/addendum-id]))))))
  (testing "request shape and canonical store validation both become 400"
    (with-redefs [clojure.core/requiring-resolve
                  (resolver
                   {'futon2.aif.morning-brief/addendum!
                    (fn [& _] (throw (ex-info "Unknown Morning Brief attempt" {})))})]
      (is (= 400 (:status (post-addendum {:attempt-id "attempt-1"
                                          :kind "repro"
                                          :title " "
                                          :body "Body"
                                          :author "joe"}))))
      (is (= 400 (:status (post-addendum {:attempt-id "missing"
                                          :kind "repro"
                                          :title "Run it"
                                          :body "Body"
                                          :author "joe"})))))))

(deftest pending-route-returns-applicable-and-answered-objectives
  (with-redefs [clojure.core/requiring-resolve
                (resolver
                 {'futon2.aif.morning-brief/items
                  (fn [] [{:attempt-id "attempt-1" :commit "abc"}
                          {:attempt-id "attempt-2"}])
                  'futon2.aif.morning-brief/reviews
                  (fn [] [{:attempt-id "attempt-1"
                           :objective :feature-verdict}])
                  'futon2.aif.morning-brief/addenda
                  (fn [] [{:morning-brief/addendum-id "mba-later"
                           :attempt-id "attempt-1" :kind :note
                           :created-at "2026-07-18T12:00:00Z"}
                          {:morning-brief/addendum-id "mba-earlier"
                           :attempt-id "attempt-1" :kind :repro
                           :created-at "2026-07-18T11:00:00Z"}])
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
      (is (= ["mba-earlier" "mba-later"]
             (mapv :morning-brief/addendum-id
                   (get-in body [:items 0 :addenda]))))
      (is (= [] (get-in body [:items 1 :answered-objectives]))))))

(deftest morning-brief-routes-report-an-unavailable-futon2-api
  (with-redefs [clojure.core/requiring-resolve (constantly nil)]
    (doseq [response [(post-review {:attempt-id "attempt-1"
                                    :objective "feature-verdict"
                                    :answer "accept-feature"
                                    :note "Ready"
                                    :reviewer "joe"})
                      (post-addendum {:attempt-id "attempt-1"
                                      :kind "repro"
                                      :title "Run it"
                                      :body "Observe it"
                                      :author "joe"})
                      (get-pending)]]
      (is (= 501 (:status response)))
      (is (= "morning-brief-unavailable" (:err (response-body response)))))))
