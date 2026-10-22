(ns futon3c.wm.cast-refresh-http-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is testing]]
            [futon3c.agency.registry :as registry]
            [futon3c.transport.http :as http]
            [futon3c.wm.ordinary-click-budget :as ordinary-budget]
            [futon3c.wm.runner-service :as service]))

(defn- request []
  {:request-method :post
   :uri "/api/alpha/wm/click"
   :headers {}
   :body (java.io.ByteArrayInputStream.
          (.getBytes
           (json/generate-string
            {:run-id "2026-10-22-aab6ddc5-924e-46e5-b28a-c12feccf4ddf"
             :trigger "duree-click-on-demand"
             :author "author-seat"
             :reviewer "reviewer-seat"
             :repair-reviewer "repair-seat"})
           "UTF-8"))})

(deftest ordinary-click-refreshes-cast-before-start
  (let [events (atom [])
        clicked (atom nil)
        old-ids {"author-seat" "author-old"
                 "reviewer-seat" "reviewer-old"
                 "repair-seat" "repair-old"}
        handler (http/make-handler {})]
    (with-redefs [service/prepare-ordinary-click-opts identity
                  service/cast-preflight-refusal
                  (fn [_]
                    (swap! events conj :preflight)
                    nil)
                  registry/reset-session!
                  (fn [seat]
                    (swap! events conj [:reset seat])
                    {:ok true :agent-id seat :old-session-id (old-ids seat)})
                  service/click!
                  (fn [opts]
                    (swap! events conj :click)
                    (reset! clicked opts)
                    {:started true})]
      (is (= 200 (:status (handler (request)))))
      (is (= [:preflight
              [:reset "author-seat"]
              [:reset "reviewer-seat"]
              [:reset "repair-seat"]
              :click]
             @events))
      (is (= ["author-old" "reviewer-old" "repair-old"]
             (mapv :old-session-id (:cast-refresh @clicked))))
      (is (every? #(= {:status :absent
                       :reason :seat-context-state-not-readable-without-invoke}
                      (:carried-before %))
                  (:cast-refresh @clicked))))))

(deftest reset-failure-refuses-before-click-or-budget
  (testing "the bad reset is terminal at the pre-budget boundary"
    (let [clicks (atom 0)
          budget-spends (atom 0)
          handler (http/make-handler {})]
      (with-redefs [service/prepare-ordinary-click-opts identity
                    service/cast-preflight-refusal (constantly nil)
                    registry/reset-session!
                    (fn [seat]
                      (if (= "reviewer-seat" seat)
                        {:ok false :error {:error/code :backing-reset-failed}}
                        {:ok true :agent-id seat :old-session-id (str seat "-old")}))
                    service/click!
                    (fn [opts]
                      (swap! clicks inc)
                      ((:ordinary-click/issue! opts) "click" "now")
                      {:started true})
                    ordinary-budget/consume!
                    (fn [& _] (swap! budget-spends inc))]
        (let [response (handler (request))
              body (json/parse-string (:body response) true)]
          (is (= 409 (:status response)))
          (is (= "wm-click-cast-refresh-failed" (:error body)))
          (is (= "reviewer-seat" (get-in body [:details :seat])))
          (is (zero? @clicks))
          (is (zero? @budget-spends)))))))
