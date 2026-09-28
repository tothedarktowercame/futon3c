(ns futon3c.agency.offer-record-cli-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.agency.act-harness :as act-harness]
            [futon3c.agency.offer-record-cli :as cli]
            [futon3c.agency.rule-record :as store]))

(def stamp {:executor "agent-a" :signer "agent-a"
            :authority {:grant "act:offer-grant"} :executor-basis :declared})
(def request
  {:record {:kind :offer/record :author "agent-a" :addressee "joe"
            :seat {:agent "agent-a" :session "session-a"}
            :at "2026-09-28T14:00:00Z"
            :options [{:option/id "1" :option/label "one"
                       :option/scope {:description "one packet"}}]}
   :idempotency-key "offer-1"})

(deftest payload-is-minted-stamped-and-scoped
  (let [payload (cli/payload request (act-harness/plain "test:offer") stamp)]
    (is (:hx/mint-id payload))
    (is (= "offer-1" (:hx/idempotency-key payload)))
    (is (= :offer/record (:hx/type payload)))
    (is (= ["agent:agent-a" "session:session-a" "agent:joe"]
           (:hx/endpoints payload)))
    (is (= 1 (get-in payload [:hx/props :offer/schema])))
    (is (= stamp (get-in payload [:hx/props :act/stamp])))))

(deftest write-authorizes-before-post-and-verifies-seat-scoped-readback
  (let [calls (atom [])
        written (atom nil)
        grant {:hx/id "act:offer-grant" :hx/type :grant/record
               :hx/props {:grant/grantor "joe" :grant/grantee "agent-a"
                          :grant/basis :explicit
                          :grant/scope {:description "offers"
                                        :act-kinds [:offer/record]
                                        :own-acts-only true}
                          :grant/interval {:from "2026-09-28T13:00:00Z"}
                          :grant/source {:id "e:joe" :author "joe"
                                         :at "2026-09-28T13:00:00Z"
                                         :quote "offers"}}}]
    (with-redefs [store/request!
                  (fn [_ method path body]
                    (swap! calls conj [method path])
                    (cond
                      (= path "/api/alpha/hyperedge/act%3Aoffer-grant") grant
                      (= method "POST")
                      (let [doc (-> body (dissoc :hx/mint-id :hx/idempotency-key
                                                 :hx/valid-time)
                                    (assoc :hx/id "act:offer"))]
                        (reset! written doc)
                        {:ok true :hx/id "act:offer"})
                      (str/includes? path "type=offer%2Frecord&end=session%3Asession-a")
                      {:hyperedges [@written]}
                      :else (throw (ex-info "unexpected" {:path path}))))]
      (let [result (cli/write! "http://store" request
                               (act-harness/plain "test:offer") stamp)]
        (is (= "act:offer" (get-in result [:record :id])))
        (is (true? (get-in result [:receipt :verified?])))
        (is (= ["GET" "POST" "GET"] (mapv first @calls)))))))
