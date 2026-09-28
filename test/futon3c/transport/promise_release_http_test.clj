(ns futon3c.transport.promise-release-http-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is]]
            [futon3c.agency.obligations-reader :as reader]
            [futon3c.agency.promise-history :as history]
            [futon3c.agency.rule-record :as store]
            [futon3c.social.test-fixtures :as fix]
            [futon3c.transport.http :as http]))

(def rec {:id "p" :agent "debtor" :beneficiary "creditor"
          :deadline "2026-09-29T00:00:00Z"
          :fulfilment-criterion {:kind :job-terminal-ok :job-id "j"
                                 :machine-evaluable? true}})
(defn history-row [id type n previous details]
  {:evidence/id id :evidence/type type :evidence/at "2026-09-28T10:00:00Z"
   :evidence/body (merge rec details
                         {:history/format 3 :history/promise-id "p"
                          :history/promise-sequence n :history/predecessor previous
                          :history/payload-edn
                          (pr-str {:record rec :changes [] :predecessor previous})})})
(def creation (history-row "e-create" :promise/park-made 1 nil {}))
(defn inputs [& rows]
  {:promise-history (into [creation] rows) :promise-outcomes []
   :agreements [] :offers [] :basis {:mode :current :t "2026-09-28T12:00:00Z"}})
(defn handler [] (http/make-handler {:registry (fix/mock-registry) :patterns (fix/mock-patterns)}))
(defn request [caller role]
  {:request-method :post :uri "/api/alpha/promise/release"
   :body (json/generate-string {:caller caller :promise-id "p" :role role
                                :at "2026-09-28T12:00:00Z"})})
(defn body [r] (json/parse-string (:body r) true))

(defn run-write [caller role input]
  (let [submitted (atom nil)
        eid "e-release"
        grant "act:grant"]
    (with-redefs-fn
      {#'reader/read-inputs (fn [& _] input)
       #'futon3c.transport.http/promise-release-grant-id (fn [& _] grant)
       #'history/record! (fn [type record _ details]
                           (reset! submitted [type record details]) eid)
       #'history/await-writes! (constantly true)
       #'store/request! (fn [_ _ _ _]
                          (let [[type _ details] @submitted]
                            {:evidence/id eid :evidence/type type
                             :evidence/body details}))}
      #(let [response ((handler) (request caller role))]
         {:response response :submitted @submitted}))))

(deftest creditor-release-and-debtor-abandonment-write-stamped-roles
  (doseq [[caller role expected] [["creditor" "creditor" "released"]
                                  ["debtor" "debtor" "abandoned"]]]
    (let [{:keys [response submitted]} (run-write caller role (inputs))
          details (nth submitted 2)]
      (is (= 200 (:status response)))
      (is (= expected (:status (body response))))
      (is (= :explicit (:release/basis details)))
      (is (= (keyword role) (:release/role details)))
      (is (= caller (get-in details [:act/stamp :signer]))))))

(deftest third-party-and-missing-grant-write-nothing
  (let [writes (atom 0)]
    (with-redefs-fn
      {#'reader/read-inputs (fn [& _] (inputs))
       #'history/record! (fn [& _] (swap! writes inc))}
      #(is (= 403 (:status ((handler) (request "third" "debtor"))))))
    (is (zero? @writes))
    (with-redefs-fn
      {#'reader/read-inputs (fn [& _] (inputs))
       #'futon3c.transport.http/promise-release-grant-id (constantly nil)
       #'history/record! (fn [& _] (swap! writes inc))}
      #(is (= "no-grant" (:reason (body ((handler) (request "debtor" "debtor")))))))
    (is (zero? @writes))))

(deftest completed-is-closed-but-plain-wake-release-remains-releasable
  (let [fulfilled {:evidence/id "e-f" :evidence/type :promise/fulfilled
                   :evidence/at "2026-09-28T11:00:00Z"
                   :evidence/body {:promise-id "p"}}
        closed (assoc (inputs) :promise-outcomes [fulfilled])]
    (with-redefs [reader/read-inputs (fn [& _] closed)]
      (is (= 409 (:status ((handler) (request "debtor" "debtor"))))))
    (let [prev {:sequence 1 :id "e-create" :type :promise/park-made}
          plain (history-row "e-release-plain" :promise/released 2 prev {})]
      (is (= 200 (:status (:response (run-write "debtor" "debtor" (inputs plain)))))))))

(deftest missing-beneficiary-does-not-block-debtor-abandonment
  (let [without-beneficiary (assoc rec :beneficiary nil)
        creation' (assoc creation :evidence/body
                         (merge without-beneficiary
                                (select-keys (:evidence/body creation)
                                             [:history/format :history/promise-id
                                              :history/promise-sequence :history/predecessor])
                                {:history/payload-edn
                                 (pr-str {:record without-beneficiary :changes []
                                          :predecessor nil})}))]
    (is (= 200 (:status (:response
                         (run-write "debtor" "debtor"
                                    (assoc (inputs) :promise-history [creation']))))))))
