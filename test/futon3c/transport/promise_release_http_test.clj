(ns futon3c.transport.promise-release-http-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is]]
            [futon3c.agency.obligations-reader :as reader]
            [futon3c.agency.promise-history :as history]
            [futon3c.agency.rule-record :as store]
            [futon3c.social.test-fixtures :as fix]
            [futon3c.transport.http :as http]))

(def rec {:id "p" :agent "debtor" :beneficiary "creditor"
          :deadline "2026-10-29T00:00:00Z"
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
(defn request
  ([caller role] (request caller role nil))
  ([caller role countersigns]
   {:request-method :post :uri "/api/alpha/promise/release"
    :body (json/generate-string
           (cond-> {:caller caller :promise-id "p" :role role
                    :reason "The promise cannot proceed"}
             countersigns (assoc :countersigns countersigns)))}))
(defn body [r] (json/parse-string (:body r) true))

(defn run-write
  ([caller role input] (run-write caller role nil input))
  ([caller role countersigns input]
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
       #(let [response ((handler) (request caller role countersigns))]
          {:response response :submitted @submitted})))))

(deftest either-party-voids-with-a-reason-and-stamp
  (doseq [[caller role expected] [["creditor" "creditor" "voided-by-creditor"]
                                  ["debtor" "debtor" "voided-by-debtor"]]]
    (let [{:keys [response submitted]} (run-write caller role (inputs))
          details (nth submitted 2)]
      (is (= 200 (:status response)))
      (is (= expected (:status (body response))))
      (is (= :explicit (:release/basis details)))
      (is (= (keyword role) (:release/role details)))
      (is (= "The promise cannot proceed" (:release/reason details)))
      (is (= caller (get-in details [:act/stamp :signer]))))))

(deftest opposite-party-countersignature-settles
  (let [prev {:sequence 1 :id "e-create" :type :promise/park-made}
        first-release (history-row "e-first" :promise/released 2 prev
                                   {:release/basis :explicit :release/role :creditor
                                    :release/by "creditor" :release/reason "Cannot proceed"})
        {:keys [response submitted]}
        (run-write "debtor" "debtor" "e-first" (inputs first-release))]
    (is (= 200 (:status response)))
    (is (= "settled" (:status (body response))))
    (is (= "e-first" (get-in submitted [2 :release/countersigns])))))

(deftest countersignature-requires-an-opposite-party-row
  (let [writes (atom 0)
        prev {:sequence 1 :id "e-create" :type :promise/park-made}
        same-side (history-row "e-first" :promise/released 2 prev
                               {:release/basis :explicit :release/role :debtor
                                :release/by "debtor" :release/reason "Cannot proceed"})]
    (doseq [input [(inputs) (inputs same-side)]]
      (with-redefs [reader/read-inputs (fn [& _] input)
                    history/record! (fn [& _] (swap! writes inc))]
        (let [r ((handler) (request "debtor" "debtor" "e-first"))]
          (is (= 409 (:status r)))
          (is (= "nothing-to-countersign" (:reason (body r)))))))
    (is (zero? @writes))))

(deftest release-requires-a-reason
  (let [writes (atom 0)]
    (with-redefs [history/record! (fn [& _] (swap! writes inc))]
      (let [r ((handler) {:request-method :post :uri "/api/alpha/promise/release"
                          :body (json/generate-string
                                 {:caller "debtor" :promise-id "p" :role "debtor"})})]
        (is (= 400 (:status r)))
        (is (= "invalid-request" (:reason (body r))))))
    (is (zero? @writes))))

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

(deftest caller-cannot-backdate-a-release
  (let [writes (atom 0)]
    (with-redefs-fn
      {#'reader/read-inputs (fn [& _] (inputs))
       #'futon3c.transport.http/promise-release-grant-id (fn [& _] "act:grant")
       #'history/record! (fn [& _] (swap! writes inc))}
      #(let [r ((handler) {:request-method :post :uri "/api/alpha/promise/release"
                           :body (json/generate-string
                                  {:caller "debtor" :promise-id "p" :role "debtor"
                                   :reason "Cannot proceed"
                                   :at "2026-09-01T00:00:00Z"})})]
         (is (= 400 (:status r)))
         (is (= "caller-supplied-at" (:reason (body r))))))
    (is (zero? @writes))))
