(ns futon3c.social.coordination-ledger-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.agency.registry :as reg]
            [futon3c.evidence.store :as estore]
            [futon3c.social.coordination-ledger :as ledger]
            [futon3c.transport.http :as http]))

(use-fixtures :each
  (fn [f]
    (estore/reset-store!)
    (binding [ledger/*test-evidence-store* estore/!store]
      (f))))

(defn- mesh-entries []
  (estore/query {:query/type :coordination
                 :query/tags [:coordination :mesh-edge]
                 :query/limit 20}))

(deftest record-invoke-edge-appends-mesh-evidence
  (let [receipt (ledger/record-invoke-edge! {:from "claude-1"
                                             :to "codex-1"
                                             :surface "dispatch"
                                             :kind :invoke})
        [entry] (mesh-entries)
        body (:evidence/body entry)]
    (is (:ok receipt))
    (is (= :coordination (:evidence/type entry)))
    (is (= #{:coordination :mesh-edge} (set (:evidence/tags entry))))
    (is (= {:edge/kind :invoke
            :edge/from "claude-1"
            :edge/to "codex-1"
            :edge/surface "dispatch"}
           (select-keys body [:edge/kind :edge/from :edge/to :edge/surface])))))

(deftest invoke-with-edge-records-invoke-and-result
  (let [calls (atom [])
        expected {:ok true :result "done" :session-id "s1"}]
    (with-redefs [reg/invoke-agent! (fn [to prompt timeout-ms]
                                      (swap! calls conj [to prompt timeout-ms])
                                      expected)]
      (is (= expected
             (ledger/invoke-with-edge! {:from "irc-user"
                                        :to "codex-2"
                                        :surface "irc"
                                        :prompt "do it"
                                        :timeout-ms 1234}))))
    (is (= ["codex-2" "do it"] (subvec (first @calls) 0 2)))
    (is (= {:timeout-ms 1234 :surface "irc"}
           (select-keys (nth (first @calls) 2) [:timeout-ms :surface])))
    (is (= (get-in (first (mesh-entries)) [:evidence/body :edge/id])
           (:turn-id (nth (first @calls) 2))))
    (let [kinds (mapv (comp :edge/kind :evidence/body) (reverse (mesh-entries)))]
      (is (= [:invoke :invoke-result] kinds)))))

(deftest blank-caller-is-unknown-not-dropped
  (ledger/record-invoke-edge! {:from " " :to "claude-6" :surface "whistle"})
  (let [[entry] (mesh-entries)]
    (is (= "unknown" (:evidence/author entry)))
    (is (= "unknown" (get-in entry [:evidence/body :edge/from])))))

(deftest scheduled-entrypoint-requires-and-records-r10-linked-dispatch
  ;; Live pin: Agency invoke record invoke-1788708049924-13300-38749afe
  ;; carried this commission identity on 2026-09-06 (PA11z exemplar).
  (let [commission-id "PA11z-library-annotator-exemplar"
        dispatch-id "invoke-1788708049924-13300-38749afe"
        commission {:commission/id commission-id :commission/from "claude-2"
                    :commission/to "codex-18"}
        result (ledger/run-scheduled-dispatch!
                {:commission commission
                 :dispatch-fn (fn [linked]
                                {:node (:node linked)
                                 :commission/id (:commission/id linked)
                                 :dispatch/id dispatch-id})})
        [entry] (estore/query {:query/type :coordination
                               :query/tags [:coordination :scheduled-dispatch :R10]
                               :query/limit 10})]
    (is (= :R10 (get-in result [:commission :node])))
    (is (= dispatch-id (get-in result [:receipt :dispatch/id])))
    (is (= {:node :R10 :process/stage :dispatched
            :commission/id commission-id}
           (select-keys (:evidence/body entry)
                        [:node :process/stage :commission/id])))
    (is (= dispatch-id (get-in entry [:evidence/body :dispatch/receipt :dispatch/id])))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"R10 scheduled dispatch receipt"
         (ledger/run-scheduled-dispatch!
          {:commission commission
           :dispatch-fn (constantly {:dispatch/id "unlinked"})})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"R10 scheduled dispatch receipt"
         (ledger/run-scheduled-dispatch!
          {:commission commission :dispatch-fn (constantly nil)})))))

(deftest coordination-edges-http-route-returns-public-view
  (ledger/record-invoke-edge! {:from "claude-1" :to "codex-1" :surface "dispatch"})
  (let [handler (http/make-handler {})
        response (handler {:request-method :get
                           :uri "/api/alpha/coordination/edges"
                           :query-string "limit=10"})
        body (json/parse-string (:body response) true)
        [edge] (:edges body)]
    (is (= 200 (:status response)))
    (is (= true (:ok body)))
    (is (= 1 (:count body)))
    (is (= "claude-1" (:from edge)))
    (is (= "codex-1" (:to edge)))
    (is (= "dispatch" (:surface edge)))))
