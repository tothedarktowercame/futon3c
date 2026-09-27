(ns futon3c.evidence.origin-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.evidence.origin :as origin]
            [futon3c.evidence.boundary :as boundary]
            [futon3c.evidence.store :as store]
            [futon3c.dev.invoke :as invoke]
            [futon3c.agents.zai-api :as zai]
            [futon3c.transport.http :as http]
            [futon3c.agency.registry :as reg]
            [futon1b-evidence :as f1b]))

(defn memory [] (atom {:entries {} :order []}))
(def base {:subject {:ref/type :agent :ref/id "p6o-test"}
           :type :coordination :claim-type :step :author "joe"
           :body {:event "p6o-probe"}})
(deftest server-writers
  (doseq [[caller surface registered? expected]
          [["joe" "emacs-repl" false :operator]
           ["claude-17" "bell" true :agent]
           ["joe" "parked-resume" false :harness]
           ["nobody" "bell" false :unknown]]]
    (let [db (memory)
          source (origin/source {:caller caller :surface surface :registered-agent? registered?})
          result (binding [origin/*input* source]
                   (invoke/emit-invoke-evidence! db "joe" "invoke-start" {} :session-id "p6o-test"))
          row (store/get-entry* db (:evidence/id result))]
      (is (:ok result))
      (is (= expected (get-in row [:evidence/origin :kind])))
      (is (= "joe" (:evidence/author row)))
      (is (= (:evidence/origin row) (get-in (f1b/build-evidence-doc row) [:doc :evidence/origin]))))))
(deftest harness-cannot-claim-operator
  (let [db (memory)
        operator (:origin (origin/stamp base {:kind :operator :actor "joe"} "bad-old-writer"))
        bad (assoc base :origin operator)
        failure (try (origin/stamp bad (origin/source {:caller "joe" :surface "parked-resume"}) "park")
                     (catch clojure.lang.ExceptionInfo e (ex-data e)))]
    (is (= :origin-conflict (:error/code failure)))
    (binding [origin/*input* (origin/harness "parked-resume" "park-p6o")]
      (#'zai/persist-transcript-safely! "joe" db
       (assoc (#'zai/transcript-entry {:agent-id "joe" :sid "p6o-test" :turn-id "bad"
                                     :profile :kimi :event :turn-start :body {}})
              :evidence/origin operator)))
    (is (empty? (:entries @db)) "The real transcript writer refused the conflicting claim")
    (let [result (boundary/append! db (origin/stamp base (origin/harness "parked-resume" "park-p6o") "park"))]
      (is (:ok result))
      (is (= "joe" (get-in result [:entry :evidence/author])))
      (is (= :harness (get-in result [:entry :evidence/origin :kind]))))))
(deftest transcript-and-unknown-boundary
  (let [db (memory)]
    (doseq [[event expected] [[:turn-start :operator] [:turn-round :agent] [:context-compaction :harness]]]
      (binding [origin/*input* {:kind :operator :actor "joe"}]
        (#'zai/persist-transcript-safely! "kimi-p6o" db
         (#'zai/transcript-entry {:agent-id "kimi-p6o" :sid "p6o-test" :turn-id "turn"
                                 :profile :kimi :event event :body {}})))
      (let [row (get-in @db [:entries (last (:order @db))])]
        (is (= expected (get-in row [:evidence/origin :kind])))
        (is (= "kimi-p6o" (:evidence/author row)))))
    (is (= :unknown (get-in (boundary/append! db base) [:entry :evidence/origin :kind])))))
(deftest dispatch-context-survives-future
  (with-redefs [http/ensure-invoke-jobs-ledger! (fn [] {:jobs {"p6o-job" {:surface "parked-resume"}}})
                reg/get-agent (constantly nil)
                reg/invoke-agent! (fn [_ _ opts]
                                    {:ok true :origin @ (future origin/*input*) :opts opts})]
    (let [r (#'http/invoke-agent-with-session-recovery! "p6o-agent" "not a header" {:caller "joe"} "p6o-job")]
      (is (= :harness (get-in r [:origin :kind])))
      (is (= "p6o-job" (get-in r [:origin :source-id])))
      (is (= (:origin r) (get-in r [:opts :origin]))))))
