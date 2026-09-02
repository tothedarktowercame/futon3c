(ns futon3c.agents.zai-memory-tool-contract-test
  "U7: R2 observation and R16 action-witness contracts for ZAI memory tools."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [futon3c.agents.zai-api :as zai]
            [futon3c.peripheral.tools :as tools]))

(def paired-memory-tools
  {"memory_search" {:tags ["u7-no-such-tag"] :limit 1}
   "memory_read" {:evidence_id "e-u7-no-such-evidence"}
   "tool_history" {}
   "evidence_graph" {:mode "reply-chain"
                     :evidence_id "e-u7-no-such-evidence"
                     :limit 1}
   "pattern_memory" {:tags ["u7-no-such-tag"] :limit 1}
   "recent_coordination" {:scope "jobs" :limit 1}
   "mission_context" {:target "M-u7-no-such-mission" :limit 1}})

(defn- registered-read-only-memory-tools
  "Derive U7's tool set from zai-api's actual family and tool registries."
  []
  (let [registered (set (map :name @#'zai/tool-specs))]
    (->> (disj (set @#'zai/memory-family-tool-names)
               ;; memory_record is the family's one WRITE tool; every
               ;; read-only member must carry an R2/R16 pair here.
               "memory_record")
         (filter registered)
         set)))

(defn- assert-paired!
  [registered paired]
  (assert (= registered paired)
          (str "Every registered read-only memory tool needs an R2/R16 pair; "
               "unpaired=" (pr-str (sort (remove paired registered)))
               ", stale-pairs=" (pr-str (sort (remove registered paired))))))

(defn- tool-call
  [tool args]
  {:id (str "tc-u7-" tool)
   :type "function"
   :function {:name tool :arguments (json/generate-string args)}})

(defn- execute
  [tool args store]
  (#'zai/execute-tool
   (tools/make-mock-backend)
   {:agent-id "zai-u7"
    :session-id-atom (atom "sid-u7")
    :session-id-fallback "sid-u7"
    :evidence-store store
    :dispatch-id "dispatch-u7"
    :turn-id "turn-u7"
    :round 1
    :profile :zaif}
   (tool-call tool args)))

(defn- typed-envelope?
  [executed]
  (let [result (:result executed)
        envelope (:result result)]
    (and (true? (:ok result))
         (map? envelope)
         (string? (:frame envelope))
         (map? (:query envelope))
         (vector? (:items envelope)))))

(defn- response-with-tool-call
  [tool args]
  {:choices [{:message {:role "assistant"
                        :content ""
                        :tool_calls [(tool-call tool args)]}}]})

(defn- final-response
  []
  {:choices [{:message {:role "assistant" :content "done"}}]})

(defn- r16-witness
  [tool args]
  (let [store (atom {:entries {} :order []})
        responses (atom [(response-with-tool-call tool args)
                         (final-response)])
        invoke (zai/make-invoke-fn
                {:agent-id "zai-u7"
                 :api-key "test-key"
                 :initial-session-id "sid-u7"
                 :evidence-store store
                 :memory-mode :full
                 :profile :zaif
                 :cwd "/home/joe/code/futon3c"})]
    (with-redefs [zai/chat! (fn [& _]
                              (let [response (first @responses)]
                                (swap! responses subvec 1)
                                response))]
      (is (= "done" (:result (invoke "exercise memory tool" nil
                                     {:dispatch-id "dispatch-u7"}))))
      (->> (:order @store)
           (map #(get-in @store [:entries %]))
           (filter #(and (= :turn-round (get-in % [:evidence/body :event]))
                         (seq (get-in % [:evidence/body :calls]))))
           first))))

(defmacro def-memory-tool-pair
  [stem tool args]
  (let [r2-name (symbol (str stem "-returns-r2-typed-envelope"))
        r16-name (symbol (str stem "-records-r16-query-witness"))]
    `(do
       (deftest ~r2-name
         (let [executed# (execute ~tool ~args
                                  (atom {:entries {} :order []}))]
           (is (typed-envelope? executed#)
               (str ~tool " returned an untyped absence: "
                    (pr-str (:result executed#))))
           (is (some? (get-in executed# [:message :content])))))

       (deftest ~r16-name
         (let [witness# (r16-witness ~tool ~args)
               call# (get-in witness# [:evidence/body :calls 0])]
           (is (map? witness#) (str ~tool " produced no R16 witness"))
           (is (= "zai-u7" (:evidence/author witness#)))
           (is (string? (:evidence/at witness#)))
           (is (= ~tool (:tool call#)))
           (is (= ~args (edn/read-string (:args call#)))))))))

(def-memory-tool-pair "memory-search" "memory_search"
  {:tags ["u7-no-such-tag"] :limit 1})

(def-memory-tool-pair "memory-read" "memory_read"
  {:evidence_id "e-u7-no-such-evidence"})

(def-memory-tool-pair "tool-history" "tool_history" {})

(def-memory-tool-pair "evidence-graph" "evidence_graph"
  {:mode "reply-chain" :evidence_id "e-u7-no-such-evidence" :limit 1})

(def-memory-tool-pair "pattern-memory" "pattern_memory"
  {:tags ["u7-no-such-tag"] :limit 1})

(def-memory-tool-pair "recent-coordination" "recent_coordination"
  {:scope "jobs" :limit 1})

(def-memory-tool-pair "mission-context" "mission_context"
  {:target "M-u7-no-such-mission" :limit 1})

(deftest registry-pins-one-r2-r16-pair-per-read-only-memory-tool
  (let [registered (registered-read-only-memory-tools)
        paired (set (keys paired-memory-tools))]
    (is (= paired registered))
    (is (thrown-with-msg?
         AssertionError #"unpaired=\(\"future_memory\"\)"
         (assert-paired! (conj registered "future_memory") paired))
        "The pin must fail when a new registry tool has no test pair")))
