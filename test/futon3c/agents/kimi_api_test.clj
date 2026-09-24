(ns futon3c.agents.kimi-api-test
  "Kimi shares zai-api's agent loop, so what needs proving is the provider
   seam: the endpoint, the model, the session-id prefix, the key resolver and
   the sampling block Kimi accepts."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.agency.clock-store :as clock]
            [futon3c.agents.kimi-api :as kimi]
            [futon3c.agents.zai-api :as zai]))

(defn- text-response [text]
  {:choices [{:message {:role "assistant" :content text}}]})

(defn- make-invoke [opts]
  (kimi/make-invoke-fn
   (merge {:agent-id "kimi-test"
           :api-key "test-key"
           :initial-session-id "kimi-sid-test"
           :evidence-store (atom {:entries {} :order []})
           :memory-mode :none
           :cwd "/tmp"}
          opts)))

(defn- capture-opts
  "Run one turn and return the opts map zai-api handed to chat!."
  [invoke]
  (let [seen (atom nil)]
    (with-redefs [zai/chat! (fn [_client opts _messages]
                              (reset! seen opts)
                              (text-response "done"))]
      (invoke "work" nil))
    @seen))

(deftest kimi-turns-address-the-kimi-coding-endpoint
  (let [opts (capture-opts (make-invoke {}))]
    (is (= kimi/default-base-url (:base-url opts)))
    (is (= kimi/default-model (:model opts)))
    (is (= "KIMI" (:env-prefix opts)))))

(deftest kimi-omits-temperature-because-kimi-refuses-it
  ;; Live 2026-09-23: any explicit temperature returns
  ;; "invalid temperature: only 0.6 is allowed for this model". nil here means
  ;; the field is left out of the request body altogether.
  (let [opts (capture-opts (make-invoke {}))]
    (is (nil? (get-in opts [:sampling :temperature])))
    (is (contains? (:sampling opts) :temperature))
    (is (nil? (get-in opts [:sampling :thinking])))
    (is (= "low" (get-in opts [:sampling :reasoning-effort])))))

(deftest zai-sampling-is-unchanged-by-the-provider-seam
  (let [invoke (zai/make-invoke-fn {:agent-id "zai-test"
                                    :api-key "test-key"
                                    :initial-session-id "zai-sid-test"
                                    :evidence-store (atom {:entries {} :order []})
                                    :memory-mode :none
                                    :cwd "/tmp"})
        opts (capture-opts invoke)]
    (is (= 0.2 (get-in opts [:sampling :temperature])))
    (is (= {:type "disabled"} (get-in opts [:sampling :thinking])))
    (is (= "none" (get-in opts [:sampling :reasoning-effort])))))

(deftest caller-supplied-model-wins-over-the-kimi-default
  (let [opts (capture-opts (make-invoke {:model "kimi-for-coding"}))]
    (is (= "kimi-for-coding" (:model opts)))))

(deftest fresh-kimi-sessions-are-prefixed-kimi
  (let [session-id (atom "kimi-old")
        invoke (make-invoke {:initial-session-id "kimi-old"
                             :session-id-atom session-id})]
    (with-redefs [zai/chat! (fn [_ _ _] (text-response "done"))]
      (let [first-result (invoke "first" nil)]
        (reset! session-id nil)
        (let [second-result (invoke "second" nil)]
          (is (= "kimi-old" (:session-id first-result)))
          (is (str/starts-with? (:session-id second-result) "kimi-"))
          (is (not= (:session-id first-result) (:session-id second-result))))))))

(deftest a-keyless-kimi-seat-refuses-instead-of-borrowing-the-zai-key
  ;; The hazard this guards: falling through to zai-api/resolve-api-key would
  ;; run a Kimi seat on the Z.AI subscription and look like it worked.
  ;; The key is resolved at construction as well as per turn, so the redefs
  ;; have to cover BOTH — a seat built outside them reads the real ~/.kimikey
  ;; and calls the live endpoint.
  (with-redefs [kimi/resolve-api-key (constantly nil)
                zai/resolve-api-key (constantly "zai-key-must-not-be-used")]
    (let [invoke (kimi/make-invoke-fn
                  {:agent-id "kimi-test"
                   :initial-session-id "kimi-sid-test"
                   :evidence-store (atom {:entries {} :order []})
                   :memory-mode :none
                   :cwd "/tmp"})
          result (invoke "work" nil)]
      (is (nil? (:result result)))
      (is (= kimi/api-key-hint (:error result))))))

;; --- Context-carry gate --------------------------------------------------
;; 2026-09-24: every kimi seat kept one conversation across all its dispatches,
;; so kimi-4 opened a M-futon-seams job carrying 335k tokens of other missions'
;; history and Kimi's 5-hour quota refused it. These pin that a job runs on
;; carried history only when it is clocked to the mission that history is for.

(deftest context-carry-decision-table
  (let [policy {:floor-tokens 1000 :cap-tokens 5000}
        decide #(:reason (zai/context-carry-decision policy %))]
    (is (= :below-floor (decide {:carried-tokens nil :job-mission nil})))
    (is (= :below-floor (decide {:carried-tokens 999 :job-mission nil
                                 :context-mission "M-a"})))
    (is (= :unclocked-job (decide {:carried-tokens 1000 :job-mission nil
                                   :context-mission "M-a"})))
    (is (= :unclocked-job (decide {:carried-tokens 1000 :job-mission nil
                                   :context-mission nil}))
        "two unclocked jobs are not the same mission")
    (is (= :mission-change (decide {:carried-tokens 1000 :job-mission "M-b"
                                    :context-mission "M-a"})))
    (is (= :mission-change (decide {:carried-tokens 1000 :job-mission "M-b"
                                    :context-mission nil})))
    (is (= :same-mission (decide {:carried-tokens 4999 :job-mission "M-a"
                                  :context-mission "M-a"})))
    (is (= :over-cap (decide {:carried-tokens 5000 :job-mission "M-a"
                              :context-mission "M-a"})))))

(defn- usage-response [text input-tokens]
  (assoc (text-response text)
         :usage {:prompt_tokens input-tokens :completion_tokens 100
                 :total_tokens (+ input-tokens 100)}))

(defn- run-jobs
  "Run each job [prompt mission input-tokens] on INVOKE. The model answers
   with the given usage. Returns the message vectors the model was sent."
  [invoke jobs]
  (let [sent (atom [])
        usage (atom nil)]
    (with-redefs [zai/chat! (fn [_client _opts messages]
                              (swap! sent conj messages)
                              (usage-response "done" @usage))]
      (doseq [[prompt mission tokens] jobs]
        (reset! usage tokens)
        (invoke prompt nil (cond-> {} mission (assoc :mission-id mission)))))
    @sent))

(defn- compactions [store]
  (->> (:order @store)
       (map #(get-in @store [:entries %]))
       (filter #(= :context-compaction (get-in % [:evidence/body :event])))
       (mapv :evidence/body)))

(deftest a-mission-change-clears-carried-history
  (let [store (atom {:entries {} :order []})
        invoke (make-invoke {:evidence-store store})
        [_ second-job] (run-jobs invoke [["first" "M-a" 200000]
                                         ["second" "M-b" 200000]])]
    (is (= 2 (count second-job))
        "system message plus the new job only; the M-a exchange is gone")
    (is (str/starts-with? (:content (last second-job)) "[Context:"))
    (is (str/ends-with? (:content (last second-job)) "second"))
    (is (= [{:reason :mission-change :carried-tokens 200100
             :context-mission "M-a" :job-mission "M-b"}]
           (mapv #(select-keys % [:reason :carried-tokens
                                  :context-mission :job-mission])
                 (compactions store))))))

(deftest the-same-mission-keeps-history-under-the-cap
  (let [store (atom {:entries {} :order []})
        invoke (make-invoke {:evidence-store store})
        [first-job second-job] (run-jobs invoke [["first" "M-a" 60000]
                                                 ["second" "M-a" 60000]])]
    (is (= 2 (count first-job)))
    (is (= 4 (count second-job))
        "system, first prompt, first answer, second prompt")
    (is (= "second" (:content (last second-job))))
    (is (empty? (compactions store)))))

(deftest the-same-mission-is-cleared-over-the-cap
  (let [store (atom {:entries {} :order []})
        invoke (make-invoke {:evidence-store store})
        [_ second-job] (run-jobs invoke [["first" "M-a" 130000]
                                         ["second" "M-a" 1000]])]
    (is (= 2 (count second-job)))
    (is (= [:over-cap] (mapv :reason (compactions store))))))

(deftest an-unclocked-job-starts-fresh
  (let [store (atom {:entries {} :order []})
        invoke (make-invoke {:evidence-store store})
        [_ second-job] (run-jobs invoke [["first" "M-a" 60000]
                                         ["second" nil 60000]])]
    (is (= 2 (count second-job)))
    (is (= [:unclocked-job] (mapv :reason (compactions store))))))

(deftest small-carried-history-is-kept-across-missions
  (let [store (atom {:entries {} :order []})
        invoke (make-invoke {:evidence-store store})
        [_ second-job] (run-jobs invoke [["first" "M-a" 5000]
                                         ["second" "M-b" 5000]])]
    (is (= 4 (count second-job)))
    (is (empty? (compactions store)))))

(deftest the-gate-reads-the-job-clock-from-the-clock-store
  ;; Live kimi jobs carry no :mission-id in the invoke context; their clock
  ;; decision is projected to clock-store at admission and the harness reads
  ;; it there. Drive the gate through that path, not the dispatch field.
  (clock/reset-store!)
  (try
    (let [store (atom {:entries {} :order []})
          invoke (make-invoke {:evidence-store store})
          sent (atom [])
          clock! (fn [n mission]
                   (clock/set-decision!
                    "kimi-test" "kimi-sid-test"
                    {:decision-id (str "d" n)
                     :decided-at (str "2026-09-24T00:00:0" n "Z")
                     :clock {:mission-id mission}}))]
      (with-redefs [zai/chat! (fn [_ _ messages]
                                (swap! sent conj messages)
                                (usage-response "done" 60000))]
        (clock! 1 "M-a")
        (invoke "first" nil)
        (clock! 2 "M-a")
        (invoke "second" nil)
        (clock! 3 "M-b")
        (invoke "third" nil))
      (is (= [2 4 2] (mapv count @sent)))
      (is (= [{:reason :mission-change :context-mission "M-a"
               :job-mission "M-b"}]
             (mapv #(select-keys % [:reason :context-mission :job-mission])
                   (compactions store)))))
    (finally
      (clock/reset-store!))))

(deftest zai-seats-without-a-policy-keep-their-history
  (let [store (atom {:entries {} :order []})
        invoke (zai/make-invoke-fn {:agent-id "zai-test"
                                    :api-key "test-key"
                                    :initial-session-id "zai-sid-test"
                                    :evidence-store store
                                    :memory-mode :none
                                    :cwd "/tmp"})
        [_ second-job] (run-jobs invoke [["first" "M-a" 200000]
                                         ["second" "M-b" 200000]])]
    (is (= 4 (count second-job)))
    (is (empty? (compactions store)))))
