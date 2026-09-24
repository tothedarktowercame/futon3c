(ns futon3c.agents.kimi-api-test
  "Kimi shares zai-api's agent loop, so what needs proving is the provider
   seam: the endpoint, the model, the session-id prefix, the key resolver and
   the sampling block Kimi accepts."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.agency.followup-queue :as followups]
            [futon3c.agency.registry :as reg]
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

(def ^:private live-target
  "Kimi seats refuse work without a target; this one exists on this machine
   (a-live-target-resolves-on-this-machine)."
  {:work-target "M-autoclock-in"})

(defn- capture-opts
  "Run one turn and return the opts map zai-api handed to chat!."
  [invoke]
  (let [seen (atom nil)]
    (with-redefs [zai/chat! (fn [_client opts _messages]
                              (reset! seen opts)
                              (text-response "done"))]
      (invoke "work" nil live-target))
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
      (let [first-result (invoke "first" nil live-target)]
        (reset! session-id nil)
        (let [second-result (invoke "second" nil live-target)]
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

;; --- Work-target gate ----------------------------------------------------
;; 2026-09-24: every kimi seat kept one conversation across all its dispatches,
;; so kimi-4 opened a job carrying 335k tokens of earlier work and Kimi's
;; 5-hour quota refused it. Joe's rule: work on a kimi seat names its mission,
;; excursion or ticket; when the target changes, the conversation is cleared.

(deftest context-carry-decision-table
  (let [policy {:cap-tokens 5000}
        decide #(:reason (zai/context-carry-decision policy %))]
    (is (= :work-target-required (decide {:job-target nil})))
    (is (= :work-target-unresolved (decide {:job-target "M-nope"
                                            :target-resolved? false})))
    (is (= :fresh (decide {:job-target "M-a" :target-resolved? true
                           :carried-tokens nil})))
    (is (= :target-change (decide {:job-target "T-b" :target-resolved? true
                                   :carried-tokens 10 :context-target "M-a"}))
        "any change clears, however small the conversation")
    (is (= :same-target (decide {:job-target "M-a" :target-resolved? true
                                 :carried-tokens 4999 :context-target "M-a"})))
    (is (= :over-cap (decide {:job-target "M-a" :target-resolved? true
                              :carried-tokens 5000 :context-target "M-a"})))
    (is (= :continuation (decide {:job-target nil :continuation? true
                                  :carried-tokens 4999 :context-target "M-a"}))
        "a bell reply to the seat continues its target without naming one")
    (is (= :over-cap (decide {:job-target nil :continuation? true
                              :carried-tokens 5000 :context-target "M-a"})))))

(deftest work-targets-resolve-only-in-canonical-repos
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "kimi-targets" (make-array java.nio.file.attribute.FileAttribute 0)))
        touch (fn [rel]
                (let [f (java.io.File. root rel)]
                  (.mkdirs (.getParentFile f))
                  (spit f "x")))]
    (touch "futon3c/holes/missions/M-real.md")
    (touch "futon2/holes/E-top.md")
    (touch "futon2/holes/tickets/T-tick.md")
    (touch "futon3c-some-worktree/holes/missions/M-worktree-only.md")
    (let [root (.getPath root)]
      (is (zai/resolve-work-target "M-real" root))
      (is (zai/resolve-work-target "E-top" root))
      (is (zai/resolve-work-target "T-tick" root))
      (is (nil? (zai/resolve-work-target "M-worktree-only" root)))
      (is (nil? (zai/resolve-work-target "M-absent" root)))
      (is (nil? (zai/resolve-work-target "C-campaign" root))
          "campaigns are not work targets")
      (is (nil? (zai/resolve-work-target "M-../../etc/passwd" root))))))

(deftest a-live-target-resolves-on-this-machine
  (is (zai/resolve-work-target "M-autoclock-in")))

(defn- usage-response [text input-tokens]
  (assoc (text-response text)
         :usage {:prompt_tokens input-tokens :completion_tokens 100
                 :total_tokens (+ input-tokens 100)}))

(defn- run-jobs
  "Run each job [prompt invoke-context input-tokens] on INVOKE; the model
   answers with that usage. Returns [results message-vectors-sent]."
  [invoke jobs]
  (let [sent (atom [])
        usage (atom nil)
        results (atom [])]
    (with-redefs [zai/chat! (fn [_client _opts messages]
                              (swap! sent conj messages)
                              (usage-response "done" @usage))
                  zai/resolve-work-target (fn [t] (when (#{"M-a" "E-b" "T-c"} t)
                                                    (str "/holes/" t ".md")))]
      (doseq [[prompt ctx tokens] jobs]
        (reset! usage tokens)
        (swap! results conj (invoke prompt nil ctx))))
    [@results @sent]))

(defn- compactions [store]
  (->> (:order @store)
       (map #(get-in @store [:entries %]))
       (filter #(= :context-compaction (get-in % [:evidence/body :event])))
       (mapv :evidence/body)))

(deftest the-callers-clock-is-the-default-target
  ;; Joe (2026-09-24): the caller clocks in and sends that as the target.
  ;; :inherited-clock is the caller's clock captured when the job was created.
  (let [store (atom {:entries {} :order []})
        invoke (make-invoke {:evidence-store store})
        clocked (fn [clock] {:caller "claude-x" :inherited-clock {:clock clock}})
        [results sent]
        (run-jobs invoke [["first" (clocked {:mission-id "M-a"}) 1000]
                          ["second" (clocked {:mission-id "M-a"}) 1000]
                          ["third" (clocked {:mission-id "M-a" :excursion-id "E-b"}) 1000]
                          ["fourth" (assoc (clocked {:mission-id "M-a"})
                                           :work-target "T-c") 1000]])]
    (is (every? #(= "done" (:result %)) results))
    (is (= [2 4 2 2] (mapv count sent))
        "same clock keeps; the excursion is more specific; a named target beats the clock")
    (is (= [{:reason :target-change :context-target "M-a" :job-target "E-b"
             :target-source :caller-clock}
            {:reason :target-change :context-target "E-b" :job-target "T-c"
             :target-source :dispatch}]
           (mapv #(select-keys % [:reason :context-target :job-target :target-source])
                 (compactions store))))))

(deftest a-campaign-clock-is-not-a-work-target
  (let [invoke (make-invoke {})
        [[result] sent] (run-jobs invoke [["work" {:inherited-clock
                                                   {:clock {:campaign-id "C-x"}}} 1000]])]
    (is (empty? sent))
    (is (str/includes? (:error result) "without a work target"))))

(deftest a-refusal-reminds-the-caller-like-inbox-zero
  (let [f (java.io.File/createTempFile "kimi-followups" ".edn")]
    (.delete f)
    (binding [followups/*path-override* (.getPath f)]
      (followups/clear!)
      (reg/register-agent! {:agent-id {:id/value "claude-kimi-caller" :id/type :continuity}
                            :type :claude
                            :session-id "caller-session-1"
                            :invoke-fn (fn [_ _] {:result "ok"})
                            :capabilities [:explore]})
      (try
        (let [invoke (make-invoke {})]
          (run-jobs invoke [["one" {:caller "claude-kimi-caller"} 1000]
                            ["two" {:caller "claude-kimi-caller"} 1000]])
          (let [queued (get-in (followups/snapshot)
                               [:queued ["claude-kimi-caller" "caller-session-1"]])]
            (is (= 1 (count queued)) "one outstanding reminder per caller session")
            (is (= :kimi-work-target (:type (first queued))))
            (is (str/starts-with? (:prompt (first queued))
                                  "You can't use a Kimi seat without a work target"))))
        (finally
          (reg/deregister-agent! "claude-kimi-caller")
          (followups/clear!)
          (.delete f))))))

(deftest work-without-a-target-is-refused-before-any-model-call
  (let [invoke (make-invoke {})
        [[result] sent] (run-jobs invoke [["work" {:caller "claude-10"} 1000]])]
    (is (empty? sent))
    (is (nil? (:result result)))
    (is (str/includes? (:error result) "without a work target"))))

(deftest an-unknown-target-is-refused
  (let [invoke (make-invoke {})
        [[result] sent] (run-jobs invoke [["work" {:work-target "M-aif-full-loop-70"} 1000]])]
    (is (empty? sent))
    (is (str/includes? (:error result) "M-aif-full-loop-70"))))

(deftest a-target-change-clears-the-conversation
  (let [store (atom {:entries {} :order []})
        invoke (make-invoke {:evidence-store store})
        [_ [first-job second-job third-job]]
        (run-jobs invoke [["first" {:work-target "M-a"} 1000]
                          ["second" {:work-target "M-a"} 1000]
                          ["third" {:work-target "T-c"} 1000]])]
    (is (= [2 4 2] (mapv count [first-job second-job third-job]))
        "same target keeps the exchange; the new target starts from the system message")
    (is (str/starts-with? (:content (last third-job)) "[Context:"))
    (is (str/ends-with? (:content (last third-job)) "third"))
    (is (= [{:reason :target-change :context-target "M-a" :job-target "T-c"}]
           (mapv #(select-keys % [:reason :context-target :job-target])
                 (compactions store))))))

(deftest mission-id-counts-as-the-target
  (let [store (atom {:entries {} :order []})
        invoke (make-invoke {:evidence-store store})
        [_ sent] (run-jobs invoke [["first" {:mission-id "M-a"} 1000]
                                   ["second" {:work-target "E-b"} 1000]])]
    (is (= [2 2] (mapv count sent)))
    (is (= [:target-change] (mapv :reason (compactions store))))))

(deftest a-bell-reply-continues-the-seats-target
  (let [store (atom {:entries {} :order []})
        invoke (make-invoke {:evidence-store store})
        [[_ reply-result] sent]
        (run-jobs invoke [["first" {:work-target "M-a"} 1000]
                          ["reply" {:caller "auto-bellback"} 1000]
                          ["next" {:work-target "M-a"} 1000]])]
    (is (= "done" (:result reply-result)))
    (is (= [2 4 6] (mapv count sent))
        "the reply and the next same-target job both run on the M-a conversation")
    (is (empty? (compactions store)))))

(deftest a-same-target-conversation-past-the-cap-is-cleared
  (let [store (atom {:entries {} :order []})
        invoke (make-invoke {:evidence-store store})
        [_ sent] (run-jobs invoke [["first" {:work-target "M-a"} 130000]
                                   ["second" {:work-target "M-a"} 1000]])]
    (is (= [2 2] (mapv count sent)))
    (is (= [:over-cap] (mapv :reason (compactions store))))))

(deftest zai-seats-need-no-target-and-keep-their-history
  (let [store (atom {:entries {} :order []})
        invoke (zai/make-invoke-fn {:agent-id "zai-test"
                                    :api-key "test-key"
                                    :initial-session-id "zai-sid-test"
                                    :evidence-store store
                                    :memory-mode :none
                                    :cwd "/tmp"})
        [_ sent] (run-jobs invoke [["first" {} 200000]
                                   ["second" {:work-target "M-a"} 200000]])]
    (is (= [2 4] (mapv count sent)))
    (is (empty? (compactions store)))))
