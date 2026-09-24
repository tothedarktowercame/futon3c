(ns futon3c.agents.kimi-api-test
  "Kimi shares zai-api's agent loop, so what needs proving is the provider
   seam: the endpoint, the model, the session-id prefix, the key resolver and
   the sampling block Kimi accepts."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.agency.clock-store :as clock-store]
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

(def ^:private requisition
  "Kimi seats refuse calls without a requisition line; M-autoclock-in exists
   on this machine (a-live-target-resolves-on-this-machine)."
  "Requisition: M-autoclock-in — kimi-api-test\n\n")

(defn- capture-opts
  "Run one turn and return the opts map zai-api handed to chat!."
  [invoke]
  (let [seen (atom nil)]
    (with-redefs [zai/chat! (fn [_client opts _messages]
                              (reset! seen opts)
                              (text-response "done"))]
      (invoke (str requisition "work") nil))
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
      (let [first-result (invoke (str requisition "first") nil)]
        (reset! session-id nil)
        (let [second-result (invoke (str requisition "second") nil)]
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

;; --- Requisition gate ----------------------------------------------------
;; 2026-09-24: every kimi seat kept one conversation across all its dispatches,
;; so kimi-4 opened a job carrying 335k tokens of earlier work and Kimi's
;; 5-hour quota refused it. Joe's rule: each call carries a one-line
;; requisition naming its mission, excursion or ticket and a purpose; when the
;; target changes, the conversation is cleared.

(def ^:private wrapped-bell
  ;; The shape a bell prompt has by the time it reaches the seat.
  (str "--- CURRENT TURN ---\nSurface: bell\nFrom: claude-10\nTo: kimi-4\n"
       "Type: request\n---\n\n%s\n\nThe actual request text, which may "
       "mention M-other-mission in passing.\n"))

(deftest requisition-lines-parse-inside-a-wrapped-prompt
  (let [parse #(zai/parse-requisition (format wrapped-bell %))]
    (is (= {:target "M-futon-seams" :purpose "prototype PROOF-2a"}
           (parse "Requisition: M-futon-seams — prototype PROOF-2a")))
    (is (= {:target "T-tick" :purpose "fix it"} (parse "Requisition: T-tick -- fix it")))
    (is (= {:target "E-ex" :purpose "look"} (parse "> Requisition: E-ex - look"))
        "a quoted line still counts")
    (is (= {:target "M-futon-seams" :purpose nil} (parse "Requisition: M-futon-seams")))
    (is (nil? (parse "Please work on M-futon-seams.")) "a mention is not a requisition")
    (is (nil? (zai/parse-requisition "We need a requisition: M-a — x"))
        "only a line that starts with the keyword")
    (is (= {:error :ambiguous :targets ["M-a" "M-b"]}
           (parse "Requisition: M-a — x\nRequisition: M-b — y")))
    (is (= {:target "M-a" :purpose "x"}
           (parse "Requisition: M-a — x\n> Requisition: M-a — x"))
        "a forwarded copy of the same requisition is fine")))

(deftest requisition-decision-table
  (let [known #{"M-a" "E-b" "T-c"}
        decide #(:reason (zai/requisition-decision (assoc % :resolve-fn known)))]
    (is (= :requisition-required (decide {:requisition nil})))
    (is (= :continuation (decide {:requisition nil :continuation? true})))
    (is (= :requisitioned (decide {:requisition {:target "M-a" :purpose "p"}
                                   :continuation? true}))
        "a continuation may still requisition")
    (is (= :requisition-ambiguous (decide {:requisition {:error :ambiguous}})))
    (is (= :requisition-purpose-required (decide {:requisition {:target "M-a"}})))
    (is (= :requisition-unresolved (decide {:requisition {:target "M-nope" :purpose "p"}})))
    (is (= :requisition-unresolved (decide {:requisition {:target "C-camp" :purpose "p"}}))
        "campaigns are not work targets")
    (is (= :requisitioned (decide {:requisition {:target "T-c" :purpose "p"}})))))

(deftest context-carry-decision-table
  (let [policy {:cap-tokens 5000}
        decide #(:reason (zai/context-carry-decision policy %))]
    (is (= :fresh (decide {:job-target "M-a" :carried-tokens nil})))
    (is (= :target-change (decide {:job-target "T-b" :carried-tokens 10
                                   :context-target "M-a"}))
        "any change clears, however small the conversation")
    (is (= :same-target (decide {:job-target "M-a" :carried-tokens 4999
                                 :context-target "M-a"})))
    (is (= :over-cap (decide {:job-target "M-a" :carried-tokens 5000
                              :context-target "M-a"})))
    (is (= :same-target (decide {:job-target nil :carried-tokens 4999
                                 :context-target "M-a"}))
        "a continuation keeps the seat's target")
    (is (= :over-cap (decide {:job-target nil :carried-tokens 5000
                              :context-target "M-a"})))))

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
      (is (nil? (zai/resolve-work-target "C-campaign" root)))
      (is (nil? (zai/resolve-work-target "M-../../etc/passwd" root))))))

(deftest a-live-target-resolves-on-this-machine
  (is (zai/resolve-work-target "M-autoclock-in")))

(defn- usage-response [text input-tokens]
  (assoc (text-response text)
         :usage {:prompt_tokens input-tokens :completion_tokens 100
                 :total_tokens (+ input-tokens 100)}))

(defn- req [target prompt]
  (str "Requisition: " target " — test purpose\n\n" prompt))

(def ^:private !summary-calls (atom []))

(defn- summary-call? [messages]
  (str/starts-with? (str (:content (first messages))) "You are compacting"))

(defn- run-jobs
  "Run each job [prompt invoke-context input-tokens] on INVOKE; the model
   answers with that usage. Summary calls answer \"SUMMARY-TEXT\" (or
   SUMMARY-RESPONSE when given) and are recorded in !summary-calls, not in
   the job messages. Returns [results message-vectors-sent]."
  [invoke jobs & {:keys [summary-response]}]
  (let [sent (atom [])
        usage (atom nil)
        results (atom [])]
    (reset! !summary-calls [])
    (with-redefs [zai/chat! (fn [_client opts messages]
                              (if (summary-call? messages)
                                (do (swap! !summary-calls conj {:opts opts :messages messages})
                                    (or summary-response
                                        (usage-response "SUMMARY-TEXT" 5000)))
                                (do (swap! sent conj messages)
                                    (usage-response "done" @usage))))
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

(deftest a-call-without-a-requisition-is-refused-before-any-model-call
  (let [invoke (make-invoke {})
        [results sent]
        (run-jobs invoke [["work on M-a please" {:caller "claude-10"} 1000]
                          ["work" {:work-target "M-a"} 1000]
                          ["work" {:mission-id "M-a"} 1000]])]
    (is (empty? sent) "a mention, a payload field or a dispatch mission is not a requisition")
    (doseq [r results]
      (is (nil? (:result r)))
      (is (str/includes? (:error r) "without a requisition")))))

(deftest the-refusal-suggests-the-callers-clock
  (let [invoke (make-invoke {})
        [[clocked unclocked] _]
        (run-jobs invoke [["work" {:inherited-clock {:clock {:mission-id "M-a"}}} 1000]
                          ["work" {} 1000]])]
    (is (str/includes? (:error clocked) "your clock says M-a"))
    (is (str/includes? (:error clocked) "Requisition: M-a — <purpose>"))
    (is (str/includes? (:error unclocked) "You are not clocked in"))))

(deftest a-requisition-without-purpose-or-for-an-unknown-target-is-refused
  (let [invoke (make-invoke {})
        [[no-purpose unknown] sent]
        (run-jobs invoke [["Requisition: M-a\n\nwork" {} 1000]
                          [(req "M-aif-full-loop-70" "work") {} 1000]])]
    (is (empty? sent))
    (is (str/includes? (:error no-purpose) "has no purpose"))
    (is (str/includes? (:error unknown) "M-aif-full-loop-70"))))

(deftest a-target-change-clears-the-conversation
  (let [store (atom {:entries {} :order []})
        invoke (make-invoke {:evidence-store store})
        [_ [first-job second-job third-job]]
        (run-jobs invoke [[(req "M-a" "first") {} 1000]
                          [(req "M-a" "second") {} 1000]
                          [(req "T-c" "third") {} 1000]])]
    (is (= [2 4 2] (mapv count [first-job second-job third-job]))
        "same target keeps the exchange; the new target starts from the system message")
    (is (str/starts-with? (:content (last third-job)) "[Context:"))
    (is (str/includes? (:content (last third-job))
                       "[Summary of the earlier conversation]\nSUMMARY-TEXT\n[End of summary]"))
    (is (str/ends-with? (:content (last third-job)) "third"))
    (is (= 1 (count @!summary-calls)))
    (is (= [{:reason :target-change :context-target "M-a" :job-target "T-c"
             :purpose "test purpose" :method :summary :summary-chars 12}]
           (mapv #(select-keys % [:reason :context-target :job-target :purpose
                                  :method :summary-chars])
                 (compactions store))))))

(deftest the-summary-call-sends-a-plain-transcript-without-tools
  (let [invoke (make-invoke {})
        big (apply str (repeat 10000 "x"))]
    (with-redefs [zai/chat! (fn [_ opts messages]
                              (if (summary-call? messages)
                                (do (swap! !summary-calls conj {:opts opts :messages messages})
                                    (usage-response "SUMMARY-TEXT" 5000))
                                (if (= "tool" (:role (last messages)))
                                  (usage-response "done" 1000)
                                  (assoc-in (usage-response "" 1000)
                                            [:choices 0 :message :tool_calls]
                                            [{:id "c1" :type "function"
                                              :function {:name "read_file"
                                                         :arguments "{\"path\":\"/tmp/x\"}"}}]))))
                  zai/resolve-work-target (fn [t] (when (#{"M-a" "T-c"} t) t))]
      (with-redefs-fn {#'zai/execute-tool
                       (fn [& _]
                         {:detail {} :result {:ok true}
                          :message {:role "tool" :tool_call_id "c1"
                                    :name "read_file" :content big}})}
        (fn []
          (reset! !summary-calls [])
          (invoke (req "M-a" "read a big file") nil {})
          (invoke (req "T-c" "something else") nil {}))))
    (let [{:keys [opts messages]} (first @!summary-calls)
          transcript (:content (second messages))]
      (is (true? (:no-tools? opts)) "the summary must answer in text")
      (is (= 2 (count messages)) "instruction plus one plain-text transcript")
      (is (str/includes? (:content (first messages)) "different target (T-c)"))
      (is (str/includes? transcript "[tool call read_file]"))
      (is (str/includes? transcript "chars elided") "old tool output is micro-compacted")
      (is (< (count transcript) 5000)))))

(deftest a-failed-summary-falls-back-to-a-clear
  (let [store (atom {:entries {} :order []})
        invoke (make-invoke {:evidence-store store})
        [_ [_ second-job]]
        (run-jobs invoke [[(req "M-a" "first") {} 1000]
                          [(req "T-c" "second") {} 1000]]
                  :summary-response {:error {:message "HTTP 403"}})]
    (is (= 2 (count second-job)))
    (is (str/includes? (:content (last second-job)) "cleared (summary failed: "))
    (is (not (str/includes? (:content (last second-job)) "[Summary of")))
    (is (= [:clear] (mapv :method (compactions store))))))

(deftest turn-start-records-the-requisition
  (let [store (atom {:entries {} :order []})
        invoke (make-invoke {:evidence-store store})]
    (run-jobs invoke [[(req "E-b" "go") {} 1000]])
    (is (= [{:reason :requisitioned :target "E-b" :purpose "test purpose"}]
           (->> (:order @store)
                (map #(get-in @store [:entries % :evidence/body]))
                (filter #(= :turn-start (:event %)))
                (mapv :requisition))))))

(deftest a-bell-reply-continues-the-seats-target
  (let [store (atom {:entries {} :order []})
        invoke (make-invoke {:evidence-store store})
        [[_ reply-result] sent]
        (run-jobs invoke [[(req "M-a" "first") {} 1000]
                          ["reply" {:caller "auto-bellback"} 1000]
                          [(req "M-a" "next") {} 1000]])]
    (is (= "done" (:result reply-result)))
    (is (= [2 4 6] (mapv count sent))
        "the reply and the next same-target job both run on the M-a conversation")
    (is (empty? (compactions store)))))

(deftest a-same-target-conversation-past-the-cap-is-cleared
  (let [store (atom {:entries {} :order []})
        invoke (make-invoke {:evidence-store store})
        [_ sent] (run-jobs invoke [[(req "M-a" "first") {} 600000]
                                   [(req "M-a" "second") {} 1000]])]
    (is (= [2 2] (mapv count sent)))
    (is (str/includes? (get-in (first @!summary-calls) [:messages 0 :content])
                       "continues the same target"))
    (is (= [[:over-cap :summary]] (mapv (juxt :reason :method) (compactions store))))))

(deftest the-kimi-cap-leaves-room-for-long-jobs
  ;; k3 and kimi-for-coding report context_length 1048576 (2026-09-24).
  (is (= 512000 (:cap-tokens kimi/default-context-policy))))

(defn- with-caller [agent-id session f]
  (let [file (java.io.File/createTempFile "kimi-followups" ".edn")]
    (.delete file)
    (binding [followups/*path-override* (.getPath file)]
      (followups/clear!)
      (reg/register-agent! {:agent-id {:id/value agent-id :id/type :continuity}
                            :type :claude
                            :session-id session
                            :invoke-fn (fn [_ _] {:result "ok"})
                            :capabilities [:explore]})
      (try
        (f #(get-in (followups/snapshot) [:queued [agent-id session]]))
        (finally
          (reg/deregister-agent! agent-id)
          (clock-store/reset-store!)
          (followups/clear!)
          (.delete file))))))

(deftest a-refusal-reminds-the-caller-like-inbox-zero
  (with-caller "claude-kimi-caller" "caller-session-1"
    (fn [queued]
      (run-jobs (make-invoke {}) [["one" {:caller "claude-kimi-caller"} 1000]
                                  ["two" {:caller "claude-kimi-caller"} 1000]])
      (is (= 1 (count (queued))) "one outstanding reminder per caller session")
      (is (= :kimi-work-target (:type (first (queued)))))
      (is (str/starts-with? (:prompt (first (queued)))
                            "You can't use a Kimi seat without a requisition")))))

(deftest a-requisition-off-the-callers-clock-reminds-it-to-reclock
  (with-caller "claude-kimi-caller" "caller-session-2"
    (fn [queued]
      ;; The reminder reads the caller's clock when the job starts; the
      ;; job's inherited clock (a snapshot from creation) is stale here.
      (clock-store/set-dispatch-mission! "claude-kimi-caller" "caller-session-2" "M-a")
      (let [ctx {:caller "claude-kimi-caller"
                 :inherited-clock {:clock {:mission-id "M-stale"}}}]
        (run-jobs (make-invoke {}) [[(req "M-a" "on the clock") ctx 1000]
                                    [(req "T-c" "off the clock") ctx 1000]
                                    [(req "T-c" "again") ctx 1000]])
        (is (= 1 (count (queued))) "none for the clocked target; one per session for the other")
        (is (str/starts-with? (:prompt (first (queued))) "You requisitioned kimi-test for T-c"))
        (is (str/includes? (:prompt (first (queued))) "this reminder clocks you onto it"))
        (is (not (str/includes? (:prompt (first (queued))) "M-a"))
            "naming the caller's clock too would make the delivered turn :ambiguous and unclock it")))))

(deftest zai-seats-need-no-requisition-and-keep-their-history
  (let [store (atom {:entries {} :order []})
        invoke (zai/make-invoke-fn {:agent-id "zai-test"
                                    :api-key "test-key"
                                    :initial-session-id "zai-sid-test"
                                    :evidence-store store
                                    :memory-mode :none
                                    :cwd "/tmp"})
        [_ sent] (run-jobs invoke [["first" {} 200000]
                                   [(req "M-a" "second") {} 200000]])]
    (is (= [2 4] (mapv count sent)))
    (is (empty? (compactions store)))))
