(ns futon3c.apm.live-learning-phases
  "Live Student/Guide/Scribe/close adapters for the APM map/reduce cycle."
  (:require [clojure.string :as str]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.frame-cycle-handlers :as handlers]
            [futon3c.apm.live-job-driver :as driver]
            [futon3c.apm.live-preflight-runtime :as runtime]
            [futon3c.apm.typed-role-submission :as submission])
  (:import [java.util UUID]))

(def role-for-kind
  {:student-attempt :student :guide-intervention :guide
   :scribe-reduce :scribe :close-frame :guide})

(defn- producer-phase [contract artifact]
  (some (fn [phase]
          (when (contains? (get-in contract [:phases phase :produces]) artifact)
            phase))
        (:phase-order contract)))

(defn required-input-receipt-ids [contract phase receipts]
  (->> (get-in contract [:phases phase :requires])
       (map #(producer-phase contract %))
       distinct
       (keep #(get-in receipts [% :receipt/id]))
       set))

(defn build-request
  [{:keys [contract action ledger unit role-card seat workspace receipts
           snapshot-access turn-timeout-ms]
    :or {turn-timeout-ms 3600000}}]
  (let [kind (:kind action)
        phase (:phase action)
        attempt-ordinal (or (:ordinal action)
                            ({:student-attempt-1 1
                              :student-attempt-2 2
                              :student-attempt-3 3} phase))
        role (role-for-kind kind)
        expected-agent (str (:frame/id unit) "-" (name role))
        input-ids (required-input-receipt-ids contract phase receipts)
        promotion-receipt (get receipts :promote-solver)
        required-artifacts (get-in contract [:phases phase :requires])
        expected-input-count (count (distinct (map #(producer-phase contract %)
                                                   required-artifacts)))
        findings (cond-> []
                   (nil? role) (conj :learning-kind-invalid)
                   (not= (:frame/id unit) (:frame-id action)) (conj :frame-mismatch)
                   (not= (:problem/id unit) (:problem-id action)) (conj :problem-mismatch)
                   (not= expected-agent (:agent-id seat)) (conj :seat-mismatch)
                   (not (true? (:invoke-ready? seat))) (conj :seat-not-ready)
                   (not (and (string? (:path role-card)) (string? (:blob role-card))))
                   (conj :role-card-pin-missing)
                   (not= expected-input-count (count input-ids))
                   (conj :required-input-receipts-missing)
                   (and (= :student-attempt kind)
                        (not (string? (:workspace/path workspace))))
                   (conj :student-workspace-missing)
                   (and (= :student-attempt kind)
                        (not (contains? #{1 2 3} attempt-ordinal)))
                   (conj :student-attempt-ordinal-missing)
                   (and (= :student-attempt kind) promotion-receipt
                        (not (and (:ok snapshot-access)
                                  (= (:receipt/snapshot-digest promotion-receipt)
                                     (get-in snapshot-access
                                             [:snapshot :snapshot/digest])))))
                   (conj :student-snapshot-access-unverified))
        findings (cond-> findings
                   (and (= :scribe-reduce kind) (= :promote-solver phase)
                        (not (and (string? (get-in unit [:problem :blob]))
                                  (string? (get-in unit [:problem :path]))
                                  (string? (:receipt/final-head
                                            (get receipts :solve))))))
                   (conj :promotion-residual-inputs-missing))]
    (if (seq findings)
      {:ok false :error/code :live-learning-request-invalid :findings findings}
      (let [body (cond-> {:dispatch/type kind :phase phase :role role
                          :agent-id (:agent-id seat)
                          :frame-id (:frame/id unit) :problem-id (:problem/id unit)
                          :ledger-digest (:digest ledger)
                          :role-card-path (:path role-card) :role-card-blob (:blob role-card)
                          :input-receipt-ids input-ids
                          :turn-timeout-ms turn-timeout-ms}
                   (= :student-attempt kind)
                   (assoc :attempt-ordinal attempt-ordinal
                          :workspace (:workspace/path workspace)
                          :fresh-session? true
                          :fresh-session-nonce (str (UUID/randomUUID)))
                   (and (= :student-attempt kind) promotion-receipt)
                   (assoc :memory-snapshot
                          {:receipt-id (:receipt/id promotion-receipt)
                           :snapshot-id (:receipt/snapshot-id promotion-receipt)
                           :snapshot-digest (:receipt/snapshot-digest promotion-receipt)
                           :accessible-memory-ids
                           (vec (sort (:accessible-memory-ids snapshot-access)))})
                   (and (= :scribe-reduce kind) (= :promote-solver phase))
                   (assoc :base-problem-blob (get-in unit [:problem :blob])
                          :problem-path (get-in unit [:problem :path])
                          :solver-final-head
                          (:receipt/final-head (get receipts :solve)))
                   (= :guide-intervention kind)
                   (assoc :intervention-ordinal (:ordinal action)
                          :input-attempt-id
                          (:receipt/id (get receipts
                                           (keyword (str "student-attempt-"
                                                         (:ordinal action)))))))]
        {:ok true :request (submission/prepare-request
                            (assoc body :dispatch/id
                                   (machine/ledger-digest [body])))}))))

(defn validate-terminal [request ticket job]
  (let [kind (:dispatch/type request)
        report (:report job)
        memory-use (:memory-use report)
        snapshot-binding (select-keys memory-use
                                      [:receipt-id :snapshot-id
                                       :snapshot-digest])
        allowed-memory-ids (set (get-in request
                                        [:memory-snapshot
                                         :accessible-memory-ids]))
        surfaced-memory-ids (set (:surfaced-ids memory-use))
        used-memory-ids (set (:used-ids memory-use))
        findings
        (cond-> []
          (not= (:job-id ticket) (:job-id job)) (conj :job-id-mismatch)
          (not= (:agent-id request) (:agent-id job)) (conj :agent-id-mismatch)
          (not= :done (:state job)) (conj :job-not-done)
          (not= 0 (:command-own-exit report)) (conj :command-own-exit-nonzero)
          (not= (:frame-id request) (:frame-id report)) (conj :frame-mismatch)
          (not= (:problem-id request) (:problem-id report)) (conj :problem-mismatch)
          (and (= :student-attempt kind)
               (not (string? (:session-id job)))) (conj :fresh-session-id-missing)
          (and (= :student-attempt kind)
               (not (map? (:memory-use report)))) (conj :memory-use-evidence-missing)
          (and (= :student-attempt kind)
               (map? memory-use)
               (not (and (vector? (:surfaced-ids memory-use))
                         (vector? (:used-ids memory-use))
                         (vector? (:queries memory-use))
                         (every? string? (:queries memory-use)))))
          (conj :student-memory-use-ids-invalid)
          (and (= :student-attempt kind)
               (:memory-snapshot request)
               (not= (:memory-snapshot request)
                     (assoc snapshot-binding
                            :accessible-memory-ids
                            (get-in request [:memory-snapshot
                                             :accessible-memory-ids]))))
          (conj :student-memory-snapshot-mismatch)
          (and (= :student-attempt kind)
               (map? memory-use)
               (not (every? allowed-memory-ids surfaced-memory-ids)))
          (conj :student-memory-surfaced-outside-snapshot)
          (and (= :student-attempt kind)
               (map? memory-use)
               (not (every? surfaced-memory-ids used-memory-ids)))
          (conj :student-memory-used-without-surfacing)
          (and (= :guide-intervention kind)
               (not= false (get-in report [:channel-audit :direct-student-contact?])))
          (conj :guide-channel-isolation-unproved)
          (and (= :scribe-reduce kind)
               (not (every? #(coll? (get report %))
                            [:lanes :dispositions :promotion-reviews])))
          (conj :scribe-reduction-evidence-missing)
          (and (= :promote-solver (:phase request))
               (not (and (vector? (:memory-candidates report))
                         (seq (:memory-candidates report)))))
          (conj :solver-promotion-candidates-invalid)
          (and (= :close-frame kind)
               (not (and (string? (:trace-id report))
                         (= :closed (:result report)))))
          (conj :close-evidence-invalid))]
    (if (seq findings)
      {:ok false :error/code :live-learning-terminal-invalid :findings findings}
      {:ok true :report report})))

(defn receipt [contract action receipts request ticket job validated]
  (let [kind (:kind action)
        report (:report validated)
        common {:receipt/frame-id (:frame-id request)
                :receipt/problem-id (:problem-id request)}
        body
        (merge common
               (case kind
                 :student-attempt
                 {:receipt/type :student-attempt
                  :receipt/attempt-ordinal (:attempt-ordinal request)
                  :receipt/fresh-session-id (:session-id job)
                  :receipt/job-id (:job-id ticket)
                  :receipt/outcome (:outcome report)
                  :receipt/failure-account (:failure-account report)
                  :receipt/memory-use (:memory-use report)
                  :receipt/memory-snapshot (:memory-snapshot request)}
                 :guide-intervention
                 {:receipt/type :guide-intervention
                  :receipt/intervention-ordinal (:intervention-ordinal request)
                  :receipt/mode (:mode report)
                  :receipt/input-attempt-id (:input-attempt-id request)
                  :receipt/effect (:effect report)
                  :receipt/channel-audit (:channel-audit report)}
                 :scribe-reduce
                 (if (= :promote-solver (:phase action))
                   {:receipt/type :solver-promotion
                    :receipt/input-receipt-ids (:input-receipt-ids request)
                    :receipt/lanes (:lanes report)
                    :receipt/dispositions (:dispositions report)
                    :receipt/promotion-reviews (:promotion-reviews report)
                    :receipt/snapshot-id (get-in report [:memory-snapshot :snapshot-id])
                    :receipt/snapshot-digest
                    (get-in report [:memory-snapshot :snapshot-digest])
                    :receipt/snapshot-path
                    (get-in report [:memory-snapshot :snapshot-path])
                    :receipt/reviewed-memory-ids
                    (get-in report [:memory-snapshot :reviewed-memory-ids])
                    :receipt/independent-review? true}
                   {:receipt/type :scribe-reduce
                    :receipt/input-receipt-ids (:input-receipt-ids request)
                    :receipt/lanes (:lanes report)
                    :receipt/dispositions (:dispositions report)
                    :receipt/promotion-reviews (:promotion-reviews report)})
                 :close-frame
                 {:receipt/type :frame-close
                  :receipt/input-receipt-ids (:input-receipt-ids request)
                  :receipt/trace-id (:trace-id report)
                  :receipt/result (:result report)}))
        addressed (assoc body :receipt/id (machine/ledger-digest [body]))]
    (handlers/validate-completion contract action addressed receipts)))

(defn prompt [request]
  (str (str/upper-case (:frame-id request)) " " (name (:phase request))
       " — follow frozen role card "
       (:role-card-path request) " at blob " (:role-card-blob request) ".\n"
       "Authority and exact receipt inputs:\n" (pr-str request) "\n"
       (case (:dispatch/type request)
         :student-attempt
         (str "Attempt the problem independently. The :memory-snapshot map is "
              "the complete memory authority: do not query, read, or use any "
              "memory ID absent from :accessible-memory-ids. Return :memory-use "
              "with the exact :receipt-id, :snapshot-id, and :snapshot-digest "
              "from the request, plus vector-valued :surfaced-ids and :used-ids. "
              "Also return vector-valued :queries containing the exact search "
              "strings used (an empty vector means no query was run). "
              "Record an explicit failure account even on success.")
         :guide-intervention "Improve only the memory store or harness channel. Do not contact the Student directly."
         :scribe-reduce (if (= :promote-solver (:phase request))
                          (str "Mine the verified Solver trace and return memory "
                               "candidates plus all four typed lane entries. Each "
                               "lane is {:lane KEYWORD :status :ran|:ran-empty|:not-run}; "
                               "empty or unrun lanes require a nonblank :reason. "
                               "The controller owns independent review and snapshot publication.")
                          "Reduce the certified receipts into lanes, dispositions, and promotion reviews.")
         :close-frame "Audit the complete receipt graph and return a content-addressable trace result.")
       (if-let [job-id (:submission/job-id request)]
         (str " Completion is accepted only through the typed submission tool; "
              "follow the shared completion contract "
              (pr-str submission/completion-contract) ". "
              "conversational output is never a receipt. Run the template command, "
              "fill every null in the generated JSON, then run the submit command:\n"
              (submission/command request {:job-id job-id})
              "\nFix any field-level errors before ending the turn.")
         " Await activation before submitting completion.")))

(defn terminal-repair-request
  "Create the sole authority-preserving repair dispatch for an invalid typed
  role terminal. The rejected findings become durable request data."
  [request ticket job failure]
  (let [contract-migration?
        (= :typed-submission-contract-migration (:repair/kind failure))
        migration-nonce (when contract-migration?
                          (machine/ledger-digest
                           [(:dispatch/id request) (:ticket/id ticket)
                            (:job-id job) submission/completion-contract]))
        body (-> request
                 (dissoc :dispatch/id)
                 (assoc :fresh-session? contract-migration?
                        :repair/attempt (if contract-migration?
                                          :typed-contract-migration-1
                                          1)
                        :repair/of-job-id (:job-id job)
                        :repair/of-ticket-id (:ticket/id ticket)
                        :repair/findings (vec (:findings failure)))
                 (cond-> contract-migration?
                   (assoc :fresh-session-nonce migration-nonce
                          :repair/kind :typed-submission-contract-migration)))]
    {:ok true :request (submission/prepare-request
                        (assoc body :dispatch/id
                               (machine/ledger-digest [body])))}))

(defn run-live!
  [{:keys [contract action receipts request state-path agency-base
           snapshot-publish-fn]
    :or {agency-base "http://localhost:7070"}}]
  (driver/drive!
   {:request request :state (runtime/read-state state-path)
    :announce-fn
    (fn [req]
      (let [response (runtime/http-json
                      "POST" (str agency-base "/api/alpha/invoke/announce")
                      {:agent-id (:agent-id req) :prompt (prompt req)
                       :surface "emacs-repl" :caller "countdown-control"})]
        {:ok (and (= 202 (:http/status response)) (:ok response))
         :job-id (:job-id response)}))
    :activate-fn
    (fn [req ticket]
      (let [reset-response (when (:fresh-session? req)
                             (runtime/http-json
                              "POST" (str agency-base "/api/alpha/agents/"
                                          (:agent-id req) "/reset-session") {}))
            reset-ok? (or (nil? reset-response)
                          (and (= 200 (:http/status reset-response))
                               (:ok reset-response)))]
        (if-not reset-ok?
          {:ok false :error/code :student-session-reset-failed}
          (let [response (runtime/http-json
                          "POST" (str agency-base "/api/alpha/invoke/activate")
                          {:agent-id (:agent-id req)
                           :prompt (prompt (assoc req :submission/job-id
                                                 (:job-id ticket)))
                           :surface "emacs-repl" :caller "countdown-control"
                           :job-id (:job-id ticket)})]
            {:ok (and (= 202 (:http/status response)) (:ok response)
                      (:accepted response))}))))
    :job-fn
    (fn [job-id]
      (runtime/job->terminal
       (runtime/http-json "GET" (str agency-base "/api/alpha/invoke/jobs/" job-id))))
    :persist-fn #(runtime/atomic-persist! state-path %)
    :ticket-register-fn submission/register!
    :terminal-submission-provider (fn [_ ticket _]
                                    (submission/submitted (:job-id ticket)))
    :terminal-validator validate-terminal
    :terminal-repair-request-fn terminal-repair-request
    :receipt-provider
    (fn [request ticket job validated]
      (if (= :promote-solver (:phase action))
        (if-not (fn? snapshot-publish-fn)
          {:ok false :error/code :solver-snapshot-publisher-missing}
          (let [published (snapshot-publish-fn (:report validated))]
            (if-not (:ok published)
              published
              (let [snap (:snapshot published)
                    report (assoc (:report validated) :memory-snapshot
                                  {:snapshot-id (:snapshot/id snap)
                                   :snapshot-digest (:snapshot/digest snap)
                                   :snapshot-path (:path published)
                                   :reviewed-memory-ids
                                   (mapv :memory-id (:snapshot/memories snap))
                                   :independent-review? true})]
                (receipt contract action receipts request ticket job
                         (assoc validated :report report))))))
        (receipt contract action receipts request ticket job validated)))}))
