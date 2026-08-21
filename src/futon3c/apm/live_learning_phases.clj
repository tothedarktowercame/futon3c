(ns futon3c.apm.live-learning-phases
  "Live Student/Guide/Scribe/close adapters for the APM map/reduce cycle."
  (:require [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.frame-cycle-handlers :as handlers]
            [futon3c.apm.live-job-driver :as driver]
            [futon3c.apm.live-preflight-runtime :as runtime])
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
           snapshot-access]}]
  (let [kind (:kind action)
        phase (:phase action)
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
                   (and (= :student-attempt kind) promotion-receipt
                        (not (and (:ok snapshot-access)
                                  (= (:receipt/snapshot-digest promotion-receipt)
                                     (get-in snapshot-access
                                             [:snapshot :snapshot/digest])))))
                   (conj :student-snapshot-access-unverified))]
    (if (seq findings)
      {:ok false :error/code :live-learning-request-invalid :findings findings}
      (let [body (cond-> {:dispatch/type kind :phase phase :role role
                          :agent-id (:agent-id seat)
                          :frame-id (:frame/id unit) :problem-id (:problem/id unit)
                          :ledger-digest (:digest ledger)
                          :role-card-path (:path role-card) :role-card-blob (:blob role-card)
                          :input-receipt-ids input-ids :turn-timeout-ms 3600000}
                   (= :student-attempt kind)
                   (assoc :attempt-ordinal (:ordinal action)
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
                   (= :guide-intervention kind)
                   (assoc :intervention-ordinal (:ordinal action)
                          :input-attempt-id
                          (:receipt/id (get receipts
                                           (keyword (str "student-attempt-"
                                                         (:ordinal action)))))))]
        {:ok true :request (assoc body :dispatch/id
                                  (machine/ledger-digest [body]))}))))

(defn validate-terminal [request ticket job]
  (let [kind (:dispatch/type request)
        report (:report job)
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
               (:memory-snapshot request)
               (not= (:memory-snapshot request)
                     (select-keys (:memory-use report)
                                  [:receipt-id :snapshot-id :snapshot-digest])))
          (conj :student-memory-snapshot-mismatch)
          (and (= :guide-intervention kind)
               (not= false (get-in report [:channel-audit :direct-student-contact?])))
          (conj :guide-channel-isolation-unproved)
          (and (= :scribe-reduce kind)
               (not (every? #(coll? (get report %))
                            [:lanes :dispositions :promotion-reviews])))
          (conj :scribe-reduction-evidence-missing)
          (and (= :promote-solver (:phase request))
               (not (and (string? (get-in report [:memory-snapshot :snapshot-id]))
                         (string? (get-in report [:memory-snapshot :snapshot-digest]))
                         (seq (get-in report [:memory-snapshot :reviewed-memory-ids]))
                         (true? (get-in report [:memory-snapshot :independent-review?])))))
          (conj :solver-promotion-snapshot-invalid)
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
  (str "F19 " (name (:phase request)) " — follow frozen role card "
       (:role-card-path request) " at blob " (:role-card-blob request) ".\n"
       "Authority and exact receipt inputs:\n" (pr-str request) "\n"
       (case (:dispatch/type request)
         :student-attempt "Attempt the problem independently. Record memory retrieval/use and an explicit failure account even on success."
         :guide-intervention "Improve only the memory store or harness channel. Do not contact the Student directly."
         :scribe-reduce (if (= :promote-solver (:phase request))
                          "Mine the verified Solver trace, independently review deposits, and publish an immutable content-addressed eligible-memory snapshot for the Student."
                          "Reduce the certified receipts into lanes, dispositions, and promotion reviews.")
         :close-frame "Audit the complete receipt graph and return a content-addressable trace result.")
       " Return exactly one EDN map including :command-own-exit, :frame-id, and :problem-id."))

(defn run-live!
  [{:keys [contract action receipts request state-path agency-base]
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
                          {:agent-id (:agent-id req) :prompt (prompt req)
                           :surface "emacs-repl" :caller "countdown-control"
                           :job-id (:job-id ticket)})]
            {:ok (and (= 202 (:http/status response)) (:ok response)
                      (:accepted response))}))))
    :job-fn
    (fn [job-id]
      (runtime/job->terminal
       (runtime/http-json "GET" (str agency-base "/api/alpha/invoke/jobs/" job-id))))
    :persist-fn #(runtime/atomic-persist! state-path %)
    :terminal-validator validate-terminal
    :receipt-provider (partial receipt contract action receipts)}))
