(ns futon3c.apm.live-proof-phases
  "Live request and terminal-receipt adapters for preflight, solve, and verify."
  (:require [clojure.set :as set]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.frame-cycle-contract :as cycle]
            [futon3c.apm.live-job-driver :as driver]
            [futon3c.apm.live-preflight :as preflight]
            [futon3c.apm.live-preflight-runtime :as runtime]))

(def permitted-axioms '#{propext Classical.choice Quot.sound})

(def proof-report-fields
  #{:command-own-exit :branch :base-revision :final-head :committed?
    :statement-unchanged? :lean :axioms :clean-before? :clean-after? :mutations})

(defn- address-request [body]
  (assoc body :dispatch/id (machine/ledger-digest [body])))

(defn build-request
  [{:keys [kind action ledger unit role-card seat workspace solve-receipt]}]
  (if (= :preflight kind)
    (preflight/build-request {:ledger ledger :unit unit :role-card role-card
                              :seat seat :timeouts (:timeouts action)})
    (let [problem (:problem unit)
          expected-role (if (= :solve kind) :solver :proctor)
          expected-agent (str (:frame/id unit) "-" (name expected-role))
          findings (cond-> []
                     (not (contains? #{:solve :verify} kind)) (conj :proof-kind-invalid)
                     (not= (:frame/id unit) (:frame-id action)) (conj :frame-mismatch)
                     (not= (:problem/id unit) (:problem-id action)) (conj :problem-mismatch)
                     (not= expected-agent (:agent-id seat)) (conj :seat-mismatch)
                     (not (true? (:invoke-ready? seat))) (conj :seat-not-ready)
                     (not= (:revision problem) (:base-revision workspace))
                     (conj :workspace-base-mismatch)
                     (not= (:blob problem) (:problem/blob workspace))
                     (conj :workspace-blob-mismatch)
                     (not (and (string? (:path role-card)) (string? (:blob role-card))))
                     (conj :role-card-pin-missing)
                     (and (= :verify kind) (not (string? (:receipt/id solve-receipt))))
                     (conj :solve-receipt-missing))]
      (if (seq findings)
        {:ok false :error/code :live-proof-request-invalid :findings findings}
        {:ok true
         :request
         (address-request
          (cond-> {:dispatch/type (keyword (str "frame-" (name kind)))
                   :phase kind :agent-id (:agent-id seat)
                   :frame-id (:frame/id unit) :problem-id (:problem/id unit)
                   :ledger-digest (:digest ledger)
                   :role-card-path (:path role-card) :role-card-blob (:blob role-card)
                   :workspace (:workspace/path workspace)
                   :branch (:branch workspace) :base-revision (:revision problem)
                   :problem-path (:path problem) :problem-blob (:blob problem)
                   :turn-timeout-ms (get-in action [:timeouts :turn-ms])}
            (= :verify kind) (assoc :solve-receipt-id (:receipt/id solve-receipt)
                                    :certified-final-head
                                    (:receipt/final-head solve-receipt))))}))))

(defn validate-terminal [kind request ticket job]
  (let [report (:report job)
        missing (set/difference proof-report-fields (set (keys report)))
        lean (:lean report)
        findings
        (cond-> []
          (not= (:job-id ticket) (:job-id job)) (conj :job-id-mismatch)
          (not= (:agent-id request) (:agent-id job)) (conj :agent-id-mismatch)
          (not= :done (:state job)) (conj :job-not-done)
          (seq missing) (conj :proof-report-fields-missing)
          (not= 0 (:command-own-exit report)) (conj :command-own-exit-nonzero)
          (not= (:branch request) (:branch report)) (conj :branch-mismatch)
          (not= (:base-revision request) (:base-revision report))
          (conj :base-revision-mismatch)
          (not (true? (:committed? report))) (conj :final-head-not-committed)
          (not (and (string? (:final-head report))
                    (re-matches #"[0-9a-f]{40}" (:final-head report))))
          (conj :final-head-invalid)
          (and (= :verify kind)
               (not= (:certified-final-head request) (:final-head report)))
          (conj :verify-final-head-mismatch)
          (not (true? (:statement-unchanged? report))) (conj :statement-changed)
          (not= {:exit 0 :warnings 0 :sorry-warnings 0 :errors 0}
                (select-keys lean [:exit :warnings :sorry-warnings :errors]))
          (conj :lean-proof-invalid)
          (not (set/subset? (set (:axioms report)) permitted-axioms))
          (conj :axioms-not-permitted)
          (not (true? (:clean-before? report))) (conj :workspace-not-clean-before)
          (not (true? (:clean-after? report))) (conj :workspace-not-clean-after)
          (seq (:mutations report)) (conj :uncommitted-mutations-observed))]
    (if (seq findings)
      {:ok false :error/code :live-proof-terminal-invalid
       :findings findings :missing missing}
      {:ok true :report report})))

(defn receipt [contract kind request ticket _job validated]
  (let [report (:report validated)
        body (case kind
               :solve {:receipt/type :frame-solve
                       :receipt/frame-id (:frame-id request)
                       :receipt/problem-id (:problem-id request)
                       :receipt/job-id (:job-id ticket)
                       :receipt/final-head (:final-head report)
                       :receipt/lean (:lean report)
                       :receipt/axioms (:axioms report)
                       :receipt/statement-unchanged? true}
               :verify {:receipt/type :frame-verify
                        :receipt/frame-id (:frame-id request)
                        :receipt/problem-id (:problem-id request)
                        :receipt/job-id (:job-id ticket)
                        :receipt/solve-receipt-id (:solve-receipt-id request)
                        :receipt/final-head (:final-head report)
                        :receipt/mathematical-sound? true})
        addressed (assoc body :receipt/id (machine/ledger-digest [body]))
        checked (cycle/validate-receipt contract kind addressed)]
    (if (:ok checked) {:ok true :certificate addressed} checked)))

(defn drive!
  [{:keys [kind contract request] :as options}]
  (if (= :preflight kind)
    (driver/drive!
     (assoc (select-keys options [:state :announce-fn :activate-fn :job-fn :persist-fn])
            :request request
            :terminal-validator preflight/validate-terminal
            :receipt-provider (fn [r t j _] (preflight/receipt contract r t j))))
    (driver/drive!
     (assoc (select-keys options [:state :announce-fn :activate-fn :job-fn :persist-fn])
            :request request
            :terminal-validator (partial validate-terminal kind)
            :receipt-provider (partial receipt contract kind)))))

(defn prompt [request]
  (str "F19 " (name (:phase request)) " — use only this frozen dispatch authority:\n"
       (pr-str request) "\n"
       (case (:phase request)
         :solve "Work in the registered solver workspace. Commit the completed proof."
         :verify "Independently verify the certified solver head; do not mutate it."
         "Perform the registered read-only preflight.")
       " Return exactly one EDN map with keys " (pr-str proof-report-fields) "."))

(defn run-live!
  [{:keys [kind contract request state-path agency-base]
    :or {agency-base "http://localhost:7070"}}]
  (drive!
   {:kind kind :contract contract :request request
    :state (runtime/read-state state-path)
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
      (let [response (runtime/http-json
                      "POST" (str agency-base "/api/alpha/invoke/activate")
                      {:agent-id (:agent-id req) :prompt (prompt req)
                       :surface "emacs-repl" :caller "countdown-control"
                       :job-id (:job-id ticket)})]
        {:ok (and (= 202 (:http/status response)) (:ok response)
                  (:accepted response))}))
    :job-fn
    (fn [job-id]
      (runtime/job->terminal
       (runtime/http-json "GET" (str agency-base "/api/alpha/invoke/jobs/" job-id))))
    :persist-fn #(runtime/atomic-persist! state-path %)}))
