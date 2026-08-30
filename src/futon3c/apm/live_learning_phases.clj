(ns futon3c.apm.live-learning-phases
  "Live Student/Guide/Scribe/close adapters for the APM map/reduce cycle."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.java.shell :as shell]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.frame-cycle-handlers :as handlers]
            [futon3c.apm.live-job-driver :as driver]
            [futon3c.apm.job-port :as job-port]
            [futon3c.apm.live-preflight-runtime :as runtime]
            [futon3c.apm.memory-access-gate :as access-gate]
            [futon3c.apm.promotion-pipeline :as pipeline]
            [futon3c.apm.role-memory-search :as role-memory]
            [futon3c.apm.typed-role-submission :as submission]
            [futon3c.apm.workspace-lifecycle :as workspace-lifecycle])
  (:import [java.nio.charset StandardCharsets]
           [java.nio.file CopyOption Files OpenOption Path StandardCopyOption
            StandardOpenOption]
           [java.nio.file.attribute FileAttribute]
           [java.util UUID]))

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

(defn- cascade-request
  "EXCLUDE-IDS are never offered, whichever route reaches them: the attempt's
   withheld shelf entries (same-problem holdout, amendment 8) would otherwise
   return through the sibling route, since they sit on the seeds' own
   patterns. :holdout-excluded counts what the exclusion removed, so the
   receipt's holdout and cascade records agree."
  [config seed-ids exclude-ids authority cascade-fn cascade-readers cascade-readers-fn]
  (let [started (System/nanoTime)]
    (try
      (let [readers (if (fn? cascade-readers-fn)
                      (cascade-readers-fn)
                      cascade-readers)
            options (cond-> (or readers {})
                      (some? (:cap config)) (assoc :cap (:cap config))
                      (some? (:routes config)) (assoc :routes
                                                      (set (:routes config)))
                      (seq exclude-ids) (assoc :exclude (set exclude-ids)))
            expanded (cascade-fn seed-ids options)
            offers0 (->> (:routes expanded)
                        (remove #(= :leaf (get-in % [1 :route])))
                        (mapv (fn [[memory-id {:keys [route hops pattern]}]]
                                {:memory-id memory-id
                                 :route route
                                 :hops hops
                                 :pattern pattern
                                 :pattern-hook
                                 (or (get-in expanded
                                             [:pattern-surfaces pattern
                                              :offer/pattern-hook])
                                     (get-in expanded
                                             [:pattern-surfaces pattern
                                              :pattern/hook])
                                     (get-in expanded
                                             [:pattern-surfaces pattern :hook])
                                     (get-in expanded
                                             [:pattern-surfaces pattern :entity
                                              :entity/props :hook])
                                     (get-in expanded
                                             [:pattern-surfaces pattern :entity
                                              :props :hook]))
                                 :depositor (get-in expanded
                                                    [:memory-metadata memory-id
                                                     :depositor])
                                 :provenance (get-in expanded
                                                     [:memory-metadata memory-id
                                                      :provenance])})))
            gated (access-gate/enforce-carrier :cascade authority offers0)
            prompt-gated (access-gate/enforce-carrier
                          :prompt-assembly authority (:allowed gated))
            offers (mapv #(dissoc % :depositor :provenance)
                         (:allowed prompt-gated))]
        {:routes-enabled (:routes-enabled expanded)
         :cap (:cap expanded)
         :truncated? (:truncated? expanded)
         :expanded-available (:expanded-available expanded)
         :offers offers
         :holdout-decision (:evidence gated)
         :prompt-holdout-decision (:evidence prompt-gated)
         :histogram (frequencies (map :route offers))
         :holdout-excluded (or (:excluded-offers expanded) 0)
         :expansion-ms (quot (- (System/nanoTime) started) 1000000)})
      (catch Throwable t
        {:error (or (.getMessage t) (.getName (class t)))
         :expansion-ms (quot (- (System/nanoTime) started) 1000000)}))))

(declare memory-use-audit)

(defn build-request
  [{:keys [contract action ledger unit role-card seat seat-role workspace receipts
           snapshot-access student-attempt-inputs turn-timeout-ms terminal-budgets
           cascade-fn cascade-readers cascade-readers-fn authorized-mode]
    :or {turn-timeout-ms 3600000}}]
  (let [kind (:kind action)
        phase (:phase action)
        phase-ordinal (get-in contract [:phases phase :ordinal])
        attempt-ordinal (or (:ordinal action)
                            phase-ordinal
                            ({:student-attempt-1 1
                              :student-attempt-2 2
                              :student-attempt-3 3} phase))
        role (or seat-role (role-for-kind kind))
        expected-agent (str (:frame/id unit) "-" (name role))
        terminal-budget (merge driver/default-terminal-budget
                               (get terminal-budgets role))
        input-ids (required-input-receipt-ids contract phase receipts)
        ;; The Student binds to the latest reviewed snapshot: a Guide's
        ;; union snapshot when one was published, else the Solver promotion.
        promotion-receipt (when (= :student-attempt kind)
                            (handlers/latest-snapshot-receipt
                             receipts (or attempt-ordinal 1)))
        required-artifacts (get-in contract [:phases phase :requires])
        expected-input-count (count (distinct (map #(producer-phase contract %)
                                                   required-artifacts)))
        findings (cond-> []
                   (nil? role) (conj :learning-kind-invalid)
                   (not= (:frame/id unit) (:frame-id action)) (conj :frame-mismatch)
                   (not= (:problem/id unit) (:problem-id action)) (conj :problem-mismatch)
                   (not= expected-agent (:agent-id seat)) (conj :seat-mismatch)
                   (not (true? (:invoke-ready? seat))) (conj :seat-not-ready)
                   (and (some? (:ordinal action))
                        (not= (:ordinal action) phase-ordinal))
                   (conj :action-ordinal-mismatch)
                   (not (and (string? (:path role-card)) (string? (:blob role-card))))
                   (conj :role-card-pin-missing)
                   (not (every? pos-int? (vals terminal-budget)))
                   (conj :terminal-budget-invalid)
                   (not= expected-input-count (count input-ids))
                   (conj :required-input-receipts-missing)
                   (and (= :student-attempt kind)
                        (not (string? (:workspace/path workspace))))
                   (conj :student-workspace-missing)
                   (and (= :student-attempt kind)
                        (not (contains? #{1 2 3} attempt-ordinal)))
                   (conj :student-attempt-ordinal-missing)
                   (and (= :guide-intervention kind)
                        (not (contains? #{:store-mode :harness-mode}
                                        authorized-mode)))
                   (conj :guide-authorized-mode-missing)
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
                   (conj :promotion-residual-inputs-missing))
        snapshot-memories (get-in snapshot-access [:snapshot :snapshot/memories])
        invalid-origin?
        (fn [memory]
          (let [origin (:provenance memory)
                depositor-frame (some->> (:depositor memory)
                                         (re-matches #"^(f[0-9]+)-.+$") second)]
            (not (and (map? origin)
                      (every? #(and (string? %) (not (str/blank? %)))
                              ((juxt :campaign-id :frame-id :problem-id) origin))
                      (= depositor-frame (:frame-id origin))))))
        findings (cond-> findings
                   (and (= :scribe-reduce kind) (= :scribe-reduce phase)
                        (not (and (= 3 (count student-attempt-inputs))
                                  (every? #(and (string? (:job-id %))
                                                (string? (:job-trace-ref %))
                                                (map? (:memory-use %)))
                                          student-attempt-inputs)
                                  (string? (get-in unit [:problem :blob]))
                                  (string? (:receipt/final-head
                                            (get receipts :solve))))))
                   (conj :student-trace-inputs-missing)
                   (and (= :student-attempt kind) (= 1 attempt-ordinal)
                        (some invalid-origin? snapshot-memories))
                   (conj :student-snapshot-provenance-invalid))]
    (cond
      (and (= :student-attempt kind) (nil? promotion-receipt))
      {:ok false
       :error/code :student-memory-snapshot-required
       :findings [:promotion-receipt-missing]
       :phase phase :frame-id (:frame-id action) :problem-id (:problem-id action)}

      (seq findings)
      {:ok false :error/code :live-learning-request-invalid :findings findings}

      :else
      (let [all-accessible-ids (set (:accessible-memory-ids snapshot-access))
            holdout? (and (= :student-attempt kind) (= 1 attempt-ordinal))
            holdout-authority {:problem-id (:problem/id unit)
                               :frame-id (:frame/id unit)
                               :shelf/holdout (when holdout? :same-problem)}
            snapshot-gate (access-gate/enforce-carrier
                           :shelf-materialization holdout-authority
                           (vec snapshot-memories))
            snapshot-prompt-gate (access-gate/enforce-carrier
                                  :prompt-assembly holdout-authority
                                  (:allowed snapshot-gate))
            withheld-ids
            (if holdout?
              (->> (:excluded snapshot-gate)
                   (map :candidate/id)
                   (filter string?)
                   distinct
                   sort
                   vec)
              [])
            accessible-ids (vec (sort (if holdout?
                                        (apply disj all-accessible-ids withheld-ids)
                                        all-accessible-ids)))
            cascade-config (:memory-cascade unit)
            cascade (when (and (= :student-attempt kind)
                               (true? (:enabled? cascade-config)))
                      (cascade-request
                       cascade-config
                       accessible-ids
                       withheld-ids
                       holdout-authority
                       cascade-fn cascade-readers cascade-readers-fn))
            body (cond-> {:dispatch/type kind :phase phase :role role
                          :agent-id (:agent-id seat)
                          :frame-id (:frame/id unit) :problem-id (:problem/id unit)
                          :ledger-digest (:digest ledger)
                          :role-card-path (:path role-card) :role-card-blob (:blob role-card)
                          :input-receipt-ids input-ids
                          :terminal-budget terminal-budget
                          :turn-timeout-ms turn-timeout-ms}
                   (= :student-attempt kind)
                   (assoc :attempt-ordinal attempt-ordinal
                          :workspace (:workspace/path workspace)
                          :fresh-session? true
                          :fresh-session-nonce (str (UUID/randomUUID)))
                   holdout?
                   (assoc :shelf/holdout :same-problem
                          :shelf/withheld-ids withheld-ids
                          :shelf/withheld-count (count withheld-ids))
                   (and holdout? (seq snapshot-memories))
                   (assoc :memory-access-decisions
                          {:shelf-materialization (:evidence snapshot-gate)
                           :prompt-assembly (:evidence snapshot-prompt-gate)})
                   (and (= :student-attempt kind) (some? cascade))
                   (assoc :memory-cascade cascade)
                   ;; The base is what each fresh attempt is reset to and what
                   ;; the archived source is measured against.
                   (and (= :student-attempt kind)
                        (string? (:base-revision workspace)))
                   (assoc :base-revision (:base-revision workspace)
                          :problem-path (:problem/path workspace))
                   (and (= :student-attempt kind) promotion-receipt)
                   (assoc :memory-snapshot
                          {:receipt-id (:receipt/id promotion-receipt)
                           :snapshot-id (:receipt/snapshot-id promotion-receipt)
                           :snapshot-digest (:receipt/snapshot-digest promotion-receipt)
                           :accessible-memory-ids
                           accessible-ids})
                   (and (= :scribe-reduce kind) (= :promote-solver phase))
                   (assoc :base-problem-blob (get-in unit [:problem :blob])
                          :problem-path (get-in unit [:problem :path])
                          :source-attempt-ids
                          [(or (:receipt/job-id (get receipts :solve))
                               (:receipt/id (get receipts :solve)))]
                          :solver-final-head
                          (:receipt/final-head (get receipts :solve)))
                   (and (= :scribe-reduce kind) (= :scribe-reduce phase))
                   (assoc :student-attempts student-attempt-inputs
                          :base-problem-blob (get-in unit [:problem :blob])
                          :problem-path (get-in unit [:problem :path])
                          :solver-final-head
                          (:receipt/final-head (get receipts :solve)))
                   (= :guide-intervention kind)
                   (assoc :mode authorized-mode
                          :intervention-ordinal phase-ordinal
                          :input-attempt-id
                          (:receipt/id (get receipts
                                           (keyword (str "student-attempt-"
                                                         phase-ordinal))))
                          ;; Reviewer inputs for a store-mode deposit: the
                          ;; promotion Proctor card refuses to judge without
                          ;; the base blob and the Solver's final head.
                          :base-problem-blob (get-in unit [:problem :blob])
                          :problem-path (get-in unit [:problem :path])
                          :solver-final-head
                          (:receipt/final-head (get receipts :solve))
                          :prior-snapshot
                          (let [prior (handlers/latest-snapshot-receipt
                                       receipts (inc phase-ordinal))]
                            (some-> (handlers/snapshot-binding prior)
                                    (assoc :snapshot-path
                                           (:receipt/snapshot-path prior)))))
                   (= :close-frame kind)
                   (assoc :memory-use-audit (memory-use-audit receipts)))]
        {:ok true :request (submission/prepare-request
                            (assoc body :dispatch/id
                                   (machine/ledger-digest [body])))}))))

(defn- canonical-close-result [result]
  (cond
    (keyword? result) result
    (string? result) (keyword result)
    :else nil))

(defn memory-use-audit
  "Canonical per-memory close evidence derived from durable Student receipts.
   Memory ids are opaque nonblank strings; no UUID shape is imposed."
  [receipts]
  (->> receipts
       vals
       (filter #(contains? #{:student-attempt
                             :student-observation-missing
                             :student-observation-recovered}
                           (:receipt/type %)))
       (mapcat (fn [receipt]
                 (map (fn [memory-id]
                        {:memory-id memory-id
                         :attempt-ordinal (:receipt/attempt-ordinal receipt)})
                      (get-in receipt [:receipt/memory-use :used-ids]))))
       (group-by :memory-id)
       (map (fn [[memory-id uses]]
              {:memory-id memory-id
               :attempt-ordinals (->> uses (map :attempt-ordinal)
                                      distinct sort vec)}))
       (sort-by :memory-id)
       vec))

(defn- valid-memory-use-audit? [audit]
  (and (vector? audit)
       (every? (fn [entry]
                 (and (= #{:memory-id :attempt-ordinals} (set (keys entry)))
                      (string? (:memory-id entry))
                      (not (str/blank? (:memory-id entry)))
                      (vector? (:attempt-ordinals entry))
                      (seq (:attempt-ordinals entry))
                      (every? pos-int? (:attempt-ordinals entry))))
               audit)))

(defn- controller-memory-use
  [request ticket used-ids]
  (let [predecessor-receipts
        (role-memory/recorded-receipts-for-job (:repair/of-job-id request))
        current-receipts
        (role-memory/recorded-receipts-for-job (:job-id ticket))
        search-receipts (vec (concat predecessor-receipts current-receipts))
        surfaced-ids
        (into (set (get-in request [:memory-snapshot :accessible-memory-ids]))
              (concat
               (mapcat #(role-memory/gated-receipt-surfaced-ids
                         request :inherited-repair %)
                       predecessor-receipts)
               (mapcat role-memory/receipt-surfaced-ids current-receipts)))]
    (merge (:memory-snapshot request)
           {:surfaced-ids (vec (sort surfaced-ids))
            :used-ids (vec used-ids)
            :queries (->> search-receipts
                          (map :query)
                          (filter string?)
                          distinct
                          vec)})))

(defn validate-terminal [request ticket job]
  (let [kind (:dispatch/type request)
        report (:report job)
        submitted-memory-use (:memory-use report)
        current-search-receipts
        (role-memory/recorded-receipts-for-job (:job-id ticket))
        predecessor-search-receipts
        (role-memory/recorded-receipts-for-job (:repair/of-job-id request))
        searched-memory-ids
        (set (concat
              (mapcat #(role-memory/gated-receipt-surfaced-ids
                        request :inherited-repair %)
                      predecessor-search-receipts)
              (mapcat role-memory/receipt-surfaced-ids
                      current-search-receipts)))
        snapshot-memory-ids
        (set (get-in request [:memory-snapshot :accessible-memory-ids]))
        cascade-memory-ids
        (set (map :memory-id (get-in request [:memory-cascade :offers])))
        ;; A withheld id (same-problem holdout, amendment 8) is not citable
        ;; by any channel: not the shelf it was removed from, not a search
        ;; hit on the store, not a cascade offer. Otherwise the holdout is
        ;; only as strong as the weakest channel, and a leak reads as a
        ;; cross-problem use.
        withheld-memory-ids (set (:shelf/withheld-ids request))
        allowed-memory-ids
        (set/difference (into snapshot-memory-ids
                              (concat searched-memory-ids cascade-memory-ids))
                        withheld-memory-ids)
        used-memory-ids (set (:used-ids submitted-memory-use))
        memory-use (controller-memory-use request ticket
                                          (:used-ids submitted-memory-use))
        guide-deposit-validation
        (when (and (= :guide-intervention kind)
                   (some? (:candidates report)))
          (pipeline/validate-guide-deposit
           {:depositor (:agent-id request)
            :candidates (:candidates report)}))
        submitted-mode (submission/wire-keyword (:mode report))
        channel-audit-normalization
        (if (= :guide-intervention kind)
          (submission/normalize-predicate-keys
           (:channel-audit report)
           (->> (get-in (submission/evidence-shape request) [:channel-audit])
                keys
                (filter #(.endsWith ^String (name %) "?"))))
          {:ok true :value (:channel-audit report)})
        channel-audit (:value channel-audit-normalization)
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
               (map? submitted-memory-use)
               (not (and (vector? (:used-ids submitted-memory-use))
                         (every? string? (:used-ids submitted-memory-use)))))
          (conj :student-memory-use-ids-invalid)
          (and (= :student-attempt kind)
               (map? submitted-memory-use)
               (not (every? allowed-memory-ids used-memory-ids)))
          (conj :student-memory-used-without-surfacing)
          (and (= :student-attempt kind)
               (map? submitted-memory-use)
               (seq (set/intersection used-memory-ids withheld-memory-ids)))
          (conj :student-memory-used-despite-holdout)
          (and (= :guide-intervention kind)
               (not (:ok channel-audit-normalization)))
          (conj :wire-predicate-key-conflict)
          (and (= :guide-intervention kind)
               (:ok channel-audit-normalization)
               (not= false (:direct-student-contact? channel-audit)))
          (conj :guide-channel-isolation-unproved)
          (and (= :guide-intervention kind)
               (not= (:mode request) submitted-mode))
          (conj :guide-mode-authority-mismatch)
          ;; Store-mode candidates are the Guide's channel to the Student's
          ;; shelf; they must be gate-shaped here so the reviewer never sees
          ;; an unbound candidate, and harness-mode may not carry any.
          (and (= :guide-intervention kind)
               (some? (:candidates report))
               (not (and (vector? (:candidates report))
                         (:ok guide-deposit-validation))))
          (conj :guide-candidates-invalid)
          (and (= :guide-intervention kind)
               (seq (:candidates report))
               (not= :store-mode submitted-mode))
          (conj :guide-candidates-outside-store-mode)
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
                         (contains? #{:closed :partial}
                                    (canonical-close-result
                                     (:result report)))
                         (valid-memory-use-audit? (:memory-use-audit report))
                         (= (:memory-use-audit request)
                            (:memory-use-audit report)))))
          (conj :close-evidence-invalid))]
    (if (seq findings)
      (cond-> {:ok false :error/code :live-learning-terminal-invalid
               :findings findings}
        (and (some #{:guide-candidates-invalid} findings)
             (seq (:findings guide-deposit-validation)))
        (assoc-in [:finding/details :guide-candidates-invalid]
                  (:findings guide-deposit-validation))
        (some #{:scribe-reduction-evidence-missing} findings)
        (assoc-in [:finding/details :scribe-reduction-evidence-missing]
                  (->> [:lanes :dispositions :promotion-reviews]
                       (remove #(coll? (get report %))) vec))
        (some #{:close-evidence-invalid} findings)
        (assoc-in [:finding/details :close-evidence-invalid]
                  (cond-> []
                    (not (string? (:trace-id report))) (conj :trace-id)
                    (not (contains? #{:closed :partial}
                                    (canonical-close-result (:result report))))
                    (conj :result)
                    (not (valid-memory-use-audit? (:memory-use-audit report)))
                    (conj :memory-use-audit-shape)
                    (and (valid-memory-use-audit? (:memory-use-audit report))
                         (not= (:memory-use-audit request)
                               (:memory-use-audit report)))
                    (conj :memory-use-audit-mismatch))))
      {:ok true :report (cond-> report
                          (= :guide-intervention kind)
                          (assoc :channel-audit channel-audit)
                          (= :student-attempt kind)
                          (assoc :memory-use memory-use))})))

(defn receipt [contract action receipts request ticket job validated]
  (let [kind (:kind action)
        report (:report validated)
        common {:receipt/frame-id (:frame-id request)
                :receipt/problem-id (:problem-id request)}
        body
        (merge common
               (case kind
                 :student-attempt
                 (cond-> {:receipt/type :student-attempt
                          :receipt/attempt-ordinal (:attempt-ordinal request)
                          :receipt/fresh-session-id (:session-id job)
                          :receipt/job-id (:job-id ticket)
                          :receipt/outcome (:outcome report)
                          :receipt/failure-account (:failure-account report)
                          :receipt/memory-use (:memory-use report)
                          :receipt/memory-snapshot
                          (select-keys (:memory-snapshot request)
                                       [:receipt-id :snapshot-id :snapshot-digest])}
                   (= :same-problem (:shelf/holdout request))
                   (assoc :shelf/holdout :same-problem
                          :shelf/withheld-ids (:shelf/withheld-ids request)
                          :shelf/withheld-count (:shelf/withheld-count request))
                   (map? (:memory-cascade request))
                   (assoc :receipt/memory-cascade
                          (let [cascade (:memory-cascade request)
                                used-ids (set (get-in report
                                                     [:memory-use :used-ids]))]
                            (if (vector? (:offers cascade))
                              (-> cascade
                                  (update :offers
                                          #(mapv (fn [offer]
                                                   (dissoc offer :pattern-hook))
                                                 %))
                                  (assoc :used-via-cascade
                                         (->> (:offers cascade)
                                              (filter #(contains? used-ids
                                                                  (:memory-id %)))
                                              (mapv #(select-keys
                                                      % [:memory-id :route
                                                         :pattern])))))
                              cascade)))
                   (map? (:source validated))
                   (assoc :receipt/source (:source validated))
                   (map? (:candidate validated))
                   (assoc :receipt/candidate (:candidate validated)))
                 :guide-intervention
                 (let [snapshot (:memory-snapshot report)]
                   (cond-> {:receipt/type :guide-intervention
                            :receipt/intervention-ordinal
                            (get-in contract [:phases (:phase action) :ordinal])
                            :receipt/mode (:mode request)
                            :receipt/input-attempt-id
                            (:receipt/id
                             (get receipts
                                  (keyword
                                   (str "student-attempt-"
                                        (get-in contract
                                                [:phases (:phase action) :ordinal])))))
                            :receipt/effect (:effect report)
                            :receipt/channel-audit (:channel-audit report)}
                     ;; Present only when store-mode candidates were reviewed
                     ;; and a union snapshot published; the next Student
                     ;; attempt binds to it.
                     (string? (:snapshot-digest snapshot))
                     (assoc :receipt/snapshot-id (:snapshot-id snapshot)
                            :receipt/snapshot-digest (:snapshot-digest snapshot)
                            :receipt/snapshot-path (:snapshot-path snapshot)
                            :receipt/reviewed-memory-ids
                            (:reviewed-memory-ids snapshot)
                            :receipt/promotion-reviews
                            (:promotion-reviews snapshot)
                            :receipt/independent-review? true)))
                 :scribe-reduce
                 (if (= :promote-solver (:phase action))
                   {:receipt/type :solver-promotion
                    :receipt/job-id (:job-id ticket)
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
                    :receipt/job-id (:job-id ticket)
                    :receipt/input-receipt-ids (:input-receipt-ids request)
                    :receipt/lanes (:lanes report)
                    :receipt/dispositions (:dispositions report)
                    :receipt/promotion-reviews (:promotion-reviews report)})
                 :close-frame
                 (let [observation-missing?
                       (some #(= :student-observation-missing (:receipt/type %))
                             (vals receipts))]
                   {:receipt/type :frame-close
                  :receipt/input-receipt-ids (:input-receipt-ids request)
                  :receipt/trace-id (:trace-id report)
                  :receipt/memory-use-audit (:memory-use-audit report)
                  :receipt/result (if observation-missing?
                                    :partial
                                    (canonical-close-result (:result report)))
                  :receipt/learning-outcome (if observation-missing?
                                              :partially-observed
                                              :observed)})))
        addressed (assoc body :receipt/id (machine/ledger-digest [body]))]
    (handlers/validate-completion contract action addressed receipts)))

(defn missing-observation-receipt
  "Controller evidence that a Student job ended without a valid typed receipt.
  It is an alternate observation producer, never a Student-authored attempt."
  ([contract action receipts request ticket job repair-attempts collection-evidence]
   (missing-observation-receipt contract action receipts request ticket job
                                repair-attempts collection-evidence nil))
  ([contract action receipts request ticket job repair-attempts collection-evidence
    archive-fn]
   (missing-observation-receipt contract action receipts request ticket job
                                repair-attempts collection-evidence archive-fn nil))
  ([contract action receipts request ticket job repair-attempts collection-evidence
    archive-fn candidate-fn]
   (missing-observation-receipt contract action receipts request ticket job
                                repair-attempts collection-evidence archive-fn
                                candidate-fn nil))
  ([contract action receipts request ticket job repair-attempts collection-evidence
    archive-fn candidate-fn reset-invalid-fn]
  (let [workspace (:workspace request)
        head-result (when (string? workspace)
                      (shell/sh "git" "-C" workspace "rev-parse" "HEAD"))
        ;; The source is archived even without a typed receipt: an
        ;; unobserved attempt's worktree is still evidence.
        archived (when (fn? archive-fn) (archive-fn))
        candidate (when (fn? candidate-fn) (candidate-fn))
        reset-invalid (when (and candidate (not (:ok candidate))
                                 (fn? reset-invalid-fn))
                        (reset-invalid-fn))
        observation-recovered? (or (:ok archived) (map? candidate))
        body (cond-> {:receipt/type (if observation-recovered?
                                     :student-observation-recovered
                                     :student-observation-missing)
              :receipt/frame-id (:frame-id request)
              :receipt/problem-id (:problem-id request)
              :receipt/attempt-ordinal (:attempt-ordinal request)
              :receipt/job-id (:job-id ticket)
              :receipt/author :controller
              :receipt/reason (if observation-recovered?
                                :typed-submission-collection-failed-but-observation-recovered
                                :typed-submission-missing)
              :receipt/repair-attempts repair-attempts
              :receipt/memory-use (controller-memory-use request ticket [])
              :receipt/memory-snapshot
              (select-keys (:memory-snapshot request)
                           [:receipt-id :snapshot-id :snapshot-digest])
              :receipt/harness-observed
              {:job (select-keys job [:job-id :agent-id :state :terminal-code
                                      :session-id])
               :collection collection-evidence
               :workspace {:path workspace
                           :head (when (and head-result (zero? (:exit head-result)))
                                   (str/trim (:out head-result)))
                           :source (if (:ok archived)
                                     (:source archived)
                                     (some-> archived
                                             (select-keys [:error/code :path])))
                           :candidate (when candidate
                                        (if (:ok candidate)
                                          (:candidate candidate)
                                          (select-keys candidate
                                                       [:error/code :head :ref])))
                           :reset-after-rejection
                           (when reset-invalid
                             (select-keys reset-invalid
                                          [:ok :head :preservation-ref]))}
               :memory {:snapshot (:memory-snapshot request)}}
              :receipt/candidate-disposition
              (cond
                (and candidate (:ok candidate)) :certified
                candidate :rejected-evidence
                :else :absent)}
               (and candidate (:ok candidate))
               (assoc :receipt/candidate (:candidate candidate)))
        addressed (assoc body :receipt/id (machine/ledger-digest [body]))]
    ;; Candidate preservation is evidence collection, not an alternate proof
    ;; validator.  A rejected candidate remains pinned and described under
    ;; :receipt/harness-observed, but must neither be certified under
    ;; :receipt/candidate nor prevent the controller-authored missing
    ;; observation from advancing the frame. When durable attempt evidence
    ;; exists, record recovery rather than claiming the observation was absent.
    (if (and reset-invalid (not (:ok reset-invalid)))
      {:ok false :error/code :student-invalid-candidate-reset-failed
       :candidate (select-keys candidate [:error/code :head :ref])
       :reset reset-invalid}
      (handlers/validate-completion contract action addressed receipts)))))

(defn prepare-student-workspace!
  "Before an original fresh Student attempt, return the Student worktree to
  its registered base so attempt k+1 cannot read attempt k's work. Repairs
  re-dispatch the same attempt and keep the worktree."
  [request reset-fn]
  (cond
    (not (and (= :student-attempt (:dispatch/type request))
              (true? (:fresh-session? request))
              (nil? (:repair/attempt request))))
    {:ok true :status :not-applicable}

    (not (and (string? (:workspace request))
              (string? (:base-revision request))))
    {:ok false :error/code :student-workspace-base-unknown}

    :else
    (let [reset (reset-fn {:workspace/path (:workspace request)
                           :base-revision (:base-revision request)
                           :problem/path (:problem-path request)})]
      (if (:ok reset)
        {:ok true :status :reset :reset reset}
        {:ok false :error/code :student-workspace-reset-failed
         :finding reset}))))

(defn source-archive-directory [state-path phase]
  (str (.resolveSibling (Path/of (str state-path) (make-array String 0))
                        (str (name phase) "-source"))))

(defn packet-archive-path [state-path phase]
  (.resolveSibling (Path/of (str state-path) (make-array String 0))
                   (str (name phase) "-packet.txt")))

(defn archive-rendered-packet!
  "Atomically archive the exact text delivered at activation. Packet archival
  is evidence collection, so an I/O failure is returned for logging rather
  than thrown across the job activation boundary."
  [state-path phase packet]
  (let [target (.toAbsolutePath ^Path (packet-archive-path state-path phase))
        directory (.getParent target)]
    (try
      (Files/createDirectories directory (make-array FileAttribute 0))
      (let [temporary (Files/createTempFile
                       directory (str "." (name phase) "-packet-") ".tmp"
                       (make-array FileAttribute 0))]
        (try
          (Files/writeString
           temporary packet StandardCharsets/UTF_8
           (into-array OpenOption [StandardOpenOption/WRITE
                                   StandardOpenOption/TRUNCATE_EXISTING
                                   StandardOpenOption/SYNC]))
          (Files/move temporary target
                      (into-array CopyOption
                                  [StandardCopyOption/ATOMIC_MOVE
                                   StandardCopyOption/REPLACE_EXISTING]))
          {:ok true :path (str target)}
          (finally
            (Files/deleteIfExists temporary))))
      (catch Throwable t
        {:ok false :error/code :rendered-packet-archive-failed
         :path (str target) :error/message (.getMessage t)}))))

(defn archive-student-source!
  "Archive the Student's problem file beside the phase state before the
  worktree is reset for the next attempt or retired with the frame."
  [request state-path archive-fn]
  (if-not (and (string? (:workspace request))
               (string? (:problem-path request)))
    {:ok false :error/code :student-source-unknown}
    (archive-fn {:workspace/path (:workspace request)
                 :problem/path (:problem-path request)
                 :archive-directory (source-archive-directory
                                     state-path (:phase request))})))

(def repair-finding-instructions
  {:typed-submission-missing
   ["Your completion was rejected because no typed submission was received."
    "Conversational prose is not a receipt; the registered submission record was absent."
    "Use the command below to create, complete, and submit the typed JSON payload."]
   :job-id-mismatch
   ["Your completion named the wrong job." "The submitted job id did not match this dispatch."
    "Regenerate the payload with the command below; do not copy authority fields from another turn."]
   :agent-id-mismatch
   ["Your completion named the wrong agent." "The submitted agent id did not match this role seat."
    "Regenerate the payload below and retain its generated authority fields."]
   :job-not-done
   ["Your completion was submitted before the job was terminal." "Validation observed a non-done job state."
    "Finish the assigned work, then submit the completed payload with the command below."]
   :command-own-exit-nonzero
   ["Your completion reported a failing command." "The command-owned exit code was not zero."
    "Fix the command failure, rerun it, record exit 0, and submit the revised payload below."]
   :frame-mismatch
   ["Your completion named the wrong frame." "The report frame id differed from this dispatch."
    "Use the generated payload below without replacing its frame authority."]
   :problem-mismatch
   ["Your completion named the wrong problem." "The report problem id differed from this dispatch."
    "Use the generated payload below without replacing its problem authority."]
   :fresh-session-id-missing
   ["Your Student completion lacked a fresh session identity." "The terminal job carried no session id."
    "Complete this repair in the active session and submit through the command below."]
   :memory-use-evidence-missing
   ["Your Student completion omitted memory-use evidence." "The report did not contain a memory-use map."
    "Fill memory-use.used-ids with a JSON array (empty if none were used), then submit below."]
   :student-memory-use-ids-invalid
   ["Your memory-use evidence had the wrong shape." "used-ids must be a vector of memory-id strings."
    "Replace used-ids with a JSON array of strings and resubmit below."]
   :student-memory-used-without-surfacing
   ["Your completion cited memory that was not made available." "At least one used id was absent from the controller-authorized shelf and searches."
    "Remove unauthorized ids, retaining only memories actually surfaced to this turn, then submit below."]
   :student-memory-used-despite-holdout
   ["Your completion cited a withheld same-problem memory." "The holdout receipt forbids that memory on this attempt."
    "Remove every withheld id and revise the work without it before submitting below."]
   :guide-channel-isolation-unproved
   ["Your Guide completion did not prove channel isolation." "channel-audit.direct-student-contact? was not explicitly false."
    "Do not contact the Student; set the audit field from the actual conduct and submit below."]
   :wire-predicate-key-conflict
   ["Your completion supplied two spellings of the same predicate field."
    "The trailing-question-mark key and its JSON alias carried different values."
    "Remove the duplicate and submit one truthful value for direct-student-contact?."]
   :guide-candidates-invalid
   ["Your Guide candidates were rejected." "The candidates did not satisfy the typed guide-deposit contract."
    "Correct every candidate to the generated schema and submit a non-empty candidate vector using the command below."]
   :guide-candidates-outside-store-mode
   ["Your Guide completion proposed candidates outside store mode." "Non-empty candidates require mode store-mode."
    "Set mode to store-mode for genuine deposits, otherwise remove the candidates, then submit below."]
   :guide-mode-authority-mismatch
   ["Your Guide completion did not echo its authorized mode."
    "The submitted mode differed from the mode fixed in this dispatch packet."
    "Copy the packet's :mode value exactly into the typed submission; do not choose a mode yourself."]
   :scribe-reduction-evidence-missing
   ["Your Scribe reduction was incomplete." "Lanes, dispositions, or promotion reviews were missing or not collections."
    "Fill all generated reduction collections, including explicit empty dispositions, then submit below."]
   :solver-promotion-candidates-invalid
   ["Your Solver promotion supplied no valid memory candidates." "Promotion requires a non-empty vector of typed candidates."
    "Add at least one schema-valid candidate derived from the verified trace, then submit below."]
   :close-evidence-invalid
   ["Your close-frame evidence was rejected."
    "The trace id, canonical closed-or-partial result, and controller-derived :memory-use-audit must all match the request contract."
    "Supply the checker-bound trace id and result, and copy :memory-use-audit from the request verbatim, then submit below."]})

(def specific-finding-instructions
  {:depositor-missing
   "The deposit's :depositor must be a string naming this Guide seat."
   :candidates-missing
   "The deposit's :candidates must be a non-empty vector."
   :candidate-shape-invalid
   (str "Every candidate must contain string :memory-id and :content-digest, "
        "plus vector :pattern-ids and :source-attempts.")
   :memory-use-audit-shape
   ":memory-use-audit must be a vector in the exact controller-derived request shape."
   :memory-use-audit-mismatch
   ":memory-use-audit must be returned verbatim from the close-frame request."})

(defn- rendered-finding-detail [request finding]
  (let [detail (get-in request [:repair/validation-output
                                :finding/details finding])]
    (cond
      (and (= :guide-candidates-invalid finding) (seq detail))
      (str/join " " (map #(or (specific-finding-instructions %)
                              (str "Unrendered validator finding " (name %) "."))
                           detail))

      (and (= :scribe-reduction-evidence-missing finding) (seq detail))
      (str "Missing or invalid collection fields: "
           (str/join ", " (map name detail)) ".")

      (and (= :close-evidence-invalid finding) (seq detail))
      (str/join " " (map #(or (specific-finding-instructions %)
                                (str "Missing or invalid close field " (name %) "."))
                             detail))

      :else nil)))

(defn repair-instructions [request]
  (when-let [findings (seq (:repair/findings request))]
    (let [missing (remove repair-finding-instructions findings)]
      (when (seq missing)
        (throw (ex-info "Terminal repair finding has no actionable instruction"
                        {:error/code :terminal-repair-instruction-missing
                         :findings (vec missing)})))
      (str "REVISE AND RESUBMIT — THE PREVIOUS COMPLETION WAS REJECTED.\n"
           (str/join
            "\n"
            (map-indexed
             (fn [index finding]
               (let [[rejected why action]
                     (get repair-finding-instructions finding)
                     detail (rendered-finding-detail request finding)]
                 (str (inc index) ". " rejected "\nWHY: " why
                      (when detail (str "\nDETAIL: " detail))
                      "\nDO THIS: " action)))
             findings))
           (when-let [message (get-in request [:repair/validation-output
                                               :report/error :error/message])]
             (str "\nVALIDATOR MESSAGE: " message "\n"))
           "\nCORRECTIVE COMMAND:\n"
           (submission/command request {:job-id (:submission/job-id request)})
           "\nIf the revised typed submission is still invalid, the existing repair budget is exhausted and this role terminal will be rejected.\n\n"))))

(defn prompt [request]
  (str (repair-instructions request)
       (str/upper-case (:frame-id request)) " " (name (:phase request))
       " — follow frozen role card "
       (:role-card-path request) " at blob " (:role-card-blob request) ".\n"
       "Authority and exact receipt inputs:\n" (pr-str request) "\n"
       (case (:dispatch/type request)
         :student-attempt
         (str "Attempt the problem independently. The :memory-snapshot map is "
              "the reviewed starting shelf. You may also use the controller-owned "
              "open mathematics search command; any additionally surfaced memory "
              "is recorded automatically in a job-bound typed receipt. "
              (when (seq (get-in request [:memory-cascade :offers]))
                (str "The :memory-cascade offers are also readable and citable, "
                     "each labelled by the pattern route that reached it. "))
              "Return :memory-use with only vector-valued :used-ids naming "
              "memories actually used (an empty vector means none). Snapshot "
              "binding, surfaced ids, queries, and search receipt ids are "
              "controller-owned and must not be copied into the submission. "
              "Record an explicit failure account even on success.")
         :guide-intervention
         (str "AUTHORIZED MODE: " (name (:mode request)) ". "
              "Echo this exact value as :mode in the typed submission. "
              "Improve only the memory store or harness channel. "
              "Do not contact the Student directly.")
         :scribe-reduce (if (= :promote-solver (:phase request))
                          (str "Mine the verified Solver trace and return memory "
                               "candidates plus all four typed lane entries. Each "
                               "lane is {:lane KEYWORD :status :ran|:ran-empty|:not-run}; "
                               "empty or unrun lanes require a nonblank :reason. "
                               "The controller owns independent review and snapshot publication.")
                          (str "Mine the Student's turns into arc-lane rewrite rules "
                               "and trajectory/challenge memories; return :lanes, "
                               ":dispositions, :promotion-reviews and :memory-candidates."))
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
        findings (vec (:findings failure))
        missing-instructions (vec (remove repair-finding-instructions findings))
        body (-> request
                 (dissoc :dispatch/id)
                 (assoc :fresh-session? contract-migration?
                        :repair/attempt (if contract-migration?
                                          :typed-contract-migration-1
                                          (:repair/next-attempt failure 1))
                        :repair/of-job-id (:job-id job)
                        :repair/of-ticket-id (:ticket/id ticket)
                        :repair/findings findings
                        :repair/fault-origin
                        (or (:repair/fault-origin failure) :agent)
                        :repair/validation-output
                        {:error/code (:error/code failure)
                         :findings findings
                         :finding/details (:finding/details failure)
                         :report/error (:report/error job)})
                 (cond-> contract-migration?
                   (assoc :fresh-session-nonce migration-nonce
                          :repair/kind :typed-submission-contract-migration)))]
    (if (seq missing-instructions)
      {:ok false :error/code :terminal-repair-instruction-missing
       :findings missing-instructions}
      {:ok true :request (submission/prepare-request
                          (assoc body :dispatch/id
                                 (machine/ledger-digest [body])))})))

(defn posthoc-terminal-repair-request
  "Rebuild a post-hoc repair from current campaign authority. A predecessor
  packet that lacked Guide mode is an apparatus fault; other validated role
  failures remain agent faults unless their producer classified them more
  specifically."
  [fresh-request active-request ticket job failure]
  (let [rebuilt (or fresh-request active-request)
        apparatus-packet-defect?
        (and (= :frame-cycle-guide-mode-invalid (:error/code failure))
             (nil? (:mode active-request))
             (contains? #{:store-mode :harness-mode} (:mode rebuilt)))]
    (terminal-repair-request
     rebuilt ticket job
     (cond-> failure
       apparatus-packet-defect?
       (assoc :findings [:guide-mode-authority-mismatch]
              :repair/fault-origin :apparatus)))))

(declare guide-promotion-step!)

(defn run-live!
  [{:keys [contract action receipts request fresh-request state-path agency-base
           snapshot-publish-fn workspace-reset-fn source-archive-fn
           student-candidate-fn preparation guide-promotion]
    :or {agency-base "http://localhost:7070"
         workspace-reset-fn workspace-lifecycle/reset-to-base!
         source-archive-fn workspace-lifecycle/archive-problem-source!
         student-candidate-fn workspace-lifecycle/preserve-student-candidate!}}]
  (driver/drive!
   {:request request :state (runtime/read-state state-path)
    :announce-fn
    (fn [req]
      (let [req (submission/with-job-authority req)
            announced (job-port/announce!
                       agency-base
                       {:agent-id (:agent-id req) :prompt (prompt req)
                        :job-id (:submission/job-id req)})]
        announced))
    :activate-fn
    (fn [req ticket]
      (let [prepared (prepare-student-workspace! req workspace-reset-fn)
            reset-response (when (and (:ok prepared) (:fresh-session? req))
                             (runtime/http-json
                              "POST" (str agency-base "/api/alpha/agents/"
                                          (:agent-id req) "/reset-session") {}))
            reset-ok? (or (nil? reset-response)
                          (and (= 200 (:http/status reset-response))
                               (:ok reset-response)))]
        (cond
          (not (:ok prepared)) prepared
          (not reset-ok?)
          {:ok false :error/code :student-session-reset-failed}
          :else
          (let [packet (prompt (submission/with-job-authority req))
                archived (archive-rendered-packet! state-path (:phase req)
                                                   packet)
                _ (when-not (:ok archived)
                    (binding [*out* *err*]
                      (println "[apm.packet-archive]" (pr-str archived))))
                activated (job-port/activate!
                           agency-base
                           {:agent-id (:agent-id req)
                            :prompt packet
                            :job-id (:job-id ticket)})]
            (cond-> activated
              (not (:ok archived))
              (assoc :packet/archive-finding archived))))))
    :job-fn
    (fn [job-id]
      (job-port/observe agency-base job-id))
    :cancel-fn
    (fn [job-id]
      (job-port/cancel! agency-base job-id
                        "typed-submission activation supersession"))
    :persist-fn #(runtime/atomic-persist! state-path %)
    :ticket-register-fn submission/register!
    :terminal-submission-provider (fn [_ ticket _]
                                    (submission/submitted (:job-id ticket)))
    :terminal-validator validate-terminal
    :posthoc-fault-origin-fn
    (fn [active-request failure]
      (if (and (= :frame-cycle-guide-mode-invalid (:error/code failure))
               (nil? (:mode active-request))
               (contains? #{:store-mode :harness-mode}
                          (:mode (or fresh-request active-request))))
        :apparatus
        :agent))
    :terminal-repair-request-fn
    (fn [active-request ticket job failure]
      (posthoc-terminal-repair-request fresh-request active-request ticket job
                                       failure))
    :terminal-budget-config (or (:terminal-budget request)
                                driver/default-terminal-budget)
    :missing-observation-provider
    (when (= :student-attempt (:kind action))
      (fn [request ticket job repair-attempts collection-evidence]
        (missing-observation-receipt contract action receipts request ticket job
                                     repair-attempts collection-evidence
                                     #(archive-student-source!
                                       request state-path source-archive-fn)
                                     #(student-candidate-fn
                                       {:lease (get-in preparation
                                                       [:workspaces :student])
                                        :attempt-ordinal
                                        (:attempt-ordinal request)})
                                     #(workspace-reset-fn
                                       (get-in preparation
                                               [:workspaces :student])))))
    :receipt-provider
    (fn [request ticket job validated]
      (cond
        (= :promote-solver (:phase action))
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

        ;; Preserve and compile before certifying: the receipt names an exact
        ;; Git candidate, so a dirty but valid Student result cannot vanish at
        ;; the next reset or retirement boundary.
        (= :student-attempt (:kind action))
        (let [candidate (student-candidate-fn
                         {:lease (get-in preparation [:workspaces :student])
                          :attempt-ordinal (:attempt-ordinal request)})]
          (if-not (:ok candidate)
            candidate
            (let [archived (archive-student-source! request state-path
                                                    source-archive-fn)]
              (if-not (:ok archived)
                archived
                (receipt contract action receipts request ticket job
                         (assoc validated
                                :source (:source archived)
                                :candidate (:candidate candidate)))))))

        ;; A store-mode Guide deposit is reviewed independently and published
        ;; as a union snapshot before the Guide receipt exists, so the receipt
        ;; can carry the snapshot the next Student attempt binds to.
        (and (= :guide-intervention (:kind action))
             (seq (get-in validated [:report :candidates])))
        (if-not (map? guide-promotion)
          {:ok false :error/code :guide-promotion-driver-missing}
          (let [stepped (guide-promotion-step! guide-promotion request
                                               (:report validated))]
            (if (= :certified (:status stepped))
              (receipt contract action receipts request ticket job
                       (assoc-in validated [:report :memory-snapshot]
                                 (:memory-snapshot stepped)))
              stepped)))

        :else
        (receipt contract action receipts request ticket job validated)))}))

(defn guide-promotion-step!
  "Drive the independent review of a Guide's store-mode candidates. The
  review state lives beside the Guide phase state; RUN-FN steps the durable
  promotion machine from it. Returns :awaiting-terminal until the reviewer's
  verdicts are published, then :certified with the union snapshot."
  [{:keys [state-path run-fn persist-candidates-fn]} request report]
  (let [state-path (Path/of (str state-path) (make-array String 0))
        state (runtime/read-state state-path)]
    (cond
      (nil? state)
      (let [gated (pipeline/validate-guide-deposit
                   {:depositor (:agent-id request)
                    :candidates (:candidates report)}
                   {:problem-id (:problem-id request)
                    :solver-certified-source
                    (:solver-certified-source request)})]
        (if-not (:ok gated)
          {:ok false :error/code :guide-candidates-invalid
           :findings (:findings gated)}
          (if-not (fn? persist-candidates-fn)
            {:ok false :error/code :guide-candidate-persistence-missing}
            (let [deposit {:depositor (:agent-id request)
                           :dispatch/id (:dispatch/id request)
                           :prior-snapshot (:prior-snapshot request)
                           :candidates (:candidates gated)}
                  materialized (persist-candidates-fn deposit request)]
              (if-not (:ok materialized)
                materialized
                (let [seeded {:state/type :promotion :stage :review-pending
                              :deposit (:deposit materialized)
                              :candidates (:candidates materialized)
                              :mechanical-reviews (:mechanical-reviews gated)}
                      persisted (runtime/atomic-persist! state-path seeded)]
                  (if-not (:ok persisted)
                    {:ok false :error/code :guide-promotion-persistence-failed}
                    (run-fn))))))))

      (= :promotion-certified (:state/type state))
      (let [published (:receipt state)]
        {:ok true :status :certified
         :memory-snapshot
         {:snapshot-id (:receipt/snapshot-id published)
          :snapshot-digest (:receipt/snapshot-digest published)
          :snapshot-path (:receipt/snapshot-path published)
          :reviewed-memory-ids (:receipt/reviewed-memory-ids published)
          :promotion-reviews (:receipt/promotion-reviews published)
          :independent-review? true}})

      :else
      (let [stepped (run-fn)]
        (if (= :certified (:status stepped))
          (guide-promotion-step! {:state-path state-path :run-fn run-fn
                                  :persist-candidates-fn persist-candidates-fn}
                                 request report)
          stepped)))))
