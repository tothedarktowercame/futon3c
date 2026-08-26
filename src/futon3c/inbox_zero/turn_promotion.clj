(ns futon3c.inbox-zero.turn-promotion
  "Turn-end composition of the inbox-zero promotion pipeline.

  The default mode is :off. :propose performs read-only planning, screening,
  routing, and honest delivery; :execute additionally commits and applies the
  ordinary-vs-outlier push policy. Tier-1 exact seats use the durable followup
  queue. Tier-2/3 transports do not yet exist, so those decisions are appended
  to a durable ledger and printed loudly instead of being forged as deliveries.

  Mode resolution: explicit `:mode` in options, else the live operator override
  set with `set-mode!`, else FUTON3C_INBOX_ZERO_PROMOTION, else :off. The
  override exists so the operator can flip propose/execute in the serving JVM
  without a restart; the env default in scripts/dev-zone-env is what survives
  one."
  (:require [babashka.http-client :as http]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3.inbox-zero.escalation :as escalation]
            [futon3.inbox-zero.promote-exec :as promote-exec]
            [futon3.inbox-zero.promote-push :as promote-push]
            [futon3.inbox-zero.promotion :as promotion]
            [futon3.inbox-zero.projection :as projection]
            [futon3.inbox-zero.state :as inbox-state]
            [futon3c.agency.clock-store :as clock-store])
  (:import [java.nio.file Files StandardCopyOption]
           [java.nio.file.attribute FileAttribute]
           [java.util Date]))

(def default-state-path "/home/joe/code/storage/inbox-zero/state.edn")
(defonce ^:private ledger-monitor (Object.))

(defn- mode-keyword [mode]
  (let [value (some-> mode name str/lower-case keyword)]
    (if (#{:off :propose :execute} value) value :off)))

(defn- atomic-write! [path value]
  (let [target (.toPath (io/file path))
        parent (or (.getParent target) (.toPath (io/file ".")))
        attrs (make-array FileAttribute 0)]
    (Files/createDirectories parent attrs)
    (let [tmp (Files/createTempFile parent ".escalation-" ".edn" attrs)]
      (try
        (spit (.toFile tmp) (str (pr-str value) "\n"))
        (Files/move tmp target
                    (into-array StandardCopyOption
                                [StandardCopyOption/ATOMIC_MOVE
                                 StandardCopyOption/REPLACE_EXISTING]))
        (finally (Files/deleteIfExists tmp))))))

(defn append-escalation!
  "Atomically append DECISION to the durable EDN vector at PATH."
  [path decision]
  (locking ledger-monitor
    (let [file (io/file path)
          existing (if (.exists file) (edn/read-string (slurp file)) [])]
      (when-not (vector? existing)
        (throw (ex-info "Escalation ledger must contain a vector"
                        {:error/type :inbox-zero/invalid-escalation-ledger
                         :path path})))
      (let [next-value (conj existing decision)]
        (atomic-write! path next-value)
        next-value))))

(defonce ^:private refusal-counts (atom {}))

(defn- incomplete-seat
  "Which half of the exact seat is missing, or nil when both are present."
  [agent-id session-id]
  (let [blank? (fn [v] (str/blank? (some-> v str)))]
    (cond
      (and (blank? agent-id) (blank? session-id)) :no-seat
      (blank? agent-id)                           :no-agent-id
      (blank? session-id)                         :no-session-id
      :else nil)))

(defn- refuse-incomplete-seat!
  "Refuse to plan for a seat id that cannot join a claim, and say so.

  A plan is computed for `seat:<agent>:<session>` and `plan-promotion` includes
  a path only when a claim carries that exact string. A blank half therefore
  yields a seat that matches nothing: every path reads :unattributed or
  :other-seat, every plan holds, and every hold escalates to the sweeper.
  Measured on zone before this guard — 3,263 such plans over 30 hours from six
  sessionless seats, :include empty in every one. Planning anyway manufactures
  a hold that reads as a judgement about dirt when it is a judgement about
  nothing.

  Ledgered once per (agent, reason) per process with a running count, then
  counted silently and re-printed every hundredth, so the refusal is visible
  without replacing one flood with another."
  [agent-id session-id reason {:keys [ledger-fn ledger-path print-fn now-fn]}]
  (let [seat-id (str "seat:" agent-id ":" session-id)
        k [(str agent-id) reason]
        n (get (swap! refusal-counts update k (fnil inc 0)) k)]
    (when (or (= 1 n) (zero? (mod n 100)))
      (print-fn (str "[inbox-zero] REFUSED turn promotion for " (pr-str seat-id)
                     " — " (name reason)
                     ": a seat id with a blank half joins to no claim, so every"
                     " plan would hold on nothing (occurrence " n ")")))
    (when (= 1 n)
      (try
        (ledger-fn ledger-path
                   {:record/type :inbox-zero/refusal
                    :refusal/reason reason
                    :refusal/agent-id (str agent-id)
                    :refusal/seat-id seat-id
                    :refusal/first-at (now-fn)
                    :refusal/note (str "no plan computed; subsequent identical "
                                       "refusals are counted in-process only")})
        (catch Throwable error
          (try (print-fn (str "[inbox-zero] refusal ledger append failed: "
                              (.getMessage error)))
               (catch Throwable _)))))
    {:verdict :refused :reason reason :seat/id seat-id :occurrence n}))

(defn- parse-seat [seat-id]
  (when-let [[_ agent session] (and (string? seat-id)
                                    (re-matches #"seat:([^:]+):(.+)" seat-id))]
    [agent session]))

(defn- default-roster []
  (let [port (or (System/getenv "FUTON3C_PORT") "7070")
        response (http/get (str "http://127.0.0.1:" port "/api/alpha/agents")
                           {:throw false})]
    (if-not (= 200 (:status response))
      #{}
      (let [body (json/parse-string (:body response) true)]
        (->> (:agents body)
             (keep (fn [[agent-id agent]]
                     (when-let [session (:session-id agent)]
                       (str "seat:" (name agent-id) ":" session))))
             set)))))

(defn- default-deliver [followup-url payload]
  (http/post followup-url {:headers {"Content-Type" "application/json"}
                           :body (json/generate-string payload)
                           :throw false}))

(defn- read-gates []
  (if-let [raw (some-> (System/getenv "FUTON3C_INBOX_ZERO_PROMOTION_GATES")
                       str/trim not-empty)]
    (edn/read-string raw)
    []))

(defn- repo-roots [state]
  (->> (projection/current-observations state)
       vals
       (reduce (fn [result observation]
                 (assoc result [(:repo/id observation) (:worktree/id observation)]
                        (:repo/root observation)))
               {})))

(defn- enrich-sizes [plan repo-root size-fn]
  (update plan :include
          (fn [entries]
            (mapv (fn [entry]
                    (if-let [size (try (size-fn repo-root (:path entry))
                                       (catch Exception _ nil))]
                      (assoc entry :size size)
                      entry))
                  entries))))

(defn- commit-message [agent-id plan clock]
  (str "inbox-zero: promote " (count (:include plan)) " path(s) for " agent-id
       "\n\n"
       (str/join "\n" (map :path (:include plan)))
       (when-let [mission (:mission-id clock)]
         (str "\n\nMission: " mission))))

(defn- would-message [plan]
  (format "inbox-zero would promote %d path(s) in %s: %s"
          (count (:include plan)) (:repo/id plan)
          (str/join ", " (map :path (:include plan)))))

(defn- plan-of [item] (or (:plan item) item))

(defn- actionable-proposal?
  "True when ITEM carries included paths, i.e. work the seat can act on.

  A plan with no included paths (:nothing-promotable and friends) is
  diagnostic output, not work for the agent, regardless of its verdict label.
  Delivering it starts a model turn whose completion launches another
  inbox-zero pass, so one no-op report per repository becomes a feedback loop
  (codex-19, 2026-08-26). Such plans are neither delivered nor ledgered; they
  stay observable in the returned :plans."
  [item]
  (boolean (seq (:include (plan-of item)))))

(defn- tier-one-payload [decision]
  (let [item (:route/item decision)
        plan (plan-of item)
        [agent session] (parse-seat (:route/recipient decision))
        reason (or (:route/reason decision) :propose)]
    {:agent agent :session session :type "inbox-zero"
     ;; One outstanding followup per seat/repo/worktree/reason. Membership is
     ;; deliberately NOT part of the key: when it was, every projection change
     ;; minted a fresh notice, which is how 84 of them queued on one seat.
     :dedupe-key [(:seat/id plan) (:repo/id plan) (:worktree/id plan) reason]
     :prompt (:route/message decision)
     :metadata {:route-tier 1 :reason reason
                :repo-id (:repo/id plan) :worktree-id (:worktree/id plan)}}))

(defn- ledger-decisions!
  "Propose mode: record every decision in the ledger; deliver nothing to seats.
  A dry run's audience is the operator reading the ledger, not the seats."
  [decisions {:keys [ledger-fn ledger-path print-fn]}]
  (doseq [decision decisions]
    (ledger-fn ledger-path (assoc decision :delivery :ledger-only :mode :propose)))
  (when (seq decisions)
    (print-fn (str "[inbox-zero] propose: " (count decisions)
                   " decision(s) ledgered, none delivered")))
  decisions)

(defn- deliver-decisions!
  [decisions {:keys [deliver! ledger-fn ledger-path print-fn]}]
  (doseq [decision decisions]
    (if (= 1 (:route/tier decision))
      (try
        (let [response (deliver! (tier-one-payload decision))]
          (when-not (= 200 (:status response))
            (print-fn (str "[inbox-zero] tier-1 delivery failed status="
                           (:status response) " recipient=" (:route/recipient decision)))))
        (catch Throwable error
          (print-fn (str "[inbox-zero] tier-1 delivery threw recipient="
                         (:route/recipient decision) ": " (.getMessage error)))))
      (do
        (ledger-fn ledger-path decision)
        (print-fn (str "[inbox-zero] ESCALATION tier=" (:route/tier decision)
                       " recipient=" (:route/recipient decision) " — "
                       (:route/message decision))))))
  decisions)

(defonce ^:private !mode-override (atom nil))

(defn set-mode!
  "Override the promotion mode for this JVM (:off, :propose, :execute), or
  clear the override with nil so the environment default applies again.
  Returns the mode now in effect for option-less calls."
  [mode]
  (reset! !mode-override (some-> mode mode-keyword))
  (mode-keyword (or @!mode-override
                    (System/getenv "FUTON3C_INBOX_ZERO_PROMOTION"))))

(defn promote-at-turn-end!
  "Run the configured turn-end pipeline for exact AGENT-ID/SESSION-ID.

  Every collaborator is injectable. Any throwable is caught and printed so
  this dev-facing entry never propagates backpressure into invoke completion."
  [agent-id session-id options]
  (let [mode (mode-keyword (or (:mode options)
                               @!mode-override
                               (System/getenv "FUTON3C_INBOX_ZERO_PROMOTION")))]
    (when-not (= :off mode)
      (let [print-fn (or (:print-fn options) println)]
        (if-let [reason (incomplete-seat agent-id session-id)]
          ;; Refuse before any planning: an inexact seat cannot produce a plan
          ;; that means anything, and a held plan is not the honest verdict.
          (refuse-incomplete-seat!
           agent-id session-id reason
           {:ledger-fn (or (:ledger-fn options) append-escalation!)
            :ledger-path (or (:escalation-ledger-path options)
                             (System/getenv "FUTON3C_INBOX_ZERO_ESCALATION_LEDGER")
                             (str (.getParent
                                   (io/file (or (:state-path options)
                                                (System/getenv "FUTON3C_INBOX_ZERO_STATE_PATH")
                                                default-state-path)))
                                  "/escalation-ledger.edn"))
            :print-fn print-fn
            :now-fn (or (:now-fn options) #(Date.))})
          (try
            (let [state-path (or (:state-path options)
                                 (System/getenv "FUTON3C_INBOX_ZERO_STATE_PATH")
                                 default-state-path)
                  now-fn (or (:now-fn options) #(Date.))
                  load-state-fn (or (:load-state-fn options) inbox-state/load-state)
                  plan-fn (or (:plan-fn options) promotion/plan-promotion)
                  screen-fn (or (:screen-fn options) escalation/screen-sensitivity)
                  execute-fn (or (:execute-fn options) promote-exec/execute-plan!)
                  push-fn (or (:push-fn options) promote-push/push-promoted!)
                  route-fn (or (:route-fn options) escalation/route)
                  roster-fn (or (:roster-fn options) default-roster)
                  clock-fn (or (:clock-fn options) clock-store/current-clock)
                  size-fn (or (:size-fn options)
                              (fn [root path]
                                (let [file (io/file root path)]
                                  (when (and (.exists file) (.isFile file)) (.length file)))))
                  gates (if (contains? options :gates) (:gates options) (read-gates))
                  followup-url (or (:followup-url options)
                                   (System/getenv "FUTON3C_INBOX_ZERO_FOLLOWUP_URL")
                                   "http://127.0.0.1:7070/api/alpha/followups")
                  deliver! (or (:deliver! options) #(default-deliver followup-url %))
                  ledger-path (or (:escalation-ledger-path options)
                                  (System/getenv "FUTON3C_INBOX_ZERO_ESCALATION_LEDGER")
                                  (str (.getParent (io/file state-path))
                                       "/escalation-ledger.edn"))
                  ledger-fn (or (:ledger-fn options) append-escalation!)
                  state (load-state-fn state-path)
                  roots (repo-roots state)
                  seat-id (str "seat:" agent-id ":" session-id)
                  plans (plan-fn state seat-id (now-fn))
                  screened (mapv (fn [plan]
                                   (let [root (get roots [(:repo/id plan)
                                                         (:worktree/id plan)])]
                                     (screen-fn (enrich-sizes plan root size-fn)
                                                escalation/default-rules)))
                                 plans)
                  live-seats (roster-fn)
                  route-context {:live-seats live-seats
                                 :sweeper-recipient (or (:sweeper-recipient options)
                                                        "street-sweeper")
                                 :operator-recipient (or (:operator-recipient options) "joe")}
                  outcomes
                  (if (= :propose mode)
                    screened
                    (mapv
                     (fn [plan]
                       (if (= :held (:verdict plan))
                         plan
                         (let [root (get roots [(:repo/id plan) (:worktree/id plan)])
                               executed (execute-fn
                                         plan {:repo-root root :gates gates
                                               :message (commit-message
                                                         agent-id plan
                                                         (clock-fn agent-id session-id))})]
                           (if (= :committed (:verdict executed))
                             (assoc (push-fn {:repo-root root})
                                    :seat/id seat-id :repo/id (:repo/id plan)
                                    :worktree/id (:worktree/id plan) :plan plan)
                             executed))))
                     screened))
                  considered (if (= :propose mode)
                               outcomes
                               (filterv #(or (= :held (:verdict %))
                                             (= :escalate (:verdict %))) outcomes))
                  ;; Only plans with included paths can be work for a seat;
                  ;; empty-include diagnostics are dropped here in every mode.
                  routable (filterv actionable-proposal? considered)
                  decisions (route-fn routable route-context)
                  decisions (if (= :propose mode)
                              (mapv (fn [decision]
                                      (if (= 1 (:route/tier decision))
                                        (assoc decision :route/message
                                               (would-message
                                                (plan-of (:route/item decision))))
                                        decision))
                                    decisions)
                              decisions)]
              (if (= :propose mode)
                (ledger-decisions! decisions
                                   {:ledger-fn ledger-fn :ledger-path ledger-path
                                    :print-fn print-fn})
                (deliver-decisions! decisions
                                    {:deliver! deliver! :ledger-fn ledger-fn
                                     :ledger-path ledger-path :print-fn print-fn}))
              {:mode mode :plans screened :outcomes outcomes :decisions decisions
               :diagnostic (- (count considered) (count routable))})
            (catch Throwable error
              (try
                (print-fn (str "[inbox-zero] turn promotion failed for " agent-id
                               ": " (.getMessage error)))
                (catch Throwable _))
              {:mode mode :error error})))))))

(def ^:private default-quiet-ms 20000)
(def ^:private default-max-wait-ms (* 3 60 60 1000))
(defonce ^:private !pending-launches (atom {}))

(defn- seat-invoking?
  "The registry's :invoking status is the authoritative busy signal across every
  turn path (see transport.http/agent-in-flight-turn?). Unknown seat → not busy."
  [agent-id]
  (try
    (= :invoking (get-in ((requiring-resolve 'futon3c.agency.registry/registry-status))
                         [:agents (str agent-id) :status]))
    (catch Throwable _ false)))

(defn launch-at-turn-end!
  "Debounced, idle-gated, fire-and-forget launch of promote-at-turn-end!.

  Callers may invoke this many times during one turn (the live invoke stream
  still calls it per tool-result batch until the JVM restarts on cdde47c8).
  Each call supersedes the previous pending one; after QUIET-MS of no further
  calls the seat's registry status is consulted, and promotion runs only once
  the seat is no longer :invoking — i.e. once per completed turn, never
  between two edits of one sequence (Joe, 2026-08-25: an I-0 violation).
  Gives up, loudly, if the seat is still busy after MAX-WAIT-MS.
  All failures are printed and never rethrown. Returns the future."
  ([agent-id session-id] (launch-at-turn-end! agent-id session-id {}))
  ([agent-id session-id
    {:keys [quiet-ms max-wait-ms invoking?-fn run-fn print-fn run-options]
     :or {quiet-ms default-quiet-ms max-wait-ms default-max-wait-ms
          invoking?-fn seat-invoking? run-fn promote-at-turn-end!
          print-fn println run-options {}}}]
   (let [k [(str agent-id) (str session-id)]
         n (get (swap! !pending-launches update k (fnil inc 0)) k)]
     (future
       (try
         (loop [waited 0]
           (Thread/sleep (long quiet-ms))
           (let [busy? (invoking?-fn agent-id)]
             (cond
               (not= n (get @!pending-launches k))
               :superseded

               (and busy? (< waited max-wait-ms))
               (recur (+ waited quiet-ms))

               busy?
               (do (swap! !pending-launches dissoc k)
                   (print-fn (str "[inbox-zero] turn-end promotion for " agent-id
                                  " gave up after " waited "ms: seat still invoking"))
                   :gave-up)

               :else
               (do (swap! !pending-launches dissoc k)
                   (run-fn agent-id session-id run-options)))))
         (catch Throwable error
           (try
             (print-fn (str "[inbox-zero] turn-end launch failed for " agent-id
                            ": " (.getMessage error)))
             (catch Throwable _))
           nil))))))
