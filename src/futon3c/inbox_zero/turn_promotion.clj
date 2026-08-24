(ns futon3c.inbox-zero.turn-promotion
  "Turn-end composition of the inbox-zero promotion pipeline.

  The default mode is :off. :propose performs read-only planning, screening,
  routing, and honest delivery; :execute additionally commits and applies the
  ordinary-vs-outlier push policy. Tier-1 exact seats use the durable followup
  queue. Tier-2/3 transports do not yet exist, so those decisions are appended
  to a durable ledger and printed loudly instead of being forged as deliveries."
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
  (:import [java.nio.charset StandardCharsets]
           [java.nio.file Files StandardCopyOption]
           [java.nio.file.attribute FileAttribute]
           [java.security MessageDigest]
           [java.util Date]))

(def default-state-path "/home/joe/code/storage/inbox-zero/state.edn")
(defonce ^:private ledger-monitor (Object.))

(defn- mode-keyword [mode]
  (let [value (some-> mode name str/lower-case keyword)]
    (if (#{:off :propose :execute} value) value :off)))

(defn- sha-256 [value]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256")
                        (.getBytes (pr-str value) StandardCharsets/UTF_8))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))

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

(defn- held-message [plan]
  (let [by-reason (frequencies (map :reason (:exclude plan)))
        reason-labels [[:unattributed "unattributed"]
                       [:other-seat "other-seat"]
                       [:ambiguous "ambiguous"]]
        counts (->> reason-labels
                    (keep (fn [[reason label]]
                            (let [n (get by-reason reason 0)]
                              (when (pos? n) (str n " " label)))))
                    (str/join ", "))
        unattributed (->> (:exclude plan)
                          (filter #(= :unattributed (:reason %)))
                          (map :path)
                          sort
                          vec)
        shown (take 5 unattributed)
        more (- (count unattributed) (count shown))]
    (str "inbox-zero: nothing promotable for " (:seat/id plan)
         " in " (:repo/id plan) " — "
         (if (seq counts) counts "no exclusions recorded")
         (when (seq unattributed)
           (str "; unattributed: " (str/join ", " shown)
                (when (pos? more) (str " (+" more " more)")))))))

(defn- propose-message [plan]
  (if (seq (:include plan))
    (would-message plan)
    (held-message plan)))

(defn- member-hash [item]
  (sha-256 (select-keys (or (:plan item) item)
                           [:seat/id :repo/id :worktree/id :include :exclude])))

(defn- tier-one-payload [decision]
  (let [item (:route/item decision)
        plan (or (:plan item) item)
        [agent session] (parse-seat (:route/recipient decision))
        reason (or (:route/reason decision) :propose)]
    {:agent agent :session session :type "inbox-zero"
     :dedupe-key [(:seat/id plan) (:repo/id plan) (:worktree/id plan)
                  reason (member-hash item)]
     :prompt (:route/message decision)
     :metadata {:route-tier 1 :reason reason
                :repo-id (:repo/id plan) :worktree-id (:worktree/id plan)}}))

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

(defn promote-at-turn-end!
  "Run the configured turn-end pipeline for exact AGENT-ID/SESSION-ID.

  Every collaborator is injectable. Any throwable is caught and printed so
  this dev-facing entry never propagates backpressure into invoke completion."
  [agent-id session-id options]
  (let [mode (mode-keyword (or (:mode options)
                               (System/getenv "FUTON3C_INBOX_ZERO_PROMOTION")))]
    (when-not (= :off mode)
      (let [print-fn (or (:print-fn options) println)]
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
                routable (if (= :propose mode)
                           outcomes
                           (filterv #(or (= :held (:verdict %))
                                         (= :escalate (:verdict %))) outcomes))
                decisions (route-fn routable route-context)
                decisions (if (= :propose mode)
                            (mapv (fn [decision]
                                    (if (= 1 (:route/tier decision))
                                      (assoc decision :route/message
                                             (propose-message (:route/item decision)))
                                      decision))
                                  decisions)
                            decisions)]
            (deliver-decisions! decisions
                                {:deliver! deliver! :ledger-fn ledger-fn
                                 :ledger-path ledger-path :print-fn print-fn})
            {:mode mode :plans screened :outcomes outcomes :decisions decisions})
          (catch Throwable error
            (try
              (print-fn (str "[inbox-zero] turn promotion failed for " agent-id
                             ": " (.getMessage error)))
              (catch Throwable _))
            {:mode mode :error error}))))))

(defn launch-at-turn-end!
  "Fire-and-forget dev seam; all failures are printed and never rethrown."
  [agent-id session-id]
  (future (promote-at-turn-end! agent-id session-id {})))
