(ns futon3c.inbox-zero.sweeper
  "Bounded proposal-only sweeps over unattributed inbox-zero paths.

  This lane never mints claims. It proposes confirmation to a live exact seat,
  ledgers unavailable or ambiguous cases, and leaves insufficient backlog
  quiet and countable."
  (:require [babashka.http-client :as http]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3.inbox-zero.infer :as infer]
            [futon3.inbox-zero.projection :as projection]
            [futon3.inbox-zero.state :as inbox-state]
            [futon3c.inbox-zero.infer-adapters :as infer-adapters]
            [futon3c.inbox-zero.turn-promotion :as turn-promotion])
  (:import [java.nio.charset StandardCharsets]
           [java.nio.file Files StandardCopyOption]
           [java.nio.file.attribute FileAttribute]
           [java.security MessageDigest]
           [java.util Date]))

(def default-max-paths 25)
(def default-interval-ms 1800000)
(defonce ^:private !loop (atom nil))
(defonce ^:private proposals-monitor (Object.))

(defn- sha-256 [value]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256")
                        (.getBytes (pr-str value) StandardCharsets/UTF_8))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))

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
      (->> (:agents (json/parse-string (:body response) true))
           (keep (fn [[agent-id agent]]
                   (when-let [session (:session-id agent)]
                     (str "seat:" (name agent-id) ":" session))))
           set))))

(defn- default-deliver [url payload]
  (http/post url {:headers {"Content-Type" "application/json"}
                  :body (json/generate-string payload)
                  :throw false}))

(defn- evidence-hash [candidate]
  (->> (:evidence candidate) (keep :source/id) distinct sort vec sha-256))

(defn- proposal-key [result candidate]
  [(:path/key result) (evidence-hash candidate)])

(defn- atomic-write! [path value]
  (let [target (.toPath (io/file path))
        parent (or (.getParent target) (.toPath (io/file ".")))
        attrs (make-array FileAttribute 0)]
    (Files/createDirectories parent attrs)
    (let [tmp (Files/createTempFile parent ".attribution-proposals-" ".edn" attrs)]
      (try
        (spit (.toFile tmp) (str (pr-str value) "\n"))
        (Files/move tmp target
                    (into-array StandardCopyOption
                                [StandardCopyOption/ATOMIC_MOVE
                                 StandardCopyOption/REPLACE_EXISTING]))
        (finally (Files/deleteIfExists tmp))))))

(defn- load-proposals [path print-fn]
  (locking proposals-monitor
    (try
      (let [file (io/file path)]
        (if-not (.exists file)
          {}
          (let [value (edn/read-string (slurp file))]
            (if (map? value)
              value
              (throw (ex-info "proposal ledger is not an EDN map" {}))))))
      (catch Throwable error
        (print-fn (str "[inbox-zero] attribution proposals unreadable; "
                       "starting empty: " (.getMessage error)))
        {}))))

(defn- record-proposal! [path proposals key value]
  (locking proposals-monitor
    (let [next-value (assoc @proposals key value)]
      (atomic-write! path next-value)
      (reset! proposals next-value))))

(defn- already-count [counts entry]
  (update counts (if (= :proposed (:outcome entry))
                   :already-proposed
                   :already-ledgered)
          inc))

(defn- followup-payload [result candidate]
  (let [[agent session] (parse-seat (:seat/id candidate))
        path-key (:path/key result)
        confirmation-body
        (json/generate-string
         {:agent agent :session session
          :path-key {:repo-id (:repo/id path-key)
                     :worktree-id (:worktree/id path-key)
                     :path (:path path-key)}
          :response-id (str "inbox-zero-confirm:" (evidence-hash candidate))})]
    {:agent agent :session session :type "inbox-zero"
     :dedupe-key [path-key (:seat/id candidate)
                  (evidence-hash candidate)]
     :prompt (str (infer/confirmation-followup-text result candidate)
                  " Confirm with: curl -sS -X POST "
                  "http://127.0.0.1:7070/api/alpha/inbox-zero/confirm-attribution "
                  "-H 'Content-Type: application/json' -d '" confirmation-body "'")
     :metadata {:proposal/type :inbox-zero/attribution
                :path/key path-key
                :confidence (:confidence candidate)}}))

(defn- ledger-decision [result reason]
  {:route/tier 2
   :route/recipient "street-sweeper"
   :route/item result
   :route/reason reason
   :route/message
   (if (= :ambiguous reason)
     (str "Attribution ambiguous for " (:path/key result) ": "
          (str/join ", " (map :seat/id (:candidates result))))
     (str "Attribution candidate unavailable for " (:path/key result) ": "
          (get-in result [:candidates 0 :seat/id])))})

(defn- empty-counts [unswept]
  {:swept 0 :proposed 0 :ledgered 0 :insufficient 0 :errored 0
   :already-proposed 0 :already-ledgered 0
   :unswept unswept})

(defn sweep-attributions!
  "Run one bounded attribution proposal pass; all collaborators are injectable."
  [options]
  (let [print-fn (or (:print-fn options) println)]
    (try
      (let [state-path (or (:state-path options)
                           (System/getenv "FUTON3C_INBOX_ZERO_STATE_PATH")
                           "/home/joe/code/storage/inbox-zero/state.edn")
            load-state-fn (or (:load-state-fn options) inbox-state/load-state)
            build-bundle-fn (or (:build-bundle-fn options)
                                infer-adapters/build-evidence-bundle)
            infer-fn (or (:infer-fn options) infer/infer-attribution)
            roster-fn (or (:roster-fn options) default-roster)
            followup-url (or (:followup-url options)
                             (System/getenv "FUTON3C_INBOX_ZERO_FOLLOWUP_URL")
                             "http://127.0.0.1:7070/api/alpha/followups")
            deliver! (or (:deliver! options) #(default-deliver followup-url %))
            ledger-fn (or (:ledger-fn options) turn-promotion/append-escalation!)
            ledger-path (or (:escalation-ledger-path options)
                            (System/getenv "FUTON3C_INBOX_ZERO_ESCALATION_LEDGER")
                            (str (.getParent (io/file state-path))
                                 "/escalation-ledger.edn"))
            state-parent (or (.getParent (io/file state-path)) ".")
            proposals-path (or (:proposals-path options)
                               (str state-parent "/attribution-proposals.edn"))
            now-fn (or (:now-fn options) #(Date.))
            max-paths (max 0 (long (or (:max-paths options) default-max-paths)))
            state (load-state-fn state-path)
            paths (->> (:unattributed (projection/project-dirty-sets state (now-fn)))
                       (sort-by (juxt :worktree/id :path)) vec)
            selected (subvec paths 0 (min max-paths (count paths)))
            unswept (- (count paths) (count selected))
            live-seats (roster-fn)
            proposals (atom (load-proposals proposals-path print-fn))
            bundle-options (assoc options :state-path state-path
                                           :load-state-fn (constantly state))]
        (when (pos? unswept)
          (print-fn (str "[inbox-zero] attribution sweep left " unswept
                         " path(s) unswept (max-paths=" max-paths ")")))
        (reduce
         (fn [counts path-fact]
           (try
             (let [bundle (build-bundle-fn path-fact bundle-options)
                   result (infer-fn path-fact bundle)
                   candidate (first (:candidates result))
                   key (proposal-key result candidate)
                   prior (get @proposals key)
                   counts (update counts :swept inc)]
               (case (:verdict result)
                 :propose
                 (if prior
                   (already-count counts prior)
                   (if (contains? live-seats (:seat/id candidate))
                     (let [response (deliver! (followup-payload result candidate))]
                       (when (not= 200 (:status response))
                         (throw (ex-info "Attribution followup delivery failed"
                                         {:status (:status response)})))
                       (record-proposal!
                        proposals-path proposals key
                        {:seat/id (:seat/id candidate) :outcome :proposed
                         :reason :live-seat :at (now-fn)})
                       (update counts :proposed inc))
                     (do
                       (ledger-fn ledger-path
                                  (ledger-decision result :seat-not-live))
                       (record-proposal!
                        proposals-path proposals key
                        {:seat/id (:seat/id candidate) :outcome :ledgered
                         :reason :seat-not-live :at (now-fn)})
                       (-> counts (update :proposed inc) (update :ledgered inc)))))

                 :ambiguous
                 (if prior
                   (already-count counts prior)
                   (do
                     (ledger-fn ledger-path (ledger-decision result :ambiguous))
                     (record-proposal!
                      proposals-path proposals key
                      {:seat/id (:seat/id candidate) :outcome :ledgered
                       :reason :ambiguous :at (now-fn)})
                     (update counts :ledgered inc)))

                 :insufficient (update counts :insufficient inc)
                 (throw (ex-info "Unknown attribution verdict" {:result result}))))
             (catch Throwable error
               (print-fn (str "[inbox-zero] attribution sweep path failed "
                              (:worktree/id path-fact) "/" (:path path-fact)
                              ": " (.getMessage error)))
               (-> counts (update :swept inc) (update :errored inc)))))
         (empty-counts unswept)
         selected))
      (catch Throwable error
        (try
          (print-fn (str "[inbox-zero] attribution sweep failed: "
                         (.getMessage error)))
          (catch Throwable _))
        (empty-counts 0)))))

(defn start-loop!
  "Start one delayed background sweep loop. Repeated starts are idempotent."
  [{:keys [interval-ms print-fn] :as options}]
  (let [interval-ms (long (or interval-ms default-interval-ms))
        print-fn (or print-fn println)]
    (when-not (and @!loop (not (future-done? @!loop)))
      (reset!
       !loop
       (future
         (try
           (loop []
             (Thread/sleep interval-ms)
             (try
               (#'sweep-attributions! (dissoc options :interval-ms))
               (catch Throwable error
                 (print-fn (str "[inbox-zero] attribution sweep loop threw: "
                                (.getMessage error)))))
             (recur))
           (catch InterruptedException _)
           (catch Throwable error
             (print-fn (str "[inbox-zero] attribution sweep loop stopped: "
                            (.getMessage error)))))))
      @!loop)))
