(ns futon3c.apm.csquare-synthetic-campaign
  "Isolated ten-frame campaign smoke.  The solver is programmatic; Lean,
  operational-trace checking, campaign closure, and coordinator scheduling are
  the production implementations."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.campaign-trace :as trace]
            [futon3c.apm.durable-coordinator :as coordinator]
            [futon3c.apm.live-preflight-runtime :as persistence]
            [futon3c.apm.live-regulator :as regulator]
            [futon3c.apm.semantic-progress-watchdog :as watchdog])
  (:import [java.nio.file Path]))

(def campaign-id "csquare-synthetic-campaign-v1")
(def coordinator-id campaign-id)
(def adapter-key :apm/csquare-synthetic-campaign)
(def root "data/apm-campaigns/csquare-synthetic-campaign-v1")
(def registry-path (str root "/registry.edn"))
(def coordinator-state-path (str root "/coordinator.edn"))
(def campaign-state-path (str root "/campaign.edn"))
(def problem-count 10)
(def phases [:solve :verify :close])

(def template
  "import Mathlib\n\nexample : 1 + 1 = 2 := by\n  sorry\n")

(defn- path [s] (Path/of (str s) (make-array String 0)))

(defn- persist! [target value]
  (persistence/atomic-persist! (path target) value))

(defn- event [sequence type body]
  {:event/id (str "csquare-e" sequence)
   :event/seq sequence :event/type type
   :event/campaign-id campaign-id :event/actor "csquare"
   :event/at (format "2026-08-28T00:%02d:00Z" (mod sequence 60))
   :event/expected-version sequence :event/body body})

(defn- initial-campaign []
  {:state/type :csquare-campaign
   :campaign/id campaign-id :next-frame 1 :frames [] :priors []
   :events [(event 0 :campaign/registered
                   {:series :apm :manifest-hash "csquare-isolated-v1"
                    :closure-policy-version 1 :phase-order phases})
            (event 1 :block/opened {:block-id "csquare-block" :ordinal 1})]})

(defn- read-campaign-state []
  (if (.exists (io/file campaign-state-path))
    (edn/read-string (slurp campaign-state-path))
    (initial-campaign)))

(defn- write-proof! [frame-number]
  (let [directory (io/file root "corpus" (format "c%02d" frame-number))
        source (io/file directory "Main.lean")
        terminal (io/file directory "programmatic-terminal.edn")]
    (.mkdirs directory)
    (spit source template)
    ;; The synthetic solver performs the promised edit, rather than asking a
    ;; gate to pretend the sorry-bearing source passed.
    (spit source (.replace (slurp source) "sorry" "norm_num"))
    (let [checked (shell/sh "lake" "env" "lean" (.getCanonicalPath source)
                            :dir "/home/joe/code/apm-lean")
          record {:terminal-job-id (format "csquare-solver-%02d" frame-number)
                  :source (.getCanonicalPath source) :exit (:exit checked)
                  :stdout (:out checked) :stderr (:err checked)}]
      (spit terminal (pr-str record))
      (assoc record :terminal-file (.getCanonicalPath terminal)))))

(defn- durable-observations! [frame-number terminal predecessor]
  (let [directory (io/file root "frames" (format "c%02d" frame-number)
                           "durable")
        documents
        [{:watchdog/trace-observation
          {:coordinator-enabled? true :elapsed-ms 1
           :valid-external-wait? false :semantic-cursor-advanced? true
           :coordinator-disabled? false :first-violation-recorded? false}}
         {:trace/successor-observation
          {:predecessor-id predecessor
           :terminal-evidence-id (str predecessor "-terminal")
           :collection-evidence-id (str predecessor "-collection")
           :disposition "closed" :predecessor-persisted? true
           :successor-announced-id (:terminal-job-id terminal)
           :successor-activated-id (:terminal-job-id terminal)}}
         {:trace/delivery-observation
          {:terminal-job-id (:terminal-job-id terminal)
           :delivery-status "delivered"
           ;; programmatic-terminal.edn is the observable inbox action.
           :inbox-file-created? true :registered-push-performed? false
           :polling-available? true}}]]
    (.mkdirs directory)
    (mapv (fn [index document]
            (let [target (io/file directory (str index ".edn"))]
              (spit target (pr-str document))
              (edn/read-string (slurp target))))
          (range) documents)))

(defn- append-frame-events [events frame-id receipt]
  (let [start (count events)]
    (into events
          [(event start :frame/opened
                  {:frame-id frame-id :block-id "csquare-block"
                   :problem-id frame-id :registration-hash frame-id
                   :harness-hash "csquare-programmatic-solver-v1"})
           (event (inc start) :frame/advanced
                  {:frame-id frame-id :from :solve :to :verify
                   :certificate {:programmatic-solver :edited-source}})
           (event (+ start 2) :frame/advanced
                  {:frame-id frame-id :from :verify :to :close
                   :certificate {:lean/exit 0}})
           (event (+ start 3) :frame/closed
                  {:frame-id frame-id :certificate receipt})])))

(defn process-one!
  "Run one frame from the durable campaign cursor. Returns a coordinator
  postcondition, never a fabricated gate verdict."
  []
  (let [state (read-campaign-state)
        n (:next-frame state)]
    (if (> n problem-count)
      {:ok true :status :batch-complete}
      (let [frame-id (format "c%02d" n)
            priors (:priors state)
            watchdog-armed?
            (boolean (watchdog/running?
                      (str "semantic-progress:" coordinator-id)))
            terminal (write-proof! n)]
        (if-not watchdog-armed?
          {:ok false :error/code :csquare-watchdog-not-armed
           :frame-id frame-id}
          (if-not (zero? (:exit terminal))
          {:ok false :error/code :csquare-real-lean-verification-failed
           :frame-id frame-id :terminal terminal}
          (let [documents (durable-observations!
                           n terminal (or (:terminal-job-id (peek priors))
                                          "csquare-campaign-start"))
                issued (trace/issue-combined-trace-receipt!
                        {:certificate {:receipt/type :csquare-frame-close
                                       :receipt/frame-id frame-id}
                         :durable-documents documents
                         :trace-path (io/file root "frames" frame-id
                                              "combined-trace.json")})]
            (if-not (:ok issued)
              issued
              (let [frame-events (append-frame-events
                                  (:events state) frame-id
                                  (:certificate issued))
                    events (if (= n problem-count)
                             (let [start (count frame-events)]
                               (conj frame-events
                                     (event start :block/closed
                                            {:block-id "csquare-block"
                                             :certificate {:frames problem-count}})
                                     (event (inc start) :campaign/closed
                                            {:certificate (:certificate issued)})))
                             frame-events)
                    projection (machine/projection events)
                    frame-record {:frame/id frame-id
                                  :prior-count (count priors)
                                  :terminal-job-id (:terminal-job-id terminal)
                                  :trace/digest (get-in issued
                                                        [:certificate :trace/digest])
                                  :checker/status (get-in issued
                                                          [:certificate :trace/checker-receipt
                                                           :checker/status])
                                  :watchdog/armed watchdog-armed?}
                    next-state (-> state
                                   (assoc :events events :next-frame (inc n))
                                   (assoc :projection/status
                                          (:projection/status projection))
                                   (assoc :campaign/status
                                          (:campaign/status projection))
                                   (update :frames conj frame-record)
                                   (update :priors conj terminal))]
                (if-not (= :valid (:projection/status projection))
                  {:ok false :error/code :csquare-frame-closure-refused
                   :frame-id frame-id :projection projection}
                  (let [saved (persist! campaign-state-path next-state)]
                    (if (:ok saved)
                      {:ok true :status (if (= n problem-count)
                                         :batch-complete :phase-advanced)
                       :frame-id frame-id :prior-count (count priors)}
                      saved))))))))))))

(defn adapter-constructor [_]
  {:decide-fn
   (fn [state]
     (let [ordinal (inc (or (:regulator/ticks state) 0))]
       {:ok true :coordinator/action :activate
        :coordinator/intent
        {:job-id (str "csquare-tick-" ordinal)
         :dispatch/id (str "csquare-dispatch-" ordinal)
         :dispatch/action :csquare/tick
         :dispatch/parameters {:deadline-ms (+ (System/currentTimeMillis) 300000)}
         :expected/postcondition
         {:status/one-of [:phase-advanced :batch-complete]}}}))
   :reconcile-fn
   (fn [_ _]
     (let [result (process-one!)]
       (if-not (:ok result)
         result
         {:ok true
          :status (if (= :batch-complete (:status result))
                    :frame-complete :queue-tick-complete)
          :coordinator/clear-intent? true :queue/result result})))})

(defn register-adapter! []
  (coordinator/register-adapter! adapter-key adapter-constructor))

(defn start!
  "Start only C□ through the typed production coordinator registry."
  []
  (register-adapter!)
  (.mkdirs (io/file root))
  (when-not (.exists (io/file coordinator-state-path))
    (persist! coordinator-state-path (regulator/initial-state coordinator-id)))
  (let [registered
        (coordinator/register!
         {:registry-path registry-path :coordinator-id coordinator-id
          :adapter adapter-key :config {} :state-path coordinator-state-path
          :period-ms 250})]
    (if (:ok registered)
      (coordinator/start-registered! registry-path coordinator-id)
      registered)))

(defn result []
  (let [campaign (when (.exists (io/file campaign-state-path))
                   (edn/read-string (slurp campaign-state-path)))
        status (coordinator/status registry-path coordinator-id)
        frames (:frames campaign)
        prior-counts (mapv :prior-count frames)
        pass? (and (= problem-count (count frames))
                   (= (vec (range problem-count)) prior-counts)
                   (every? #(= :accepted (:checker/status %)) frames)
                   (every? :watchdog/armed frames)
                   (= :valid (:projection/status campaign))
                   (= :closed (:campaign/status campaign))
                   (= :complete (get-in status [:durable-state
                                                :regulator/status]))
                   (nil? (:tick-claim status)))]
    {:ok pass? :status (if pass? :batch-complete :failed)
     :campaign/id campaign-id :frame-count (count frames)
     :campaign/projection-status (:projection/status campaign)
     :campaign/status (:campaign/status campaign)
     :frame-ids (mapv :frame/id frames) :prior-counts prior-counts
     :watchdog/armed-at-end
     (boolean (watchdog/running? (str "semantic-progress:" coordinator-id)))
     :outstanding-tick-claim (:tick-claim status)
     :coordinator/status (get-in status [:durable-state :regulator/status])
     :frames frames}))
