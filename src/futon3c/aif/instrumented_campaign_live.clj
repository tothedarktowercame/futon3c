(ns futon3c.aif.instrumented-campaign-live
  "Preregistered, single-flight live execution of the R11/R12/R15/R17 campaign.

  Proposal cost is one plus the mission's live open-hole count, capped at six
  scope points. Utility is a monotone transform of the War Machine rank. The
  two independently ranked fields are missions housed in futon3c and missions
  housed elsewhere in the stack. This definition is recorded with every run."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [futon2.aif.full-loop-runner :as full-loop]
            [futon3c.aif.instrumented-campaign :as campaign]
            [futon3c.evidence.boundary :as evidence]
            [futon3c.logic.capability-star-map-extractor :as star-map]
            [futon3c.peripheral.live-wm-selection :as live-selection]
            [futon3c.wm.runner-service :as runner-service])
  (:import (java.security MessageDigest)
           (java.time Instant)
           (java.util UUID)))

(def phase-log
  "/home/joe/code/futon2/data/wm-instrumented-campaign-phases.edn.log")

(def plan
  {:campaign/patterns [:R11 :R12 :R15 :R17]
   :campaign/ticks 2
   :r11/fields {:inside-futon3c "advance-mission actions whose mission path is in futon3c"
                :rest-of-stack "advance-mission actions housed elsewhere in the stack"}
   :r11/cost-definition "min(6, 1 + live open-hole-count), in bounded open-hole scope points"
   :r11/utility-definition "1000 - live War Machine rank; preserves live ordering"
   :r11/leaf-budget 6
   :r11/shared-budget 7
   :r15/class-definition {:inside-futon3c :close-hole
                          :rest-of-stack :advance-capability}
   :r15/initial-slow-mode :exploration
   :r17/corpus-definition "live WM capability-to-mission scope and production observations"
   :execution {:author "claude-7"
               :reviewer "codex-7"
               :repair-reviewer "codex-1"
               :cohort? false}})

(def initial-status
  {:running? false :campaign-id nil :phase nil :started-at nil
   :last-result nil :last-error nil})

(defonce !status (atom initial-status))
(defonce !result (atom nil))

(defn status [] @!status)
(defn result [] @!result)

(defn- sha256 [value]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256")
                        (.getBytes (pr-str value) "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))

(defn- repository-field [mission-path]
  (if (str/starts-with? (or mission-path "") "/home/joe/code/futon3c/")
    :inside-futon3c
    :rest-of-stack))

(defn- proposal [entry]
  (let [action (:action entry)
        field-id (repository-field (:mission-path action))
        rank (:rank entry)
        holes (long (or (:open-hole-count action) 0))]
    {:id (str (name field-id) "/" (:target action))
     :rank rank
     :action action
     :cost (min 6 (inc holes))
     :utility (- 1000 rank)
     :controller-score (:controller-score entry)
     :source/open-hole-count holes
     :source/field field-id}))

(defn- proposals-by-field [judgement]
  (->> (:ranked-actions judgement)
       (filter #(= :advance-mission (get-in % [:action :type])))
       (map proposal)
       (group-by :source/field)
       (map (fn [[field-id proposals]]
              [field-id (vec (take 8 (sort-by :rank proposals)))]))
       (into {})))

(defn- r11-request [judgement context]
  (let [fields (proposals-by-field judgement)]
    (when-not (every? #(seq (get fields %)) [:inside-futon3c :rest-of-stack])
      (throw (ex-info "Live judgement did not contain both preregistered proposal fields"
                      {:field-counts (update-vals fields count)})))
    {:root-id :instrumented-campaign/shared
     :shared-budget 7
     :context {:campaign/id (:campaign/id context)
               :cost-definition (:r11/cost-definition plan)
               :utility-definition (:r11/utility-definition plan)}
     :fields (mapv (fn [field-id]
                     {:id field-id :budget 6 :proposals (get fields field-id)})
                   [:inside-futon3c :rest-of-stack])}))

(def fast-state
  {:arrows {} :cap-overlay {} :reachable #{"campaign/root"}})

(defn- r15-input [_judgement arbitration context]
  {:state fast-state
   :moves
   (mapv
    (fn [proposal]
      (let [field (:proposal/field-id proposal)]
        {:move/id (:id proposal)
         :proposal/id (:id proposal)
         :move/class (get-in plan [:r15/class-definition field])
         :have "campaign/root"
         :want (get-in proposal [:proposal/action :target])
         :score 1.0
         :step-score-delta (double (:controller-score proposal))
         :rank (:rank proposal)
         :move/terminal? false}))
    (:selected arbitration))
   :opts {:slow-mode (get-in context [:slow-state :slow/mode])
          :horizon 1 :top-k 5}})

(defn- r17-corpus [graph]
  (let [observations
        (vec
         (for [[mission-id mission] (:missions graph)
               capability-id (distinct (concat (:scope mission)
                                               (:produces mission)))
               :when (contains? (:capabilities graph) capability-id)]
           [capability-id mission-id]))]
    {:capabilities (vec (sort (distinct (map first observations))))
     :edges observations
     :discharges []}))

(defn- campaign-tick [tick-id]
  {:tick/id tick-id
   :tick/as-of (str (Instant/now))
   :runner/opts (merge (:execution plan)
                       {:trigger :instrumented-two-tick-campaign
                        :phase-log phase-log})
   :r11/request r11-request
   :r15/input r15-input})

(defn- phase-sink [campaign-id event]
  (io/make-parents phase-log)
  (spit phase-log (str (pr-str event) "\n") :append true)
  (swap! !status assoc :phase (:phase event)
         :attempt-id (:attempt-id event))
  (swap! runner-service/!status assoc :phase (:phase event)
         :attempt-id (:attempt-id event)
         :click-id campaign-id))

(defn- live-runner [campaign-id opts]
  (full-loop/run-opportunity!
   (assoc opts
          :phase-log-fn #(phase-sink campaign-id %)
          :strategic-selection-invoke-fn
          (fn [request]
            {:ok true :selection (live-selection/validated-selection request)}))))

(defn- receipt-fn [campaign-id]
  (fn [record]
    (evidence/append-default!
     {:subject {:ref/type :decision :ref/id campaign-id}
      :type :reflection
      :claim-type :observation
      :author "wm-instrumented-campaign"
      :body record
      :tags [:aif :instrumented-campaign :r11 :r12 :r15 :r17]})))

(defn- release-runner! [campaign-id summary]
  (swap! runner-service/!status
         (fn [current]
           (if (= campaign-id (:click-id current))
             (assoc current :running? false :phase nil :last-result summary)
             current))))

(defn- execute! [campaign-id corpus]
  (try
    (let [campaign-result
          (campaign/run-two-tick!
           {:campaign/id campaign-id
            :campaign/plan (assoc plan :r17/corpus-sha256 (sha256 corpus))
            :initial-slow-state {:slow/mode :exploration :slow/intrinsics {}}
            :tick-a (campaign-tick :tick-a)
            :tick-b (fn [_] (campaign-tick :tick-b))
            :r17/run {:run-id (str campaign-id "/r17-intertick")
                      :parent-model {:id "capability-star-map/wm"
                                     :revision (str "sha256:" (sha256 corpus))}
                      :corpus corpus}
            :runner-fn #(live-runner campaign-id %)
            :record-fn (receipt-fn campaign-id)})
          summary {:campaign/status (:campaign/status campaign-result)
                   :campaign/compliant? (:campaign/compliant? campaign-result)
                   :campaign/timing (:campaign/timing campaign-result)
                   :attempt-ids (mapv #(get-in % [:runner/result :attempt-id])
                                      (:campaign/ticks campaign-result))
                   :evidence/id (get-in campaign-result
                                        [:campaign/storage-receipt :evidence/id])}]
      (reset! !result campaign-result)
      (swap! !status assoc :running? false :phase nil :last-result summary)
      (release-runner! campaign-id summary))
    (catch Throwable throwable
      (let [failure {:campaign/status :failed
                     :error (or (.getMessage throwable)
                                (.getName (class throwable)))}]
        (reset! !result {:error throwable :failure failure})
        (swap! !status assoc :running? false :phase nil :last-error failure)
        (release-runner! campaign-id failure)))))

(defn queue!
  "Freeze the live R17 corpus and queue one two-tick campaign in this JVM."
  ([] (queue! (str "wm-instrumented-" (UUID/randomUUID))))
  ([campaign-id]
   (let [graph (star-map/build-graph {:structural-holes? false})
         corpus (r17-corpus graph)
         current @runner-service/!status]
     (if (:running? current)
       {:rejected :already-running :click-id (:click-id current)}
       (let [started-at (str (Instant/now))
             reservation (assoc current :running? true :click-id campaign-id
                                :phase :campaign-starting :attempt-id nil
                                :started-at started-at)]
         (if-not (compare-and-set! runner-service/!status current reservation)
           (recur campaign-id)
           (let [runnable (bound-fn [] (execute! campaign-id corpus))
                 thread (Thread. ^Runnable runnable "wm-instrumented-campaign")]
             (reset! !result nil)
             (reset! !status {:running? true :campaign-id campaign-id
                              :phase :campaign-starting :started-at started-at
                              :last-result nil :last-error nil})
             (.setDaemon thread true)
             (try
               (.start thread)
               {:campaign-id campaign-id
                :started-at started-at
                :r17/corpus-sha256 (sha256 corpus)
                :r17/observations (count (:edges corpus))}
               (catch Throwable throwable
                 (release-runner! campaign-id
                                  {:campaign/status :thread-start-failed})
                 (swap! !status assoc :running? false
                        :last-error {:campaign/status :thread-start-failed
                                     :error (.getMessage throwable)})
                 (throw throwable))))))))))
