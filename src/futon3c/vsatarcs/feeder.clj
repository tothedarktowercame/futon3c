(ns futon3c.vsatarcs.feeder
  "Pilot → VSATARCS bilateral-evidence feeder.

   v0 design choice: periodic poll, not file-watch. The canonical
   `vsatarcs-alignment-completeness.aif.edn` file is the persistent dedup state,
   so restarts do not require an auxiliary cursor file. Every tick:

   1. Read current pilot-adjacent substrates
   2. Derive candidate bilateral-evidence entries
   3. Deduplicate by deterministic content-hash and :vsatarcs-id
   4. Append only novel entries to the canonical :bilateral-evidence vector

   Entries are marked `:auto-generated true` and `:review-status :pending` so
   later human refinement can replace or polish the prose without ambiguity."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.util.edn-comment-preserving :as edn-comments])
  (:import [java.security MessageDigest]
           [java.time Instant ZoneOffset]
           [java.util.concurrent Executors ScheduledExecutorService TimeUnit]))

(def ^:private !state (atom nil))

(declare stop! status)

(def default-config
  {:vsatarcs-aif-path "/home/joe/code/futon4/docs/vsatarcs-alignment-completeness.aif.edn"
   :pilot-inhabitations-path "/home/joe/code/futon5a/data/pilot-inhabitations.edn"
   :anchors-path "/home/joe/code/futon5a/data/wm-ui-anchors.edn"
   :vocabulary-path "/home/joe/code/futon5a/data/war-machine-strategic-vocabulary.edn"
   :wm-core-cljs-path "/home/joe/code/futon2/web/war-machine/src/war_machine/client/core.cljs"
   :wm-judge-clj-path "/home/joe/code/futon2/scripts/futon2/report/war_machine.clj"
   :missions-dir "/home/joe/code/futon3c/holes/missions"
   :poll-ms 60000})

(def ^:private substantive-pilot-events
  #{:substantive-write :cycle-complete :substrate-creation})

(defn- now-iso []
  (str (Instant/now)))

(defn- log!
  [level message & [data]]
  (let [line (cond-> {:at (now-iso) :level level :message message}
               data (assoc :data data))]
    (println (pr-str line))
    (swap! !state
           (fn [s]
             (if (map? s)
               (update s :log (fnil conj []) line)
               s)))))

(defn- read-edn-file
  [path]
  (-> path slurp edn/read-string))

(defn- file-date
  [path]
  (-> (io/file path)
      .lastModified
      Instant/ofEpochMilli
      (.atZone ZoneOffset/UTC)
      .toLocalDate
      str))

(defn- sha-256
  [s]
  (let [digest (MessageDigest/getInstance "SHA-256")]
    (.update digest (.getBytes (str s) "UTF-8"))
    (apply str (map #(format "%02x" %) (.digest digest)))))

(defn- slugify
  [x]
  (-> (str x)
      str/lower-case
      (str/replace #"[^a-z0-9]+" "-")
      (str/replace #"(^-+|-+$)" "")))

(defn- bilateral-entries
  [path]
  (vec (:bilateral-evidence (read-edn-file path))))

(defn- existing-entry-state
  [entries]
  {:ids (into #{} (keep :vsatarcs-id) entries)
   :hashes (into #{} (keep :auto-content-hash) entries)})

(defn- auto-entry
  [{:keys [vsatarcs-id wm-id principle evidence-kind landed note
           auto-source auto-suggested-kind writer-event-id forward-pointer]}]
  (let [hash-input {:vsatarcs-id vsatarcs-id
                    :wm-id wm-id
                    :principle principle
                    :evidence-kind evidence-kind
                    :landed landed
                    :auto-source auto-source}
        content-hash (sha-256 (pr-str hash-input))]
    (cond-> {:vsatarcs-id vsatarcs-id
             :wm-id wm-id
             :principle principle
             :evidence-kind evidence-kind
             :landed landed
             :note note
             :auto-generated true
             :review-status :pending
             :auto-source auto-source
             :auto-content-hash content-hash}
      auto-suggested-kind (assoc :auto-suggested-kind auto-suggested-kind)
      writer-event-id (assoc :writer-event-id writer-event-id)
      forward-pointer (assoc :forward-pointer forward-pointer))))

(defn- substantive-pilot-event?
  [ev]
  (and (contains? substantive-pilot-events (:event ev))
       (not= :observation-only (:stage ev))))

(defn- pilot-event-principle
  [ev]
  (let [sub-kind (get-in ev [:target :new-sub-kind])
        tool (:tool ev)
        event (:event ev)]
    (cond
      (= sub-kind :symptom-to-root-traceback) :pilot-symptom-to-root-traceback
      (= sub-kind :placeholder-to-living-view) :pilot-placeholder-to-living-view
      (= tool :pilot-action) :pilot-recursive-substrate-repair
      (= tool :anchor-flip) :pilot-envelope-capability-demonstration
      (= event :substrate-creation) :pilot-inhabitation-substrate-bootstrap
      :else :pilot-substantive-cycle)))

(defn- pilot-event-wm-id
  [ev]
  (cond
    (:cycle-id ev) (str "M-war-machine-pilot cycle " (:cycle-id ev))
    (= (:event ev) :substrate-creation) "M-war-machine-pilot pilot-inhabitations substrate bootstrap"
    (:cited-consent-gate-event-id ev) (str "M-war-machine-pilot consent-gated event " (:cited-consent-gate-event-id ev))
    :else (str "M-war-machine-pilot event " (:id ev))))

(defn- pilot-event-note
  [ev]
  (str/join
   " "
   (remove str/blank?
           [(when-let [tool (:tool ev)]
              (str "Tool " tool "."))
            (when-let [cycle-id (:cycle-id ev)]
              (str "Cycle " cycle-id "."))
            (when-let [anchor (get-in ev [:target :anchor])]
              (str "Target anchor " anchor "."))
            (or (:note ev) (:context ev))
            (when-let [cite (:provenance-cite ev)]
              (str "Provenance: " cite "."))])))

(defn- pilot-event->entry
  [ev]
  (let [landed (subs (or (:at ev) (:at-approx ev) (now-iso)) 0 10)
        consent-gate-id (:cited-consent-gate-event-id ev)
        evidence-kind (if consent-gate-id
                        :consent-gated-writer-event
                        :one-sided-extension)
        suggested-kind (cond
                         (= (:event ev) :substrate-creation) :new-substrate-class
                         (= (:tool ev) :anchor-flip) :capability-demonstration
                         (= (:tool ev) :pilot-action) :recursive-qa-closure
                         (= (get-in ev [:target :new-sub-kind]) :placeholder-to-living-view) :reframe-of-substrate-disposition
                         :else nil)]
    (auto-entry
     {:vsatarcs-id (str "hx:vsatarcs-align:auto:pilot-event-" (slugify (:id ev)))
      :wm-id (pilot-event-wm-id ev)
      :principle (pilot-event-principle ev)
      :evidence-kind evidence-kind
      :landed landed
      :note (pilot-event-note ev)
      :auto-source {:from :pilot-inhabitations
                    :event-id (:id ev)
                    :event-kind (:event ev)
                    :pilot-agent (:pilot-agent ev)}
      :auto-suggested-kind suggested-kind
      :writer-event-id (or (:id ev) consent-gate-id)})))

(defn- scan-pilot-inhabitations
  [{:keys [pilot-inhabitations-path]}]
  (let [events (:events (read-edn-file pilot-inhabitations-path))]
    (->> events
         (filter substantive-pilot-event?)
         (mapv pilot-event->entry))))

(defn- scan-stop-the-line-vocabulary
  [{:keys [vocabulary-path]}]
  (let [text (slurp vocabulary-path)]
    (when (and (str/includes? text ":μ/override-modes")
               (str/includes? text ":stop-the-line")
               (str/includes? text "override-mode"))
      [(auto-entry
        {:vsatarcs-id "hx:vsatarcs-align:auto:pilot-stop-the-line-vocabulary"
         :wm-id "M-war-machine-pilot v1 cycle-4 vocabulary-extension"
         :principle :pilot-override-mode-vocabulary-extension
         :evidence-kind :one-sided-extension
         :landed (file-date vocabulary-path)
         :note "The strategic vocabulary now carries :μ/override-modes with :stop-the-line as the first override-mode, giving the pilot/WM apparatus an explicit halt signal distinct from equilibrium-classification modes."
         :auto-source {:from :war-machine-strategic-vocabulary
                       :file vocabulary-path
                       :markers [":μ/override-modes" ":stop-the-line" "override-mode"]}
         :auto-suggested-kind :vocabulary-extension})])))

(defn- scan-inhabitation-log-ui
  [{:keys [wm-core-cljs-path]}]
  (let [text (slurp wm-core-cljs-path)]
    (when (and (str/includes? text "Inhabitation Log")
               (str/includes? text "pilot-inhabitations")
               (str/includes? text "replaces the static Pilot Contract"))
      [(auto-entry
        {:vsatarcs-id "hx:vsatarcs-align:auto:pilot-inhabitation-log-ui-swap"
         :wm-id "M-war-machine-pilot v1 cycle-3 UI swap (anchor 0011)"
         :principle :pilot-inhabitation-log-ui-swap
         :evidence-kind :one-sided-extension
         :landed (file-date wm-core-cljs-path)
         :note "The War Machine frontend now renders an Inhabitation Log card backed by pilot-inhabitations data, replacing the static Pilot Contract placeholder. The card exposes current/previous inhabitant state and expandable work traces."
         :auto-source {:from :wm-core-cljs
                       :file wm-core-cljs-path
                       :markers ["Inhabitation Log" "pilot-inhabitations" "Pilot Contract"]}
         :auto-suggested-kind :ui-discharge})])))

(defn- scan-stop-the-line-implementation
  [{:keys [wm-core-cljs-path wm-judge-clj-path]}]
  (let [core-text (slurp wm-core-cljs-path)
        judge-text (slurp wm-judge-clj-path)
        landed (last (sort [(file-date wm-core-cljs-path)
                            (file-date wm-judge-clj-path)]))]
    (when (and (str/includes? core-text "stop-the-line-banner")
               (str/includes? core-text ":judgement :mode")
               (str/includes? judge-text "mode to :stop-the-line")
               (str/includes? judge-text "OVERRIDE-MODE check"))
      [(auto-entry
        {:vsatarcs-id "hx:vsatarcs-align:auto:pilot-stop-the-line-implementation"
         :wm-id "M-war-machine-pilot v1 cycle-4 implementation"
         :principle :pilot-override-mode-realised-in-code
         :evidence-kind :one-sided-extension
         :landed landed
         :note "The :stop-the-line override mode is now realised end-to-end: judge elevates metabolic tier :stop-the-line over the inferred base-mode, and the frontend renders a top-of-dashboard stop-the-line banner from :judgement.mode."
         :auto-source {:from :wm-implementation
                       :files [wm-judge-clj-path wm-core-cljs-path]
                       :markers ["mode to :stop-the-line"
                                 "stop-the-line-banner"
                                 ":judgement :mode"]}
         :auto-suggested-kind :vocabulary-realised-in-code})])))

(def ^:private excursion-doc-catalog
  {"E-street-sweeper.md"
   {:vsatarcs-id "hx:vsatarcs-align:auto:pilot-e-street-sweeper-excursion"
    :wm-id "M-war-machine-pilot v1 cycle-5 excursion-handoff"
    :principle :pilot-cross-agent-excursion-handoff
    :evidence-kind :one-sided-extension
    :note "E-street-sweeper scopes the first E-prefix excursion out of the pilot mission: a cross-agent hop from the war-machine-pilot surface into a working-tree-closure peripheral. It demonstrates the hop/handoff shape without overlapping the VSATARCS feed excursion."
    :auto-suggested-kind :cross-agent-excursion}

   "E-night-shift.md"
   {:vsatarcs-id "hx:vsatarcs-align:auto:pilot-e-night-shift-excursion"
    :wm-id "M-war-machine-pilot v1 cycle-6 night-shift excursion"
    :principle :pilot-branch-isolated-code-modification-excursion
    :evidence-kind :one-sided-extension
    :note "E-night-shift scopes a branch-isolated code-modification peripheral out of the pilot mission. It upgrades hop-based trust from working-tree closure to PR-shaped code repair while preserving operator review as the merge boundary."
    :auto-suggested-kind :cross-agent-excursion}})

(defn- excursion-doc->entry
  [missions-dir filename descriptor]
  (let [path (str missions-dir "/" filename)]
    (when (.exists (io/file path))
      (auto-entry
       (assoc descriptor
              :landed (file-date path)
              :auto-source {:from :mission-doc
                            :file path
                            :kind :excursion})))))

(defn- scan-excursion-docs
  [{:keys [missions-dir]}]
  (->> excursion-doc-catalog
       (keep (fn [[filename descriptor]]
               (excursion-doc->entry missions-dir filename descriptor)))
       vec))

(defn candidate-entries
  "Return all current candidate bilateral-evidence entries derivable from the
   configured pilot substrates."
  ([]
   (candidate-entries default-config))
  ([config]
   (->> [(scan-pilot-inhabitations config)
         (scan-stop-the-line-vocabulary config)
         (scan-inhabitation-log-ui config)
         (scan-stop-the-line-implementation config)
         (scan-excursion-docs config)]
        (mapcat #(or % []))
        (sort-by (juxt :landed :vsatarcs-id))
        vec)))

(defn- novel-candidates
  [existing candidates]
  (let [{:keys [ids hashes]} (existing-entry-state existing)]
    (reduce (fn [acc entry]
              (cond
                (contains? ids (:vsatarcs-id entry))
                (do (log! :skip "existing vsatarcs-id; not overwriting"
                          {:vsatarcs-id (:vsatarcs-id entry)})
                    acc)

                (contains? hashes (:auto-content-hash entry))
                (do (log! :skip "existing auto-content-hash; duplicate suppressed"
                          {:vsatarcs-id (:vsatarcs-id entry)
                           :auto-content-hash (:auto-content-hash entry)})
                    acc)

                :else
                (conj acc entry)))
            []
            candidates)))

(defn ingest!
  "Scan current substrates and append novel auto-generated bilateral-evidence
   entries to the canonical VSATARCS file."
  ([]
   (ingest! default-config))
  ([config]
   (let [existing (bilateral-entries (:vsatarcs-aif-path config))
         candidates (candidate-entries config)
         novel (novel-candidates existing candidates)]
     (if (seq novel)
       (let [result (edn-comments/append-items-to-top-level-vector-preserving-comments
                     (:vsatarcs-aif-path config) :bilateral-evidence novel)]
         (log! :write "appended bilateral-evidence entries"
               {:count (count novel)
                :vsatarcs-ids (mapv :vsatarcs-id novel)
                :result result})
         {:ok true
          :candidate-count (count candidates)
          :appended-count (count novel)
          :appended-ids (mapv :vsatarcs-id novel)})
       (do
         (log! :noop "no novel bilateral-evidence entries detected"
               {:candidate-count (count candidates)})
         {:ok true
          :candidate-count (count candidates)
          :appended-count 0
          :appended-ids []})))))

(defn tick!
  "Run one feeder pass immediately."
  ([] (tick! default-config))
  ([config]
   (swap! !state
          (fn [s]
            (cond-> (or s {})
              true (assoc :last-tick-at (now-iso))
              true (assoc :config config))))
   (try
     (let [result (ingest! config)]
       (swap! !state assoc :last-result result)
       result)
     (catch Throwable t
       (let [failure {:ok false
                      :error (.getMessage t)}]
         (swap! !state assoc :last-result failure)
         (log! :error "feeder tick failed" {:error (.getMessage t)})
         (throw t))))))

(defn start!
  "Start periodic polling. Returns current status."
  ([] (start! default-config))
  ([config]
   (stop!)
   (let [^ScheduledExecutorService executor (Executors/newSingleThreadScheduledExecutor)]
     (reset! !state {:started-at (now-iso)
                     :config config
                     :executor executor
                     :log []})
     (.scheduleWithFixedDelay executor
                              #(try
                                 (tick! config)
                                 (catch Throwable _))
                              0
                              (long (:poll-ms config))
                              TimeUnit/MILLISECONDS)
     (log! :info "vsatarcs feeder started" {:poll-ms (:poll-ms config)})
     (status))))

(defn stop!
  "Stop periodic polling and clear executor state."
  []
  (when-let [^ScheduledExecutorService executor (:executor @!state)]
    (.shutdownNow executor))
  (swap! !state dissoc :executor)
  (when (map? @!state)
    (log! :info "vsatarcs feeder stopped"))
  (status))

(defn status
  "Return a lightweight feeder status snapshot."
  []
  (let [s @!state]
    {:running? (boolean (:executor s))
     :started-at (:started-at s)
     :last-tick-at (:last-tick-at s)
     :last-result (:last-result s)
     :log-tail (vec (take-last 10 (:log s)))}))
